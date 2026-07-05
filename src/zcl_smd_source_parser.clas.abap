CLASS zcl_smd_source_parser DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS: parse_tokens IMPORTING i_program TYPE program io_debugger TYPE REF TO zcl_smd_debugger_base.

ENDCLASS.

CLASS zcl_smd_source_parser IMPLEMENTATION.

  METHOD parse_tokens.

    DATA: lr_scan         TYPE REF TO cl_ci_scan,
          prev            TYPE string,
          change          TYPE string,
          split           TYPE TABLE OF string,
          o_scan          TYPE REF TO cl_ci_scan,
          o_statement     TYPE REF TO if_ci_kzn_statement_iterator,
          o_procedure     TYPE REF TO if_ci_kzn_statement_iterator,
          token           TYPE zcl_smd_smd_window=>ts_kword,
          calculated_var  TYPE zcl_smd_smd_window=>calculated_var,
          composed_var    TYPE zcl_smd_smd_window=>composed_vars,
          tokens          TYPE zcl_smd_smd_window=>tt_kword,
          calculated_vars TYPE  zcl_smd_smd_window=>tt_calculated,
          composed        TYPE zcl_smd_smd_window=>tt_composed,
          call            TYPE zcl_smd_smd_window=>ts_calls,
          call_line       TYPE zcl_smd_smd_window=>ts_calls_line,
          int_table       TYPE zcl_smd_smd_window=>ts_int_tabs,
          int_tables      TYPE zcl_smd_smd_window=>tt_tabs,
          eventtype       TYPE string,
          eventname       TYPE string,
          param           TYPE zcl_smd_smd_window=>ts_params,
          par             TYPE char1,
          type            TYPE char1,
          class           TYPE xfeld,
          cl_name         TYPE string,
          preferred       TYPE xfeld.

    "CLEAR mv_step.

    READ TABLE io_debugger->mo_window->mt_source WITH KEY include = i_program INTO DATA(source).
    IF sy-subrc <> 0.

      source-source = cl_ci_source_include=>create( p_name = i_program ).
      o_scan = NEW cl_ci_scan( p_include = source-source ).

      source-include = i_program.

      o_statement = cl_cikzn_scan_iterator_factory=>get_statement_iterator( ciscan = o_scan ).
      o_procedure = cl_cikzn_scan_iterator_factory=>get_procedure_iterator( ciscan = o_scan ).

      TRY.
          o_statement->next( ).
        CATCH cx_scan_iterator_reached_end.
          EXIT.
      ENDTRY.

      DATA(kw) = o_statement->get_keyword( ).

      DATA(word) = o_statement->get_token( offset = 2 ).

      o_procedure->statement_index = o_statement->statement_index.
      o_procedure->statement_type = o_statement->statement_type.

      DATA(max) = lines( o_scan->statements ).
      DO.
        CLEAR token-tt_calls.
        TRY.
            o_procedure->next( ).
          CATCH cx_scan_iterator_reached_end.
        ENDTRY.

        kw = o_procedure->get_keyword( ).

        token-name = kw.
        token-index = o_procedure->statement_index.
        READ TABLE o_scan->statements INDEX o_procedure->statement_index INTO DATA(statement).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        READ TABLE o_scan->tokens INDEX statement-from INTO DATA(scan_token).
        token-line = calculated_var-line = composed_var-line = scan_token-row.

        DATA new TYPE xfeld.

        IF kw = 'CLASS'.
          class = abap_true.
        ENDIF.

        IF kw = 'FORM' OR kw = 'METHOD' OR kw = 'METHODS' OR kw = 'CLASS-METHODS'.
          int_table-eventtype = eventtype = param-event =  kw.

          CLEAR eventname.
          IF kw = 'FORM'.
            CLEAR: class, param-class.
          ELSE.
            int_table-eventtype = eventtype = param-event =  'METHOD'.
          ENDIF.
        ENDIF.

        IF kw = 'ENDFORM' OR kw = 'ENDMETHOD'.
          CLEAR: eventtype, eventname, int_table.
          IF param-param IS INITIAL. "No params - save empty row if no params
            READ TABLE source-t_params WITH KEY event = param-event name = param-name TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              CLEAR param-type.
              APPEND param TO source-t_params.
            ENDIF.
          ENDIF.
        ENDIF.

        CLEAR prev.
        IF kw = 'ASSIGN' OR kw = 'ADD' OR kw = 'SUBTRACT' .
          DATA(count) = 0.
        ENDIF.
        CLEAR: new, token-to_evname, token-to_evtype .


        WHILE 1 = 1.
          IF kw IS INITIAL.
            EXIT.
          ENDIF.
          CLEAR change.
          word = o_procedure->get_token( offset = sy-index ).

          IF ( word CS '(' AND ( NOT word CS ')' ) ) OR word CS '->' OR word CS '=>'."can be method call
            call-name = word.
            call-event = 'METHOD'.
            REPLACE ALL OCCURRENCES OF '(' IN call-name WITH ''.
            FIND FIRST OCCURRENCE OF '->' IN  call-name.
            IF sy-subrc = 0.
              SPLIT call-name  AT '->' INTO TABLE split.
              call-name = split[ 2 ].
            ENDIF.

            FIND FIRST OCCURRENCE OF '=>' IN  call-name.
            IF sy-subrc = 0.
              SPLIT call-name  AT '=>' INTO TABLE split.
              call-name = split[ 2 ].
            ENDIF.
            token-to_evname = call-name.
            token-to_evtype = call-event = 'METHOD'.
            IF new = abap_true.
              call-name =  token-to_evname = 'CONSTRUCTOR'.
            ENDIF.
          ENDIF.

          IF sy-index = 1 AND token-name = word.
            CONTINUE.
          ENDIF.

          IF sy-index = 2 AND ( kw = 'DATA' OR kw = 'PARAMETERS' ).
            "WRITE: 'var =', token.
            int_table-name = word.
          ENDIF.

          IF sy-index = 2 AND kw = 'PERFORM'.
            token-to_evname = call-name = word.
            token-to_evtype = call-event = 'FORM'.
          ENDIF.

          IF sy-index = 2 AND class = abap_true AND param-class IS INITIAL.
            call_line-class = param-class = word.
          ENDIF.

          IF sy-index = 2 AND eventtype IS NOT INITIAL AND eventname IS INITIAL.
            int_table-eventname = eventname = param-name = word.

            MOVE-CORRESPONDING int_table TO call_line.
            call_line-index = o_procedure->statement_index + 1.
            "methods in definition should be overwrited by Implementation section
            READ TABLE source-tt_calls_line WITH KEY eventname = call_line-eventname eventtype = call_line-eventtype ASSIGNING FIELD-SYMBOL(<call_line>).
            IF sy-subrc = 0.
              <call_line> = call_line.
            ELSE.
              APPEND call_line TO source-tt_calls_line.
            ENDIF.

          ENDIF.

          IF word = ''.
            CLEAR call.
            CASE kw.
              WHEN 'COMPUTE'.
                IF  NOT prev CO '0123456789.+-/* '.
                  composed_var-name = prev.
                  APPEND  composed_var TO composed.
                ENDIF.
              WHEN 'CLEAR' OR 'SORT' OR 'CONDENSE'."no logic
              WHEN 'FORM'.
                IF param-name IS NOT INITIAL.
                  APPEND param TO source-t_params.
                  CLEAR param.
                ENDIF.
            ENDCASE.
            EXIT.
          ENDIF.

          IF word = 'USING' OR word = 'IMPORTING'.
            param-type = 'I'.
            CLEAR: type, par.
          ELSEIF word = 'CHANGING' OR word = 'EXPORTING' OR word = 'RETURNING'.

            IF param-param IS NOT INITIAL.
              APPEND param TO source-t_params.
              CLEAR: type, par, param-param.
            ENDIF.

            param-type = 'E'.
            CLEAR: type, par.
          ELSEIF word = 'OPTIONAL' OR word = 'PREFERRED'.
            CONTINUE.
          ELSEIF word = 'PARAMETER'.
            preferred = abap_true.
            CONTINUE.
          ENDIF.

          IF preferred = abap_true.
            READ TABLE source-t_params WITH KEY event = 'METHOD' name = param-name param = word ASSIGNING FIELD-SYMBOL(<param>).
            IF sy-subrc = 0.
              <param>-preferred = abap_true.
            ENDIF.

            CLEAR preferred.
            CONTINUE.
          ENDIF.

          IF word <> 'CHANGING' AND word <> 'EXPORTING' AND word <> 'RETURNING' AND word <> 'IMPORTING' AND word <> 'USING'.
            IF kw = 'FORM' OR kw = 'METHODS' OR kw = 'CLASS-METHODS'.
              IF par = abap_true AND type IS INITIAL AND word NE 'TYPE'.

                APPEND param TO source-t_params.
                CLEAR: par, param-param.
              ENDIF.

              IF par IS INITIAL AND sy-index > 3.
                param-param = word.
                par = abap_true.
                CONTINUE.
              ENDIF.
              IF par = abap_true AND type IS INITIAL AND word = 'TYPE'.
                type = abap_true.
                CONTINUE.
              ENDIF.
              IF par = abap_true AND type = abap_true.

                APPEND param TO source-t_params.
                CLEAR: type, par, param-param.
              ENDIF.
            ENDIF.
          ENDIF.

          DATA temp TYPE char30.
          temp = word.

          IF temp+0(5) = 'DATA('.
            SHIFT temp LEFT BY 5 PLACES.
            REPLACE ALL OCCURRENCES OF ')' IN temp WITH ''.
          ENDIF.

          IF temp+0(6) = '@DATA('.
            SHIFT temp LEFT BY 6 PLACES.
            REPLACE ALL OCCURRENCES OF ')' IN temp WITH ''.
          ENDIF.

          IF temp+0(13) = 'FIELD-SYMBOL('.
            SHIFT temp LEFT BY 13 PLACES.
            REPLACE ALL OCCURRENCES OF ')' IN temp WITH ''.
          ENDIF.

          IF word = 'NEW'.
            new = abap_true.
          ENDIF.

          FIND FIRST OCCURRENCE OF '->' IN word.
          IF sy-subrc = 0.
            CLEAR new.
          ENDIF.

          CASE kw.
            WHEN 'DATA' OR 'PARAMETERS'.
              IF (  prev = 'OF' ) AND temp <> 'TABLE' AND temp <> 'OF'.
                int_table-type = temp.
                APPEND int_table TO int_tables.
              ENDIF.

            WHEN 'COMPUTE'.
              IF temp CA '=' AND new IS INITIAL..
                change = prev.
              ENDIF.

              IF ( prev = '=' OR prev CA '+-/*' ) AND temp <> 'NEW'.
                IF NOT temp  CA '()' .
                  IF NOT temp  CO '0123456789. '.
                    composed_var-name = temp.
                    APPEND  composed_var TO composed.
                    IF call IS NOT INITIAL.
                      call-outer = temp.
                      READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                      IF sy-subrc <> 0.
                        APPEND call TO token-tt_calls.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.

            WHEN 'PERFORM' .

              IF  temp = 'USING' OR temp = 'CHANGING' .
                CLEAR prev.
              ENDIF.

              IF  prev = 'USING' OR prev = 'CHANGING' .

                IF NOT temp  CA '()' .
                  IF NOT temp  CO '0123456789. '.
                    call-outer = temp.
                    READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                    IF sy-subrc <> 0.
                      APPEND call TO token-tt_calls.
                    ENDIF.
                    change = temp.
                  ENDIF.
                ENDIF.
              ENDIF.

            WHEN 'CREATE' OR 'CALL'.
              DATA: import TYPE xfeld,
                    export.

              IF prev = 'FUNCTION' AND kw = 'CALL'.
                token-to_evtype =   call-event = 'FUNCTION'.
                token-to_evname =  call-name = word.
                REPLACE ALL OCCURRENCES OF '''' IN  token-to_evname WITH ''.
              ENDIF.

              IF word = 'EXPORTING' OR word = 'CHANGING' OR word = 'TABLES'.
                export = abap_true.
                CLEAR import.
                CONTINUE.

              ELSEIF word = 'IMPORTING'.
                import = abap_true.
                CLEAR export.
                CONTINUE.

              ENDIF.

              IF prev = 'OBJECT'.
                "WRITE : 'value', temp.
*          CONTINUE.
              ENDIF.

              IF  prev = '='.
                IF NOT temp  CA '()'.
                  IF NOT temp  CO '0123456789. '.
                    IF import = abap_true.
                      call-outer = temp.
                      READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                      IF sy-subrc <> 0.
                        APPEND call TO token-tt_calls.
                      ENDIF.
                      calculated_var-name = temp.
                      APPEND  calculated_var TO calculated_vars.
                    ELSEIF export = abap_true.
                      call-outer = temp.
                      READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                      IF sy-subrc <> 0.
                        APPEND call TO token-tt_calls.
                      ENDIF.
                      composed_var-name = temp.
                      APPEND  composed_var TO composed.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ELSE.
                IF NOT temp  CO '0123456789. ' AND temp <> '=' AND ( import = abap_true OR export = abap_true ).
                  call-inner = temp.
                ENDIF.
              ENDIF.

            WHEN 'CLEAR' OR 'SORT'.
              change = temp.
            WHEN  'CONDENSE'.

              IF temp <> 'NO-GAPS'.
                change = temp.
              ENDIF.
            WHEN 'ASSIGN' OR 'UNASSIGN'.
              ADD 1 TO count.
              IF count <> 2.
                change = temp.
              ENDIF.
            WHEN 'ADD' OR 'SUBTRACT'.
              ADD 1 TO count.
              IF count = 1.
                IF  NOT temp CO '0123456789.() '.
                  composed_var-name = temp.
                  APPEND  composed_var TO composed.
                ENDIF.
              ENDIF.
              IF count = 3.
                change = temp.
              ENDIF.
            WHEN 'READ'.
              IF prev =  'INTO' OR prev =  'ASSIGNING'.
                change = temp.
              ENDIF.

            WHEN 'SELECT'.
              IF  ( prev =  'INTO' OR prev =  '(' ) AND ( temp <> 'TABLE' AND temp <> '('  AND temp <> ')' AND  temp <> ',' ).
                change = temp.
              ENDIF.

            WHEN OTHERS.

          ENDCASE.
          IF call-event = 'METHOD'.
            IF word = 'EXPORTING' OR word = 'CHANGING' OR word = 'TABLES'.
              export = abap_true.
              CLEAR import.
              CONTINUE.

            ELSEIF word = 'IMPORTING'.
              import = abap_true.
              CLEAR export.
              CONTINUE.
            ENDIF.

            IF  temp = 'USING' OR temp = 'CHANGING' .
              CLEAR prev.
            ENDIF.

            IF  prev = 'USING' OR prev = 'CHANGING' .

              IF NOT temp  CA '()' .
                IF NOT temp  CO '0123456789. '.
                  call-outer = temp.
                  READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                  IF sy-subrc <> 0.
                    APPEND call TO token-tt_calls.
                  ENDIF.
                  change = temp.
                ENDIF.
              ENDIF.
            ENDIF.

            IF  prev = '='.
              IF NOT temp  CA '()'.
                IF NOT temp  CO '0123456789. '.
                  IF import = abap_true.
                    call-outer = temp.
                    READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                    IF sy-subrc <> 0.
                      APPEND call TO token-tt_calls.
                    ENDIF.

                    calculated_var-name = temp.
                    APPEND  calculated_var TO calculated_vars.
                  ELSEIF export = abap_true.
                    call-outer = temp.
                    READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                    IF sy-subrc <> 0.
                      APPEND call TO token-tt_calls.
                    ENDIF.
                    composed_var-name = temp.
                    APPEND  composed_var TO composed.
                  ENDIF.
                ENDIF.
              ENDIF.
            ELSE.
              IF NOT temp  CO '0123456789. ' AND temp <> '=' AND ( import = abap_true OR export = abap_true ).
                call-inner = temp.
              ENDIF.
            ENDIF.

          ENDIF.

          IF temp = '(' .
            prev = temp.
            CONTINUE.
          ENDIF.

          IF  NOT temp  CA '()'.
            IF temp <> 'TABLE' AND temp <> 'NEW'  AND prev <> '('.
              IF  kw <> 'PERFORM'.
                prev = temp.
              ELSEIF word = 'USING' OR word = 'CHANGING'.
                prev = temp.
              ENDIF.
            ENDIF.
          ENDIF.

          IF change IS NOT INITIAL.
            calculated_var-name = change.
            APPEND calculated_var TO calculated_vars.

            IF change+0(1) = '<'.

              SPLIT change AT '-' INTO TABLE split.
              change = split[ 1 ].
              IF eventtype IS INITIAL. "Global fs
                READ TABLE io_debugger->mo_window->mt_globals_set WITH KEY program = i_program ASSIGNING FIELD-SYMBOL(<globals_set>).
                IF sy-subrc <> 0.
                  APPEND INITIAL LINE TO io_debugger->mo_window->mt_globals_set ASSIGNING <globals_set>.
                  <globals_set>-program = i_program.
                ENDIF.
                READ TABLE  <globals_set>-mt_fs WITH KEY name = change TRANSPORTING NO FIELDS.
                IF sy-subrc <> 0.
                  APPEND INITIAL LINE TO  <globals_set>-mt_fs ASSIGNING FIELD-SYMBOL(<gl_fs>).
                  <gl_fs>-name = change.
                ENDIF.

              ELSE."local fs
                READ TABLE io_debugger->mo_window->mt_locals_set
                 WITH KEY program = i_program eventtype = eventtype eventname = eventname
                 ASSIGNING FIELD-SYMBOL(<locals_set>).
                IF sy-subrc <> 0.
                  APPEND INITIAL LINE TO io_debugger->mo_window->mt_locals_set ASSIGNING <locals_set>.
                  <locals_set>-program = i_program.
                  <locals_set>-eventname = eventname.
                  <locals_set>-eventtype = eventtype.
                ENDIF.
                READ TABLE <locals_set>-mt_fs WITH KEY name = change TRANSPORTING NO FIELDS.
                IF sy-subrc <> 0.
                  APPEND INITIAL LINE TO <locals_set>-mt_fs ASSIGNING FIELD-SYMBOL(<loc_fs>).
                  <loc_fs>-name = change.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDWHILE.
        token-from = statement-from.
        token-to = statement-to.
        APPEND token TO tokens.
        IF o_procedure->statement_index = max.
          EXIT.
        ENDIF.

      ENDDO.

      "Fill keyword links for perform

      LOOP AT tokens ASSIGNING FIELD-SYMBOL(<s_token>) WHERE tt_calls IS NOT INITIAL.

        READ TABLE <s_token>-tt_calls INDEX 1 INTO call.
        DATA(index) = 0.
        LOOP AT source-t_params INTO param WHERE event = call-event AND name = call-name .
          ADD 1 TO index.
          READ TABLE <s_token>-tt_calls INDEX index ASSIGNING FIELD-SYMBOL(<call>).
          IF sy-subrc = 0.
            <call>-inner = param-param.
            IF param-type = 'I'.
              <call>-type = '>'.
            ELSE.
              <call>-type = '<'.
            ENDIF.
          ENDIF.
        ENDLOOP.

      ENDLOOP.

      "clear value(var) to var.
      LOOP AT source-t_params ASSIGNING <param>.
        REPLACE ALL OCCURRENCES OF 'VALUE(' IN <param>-param WITH ''.
        REPLACE ALL OCCURRENCES OF ')' IN <param>-param WITH ''.
      ENDLOOP.

      source-scan = o_scan.
      source-t_keytokens = tokens.
      source-t_calculated = calculated_vars.
      source-t_composed = composed.
      source-tt_tabs = int_tables.
      APPEND source TO io_debugger->mo_window->mt_source.

    ENDIF.

  ENDMETHOD.


ENDCLASS.
