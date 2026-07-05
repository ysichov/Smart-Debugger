CLASS zcl_smd_mermaid DEFINITION PUBLIC INHERITING FROM zcl_smd_popup CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: BEGIN OF ts_if,
             if_ind      TYPE i,
             end_ind     TYPE i,
             before_else TYPE i,
           END OF ts_if,
           tt_if TYPE STANDARD TABLE OF ts_if WITH EMPTY KEY.


    DATA: mo_debugger     TYPE REF TO zcl_smd_debugger_base,
          mo_mm_container TYPE REF TO cl_gui_container,
          mo_mm_toolbar   TYPE REF TO cl_gui_container,
          mo_toolbar      TYPE REF TO cl_gui_toolbar,
          mo_diagram      TYPE REF TO object,
          mv_type         TYPE string,
          ms_if           TYPE ts_if,
          mt_if           TYPE tt_if,
          mv_step         TYPE i,
          mt_steps        TYPE zcl_smd_appl=>tt_steps.

    METHODS: constructor IMPORTING io_debugger TYPE REF TO zcl_smd_debugger_base
                                   i_type      TYPE string,

      steps_flow IMPORTING i_direction TYPE ui_func OPTIONAL,
      magic_search IMPORTING i_direction TYPE ui_func OPTIONAL,
      code_execution_scanner,
      parse_call IMPORTING i_program TYPE program i_index TYPE i i_stack TYPE i i_event TYPE string,
      add_toolbar_buttons,
      hnd_toolbar FOR EVENT function_selected OF cl_gui_toolbar IMPORTING fcode,
      open_mermaid IMPORTING i_mm_string TYPE string.

ENDCLASS.

CLASS zcl_smd_mermaid IMPLEMENTATION.

  METHOD constructor.

    DATA text TYPE text100.

    super->constructor( ).

    mo_debugger = io_debugger.
    mv_type = i_type.

    CHECK zcl_smd_appl=>is_mermaid_active = abap_true.

    CASE mv_type.
      WHEN 'DIAG'.
        text = 'Calls flow'.
      WHEN 'SMART'.
        text = 'Calculations sequence'.
    ENDCASE.

    IF mo_box IS INITIAL.
      mo_box = create( i_name = text i_width = 1000 i_hight = 300 ).
      "save new popup ref
      APPEND INITIAL LINE TO zcl_smd_appl=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>).
      <popup>-parent = mo_debugger->mo_window->mo_box.
      <popup>-child = mo_box.

      SET HANDLER on_box_close FOR mo_box.

      CREATE OBJECT mo_splitter
        EXPORTING
          parent  = mo_box
          rows    = 2
          columns = 1
        EXCEPTIONS
          OTHERS  = 1.

      mo_splitter->get_container(
        EXPORTING
          row       = 2
          column    = 1
        RECEIVING
          container = mo_mm_container ).

      mo_splitter->get_container(
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = mo_mm_toolbar ).

      mo_splitter->set_row_height( id = 1 height = '3' ).
      mo_splitter->set_row_height( id = 2 height = '70' ).

      mo_splitter->set_row_sash( id    = 1
                                 type  = 0
                                 value = 0 ).

      CREATE OBJECT mo_toolbar EXPORTING parent = mo_mm_toolbar.
      add_toolbar_buttons( ).
      mo_toolbar->set_visible( 'X' ).
    ENDIF.
    CASE mv_type.
      WHEN 'DIAG'.
        steps_flow( ).
      WHEN 'SMART'.
        magic_search( ).
    ENDCASE.

  ENDMETHOD.

  METHOD steps_flow.

    TYPES: BEGIN OF lty_entity,
             event TYPE string,
             name  TYPE string,
           END OF lty_entity,
           BEGIN OF t_ind,
             from TYPE i,
             to   TYPE i,
           END OF t_ind  .

    DATA: mm_string TYPE string,
          name      TYPE string,
          entities  TYPE TABLE OF lty_entity,
          entity    TYPE lty_entity,
*          ind1      TYPE i,
*          ind2      TYPE i,
          parts     TYPE TABLE OF string,
          step      LIKE LINE OF mo_debugger->mt_steps,
          ind       TYPE t_ind,
          indexes   TYPE TABLE OF t_ind.


    DATA(copy) = mo_debugger->mt_steps.

    LOOP AT copy ASSIGNING FIELD-SYMBOL(<copy>).
      IF <copy>-eventtype = 'METHOD'.
        SPLIT <copy>-program AT '=' INTO TABLE parts.
        <copy>-eventname = entity-name = |"{ parts[ 1 ] }->{ <copy>-eventname }"|.
        entity-event = <copy>-eventtype.

      ELSEIF <copy>-eventtype = 'FUNCTION'.
        <copy>-eventname = entity-name = |"{ <copy>-eventtype }:{ <copy>-eventname }"|.
      ELSE.
        <copy>-eventname = entity-name = |"{ <copy>-program }:{ <copy>-eventname }"|.
      ENDIF.

      COLLECT entity INTO entities.
    ENDLOOP.

    CLEAR step.

    IF i_direction IS INITIAL.
      mm_string = |graph TD\n |.
    ELSE.
      mm_string = |graph { i_direction }\n |.
    ENDIF.

    LOOP AT copy INTO DATA(step2).
      IF step IS INITIAL.
        step = step2.
        CONTINUE.
      ENDIF.
      IF step2-stacklevel > step-stacklevel.

        READ TABLE entities WITH KEY name = step-eventname TRANSPORTING NO FIELDS.
        ind-from = sy-tabix.
        READ TABLE entities WITH KEY name = step2-eventname TRANSPORTING NO FIELDS.
        ind-to = sy-tabix.
        READ TABLE indexes WITH KEY from = ind-from to = ind-to TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          mm_string = |{ mm_string }{ ind-from }({ step-eventname }) --> { ind-to }({ step2-eventname })\n|.
          APPEND ind TO indexes.
        ENDIF.
      ENDIF.
      step = step2.
    ENDLOOP.
    mm_string = |{  mm_string }\n|.

    open_mermaid( mm_string ).

  ENDMETHOD.

  METHOD magic_search.

    DATA: add         TYPE xfeld,
          mm_string   TYPE string,
          sub         TYPE string,
          form        TYPE string,
          direction   TYPE string,
          box_s       TYPE string,
          box_e       TYPE string,
          ind2        TYPE i,
          start       TYPE i,
          end         TYPE i,
          bool        TYPE string,
          block_first TYPE i,
          els_before  TYPE i.

    TYPES: BEGIN OF ts_line,
             cond       TYPE string,
             include    TYPE string,
             line       TYPE i,
             ind        TYPE i,
             event      TYPE string,
             stack      TYPE i,
             code       TYPE string,
             arrow      TYPE string,
             subname    TYPE string,
             del        TYPE flag,
             els_before TYPE i,
             els_after  TYPE i,
           END OF ts_line.

    DATA: line      TYPE ts_line,
          lines     TYPE STANDARD TABLE OF ts_line,
          pre_stack TYPE ts_line,
          opened    TYPE i.

    CLEAR mo_debugger->mo_window->mt_watch.

    IF lines( mo_debugger->mt_steps ) > 1.
      DATA(steps) = mo_debugger->mt_steps.
    ELSE.
      code_execution_scanner( ).
      steps = mt_steps.
    ENDIF.

    DATA: yes TYPE xfeld.
    DATA(selected_var) = mo_debugger->mt_selected_var.

    LOOP AT steps INTO DATA(step).
      READ TABLE mo_debugger->mo_window->mt_source WITH KEY include = step-include INTO DATA(source).
      READ TABLE source-t_keytokens WITH KEY line = step-line INTO DATA(keyword).
      LOOP AT keyword-tt_calls INTO DATA(call).

        READ TABLE selected_var WITH KEY name = call-outer TRANSPORTING NO FIELDS.
        IF sy-subrc = 0 OR mo_debugger->mt_selected_var IS INITIAL.
          yes = abap_true.
        ENDIF.

        READ TABLE selected_var WITH KEY name = call-inner TRANSPORTING NO FIELDS.
        IF sy-subrc = 0 OR mo_debugger->mt_selected_var IS INITIAL.
          yes = abap_true.
        ENDIF.
      ENDLOOP.
      IF yes = abap_true.
        LOOP AT keyword-tt_calls INTO call.
          READ TABLE selected_var WITH KEY name = call-outer TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND INITIAL LINE TO  selected_var ASSIGNING FIELD-SYMBOL(<selected>).
            <selected>-name = call-outer.
          ENDIF.

          READ TABLE selected_var WITH KEY name = call-inner TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND INITIAL LINE TO  selected_var ASSIGNING <selected>.
            <selected>-name = call-inner.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    "deleting empty cycles.
    DATA: prev    LIKE LINE OF steps,
          pre_key TYPE string.

    READ TABLE mo_debugger->mo_window->mt_source WITH KEY include = step-include INTO source.

    LOOP AT steps ASSIGNING FIELD-SYMBOL(<step>).
      DATA(ind) = sy-tabix.
      READ TABLE source-t_keytokens WITH KEY line = <step>-line INTO DATA(key).
      IF prev IS NOT INITIAL.
        IF ( key-name = 'ENDDO' OR key-name = 'ENDWHILE' OR key-name = 'ENDLOOP' OR key-name = 'ENDIF' )  AND
           ( pre_key = 'DO' OR pre_key = 'LOOP'  OR pre_key = 'WHILE'  OR pre_key = 'IF' ).
          <step>-first = 'D'."to delete
          READ TABLE steps INDEX ind - 1 ASSIGNING FIELD-SYMBOL(<step_prev>).
          <step_prev>-first = 'D'.
        ENDIF.
      ENDIF.
      prev = <step>.
      pre_key = key-name.
    ENDLOOP.

    DELETE steps WHERE first = 'D'.

    SORT steps BY step DESCENDING.

    "collecting dependents variables
    LOOP AT steps INTO step.

      READ TABLE mo_debugger->mo_window->mt_source WITH KEY include = step-include INTO source.

      LOOP AT source-t_calculated INTO DATA(calculated_var) WHERE line = step-line.
        READ TABLE selected_var WITH KEY name = calculated_var-name TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.

          LOOP AT source-t_composed INTO DATA(composed_var) WHERE line = step-line.
            READ TABLE selected_var WITH KEY name = composed_var-name TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO  selected_var ASSIGNING <selected>.
              <selected>-name = composed_var-name.
            ENDIF.
          ENDLOOP.
        ENDIF.
        "adding returning values
*        LOOP AT source-t_params INTO DATA(param).
*          READ TABLE mo_debugger->mt_selected_var WITH KEY name = param-param TRANSPORTING NO FIELDS.
*          IF sy-subrc <> 0.
*            APPEND INITIAL LINE TO  mo_debugger->mt_selected_var ASSIGNING <selected>.
*            <selected>-name = param-param.
*          ENDIF.
*        ENDLOOP.
      ENDLOOP.

      READ TABLE source-t_keytokens WITH KEY line = step-line INTO keyword.
      LOOP AT keyword-tt_calls INTO call.

        READ TABLE selected_var WITH KEY name = call-outer TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO  selected_var ASSIGNING <selected>.
          <selected>-name = call-inner.
        ENDIF.
      ENDLOOP.

    ENDLOOP.
    SORT selected_var.
    DELETE ADJACENT DUPLICATES FROM selected_var.

    "collecting watchpoints
    CLEAR mo_debugger->mo_window->mt_coverage.

    LOOP AT  steps INTO step.

      READ TABLE mo_debugger->mo_window->mt_source WITH KEY include = step-include INTO source.
      READ TABLE source-t_keytokens WITH KEY line = step-line INTO key.

      CLEAR line-cond.
      IF key-name = 'IF' OR key-name = 'ELSE' OR key-name = 'ENDIF' OR key-name = 'ELSEIF' OR
         key-name = 'CASE' OR key-name = 'WHEN' OR key-name = 'ENDCASE' OR
          key-name = 'DO' OR key-name = 'ENDDO'  OR key-name = 'LOOP'  OR key-name = 'ENDLOOP' OR key-name = 'WHILE' OR key-name = 'ENDWHILE'.
        APPEND INITIAL LINE TO mo_debugger->mo_window->mt_watch ASSIGNING FIELD-SYMBOL(<watch>).

        <watch>-program = step-program.
        <watch>-line = line-line = step-line.

        INSERT line INTO lines INDEX 1 ASSIGNING FIELD-SYMBOL(<line>).
        <line>-cond = key-name.
        <line>-event = step-eventname.
        <line>-stack = step-stacklevel.
        <line>-include = step-include.
      ENDIF.
      CLEAR ind.
      LOOP AT  source-t_calculated INTO calculated_var WHERE line = step-line.
        ADD 1 TO ind.
        LOOP AT source-t_composed INTO composed_var WHERE line = step-line.
          READ TABLE selected_var WITH KEY name = composed_var-name TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            APPEND INITIAL LINE TO  selected_var ASSIGNING <selected>.
            <selected>-name = composed_var-name.
          ENDIF.
        ENDLOOP.

        READ TABLE selected_var WITH KEY name = calculated_var-name TRANSPORTING NO FIELDS.
        IF sy-subrc = 0 OR mo_debugger->mt_selected_var IS INITIAL.

          APPEND INITIAL LINE TO mo_debugger->mo_window->mt_watch ASSIGNING <watch>.
          <watch>-program = step-program.
          <watch>-line = line-line = step-line.

          "should be commented for Smart debugger
*          LOOP AT lines ASSIGNING <line> WHERE line = line-line AND event = step-eventname AND stack = step-stacklevel .
*            <line>-del = abap_true.
*          ENDLOOP.
          IF ind = 1.
            line-event = step-eventname.
            line-stack = step-stacklevel.
            line-include = step-include.
            INSERT line INTO lines INDEX 1.
          ENDIF.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

    DELETE lines WHERE del = abap_true.

    "getting code texts and calls params
    LOOP AT lines ASSIGNING <line>.
      ind = sy-tabix.

      READ TABLE mo_debugger->mo_window->mt_source WITH KEY include = <line>-include INTO source.
      READ TABLE source-t_keytokens WITH KEY line = <line>-line INTO keyword.
      LOOP AT source-scan->tokens FROM keyword-from TO keyword-to INTO DATA(token).
        IF token-str = 'USING' OR token-str = 'EXPORTING' OR token-str = 'IMPORTING' OR token-str = 'CHANGING'.
          EXIT.
        ENDIF.
        IF <line>-code IS INITIAL.
          <line>-code = token-str.
        ELSE.
          <line>-code = |{  <line>-code } { token-str }|.
        ENDIF.
      ENDLOOP.

      IF keyword-to_evname IS NOT INITIAL.
        SORT keyword-tt_calls BY outer.
        DELETE ADJACENT DUPLICATES FROM keyword-tt_calls.
        LOOP AT keyword-tt_calls INTO call.
          IF sy-tabix <> 1.
            <line>-arrow = |{ <line>-arrow }, |.
          ENDIF.
          <line>-arrow  = |{ <line>-arrow  } { call-outer } { call-type } { call-inner }|.
          <line>-subname = call-name.
          REPLACE ALL OCCURRENCES OF '''' IN <line>-subname WITH ''.
          REPLACE ALL OCCURRENCES OF '"' IN  <line>-code WITH ''.
        ENDLOOP.
      ENDIF.
      REPLACE ALL OCCURRENCES OF '''' IN <line>-subname WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN <line>-arrow WITH ''.
      REPLACE ALL OCCURRENCES OF ')' IN <line>-arrow WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN <line>-subname WITH ''.
      REPLACE ALL OCCURRENCES OF ')' IN <line>-subname WITH ''.
    ENDLOOP.

    "check subform execution steps existance and if/case structures build

    DATA: if_depth   TYPE i,
          when_count TYPE i.
    LOOP AT lines ASSIGNING <line> WHERE code <> 'DO' AND code <> 'ENDDO' AND code <> 'WHILE' AND code <> 'ENDWHILE' AND code <> 'LOOP' AND code <> 'ENDLOOP' .
      <line>-ind = sy-tabix.

      FIELD-SYMBOLS: <if> TYPE ts_if.
      IF <line>-cond = 'IF' OR  <line>-cond = 'CASE'.
        ADD 1 TO if_depth.
        CLEAR when_count.
        APPEND INITIAL LINE TO mt_if  ASSIGNING <if>.
        <if>-if_ind = <line>-ind.

      ENDIF.

      IF <line>-cond = 'ENDIF' OR <line>-cond = 'ENDCASE'.
        <if>-end_ind = <line>-ind.
        SUBTRACT 1 FROM if_depth.
        LOOP AT mt_if  ASSIGNING <if> WHERE end_ind = 0.
        ENDLOOP.
      ENDIF.

      IF <line>-cond = 'WHEN'.
        ADD 1 TO when_count.
      ENDIF.

      IF <line>-cond = 'ELSE' OR <line>-cond = 'ELSEIF'.

        <line>-els_before = els_before.
        <line>-els_after = <line>-ind.
        DATA(counter) = <line>-ind + 1.
        DO.
          READ TABLE lines INDEX counter INTO line.
          IF sy-subrc <> 0.
            CLEAR <line>-els_after.
            EXIT.
          ENDIF.

          IF line-cond = 'ELSE' OR line-cond = 'ELSEIF'.
            CLEAR <line>-els_after.
            EXIT.
          ELSEIF  line-cond <> 'DO' AND line-cond <> 'ENDDO' AND line-cond <> 'WHILE' AND line-cond <> 'ENDWHILE' AND line-cond <> 'LOOP' AND line-cond <> 'ENDLOOP'.
            <line>-els_after = counter.
            EXIT.
          ELSE.
            ADD 1 TO counter.

          ENDIF.
        ENDDO.
        IF when_count = 1. "to refactor
*          <if>-if_ind = els_before.
*          CLEAR <line>-els_before.
        ENDIF.
      ENDIF.

      IF <line>-cond = 'WHEN'.

        <line>-els_before = els_before.
        <line>-els_after = <line>-ind.
        counter = <line>-ind + 1.
        DO.
          READ TABLE lines INDEX counter INTO line.
          IF sy-subrc <> 0.
            CLEAR <line>-els_after.
            EXIT.
          ENDIF.

          IF line-cond = 'WHEN'.
            CLEAR <line>-els_after.
            EXIT.
          ELSEIF  line-cond <> 'DO' AND line-cond <> 'ENDDO' AND line-cond <> 'WHILE' AND line-cond <> 'ENDWHILE' AND line-cond <> 'LOOP' AND line-cond <> 'ENDLOOP'.
            <line>-els_after = counter.
            EXIT.
          ELSE.
            ADD 1 TO counter.

          ENDIF.
        ENDDO.
        IF when_count = 1.
          <if>-if_ind = els_before.
          CLEAR <line>-els_before.
        ENDIF.
      ENDIF.

      IF <line>-cond <> 'ELSE' AND <line>-cond <> 'ELSEIF' AND <line>-cond <> 'WHEN'.
        els_before = <line>-ind.
      ELSE.
        CLEAR   els_before.
      ENDIF.

      READ TABLE lines WITH KEY event = <line>-subname TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CLEAR <line>-arrow.
      ENDIF.
    ENDLOOP.

    IF mt_if IS INITIAL AND ms_if-if_ind IS NOT INITIAL.
      INSERT ms_if INTO mt_if INDEX 1.
    ENDIF.

    IF lines( lines ) > 0.
      IF lines[ lines( lines ) ]-arrow IS NOT INITIAL.
        CLEAR lines[ lines( lines ) ]-arrow .
      ENDIF.
    ENDIF.

    "creating mermaid code
    CHECK lines IS NOT INITIAL.

    IF i_direction IS INITIAL.
      IF lines( lines ) < 100.
        direction = 'LR'.
      ELSE.
        direction = 'TB'.
      ENDIF.
    ELSE.
      direction = i_direction.
    ENDIF.

    mm_string = |graph { direction }\n |.

    LOOP AT lines INTO line WHERE cond <> 'ELSE' AND cond <> 'ELSEIF' AND  cond <> 'WHEN'.

      ind = sy-tabix.

      IF line-cond IS INITIAL.
        box_s = '('.
        box_e = ')'.
      ELSE.
        box_s = '{'.
        box_e = '}'.
      ENDIF.

      IF pre_stack IS INITIAL.
        pre_stack = line.
      ENDIF.

      IF ( pre_stack-stack > line-stack OR pre_stack-event <> line-event ) AND opened > 0 AND sub IS INITIAL.
        IF pre_stack-stack = line-stack AND pre_stack-event <> line-event.
          DATA(times) = 1.
        ELSE.
          times = pre_stack-stack - line-stack.
        ENDIF.

        DO times TIMES.
          mm_string = |{ mm_string } end\n|.
          SUBTRACT 1 FROM opened.
          IF opened = 0.
            EXIT.
          ENDIF.
        ENDDO.

      ENDIF.
      DATA: name TYPE string.
      IF    line-cond = 'LOOP' OR line-cond = 'DO' OR line-cond = 'WHILE' OR line-arrow IS NOT INITIAL .
        IF line-arrow IS NOT INITIAL.
          mm_string = |{ mm_string }{ ind }{ box_s }"{ line-code }"{ box_e }\n|.
          pre_stack = line.

        ENDIF.

        "IF strlen( line-code ) > 50.
        "lv_name = line-code+0(50).
        "ELSE.
        name = line-code.
        "ENDIF.
        REPLACE ALL OCCURRENCES OF `PERFORM` IN name WITH `FORM` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF `CALL FUNCTION` IN name WITH `FUNCTION` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF `CALL METHOD` IN name WITH `METHOD` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF `<` IN name WITH `` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF '>' IN name WITH `` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF `-` IN name WITH `~` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF ` ` IN name WITH `&nbsp;` IN CHARACTER MODE.

        mm_string = |{ mm_string } subgraph S{ ind }["{ name }"]\n  direction { direction }\n|.
        ADD 1 TO opened.
        start = ind.
        CONTINUE.
      ENDIF.

      IF line-cond = 'ENDLOOP' OR line-cond = 'ENDDO' OR line-cond = 'ENDWHILE'.
        SUBTRACT 1 FROM opened.
        mm_string = |{ mm_string } end\n|.
        CONTINUE.
      ENDIF.

      mm_string = |{ mm_string }{ ind }{ box_s }"{ line-code }"{ box_e }\n|.
      pre_stack = line.

    ENDLOOP.

    DO opened TIMES.
      mm_string = |{ mm_string } end\n|.
      SUBTRACT 1 FROM opened.
    ENDDO.

    DATA: if_ind TYPE i.
    CLEAR pre_stack.
    LOOP AT lines INTO line WHERE cond <> 'LOOP' AND cond <> 'DO' AND cond <> 'WHILE' AND cond <> 'ENDLOOP' AND cond <> 'ENDDO' AND cond <> 'ENDWHILE'.

      IF line-cond = 'IF' OR line-cond = 'CASE' .
        ADD 1 TO if_ind.
        READ TABLE mt_if INDEX if_ind INTO ms_if.
      ENDIF.


      IF pre_stack IS INITIAL.
        IF line-cond = 'WHEN' OR line-cond = 'ELSE' OR line-cond = 'ELSEIF'.
          pre_stack = lines[ <if>-if_ind ].
        ELSE.
          pre_stack = line.

          IF line-arrow IS NOT INITIAL.
            sub = '|"' && line-arrow && '"|'.
          ELSE.
            CLEAR sub.
          ENDIF.

          CONTINUE.
        ENDIF.

      ENDIF.

      IF line-cond = 'ELSE' OR line-cond = 'ELSEIF' OR line-cond = 'WHEN'.
        bool = '|' && line-code && '|'.
        IF line-els_after IS NOT INITIAL.
          mm_string = |{ mm_string }{ ms_if-if_ind }-->{ bool }{ line-els_after }\n|.
          DATA(diff) = ms_if-end_ind - line-els_after.
          DATA(last_els) = line-els_after.
          IF line-cond <> 'WHEN' AND line-cond <> 'ELSEIF'  AND  diff > 1 AND line-els_after <> ms_if-end_ind.
            mm_string = |{ mm_string }{  line-els_after }-->{ ms_if-end_ind }\n|.
          ENDIF.
        ELSE.
          mm_string = |{ mm_string }{ ms_if-if_ind }-->{ bool }{ ms_if-end_ind }\n|.
        ENDIF.

        IF line-els_before IS NOT INITIAL AND line-els_before <> ms_if-if_ind.
          mm_string = |{ mm_string }{ line-els_before }-->{ ms_if-end_ind }\n|.
        ENDIF.

        IF lines[ line-ind + 1 ]-cond <> 'ENDIF' AND lines[ line-ind + 1 ]-cond <> 'ENDCASE'.
          CLEAR pre_stack.
        ENDIF.
        CONTINUE.
      ENDIF.

      IF   pre_stack-cond NE 'ELSE' AND pre_stack-cond NE 'ELSEIF' AND pre_stack-cond NE 'WHEN' AND NOT ( last_els = line-ind ).

        mm_string = |{ mm_string }{ pre_stack-ind }-->{ sub }{ line-ind }\n|.

        IF line-arrow IS NOT INITIAL.
          sub = '|"' && line-arrow && '"|'.
        ELSE.
          CLEAR sub.
        ENDIF.

      ENDIF.

      pre_stack = line.

      IF line-cond = 'ENDIF' OR line-cond = 'ENDCASE'.
        DELETE mt_if INDEX if_ind.
        SUBTRACT 1 FROM if_ind.
        READ TABLE mt_if INDEX if_ind INTO ms_if.
      ENDIF.

    ENDLOOP.
    mm_string = |{  mm_string }\n|.

    open_mermaid( mm_string ).
  ENDMETHOD.

  METHOD parse_call.
    DATA: statement TYPE i,
          stack     TYPE i.

    stack = i_stack + 1.
    statement = i_index.
    READ TABLE mo_debugger->mo_window->mt_source WITH KEY include = i_program INTO DATA(source).
    DO.
      READ TABLE source-t_keytokens WITH KEY index =  statement INTO DATA(key).
      IF key-name = 'DATA'.
        ADD 1 TO statement.
        CONTINUE.
      ENDIF.
      ADD 1 TO mv_step.
      APPEND INITIAL LINE TO mt_steps ASSIGNING FIELD-SYMBOL(<step>).

      <step>-step = mv_step.
      <step>-line = key-line.
      <step>-eventname = i_event.
      <step>-eventtype = 'FORM'.
      <step>-stacklevel = stack.
      <step>-program = i_program.
      <step>-include = i_program.

      IF key-to_evname IS NOT INITIAL.
        READ TABLE source-tt_calls_line WITH KEY eventname = key-to_evname eventtype = key-to_evtype INTO DATA(call_line).

        parse_call( EXPORTING i_index = call_line-index
                                                 i_event = call_line-eventname
                                                 i_program = i_program
                                                 i_stack   = stack
                                                  ).

      ENDIF.

      IF key-name = 'ENDFORM' OR key-name = 'ENDMETHOD'.
        RETURN.
      ENDIF.

      ADD 1 TO statement.
    ENDDO.
  ENDMETHOD.


  METHOD code_execution_scanner.
    "code execution scanner
    DATA: max       TYPE i,
          step      TYPE i,
          call_line TYPE zcl_smd_smd_window=>ts_calls_line.

    READ TABLE mo_debugger->mo_window->mt_source INDEX 1 INTO DATA(source).

    DATA: structure LIKE source-scan->structures.

    READ TABLE source-scan->structures WITH KEY type = 'E' TRANSPORTING  NO FIELDS.
    IF sy-subrc = 0.
      structure = source-scan->structures.
      DELETE structure WHERE type <> 'E'.
      SORT structure BY stmnt_type ASCENDING.
    ELSE.
      CLEAR max.
      LOOP AT source-scan->structures INTO DATA(str) WHERE type <> 'P' AND type <> 'C' .
        IF max < str-stmnt_to.
          max = str-stmnt_to.
          APPEND str TO structure.
        ENDIF.
      ENDLOOP.
    ENDIF.

    DATA: event     TYPE string,
          stack     TYPE i VALUE 1,
          statement TYPE i.
    CLEAR mv_step.
    LOOP AT structure INTO str.

      READ TABLE source-t_keytokens WITH KEY index =  statement INTO DATA(key).

      IF str-type = 'E'.
        statement = str-stmnt_from + 1.
        event = key-name.
      ELSE.
        statement = str-stmnt_from.
      ENDIF.

      WHILE statement <= str-stmnt_to.
        READ TABLE source-t_keytokens WITH KEY index =  statement INTO key.

        IF key-name = 'DATA' OR key-name = 'CONSTANTS' OR sy-subrc <> 0.
          ADD 1 TO statement.
          CONTINUE.
        ENDIF.
        ADD 1 TO mv_step.
        APPEND INITIAL LINE TO mt_steps ASSIGNING FIELD-SYMBOL(<step>).

        <step>-step = mv_step.
        <step>-line = key-line.
        <step>-eventname = event.
        <step>-eventtype = 'EVENT'.
        <step>-stacklevel = stack.
        <step>-program = source-include.
        <step>-include = source-include.

        IF key-to_evname IS NOT INITIAL.
          READ TABLE source-tt_calls_line WITH KEY eventname = key-to_evname eventtype = key-to_evtype INTO call_line.

          parse_call( EXPORTING i_index = call_line-index
                                           i_event = call_line-eventname
                                           i_program = source-include
                                           i_stack   = stack
                                            ).
        ENDIF.

        ADD 1 TO statement.
      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.


  METHOD add_toolbar_buttons.

    DATA: button TYPE ttb_button,
          events TYPE cntl_simple_events,
          event  LIKE LINE OF events.

    button  = VALUE #(
     ( function = 'TD' icon = CONV #( icon_view_expand_vertical ) quickinfo = 'Vertical' text = '' )
     ( function = 'LR' icon = CONV #( icon_view_expand_horizontal ) quickinfo = 'Horizontal' text = '' )
     ( butn_type = 3  )
     ( function = 'TEXT' icon = CONV #( icon_wd_caption ) quickinfo = 'Mermaid Diagram text' text = '' )
                    ).

    mo_toolbar->add_button_group( button ).

*   Register events
    event-eventid = cl_gui_toolbar=>m_id_function_selected.
    event-appl_event = space.
    APPEND event TO events.

    mo_toolbar->set_registered_events( events = events ).
    SET HANDLER me->hnd_toolbar FOR mo_toolbar.

  ENDMETHOD.

  METHOD hnd_toolbar.


    IF fcode = 'TEXT'.
      DATA: mm_string TYPE string,
            ref       TYPE REF TO data.
      CALL METHOD mo_diagram->('GET_SOURCE_CODE_STRING') RECEIVING result = mm_string.
      GET REFERENCE OF mm_string INTO ref.
      NEW zcl_smd_text_viewer( ref ).

      RETURN.
    ENDIF.

    CASE mv_type.
      WHEN 'DIAG'.
        steps_flow( fcode ).
      WHEN 'SMART'.
        magic_search( fcode ).

    ENDCASE.

  ENDMETHOD.

  METHOD open_mermaid.

    CHECK zcl_smd_appl=>is_mermaid_active = abap_true.

    TRY.
        IF mo_diagram IS INITIAL.
          CREATE OBJECT mo_diagram TYPE ('ZCL_WD_GUI_MERMAID_JS_DIAGRAM') EXPORTING parent = mo_mm_container
                                                                                    hide_scrollbars = abap_false.
        ENDIF.
        CALL METHOD mo_diagram->('SET_SOURCE_CODE_STRING') EXPORTING source_code = i_mm_string.
        CALL METHOD mo_diagram->('DISPLAY').

      CATCH cx_root INTO DATA(error).
        MESSAGE error TYPE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
