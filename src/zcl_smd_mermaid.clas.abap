CLASS zcl_smd_mermaid DEFINITION PUBLIC INHERITING FROM zcl_smd_popup CREATE PUBLIC.

  PUBLIC SECTION.

    DATA: mo_debugger     TYPE REF TO zcl_smd_debugger_base,
          mo_mm_container TYPE REF TO cl_gui_container,
          mo_mm_toolbar   TYPE REF TO cl_gui_container,
          mo_toolbar      TYPE REF TO cl_gui_toolbar,
          mo_diagram      TYPE REF TO object,
          mv_type         TYPE string,
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

    " Control-structure scheme of the include on display. The former
    " implementation paired IF/ENDIF and counted nesting by hand; the
    " scanner already carries that information, so it is read from there.
    READ TABLE mo_debugger->mo_window->mt_source
      WITH KEY include = mo_debugger->mo_window->m_prg-include INTO DATA(ls_src).
    IF sy-subrc <> 0.
      READ TABLE mo_debugger->mo_window->mt_source INDEX 1 INTO ls_src.
    ENDIF.
    CHECK ls_src-source IS BOUND.

    DATA(lv_mm) = zcl_smd_code_scheme=>build(
      it_source = ls_src-source->lines
      it_kw     = ls_src-t_keytokens
      io_scan   = ls_src-scan
      i_title   = CONV string( ls_src-include ) ).

    IF i_direction = 'TB'.
      REPLACE FIRST OCCURRENCE OF 'flowchart LR' IN lv_mm WITH 'flowchart TD'.
    ENDIF.

    open_mermaid( lv_mm ).
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
          call_line TYPE zcl_smd_window=>ts_calls_line.

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
