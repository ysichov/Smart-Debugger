class ZCL_SMD_WINDOW definition
  public
  inheriting from ZCL_SMD_POPUP
  create public .

public section.

  types:
    BEGIN OF ts_table,
             ref      TYPE REF TO data,
             kind(1),
             value    TYPE string,
             typename TYPE abap_abstypename,
             fullname TYPE string,
           END OF ts_table .
  types:
    BEGIN OF ts_calls,
             event TYPE string,
             type  TYPE string,
             name  TYPE string,
             outer TYPE string,
             inner TYPE string,
           END OF ts_calls .
  types:
    tt_calls TYPE STANDARD TABLE OF ts_calls WITH NON-UNIQUE KEY outer .
  types:
    BEGIN OF ts_calls_line,
             class     TYPE string,
             eventtype TYPE string,
             eventname TYPE string,
             index     TYPE i,
           END OF ts_calls_line .
  types:
    tt_calls_line TYPE STANDARD TABLE OF ts_calls_line WITH NON-UNIQUE EMPTY KEY .
  types:
    BEGIN OF ts_kword,
             index     TYPE i,
             line      TYPE i,
             name      TYPE string,
             from      TYPE i,
             to        TYPE i,
             tt_calls  TYPE tt_calls,
             to_evtype TYPE string,
             to_evname TYPE string,
           END OF ts_kword .
  types:
    BEGIN OF calculated_var ,
             line TYPE i,
             name TYPE string,
           END OF calculated_var .
  types:
    BEGIN OF composed_vars,
             line TYPE i,
             name TYPE string,
           END OF composed_vars .
  types:
    tt_kword      TYPE STANDARD TABLE OF ts_kword WITH EMPTY KEY .
  types:
    tt_calculated TYPE STANDARD TABLE OF calculated_var  WITH EMPTY KEY .
  types:
    tt_composed   TYPE STANDARD TABLE OF composed_vars WITH EMPTY KEY .
  types:
    BEGIN OF ts_params,
             class     TYPE string,
             event     TYPE string,
             name      TYPE string,
             param     TYPE string,
             type      TYPE char1,
             preferred TYPE char1,
           END OF ts_params .
  types:
    tt_params TYPE STANDARD TABLE OF ts_params WITH EMPTY KEY .
  types:
    BEGIN OF ts_int_tabs,
             eventtype TYPE string,
             eventname TYPE string,
             name      TYPE string,
             type      TYPE string,
           END OF ts_int_tabs .
  types:
    tt_tabs TYPE STANDARD TABLE OF ts_int_tabs WITH EMPTY KEY .
  types:
    BEGIN OF ts_progs,
             include       TYPE program,
             source        TYPE REF TO cl_ci_source_include,
             scan          TYPE REF TO cl_ci_scan,
             t_keytokens   TYPE tt_kword,
             t_calculated  TYPE tt_calculated,
             t_composed    TYPE tt_composed,
             t_params      TYPE tt_params,
             tt_tabs       TYPE tt_tabs,
             tt_calls_line TYPE tt_calls_line,
           END OF ts_progs .
  types:
    BEGIN OF ts_locals,
             program    TYPE tpda_program,
             eventtype  TYPE tpda_event_type,
             eventname  TYPE tpda_event,
             loc_fill   TYPE xfeld,
             locals_tab TYPE tpda_scr_locals_it,
             mt_fs      TYPE tpda_scr_locals_it,
           END OF ts_locals .
  types:
    BEGIN OF ts_globals,
             program     TYPE tpda_program,
             glob_fill   TYPE xfeld,
             globals_tab TYPE tpda_scr_globals_it,
             mt_fs       TYPE tpda_scr_locals_it,
           END OF ts_globals .
  types:
    BEGIN OF ts_watch,
             program TYPE string,
             line    TYPE i,
           END OF ts_watch .
  types:
    tt_watch TYPE STANDARD  TABLE OF ts_watch WITH EMPTY KEY .
  types:
    BEGIN OF ts_bpoint,
             program TYPE string,
             include TYPE string,
             line    TYPE i,
             type    TYPE char1,
             del     TYPE char1,
           END OF ts_bpoint .
  types:
    tt_bpoints TYPE STANDARD TABLE OF ts_bpoint WITH EMPTY KEY .
  types:
    tt_table TYPE STANDARD TABLE OF ts_table
          WITH NON-UNIQUE DEFAULT KEY .

  data M_VERSION type X .
  data M_HISTORY type X .
  data M_VISUALIZATION type X .
  data M_VARHIST type X .
  data M_ZCODE type X .
  data M_DIRECTION type X .
  data M_PRG type TPDA_SCR_PRG_INFO .
  data M_DEBUG_BUTTON like SY-UCOMM .
  data M_SHOW_STEP type XFELD .
  data MT_BPOINTS type TT_BPOINTS .
  data MO_DEBUGGER type ref to ZCL_SMD_DEBUGGER_SCRIPT .
  data MO_SPLITTER_CODE type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_SPLITTER_VAR type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_SPLITTER_STEPS type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_TOOLBAR_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_IMPORTING_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_LOCALS_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_EXPORTING_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_CODE_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_IMP_EXP_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_EDITOR_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_STEPS_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_STACK_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_HIST_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_CODE_VIEWER type ref to CL_GUI_ABAPEDIT .
  data:
    mt_stack               TYPE TABLE OF zcl_smd_appl=>t_stack .
  data MO_TOOLBAR type ref to CL_GUI_TOOLBAR .
  data MO_SALV_STACK type ref to CL_SALV_TABLE .
  data MO_SALV_STEPS type ref to CL_SALV_TABLE .
  data MO_SALV_HIST type ref to CL_SALV_TABLE .
  data MT_BREAKS type TPDA_BP_PERSISTENT_IT .
  data MT_WATCH type TT_WATCH .
  data MT_COVERAGE type TT_WATCH .
  data M_HIST_DEPTH type I .
  data M_START_STACK type I .
  data:
    mt_source              TYPE STANDARD  TABLE OF ts_progs .
  data:
    mt_params              TYPE STANDARD  TABLE OF ts_params .
  data:
    mt_locals_set          TYPE STANDARD TABLE OF ts_locals .
  data:
    mt_globals_set         TYPE STANDARD TABLE OF ts_globals .

  methods CONSTRUCTOR
    importing
      !I_DEBUGGER type ref to ZCL_SMD_DEBUGGER_SCRIPT
      !I_ADDITIONAL_NAME type STRING optional .
  methods ADD_TOOLBAR_BUTTONS .
  methods HND_TOOLBAR
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
    importing
      !FCODE .
  methods ON_STACK_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_SALV_EVENTS_TABLE
    importing
      !COLUMN
      !ROW .
  methods SET_PROGRAM
    importing
      !I_PROGRAM type PROGRAM .
  methods SHOW_COVERAGE .
  methods ON_EDITOR_DOUBLE_CLICK
    for event DBLCLICK of CL_GUI_ABAPEDIT
    importing
      !SENDER .
  methods ON_EDITOR_BORDER_CLICK
    for event BORDER_CLICK of CL_GUI_ABAPEDIT
    importing
      !CNTRL_PRESSED_SET
      !LINE
      !SHIFT_PRESSED_SET .
  methods SET_PROGRAM_LINE
    importing
      !I_LINE like SY-INDEX optional .
  methods CREATE_CODE_VIEWER .
  methods SHOW_STACK .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SMD_WINDOW IMPLEMENTATION.


  method ADD_TOOLBAR_BUTTONS.


    DATA: button TYPE ttb_button,
          events TYPE cntl_simple_events,
          event  LIKE LINE OF events.

    button  = VALUE #(
     "( function = 'VIS'  icon = CONV #( icon_flight ) quickinfo = 'Visualization switch' text = 'Visualization OFF' )
     "( function = 'HIST' icon = CONV #( icon_graduate ) quickinfo = 'Stack History switch' text = 'History On' )
     "( function = 'VARHIST' icon = CONV #( icon_graduate ) quickinfo = 'Variables History switch' text = 'Vars History On' )
     "( function = 'AI' icon = CONV #( icon_manikin_unknown_gender ) quickinfo = 'Ask AI' text = 'Ask AI' )
     ( function = 'F5' icon = CONV #( icon_debugger_step_into ) quickinfo = 'Step into' text = 'Step into' )
     ( function = 'F6' icon = CONV #( icon_debugger_step_over ) quickinfo = 'Step over' text = 'Step over' )
     ( function = 'F7' icon = CONV #( icon_debugger_step_out ) quickinfo = 'Step out' text = 'Step out' )
     ( function = 'F8' icon = CONV #( icon_debugger_continue ) quickinfo = 'to the next Breakpoint' text = 'Continue' )
     ( function = 'DIRECTION' icon = CONV #( icon_column_right ) quickinfo = 'Forward' text = 'Forward' )
     ( butn_type = 3  )
     ( function = 'DEPTH' icon = CONV #( icon_next_hierarchy_level ) quickinfo = 'History depth level' text = |Depth { m_hist_depth }| )
     ( function = 'CODE' icon = CONV #( icon_customer_warehouse ) quickinfo = 'Only Z' text = 'Only Z' )
     ( function = 'CLEARVAR' icon = CONV #( icon_select_detail ) quickinfo = 'Clear all selected variables' text = 'Clear vars' )
     ( butn_type = 3  )
     ( COND #( WHEN zcl_smd_appl=>is_mermaid_active = abap_true
      THEN VALUE #( function = 'DIAGRAM' icon = CONV #( icon_workflow_process ) quickinfo = ' Calls Flow' text = 'Diagram' ) ) )
     ( function = 'SMART' icon = CONV #( icon_wizard ) quickinfo = 'Calculations sequence' text = 'Calculations Flow' )
     ( function = 'COVERAGE' icon = CONV #( icon_wizard ) quickinfo = 'Coverage ' text = 'Coverage' )
     ( butn_type = 3  )
     ( function = 'STEPS' icon = CONV #( icon_next_step ) quickinfo = 'Steps table' text = 'Steps' )
     ( function = 'HISTORY' icon = CONV #( icon_history ) quickinfo = 'History table' text = 'History' )
     ( butn_type = 3  )
     ( function = 'ENGINE' icon = CONV #( icon_graduate ) quickinfo = 'Faster version but can skip some changes' text = 'Alpha' )
     ( function = 'DEBUG' icon = CONV #( icon_tools ) quickinfo = 'Debug' text = 'Debug' )
     ( function = 'INFO' icon = CONV #( icon_bw_gis ) quickinfo = 'Documentation' text = '' )
                    ).

    mo_toolbar->add_button_group( button ).

*   Register events
    event-eventid = cl_gui_toolbar=>m_id_function_selected.
    event-appl_event = space.
    APPEND event TO events.

    mo_toolbar->set_registered_events( events = events ).
    SET HANDLER me->hnd_toolbar FOR mo_toolbar.


  endmethod.


  method CONSTRUCTOR.

    super->constructor( ).
    mo_debugger = i_debugger.
    m_history = m_varhist =  m_zcode  = '01'.
    m_hist_depth = 9.

    mo_box = create( i_name = 'SDDE Simple Debugger Data Explorer beta v. 0.9' i_width = 1400 i_hight = 400 ).
    SET HANDLER on_box_close FOR mo_box.
    CREATE OBJECT mo_splitter
      EXPORTING
        parent  = mo_box
        rows    = 3
        columns = 1
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter->get_container(
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = mo_code_container ).

    mo_splitter->get_container(
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = mo_toolbar_container ).

    mo_splitter->set_row_height( id = 1 height = '3' ).
    mo_splitter->set_row_height( id = 2 height = '70' ).

    mo_splitter->set_row_sash( id    = 1
                               type  = 0
                               value = 0 ).

    mo_splitter->get_container(
      EXPORTING
        row       = 3
        column    = 1
      RECEIVING
        container = mo_tables_container ).

    mo_splitter->get_container(
      EXPORTING
        row       = 3
        column    = 1
      RECEIVING
        container = mo_tables_container ).

    CREATE OBJECT mo_splitter_code
      EXPORTING
        parent  = mo_code_container
        rows    = 1
        columns = 2
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter_code->get_container(
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = mo_editor_container ).

    mo_splitter_code->get_container(
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = mo_variables_container ).

    mo_splitter_code->set_column_width( EXPORTING id = 1 width = '60' ).

    CREATE OBJECT mo_splitter_var
      EXPORTING
        parent  = mo_variables_container
        rows    = 2
        columns = 1
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter_var->set_row_height( id = 1 height = '66' ).

    mo_splitter_var->get_container(
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = mo_locals_container ).

    mo_splitter_var->get_container(
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = mo_imp_exp_container ).

    CREATE OBJECT mo_splitter_imp_exp
      EXPORTING
        parent  = mo_imp_exp_container
        rows    = 1
        columns = 2
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter_imp_exp->get_container(
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = mo_importing_container ).

    mo_splitter_imp_exp->get_container(
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = mo_exporting_container ).

    SET HANDLER on_box_close FOR mo_box.

    CREATE OBJECT mo_toolbar EXPORTING parent = mo_toolbar_container.
    add_toolbar_buttons( ).
    mo_toolbar->set_visible( 'X' ).
    create_code_viewer( ).


  endmethod.


  method CREATE_CODE_VIEWER.


    DATA: events TYPE cntl_simple_events,
          event  TYPE cntl_simple_event.

    CHECK mo_code_viewer IS INITIAL.

    CREATE OBJECT mo_code_viewer
      EXPORTING
        parent           = mo_editor_container
        max_number_chars = 100.

    mo_code_viewer->init_completer( ).
    mo_code_viewer->upload_properties(
      EXCEPTIONS
        dp_error_create  = 1
        dp_error_general = 2
        dp_error_send    = 3
        OTHERS           = 4 ).

    "DATA(o_handler) = NEW zcl_smd_event_handler( mo_debugger ).

    event-eventid    = cl_gui_textedit=>event_double_click.
    APPEND event TO events.

    mo_code_viewer->set_registered_events( events ).
    mo_code_viewer->register_event_border_click( ).
    mo_code_viewer->register_event_break_changed( ).

    SET HANDLER on_editor_double_click FOR mo_code_viewer.
    SET HANDLER on_editor_border_click FOR mo_code_viewer.

    mo_code_viewer->set_statusbar_mode( statusbar_mode = cl_gui_abapedit=>true ).
    mo_code_viewer->create_document( ).
    mo_code_viewer->set_readonly_mode( 1 ).


  endmethod.


  method HND_TOOLBAR.


    CONSTANTS: c_mask TYPE x VALUE '01'.
    FIELD-SYMBOLS: <any> TYPE any.
    m_debug_button = fcode.
    READ TABLE mt_stack INDEX 1 INTO DATA(stack).
    CASE fcode.

      WHEN 'AI'.

*        READ TABLE mo_debugger->mo_window->mt_source INDEX 1 INTO DATA(source).
*        NEW zcl_smd_ai( io_source = source-source io_parent =  mo_debugger->mo_window->mo_box ).

      WHEN 'ENGINE'.
        m_version = m_version BIT-XOR c_mask.
        IF m_version IS INITIAL.
          mo_toolbar->set_button_info( EXPORTING fcode =  'ENGINE'  text = 'Alpha' quickinfo = 'Faster version' ).
        ELSE.
          mo_toolbar->set_button_info( EXPORTING fcode =  'ENGINE'  text = 'Beta' quickinfo = 'Slower version' ).
        ENDIF.
      WHEN 'VIS'.
        m_visualization = m_visualization BIT-XOR c_mask.
        IF m_visualization IS INITIAL.
          mo_toolbar->set_button_info( EXPORTING fcode =  'VIS' icon = CONV #( icon_flight ) text = 'Visualization OFF' ).
        ELSE.
          mo_toolbar->set_button_info( EXPORTING fcode =  'VIS' icon = CONV #( icon_car ) text = 'Visualization ON' ).
        ENDIF.

      WHEN 'DIRECTION'.
        m_direction = m_direction BIT-XOR c_mask.
        IF m_direction IS INITIAL.
          mo_toolbar->set_button_info( EXPORTING fcode =  'DIRECTION' icon = CONV #( icon_column_right ) text = 'Forward' quickinfo = 'Forward' ).
          mo_toolbar->set_button_info( EXPORTING fcode = 'F5' text = 'Step into' quickinfo = 'Step into' ).
          mo_toolbar->set_button_info( EXPORTING fcode = 'F8' text = 'Continue' quickinfo = 'to the next Breakpoint' ).
          mo_toolbar->set_button_visible( visible = abap_true fcode = 'F6' ).
          mo_toolbar->set_button_visible( visible = abap_true fcode = 'F7' ).
        ELSE.
          mo_toolbar->set_button_info( EXPORTING fcode =  'DIRECTION' icon = CONV #( icon_column_left ) text = 'Backward' quickinfo = 'Backward' ).
          mo_toolbar->set_button_info( EXPORTING fcode = 'F5' text = 'Step back' quickinfo = 'Step back' ).
          mo_toolbar->set_button_info( EXPORTING fcode = 'F8' text = 'to the previous stop condition' ).
          mo_toolbar->set_button_visible( visible = abap_false fcode = 'F6' ).
          mo_toolbar->set_button_visible( visible = abap_false fcode = 'F7' ).

        ENDIF.

      WHEN 'DEPTH'.
        IF m_hist_depth < 9.
          ADD 1 TO m_hist_depth.
        ELSE.
          CLEAR m_hist_depth.
        ENDIF.
        mo_toolbar->set_button_info( EXPORTING fcode = 'DEPTH' text = |Depth { m_hist_depth }| ).

      WHEN 'HIST'.
        m_history = m_history BIT-XOR c_mask.
        IF m_history IS INITIAL.
          mo_toolbar->set_button_info( EXPORTING fcode =  'HIST' icon = CONV #( icon_red_xcircle ) text = 'History OFF' ).
        ELSE.
          mo_toolbar->set_button_info( EXPORTING fcode =  'HIST' icon = CONV #( icon_graduate ) text = 'History ON' ).
        ENDIF.

      WHEN 'VARHIST'.
        m_varhist = m_varhist BIT-XOR c_mask.
        IF m_varhist IS INITIAL.
          mo_toolbar->set_button_info( EXPORTING fcode =  'VARHIST' icon = CONV #( icon_red_xcircle ) text = 'Vars History OFF' ).
        ELSE.
          mo_toolbar->set_button_info( EXPORTING fcode =  'VARHIST' icon = CONV #( icon_graduate ) text = 'Vars History ON' ).
        ENDIF.

      WHEN 'DIAGRAM'.
        DATA(o_mermaid) = NEW zcl_smd_mermaid( io_debugger = mo_debugger i_type =  'DIAG' ).

      WHEN 'SMART'.
        o_mermaid = NEW zcl_smd_mermaid( io_debugger = mo_debugger i_type =  'SMART' ).
        mo_debugger->show_step( ).

      WHEN 'COVERAGE'.
        show_coverage( ).
        mo_debugger->show_step( ).

      WHEN 'CODE'.
        m_zcode = m_zcode BIT-XOR c_mask.
        IF m_zcode IS INITIAL.
          mo_toolbar->set_button_info( EXPORTING fcode = 'CODE' text = 'Z & Standard' ).
        ELSE.
          mo_toolbar->set_button_info( EXPORTING fcode = 'CODE' text = 'Only Z code' ).
        ENDIF.

      WHEN 'CLEARVAR'.
        CLEAR: mo_debugger->mt_selected_var.

        DATA(nodes) = mo_debugger->mo_tree_local->m_tree->get_nodes( )->get_all_nodes( ).
        LOOP AT nodes INTO DATA(node).
          node-node->set_row_style( if_salv_c_tree_style=>default ).
        ENDLOOP.
        mo_debugger->run_script_hist( mo_debugger->m_hist_step ).
        mo_debugger->mo_tree_local->display( ).
        RETURN.
      WHEN 'DEBUG'."activate break_points
        mo_debugger->m_debug = mo_debugger->m_debug BIT-XOR c_mask.

      WHEN 'INFO'.
        DATA(url) = 'https://ysychov.wordpress.com/2020/07/27/abap-simple-debugger-data-explorer/'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = url.

        url = 'https://github.com/ysichov/Smart-Debugger'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = url.


      WHEN 'STEPS'.

        zcl_smd_appl=>open_int_table( i_name = 'Steps' it_tab = mo_debugger->mt_steps io_window = mo_debugger->mo_window ).

      WHEN 'HISTORY'.
        DATA: vars_hist TYPE TABLE OF zcl_smd_appl=>var_table_temp.
        LOOP AT  mo_debugger->mt_vars_hist INTO DATA(vars).
          APPEND INITIAL LINE TO vars_hist ASSIGNING FIELD-SYMBOL(<hist>).
          MOVE-CORRESPONDING vars TO <hist>.

          IF vars-ref IS BOUND.
            DATA(o_descr) = cl_abap_typedescr=>describe_by_data_ref( vars-ref ).

            IF o_descr->type_kind = cl_abap_typedescr=>typekind_table.
              <hist>-value = 'Table'.
            ELSEIF o_descr->type_kind = cl_abap_typedescr=>typekind_struct1."structure
              <hist>-value = 'Structure'.
            ELSEIF o_descr->type_kind = cl_abap_typedescr=>typekind_struct2."deep structure
              <hist>-value = 'Deep Structure'.
            ELSE.
              ASSIGN vars-ref->* TO <any>.
              IF sy-subrc = 0.
                <hist>-value = <any>.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
        zcl_smd_appl=>open_int_table( i_name = |mt_vars_hist - History({ lines( vars_hist ) })| it_tab = vars_hist io_window = mo_debugger->mo_window ).

    ENDCASE.

    IF m_direction IS INITIAL AND mo_debugger->m_hist_step = mo_debugger->m_step.
      IF fcode = 'F8'.
        m_start_stack = stack-stacklevel.

      ENDIF.
      CASE fcode.
        WHEN 'F5' OR 'F6' OR 'F6END' OR 'F6BEG' OR 'F7' OR 'F8'.

          IF fcode = 'F7'.
            mo_debugger->m_target_stack = stack-stacklevel - 1.
          ENDIF.

          mo_debugger->make_step( ).
      ENDCASE.

    ELSE.
      CASE fcode.

        WHEN 'F5' OR 'F6' OR 'F7' OR 'F8' OR 'F6BEG' OR 'F6END'.
          DO.
            mo_debugger->run_script_hist( IMPORTING es_stop = DATA(stop) ).

            IF stop = abap_true.
              READ TABLE  mo_debugger->mt_steps INTO DATA(step) INDEX mo_debugger->m_hist_step.
              set_program( step-include ).
              set_program_line( step-line ).
              mo_debugger->mo_tree_imp->display( ).
              mo_debugger->mo_tree_local->display( ).
              mo_debugger->mo_tree_exp->display( ).
              RETURN.
            ENDIF.
          ENDDO.
      ENDCASE.
    ENDIF.


  endmethod.


  method ON_EDITOR_BORDER_CLICK.


    DATA: type    TYPE char1.

    IF cntrl_pressed_set IS INITIAL.
      type = 'S'.
    ELSE.
      type = 'E'.
    ENDIF.

    LOOP AT mt_bpoints ASSIGNING FIELD-SYMBOL(<point>) WHERE line = line.
      type = <point>-type.

      CALL FUNCTION 'RS_DELETE_BREAKPOINT'
        EXPORTING
          index        = line
          mainprog     = m_prg-program
          program      = m_prg-include
          bp_type      = type
        EXCEPTIONS
          not_executed = 1
          OTHERS       = 2.

      IF sy-subrc = 0.
        <point>-del = abap_true.
      ENDIF.
    ENDLOOP.

    IF sy-subrc <> 0. "create
      CALL FUNCTION 'RS_SET_BREAKPOINT'
        EXPORTING
          index        = line
          program      = m_prg-include
          mainprogram  = m_prg-program
          bp_type      = type
        EXCEPTIONS
          not_executed = 1
          OTHERS       = 2.

    ENDIF.
    DELETE mt_bpoints WHERE del IS NOT INITIAL.
    set_program_line( ).

  endmethod.


  method ON_EDITOR_DOUBLE_CLICK.

    sender->get_selection_pos( IMPORTING from_line = DATA(fr_line) from_pos = DATA(fr_pos) to_line = DATA(to_line) to_pos = DATA(to_pos) ).


  endmethod.


  method ON_STACK_DOUBLE_CLICK.


    READ TABLE mo_debugger->mo_window->mt_stack INDEX row INTO DATA(stack).
    "only for coverage stack selection should work.

    CHECK mo_debugger->mo_window->mt_coverage IS NOT INITIAL.

    "check if we have recorded steps for choosen stack level
    READ TABLE  mo_debugger->mt_steps WITH KEY program = stack-program include = stack-include TRANSPORTING NO FIELDS.
    CHECK sy-subrc = 0.

    MOVE-CORRESPONDING stack TO mo_debugger->mo_window->m_prg.
    MOVE-CORRESPONDING stack TO mo_debugger->ms_stack.

    show_coverage( ).
    mo_debugger->show_step( ).


  endmethod.


  method SET_PROGRAM.


    zcl_smd_source_parser=>parse_tokens( i_program = i_program io_debugger = mo_debugger ).
    READ TABLE mt_source WITH KEY include = i_program INTO DATA(source).
    IF sy-subrc = 0.
      mo_code_viewer->set_text( table = source-source->lines ).
    ENDIF.


  endmethod.


  method SET_PROGRAM_LINE.


    TYPES: lntab TYPE STANDARD TABLE OF i.
    DATA lines TYPE lntab.

    mo_code_viewer->remove_all_marker( 2 ).
    mo_code_viewer->remove_all_marker( 4 ).

*    "session breakpoints
    CALL METHOD cl_abap_debugger=>read_breakpoints
      EXPORTING
        main_program         = mo_debugger->mo_window->m_prg-include
      IMPORTING
        breakpoints_complete = DATA(points)
      EXCEPTIONS
        c_call_error         = 1
        generate             = 2
        wrong_parameters     = 3
        OTHERS               = 4.

    LOOP AT points INTO DATA(point). "WHERE inclnamesrc = m_prg-include.
      APPEND INITIAL LINE TO lines ASSIGNING FIELD-SYMBOL(<line>).
      <line> = point-line.

      APPEND INITIAL LINE TO mt_bpoints ASSIGNING FIELD-SYMBOL(<point>).
      MOVE-CORRESPONDING point TO <point>.
      <point>-type = 'S'.
    ENDLOOP.
    mo_code_viewer->set_marker( EXPORTING marker_number = 2 marker_lines = lines ).

*    "exernal breakpoints
    CALL METHOD cl_abap_debugger=>read_breakpoints
      EXPORTING
        main_program         = mo_debugger->mo_window->m_prg-include
        flag_other_session   = abap_true
      IMPORTING
        breakpoints_complete = points
      EXCEPTIONS
        c_call_error         = 1
        generate             = 2
        wrong_parameters     = 3
        OTHERS               = 4.

    "blue arrow - current line
    APPEND INITIAL LINE TO lines ASSIGNING <line>.
    <line> = i_line.
    mo_code_viewer->set_marker( EXPORTING marker_number = 7 marker_lines = lines ).

    CLEAR lines.

    LOOP AT points INTO point. "WHERE inclnamesrc = m_prg-include.
      APPEND INITIAL LINE TO lines ASSIGNING <line>.
      <line> = point-line.

      APPEND INITIAL LINE TO mt_bpoints ASSIGNING <point>.
      MOVE-CORRESPONDING point TO <point>.
      <point>-type = 'E'.
    ENDLOOP.
    mo_code_viewer->set_marker( EXPORTING marker_number = 4 marker_lines = lines ).

    "watchpoints or coverage
    CLEAR lines.
    LOOP AT mt_watch INTO DATA(watch).
      APPEND INITIAL LINE TO lines ASSIGNING <line>.
      <line> = watch-line.
    ENDLOOP.

    "coverage
    LOOP AT mt_coverage INTO DATA(coverage).
      APPEND INITIAL LINE TO lines ASSIGNING <line>.
      <line> = coverage-line.
    ENDLOOP.

    mo_code_viewer->set_marker( EXPORTING marker_number = 2 marker_lines = lines ).

    IF i_line IS NOT INITIAL.
      mo_code_viewer->select_lines( EXPORTING from_line = i_line to_line = i_line ).
    ENDIF.

    mo_code_viewer->clear_line_markers( 'S' ).
    mo_code_viewer->draw( ).


  endmethod.


  method SHOW_COVERAGE.


    CLEAR: mt_watch, mt_coverage,mt_stack.
    LOOP AT mo_debugger->mt_steps INTO DATA(step).

      READ TABLE mt_stack WITH KEY include = step-include TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO mt_stack ASSIGNING FIELD-SYMBOL(<stack>).
        MOVE-CORRESPONDING step TO <stack>.
      ENDIF.

      IF step-include <> mo_debugger->mo_window->m_prg-include.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO mt_coverage ASSIGNING FIELD-SYMBOL(<coverage>).
      <coverage>-line = step-line.
    ENDLOOP.

    SORT mt_coverage.
    DELETE ADJACENT DUPLICATES FROM mt_coverage.


  endmethod.


  method SHOW_STACK.

    IF mo_salv_stack IS INITIAL.

      cl_salv_table=>factory(
        EXPORTING
          r_container  = mo_tables_container
        IMPORTING
          r_salv_table = mo_salv_stack
        CHANGING
          t_table      = mt_stack ).

      DATA:  o_column  TYPE REF TO cl_salv_column.

      DATA(o_columns) = mo_salv_stack->get_columns( ).
      "o_columns->set_optimize( 'X' ).

      o_column ?= o_columns->get_column( 'STEP' ).
      o_column->set_output_length( '3' ).
      o_column->set_short_text( 'STEP' ).

      "o_column ?= o_columns->get_column( 'STACKPOINTER' ).
      "o_column->set_output_length( '5' ).

      o_column ?= o_columns->get_column( 'STACKLEVEL' ).
      o_column->set_output_length( '5' ).

      o_column ?= o_columns->get_column( 'PROGRAM' ).
      o_column->set_output_length( '30' ).

      o_column ?= o_columns->get_column( 'INCLUDE' ).
      o_column->set_output_length( '40' ).

      o_column ?= o_columns->get_column( 'EVENTTYPE' ).
      o_column->set_output_length( '20' ).

      o_column ?= o_columns->get_column( 'EVENTNAME' ).
      o_column->set_output_length( '50' ).

      DATA(o_event) =  mo_salv_stack->get_event( ).

      SET HANDLER on_stack_double_click FOR o_event.

      mo_salv_stack->display( ).
    ELSE.
      mo_salv_stack->refresh( ).
    ENDIF.


  endmethod.
ENDCLASS.
