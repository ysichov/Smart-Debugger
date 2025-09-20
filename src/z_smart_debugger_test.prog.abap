*<SCRIPT:PERSISTENT>
REPORT  rstpda_script_template.

*<SCRIPT:HEADER>
*<SCRIPTNAME>Z_SMART_DEBUGGER_TEST</SCRIPTNAME>
*<SCRIPT_CLASS>LCL_DEBUGGER_SCRIPT</SCRIPT_CLASS>
*<SCRIPT_COMMENT>Debugger Skript: Default Template</SCRIPT_COMMENT>
*<SINGLE_STEP>X</SINGLE_STEP>

*</SCRIPT:HEADER>

*<SCRIPT:PRESETTINGS>

*</SCRIPT:PRESETTINGS>

*<SCRIPT:SCRIPT_CLASS>

*  & Smart  Debugger (Project ARIADNA - Advanced Reverse Ingeneering Abap Debugger with New Analytycs )
*  & Multi-windows program for viewing all objects and data structures in debug
*  &---------------------------------------------------------------------*
*  & version: beta 0.9.600
*  & Git https://github.com/ysichov/SDDE
*  & RU description - https://ysychov.wordpress.com/2020/07/27/abap-simple-debugger-data-explorer/
*  & EN description - https://github.com/ysichov/SDDE/wiki

*  & Written by Yurii Sychov
*  & e-mail:   ysichov@gmail.com
*  & blog:     https://ysychov.wordpress.com/blog/
*  & LinkedIn: https://www.linkedin.com/in/ysychov/
*  &---------------------------------------------------------------------*

*  & External resources
*  & https://github.com/WegnerDan/abapMermaid
*  & https://github.com/ysichov/abapMermaid - should be used this fork with scroll enabled
*  & https://gist.github.com/AtomKrieg/7f4ec2e2f49b82def162e85904b7e25b - data object visualizer

*  & Inspired by
*  & https://habr.com/ru/articles/504908/
*  & https://github.com/larshp/ABAP-Object-Visualizer - Abap Object Visualizer
*  & https://github.com/ysichov/SDE_abapgit - Simple Data Explorer

CLASS lcl_ai DEFINITION DEFERRED.
CLASS lcl_data_receiver DEFINITION DEFERRED.
CLASS lcl_data_transmitter DEFINITION DEFERRED.
CLASS lcl_rtti_tree DEFINITION DEFERRED.
CLASS lcl_window DEFINITION DEFERRED.
CLASS lcl_table_viewer DEFINITION DEFERRED.
CLASS lcl_mermaid DEFINITION DEFERRED.

CLASS lcl_box_handler DEFINITION."for memory clearing

  PUBLIC SECTION.
    METHODS: on_box_close FOR EVENT close OF cl_gui_dialogbox_container IMPORTING sender.

ENDCLASS.

CLASS lcl_appl DEFINITION.

  PUBLIC SECTION.
    TYPES: BEGIN OF sign_option_icon_s,
             sign          TYPE tvarv_sign,
             option        TYPE tvarv_opti,
             icon_name(64) TYPE c,
             icon          TYPE aqadh_type_of_icon,
           END OF sign_option_icon_s,

           BEGIN OF var_table,
             step          TYPE i,
             stack         TYPE i,
             program(40)   TYPE c,
             eventtype(30) TYPE c,
             eventname(61) TYPE c,
             first         TYPE xfeld,
             is_appear     TYPE xfeld,
             del           TYPE xfeld,
             leaf          TYPE string,
             name(1000)               ,
             path          TYPE string,
             short         TYPE string,
             key           TYPE salv_de_node_key,
             parent        TYPE string,
             cl_leaf       TYPE int4,
             ref           TYPE REF TO data,
             type          TYPE string,
             instance      TYPE string,
             objname       TYPE string,
             done          TYPE xfeld,
           END OF var_table,

           t_var_table TYPE STANDARD TABLE OF var_table WITH NON-UNIQUE DEFAULT KEY,

           BEGIN OF var_table_temp,
             step          TYPE i,
             stack         TYPE i,
             eventtype(30) TYPE c,
             eventname(61) TYPE c,
             name          TYPE string,
             value         TYPE string,
             first         TYPE xfeld,
             is_appear     TYPE xfeld,
             del           TYPE xfeld,
             program(40)   TYPE c,
             leaf          TYPE string,
             path          TYPE string,
             type          TYPE string,
             instance      TYPE string,
             objname       TYPE string,
             ref           TYPE REF TO data,
           END OF var_table_temp,

           BEGIN OF var_table_h,
             step          TYPE i,
             program(40)   TYPE c,
             eventtype(30) TYPE c,
             eventname(61) TYPE c,
             leaf          TYPE string,
             name          TYPE string,
             path          TYPE string,
             parent        TYPE string,
             short         TYPE string,
             cl_leaf       TYPE int4,  "?
             ref           TYPE REF TO data,
             tree          TYPE REF TO lcl_rtti_tree,
             time          LIKE sy-uname,
           END OF var_table_h,

           BEGIN OF t_obj,
             name       TYPE string,
             alv_viewer TYPE REF TO lcl_table_viewer,
           END OF t_obj,

           BEGIN OF t_classes_types,
             name TYPE string,
             full TYPE string,
             type TYPE char1,
             key  TYPE salv_de_node_key,
           END OF t_classes_types,

           BEGIN OF t_lang,
             spras(4),
             sptxt    TYPE sptxt,
           END OF t_lang,

           BEGIN OF t_stack,
             step       TYPE i,
             "stackpointer TYPE tpda_stack_pointer,
             stacklevel TYPE tpda_stack_level,
             line       TYPE tpda_sc_line,
             eventtype  TYPE tpda_event_type,
             eventname  TYPE tpda_event,
             program    TYPE tpda_program,
             include    TYPE tpda_include,
           END OF t_stack,

           BEGIN OF t_step_counter,
             step       TYPE i,
             stacklevel TYPE tpda_stack_level,
             line       TYPE tpda_sc_line,
             eventtype  TYPE tpda_event_type,
             eventname  TYPE tpda_event,
             first      TYPE xfeld,
             last       TYPE xfeld,
             program    TYPE tpda_program,
             include    TYPE tpda_include,
             time       LIKE sy-uzeit,
           END OF t_step_counter.

    CLASS-DATA: m_option_icons     TYPE TABLE OF sign_option_icon_s,
                mt_lang            TYPE TABLE OF t_lang,
                mt_obj             TYPE TABLE OF t_obj, "main object table
                m_ctrl_box_handler TYPE REF TO lcl_box_handler,
                c_dragdropalv      TYPE REF TO cl_dragdrop,
                is_mermaid_active  TYPE xfeld.

    CLASS-METHODS:
      init_icons_table,
      init_lang,
      check_mermaid,
      open_int_table IMPORTING it_tab    TYPE ANY TABLE OPTIONAL
                               it_ref    TYPE REF TO data OPTIONAL
                               iv_name   TYPE string
                               io_window TYPE REF TO lcl_window.

ENDCLASS.

CLASS lcl_popup DEFINITION.

  PUBLIC SECTION.
    CLASS-DATA m_counter              TYPE i.
    DATA: m_additional_name      TYPE string,
          mo_box                 TYPE REF TO cl_gui_dialogbox_container,
          mo_splitter            TYPE REF TO cl_gui_splitter_container,
          mo_splitter_imp_exp    TYPE REF TO cl_gui_splitter_container,
          mo_variables_container TYPE REF TO cl_gui_container,
          mo_tables_container    TYPE REF TO cl_gui_container.

    METHODS: constructor IMPORTING i_additional_name TYPE string OPTIONAL,
      create IMPORTING i_width       TYPE i
                       i_hight       TYPE i
                       i_name        TYPE text100 OPTIONAL
             RETURNING VALUE(ro_box) TYPE REF TO cl_gui_dialogbox_container,
      on_box_close FOR EVENT close OF cl_gui_dialogbox_container IMPORTING sender.

ENDCLASS.


CLASS lcl_popup IMPLEMENTATION.

  METHOD constructor.
    m_additional_name = i_additional_name.

  ENDMETHOD.

  METHOD create.

    DATA: l_top  TYPE i,
          l_left TYPE i.

    ADD 1 TO m_counter.
    l_top  = l_left = 1 + 2 * ( m_counter DIV 5 ) +  ( m_counter MOD 5 ) * 10.

    CREATE OBJECT ro_box
      EXPORTING
        width                       = i_width
        height                      = i_hight
        top                         = l_top
        left                        = l_left
        caption                     = i_name
        lifetime                    = 2
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        event_already_registered    = 6
        error_regist_event          = 7
        OTHERS                      = 8.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD on_box_close.
    sender->free( ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_ddic DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS: get_text_table IMPORTING i_tname TYPE tabname
                                  EXPORTING e_tab   TYPE tabname.
ENDCLASS.

CLASS lcl_ddic IMPLEMENTATION.

  METHOD get_text_table.
    CALL FUNCTION 'DDUT_TEXTTABLE_GET'
      EXPORTING
        tabname   = i_tname
      IMPORTING
        texttable = e_tab.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_dd_data DEFINITION."drag&drop data

  PUBLIC  SECTION.
    DATA: m_row    TYPE i,
          m_column TYPE lvc_s_col.

ENDCLASS.

CLASS lcl_dragdrop DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      drag FOR EVENT ondrag OF cl_gui_alv_grid IMPORTING e_dragdropobj e_row e_column ,
      drop FOR EVENT ondrop OF cl_gui_alv_grid IMPORTING e_dragdropobj e_row.

ENDCLASS.

CLASS lcl_alv_common DEFINITION.

  PUBLIC SECTION.
    CONSTANTS: c_white(4) TYPE x VALUE '00000001'. "white background

    CLASS-METHODS:
      refresh IMPORTING i_obj TYPE REF TO cl_gui_alv_grid i_layout TYPE lvc_s_layo OPTIONAL i_soft TYPE char01 OPTIONAL,
      translate_field IMPORTING i_lang TYPE ddlanguage OPTIONAL CHANGING c_fld TYPE lvc_s_fcat,
      get_selected IMPORTING i_obj TYPE REF TO cl_gui_alv_grid RETURNING VALUE(e_index) TYPE i.

ENDCLASS.

CLASS lcl_alv_common IMPLEMENTATION.

  METHOD refresh.

    DATA l_stable TYPE lvc_s_stbl.
    l_stable = 'XX'.
    IF i_layout IS SUPPLIED.
      i_obj->set_frontend_layout( i_layout ).
    ENDIF.
    i_obj->refresh_table_display( EXPORTING is_stable = l_stable i_soft_refresh = i_soft ).

  ENDMETHOD.

  METHOD translate_field.

    DATA: lt_field_info TYPE TABLE OF dfies.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = c_fld-tabname
        fieldname      = c_fld-fieldname
        langu          = i_lang
      TABLES
        dfies_tab      = lt_field_info
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.

    IF sy-subrc = 0.
      READ TABLE lt_field_info INDEX 1 INTO DATA(l_info).
      IF l_info-scrtext_l IS INITIAL AND l_info-scrtext_m IS INITIAL AND l_info-scrtext_s IS INITIAL.
        IF l_info-fieldtext IS NOT INITIAL.
          MOVE l_info-fieldtext TO: c_fld-reptext, c_fld-scrtext_l, c_fld-scrtext_m, c_fld-scrtext_s .
        ELSE.
          MOVE l_info-fieldname TO: c_fld-reptext, c_fld-scrtext_l, c_fld-scrtext_m, c_fld-scrtext_s .
        ENDIF.
      ELSE.
        c_fld-scrtext_l = l_info-scrtext_l.
        c_fld-scrtext_m = l_info-scrtext_m.
        c_fld-scrtext_s = l_info-scrtext_s.
        IF l_info-reptext IS NOT INITIAL.
          c_fld-reptext   = l_info-reptext.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD get_selected.

    i_obj->get_selected_cells( IMPORTING et_cell = DATA(lt_sel_cells) ).
    IF lines( lt_sel_cells ) > 0.
      e_index = lt_sel_cells[ 1 ]-row_id.
    ELSE.
      i_obj->get_selected_rows( IMPORTING et_index_rows = DATA(lt_sel_rows) ).
      IF lines( lt_sel_rows ) > 0.
        e_index = lt_sel_rows[ 1 ]-index.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_rtti DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      create_table_by_name IMPORTING i_tname TYPE tabname
                           CHANGING  c_table TYPE REF TO data,

      create_struc_handle IMPORTING i_tname  TYPE tabname
                          EXPORTING e_t_comp TYPE abap_component_tab
                                    e_handle TYPE REF TO cl_abap_structdescr.

ENDCLASS.

CLASS lcl_debugger_script DEFINITION DEFERRED.

CLASS lcl_source_parser DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS: parse_tokens IMPORTING iv_program TYPE program io_debugger TYPE REF TO lcl_debugger_script.

ENDCLASS.

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    DATA: mo_debugger TYPE REF TO lcl_debugger_script.
    METHODS: constructor IMPORTING io_debugger TYPE REF TO lcl_debugger_script,
      on_double_click
        FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column.
ENDCLASS.


CLASS lcl_debugger_script DEFINITION INHERITING FROM  cl_tpda_script_class_super.

  PUBLIC SECTION.
    TYPES: BEGIN OF t_obj,
             name TYPE string,
             obj  TYPE string,
           END OF t_obj,

           BEGIN OF t_sel_var,
             name   TYPE string,
             is_sel TYPE xfeld,
             refval TYPE REF TO data,
           END OF t_sel_var.

    DATA: mt_obj            TYPE TABLE OF t_obj,
          mt_compo          TYPE TABLE OF scompo,
          mt_locals         TYPE tpda_scr_locals_it,
          mt_globals        TYPE tpda_scr_globals_it,
          mt_ret_exp        TYPE tpda_scr_locals_it,
          m_counter         TYPE i,
          mt_steps          TYPE  TABLE OF lcl_appl=>t_step_counter, "source code steps
          mt_var_step       TYPE  TABLE OF lcl_appl=>var_table_h,
          m_step            TYPE i,
          m_is_find         TYPE xfeld,
          m_stop_stack      TYPE i,
          m_debug           TYPE x,
          m_refresh         TYPE xfeld, "to refactor
          m_update          TYPE xfeld,
          is_step           TYPE xfeld,
          ms_stack_prev     TYPE   lcl_appl=>t_stack,
          ms_stack          TYPE   lcl_appl=>t_stack,
          is_history        TYPE xfeld,
          m_hist_step       TYPE i,
          m_step_delta      TYPE i,
          mt_vars_hist_view TYPE STANDARD TABLE OF lcl_appl=>var_table,
          mt_vars_hist      TYPE STANDARD TABLE OF lcl_appl=>var_table,
          mt_state          TYPE STANDARD TABLE OF lcl_appl=>var_table,
          mv_recurse        TYPE i,
          mt_classes_types  TYPE TABLE OF lcl_appl=>t_classes_types,
          mo_window         TYPE REF TO lcl_window,
          mv_f7_stop        TYPE xfeld,
          m_f6_level        TYPE i,
          m_target_stack    TYPE i,
          mo_tree_imp       TYPE REF TO lcl_rtti_tree,
          mo_tree_local     TYPE REF TO lcl_rtti_tree,
          mo_tree_exp       TYPE REF TO lcl_rtti_tree,
          mt_selected_var   TYPE TABLE OF t_sel_var,
          mv_stack_changed  TYPE xfeld,
          m_variable        TYPE REF TO data,
          mt_new_string     TYPE TABLE OF  string,
          m_quick           TYPE tpda_scr_quick_info,
          mr_statements     TYPE RANGE OF string.

    METHODS: prologue  REDEFINITION,
      init    REDEFINITION,
      script  REDEFINITION,
      end     REDEFINITION,

      run_script,
      run_script_hist IMPORTING iv_step TYPE i OPTIONAL
                      EXPORTING es_stop TYPE xfeld
                      ,
      show_variables CHANGING it_var TYPE lcl_appl=>t_var_table RETURNING VALUE(rv_stop) TYPE xfeld,
      set_selected_vars,
      save_hist IMPORTING
                  iv_name        TYPE clike
                  iv_fullname    TYPE string
                  iv_type        TYPE string
                  iv_cl_leaf     TYPE int4
                  iv_parent_name TYPE string
                  ir_up          TYPE any OPTIONAL
                  i_instance     TYPE string OPTIONAL,

      f5 RETURNING VALUE(rv_stop) TYPE xfeld,
      f6 RETURNING VALUE(rv_stop) TYPE xfeld,
      f7 RETURNING VALUE(rv_stop) TYPE xfeld,
      f8 RETURNING VALUE(rv_stop) TYPE xfeld,
      make_step,
      hndl_script_buttons IMPORTING iv_stack_changed TYPE xfeld
                          RETURNING VALUE(rv_stop)   TYPE xfeld,
      get_obj_index IMPORTING iv_name TYPE any RETURNING VALUE(e_index) TYPE string,
      create_reference         IMPORTING i_name            TYPE string
                                         i_type            TYPE string
                                         i_shortname       TYPE string OPTIONAL
                                         i_quick           TYPE tpda_scr_quick_info
                                         i_parent          TYPE string OPTIONAL
                               RETURNING VALUE(e_root_key) TYPE salv_de_node_key,
      show_step.

  PRIVATE SECTION.

    CONSTANTS: BEGIN OF c_kind,
                 struct LIKE cl_abap_typedescr=>kind_struct VALUE cl_abap_typedescr=>kind_struct,
                 table  LIKE cl_abap_typedescr=>kind_table VALUE cl_abap_typedescr=>kind_table,
                 elem   LIKE cl_abap_typedescr=>kind_elem VALUE cl_abap_typedescr=>kind_elem,
                 class  LIKE cl_abap_typedescr=>kind_class VALUE cl_abap_typedescr=>kind_class,
                 intf   LIKE cl_abap_typedescr=>kind_intf VALUE cl_abap_typedescr=>kind_intf,
                 ref    LIKE cl_abap_typedescr=>kind_ref VALUE cl_abap_typedescr=>kind_ref,
               END OF c_kind.

    METHODS: transfer_variable IMPORTING i_name        TYPE string
                                         iv_type       TYPE string
                                         i_shortname   TYPE string OPTIONAL
                                         iv_value      TYPE string OPTIONAL
                                         i_parent_name TYPE string OPTIONAL
                                         i_cl_leaf     TYPE int4 OPTIONAL
                                         i_instance    TYPE string OPTIONAL,

      create_simple_var IMPORTING i_name        TYPE string
                        RETURNING VALUE(er_var) TYPE REF TO data,

      create_simple_string IMPORTING i_name          TYPE string
                           RETURNING VALUE(e_string) TYPE string,

      create_struc         IMPORTING  i_name          TYPE string
                           RETURNING  VALUE(er_struc) TYPE REF TO data
                           EXCEPTIONS type_not_found,

      create_struc2         IMPORTING i_name      TYPE string
                                      i_shortname TYPE string OPTIONAL,

      get_class_name   IMPORTING i_name        TYPE string
                       RETURNING VALUE(e_name) TYPE string,

      get_deep_struc       IMPORTING i_name TYPE string
                                     r_obj  TYPE REF TO data,

      get_table  IMPORTING i_name TYPE string
                 CHANGING  c_obj  TYPE REF TO data,

      read_class_globals.

    METHODS traverse
      IMPORTING
        io_type_descr  TYPE REF TO cl_abap_typedescr
        iv_name        TYPE clike
        iv_fullname    TYPE string OPTIONAL
        iv_type        TYPE string
        ir_up          TYPE REF TO data OPTIONAL
        iv_parent_name TYPE string OPTIONAL
        iv_struc_name  TYPE string OPTIONAL
        i_instance     TYPE string OPTIONAL
        i_cl_leaf      TYPE int4
        i_ref          TYPE xfeld OPTIONAL
        i_suffix       TYPE string OPTIONAL.

    METHODS traverse_struct
      IMPORTING io_type_descr  TYPE REF TO cl_abap_typedescr
                iv_name        TYPE clike
                iv_fullname    TYPE string OPTIONAL
                iv_type        TYPE string
                i_cl_leaf      TYPE int4
                ir_up          TYPE  REF TO data OPTIONAL
                iv_parent_name TYPE string OPTIONAL
                iv_struc_name  TYPE string OPTIONAL
                i_instance     TYPE string OPTIONAL
                i_suffix       TYPE string OPTIONAL.

    METHODS traverse_elem
      IMPORTING
        iv_name        TYPE clike
        iv_fullname    TYPE string OPTIONAL
        iv_type        TYPE string
        iv_value       TYPE any OPTIONAL
        ir_up          TYPE  REF TO data OPTIONAL
        iv_parent_name TYPE string OPTIONAL
        i_cl_leaf      TYPE int4
        i_instance     TYPE string OPTIONAL.

ENDCLASS.

CLASS lcl_mermaid DEFINITION INHERITING FROM lcl_popup FRIENDS  lcl_debugger_script.

  PUBLIC SECTION.

    TYPES: BEGIN OF ts_if,
             if_ind      TYPE i,
             end_ind     TYPE i,
             before_else TYPE i,
           END OF ts_if,
           tt_if TYPE STANDARD TABLE OF ts_if WITH EMPTY KEY.


    DATA: mo_debugger     TYPE REF TO lcl_debugger_script,
          mo_mm_container TYPE REF TO cl_gui_container,
          mo_mm_toolbar   TYPE REF TO cl_gui_container,
          mo_toolbar      TYPE REF TO cl_gui_toolbar,
          mo_diagram      TYPE REF TO zcl_wd_gui_mermaid_js_diagram,
          mv_type         TYPE string,
          ms_if           TYPE ts_if,
          mt_if           TYPE tt_if.

    METHODS: constructor IMPORTING io_debugger TYPE REF TO lcl_debugger_script
                                   iv_type     TYPE string,

      steps_flow IMPORTING iv_direction TYPE ui_func OPTIONAL,
      magic_search IMPORTING iv_direction TYPE ui_func OPTIONAL,
      add_toolbar_buttons,
      hnd_toolbar FOR EVENT function_selected OF cl_gui_toolbar IMPORTING fcode,
      open_mermaid IMPORTING iv_mm_string TYPE string.

ENDCLASS.

CLASS lcl_rtti_tree DEFINITION FINAL. " INHERITING FROM lcl_popup.

  PUBLIC SECTION.

    TYPES: BEGIN OF t_classes_leaf,
             name TYPE string,
             type TYPE char1,
             key  TYPE salv_de_node_key,
           END OF t_classes_leaf.

    TYPES: BEGIN OF ts_table,
             ref      TYPE REF TO data,
             kind(1),
             value    TYPE string,
             typename TYPE abap_abstypename,
             fullname TYPE string,
             path     TYPE string,
           END OF ts_table.

    TYPES tt_table TYPE STANDARD TABLE OF ts_table
          WITH NON-UNIQUE DEFAULT KEY.

    DATA: main_node_key   TYPE salv_de_node_key,
          m_refresh       TYPE xfeld,
          m_leaf          TYPE string,
          m_hide          TYPE x,
          m_clear         TYPE flag,
          m_locals        TYPE x,
          m_globals       TYPE x,
          m_syst          TYPE x,
          m_class_data    TYPE x,
          m_ldb           TYPE x,
          m_locals_key    TYPE salv_de_node_key,
          m_globals_key   TYPE salv_de_node_key,
          m_class_key     TYPE salv_de_node_key,
          m_syst_key      TYPE salv_de_node_key,
          m_ldb_key       TYPE salv_de_node_key,
          m_icon          TYPE salv_de_tree_image,
          mt_vars         TYPE STANDARD TABLE OF lcl_appl=>var_table,
          mt_classes_leaf TYPE TABLE OF t_classes_leaf,
          m_prg_info      TYPE tpda_scr_prg_info,
          mo_debugger     TYPE REF TO lcl_debugger_script,
          tree            TYPE REF TO cl_salv_tree.

    METHODS constructor IMPORTING i_header   TYPE clike DEFAULT 'View'
                                  i_type     TYPE xfeld OPTIONAL
                                  i_cont     TYPE REF TO cl_gui_container OPTIONAL
                                  i_debugger TYPE REF TO lcl_debugger_script OPTIONAL.

    METHODS del_variable IMPORTING  iv_full_name TYPE string i_state TYPE xfeld OPTIONAL.

    METHODS clear.

    METHODS add_buttons IMPORTING iv_type TYPE xfeld.
    METHODS add_node
      IMPORTING
        iv_name TYPE string
        iv_icon TYPE salv_de_tree_image OPTIONAL.

    METHODS add_obj_nodes
      IMPORTING
                is_var            TYPE lcl_appl=>var_table
      RETURNING VALUE(e_root_key) TYPE salv_de_node_key.

    METHODS delete_node IMPORTING iv_key TYPE salv_de_node_key.
    METHODS display IMPORTING io_debugger TYPE REF TO lcl_debugger_script OPTIONAL.

    METHODS traverse
      IMPORTING
                io_type_descr     TYPE REF TO cl_abap_typedescr
                iv_parent_key     TYPE salv_de_node_key
                iv_rel            TYPE salv_de_node_relation
                is_var            TYPE lcl_appl=>var_table
                ir_up             TYPE REF TO data OPTIONAL
                iv_parent_name    TYPE string OPTIONAL
                iv_struc_name     TYPE string OPTIONAL
      RETURNING VALUE(e_root_key) TYPE salv_de_node_key.

    METHODS traverse_struct
      IMPORTING
                io_type_descr     TYPE REF TO cl_abap_typedescr
                iv_parent_key     TYPE salv_de_node_key
                iv_rel            TYPE salv_de_node_relation
                is_var            TYPE lcl_appl=>var_table
                ir_up             TYPE REF TO data OPTIONAL
                iv_parent_name    TYPE string OPTIONAL
                iv_struc_name     TYPE string OPTIONAL
      RETURNING VALUE(e_root_key) TYPE salv_de_node_key.

    METHODS traverse_elem
      IMPORTING
                io_type_descr     TYPE REF TO cl_abap_typedescr
                iv_parent_key     TYPE salv_de_node_key
                iv_rel            TYPE salv_de_node_relation
                is_var            TYPE lcl_appl=>var_table
                iv_value          TYPE any OPTIONAL
                iv_parent_name    TYPE string OPTIONAL
      RETURNING VALUE(e_root_key) TYPE salv_de_node_key.

    METHODS traverse_obj
      IMPORTING
                iv_parent_key     TYPE salv_de_node_key
                iv_rel            TYPE salv_de_node_relation
                is_var            TYPE lcl_appl=>var_table
                iv_value          TYPE any OPTIONAL
                ir_up             TYPE REF TO data OPTIONAL
                iv_parent_name    TYPE string OPTIONAL
      RETURNING VALUE(e_root_key) TYPE salv_de_node_key.

    METHODS traverse_table
      IMPORTING
                io_type_descr     TYPE REF TO cl_abap_typedescr
                iv_parent_key     TYPE salv_de_node_key
                iv_rel            TYPE salv_de_node_relation
                is_var            TYPE lcl_appl=>var_table
                ir_up             TYPE REF TO data OPTIONAL
                iv_parent_name    TYPE string OPTIONAL
      RETURNING VALUE(e_root_key) TYPE salv_de_node_key.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_kind,
                 struct LIKE cl_abap_typedescr=>kind_struct VALUE cl_abap_typedescr=>kind_struct,
                 table  LIKE cl_abap_typedescr=>kind_table VALUE cl_abap_typedescr=>kind_table,
                 elem   LIKE cl_abap_typedescr=>kind_elem VALUE cl_abap_typedescr=>kind_elem,
                 class  LIKE cl_abap_typedescr=>kind_class VALUE cl_abap_typedescr=>kind_class,
                 intf   LIKE cl_abap_typedescr=>kind_intf VALUE cl_abap_typedescr=>kind_intf,
                 ref    LIKE cl_abap_typedescr=>kind_ref VALUE cl_abap_typedescr=>kind_ref,
               END OF c_kind.

    DATA: tree_table TYPE tt_table.


    METHODS: hndl_double_click FOR EVENT double_click OF cl_salv_events_tree IMPORTING node_key,
      hndl_user_command FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function.

ENDCLASS.

CLASS lcl_ai_api DEFINITION.

  PUBLIC SECTION.

    METHODS  call_openai  IMPORTING iv_prompt TYPE string RETURNING VALUE(rv_answer) TYPE string.
  PRIVATE SECTION.
    DATA: mv_api_key TYPE string.

    METHODS: build_request
      IMPORTING
        iv_prompt  TYPE string
      EXPORTING
        ev_payload TYPE string ,
      send_request
        IMPORTING
          iv_payload  TYPE string
        EXPORTING
          ev_response TYPE string,
      output
        IMPORTING
                  iv_prompt        TYPE string
                  iv_content       TYPE string
        RETURNING VALUE(rv_answer) TYPE string.

ENDCLASS.

CLASS lcl_ai_api IMPLEMENTATION.

  METHOD call_openai.
    DATA: lv_prompt   TYPE string,
          lv_payload  TYPE string,
          lv_response TYPE string.

    "Build payload
    CALL METHOD build_request
      EXPORTING
        iv_prompt  = iv_prompt
      IMPORTING
        ev_payload = lv_payload.

    CALL METHOD me->send_request
      EXPORTING
        iv_payload  = lv_payload
      IMPORTING
        ev_response = lv_response.

    rv_answer = output(
      EXPORTING
        iv_prompt  = iv_prompt
        iv_content = lv_response ).

  ENDMETHOD.

  METHOD build_request.

    DATA: lv_payload TYPE string.

    lv_payload = |{ '{ "model": "mistral-tiny", "messages": [{ "role": "user", "content": "' && iv_prompt &&  '" }], "max_tokens": 1000 } ' }|.

    ev_payload = lv_payload.
  ENDMETHOD.

  METHOD send_request.

    DATA: lo_http_client   TYPE REF TO if_http_client,
          lv_response_body TYPE string,
          lv_header        TYPE string.

    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING
        destination                = 'Z_LM' "SM59 local config
      IMPORTING
        client                     = lo_http_client
      EXCEPTIONS
        argument_not_found         = 1
        destination_not_found      = 2
        destination_no_authority   = 3
        plugin_not_active          = 4
        internal_error             = 5
        oa2c_set_token_error       = 6
        oa2c_missing_authorization = 7
        oa2c_invalid_config        = 8
        oa2c_invalid_parameters    = 9
        oa2c_invalid_scope         = 10
        oa2c_invalid_grant         = 11
        oa2c_secstore_adm          = 12
        OTHERS                     = 13.
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ENDIF.

    mv_api_key = 'lmstudio'. "any name for local LLMs or secret key for external
    "set request header
    lo_http_client->request->set_header_field( name = 'Content-Type' value = 'application/json' ).
    lo_http_client->request->set_header_field( name = 'Authorization' value = |Bearer { mv_api_key }| ).

    lo_http_client->request->set_method('POST').

    "set payload
    lo_http_client->request->set_cdata( iv_payload ).

    CALL METHOD lo_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.
    IF sy-subrc = 0.
      CALL METHOD lo_http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4.
      "Get response
      IF sy-subrc <> 0.
        lv_response_body = lo_http_client->response->get_data( ).
        ev_response = lv_response_body.
      ELSE.
        lv_response_body = lo_http_client->response->get_data( ).
        IF lv_response_body IS NOT INITIAL.
          ev_response = lv_response_body.
        ELSE.
          ev_response = 'Call was succeesful, but got no response'.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD output.

    DATA: lv_text(1000) TYPE c,
          lv_string     TYPE string,
          lv_content    TYPE string,
          lv_reasoning  TYPE string.

    TYPES: BEGIN OF lty_s_message,
             role              TYPE string,
             content           TYPE string,
             reasoning_content TYPE string,
           END           OF lty_s_message,
           lty_t_message TYPE STANDARD TABLE OF lty_s_message WITH NON-UNIQUE DEFAULT KEY,
           BEGIN OF lty_s_choice,
             index         TYPE string,
             message       TYPE lty_s_message,
             logprobs      TYPE string,
             finish_reason TYPE string,
           END      OF lty_s_choice,
           BEGIN OF lty_s_base_chatgpt_res,
             id      TYPE string,
             object  TYPE string,
             created TYPE string,
             model   TYPE string,
             choices TYPE TABLE OF lty_s_choice WITH NON-UNIQUE DEFAULT KEY,
           END OF lty_s_base_chatgpt_res.

    DATA ls_response TYPE lty_s_base_chatgpt_res.

    DATA: lv_binary TYPE xstring.

    DATA: lo_x2c TYPE REF TO cl_abap_conv_in_ce.
    lo_x2c = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
    lv_binary = iv_content.
    lo_x2c->convert( EXPORTING input = lv_binary
                     IMPORTING data  = lv_string ).

    /ui2/cl_json=>deserialize( EXPORTING json = lv_string CHANGING data = ls_response ).

    IF  ls_response-choices IS NOT INITIAL.
      lv_content = ls_response-choices[ 1 ]-message-content.
      lv_reasoning = ls_response-choices[ 1 ]-message-reasoning_content.
    ELSE.
      lv_content = lv_string.
    ENDIF.

    rv_answer = lv_content.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_ai DEFINITION INHERITING FROM lcl_popup.

  PUBLIC SECTION.
    DATA: mo_ai_box               TYPE REF TO cl_gui_dialogbox_container,
          mo_ai_splitter          TYPE REF TO cl_gui_splitter_container,
          mo_ai_toolbar_container TYPE REF TO cl_gui_container,
          mo_ai_toolbar           TYPE REF TO cl_gui_toolbar,
          mo_prompt_container     TYPE REF TO cl_gui_container,
          mo_answer_container     TYPE REF TO cl_gui_container,
          mo_prompt_text          TYPE REF TO cl_gui_textedit,
          mo_answer_text          TYPE REF TO cl_gui_textedit,
          mv_prompt               TYPE string,
          mv_answer               TYPE string.

    METHODS:  constructor IMPORTING io_source TYPE REF TO cl_ci_source_include,
      add_ai_toolbar_buttons,
      hnd_ai_toolbar FOR EVENT function_selected OF cl_gui_toolbar IMPORTING fcode.

ENDCLASS.

CLASS lcl_ai IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    mo_ai_box = create( i_name = 'SDDE Simple Debugger Data Explorer beta v. 0.9' i_width = 1400 i_hight = 400 ).
    CREATE OBJECT mo_ai_splitter
      EXPORTING
        parent  = mo_ai_box
        rows    = 3
        columns = 1
      EXCEPTIONS
        OTHERS  = 1.

    mo_ai_splitter->get_container(
         EXPORTING
           row       = 1
           column    = 1
         RECEIVING
           container = mo_ai_toolbar_container ).

    mo_ai_splitter->get_container(
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = mo_prompt_container ).

    mo_ai_splitter->get_container(
      EXPORTING
        row       = 3
        column    = 1
      RECEIVING
        container = mo_answer_container  ).

    mo_ai_splitter->set_row_height( id = 1 height = '3' ).

    mo_ai_splitter->set_row_sash( id    = 1
                                  type  = 0
                                  value = 0 ).


    SET HANDLER on_box_close FOR mo_ai_box.


    CREATE OBJECT mo_prompt_text
      EXPORTING
        parent                 = mo_prompt_container
      EXCEPTIONS
        error_cntl_create      = 1
        error_cntl_init        = 2
        error_cntl_link        = 3
        error_dp_create        = 4
        gui_type_not_supported = 5
        OTHERS                 = 6.
    IF sy-subrc <> 0.
      on_box_close( mo_box ).
    ENDIF.

    CREATE OBJECT mo_answer_text
      EXPORTING
        parent                 = mo_answer_container
      EXCEPTIONS
        error_cntl_create      = 1
        error_cntl_init        = 2
        error_cntl_link        = 3
        error_dp_create        = 4
        gui_type_not_supported = 5
        OTHERS                 = 6.
    IF sy-subrc <> 0.
      on_box_close( mo_box ).
    ENDIF.

    mo_answer_text->set_readonly_mode( ).

    CREATE OBJECT mo_ai_toolbar EXPORTING parent = mo_ai_toolbar_container.
    add_ai_toolbar_buttons( ).
    mo_ai_toolbar->set_visible( 'X' ).

    "set prompt
    DATA lt_string TYPE TABLE OF char255.

    APPEND INITIAL LINE TO lt_string ASSIGNING FIELD-SYMBOL(<str>).
    <str> = 'Explain please the meaning of this ABAP code'.
    mv_prompt = <str>.
    APPEND INITIAL LINE TO lt_string ASSIGNING <str>.


    LOOP AT io_source->lines INTO DATA(ls_line).
      APPEND INITIAL LINE TO lt_string ASSIGNING <str>.
      <str> = ls_line.
      mv_prompt = mv_prompt && <str>.
    ENDLOOP.

    mo_prompt_text->set_text_as_r3table( lt_string ).
    cl_gui_control=>set_focus( mo_ai_box ).

  ENDMETHOD.

  METHOD add_ai_toolbar_buttons.

    DATA: lt_button TYPE ttb_button,
          lt_events TYPE cntl_simple_events,
          ls_events LIKE LINE OF lt_events.

    lt_button  = VALUE #(
     ( function = 'AI' icon = CONV #( icon_manikin_unknown_gender ) quickinfo = 'Ask AI' text = 'Ask AI' ) ).

    mo_ai_toolbar->add_button_group( lt_button ).

*   Register events
    ls_events-eventid = cl_gui_toolbar=>m_id_function_selected.
    ls_events-appl_event = space.
    APPEND ls_events TO lt_events.

    mo_ai_toolbar->set_registered_events( events = lt_events ).
    SET HANDLER me->hnd_ai_toolbar FOR mo_ai_toolbar.

  ENDMETHOD.

  METHOD hnd_ai_toolbar.

    DATA: lv_prompt TYPE string.

    CASE fcode.

      WHEN 'AI'.

        cl_gui_cfw=>flush( ).
        DATA(lo_ai) = NEW lcl_ai_api( ).

        DATA lt_text TYPE TABLE OF char255.
        CALL METHOD mo_prompt_text->get_text_as_stream
          IMPORTING
            text = lt_text.
        CLEAR mv_prompt.
        LOOP AT lt_text INTO DATA(lv_line).
          CONCATENATE mv_prompt lv_line
                      "cl_abap_char_utilities=>newline
                 INTO mv_prompt.
        ENDLOOP.

        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN mv_prompt WITH ''.
        REPLACE ALL OCCURRENCES OF '#' IN mv_prompt WITH ''.
        REPLACE ALL OCCURRENCES OF REGEX '[[:cntrl:]]' IN mv_prompt WITH ' '.

        mv_answer = lo_ai->call_openai( mv_prompt ).
        mo_answer_text->set_textstream( mv_answer ).

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_window DEFINITION INHERITING FROM lcl_popup .

  PUBLIC SECTION.

    TYPES: BEGIN OF ts_table,
             ref      TYPE REF TO data,
             kind(1),
             value    TYPE string,
             typename TYPE abap_abstypename,
             fullname TYPE string,
           END OF ts_table,

           BEGIN OF ts_calls,
             event TYPE string,
             type  TYPE string,
             name  TYPE string,
             outer TYPE string,
             inner TYPE string,
           END OF ts_calls,
           tt_calls TYPE STANDARD TABLE OF ts_calls WITH NON-UNIQUE KEY event,

           BEGIN OF ts_calls_line,
             class     TYPE string,
             eventtype TYPE string,
             eventname TYPE string,
             index     TYPE i,
           END OF ts_calls_line,
           tt_calls_line TYPE STANDARD TABLE OF ts_calls_line WITH NON-UNIQUE EMPTY KEY,

           BEGIN OF ts_kword,
             index     TYPE i,
             line      TYPE i,
             name      TYPE string,
             from      TYPE i,
             to        TYPE i,
             tt_calls  TYPE tt_calls,
             to_evtype TYPE string,
             to_evname TYPE string,
           END OF ts_kword,

           BEGIN OF ts_calculated,
             line       TYPE i,
             calculated TYPE string,
           END OF ts_calculated,

           BEGIN OF ts_composing,
             line      TYPE i,
             composing TYPE string,
           END OF ts_composing,

           tt_kword      TYPE STANDARD TABLE OF ts_kword WITH EMPTY KEY,
           tt_calculated TYPE STANDARD TABLE OF ts_calculated WITH EMPTY KEY,
           tt_composed   TYPE STANDARD TABLE OF ts_composing WITH EMPTY KEY,

           BEGIN OF ts_params,
             class     TYPE string,
             event     TYPE string,
             name      TYPE string,
             param     TYPE string,
             type      TYPE char1,
             preferred TYPE char1,
           END OF ts_params,
           tt_params TYPE STANDARD TABLE OF ts_params WITH EMPTY KEY,

           BEGIN OF ts_int_tabs,
             eventtype TYPE string,
             eventname TYPE string,
             name      TYPE string,
             type      TYPE string,
           END OF ts_int_tabs,
           tt_tabs TYPE STANDARD TABLE OF ts_int_tabs WITH EMPTY KEY,

           BEGIN OF ts_progs,
             include       TYPE program,
             source        TYPE REF TO cl_ci_source_include,
             scan          TYPE REF TO cl_ci_scan,
             t_keywords    TYPE tt_kword,
             t_calculated  TYPE tt_calculated,
             t_composed    TYPE tt_composed,
             t_params      TYPE tt_params,
             tt_tabs       TYPE tt_tabs,
             tt_calls_line TYPE tt_calls_line,
           END OF ts_progs,

           BEGIN OF ts_locals,
             program    TYPE tpda_program,
             eventtype  TYPE tpda_event_type,
             eventname  TYPE tpda_event,
             loc_fill   TYPE xfeld,
             locals_tab TYPE tpda_scr_locals_it,
             mt_fs      TYPE tpda_scr_locals_it,
           END OF ts_locals,

           BEGIN OF ts_globals,
             program     TYPE tpda_program,
             glob_fill   TYPE xfeld,
             globals_tab TYPE tpda_scr_globals_it,
             mt_fs       TYPE tpda_scr_locals_it,
           END OF ts_globals,

           BEGIN OF ts_watch,
             program TYPE string,
             line    TYPE i,
           END OF ts_watch,
           tt_watch TYPE STANDARD  TABLE OF ts_watch WITH EMPTY KEY.

    TYPES tt_table TYPE STANDARD TABLE OF ts_table
          WITH NON-UNIQUE DEFAULT KEY.

    DATA: m_version              TYPE x, " 0 - alpha, 01 - beta
          m_history              TYPE x,
          m_visualization        TYPE x,
          m_varhist              TYPE x,
          m_zcode                TYPE x,
          m_direction            TYPE x,
          m_prg                  TYPE tpda_scr_prg_info,
          m_debug_button         LIKE sy-ucomm,
          m_show_step            TYPE xfeld,
          mo_debugger            TYPE REF TO lcl_debugger_script,
          mo_splitter_code       TYPE REF TO cl_gui_splitter_container,
          mo_splitter_var        TYPE REF TO cl_gui_splitter_container,
          mo_splitter_steps      TYPE REF TO cl_gui_splitter_container,
          mo_toolbar_container   TYPE REF TO cl_gui_container,
          mo_importing_container TYPE REF TO cl_gui_container,
          mo_locals_container    TYPE REF TO cl_gui_container,
          mo_exporting_container TYPE REF TO cl_gui_container,
          mo_code_container      TYPE REF TO cl_gui_container,
          mo_imp_exp_container   TYPE REF TO cl_gui_container,
          mo_editor_container    TYPE REF TO cl_gui_container,
          mo_steps_container     TYPE REF TO cl_gui_container,
          mo_stack_container     TYPE REF TO cl_gui_container,
          mo_hist_container      TYPE REF TO cl_gui_container,
          mo_code_viewer         TYPE REF TO cl_gui_abapedit,
          mt_stack               TYPE TABLE OF lcl_appl=>t_stack,
          mo_toolbar             TYPE REF TO cl_gui_toolbar,
          mo_salv_stack          TYPE REF TO cl_salv_table,
          mo_salv_steps          TYPE REF TO cl_salv_table,
          mo_salv_hist           TYPE REF TO cl_salv_table,
          mt_breaks              TYPE tpda_bp_persistent_it,
          mt_watch               TYPE tt_watch,
          mt_coverage            TYPE tt_watch,
          m_hist_depth           TYPE i,
          m_start_stack          TYPE i,
          mt_source              TYPE STANDARD  TABLE OF ts_progs,
          mt_params              TYPE STANDARD  TABLE OF ts_params,
          mt_locals_set          TYPE STANDARD TABLE OF ts_locals,
          mt_globals_set         TYPE STANDARD TABLE OF ts_globals.

    METHODS: constructor IMPORTING i_debugger TYPE REF TO lcl_debugger_script i_additional_name TYPE string OPTIONAL,
      add_toolbar_buttons,
      hnd_toolbar FOR EVENT function_selected OF cl_gui_toolbar IMPORTING fcode,
      set_program IMPORTING iv_program TYPE program,
      show_coverage.

    METHODS set_program_line IMPORTING iv_line LIKE sy-index.
    METHODS create_code_viewer.
    METHODS show_stack.

ENDCLASS.

CLASS lcl_debugger_script IMPLEMENTATION.

  METHOD prologue.
    super->prologue( ).
  ENDMETHOD.                    "prolog

  METHOD init.

    CONSTANTS: c_mask TYPE x VALUE '01'.

    is_step = abap_on.
    lcl_appl=>check_mermaid( ).
    lcl_appl=>init_lang( ).
    lcl_appl=>init_icons_table( ).

    mo_window = NEW lcl_window( me ).

    mo_tree_imp = NEW lcl_rtti_tree( i_header   = 'Importing parameters'
                                     i_type     = 'I'
                                     i_cont     = mo_window->mo_importing_container
                                     i_debugger = me ).

    mo_tree_local = NEW lcl_rtti_tree( i_header   = 'Variables'
                                       i_type     = 'L'
                                       i_cont     = mo_window->mo_locals_container
                                       i_debugger = me ).

    mo_tree_exp = NEW lcl_rtti_tree( i_header   = 'Exporting & Returning parameters'
                                     i_type     = 'E'
                                     i_cont     = mo_window->mo_exporting_container
                                     i_debugger = me ).

    mo_tree_local->m_locals = mo_tree_local->m_locals BIT-XOR c_mask.

  ENDMETHOD.

  METHOD create_simple_var.

    DATA: lr_symbsimple TYPE REF TO tpda_sys_symbsimple,
          lo_elem       TYPE REF TO cl_abap_elemdescr.

    CALL METHOD cl_tpda_script_data_descr=>get_quick_info
      EXPORTING
        p_var_name   = i_name
      RECEIVING
        p_symb_quick = DATA(quick).

    ASSIGN quick-quickdata TO FIELD-SYMBOL(<lv_value>).
    lr_symbsimple ?= <lv_value>.
    ASSIGN lr_symbsimple->* TO FIELD-SYMBOL(<simple>).

    CALL METHOD cl_abap_complexdescr=>describe_by_name
      EXPORTING
        p_name         = quick-abstypename
      RECEIVING
        p_descr_ref    = DATA(lo_type)
      EXCEPTIONS
        type_not_found = 1.

    lo_elem ?= lo_type.
    CREATE DATA er_var TYPE HANDLE lo_elem.
    ASSIGN er_var->* TO FIELD-SYMBOL(<new_elem>).
    <new_elem> = <simple>-valstring.

  ENDMETHOD.

  METHOD create_simple_string.

    DATA: lr_string TYPE REF TO tpda_sys_symbstring,
          lr_struc  TYPE REF TO data,
          lo_elem   TYPE REF TO cl_abap_elemdescr,
          lv_depth  TYPE i VALUE 0.

    FIELD-SYMBOLS: <string>   TYPE tpda_sys_symbstring,
                   <lv_value> TYPE any.

    e_string = 'Unknown'.  " Default value

    TRY.
        CALL METHOD cl_tpda_script_data_descr=>get_quick_info
          EXPORTING
            p_var_name   = i_name
          RECEIVING
            p_symb_quick = DATA(quick).

        ASSIGN quick-quickdata TO <lv_value>.

        " Handle only direct strings, not references
        IF quick-typid = 'g'. " Regular string
          lr_string ?= <lv_value>.
          ASSIGN lr_string->* TO <string>.
          e_string = <string>-valstring.
        ELSE.
          " For references and other types, just return a descriptive text
          e_string = |[{ quick-typid }:{ i_name }]|.
        ENDIF.

      CATCH cx_root.
        e_string = |Error: { i_name }|.
    ENDTRY.

  ENDMETHOD.

  METHOD create_struc.

    DATA: lo_new_type    TYPE REF TO cl_abap_structdescr,
          ls_comp        TYPE abap_componentdescr,
          lt_components  TYPE abap_component_tab,
          lr_symbsimple  TYPE REF TO tpda_sys_symbsimple,
          lo_struc_descr TYPE REF TO cl_tpda_script_structdescr,
          comp_full      TYPE  tpda_scr_struct_comp_it,
          comp_it        TYPE tpda_script_struc_componentsit.

    FIELD-SYMBOLS: <lv_value> TYPE any,
                   <simple>   TYPE tpda_sys_symbsimple.

    CLEAR er_struc.
    CALL METHOD cl_tpda_script_data_descr=>get_quick_info
      EXPORTING
        p_var_name   = i_name
      RECEIVING
        p_symb_quick = DATA(quick).

    lo_struc_descr ?= cl_tpda_script_data_descr=>factory( i_name ).
    lo_struc_descr->components( IMPORTING p_components_it = comp_it p_components_full_it = comp_full ).

    lcl_rtti=>create_struc_handle( EXPORTING i_tname = CONV #( replace( val = quick-abstypename sub = '/TYPE=' with = '' ) ) IMPORTING e_handle = lo_new_type ).
    IF lo_new_type IS NOT INITIAL.
      CREATE DATA er_struc TYPE HANDLE lo_new_type.
    ELSE.

      LOOP AT comp_full INTO DATA(l_comp).
        ls_comp-name = l_comp-compname.

        ASSIGN l_comp-symbquick-quickdata TO <lv_value>.
        lr_symbsimple ?= <lv_value>.
        ASSIGN lr_symbsimple->* TO <simple>.

        CALL METHOD cl_tpda_script_data_descr=>get_quick_info
          EXPORTING
            p_var_name   = |{ i_name }-{ l_comp-compname }|
          RECEIVING
            p_symb_quick = DATA(quick_sub).

        CALL METHOD cl_abap_complexdescr=>describe_by_name
          EXPORTING
            p_name         = quick_sub-abstypename
          RECEIVING
            p_descr_ref    = DATA(lo_type)
          EXCEPTIONS
            type_not_found = 1.

        IF sy-subrc = 0.
          ls_comp-type ?= lo_type.
          APPEND ls_comp TO lt_components.
        ENDIF.
      ENDLOOP.
      lo_new_type  = cl_abap_structdescr=>create( lt_components ).
      CREATE DATA er_struc TYPE HANDLE lo_new_type.
    ENDIF.

    ASSIGN er_struc->* TO FIELD-SYMBOL(<new_struc>).

    LOOP AT comp_full INTO l_comp.
      ASSIGN l_comp-symbquick-quickdata TO <lv_value>.
      lr_symbsimple ?= <lv_value>.
      ASSIGN COMPONENT l_comp-compname OF STRUCTURE <new_struc> TO FIELD-SYMBOL(<new>).
      <new> = lr_symbsimple->valstring.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_deep_struc.

    DATA: lr_struc       TYPE REF TO data,
          lo_struc_descr TYPE REF TO cl_tpda_script_structdescr,
          comp_full      TYPE  tpda_scr_struct_comp_it,
          comp_it        TYPE tpda_script_struc_componentsit,
          lr_symbsimple  TYPE REF TO tpda_sys_symbsimple,
          lr_symbstring  TYPE REF TO tpda_sys_symbstring,
          lr_symbstruc   TYPE REF TO tpda_sys_symbstruct,
          r_data         TYPE REF TO data.

    FIELD-SYMBOLS: <lv_value> TYPE any.

    ASSIGN r_obj->* TO FIELD-SYMBOL(<new_deep>).
    lo_struc_descr ?= cl_tpda_script_data_descr=>factory( i_name ).
    lo_struc_descr->components( IMPORTING p_components_it = comp_it p_components_full_it = comp_full ).

    LOOP AT comp_full INTO DATA(comp).
      CASE comp-symbquick-metatype.
        WHEN cl_tpda_script_data_descr=>mt_simple.
          ASSIGN comp-symbquick-quickdata TO <lv_value>.
          lr_symbsimple ?= <lv_value>.
          ASSIGN COMPONENT comp-compname OF STRUCTURE <new_deep> TO FIELD-SYMBOL(<new>).
          <new> = lr_symbsimple->valstring.

        WHEN cl_tpda_script_data_descr=>mt_string.
          ASSIGN comp-symbquick-quickdata TO <lv_value>.
          lr_symbstring ?= <lv_value>.
          ASSIGN COMPONENT comp-compname OF STRUCTURE <new_deep> TO <new>.
          <new> = lr_symbstring->valstring.

        WHEN cl_tpda_script_data_descr=>mt_struct.
          ASSIGN comp-symbquick-quickdata TO <lv_value>.
          ASSIGN COMPONENT comp-compname OF STRUCTURE <new_deep> TO <new>.
          GET REFERENCE OF <new> INTO lr_struc.
          lr_symbstruc ?= <lv_value>.
          get_deep_struc( EXPORTING i_name = |{ i_name }-{ comp-compname }|
                                    r_obj  = lr_struc ).

        WHEN cl_tpda_script_data_descr=>mt_tab.
          FIELD-SYMBOLS: <new_table> TYPE ANY TABLE.
          ASSIGN COMPONENT comp-compname OF STRUCTURE <new_deep> TO <new_table>.
          GET REFERENCE OF <new_table> INTO r_data.
          get_table( EXPORTING i_name = |{ i_name }-{ comp-compname }|
                     CHANGING  c_obj  = r_data ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_table. "construct deep tables

    DATA: r_data         TYPE REF TO data,
          lo_table_descr TYPE REF TO cl_tpda_script_tabledescr,
          table_clone    TYPE REF TO data,
          lo_tabl        TYPE REF TO cl_abap_tabledescr,
          lo_struc       TYPE REF TO cl_abap_structdescr,
          r_struc        TYPE REF TO data.

    FIELD-SYMBOLS: <f>         TYPE ANY TABLE,
                   <new_table> TYPE ANY TABLE.

    ASSIGN c_obj->* TO <new_table>.
    lo_tabl ?= cl_abap_typedescr=>describe_by_data( <new_table> ).
    lo_struc ?= lo_tabl->get_table_line_type( ).

    CREATE DATA r_data TYPE HANDLE lo_struc.
    ASSIGN r_data->* TO FIELD-SYMBOL(<new_line>).

    lo_table_descr ?= cl_tpda_script_data_descr=>factory( i_name ).
    TRY.
        table_clone = lo_table_descr->elem_clone( ).
        ASSIGN table_clone->* TO <f>.
        DATA: l_count TYPE i.

        LOOP AT <f> ASSIGNING FIELD-SYMBOL(<fs>).
          l_count = sy-tabix.
          CLEAR <new_line>.

          DO.
            ASSIGN COMPONENT sy-index OF STRUCTURE <fs> TO FIELD-SYMBOL(<from>).
            IF sy-subrc NE 0.
              EXIT.
            ENDIF.

            ASSIGN COMPONENT sy-index OF STRUCTURE <new_line> TO FIELD-SYMBOL(<to>).
            DESCRIBE FIELD <from> TYPE DATA(from_type).
            DESCRIBE FIELD   <to> TYPE DATA(to_type).

            IF from_type = to_type.
              <to> = <from>.
            ELSEIF to_type = 'h'.
              lo_struc ?= cl_abap_typedescr=>describe_by_data( <fs> ).
              READ TABLE lo_struc->components INDEX sy-index INTO DATA(ls_comp).
              GET REFERENCE OF <to> INTO r_data.
              get_table( EXPORTING i_name = |{ i_name }[ { l_count } ]-{ ls_comp-name }|
                         CHANGING  c_obj  = r_data ).
            ENDIF.
          ENDDO.
          INSERT <new_line> INTO TABLE <new_table>.
        ENDLOOP.
      CATCH cx_root.
        DATA(l_cnt) = lo_table_descr->linecnt( ).
        DO l_cnt TIMES.
          r_struc = create_struc( i_name = |{ i_name }[{ sy-index }]| ).
          ASSIGN r_struc->* TO <new_line>.
          INSERT <new_line> INTO TABLE <new_table>.
        ENDDO.
    ENDTRY.

  ENDMETHOD.

  METHOD transfer_variable.

    DATA: lr_struc       TYPE REF TO data,
          lo_table_descr TYPE REF TO cl_tpda_script_tabledescr,
          table_clone    TYPE REF TO data,
          l_name         TYPE string,
          l_full_name    TYPE string,
          lo_deep_handle TYPE REF TO cl_abap_datadescr,
          deep_ref       TYPE REF TO cl_abap_typedescr,
          lo_tabl        TYPE REF TO cl_abap_tabledescr,
          lo_struc       TYPE REF TO cl_abap_structdescr,
          r_header       TYPE REF TO data,
          r_elem         TYPE REF TO data.

    DATA: lv_len TYPE i.
    FIELD-SYMBOLS: <lv_value> TYPE any.

    l_full_name = i_name.

    IF i_name NE '{A:initial}'.
      TRY.
          CALL METHOD cl_tpda_script_data_descr=>get_quick_info
            EXPORTING
              p_var_name   = i_name
            RECEIVING
              p_symb_quick = m_quick.
        CATCH cx_tpda_varname .

          mo_tree_local->del_variable( EXPORTING iv_full_name = i_name i_state = 'X' ).
          RETURN.
      ENDTRY.
    ELSE.
      m_quick-typid = 'g'.
    ENDIF.

    IF i_shortname IS NOT INITIAL.
      l_name = i_shortname.
    ELSE.
      l_name = i_name.
    ENDIF.

    TRY.
        IF i_name NE '{A:initial}'.
          ASSIGN m_quick-quickdata->* TO <lv_value>.
        ENDIF.

        IF m_quick-typid = 'h'."internal table
          READ TABLE mo_window->mt_source WITH KEY include = ms_stack-include INTO DATA(ls_source).
          READ TABLE ls_source-tt_tabs WITH KEY name = i_name INTO DATA(ls_var).


          lo_table_descr ?= cl_tpda_script_data_descr=>factory( i_name ).

*          DATA(lt_comp_tpda) = lo_table_descr->components( ).
*
*          DATA: lt_comp TYPE abap_component_tab,
*                ls_comp TYPE abap_componentdescr.
*
*          LOOP AT lt_comp_tpda INTO DATA(ls_comp_tpda).
*            REPLACE ALL OCCURRENCES OF '\TYPE-POOL=ABAP\TYPE=' IN ls_comp_tpda-abstypename WITH ''.
*            REPLACE ALL OCCURRENCES OF '\TYPE=' IN ls_comp_tpda-abstypename WITH ''.
*            REPLACE ALL OCCURRENCES OF '\TYPE-POOL=' IN ls_comp_tpda-abstypename WITH ''.
*
*            IF ls_comp_tpda-abstypename+0(3) = '%_T' OR
*              ls_comp_tpda-abstypename+0(11) = '\INTERFACE=' OR
*              ls_comp_tpda-abstypename+0(7) = '\CLASS='.
*              DATA(lv_old_generation) = abap_true.
*              EXIT.
*            ENDIF.
*
*            DATA(lo_descr) = cl_abap_typedescr=>describe_by_name( ls_comp_tpda-abstypename ).
*            IF lo_descr IS INSTANCE OF cl_abap_elemdescr.
*              DATA(lo_elem) = CAST cl_abap_elemdescr( lo_descr ).
*              CLEAR ls_comp.
*              ls_comp-name = ls_comp_tpda-compname.
*              ls_comp-type = lo_elem.
*              APPEND ls_comp TO lt_comp.
*            ENDIF.
*          ENDLOOP.
*
*          lv_old_generation = abap_true.
*          IF lv_old_generation IS INITIAL.
*            "--- создаём структуру на основе component_tab
*            DATA(lo_struct) = cl_abap_structdescr=>create( lt_comp ).
*
*            "--- создаём таблицу этой структуры
*            DATA(lo_table)  = cl_abap_tabledescr=>create( lo_struct ).
*
*            "--- создаём реальный объект таблицы
*            DATA lr_table TYPE REF TO data.
*            CREATE DATA lr_table TYPE HANDLE lo_table.
*
*            ASSIGN lr_table->* TO FIELD-SYMBOL(<lt_dyn>).
*          ENDIF.

          table_clone = lo_table_descr->elem_clone( ).


          "ASSIGN table_clone->* TO FIELD-SYMBOL(<f>).

*          IF lv_old_generation IS INITIAL.
*            MOVE-CORRESPONDING <f> TO <lt_dyn>.
*          ELSE.
          ASSIGN table_clone->* TO FIELD-SYMBOL(<lt_dyn>).
*          ENDIF.

          "check header area
          DATA td       TYPE sydes_desc.
          DESCRIBE FIELD <lt_dyn> INTO td.

          READ TABLE td-names INTO DATA(l_names) INDEX 1.
          IF sy-subrc = 0.
            TRY.
                CALL METHOD cl_tpda_script_data_descr=>get_quick_info
                  EXPORTING
                    p_var_name   = |{ i_name }-{ l_names-name }|
                  RECEIVING
                    p_symb_quick = DATA(l_quick).

                lo_tabl ?= cl_abap_typedescr=>describe_by_data( <lt_dyn> ).

                lo_struc ?= lo_tabl->get_table_line_type( ).
                CREATE DATA r_header TYPE HANDLE lo_struc.
                ASSIGN r_header->* TO FIELD-SYMBOL(<header>).

                traverse( io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( r_header )
                          iv_name        = l_name
                          iv_fullname    = i_name
                          iv_type        = iv_type
                          iv_parent_name = i_parent_name
                          i_instance     = i_instance
                          i_cl_leaf      = i_cl_leaf
                          ir_up          = r_header ).

                l_name = l_name && '[]'.
                l_full_name = i_name && '[]'.
              CATCH cx_tpda_varname .
            ENDTRY.
          ENDIF.
          GET REFERENCE OF <lt_dyn> INTO lr_struc.

          traverse( io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( lr_struc )
                    iv_name        = l_name
                    iv_fullname    = l_full_name
                    iv_type        = iv_type
                    i_instance     = i_instance
                    iv_parent_name = i_parent_name
                    i_cl_leaf      = i_cl_leaf
                    ir_up          = lr_struc ).

        ELSEIF m_quick-typid = 'l'. "data ref

          DATA: ls_info TYPE tpda_scr_quick_info.

          FIELD-SYMBOLS: <ls_symobjref> TYPE tpda_sys_symbdatref.
          ls_info = cl_tpda_script_data_descr=>get_quick_info( i_name ).
          ASSIGN ls_info-quickdata->* TO <ls_symobjref>.

          " Check if the referenced object exists
          IF <ls_symobjref>-instancename IS NOT INITIAL AND
             <ls_symobjref>-instancename <> '{R:initial}' AND
             <ls_symobjref>-instancename <> '{A:initial}'.

            TRY." Try to get info about the referenced object
                DATA(ls_ref_info) = cl_tpda_script_data_descr=>get_quick_info(
                  CONV #( <ls_symobjref>-instancename ) ).

                " Handle string references specially
                IF ls_ref_info-typid = 'g'. "string
                  " Create a string variable directly
                  APPEND INITIAL LINE TO mt_new_string ASSIGNING FIELD-SYMBOL(<m_string_ref>).
                  <m_string_ref> = create_simple_string( <ls_symobjref>-instancename ).
                  GET REFERENCE OF <m_string_ref> INTO m_variable.

                  traverse( io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( m_variable )
                            iv_name        = l_name
                            iv_type        = iv_type
                            iv_fullname    = i_name
                            iv_parent_name = i_parent_name
                            i_instance     = i_instance
                            i_cl_leaf      = i_cl_leaf
                            ir_up          = m_variable ).
                ELSE.
                  " Handle other reference types as before
                  transfer_variable( EXPORTING i_name = CONV #( <ls_symobjref>-instancename )
                                               iv_type = iv_type
                                               i_shortname = i_name
                                               i_parent_name = i_parent_name
                                               i_cl_leaf = i_cl_leaf
                                               i_instance = <ls_symobjref>-instancename ).
                ENDIF.
              CATCH cx_tpda_varname.
                " Handle error - show as unresolved reference
                APPEND INITIAL LINE TO mt_new_string ASSIGNING <m_string_ref>.
                <m_string_ref> = |Unresolved reference: { <ls_symobjref>-instancename }|.
                GET REFERENCE OF <m_string_ref> INTO m_variable.

                traverse( io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( m_variable )
                          iv_name        = l_name
                          iv_type        = iv_type
                          iv_fullname    = i_name
                          iv_parent_name = i_parent_name
                          i_instance     = i_instance
                          i_cl_leaf      = i_cl_leaf
                          ir_up          = m_variable ).
            ENDTRY.
          ENDIF.

        ELSEIF m_quick-typid = 'r'. "reference

          FIELD-SYMBOLS: <symobjref> TYPE tpda_sys_symbobjref.
          ASSIGN m_quick-quickdata->* TO <symobjref>.

          save_hist( EXPORTING iv_fullname    = i_name
                               iv_name        = i_shortname
                               iv_parent_name = i_parent_name
                               iv_type        = iv_type
                               iv_cl_leaf     = i_cl_leaf
                               i_instance     = <symobjref>-instancename ).

          create_reference( EXPORTING i_name      = l_name
                                      i_type      = iv_type
                                      i_shortname = l_name
                                      i_parent    = i_parent_name
                                      i_quick     = m_quick ).

        ELSEIF m_quick-typid = 'v' OR m_quick-typid = 'u'."deep structure or structure

          CALL METHOD cl_abap_complexdescr=>describe_by_name
            EXPORTING
              p_name         = m_quick-abstypename
            RECEIVING
              p_descr_ref    = deep_ref
            EXCEPTIONS
              type_not_found = 1.

          IF sy-subrc = 0.
            lo_deep_handle ?= deep_ref.
            CREATE DATA lr_struc TYPE HANDLE lo_deep_handle.
            get_deep_struc( EXPORTING i_name = i_name r_obj = lr_struc ).
            ASSIGN lr_struc->* TO FIELD-SYMBOL(<new_deep>).

            traverse( io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( lr_struc )
                      iv_name        = l_name
                      iv_fullname    = i_name
                      iv_type        = iv_type
                      iv_parent_name = i_parent_name
                      i_instance     = i_instance
                      i_cl_leaf      = i_cl_leaf
                      ir_up          = lr_struc ).
          ELSE.
            create_struc2( EXPORTING i_name = i_name i_shortname = l_name ).
          ENDIF.

        ELSEIF m_quick-typid = 'g'."string
          APPEND INITIAL LINE TO mt_new_string ASSIGNING FIELD-SYMBOL(<m_string>).
          IF i_name NE '{A:initial}'.
            <m_string> = create_simple_string( i_name ).
          ELSE.
            <m_string> = '{A:initial}'.
          ENDIF.
          GET REFERENCE OF <m_string> INTO m_variable.
          traverse( io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( m_variable )
                    iv_name        = l_name
                    iv_type        = iv_type
                    iv_fullname    = i_name
                    iv_parent_name = i_parent_name
                    i_instance     = i_instance
                    i_cl_leaf      = i_cl_leaf
                    ir_up          = m_variable ).
        ELSE.
          lr_struc = create_simple_var( i_name ).
          ASSIGN lr_struc->* TO FIELD-SYMBOL(<new_elem>).

          traverse( io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( lr_struc )
                    iv_name        = l_name
                    iv_fullname    = i_name
                    iv_type        = iv_type
                    iv_parent_name = i_parent_name
                    ir_up          = lr_struc
                    i_cl_leaf      = i_cl_leaf
                    i_instance     = i_instance ).
        ENDIF.
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD get_class_name.

    DATA: lo_object TYPE REF TO cl_tpda_script_objectdescr,
          lo_descr  TYPE REF TO cl_tpda_script_data_descr.

    FIELD-SYMBOLS: <ls_symobjref> TYPE tpda_sys_symbobjref.

    TRY.
        CALL METHOD cl_tpda_script_data_descr=>get_quick_info
          EXPORTING
            p_var_name   = i_name
          RECEIVING
            p_symb_quick = DATA(l_quick).

        ASSIGN l_quick-quickdata->* TO <ls_symobjref>.
        IF <ls_symobjref>-instancename <> '{O:initial}'.

          lo_descr = cl_tpda_script_data_descr=>factory( <ls_symobjref>-instancename ).
          lo_object ?= lo_descr.

          e_name = lo_object->classname( ).
        ENDIF.
      CATCH cx_tpda_varname .
    ENDTRY.

  ENDMETHOD.

  METHOD create_reference.

    DATA: ls_obj        LIKE LINE OF mt_obj,
          lr_struc      TYPE REF TO data,
          lo_object     TYPE REF TO cl_tpda_script_objectdescr,
          lo_descr      TYPE REF TO cl_tpda_script_data_descr,
          lt_attributes TYPE tpda_script_object_attribut_it.

    FIELD-SYMBOLS: <ls_symobjref> TYPE tpda_sys_symbobjref.
    ASSIGN i_quick-quickdata->* TO <ls_symobjref>.
    IF <ls_symobjref>-instancename <> '{O:initial}'.

      ls_obj-name = i_name.
      ls_obj-obj = <ls_symobjref>-instancename.
      COLLECT ls_obj INTO mt_obj.

      TRY.
          lo_descr = cl_tpda_script_data_descr=>factory( <ls_symobjref>-instancename ).
          lo_object ?= lo_descr.

          lt_attributes = lo_object->attributes( ).
          DELETE lt_attributes WHERE instantiation = 1.
          SORT lt_attributes BY acckind name.

          DATA(lv_name) = lo_object->classname( ).
          DATA(lv_obj_ind) =  get_obj_index( <ls_symobjref>-instancename ).

          READ TABLE mt_classes_types WITH KEY full = lv_obj_ind TRANSPORTING NO FIELDS.
          IF sy-subrc NE 0.
            LOOP AT lt_attributes ASSIGNING FIELD-SYMBOL(<ls_attribute>).
              AT NEW acckind.
                APPEND INITIAL LINE TO mt_classes_types ASSIGNING FIELD-SYMBOL(<cl_type>).
                <cl_type>-name = i_name.
                <cl_type>-full = lv_obj_ind.
                <cl_type>-type = <ls_attribute>-acckind.
              ENDAT.
            ENDLOOP.
          ENDIF.

          DATA: lv_parent TYPE string.
          IF i_parent IS NOT INITIAL.
            lv_parent = |{ i_parent }-{ i_name }|.
          ELSE.
            lv_parent = i_name.
          ENDIF.

          LOOP AT lt_attributes ASSIGNING <ls_attribute>.

            transfer_variable( EXPORTING i_name        = |{ <ls_symobjref>-instancename  }-{ <ls_attribute>-name }|
                                         i_shortname   = <ls_attribute>-name
                                         iv_type       = i_type
                                         i_instance    = <ls_symobjref>-instancename
                                         i_cl_leaf     = <ls_attribute>-acckind
                                         i_parent_name = lv_parent ).

            READ TABLE mt_state WITH KEY path = |{ lv_parent  }-{ <ls_attribute>-name }| ASSIGNING FIELD-SYMBOL(<state>).
            IF sy-subrc = 0.
              <state>-cl_leaf = <ls_attribute>-acckind.
            ENDIF.
          ENDLOOP.
        CATCH cx_tpda_varname.
      ENDTRY.
    ENDIF.

  ENDMETHOD.

  METHOD create_struc2.

    DATA: lo_struc_descr TYPE REF TO cl_tpda_script_structdescr,
          lt_components  TYPE abap_component_tab,
          comp_full      TYPE  tpda_scr_struct_comp_it,
          ls_comp        TYPE abap_componentdescr,
          comp_it        TYPE tpda_script_struc_componentsit,
          structdescr    TYPE REF TO cl_abap_structdescr,
          r_data         TYPE REF TO data.

    FIELD-SYMBOLS: <str> TYPE any.

    lo_struc_descr ?= cl_tpda_script_data_descr=>factory( i_name ).
    lo_struc_descr->components( IMPORTING p_components_it = comp_it p_components_full_it = comp_full ).

    LOOP AT comp_it INTO DATA(l_comp).
      ls_comp-name = l_comp-compname.
      IF l_comp-typid = 'u'.
        r_data = create_struc( EXPORTING i_name = |{ l_comp-longname }| ).
      ELSE.
        r_data = create_simple_var( EXPORTING i_name = |{ l_comp-longname }| ).
      ENDIF.
      ASSIGN r_data->* TO FIELD-SYMBOL(<item>).

      CALL METHOD cl_abap_complexdescr=>describe_by_data
        EXPORTING
          p_data      = <item>
        RECEIVING
          p_descr_ref = DATA(lo_type).

      ls_comp-type ?= lo_type.
      APPEND ls_comp TO lt_components.
    ENDLOOP.

    structdescr = cl_abap_structdescr=>create( lt_components ).
    CREATE DATA r_data TYPE HANDLE structdescr.
    ASSIGN r_data->* TO <str>.

    get_deep_struc( EXPORTING i_name = i_name r_obj = r_data ).
    ASSIGN r_data->* TO FIELD-SYMBOL(<new_deep>).

  ENDMETHOD.

  METHOD script.

    run_script( ).
    show_step( ).
    me->break( ).

  ENDMETHOD.

  METHOD run_script_hist.

    DATA: lt_hist      LIKE mt_vars_hist_view,
          lv_hist_step TYPE i,
          lv_old_step  TYPE i.

    CLEAR mv_recurse.
    is_history = abap_true.
    IF m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.
    IF iv_step IS NOT INITIAL.
      lv_hist_step = iv_step.
      READ TABLE mt_steps WITH KEY step = iv_step INTO DATA(ls_steps).

    ELSE.
      IF ( mo_window->m_debug_button = 'F6BEG' OR mo_window->m_debug_button = 'F6END' ) AND m_target_stack IS INITIAL.
        READ TABLE mt_steps INTO ls_steps INDEX m_hist_step.
        m_target_stack = ls_steps-stacklevel.
      ENDIF.

      IF mo_window->m_direction IS NOT INITIAL AND m_hist_step = 1 AND mo_window->m_debug_button IS NOT INITIAL.
        es_stop = abap_true.
      ENDIF.

      IF mo_window->m_direction IS INITIAL AND m_hist_step = m_step AND mo_window->m_debug_button IS NOT INITIAL.
        es_stop = abap_true.
      ENDIF.

      lv_old_step = m_hist_step.
      IF mo_window->m_direction IS NOT INITIAL AND m_hist_step > 1 AND mo_window->m_debug_button IS NOT INITIAL.
        SUBTRACT 1 FROM m_hist_step.
      ENDIF.

      IF mo_window->m_direction IS INITIAL AND m_hist_step < m_step AND mo_window->m_debug_button IS NOT INITIAL.
        ADD 1  TO m_hist_step.
      ENDIF.

      lv_hist_step = m_hist_step.

      READ TABLE mt_steps INTO ls_steps WITH KEY step =  m_hist_step.
      READ TABLE mt_steps INTO DATA(ls_step_old) WITH KEY step =  lv_old_step.

      IF ls_steps-stacklevel <> ls_step_old-stacklevel.
        m_refresh = abap_true.
      ENDIF.

      mo_window->set_program( CONV #( ls_steps-include ) ).
      mo_window->set_program_line( ls_steps-line ).

      IF ( mo_window->m_debug_button = 'F6BEG' OR mo_window->m_debug_button = 'F6END' ) AND m_target_stack =  ls_steps-stacklevel.
        CLEAR m_target_stack.
        es_stop = abap_true.
      ENDIF.

      READ TABLE mo_window->mt_stack INTO DATA(ls_stack) INDEX 1.

      MOVE-CORRESPONDING ls_stack TO mo_window->m_prg.

      IF mo_window->m_debug_button = 'F6' AND m_stop_stack IS INITIAL.
        m_stop_stack = ls_stack-stacklevel.
      ENDIF.

    ENDIF.

    IF mo_window->m_debug_button = 'F5'.
      es_stop = abap_true.
    ENDIF.

    IF mo_window->m_debug_button = 'F6' AND m_stop_stack = ls_stack-stacklevel.

      es_stop = abap_true.
      CLEAR m_stop_stack.
    ENDIF.

    IF ( mo_window->m_debug_button = 'F6BEG' AND ls_steps-first = abap_true AND m_target_stack = ls_stack-stacklevel ) OR
       ( mo_window->m_debug_button = 'F6END' AND ls_steps-last = abap_true  AND m_target_stack = ls_stack-stacklevel ).
      CLEAR m_target_stack.
      es_stop = abap_true.
    ENDIF.

    IF iv_step IS INITIAL.
      READ TABLE mo_window->mt_breaks WITH KEY inclnamesrc = ls_steps-include linesrc = ls_steps-line INTO DATA(ls_break).
      IF sy-subrc = 0.

        es_stop = abap_true.
      ENDIF.
    ENDIF.

    IF  iv_step IS NOT INITIAL.
      es_stop = abap_true.
    ENDIF.

    IF es_stop = abap_true.

      "history state find refactoring
      DATA(lt_vars_hist) = mt_vars_hist.
      SORT lt_vars_hist BY step ASCENDING first DESCENDING.

      CLEAR lt_hist.

      LOOP AT mt_steps INTO DATA(ls_hist_step) WHERE step <= lv_hist_step.
        IF ls_hist_step-stacklevel < ls_steps-stacklevel.
          DELETE lt_hist WHERE leaf = 'LOCAL'.
        ENDIF.
        LOOP AT lt_vars_hist INTO DATA(ls_hist) WHERE step = ls_hist_step-step.
          IF m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.
          IF  mo_tree_local->m_globals IS INITIAL AND ls_hist-leaf = 'GLOBAL' OR ls_hist-program <> ls_steps-program.
            CONTINUE.
          ENDIF.
          IF  mo_tree_local->m_class_data IS INITIAL AND ls_hist-leaf = 'CLASS'.
            CONTINUE.
          ENDIF.
          IF ( ls_hist-leaf = 'LOCAL' OR ls_hist-leaf = 'IMP' OR ls_hist-leaf = 'EXP' ) AND ls_hist-stack <> ls_steps-stacklevel.
            CONTINUE.
          ENDIF.

          IF ls_hist-step = lv_hist_step AND ls_hist-first IS INITIAL.
            CONTINUE.
          ENDIF.

          IF ls_hist-del IS INITIAL.
            READ TABLE lt_hist WITH KEY name = ls_hist-name ASSIGNING FIELD-SYMBOL(<hist>).
            IF sy-subrc = 0.
              <hist> = ls_hist.
              CLEAR <hist>-done.
            ELSE.
              "check initial.
              IF mo_tree_local->m_hide IS NOT INITIAL.
                ASSIGN ls_hist-ref->* TO FIELD-SYMBOL(<new>).
                IF <new> IS NOT INITIAL.
                  APPEND INITIAL LINE TO lt_hist ASSIGNING <hist>.
                  <hist> = ls_hist.
                  CLEAR <hist>-done.
                ENDIF.
              ELSE.
                APPEND INITIAL LINE TO lt_hist ASSIGNING <hist>.
                <hist> = ls_hist.
                CLEAR <hist>-done.
              ENDIF.
            ENDIF.
          ELSE.
            IF m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.
            mo_tree_local->clear( ).
          ENDIF.
        ENDLOOP.
      ENDLOOP.
      IF m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.

      SORT lt_hist BY name.

      IF ls_step_old-stacklevel <> ls_steps-stacklevel OR m_refresh = abap_true.
        mo_tree_local->clear( ).
        mo_tree_exp->clear( ).
        mo_tree_imp->clear( ).
      ENDIF.


      IF lt_hist IS NOT INITIAL.
        show_variables( CHANGING it_var = lt_hist ).

        set_selected_vars( ).
        CLEAR m_refresh.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD run_script.

    DATA: lv_type TYPE string.
    ADD 1 TO m_counter.
    TRY.
        cl_tpda_script_abapdescr=>get_abap_src_info( IMPORTING p_prg_info = mo_window->m_prg ).
        DATA(lt_stack) = cl_tpda_script_abapdescr=>get_abap_stack( ).
        READ TABLE mo_window->mt_stack INDEX 1 INTO ms_stack_prev.

        MOVE-CORRESPONDING lt_stack TO mo_window->mt_stack.
        READ TABLE mo_window->mt_stack INDEX 1 ASSIGNING FIELD-SYMBOL(<stack>).
        <stack>-step = m_step.
        ms_stack = <stack>.

        CALL METHOD cl_tpda_script_bp_services=>get_all_bps RECEIVING p_bps_it = mo_window->mt_breaks.

        IF is_step = abap_true.
          ADD 1 TO m_step.
          m_hist_step = m_step.
          GET TIME.
          "add missed ELSE/ENDIF/ENDCASE
          READ TABLE mo_window->mt_source WITH KEY include = ms_stack-include INTO DATA(ls_source).
          READ TABLE ls_source-t_keywords WITH KEY line = ms_stack-line INTO DATA(ls_key).
          READ TABLE ls_source-t_keywords INDEX sy-tabix - 1 INTO ls_key.
          READ TABLE mt_steps INDEX m_step - 1 INTO DATA(ls_step).
          READ TABLE ls_source-t_keywords WITH KEY line = ls_step-line INTO DATA(ls_key_prev).

          IF ls_key_prev-name <> 'DO' AND ls_key_prev-name <> 'LOOP' AND ls_key_prev-name <> 'WHILE'.
            IF ls_key-name = 'ELSE' OR ls_key-name = 'ENDIF' OR ls_key-name = 'ENDCASE'.
              APPEND INITIAL LINE TO mt_steps ASSIGNING FIELD-SYMBOL(<step>).
              MOVE-CORRESPONDING ms_stack TO <step>.
              <step>-line = ls_key-line.
              <step>-step = m_step.
              ADD 1 TO m_step.
            ENDIF.
          ENDIF.

          APPEND INITIAL LINE TO mt_steps ASSIGNING <step>.
          MOVE-CORRESPONDING ms_stack TO <step>.
          <step>-time = sy-uzeit.
          <step>-step = m_step.
          CLEAR is_step.
          IF mv_stack_changed = abap_true AND  ms_stack_prev-stacklevel < ms_stack-stacklevel.
            <step>-first = abap_true.
          ENDIF.
          IF mo_window->m_prg-flag_eoev = abap_true.
            <step>-last = abap_true.
          ENDIF.
        ENDIF.

        IF mo_window->m_prg-program NE mo_tree_local->m_prg_info-program OR
          mo_window->m_prg-event-eventname NE mo_tree_local->m_prg_info-event-eventname OR
          mo_window->m_prg-event-eventtype NE mo_tree_local->m_prg_info-event-eventtype.

          CLEAR: m_step_delta,
                 mt_ret_exp,
                 mt_obj,
                 mt_ret_exp.

          mv_stack_changed = abap_true.

          DATA: lv_step TYPE i.
          lv_step = m_step - 1.
          IF mo_window->m_varhist IS NOT INITIAL.
            mo_tree_local->clear( ).
            mo_tree_exp->clear( ).
            mo_tree_imp->clear( ).
            IF ms_stack_prev-program <> ms_stack-program.
              CLEAR mt_state.
            ELSE.
              DELETE mt_state WHERE leaf NE 'GLOBAL'. "AND leaf NE 'SYST'.
            ENDIF.
          ENDIF.
        ELSE.
          CLEAR mv_stack_changed.
          m_step_delta = 1.
        ENDIF.
      CATCH cx_tpda_src_info.
    ENDTRY.

    mo_tree_local->m_prg_info = mo_window->m_prg.

    IF mo_window->m_version IS INITIAL.
      DATA: lv_optimize TYPE xfeld.
      READ TABLE mo_window->mt_source WITH KEY include = ms_stack_prev-include INTO ls_source.
      IF sy-subrc = 0.
        READ TABLE ls_source-t_keywords WITH KEY line = ms_stack_prev-line INTO DATA(ls_oper).
        IF mv_stack_changed IS INITIAL.
          IF ls_oper-name = 'COMPUTE' OR ls_oper-name = 'SELECT' OR ls_oper-name = 'CLEAR' OR  ls_oper-name = 'LOOP' OR ls_oper-name = 'SORT'
             OR ls_oper-name = 'DELETE' OR ls_oper-name = 'READ' OR  ls_oper-name = 'CONCATENATE' OR ls_oper-name = 'CONDENSE'
             OR ls_oper-name = 'APPEND' OR ls_oper-name = 'MODIFY' OR  ls_oper-name = 'CREATE' OR ls_oper-name = 'SHIFT'
             OR ls_oper-name = 'ASSIGN' OR ls_oper-name = 'UNASSIGN' OR ls_oper-name = 'TRANSLATE' OR  ls_oper-name = 'REPLACE'
             OR  ls_oper-name = 'ADD' OR  ls_oper-name = 'SUBTRACT'.
            lv_optimize = abap_true.

            IF ls_oper-name = 'UNASSIGN'.
              mo_tree_local->clear( ).
              mo_tree_exp->clear( ).
              mo_tree_imp->clear( ).
              DELETE mt_state WHERE leaf NE 'GLOBAL'.
            ENDIF.

          ENDIF.
        ENDIF.
      ELSE.
        lcl_source_parser=>parse_tokens( iv_program = mo_window->m_prg-include io_debugger = me ).
        READ TABLE mo_window->mt_source WITH KEY include = ms_stack-include INTO ls_source.
      ENDIF.
    ENDIF.

    IF mo_window->m_varhist IS NOT INITIAL.
      IF mo_tree_local->m_globals IS NOT INITIAL AND mo_tree_local->m_ldb IS NOT INITIAL.
        DATA: l_name(40),
              lt_inc       TYPE TABLE OF  d010inc.
        l_name = abap_source->program( ).

        CALL FUNCTION 'RS_PROGRAM_INDEX'
          EXPORTING
            pg_name      = l_name
          TABLES
            compo        = mt_compo
            inc          = lt_inc
          EXCEPTIONS
            syntax_error = 1
            OTHERS       = 2.
      ENDIF.

      IF mv_stack_changed = abap_true.
        IF mo_tree_local->m_locals IS NOT INITIAL.
          READ TABLE mo_window->mt_locals_set WITH KEY program = ms_stack-program
                                                       eventname = ms_stack-eventname
                                                       eventtype = ms_stack-eventtype
             INTO DATA(ls_local_set).

          IF sy-subrc = 0 AND ls_local_set-loc_fill = abap_true.
            mt_locals = ls_local_set-locals_tab.
          ELSE.
            CALL METHOD cl_tpda_script_data_descr=>locals RECEIVING p_locals_it = mt_locals.

            IF ms_stack-eventtype = 'METHOD'.
              APPEND INITIAL LINE TO mt_locals ASSIGNING FIELD-SYMBOL(<loc>).
              <loc>-name = 'ME'.
            ENDIF.
            IF ms_stack-eventtype = 'FUNCTION'.
              DATA: lv_fname              TYPE rs38l_fnam,
                    lt_exception_list     TYPE TABLE OF  rsexc,
                    lt_export_parameter   TYPE TABLE OF  rsexp,
                    lt_import_parameter   TYPE TABLE OF  rsimp,
                    lt_changing_parameter TYPE TABLE OF    rscha,
                    lt_tables_parameter   TYPE TABLE OF    rstbl.

              lv_fname = ms_stack-eventname.
              CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE'
                EXPORTING
                  funcname           = lv_fname
                TABLES
                  exception_list     = lt_exception_list
                  export_parameter   = lt_export_parameter
                  import_parameter   = lt_import_parameter
                  changing_parameter = lt_changing_parameter
                  tables_parameter   = lt_tables_parameter
                EXCEPTIONS
                  error_message      = 1
                  function_not_found = 2
                  invalid_name       = 3
                  OTHERS             = 4.
              IF sy-subrc = 0.
                LOOP AT lt_export_parameter INTO DATA(ls_exp).
                  APPEND INITIAL LINE TO mt_locals ASSIGNING <loc>.
                  <loc>-name = ls_exp-parameter.
                  <loc>-parkind = 2.
                ENDLOOP.
                LOOP AT lt_import_parameter INTO DATA(ls_imp).
                  APPEND INITIAL LINE TO mt_locals ASSIGNING <loc>.
                  <loc>-name = ls_imp-parameter.
                  <loc>-parkind = 1.
                ENDLOOP.
                LOOP AT lt_changing_parameter INTO DATA(ls_change).
                  APPEND INITIAL LINE TO mt_locals ASSIGNING <loc>.
                  <loc>-name = ls_change-parameter.
                  <loc>-parkind = 2.
                ENDLOOP.
                LOOP AT lt_tables_parameter INTO DATA(ls_table).
                  APPEND INITIAL LINE TO mt_locals ASSIGNING <loc>.
                  <loc>-name = ls_table-parameter.
                  <loc>-parkind = 2.
                ENDLOOP.
              ENDIF.
            ENDIF.

            IF mo_window->m_prg-event-eventtype = 'FORM'.
              LOOP AT ls_source-t_params INTO DATA(ls_params) WHERE name = mo_window->m_prg-event-eventname AND class IS INITIAL.
                READ TABLE mt_locals WITH KEY name = ls_params-param ASSIGNING FIELD-SYMBOL(<local>).
                IF sy-subrc = 0.
                  IF ls_params-type = 'I'.
                    <local>-parkind = '1'.
                  ELSEIF ls_params-type = 'E'.
                    <local>-parkind = '2'.
                  ENDIF.
                ENDIF.
              ENDLOOP.

              "get_form_parameters( i_prg = mo_window->m_prg i_form = ms_stack-eventname ).
            ENDIF.

            SORT mt_locals.

            ls_local_set-program = ms_stack-program.
            ls_local_set-eventname = ms_stack-eventname.
            ls_local_set-eventtype = ms_stack-eventtype.
            ls_local_set-loc_fill = abap_true.
            ls_local_set-locals_tab = mt_locals.
            APPEND ls_local_set TO mo_window->mt_locals_set.
          ENDIF.
        ENDIF.

        IF ( mo_tree_local->m_globals IS NOT INITIAL OR  mo_tree_local->m_ldb IS NOT INITIAL ) AND ms_stack_prev-program <> ms_stack-program.

          READ TABLE mo_window->mt_globals_set WITH KEY program = ms_stack-program INTO DATA(ls_global_set).

          IF sy-subrc = 0 AND ls_global_set-glob_fill = abap_true.
            mt_globals = ls_global_set-globals_tab.
          ELSE.

            CALL METHOD cl_tpda_script_data_descr=>globals RECEIVING p_globals_it = mt_globals.
            SORT mt_globals.
            IF mo_tree_local->m_globals IS NOT INITIAL AND  mo_tree_local->m_ldb IS NOT INITIAL.
              LOOP AT mt_globals ASSIGNING FIELD-SYMBOL(<global>).
                READ TABLE mt_compo WITH KEY name = <global>-name TRANSPORTING NO FIELDS.
                IF sy-subrc NE 0.
                  <global>-parisval = 'L'.
                ENDIF.
              ENDLOOP.
            ENDIF.
            ls_global_set-program = ms_stack-program.
            ls_global_set-globals_tab = mt_globals.
            ls_global_set-glob_fill = abap_true.
            APPEND ls_global_set TO mo_window->mt_globals_set.
          ENDIF.
        ENDIF.

        IF mo_tree_local->m_class_data IS NOT INITIAL.
          read_class_globals( ).
        ENDIF.

      ENDIF.

      DATA: lr_names TYPE RANGE OF string,
            lv_temp  TYPE char30.

      DATA(lt_globals) = mt_globals.
      DATA(lt_locals) = mt_locals.

      IF lv_optimize = abap_true AND m_update IS INITIAL.

        LOOP AT ls_source-t_calculated INTO DATA(ls_param) WHERE line = ms_stack_prev-line.
          lv_temp = ls_param-calculated.
          lr_names = VALUE #( BASE lr_names ( sign = 'I' option = 'EQ' low = lv_temp ) ).
        ENDLOOP.

        IF sy-subrc = 0.
          DELETE lt_globals WHERE name NOT IN lr_names.
          DELETE lt_locals WHERE name NOT IN lr_names.
        ENDIF.

      ENDIF.

      IF mo_tree_local->m_locals IS NOT INITIAL.
        LOOP AT lt_locals INTO DATA(ls_local).

          CASE ls_local-parkind.
            WHEN 0.
              lv_type = 'LOCAL'.
            WHEN 1.
              lv_type = 'IMP'.
            WHEN OTHERS.
              lv_type = 'EXP'.
          ENDCASE.

          transfer_variable( EXPORTING i_name = ls_local-name iv_type = lv_type ).
        ENDLOOP.

        READ TABLE mo_window->mt_locals_set
         WITH KEY program = ms_stack-program eventtype = ms_stack-eventtype eventname = ms_stack-eventname
         INTO DATA(ls_locals_set).
        LOOP AT ls_locals_set-mt_fs INTO DATA(ls_fs).
          transfer_variable( EXPORTING i_name = ls_fs-name iv_type = 'LOCAL' ).
        ENDLOOP.
      ENDIF.

      IF mo_tree_local->m_globals IS NOT INITIAL.

        LOOP AT lt_globals INTO DATA(ls_global)  WHERE parisval NE 'L'.
          transfer_variable( EXPORTING i_name = ls_global-name iv_type = 'GLOBAL' ).
        ENDLOOP.
        READ TABLE mo_window->mt_globals_set WITH KEY program = ms_stack-program INTO DATA(ls_globals_set).
        LOOP AT ls_globals_set-mt_fs INTO ls_fs.
          transfer_variable( EXPORTING i_name = ls_fs-name iv_type = 'GLOBAL' ).
        ENDLOOP.

      ENDIF.
    ENDIF.

    IF mo_tree_local->m_syst IS NOT INITIAL.
      transfer_variable( EXPORTING i_name = 'SYST' iv_type = 'SYST' ).
    ELSE.
      DELETE mo_tree_local->mt_vars WHERE leaf = 'SYST'.
      DELETE mt_state WHERE leaf = 'SYST'.
    ENDIF.

    IF mo_tree_local->m_ldb IS NOT INITIAL.
      LOOP AT lt_globals INTO ls_global WHERE parisval = 'L'.
        transfer_variable( EXPORTING i_name = ls_global-name iv_type = 'LDB' ).
      ENDLOOP.
    ENDIF.

    LOOP AT mt_state ASSIGNING FIELD-SYMBOL(<state>).
      CLEAR <state>-done.
    ENDLOOP.

    "check dependents variables.
    IF mt_selected_var IS NOT INITIAL.
      READ TABLE ls_source-t_keywords WITH KEY line = ms_stack_prev-line INTO DATA(ls_keyword).
      LOOP AT ls_keyword-tt_calls INTO DATA(ls_call) WHERE event = 'FORM' AND name =  ms_stack-eventname.
        READ TABLE mt_selected_var WITH KEY name = ls_call-outer TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO mt_selected_var ASSIGNING FIELD-SYMBOL(<selected>).
          <selected>-name = ls_call-inner.
        ENDIF.
      ENDLOOP.
    ENDIF.

    CLEAR: mo_window->m_show_step.
    mo_tree_imp->m_prg_info = mo_window->m_prg.

  ENDMETHOD.

  METHOD show_variables.

    FIELD-SYMBOLS: <hist> TYPE any,
                   <new>  TYPE any.

    DATA: l_rel   TYPE salv_de_node_relation,
          lv_key  TYPE salv_de_node_key,
          lo_tree TYPE REF TO  lcl_rtti_tree,
          is_skip TYPE xfeld.
    ADD 1 TO mv_recurse.
    IF mo_tree_local->m_clear = abap_true.
      mo_tree_local->clear( ).
      CLEAR mo_tree_local->m_clear.
    ENDIF.

    READ TABLE it_var WITH KEY del = abap_true TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      mo_tree_local->clear( ).
    ENDIF.

    mo_tree_imp->m_leaf =  'IMP'.
    mo_tree_exp->m_leaf =  'EXP'.

    IF mo_tree_local->m_locals_key IS NOT INITIAL AND mo_tree_local->m_locals IS INITIAL.
      mo_tree_local->delete_node( mo_tree_local->m_locals_key ).
      CLEAR mo_tree_local->m_locals_key.

      DELETE mo_tree_local->mt_vars WHERE leaf = 'LOCAL'.
      DELETE mt_state WHERE leaf = 'LOCAL'.
    ENDIF.

    IF mo_tree_local->m_globals_key IS NOT INITIAL AND mo_tree_local->m_globals IS INITIAL.
      mo_tree_local->delete_node( mo_tree_local->m_globals_key ).
      CLEAR mo_tree_local->m_globals_key.
      DELETE mo_tree_local->mt_vars WHERE leaf = 'GLOBAL'. "OR leaf = 'SYST'.
      DELETE mt_state WHERE leaf = 'GLOBAL'. "OR leaf = 'SYST'.
    ENDIF.

    IF mo_tree_local->m_class_key IS NOT INITIAL AND mo_tree_local->m_class_data IS INITIAL.
      mo_tree_local->delete_node( mo_tree_local->m_class_key ).
      DELETE mo_tree_local->mt_vars WHERE leaf = 'CLASS'.
      DELETE mt_state WHERE leaf = 'CLASS'.
    ENDIF.

    IF mo_tree_local->m_syst IS INITIAL.
      READ TABLE mo_tree_local->mt_vars WITH KEY name = 'SYST' INTO DATA(ls_var).
      IF sy-subrc = 0.
        mo_tree_local->delete_node( ls_var-key ).
        DELETE mo_tree_local->mt_vars WHERE leaf =  'SYST'.
        DELETE mt_state WHERE leaf = 'SYST'.
      ENDIF.
    ENDIF.

    IF mo_tree_local->m_ldb_key IS NOT INITIAL AND mo_tree_local->m_ldb IS INITIAL.
      mo_tree_local->delete_node( mo_tree_local->m_ldb_key ).
      CLEAR mo_tree_local->m_ldb_key.
      DELETE mo_tree_local->mt_vars WHERE leaf = 'LDB'.
      DELETE mt_state WHERE leaf = 'LDB'.
    ENDIF.

    l_rel = if_salv_c_node_relation=>last_child.

    LOOP AT it_var ASSIGNING FIELD-SYMBOL(<var>) WHERE done = abap_false.

      CASE <var>-leaf.
        WHEN 'LOCAL'.
          mo_tree_local->m_leaf =  'LOCAL'.
          IF mo_tree_local->m_locals_key IS INITIAL.
            mo_tree_local->add_node( iv_name = 'Locals' iv_icon = CONV #( icon_life_events ) ).
          ELSE.

            mo_tree_local->main_node_key = mo_tree_local->m_locals_key.
          ENDIF.
        WHEN 'GLOBAL'.
          mo_tree_local->m_leaf =  'GLOBAL'.
          IF mo_tree_local->m_globals_key IS INITIAL.
            mo_tree_local->add_node( iv_name = 'Globals' iv_icon = CONV #( icon_life_events ) ).
          ELSE.
            mo_tree_local->main_node_key = mo_tree_local->m_globals_key.
          ENDIF.
        WHEN 'LDB'.
          mo_tree_local->m_leaf =  'LDB'.
          IF mo_tree_local->m_ldb_key IS INITIAL.
            mo_tree_local->add_node( iv_name = 'LDB' iv_icon = CONV #( icon_life_events ) ).
          ELSE.
            mo_tree_local->main_node_key = mo_tree_local->m_ldb_key.
          ENDIF.

        WHEN 'SYST'.
          mo_tree_local->m_leaf =  'SYST'.
          IF mo_tree_local->m_syst_key IS INITIAL.
            mo_tree_local->add_node( iv_name = 'System variables' iv_icon = CONV #( icon_life_events ) ).
          ENDIF.
        WHEN 'CLASS'.
          mo_tree_local->m_leaf =  'CLASS'.
          IF mo_tree_local->m_class_key IS INITIAL.
            mo_tree_local->add_node( iv_name = 'Class-data global variables' iv_icon = CONV #( icon_life_events ) ).
          ENDIF.
      ENDCASE.

      READ TABLE mt_selected_var WITH KEY name = <var>-name ASSIGNING FIELD-SYMBOL(<sel>).
      IF sy-subrc = 0.

        IF <sel>-refval IS BOUND.
          ASSIGN <sel>-refval->* TO <hist>.
          ASSIGN <var>-ref->* TO <new>.

          IF <new> <> <hist>.
            <sel>-refval = <var>-ref.
            rv_stop = abap_true.
          ENDIF.
        ELSE.
          <sel>-refval = <var>-ref.
        ENDIF.
      ENDIF.

      CASE <var>-leaf.
        WHEN 'IMP'.
          lo_tree = mo_tree_imp.
        WHEN 'EXP'.
          lo_tree = mo_tree_exp.
        WHEN OTHERS.
          lo_tree = mo_tree_local.
      ENDCASE.

      IF <var>-parent IS NOT INITIAL.
        READ TABLE lo_tree->mt_vars WITH KEY path = <var>-parent TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          <var>-done = abap_true.
        ELSE.
          IF lo_tree->m_hide IS INITIAL.

            is_skip = abap_true.
            CONTINUE.
          ENDIF.
        ENDIF.
      ELSE.
        <var>-done = abap_true.
      ENDIF.

      READ TABLE lo_tree->mt_vars WITH KEY path = <var>-parent INTO ls_var.
      IF sy-subrc = 0.
        lv_key = ls_var-key.
      ELSE.
        lv_key = lo_tree->main_node_key.
      ENDIF.

      IF <var>-ref IS NOT INITIAL.
        lo_tree->traverse(
          io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( <var>-ref )
          iv_parent_key  = lv_key
          iv_rel         = l_rel
          is_var         = <var>
          ir_up          = <var>-ref
          iv_parent_name = CONV #( <var>-name ) ).
      ELSE.
        lo_tree->traverse_obj(
          iv_parent_key  = lv_key
          iv_rel         = l_rel
          is_var         = <var>
          ir_up          = <var>-ref
          iv_parent_name = CONV #( <var>-name ) ).
      ENDIF.

    ENDLOOP.

    IF is_skip = abap_true.
      CLEAR is_skip.
      IF mv_recurse < 5.
        show_variables( CHANGING it_var = it_var ).
      ENDIF.
      set_selected_vars( ).
    ENDIF.

  ENDMETHOD.

  METHOD set_selected_vars.

    DATA(lt_nodes) = mo_tree_local->tree->get_nodes( )->get_all_nodes( ).
    LOOP AT lt_nodes INTO DATA(ls_nodes).
      DATA(lv_name) = ls_nodes-node->get_text( ).
      READ TABLE mt_selected_var WITH KEY name = lv_name TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        ls_nodes-node->set_row_style( if_salv_c_tree_style=>emphasized_b ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD hndl_script_buttons.

    IF m_is_find = abap_true.
      rv_stop = abap_true.
      CLEAR m_is_find.
      RETURN.
    ENDIF.

    IF mo_window->m_debug_button = 'F5'.
      rv_stop = abap_true.

    ELSEIF mo_window->m_debug_button = 'F6'.
      IF m_f6_level IS NOT INITIAL AND m_f6_level = ms_stack-stacklevel OR mo_window->m_history IS INITIAL.
        CLEAR m_f6_level.
        rv_stop = abap_true.
      ENDIF.

    ELSEIF mo_window->m_debug_button = 'F6END'.
      IF mo_window->m_prg-flag_eoev IS NOT INITIAL AND m_target_stack = ms_stack-stacklevel.
        rv_stop = abap_true.
      ENDIF.
    ELSEIF mo_window->m_debug_button = 'F7'.

      IF m_target_stack = ms_stack-stacklevel.
        CLEAR m_target_stack.
        rv_stop = abap_true.
      ENDIF.

    ELSEIF mo_window->m_debug_button IS NOT INITIAL.
      READ TABLE mo_window->mt_breaks WITH KEY inclnamesrc = mo_window->m_prg-include linesrc = mo_window->m_prg-line INTO DATA(gs_break).
      IF sy-subrc = 0.
        rv_stop = abap_true.
      ELSE.

        IF mo_window->m_debug_button = 'F6BEG' AND m_target_stack = ms_stack-stacklevel.
          rv_stop = abap_true.
        ELSE.
          IF mo_window->m_history IS NOT INITIAL.
            IF ms_stack-stacklevel = mo_window->m_hist_depth +  mo_window->m_start_stack.
              "f6( )."to refactor
            ELSE.
              "f5( )."to refactor
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      rv_stop = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD end. "dummy method
  ENDMETHOD.

  METHOD f5.

    READ TABLE mo_window->mt_stack INTO DATA(stack) INDEX 1.

    IF mo_window->m_debug_button NE 'F5' AND mo_window->m_zcode IS NOT INITIAL.
      IF stack-program+0(1) NE 'Z' AND stack-program+0(5) NE 'SAPLZ' AND m_f6_level <> stack-stacklevel.
        f7( ).
        RETURN.
      ENDIF.
    ENDIF.

*    IF ( mo_window->m_debug_button = 'F6BEG' OR mo_window->m_debug_button = 'F6END' ) AND m_target_stack IS INITIAL.
*      m_target_stack = stack-stacklevel.
*    ENDIF.

    IF mo_window->m_debug_button = 'F7' AND m_target_stack IS INITIAL.
      m_target_stack = stack-stacklevel - 1.
    ENDIF.

    TRY.
        CALL METHOD debugger_controller->debug_step
          EXPORTING
            p_command = cl_tpda_script_debugger_ctrl=>debug_step_into.
        is_step = abap_true.
      CATCH cx_tpda_scr_rtctrl_status .
      CATCH cx_tpda_scr_rtctrl .
    ENDTRY.
    "step out and not save history for standard code if it is swithed off
    IF  mo_window->m_zcode IS NOT INITIAL.

      cl_tpda_script_abapdescr=>get_abap_src_info( IMPORTING p_prg_info = mo_window->m_prg ).
      DO.
        IF   mo_window->m_prg-program+0(1) <> 'Z' AND mo_window->m_prg-program+0(5) <> 'SAPLZ' .
          f7( ).
        ELSE.
          EXIT.
        ENDIF.
        cl_tpda_script_abapdescr=>get_abap_src_info( IMPORTING p_prg_info = mo_window->m_prg ).
      ENDDO.
    ENDIF.

    IF mv_f7_stop = abap_true.
      CLEAR m_counter.
      rv_stop = abap_true.
      m_is_find = abap_true.
    ENDIF.
    IF m_counter >= 50000."very deep history - to stop
      CLEAR m_counter.
      rv_stop = abap_true.
    ENDIF.

    IF m_counter MOD 10000 = 0.
      show_step( ).
    ENDIF.

    IF mo_window->m_debug_button = 'F5'.
      rv_stop = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD f6.

    TRY.
        CALL METHOD debugger_controller->debug_step
          EXPORTING
            p_command = cl_tpda_script_debugger_ctrl=>debug_step_over.
        is_step = abap_true.
      CATCH cx_tpda_scr_rtctrl_status .
      CATCH cx_tpda_scr_rtctrl .
    ENDTRY.

  ENDMETHOD.

  METHOD f7.

    TRY.
        CALL METHOD debugger_controller->debug_step
          EXPORTING
            p_command = cl_tpda_script_debugger_ctrl=>debug_step_out.
        is_step = abap_true.
      CATCH cx_tpda_scr_rtctrl_status .
      CATCH cx_tpda_scr_rtctrl .
    ENDTRY.

  ENDMETHOD.

  METHOD f8.

    TRY.
        CALL METHOD debugger_controller->debug_step
          EXPORTING
            p_command = cl_tpda_script_debugger_ctrl=>debug_continue.
        is_step = abap_true.
      CATCH cx_tpda_scr_rtctrl_status .
      CATCH cx_tpda_scr_rtctrl .
    ENDTRY.

  ENDMETHOD.

  METHOD make_step.

    DATA: lv_stop TYPE xfeld.

    READ TABLE mo_window->mt_stack INDEX 1 INTO DATA(ls_stack).
    IF mo_window->m_debug_button = 'F6' AND mo_window->m_history IS NOT INITIAL.
      m_f6_level = ls_stack-stacklevel.
    ENDIF.

    WHILE lv_stop IS INITIAL.

      CASE mo_window->m_debug_button.

        WHEN 'F5' OR 'F6END' OR 'F6BEG'.
          lv_stop = f5( ).
        WHEN 'F6'.
          IF mo_window->m_history IS INITIAL.
            lv_stop = f6( ).
          ELSE.
            lv_stop = f5( ).
          ENDIF.

        WHEN 'F7'.
          IF mo_window->m_history IS INITIAL.
            lv_stop = f7( ).
          ELSE.
            lv_stop = f5( ).
          ENDIF.

        WHEN 'F8'.
          IF mo_window->m_history IS INITIAL.
            lv_stop = f8( ).
          ELSE.

            IF ls_stack-stacklevel = mo_window->m_start_stack + mo_window->m_hist_depth.
              lv_stop = f6( ).
            ELSE.
              lv_stop = f5( ).
            ENDIF.
          ENDIF.

      ENDCASE.
      run_script( ).
      lv_stop = hndl_script_buttons( mv_stack_changed ).
      READ TABLE mo_window->mt_stack INDEX 1 INTO ls_stack.

    ENDWHILE.
    show_step( ).
    me->break( ).

  ENDMETHOD.

  METHOD get_obj_index.

    FIND FIRST OCCURRENCE OF '*' IN iv_name MATCH OFFSET DATA(lv_offset).
    e_index =  iv_name+0(lv_offset).

  ENDMETHOD.

  METHOD show_step.

    show_variables( CHANGING it_var = mt_state ).
    set_selected_vars( ).
    mo_window->set_program( CONV #( mo_window->m_prg-include ) ).
    mo_window->set_program_line( mo_window->m_prg-line ).
    mo_window->show_stack( ).
    mo_tree_imp->display( ).
    mo_tree_local->display( ).
    mo_tree_exp->display( ).
    IF mo_window->m_debug_button NE 'F5'.
      mo_window->m_show_step = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD read_class_globals.

    DATA: lt_compo_tmp TYPE TABLE OF scompo,
          l_class      TYPE seu_name.

    mo_tree_local->m_leaf = 'Class-data global variables'.
    IF mo_tree_local->m_class_data IS NOT INITIAL.

      "global classes
      CALL METHOD cl_tpda_script_abapdescr=>get_loaded_programs
        IMPORTING
          p_progs_it = DATA(lt_progs).

      DELETE lt_progs WHERE sys = abap_true.

      LOOP AT lt_progs INTO DATA(ls_prog) WHERE name+30(2) = 'CP' AND ( name+0(1) = 'Z' OR name+0(1) = 'Y' ) .

        CLEAR ls_prog-name+30(2).
        REPLACE ALL OCCURRENCES OF '=' IN ls_prog-name WITH ''.

        DATA refc TYPE REF TO cl_abap_objectdescr.
        CALL METHOD cl_abap_classdescr=>describe_by_name
          EXPORTING
            p_name         = ls_prog-name
          RECEIVING
            p_descr_ref    = DATA(ref)
          EXCEPTIONS
            type_not_found = 1
            OTHERS         = 2.

        refc ?= ref.

        READ TABLE mt_obj WITH KEY name = l_class TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
          APPEND INITIAL LINE TO mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
          <obj>-name = l_class.
        ENDIF.

        save_hist( EXPORTING iv_fullname    = CONV #( ls_prog-name )
                             iv_name        = CONV #( ls_prog-name )
                             iv_parent_name = ''
                             iv_type        = 'CLASS'
                             iv_cl_leaf     = 0
                             i_instance     = CONV #( ls_prog-name ) ).


        LOOP AT refc->attributes INTO DATA(ls_atr).
          transfer_variable( EXPORTING i_name =  CONV #( |{ ls_prog-name }=>{ ls_atr-name }| )
                             i_shortname = CONV #( ls_atr-name )
                             i_parent_name = CONV #( ls_prog-name )
                              iv_type = 'CLASS' ).
        ENDLOOP.
      ENDLOOP.

    ENDIF.

    lt_compo_tmp = mt_compo.
    DELETE lt_compo_tmp WHERE  type NE 'D'.

  ENDMETHOD.

  METHOD save_hist.

    DATA: lv_add        TYPE xfeld,
          lv_add_hist   TYPE xfeld,
          lv_name2(100),
          lv_full_name  TYPE string.

    CHECK m_hist_step = m_step AND mo_window->m_direction IS INITIAL.
    IF ir_up IS SUPPLIED.
      ASSIGN ir_up->* TO FIELD-SYMBOL(<ir_up>).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

    IF i_instance IS INITIAL.
      lv_full_name = iv_fullname.
    ELSE.
      IF iv_parent_name IS INITIAL.
        IF iv_name IS NOT INITIAL.
          lv_full_name = iv_name.
        ELSE.
          lv_full_name = iv_fullname.
        ENDIF.
      ELSE.
        IF iv_fullname+0(3) = '{O:'.
          lv_full_name = iv_fullname.
        ELSE.
          lv_full_name =  |{ iv_parent_name }-{ iv_name }|.
        ENDIF.
      ENDIF.
    ENDIF.
    IF i_instance IS INITIAL.
      READ TABLE mt_state
           WITH KEY name = lv_full_name
                    program = mo_window->mt_stack[ 1 ]-program
            ASSIGNING FIELD-SYMBOL(<state>).
    ELSE.
      READ TABLE mt_state
          WITH KEY name = lv_full_name
                   program = mo_window->mt_stack[ 1 ]-program
                   instance = i_instance
           ASSIGNING <state>.
    ENDIF.

    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO mt_state ASSIGNING <state>.
      <state>-stack = mo_window->mt_stack[ 1 ]-stacklevel.
      <state>-step  = m_step - m_step_delta.
      <state>-program   = mo_window->m_prg-program.
      <state>-eventtype = mo_window->m_prg-eventtype.
      <state>-eventname = mo_window->m_prg-eventname.
      <state>-name = lv_full_name.

      IF iv_name IS NOT INITIAL.
        <state>-short = iv_name.
      ELSE.
        <state>-short = iv_fullname.
      ENDIF.

      <state>-leaf = iv_type.
      <state>-is_appear = abap_true.
      <state>-parent = iv_parent_name.
      <state>-instance = i_instance.

      IF iv_parent_name IS NOT INITIAL.
        IF iv_name IS NOT INITIAL.
          <state>-path =  |{ iv_parent_name }-{ iv_name }|.
        ELSE.
          <state>-path =  |{ iv_parent_name }-{ iv_fullname }|.
        ENDIF.
      ELSE.
        IF i_instance IS INITIAL.
          <state>-path = iv_fullname.
        ELSE.
          IF iv_parent_name IS INITIAL.
            IF iv_name IS NOT INITIAL.
              <state>-path = iv_name.
            ELSE.
              <state>-path = iv_fullname.
            ENDIF.
          ELSE.

            IF iv_name IS NOT INITIAL.
              <state>-path =  |{ iv_parent_name }-{ iv_name }|.
            ELSE.
              <state>-path =  |{ iv_parent_name }-{ iv_fullname }|.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF m_hist_step > 1.
      <state>-step = m_hist_step - 1."m_step - m_step_delta.
    ELSE.
      <state>-step = m_hist_step.
    ENDIF.
    <state>-instance = i_instance.

    IF ir_up IS SUPPLIED.
      <state>-ref = ir_up.

      DATA(lo_elem) = cl_abap_typedescr=>describe_by_data_ref( <state>-ref ).

      lv_name2 = iv_fullname.

      IF lv_name2+0(2) NE '{O'.

        IF <state>-leaf NE 'GLOBAL' AND <state>-leaf NE 'CLASS'.
          READ TABLE mt_vars_hist_view
           WITH KEY stack = <state>-stack
                    name = iv_fullname
                    eventtype = <state>-eventtype
                    eventname = <state>-eventname
                    INTO DATA(lv_hist).
        ELSE.
          READ TABLE mt_vars_hist_view
           WITH KEY name = iv_fullname
                    INTO lv_hist.
        ENDIF.

        IF sy-subrc NE 0.
          lv_add_hist = lv_add = abap_on.
        ELSE.
          ASSIGN lv_hist-ref->* TO FIELD-SYMBOL(<hist>).

          IF <hist> NE <ir_up>.
            lv_add_hist = lv_add = abap_on.
          ENDIF.
        ENDIF.
      ELSE.
        READ TABLE mt_vars_hist_view WITH KEY name = <state>-name INTO lv_hist.
        IF sy-subrc = 0.
          ASSIGN lv_hist-ref->* TO <hist>.
          IF <hist> NE <ir_up>.
            lv_add_hist = lv_add = abap_on.
          ENDIF.
        ELSE.
          lv_add_hist = lv_add = abap_on.
        ENDIF.

      ENDIF.
      IF mv_stack_changed = abap_true.
        lv_add = abap_on.
      ENDIF.

      lo_elem = cl_abap_typedescr=>describe_by_data_ref( <state>-ref ).
      IF <state>-type IS INITIAL.
        <state>-type = lo_elem->absolute_name.
      ENDIF.

      IF lv_add = abap_on.

        CLEAR <state>-first.

        IF  ms_stack_prev-stacklevel IS INITIAL OR
         ms_stack-stacklevel > ms_stack_prev-stacklevel.
          <state>-first = 'X'.
        ENDIF.

        <state>-cl_leaf = iv_cl_leaf.
        INSERT <state> INTO mt_vars_hist_view INDEX 1.

        IF  lv_add_hist = abap_true.
          INSERT <state> INTO mt_vars_hist INDEX 1.
          READ TABLE mt_selected_var WITH KEY name = <state>-name TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            m_is_find = abap_true.
          ENDIF.
        ENDIF.

      ENDIF.
    ELSE. "main node without data
      IF m_hist_step > 1.
        <state>-step = m_hist_step - 1.
      ELSE.
        <state>-step = m_hist_step.
      ENDIF.

      READ TABLE mt_vars_hist WITH KEY name = <state>-name INTO DATA(ls_hist).
      IF sy-subrc = 0.
        IF <state>-instance <> ls_hist-instance.
          <state>-del = abap_true.
          INSERT <state> INTO mt_vars_hist INDEX 1.
        ENDIF.
      ELSE.
        <state>-first = 'X'.
        INSERT <state> INTO mt_vars_hist INDEX 1.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD traverse.

    "create new data
    DATA: lr_new   TYPE REF TO data,
          lr_struc TYPE REF TO data.

    FIELD-SYMBOLS: <new>      TYPE any,
                   <tab_from> TYPE ANY TABLE,
                   <tab_to>   TYPE STANDARD TABLE,
                   <ir_up>    TYPE any.
    ASSIGN ir_up->* TO <ir_up>.
    DESCRIBE FIELD ir_up TYPE DATA(lv_type).
    IF lv_type NE cl_abap_typedescr=>typekind_table.
      CREATE DATA lr_new LIKE <ir_up>.
      ASSIGN lr_new->*  TO <new>.
      ASSIGN ir_up->* TO <new>.
      GET REFERENCE OF <new> INTO lr_new.
    ELSE.
      ASSIGN ir_up->* TO <tab_from>.
      CREATE DATA lr_struc LIKE LINE OF <tab_from>.
      ASSIGN lr_struc->* TO FIELD-SYMBOL(<ls_record>).
      CREATE DATA lr_new LIKE STANDARD TABLE OF <ls_record>.
      ASSIGN lr_new->* TO <tab_to>.
      <tab_to> = <tab_from>.
    ENDIF.
    GET REFERENCE OF <new> INTO m_variable.

    DATA td TYPE sydes_desc.
    DESCRIBE FIELD ir_up INTO td.

    m_variable = lr_new.

    CASE io_type_descr->kind.
      WHEN c_kind-struct.
        IF iv_struc_name IS SUPPLIED.
          traverse_struct( io_type_descr  = io_type_descr
                           iv_name        = iv_name
                           iv_fullname    = iv_fullname
                           iv_type        = iv_type
                           ir_up          = ir_up
                           iv_parent_name = iv_parent_name
                           i_instance     = i_instance
                           i_cl_leaf      = i_cl_leaf
                           iv_struc_name  = iv_struc_name
                           i_suffix       = i_suffix ).
        ELSE.
          traverse_struct( io_type_descr  = io_type_descr
                           iv_name        = iv_name
                           iv_fullname    = iv_fullname
                           iv_type        = iv_type
                           ir_up          = ir_up
                           i_instance     = i_instance
                           i_cl_leaf      = i_cl_leaf
                           iv_parent_name = iv_parent_name ).
        ENDIF.

      WHEN c_kind-elem.
        traverse_elem( iv_name        = iv_name
                       iv_fullname    = iv_fullname
                       iv_type        = iv_type
                       ir_up          = ir_up
                       i_instance     = i_instance
                       i_cl_leaf      = i_cl_leaf
                       iv_parent_name = iv_parent_name ).

      WHEN c_kind-table.
        traverse_elem( iv_name        = iv_name
                       iv_fullname    = iv_fullname
                       iv_type        = iv_type
                       ir_up          = ir_up
                       i_instance     = i_instance
                       i_cl_leaf      = i_cl_leaf
                       iv_parent_name = iv_parent_name ).
    ENDCASE.

  ENDMETHOD.

  METHOD traverse_struct.

    DATA: lt_component    TYPE abap_component_tab,
          ls_component    LIKE LINE OF lt_component,
          lo_struct_descr TYPE REF TO cl_abap_structdescr,
          lv_string       TYPE string,
          lv_parent       TYPE string.

    lo_struct_descr ?= io_type_descr.

    IF  ( iv_struc_name IS SUPPLIED AND iv_struc_name IS NOT INITIAL ) OR iv_struc_name IS NOT SUPPLIED.
      IF iv_name IS NOT INITIAL.
        save_hist( EXPORTING ir_up          = ir_up
                             iv_fullname    = iv_fullname
                             iv_name        = iv_name
                             iv_type        = iv_type
                             iv_parent_name = iv_parent_name
                             iv_cl_leaf     = i_cl_leaf
                             i_instance     = i_instance ).

      ENDIF.
    ENDIF.

    lt_component = lo_struct_descr->get_components( ).

    LOOP AT lt_component INTO ls_component.
      IF ls_component-name IS INITIAL AND ls_component-suffix IS NOT INITIAL.
        DATA(lv_suffix) =  ls_component-suffix.
      ENDIF.

      IF i_suffix IS NOT INITIAL.
        ls_component-name = ls_component-name && i_suffix.
      ENDIF.
      DATA: lr_new_struc TYPE REF TO data.
      ASSIGN ir_up->* TO FIELD-SYMBOL(<up>).
      IF ls_component-name IS INITIAL.
        lr_new_struc = ir_up.
      ELSE.
        ASSIGN COMPONENT ls_component-name OF STRUCTURE <up> TO FIELD-SYMBOL(<new>).
        GET REFERENCE OF <new> INTO lr_new_struc.
      ENDIF.

      IF ls_component-name IS NOT INITIAL.
        lv_string = |{ iv_fullname }-{ ls_component-name }|.
      ELSE.
        lv_string = iv_fullname.
      ENDIF.

      TRY.
          CALL METHOD cl_tpda_script_data_descr=>get_quick_info
            EXPORTING
              p_var_name   = lv_string
            RECEIVING
              p_symb_quick = DATA(l_quick).
        CATCH cx_tpda_varname .
      ENDTRY.

      IF l_quick-typid = 'r'.
        DATA: lr_variable TYPE REF TO data. "need to refaktor
        lr_variable = m_variable.

        FIELD-SYMBOLS: <symobjref> TYPE tpda_sys_symbobjref.
        ASSIGN l_quick-quickdata->* TO <symobjref>.

        save_hist( EXPORTING iv_fullname    = lv_string
                             iv_name        = ls_component-name
                             iv_parent_name = iv_fullname
                             iv_type        = iv_type
                             iv_cl_leaf     = i_cl_leaf
                             i_instance     = <symobjref>-instancename ).

        create_reference( EXPORTING i_name      = lv_string
                                    i_type      = iv_type
                                    i_shortname = ls_component-name
                                    i_quick     = l_quick ).

        m_variable = lr_variable.
      ELSE.
        IF iv_name IS NOT INITIAL.
          IF iv_parent_name IS NOT INITIAL.
            lv_parent = |{ iv_parent_name }-{ iv_name }|.
          ELSE.
            lv_parent = iv_name.
          ENDIF.
        ELSE.
          lv_parent = iv_parent_name.
        ENDIF.
        traverse( io_type_descr  = ls_component-type
                  iv_name        = ls_component-name
                  iv_fullname    = lv_string
                  iv_type        = iv_type
                  ir_up          = lr_new_struc
                  iv_parent_name = lv_parent
                  iv_struc_name  = ls_component-name
                  i_cl_leaf      = i_cl_leaf
                  i_instance     = i_instance
                  i_suffix       = lv_suffix ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD traverse_elem.

    save_hist( EXPORTING ir_up          = ir_up
                         iv_fullname    = iv_fullname
                         iv_name        = iv_name
                         iv_parent_name = iv_parent_name
                         iv_type        = iv_type
                         iv_cl_leaf     = i_cl_leaf
                         i_instance     = i_instance ).

  ENDMETHOD.

ENDCLASS.                    "lcl_debugger_script IMPLEMENTATION

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD constructor.
    mo_debugger = io_debugger.
  ENDMETHOD.

  METHOD on_double_click.

    READ TABLE mo_debugger->mo_window->mt_stack INDEX row INTO DATA(ls_stack).
    "only for coverage stack selection should work.

    CHECK mo_debugger->mo_window->mt_coverage IS NOT INITIAL.

    "check if we have recorded steps for choosen stack level
    READ TABLE  mo_debugger->mt_steps WITH KEY program = ls_stack-program include = ls_stack-include TRANSPORTING NO FIELDS.
    CHECK sy-subrc = 0.

    MOVE-CORRESPONDING ls_stack TO mo_debugger->mo_window->m_prg.
    MOVE-CORRESPONDING ls_stack TO mo_debugger->ms_stack.

    mo_debugger->mo_window->show_coverage( ).
    mo_debugger->show_step( ).

  ENDMETHOD.
ENDCLASS.


CLASS lcl_types DEFINITION ABSTRACT.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF selection_display_s,
        ind         TYPE i,
        field_label TYPE lvc_fname,
        int_type(1),
        inherited   TYPE aqadh_type_of_icon,
        emitter     TYPE aqadh_type_of_icon,
        sign        TYPE tvarv_sign,
        opti        TYPE tvarv_opti,
        option_icon TYPE aqadh_type_of_icon,
        low         TYPE string,
        high        TYPE string,
        more_icon   TYPE aqadh_type_of_icon,
        range       TYPE aqadh_t_ranges,
        name        TYPE reptext,
        element     TYPE text60,
        domain      TYPE text60,
        datatype    TYPE string,
        length      TYPE i,
        transmitter TYPE REF TO lcl_data_transmitter,
        receiver    TYPE REF TO lcl_data_receiver,
        color       TYPE lvc_t_scol,
        style       TYPE lvc_t_styl,
      END OF selection_display_s,
      BEGIN OF t_sel_row,
        sign        TYPE tvarv_sign,
        opti        TYPE tvarv_opti,
        option_icon TYPE aqadh_type_of_icon,
        low         TYPE string, "aqadh_range_value,
        high        TYPE string, "aqadh_range_value,
        more_icon   TYPE aqadh_type_of_icon,
        range       TYPE aqadh_t_ranges,
      END OF t_sel_row.

    CLASS-DATA: mt_sel TYPE TABLE OF lcl_types=>selection_display_s.

ENDCLASS.

CLASS lcl_window IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mo_debugger = i_debugger.
    m_history = m_varhist =  m_zcode  = '01'.
    m_hist_depth = 9.

    mo_box = create( i_name = 'SDDE Simple Debugger Data Explorer beta v. 0.9' i_width = 1400 i_hight = 400 ).
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

  ENDMETHOD.

  METHOD add_toolbar_buttons.

    DATA: lt_button TYPE ttb_button,
          lt_events TYPE cntl_simple_events,
          ls_events LIKE LINE OF lt_events.

    lt_button  = VALUE #(
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
     ( COND #( WHEN lcl_appl=>is_mermaid_active = abap_true
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

    mo_toolbar->add_button_group( lt_button ).

*   Register events
    ls_events-eventid = cl_gui_toolbar=>m_id_function_selected.
    ls_events-appl_event = space.
    APPEND ls_events TO lt_events.

    mo_toolbar->set_registered_events( events = lt_events ).
    SET HANDLER me->hnd_toolbar FOR mo_toolbar.

  ENDMETHOD.

  METHOD set_program.

    lcl_source_parser=>parse_tokens( iv_program = iv_program io_debugger = mo_debugger ).
    READ TABLE mt_source WITH KEY include = iv_program INTO DATA(ls_source).
    IF sy-subrc = 0.
      mo_code_viewer->set_text( table = ls_source-source->lines ).
    ENDIF.

  ENDMETHOD.

  METHOD set_program_line.

    TYPES: lntab TYPE STANDARD TABLE OF i.
    DATA lt_lines TYPE lntab.

    "blue arrow - current line
    APPEND INITIAL LINE TO lt_lines ASSIGNING FIELD-SYMBOL(<line>).
    <line> = iv_line.
    mo_code_viewer->set_marker( EXPORTING marker_number = 7 marker_lines = lt_lines ).

    "breakpoints
    LOOP AT mt_breaks INTO DATA(ls_break) WHERE inclnamesrc = m_prg-include.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <line>.
      <line> = ls_break-linesrc.
    ENDLOOP.
    mo_code_viewer->set_marker( EXPORTING marker_number = 9 marker_lines = lt_lines ).

    "watchpoints or coverage
    CLEAR lt_lines.
    LOOP AT mt_watch INTO DATA(ls_watch).
      APPEND INITIAL LINE TO lt_lines ASSIGNING <line>.
      <line> = ls_watch-line.
    ENDLOOP.

    "coverage
    LOOP AT mt_coverage INTO DATA(ls_coverage).
      APPEND INITIAL LINE TO lt_lines ASSIGNING <line>.
      <line> = ls_coverage-line.
    ENDLOOP.

    mo_code_viewer->set_marker( EXPORTING marker_number = 2 marker_lines = lt_lines ).

    mo_code_viewer->select_lines( EXPORTING from_line = iv_line to_line = iv_line ).

  ENDMETHOD.

  METHOD create_code_viewer.

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

    mo_code_viewer->set_statusbar_mode( statusbar_mode = cl_gui_abapedit=>true ).
    mo_code_viewer->create_document( ).
    mo_code_viewer->set_readonly_mode( 1 ).

  ENDMETHOD.

  METHOD show_stack.
    IF mo_salv_stack IS INITIAL.

      cl_salv_table=>factory(
        EXPORTING
          r_container  = mo_tables_container
        IMPORTING
          r_salv_table = mo_salv_stack
        CHANGING
          t_table      = mt_stack ).

      DATA:  lo_column  TYPE REF TO cl_salv_column.

      DATA(lo_columns) = mo_salv_stack->get_columns( ).
      "lo_columns->set_optimize( 'X' ).

      lo_column ?= lo_columns->get_column( 'STEP' ).
      lo_column->set_output_length( '3' ).
      lo_column->set_short_text( 'STEP' ).

      "lo_column ?= lo_columns->get_column( 'STACKPOINTER' ).
      "lo_column->set_output_length( '5' ).

      lo_column ?= lo_columns->get_column( 'STACKLEVEL' ).
      lo_column->set_output_length( '5' ).

      lo_column ?= lo_columns->get_column( 'PROGRAM' ).
      lo_column->set_output_length( '30' ).

      lo_column ?= lo_columns->get_column( 'INCLUDE' ).
      lo_column->set_output_length( '40' ).

      lo_column ?= lo_columns->get_column( 'EVENTTYPE' ).
      lo_column->set_output_length( '20' ).

      lo_column ?= lo_columns->get_column( 'EVENTNAME' ).
      lo_column->set_output_length( '50' ).

      DATA(lo_event) =  mo_salv_stack->get_event( ).

      DATA(lo_handler) = NEW lcl_event_handler( mo_debugger ).

      " Event double click
      SET HANDLER lo_handler->on_double_click FOR lo_event.

      mo_salv_stack->display( ).
    ELSE.
      mo_salv_stack->refresh( ).
    ENDIF.

  ENDMETHOD.

  METHOD show_coverage.

    CLEAR: mt_watch, mt_coverage,mt_stack.
    LOOP AT mo_debugger->mt_steps INTO DATA(ls_step).

      READ TABLE mt_stack WITH KEY include = ls_step-include TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO mt_stack ASSIGNING FIELD-SYMBOL(<stack>).
        MOVE-CORRESPONDING ls_step TO <stack>.
      ENDIF.

      IF ls_step-include <> mo_debugger->mo_window->m_prg-include.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO mt_coverage ASSIGNING FIELD-SYMBOL(<fs_coverage>).
      <fs_coverage>-line = ls_step-line.
    ENDLOOP.

    SORT mt_coverage.
    DELETE ADJACENT DUPLICATES FROM mt_coverage.

  ENDMETHOD.

  METHOD hnd_toolbar.

    CONSTANTS: c_mask TYPE x VALUE '01'.
    FIELD-SYMBOLS: <fs_any> TYPE any.
    m_debug_button = fcode.
    READ TABLE mt_stack INDEX 1 INTO DATA(ls_stack).
    CASE fcode.

      WHEN 'AI'.

        READ TABLE mo_debugger->mo_window->mt_source INDEX 1 INTO DATA(ls_source).
        NEW lcl_ai( ls_source-source ).

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
        DATA(lo_mermaid) = NEW lcl_mermaid( io_debugger = mo_debugger iv_type =  'DIAG' ).

      WHEN 'SMART'.
        lo_mermaid = NEW lcl_mermaid( io_debugger = mo_debugger iv_type =  'SMART' ).
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

        DATA(lt_nodes) = mo_debugger->mo_tree_local->tree->get_nodes( )->get_all_nodes( ).
        LOOP AT lt_nodes INTO DATA(ls_nodes).
          ls_nodes-node->set_row_style( if_salv_c_tree_style=>default ).
        ENDLOOP.
        mo_debugger->run_script_hist( mo_debugger->m_hist_step ).
        mo_debugger->mo_tree_local->display( ).
        RETURN.
      WHEN 'DEBUG'."activate break_points
        mo_debugger->m_debug = mo_debugger->m_debug BIT-XOR c_mask.

      WHEN 'INFO'.
        DATA(l_url) = 'https://ysychov.wordpress.com/2020/07/27/abap-simple-debugger-data-explorer/'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.

        l_url = 'https://github.com/ysichov/Smart-Debugger'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.


      WHEN 'STEPS'.

        lcl_appl=>open_int_table( iv_name = 'Steps' it_tab = mo_debugger->mt_steps io_window = mo_debugger->mo_window ).

      WHEN 'HISTORY'.
        DATA: lt_hist TYPE TABLE OF lcl_appl=>var_table_temp.
        LOOP AT  mo_debugger->mt_vars_hist INTO DATA(ls_vars).
          APPEND INITIAL LINE TO lt_hist ASSIGNING FIELD-SYMBOL(<hist>).
          MOVE-CORRESPONDING ls_vars TO <hist>.

          IF ls_vars-ref IS BOUND.
            DATA(lo_descr) = cl_abap_typedescr=>describe_by_data_ref( ls_vars-ref ).

            IF lo_descr->type_kind = cl_abap_typedescr=>typekind_table.
              <hist>-value = 'Table'.
            ELSEIF lo_descr->type_kind = cl_abap_typedescr=>typekind_struct1."structure
              <hist>-value = 'Structure'.
            ELSEIF lo_descr->type_kind = cl_abap_typedescr=>typekind_struct2."deep structure
              <hist>-value = 'Deep Structure'.
            ELSE.
              ASSIGN ls_vars-ref->* TO <fs_any>.
              IF sy-subrc = 0.
                <hist>-value = <fs_any>.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
        lcl_appl=>open_int_table( iv_name = |mt_vars_hist - History({ lines( lt_hist ) })| it_tab = lt_hist io_window = mo_debugger->mo_window ).

    ENDCASE.

    IF m_direction IS INITIAL AND mo_debugger->m_hist_step = mo_debugger->m_step.
      IF fcode = 'F8'.
        m_start_stack = ls_stack-stacklevel.

      ENDIF.
      CASE fcode.
        WHEN 'F5' OR 'F6' OR 'F6END' OR 'F6BEG' OR 'F7' OR 'F8'.

          IF fcode = 'F7'.
            mo_debugger->m_target_stack = ls_stack-stacklevel - 1.
          ENDIF.

          mo_debugger->make_step( ).
      ENDCASE.

    ELSE.
      CASE fcode.

        WHEN 'F5' OR 'F6' OR 'F7' OR 'F8' OR 'F6BEG' OR 'F6END'.
          DO.
            mo_debugger->run_script_hist( IMPORTING es_stop = DATA(lv_stop) ).

            IF lv_stop = abap_true.
              READ TABLE  mo_debugger->mt_steps INTO DATA(ls_step) INDEX mo_debugger->m_hist_step.
              set_program( CONV #( ls_step-include ) ).
              set_program_line( ls_step-line ).
              mo_debugger->mo_tree_imp->display( ).
              mo_debugger->mo_tree_local->display( ).
              mo_debugger->mo_tree_exp->display( ).
              RETURN.
            ENDIF.
          ENDDO.
      ENDCASE.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_sel_opt DEFINITION DEFERRED.

CLASS lcl_rtti IMPLEMENTATION.

  METHOD create_struc_handle.
    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = i_tname
                                         RECEIVING  p_descr_ref    = DATA(lo_descr)
                                         EXCEPTIONS type_not_found = 1 ).
    IF sy-subrc = 0.
      e_handle ?= lo_descr.
    ELSE.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD create_table_by_name.

    DATA: lo_new_tab  TYPE REF TO cl_abap_tabledescr,
          lo_new_type TYPE REF TO cl_abap_structdescr.

    create_struc_handle( EXPORTING i_tname = i_tname IMPORTING e_handle = lo_new_type ).
    lo_new_tab = cl_abap_tabledescr=>create(
      p_line_type  = lo_new_type
      p_table_kind = cl_abap_tabledescr=>tablekind_std
      p_unique     = abap_false ).
    CREATE DATA c_table TYPE HANDLE lo_new_tab.  "Create a New table type
  ENDMETHOD.

ENDCLASS.

CLASS lcl_data_transmitter DEFINITION.

  PUBLIC SECTION.
    EVENTS: data_changed EXPORTING VALUE(e_row) TYPE lcl_types=>t_sel_row,
      col_changed EXPORTING VALUE(e_column) TYPE lvc_fname.
    METHODS: emit IMPORTING e_row TYPE lcl_types=>t_sel_row,
      emit_col IMPORTING e_column TYPE lvc_fname.

ENDCLASS.

CLASS lcl_data_transmitter IMPLEMENTATION.

  METHOD  emit.
    RAISE EVENT data_changed EXPORTING e_row = e_row.

  ENDMETHOD.

  METHOD emit_col.
    RAISE EVENT col_changed EXPORTING e_column = e_column.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_data_receiver DEFINITION.

  PUBLIC SECTION.
    DATA: mo_transmitter TYPE REF TO lcl_data_transmitter,
          lo_tab_from    TYPE REF TO lcl_table_viewer,
          lo_sel_to      TYPE REF TO lcl_sel_opt,
          m_from_field   TYPE lvc_fname,
          m_to_field     TYPE lvc_fname.
    METHODS: constructor
      IMPORTING io_transmitter TYPE REF TO lcl_data_transmitter OPTIONAL
                io_tab_from    TYPE REF TO lcl_table_viewer OPTIONAL
                io_sel_to      TYPE REF TO lcl_sel_opt OPTIONAL
                i_from_field   TYPE lvc_fname OPTIONAL
                i_to_field     TYPE lvc_fname OPTIONAL,
      shut_down,
      update FOR EVENT data_changed OF lcl_data_transmitter IMPORTING e_row,
      update_col FOR EVENT col_changed OF lcl_data_transmitter IMPORTING e_column,
      on_grid_button_click
        FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING
          es_col_id
          es_row_no.

ENDCLASS.

CLASS lcl_sel_opt DEFINITION.

  PUBLIC SECTION.
    DATA: mo_debugger TYPE REF TO lcl_table_viewer,
          mo_sel_alv  TYPE REF TO cl_gui_alv_grid,
          mt_fcat     TYPE lvc_t_fcat,
          mt_sel_tab  TYPE TABLE OF lcl_types=>selection_display_s,
          ms_layout   TYPE lvc_s_layo.

    EVENTS: selection_done.
    METHODS:
      constructor IMPORTING io_viewer TYPE REF TO lcl_table_viewer io_container TYPE REF TO cl_gui_container,
      raise_selection_done,
      update_sel_tab,
      set_value IMPORTING  i_field TYPE any i_low TYPE any OPTIONAL i_high TYPE any OPTIONAL i_clear TYPE xfeld DEFAULT abap_true ,
      update_sel_row CHANGING c_sel_row TYPE lcl_types=>selection_display_s.

  PRIVATE SECTION.
    METHODS:
      init_fcat IMPORTING i_dd_handle TYPE i,
      handle_sel_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid IMPORTING e_fieldname es_row_no er_event_data,
      on_grid_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING
          es_col_id
          es_row_no,
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING  er_data_changed,
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      handle_doubleclick FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_column es_row_no,
      handle_context_menu_request FOR EVENT context_menu_request OF cl_gui_alv_grid IMPORTING e_object.

ENDCLASS.

CLASS lcl_table_viewer DEFINITION INHERITING FROM lcl_popup.

  PUBLIC SECTION.
    TYPES: BEGIN OF t_column_emitter,
             column  TYPE lvc_fname,
             emitter TYPE REF TO lcl_data_transmitter,
           END OF t_column_emitter,
           BEGIN OF t_elem,
             field TYPE fieldname,
             elem  TYPE ddobjname,
           END OF t_elem.

    DATA: m_lang             TYPE ddlanguage,
          m_tabname          TYPE tabname,
          mo_alv             TYPE REF TO cl_gui_alv_grid,
          mo_sel             TYPE REF TO lcl_sel_opt,
          mr_table           TYPE REF TO data,
          mo_sel_parent      TYPE REF TO cl_gui_container,
          mo_alv_parent      TYPE REF TO cl_gui_container,
          mt_alv_catalog     TYPE lvc_t_fcat,
          mt_fields          TYPE TABLE OF t_elem,
          mo_column_emitters TYPE TABLE OF t_column_emitter,
          mo_sel_width       TYPE i,
          m_visible,
          m_std_tbar         TYPE x,
          m_show_empty       TYPE i,
          mo_window          TYPE REF TO lcl_window.

    METHODS:
      constructor IMPORTING i_tname           TYPE any OPTIONAL
                            i_additional_name TYPE string OPTIONAL
                            ir_tab            TYPE REF TO data OPTIONAL
                            io_window         TYPE REF TO lcl_window,
      refresh_table FOR EVENT selection_done OF lcl_sel_opt.

  PRIVATE SECTION.
    METHODS:
      create_popup,
      create_alv,
      create_sel_alv,
      set_header,
      create_field_cat IMPORTING i_tname           TYPE tabname
                       RETURNING VALUE(et_catalog) TYPE lvc_t_fcat,
      translate_field IMPORTING i_lang TYPE ddlanguage CHANGING c_fld TYPE lvc_s_fcat,
      handle_tab_toolbar  FOR EVENT toolbar OF cl_gui_alv_grid  IMPORTING e_object,
      before_user_command FOR EVENT before_user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      handle_doubleclick FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_column es_row_no.

ENDCLASS.

CLASS lcl_text_viewer DEFINITION FINAL INHERITING FROM lcl_popup.

  PUBLIC SECTION.
    DATA: mo_text     TYPE REF TO cl_gui_textedit.
    METHODS: constructor IMPORTING ir_str TYPE REF TO data.
ENDCLASS.

CLASS lcl_text_viewer IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mo_box = create( i_name = 'text' i_width = 700 i_hight = 200 ).
    CREATE OBJECT mo_splitter
      EXPORTING
        parent  = mo_box
        rows    = 1
        columns = 1
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter->get_container(
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = mo_variables_container ).

    SET HANDLER on_box_close FOR mo_box.

    CREATE OBJECT mo_text
      EXPORTING
        parent                 = mo_variables_container
      EXCEPTIONS
        error_cntl_create      = 1
        error_cntl_init        = 2
        error_cntl_link        = 3
        error_dp_create        = 4
        gui_type_not_supported = 5
        OTHERS                 = 6.
    IF sy-subrc <> 0.
      on_box_close( mo_box ).
    ENDIF.

    mo_text->set_readonly_mode( ).
    FIELD-SYMBOLS <str> TYPE string.
    ASSIGN ir_str->* TO <str>.
    DATA lt_string TYPE TABLE OF char255.

    WHILE strlen( <str> ) > 255.
      APPEND <str>+0(255) TO lt_string.
      SHIFT <str> LEFT BY 255 PLACES.
    ENDWHILE.

    APPEND <str> TO lt_string.
    mo_text->set_text_as_r3table( lt_string ).
    CALL METHOD cl_gui_cfw=>flush.
    mo_text->set_focus( mo_box ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_data_receiver IMPLEMENTATION.

  METHOD constructor.

    lo_sel_to = io_sel_to.
    m_from_field =  i_from_field.
    m_to_field =  i_to_field.
    lo_tab_from = io_tab_from.
    mo_transmitter = io_transmitter.

    IF mo_transmitter IS NOT INITIAL.
      IF lo_tab_from IS INITIAL.
        SET HANDLER me->update FOR io_transmitter.
      ELSE.
        SET HANDLER me->update_col FOR io_transmitter.
      ENDIF.
    ELSE.
      SET HANDLER me->update FOR ALL INSTANCES.
    ENDIF.

  ENDMETHOD.

  METHOD shut_down.

    IF mo_transmitter IS NOT INITIAL.
      SET HANDLER me->update FOR mo_transmitter  ACTIVATION space.
    ELSE.
      SET HANDLER me->update FOR ALL INSTANCES  ACTIVATION space.
    ENDIF.
    CLEAR lo_sel_to.

  ENDMETHOD.

  METHOD on_grid_button_click.

    FIELD-SYMBOLS: <f_tab>   TYPE STANDARD TABLE.

    CHECK m_from_field = es_col_id-fieldname.
    ASSIGN lo_tab_from->mr_table->* TO <f_tab>.
    READ TABLE <f_tab> INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).
    ASSIGN COMPONENT es_col_id-fieldname OF STRUCTURE <tab> TO  FIELD-SYMBOL(<f_field>).
    CHECK lo_sel_to IS NOT INITIAL.
    lo_sel_to->set_value( i_field = m_to_field i_low = <f_field> ).
    lo_sel_to->raise_selection_done( ).

  ENDMETHOD.

  METHOD  update.

    DATA: l_updated.

    READ TABLE lo_sel_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to>) WITH KEY field_label = m_to_field.
    IF <to>-range[] = e_row-range[].
      l_updated = abap_true."so as not to have an infinite event loop
    ENDIF.
    MOVE-CORRESPONDING e_row TO <to>.
    IF <to>-transmitter IS BOUND AND l_updated IS INITIAL.
      <to>-transmitter->emit( EXPORTING e_row = e_row ).
    ENDIF.
    lo_sel_to->raise_selection_done( ).

  ENDMETHOD.

  METHOD update_col.

    DATA: l_updated,
          lt_sel_row   TYPE lcl_types=>t_sel_row.

    FIELD-SYMBOLS: <tab>   TYPE STANDARD TABLE,
                   <field> TYPE any.

    CHECK lo_sel_to IS NOT INITIAL.
    READ TABLE lo_sel_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to>) WITH KEY field_label = m_to_field.
    DATA(lt_old_range) = <to>-range.
    CLEAR: <to>-sign, <to>-opti, <to>-low, <to>-high, <to>-range.
    ASSIGN lo_tab_from->mr_table->* TO <tab>.

    LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<row>).
      ASSIGN COMPONENT e_column OF STRUCTURE <row> TO <field>.
      IF line_exists( <to>-range[ low = <field> ] ).
        APPEND VALUE #( sign = 'I' opti = 'EQ' low = <field> ) TO <to>-range.
      ENDIF.
    ENDLOOP.

    IF sy-subrc NE 0." empty column
      APPEND VALUE #( sign = 'I' opti = 'EQ' low = '' ) TO <to>-range.
    ENDIF.

    LOOP AT <to>-range ASSIGNING FIELD-SYMBOL(<sel>).
      <to>-low = <sel>-low.
      lo_sel_to->update_sel_row( CHANGING c_sel_row = <to> ).
      EXIT.
    ENDLOOP.

    MOVE-CORRESPONDING <to> TO lt_sel_row.
    IF <to>-range = lt_old_range.
      l_updated = abap_true."so as not to have an infinite event loop
    ENDIF.
    IF <to>-transmitter IS BOUND AND l_updated IS INITIAL.
      <to>-transmitter->emit( EXPORTING e_row = lt_sel_row ).
      lo_sel_to->raise_selection_done( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_box_handler IMPLEMENTATION.

  METHOD on_box_close.
    DATA: lv_tabix LIKE sy-tabix.
    sender->free( ).

    "Free Memory
    LOOP AT lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>) WHERE alv_viewer IS NOT INITIAL.
      IF <obj>-alv_viewer->mo_box = sender.
        lv_tabix = sy-tabix.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF sy-subrc = 0.
      FREE <obj>-alv_viewer->mr_table.
      FREE <obj>-alv_viewer->mo_alv.

      "shutdown receivers.
      IF <obj>-alv_viewer->mo_sel IS NOT INITIAL.
        LOOP AT <obj>-alv_viewer->mo_sel->mt_sel_tab INTO DATA(l_sel).
          IF l_sel-receiver IS BOUND.
            l_sel-receiver->shut_down( ).
          ENDIF.
        ENDLOOP.
      ENDIF.
      FREE <obj>-alv_viewer.
      IF lv_tabix NE 0.
        DELETE lcl_appl=>mt_obj INDEX lv_tabix.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "ON_BOX_CLOSE

ENDCLASS.               "lcl_box_handler

CLASS lcl_table_viewer IMPLEMENTATION.

  METHOD constructor.

    DATA: ls_comp         TYPE abap_componentdescr,
          lt_comp_notab   TYPE abap_component_tab,
          lt_comp_tab2str TYPE abap_component_tab,
          lt_comp_str     TYPE abap_component_tab,
          lv_s            TYPE string,
          lv_data         TYPE REF TO data.

    DATA: l_notab   TYPE REF TO data,
          l_tab2str TYPE REF TO data.

    DATA: handle_notab   TYPE REF TO cl_abap_structdescr,
          handle_tab2str TYPE REF TO cl_abap_structdescr,
          lo_new_tab     TYPE REF TO cl_abap_tabledescr.

    FIELD-SYMBOLS: <notab>   TYPE STANDARD TABLE,
                   <tab2str> TYPE STANDARD TABLE,
                   <any_tab> TYPE ANY TABLE,
                   <temptab> TYPE ANY TABLE.

    super->constructor( i_additional_name = i_additional_name ).
    mo_window = io_window.
    m_lang = sy-langu.
    mo_sel_width = 0.
    m_tabname = i_tname.
    create_popup( ).

    IF ir_tab IS NOT BOUND.
      lcl_rtti=>create_table_by_name( EXPORTING i_tname = m_tabname CHANGING c_table = mr_table ).
    ELSE.
      FIELD-SYMBOLS:<any> TYPE any.
      ASSIGN ir_tab->* TO <any>.
      DATA lo_tabl  TYPE REF TO cl_abap_tabledescr.
      DATA lo_struc TYPE REF TO cl_abap_structdescr.
      lo_tabl ?= cl_abap_typedescr=>describe_by_data( <any> ).
      TRY.
          lo_struc ?= lo_tabl->get_table_line_type( ).
          ASSIGN ir_tab->* TO <any_tab>.
          TRY.
              LOOP AT lo_struc->components INTO DATA(comp).

                IF comp-type_kind NE 'h'.
                  ls_comp-name = comp-name.
                  ls_comp-type ?= lo_struc->get_component_type( comp-name ).
                  APPEND ls_comp TO lt_comp_notab.
                  APPEND ls_comp TO lt_comp_tab2str.
                ELSE.
                  ls_comp-name = comp-name.
                  ls_comp-type ?= cl_abap_typedescr=>describe_by_data( lv_s ).
                  APPEND ls_comp TO lt_comp_tab2str.
                  APPEND ls_comp TO lt_comp_str.

                  ls_comp-name = comp-name && '_REF'.
                  ls_comp-type ?= cl_abap_typedescr=>describe_by_data( lv_data ).
                  APPEND ls_comp TO lt_comp_tab2str.
                ENDIF.
              ENDLOOP.
            CATCH cx_sy_move_cast_error.
          ENDTRY.

          TRY.
              handle_notab  = cl_abap_structdescr=>create( lt_comp_notab ).
              handle_tab2str  = cl_abap_structdescr=>create( lt_comp_tab2str ).

              lo_new_tab = cl_abap_tabledescr=>create(
                p_line_type  = handle_notab
                p_table_kind = cl_abap_tabledescr=>tablekind_std
                p_unique     = abap_false ).

              CREATE DATA l_notab TYPE HANDLE lo_new_tab.

              lo_new_tab = cl_abap_tabledescr=>create(
                p_line_type  = handle_tab2str
                p_table_kind = cl_abap_tabledescr=>tablekind_std
                p_unique     = abap_false ).

              CREATE DATA l_tab2str TYPE HANDLE lo_new_tab.

              ASSIGN l_notab->* TO <notab>.
              MOVE-CORRESPONDING <any_tab> TO <notab>.
              ASSIGN l_tab2str->* TO <tab2str>.
              MOVE-CORRESPONDING <notab> TO <tab2str>.

              LOOP AT <any_tab> ASSIGNING FIELD-SYMBOL(<old_struc>).
                READ TABLE <tab2str> ASSIGNING FIELD-SYMBOL(<new_struc>) INDEX sy-tabix.
                LOOP AT lt_comp_str INTO ls_comp.
                  ASSIGN COMPONENT ls_comp-name OF STRUCTURE <new_struc> TO FIELD-SYMBOL(<field>).
                  ASSIGN COMPONENT ls_comp-name OF STRUCTURE <old_struc> TO <temptab>.
                  <field> = | { icon_view_table } [{ lines( <temptab> ) }] |.
                  ASSIGN COMPONENT ls_comp-name  OF STRUCTURE <old_struc> TO <field>.
                  ASSIGN COMPONENT |{ ls_comp-name }_REF| OF STRUCTURE <new_struc> TO FIELD-SYMBOL(<ref>).
                  GET REFERENCE OF <field> INTO <ref>.
                ENDLOOP.
              ENDLOOP.

              GET REFERENCE OF <tab2str> INTO mr_table.
            CATCH cx_root.
              mr_table = ir_tab.
          ENDTRY.
        CATCH cx_sy_move_cast_error.  "no structure
          ls_comp-name = 'FIELD'.
          ls_comp-type ?= cl_abap_typedescr=>describe_by_data( lv_s ).
          APPEND ls_comp TO lt_comp_tab2str.

          handle_tab2str  = cl_abap_structdescr=>create( lt_comp_tab2str ).
          lo_new_tab = cl_abap_tabledescr=>create(
            p_line_type  = handle_tab2str
            p_table_kind = cl_abap_tabledescr=>tablekind_std
            p_unique     = abap_false ).

          CREATE DATA l_tab2str TYPE HANDLE lo_new_tab.
          ASSIGN l_tab2str->* TO <tab2str>.
          ASSIGN ir_tab->* TO <any_tab>.

          LOOP AT <any_tab> ASSIGNING <old_struc>.
            APPEND INITIAL LINE TO <tab2str> ASSIGNING <new_struc>.
            ASSIGN COMPONENT 'FIELD' OF STRUCTURE <new_struc> TO <field>.
            <field> = <old_struc>.
          ENDLOOP.
          GET REFERENCE OF <tab2str> INTO mr_table.
      ENDTRY.
    ENDIF.

    create_alv( ).
    create_sel_alv( ).
    mo_alv->set_focus( mo_alv ).

  ENDMETHOD.

  METHOD create_popup.

    mo_box = create( i_width = 800 i_hight = 150 ).

    CREATE OBJECT mo_splitter
      EXPORTING
        parent  = mo_box
        rows    = 1
        columns = 2
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter->set_column_mode( mode = mo_splitter->mode_absolute ).
    mo_splitter->set_column_width( id = 1 width = mo_sel_width ).

    CALL METHOD:
     mo_splitter->get_container(  EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = mo_sel_parent ),

      mo_splitter->get_container
       EXPORTING
        row       = 1
        column    = 2
       RECEIVING
        container = mo_alv_parent.

    IF lcl_appl=>m_ctrl_box_handler IS INITIAL.
      lcl_appl=>m_ctrl_box_handler = NEW #( ).
    ENDIF.
    SET HANDLER lcl_appl=>m_ctrl_box_handler->on_box_close FOR mo_box.

  ENDMETHOD.

  METHOD create_alv.

    DATA: ls_layout TYPE lvc_s_layo,
          effect    TYPE i,
          lt_f4     TYPE lvc_t_f4.

    FIELD-SYMBOLS: <f_tab>   TYPE table.

    mo_alv = NEW #( i_parent = mo_alv_parent ).
    mt_alv_catalog = create_field_cat( m_tabname ).

    IF mt_alv_catalog IS INITIAL.
      RETURN. "todo show tables without structure
    ENDIF.

    ASSIGN mr_table->* TO <f_tab>.
    set_header( ).
    ls_layout-cwidth_opt = abap_true.
    ls_layout-sel_mode = 'D'.
    CREATE OBJECT lcl_appl=>c_dragdropalv.
    effect = cl_dragdrop=>move + cl_dragdrop=>copy.

    CALL METHOD lcl_appl=>c_dragdropalv->add
      EXPORTING
        flavor     = 'Line' ##NO_TEXT
        dragsrc    = abap_true
        droptarget = abap_true
        effect     = effect.

    CALL METHOD lcl_appl=>c_dragdropalv->get_handle IMPORTING handle = DATA(handle_alv).
    ls_layout-s_dragdrop-grid_ddid = handle_alv.

    SET HANDLER   before_user_command
                  handle_user_command
                  handle_tab_toolbar
                  handle_doubleclick
                  lcl_dragdrop=>drag
                  FOR mo_alv.

    CALL METHOD mo_alv->set_table_for_first_display
      EXPORTING
        i_save          = abap_true
        i_default       = abap_true
        is_layout       = ls_layout
      CHANGING
        it_fieldcatalog = mt_alv_catalog
        it_outtab       = <f_tab>.

    mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = mt_alv_catalog ).
    LOOP AT mt_alv_catalog ASSIGNING FIELD-SYMBOL(<catalog>).
      CLEAR <catalog>-key.
      DATA(ls_f4) = VALUE lvc_s_f4( register = abap_true chngeafter = abap_true fieldname = <catalog>-fieldname ).
      INSERT ls_f4 INTO TABLE lt_f4.
    ENDLOOP.

    mo_alv->register_f4_for_fields( it_f4 = lt_f4 ).
    mo_alv->set_frontend_fieldcatalog( EXPORTING it_fieldcatalog = mt_alv_catalog ).

    LOOP AT mt_alv_catalog ASSIGNING FIELD-SYMBOL(<cat>) WHERE scrtext_l IS INITIAL.
      lcl_alv_common=>translate_field( CHANGING c_fld = <cat> ).
    ENDLOOP.

    mo_alv->set_frontend_fieldcatalog( EXPORTING it_fieldcatalog = mt_alv_catalog ).
    me->handle_user_command( EXPORTING e_ucomm = 'TECH' ).
    me->handle_user_command( EXPORTING e_ucomm = 'SHOW' ).
    mo_alv->set_toolbar_interactive( ).

  ENDMETHOD.

  METHOD translate_field.

    DATA: l_dd04 TYPE dd04v.

    READ TABLE mt_fields INTO DATA(l_field) WITH KEY field = c_fld-fieldname.
    CHECK l_field-elem IS NOT INITIAL.
    CLEAR l_dd04.

    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING
        name          = CONV ddobjname( l_field-elem )
        langu         = i_lang
      IMPORTING
        dd04v_wa      = l_dd04
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc = 0.
      IF l_dd04-reptext IS NOT INITIAL.
        MOVE-CORRESPONDING l_dd04 TO c_fld.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD create_sel_alv.

    IF mo_sel IS INITIAL.
      mo_sel     = NEW #( io_viewer = me io_container = mo_sel_parent ).
      SET HANDLER refresh_table FOR mo_sel.
    ELSE.
      mo_sel->update_sel_tab( ).
    ENDIF.

  ENDMETHOD.

  METHOD set_header.

    DATA: lv_text       TYPE as4text,
          lv_header(80) TYPE c.

    SELECT SINGLE ddtext INTO lv_text
      FROM dd02t
     WHERE tabname = m_tabname
       AND ddlanguage = m_lang.

    lv_header = |{ m_tabname } - { lv_text } { m_additional_name }|.
    mo_box->set_caption( lv_header ).

  ENDMETHOD.

  METHOD handle_tab_toolbar.

    IF m_visible IS INITIAL.
      DATA(lt_toolbar) = VALUE ttb_button(
       ( function = 'SEL_ON' icon = icon_arrow_left quickinfo = 'Show Select-Options'  butn_type = 0 )
       ( butn_type = 3 ) ).
    ENDIF.

    APPEND VALUE #( function = 'TECH' icon = icon_wd_caption quickinfo = 'Tech names'  butn_type = 0 ) TO lt_toolbar.

    LOOP AT lcl_appl=>mt_lang INTO DATA(lang).
      IF sy-tabix > 10.
        EXIT.
      ENDIF.
      APPEND VALUE #( function = lang-spras icon = icon_foreign_trade quickinfo = lang-sptxt butn_type = 0 text = lang-sptxt ) TO lt_toolbar.
    ENDLOOP.

    lt_toolbar = VALUE ttb_button( BASE lt_toolbar
     ( function = 'SHOW'  icon = icon_list  quickinfo = 'Show empty columns'   butn_type = 0  )
     ( function = 'TBAR' icon = COND #( WHEN m_std_tbar IS INITIAL THEN icon_column_right ELSE icon_column_left )
        quickinfo = COND #( WHEN m_std_tbar IS INITIAL THEN 'Show standard ALV function'  ELSE 'Hide standard ALV function') )
     ( butn_type = 3 ) ).

    IF m_std_tbar IS INITIAL.
      e_object->mt_toolbar =  lt_toolbar.
    ELSE.
      e_object->mt_toolbar =  lt_toolbar = VALUE ttb_button( BASE lt_toolbar ( LINES OF e_object->mt_toolbar ) ).
    ENDIF.

  ENDMETHOD.

  METHOD create_field_cat.

    DATA: lr_field       TYPE REF TO data,
          lr_table_descr TYPE REF TO cl_abap_structdescr,
          lr_data_descr  TYPE REF TO cl_abap_datadescr,
          it_tabdescr    TYPE abap_compdescr_tab,
          l_texttab      TYPE tabname,
          lr_temp        TYPE REF TO data,
          l_name         TYPE string,
          l_dd04         TYPE dd04v.

    FIELD-SYMBOLS: <tab>   TYPE STANDARD TABLE,
                   <struc> TYPE any,
                   <field> TYPE any.

    ASSIGN mr_table->* TO <tab>.
    CREATE DATA lr_temp LIKE LINE OF <tab>.
    ASSIGN lr_temp->* TO <struc>.

    TRY.
        lr_table_descr ?= cl_abap_typedescr=>describe_by_data_ref( lr_temp ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    it_tabdescr[] = lr_table_descr->components[].
    lcl_ddic=>get_text_table( EXPORTING i_tname = i_tname IMPORTING e_tab = l_texttab ).

    LOOP AT it_tabdescr INTO DATA(ls)
       WHERE type_kind NE 'h'
         AND type_kind NE 'l'.
      DATA(l_ind) = sy-tabix.

      ASSIGN COMPONENT ls-name OF STRUCTURE <struc> TO <field>.
      GET REFERENCE OF <field> INTO lr_field.
      lr_data_descr ?= cl_abap_typedescr=>describe_by_data_ref( lr_field ).
      l_name = lr_data_descr->absolute_name.
      REPLACE ALL OCCURRENCES OF '\TYPE=' IN l_name WITH ''.
      APPEND VALUE #( field = ls-name elem = l_name ) TO mt_fields.

      CLEAR l_dd04.
      CALL FUNCTION 'DDIF_DTEL_GET'
        EXPORTING
          name          = CONV ddobjname( l_name )
          langu         = m_lang
        IMPORTING
          dd04v_wa      = l_dd04
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.

      APPEND INITIAL LINE TO et_catalog ASSIGNING FIELD-SYMBOL(<catalog>).

      <catalog>-col_pos = l_ind.
      <catalog>-style = lcl_alv_common=>c_white.
      <catalog>-fieldname = ls-name.
      <catalog>-f4availabl = abap_true.

      IF l_dd04 IS INITIAL.
        <catalog>-scrtext_s = <catalog>-scrtext_m = <catalog>-scrtext_l = <catalog>-reptext = <catalog>-fieldname = ls-name.
      ELSE.
        MOVE-CORRESPONDING l_dd04 TO <catalog>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD handle_doubleclick.

    DATA: lo_table_descr TYPE REF TO cl_tpda_script_tabledescr,
          table_clone    TYPE REF TO data.
    FIELD-SYMBOLS: <f_tab>  TYPE STANDARD TABLE.

    CHECK es_row_no-row_id IS NOT INITIAL.
    ASSIGN mr_table->* TO  <f_tab>.
    READ TABLE <f_tab> INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).
    ASSIGN COMPONENT e_column-fieldname  OF STRUCTURE <tab> TO FIELD-SYMBOL(<val>).

    CASE e_column-fieldname.
      WHEN 'VALUE'.
        IF sy-subrc = 0.
          IF <val> = 'Table'.
            ASSIGN COMPONENT 'REF'  OF STRUCTURE <tab> TO FIELD-SYMBOL(<ref>).
            lcl_appl=>open_int_table( EXPORTING iv_name = CONV #( e_column-fieldname ) it_ref = <ref> io_window = mo_window ).
          ENDIF.
        ELSE.
          TRY.
              lo_table_descr ?= cl_tpda_script_data_descr=>factory( |{ m_additional_name }[ { es_row_no-row_id } ]-{ e_column-fieldname }| ).
              table_clone = lo_table_descr->elem_clone( ).
              lcl_appl=>open_int_table( EXPORTING iv_name = |{ m_additional_name }[ { es_row_no-row_id } ]-{ e_column-fieldname }| it_ref = table_clone io_window = mo_window ).
            CATCH cx_sy_move_cast_error.
          ENDTRY.
        ENDIF.
      WHEN 'STEP'.
        MOVE-CORRESPONDING <tab> TO mo_window->m_prg.
        MOVE-CORRESPONDING <tab> TO mo_window->mo_debugger->ms_stack.

        mo_window->show_coverage( ).
        mo_window->mo_debugger->show_step( ).
      WHEN OTHERS. "check if it is an embedded table.
        TRY.
            lo_table_descr ?= cl_tpda_script_data_descr=>factory( |{ m_additional_name }[ { es_row_no-row_id } ]-{ e_column-fieldname }| ).
            table_clone = lo_table_descr->elem_clone( ).
            lcl_appl=>open_int_table( EXPORTING iv_name = |{ m_additional_name }[ { es_row_no-row_id } ]-{ e_column-fieldname }| it_ref = table_clone io_window = mo_window ).
          CATCH cx_sy_move_cast_error.
        ENDTRY.
    ENDCASE.

  ENDMETHOD.

  METHOD before_user_command.

    CASE e_ucomm.
      WHEN '&INFO'.
        DATA(l_url) = 'https://ysychov.wordpress.com/2020/02/10/simple-data-explorer/'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.
    ENDCASE.

  ENDMETHOD.

  METHOD handle_user_command.

    DATA: it_fields     TYPE lvc_t_fcat,
          lv_clause(45),
          lv_sel_width  TYPE i.

    FIELD-SYMBOLS: <f_tab>  TYPE STANDARD  TABLE.
    ASSIGN mr_table->* TO <f_tab>.
    mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = it_fields[] ).
    IF e_ucomm = 'SEL_ON' AND m_visible IS INITIAL.
      create_sel_alv( ).
      m_visible = abap_true.
      IF mo_sel_width = 0.
        lv_sel_width = 500.
      ELSE.
        lv_sel_width = mo_sel_width.
      ENDIF.

      mo_splitter->set_column_width( EXPORTING id = 1 width = lv_sel_width ).
      mo_alv->set_toolbar_interactive( ).
      RETURN.
    ELSEIF e_ucomm = 'TBAR'.
      m_std_tbar = BIT-NOT  m_std_tbar.
    ELSE.
      IF e_ucomm = 'SHOW'.
        IF m_show_empty IS INITIAL.
          m_show_empty = 1.
        ELSE.
          CLEAR m_show_empty.
        ENDIF.
      ENDIF.

      LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<fields>) WHERE domname NE 'MANDT'.
        <fields>-col_pos = sy-tabix.
        CASE e_ucomm.

          WHEN 'SHOW'.
            IF m_show_empty = abap_false.
              <fields>-no_out = ' '.
            ELSE.
              lv_clause = |{ <fields>-fieldname } IS NOT INITIAL|.
              LOOP AT <f_tab> ASSIGNING FIELD-SYMBOL(<f_line>)  WHERE (lv_clause).
                EXIT.
              ENDLOOP.
              IF sy-subrc NE 0.
                <fields>-no_out = abap_true.
              ENDIF.
            ENDIF.

          WHEN 'TECH'. "technical field name
            <fields>-scrtext_l = <fields>-scrtext_m = <fields>-scrtext_s =  <fields>-reptext = <fields>-fieldname.

          WHEN OTHERS. "header names translation
            IF line_exists( lcl_appl=>mt_lang[ spras = e_ucomm ] ).
              translate_field( EXPORTING i_lang = CONV #( e_ucomm )  CHANGING c_fld = <fields> ).
              IF mo_sel IS BOUND.
                READ TABLE mo_sel->mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) WITH KEY field_label = <fields>-fieldname.
                IF sy-subrc = 0.
                  IF <fields>-scrtext_l IS NOT INITIAL.
                    <sel>-name = <fields>-scrtext_l.
                  ENDIF.
                  IF <sel>-name IS INITIAL.
                    IF <fields>-reptext IS NOT INITIAL.
                      <sel>-name = <fields>-reptext.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    IF line_exists( lcl_appl=>mt_lang[ spras = e_ucomm ] ).
      m_lang = e_ucomm.
      set_header( ).
      mo_sel->set_value( i_field = 'SPRSL' i_low = m_lang ).
    ENDIF.

    CALL METHOD mo_alv->set_frontend_fieldcatalog EXPORTING it_fieldcatalog = it_fields[].

    lcl_alv_common=>refresh( mo_alv ).
    IF mo_sel IS BOUND.
      IF  e_ucomm = 'HIDE' OR e_ucomm = 'SHOW' OR e_ucomm = 'UPDATE' .
        mo_sel->update_sel_tab( ).
      ENDIF.
      lcl_alv_common=>refresh( mo_sel->mo_sel_alv ).
      mo_sel->mo_sel_alv->refresh_table_display(  ).
    ENDIF.

  ENDMETHOD.                           "handle_user_command

  METHOD refresh_table.

    DATA: ls_row    TYPE lcl_types=>t_sel_row,
          lt_filter TYPE lvc_t_filt.

    CLEAR lt_filter.
    set_header( ).

    LOOP AT mo_sel->mt_sel_tab  ASSIGNING FIELD-SYMBOL(<sel>).
      IF <sel>-transmitter IS NOT INITIAL.
        MOVE-CORRESPONDING <sel> TO ls_row.
        <sel>-transmitter->emit( e_row = ls_row ).
      ENDIF.
      LOOP AT <sel>-range INTO DATA(l_range).
        APPEND VALUE #( fieldname = <sel>-field_label
                              low = l_range-low
                             high = l_range-high
                             sign = l_range-sign
                           option = l_range-opti ) TO lt_filter.
      ENDLOOP.
    ENDLOOP.

    IF mo_sel->mt_sel_tab IS NOT INITIAL.
      CALL METHOD mo_alv->set_filter_criteria
        EXPORTING
          it_filter = lt_filter.
      lcl_alv_common=>refresh( mo_sel->mo_sel_alv ).
      lcl_alv_common=>refresh( mo_alv ).
      mo_sel->mo_debugger->handle_user_command( 'SHOW' ).
      LOOP AT mo_column_emitters INTO DATA(l_emit).
        l_emit-emitter->emit_col( l_emit-column ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_sel_opt IMPLEMENTATION.
  METHOD constructor.
    DATA: effect     TYPE i,
          handle_alv TYPE i.

    mo_debugger = io_viewer.
    mo_sel_alv = NEW #( i_parent = io_container ).
    update_sel_tab( ).
    CREATE OBJECT lcl_appl=>c_dragdropalv.
    effect =  cl_dragdrop=>copy. " + cl_dragdrop=>move.

    CALL METHOD lcl_appl=>c_dragdropalv->add
      EXPORTING
        flavor     = 'Line'
        dragsrc    = abap_true
        droptarget = abap_true
        effect     = effect.

    CALL METHOD lcl_appl=>c_dragdropalv->get_handle IMPORTING handle = handle_alv.
    ms_layout-s_dragdrop-col_ddid = handle_alv.
    init_fcat( handle_alv ).
    ms_layout-cwidth_opt = abap_true.
    ms_layout-col_opt = abap_true.
    ms_layout-ctab_fname = 'COLOR'.
    ms_layout-stylefname = 'STYLE'.

    "fields for F4 event handling
    DATA(gt_f4) = VALUE  lvc_t_f4( register   = abap_true chngeafter = abap_true
                             ( fieldname  = 'LOW'  )
                             ( fieldname  = 'HIGH'  ) ).

    mo_sel_alv->register_f4_for_fields( it_f4 = gt_f4 ).
    mo_sel_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
    mo_sel_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    SET HANDLER handle_user_command
                handle_sel_toolbar
                lcl_dragdrop=>drag
                lcl_dragdrop=>drop
                on_data_changed
                on_data_changed_finished
                on_grid_button_click
                handle_context_menu_request
                handle_doubleclick
                on_f4 FOR mo_sel_alv.

    CALL METHOD mo_sel_alv->set_table_for_first_display
      EXPORTING
        i_save          = abap_true
        i_default       = abap_true
        is_layout       = ms_layout
      CHANGING
        it_outtab       = mt_sel_tab[]
        it_fieldcatalog = mt_fcat.

    mo_sel_alv->set_toolbar_interactive( ).

  ENDMETHOD.

  METHOD init_fcat.

    mt_fcat = VALUE #(
     ( fieldname = 'IND'         coltext = '№'  outputlen = 3 style = '00000003' )
     ( fieldname = 'FIELD_LABEL' coltext = 'Label'  outputlen = 30 dragdropid = i_dd_handle )
     ( fieldname = 'SIGN'        coltext = 'SIGN'   tech = abap_true )
     ( fieldname = 'OPTI'        coltext = 'Option' tech = abap_true )
     ( fieldname = 'OPTION_ICON' coltext = 'Option' icon = abap_true outputlen = 4 style = cl_gui_alv_grid=>mc_style_button )
     ( fieldname = 'LOW'         coltext = 'From data' edit = abap_true lowercase = abap_true outputlen = 45 style = cl_gui_alv_grid=>mc_style_f4 col_opt = abap_true  )
     ( fieldname = 'HIGH'        coltext = 'To data' edit = abap_true lowercase = abap_true outputlen = 45 style = cl_gui_alv_grid=>mc_style_f4  col_opt = abap_true )
     ( fieldname = 'MORE_ICON'   coltext = 'Range' icon = abap_true  style = cl_gui_alv_grid=>mc_style_button  )
     ( fieldname = 'RANGE'   tech = abap_true  )
     ( fieldname = 'INHERITED'   coltext = 'Inh.' icon = abap_true outputlen = 4 seltext = 'Inherited' style = '00000003')
     ( fieldname = 'EMITTER'    coltext = 'Emit.' icon = abap_true outputlen = 4 seltext = 'Emitter' style = '00000003')
     ( fieldname = 'NAME' coltext = 'Field name'  outputlen = 60 style = '00000003')
     ( fieldname = 'ELEMENT' coltext = 'Data element'  outputlen = 15 style = '00000209' )
     ( fieldname = 'DOMAIN'  coltext = 'Domain'  outputlen = 15 style = '00000209' )
     ( fieldname = 'DATATYPE' coltext = 'Type'  outputlen = 5 style = '00000003')
     ( fieldname = 'LENGTH' coltext = 'Length'  outputlen = 5 style = '00000003')
     ( fieldname = 'TRANSMITTER'   tech = abap_true  )
     ( fieldname = 'RECEIVER'    tech = abap_true  )
     ( fieldname = 'COLOR'    tech = abap_true  ) ).

  ENDMETHOD.

  METHOD raise_selection_done.

    DATA: ls_row TYPE lcl_types=>t_sel_row.

    lcl_alv_common=>refresh( mo_sel_alv ).
    RAISE EVENT selection_done.
    LOOP AT mt_sel_tab  ASSIGNING FIELD-SYMBOL(<sel>).
      IF <sel>-transmitter IS NOT INITIAL.
        MOVE-CORRESPONDING <sel> TO ls_row.
        <sel>-transmitter->emit( e_row = ls_row ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD update_sel_tab.

    IF mt_sel_tab[] IS NOT INITIAL.
      DATA(lt_copy) = mt_sel_tab.
    ENDIF.
    CLEAR mt_sel_tab[].
    mo_debugger->mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = mo_debugger->mt_alv_catalog ).
    LOOP AT mo_debugger->mt_alv_catalog INTO DATA(l_catalog) WHERE domname NE 'MANDT'.
      DATA(lv_ind) = sy-tabix.
      APPEND INITIAL LINE TO mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel_tab>).
      READ TABLE lt_copy INTO DATA(ls_copy) WITH KEY field_label = l_catalog-fieldname.

      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_copy TO <sel_tab>.
      ELSE.
        <sel_tab>-option_icon = icon_led_inactive.
        <sel_tab>-more_icon = icon_enter_more.
      ENDIF.

      <sel_tab>-ind = lv_ind.
      <sel_tab>-field_label = l_catalog-fieldname.
      <sel_tab>-int_type = l_catalog-inttype.
      <sel_tab>-element = l_catalog-rollname.
      <sel_tab>-domain =  l_catalog-domname.
      <sel_tab>-datatype = l_catalog-datatype.
      <sel_tab>-length = l_catalog-outputlen.
      lcl_alv_common=>translate_field( EXPORTING i_lang = mo_debugger->m_lang CHANGING c_fld = l_catalog ).
      <sel_tab>-name = l_catalog-scrtext_l.
    ENDLOOP.

  ENDMETHOD.

  METHOD handle_sel_toolbar.

    e_object->mt_toolbar[] = VALUE #( butn_type = 0 disabled = ''
     ( function = 'SEL_OFF' icon = icon_arrow_right    quickinfo = 'Hide' )
     ( function = 'SEL_CLEAR' icon = icon_delete_row    quickinfo = 'Clear Select-Options' ) ).

  ENDMETHOD.

  METHOD set_value.

    READ TABLE mt_sel_tab ASSIGNING FIELD-SYMBOL(<to>) WITH KEY field_label = i_field.
    CHECK sy-subrc = 0.
    IF i_low IS SUPPLIED.
      IF i_clear IS INITIAL.
        APPEND VALUE #( sign = 'I' opti = 'EQ' low = i_low high = i_high ) TO <to>-range.
      ELSE.
        CLEAR:  <to>-opti, <to>-sign,<to>-range.
        IF i_low IS SUPPLIED.
          <to>-low = i_low.
        ENDIF.
        IF i_high IS SUPPLIED.
          <to>-high = i_high.
        ENDIF.
        update_sel_row( CHANGING c_sel_row = <to> ).
      ENDIF.
    ELSE.
      CLEAR:  <to>-opti, <to>-sign.
      <to>-high = i_high.
      update_sel_row( CHANGING c_sel_row = <to> ).
    ENDIF.
    IF <to>-transmitter IS BOUND.
      DATA: ls_row TYPE lcl_types=>t_sel_row.
      MOVE-CORRESPONDING <to> TO ls_row.
      <to>-transmitter->emit( EXPORTING e_row = ls_row ).
    ENDIF.

  ENDMETHOD.

  METHOD handle_doubleclick.

    DATA: it_bdcdata TYPE TABLE OF  bdcdata.

    CHECK es_row_no-row_id IS NOT INITIAL.

    READ TABLE mt_sel_tab INDEX es_row_no-row_id INTO DATA(l_sel).
    APPEND VALUE #( program = 'SAPLSD_ENTRY' dynpro = '1000' dynbegin = abap_true ) TO it_bdcdata.
    APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WB_DISPLAY' ) TO it_bdcdata.

    IF e_column = 'ELEMENT'.
      SET PARAMETER ID 'DTYP' FIELD l_sel-element.
      APPEND VALUE #( fnam = 'RSRD1-DDTYPE' fval = abap_true ) TO it_bdcdata.
      CALL TRANSACTION 'SE11' USING it_bdcdata MODE 'E'.
    ELSEIF e_column = 'DOMAIN'.
      SET PARAMETER ID 'DOM' FIELD l_sel-domain.
      APPEND VALUE #( fnam = 'RSRD1-DOMA' fval = abap_true ) TO it_bdcdata.
      CALL TRANSACTION 'SE11' USING it_bdcdata MODE 'E'.
    ELSE.
      CALL FUNCTION 'DOCU_CALL'
        EXPORTING
          id                = 'DE'
          langu             = mo_debugger->m_lang
          object            = l_sel-element
          typ               = 'E'
          displ             = abap_true
          displ_mode        = 3
          use_sec_langu     = abap_true
          display_shorttext = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD update_sel_row. "select patterns rules

    IF c_sel_row-high IS INITIAL AND c_sel_row-opti = 'BT'.
      CLEAR c_sel_row-opti.
    ENDIF.

    IF c_sel_row-low IS NOT INITIAL AND c_sel_row-opti IS INITIAL.
      c_sel_row-sign = 'I'.
      c_sel_row-opti = 'EQ'.
    ENDIF.

    IF c_sel_row-high IS NOT INITIAL AND c_sel_row-opti NE 'NB' .
      c_sel_row-opti = 'BT'.
    ENDIF.

    IF c_sel_row-sign IS INITIAL AND c_sel_row-opti IS INITIAL.
      CLEAR: c_sel_row-low, c_sel_row-low.
    ENDIF.

    IF c_sel_row-low CA  '*%+&' AND c_sel_row-opti <> 'NP'.
      c_sel_row-sign = 'I'.
      c_sel_row-opti = 'CP'.
    ENDIF.

    IF c_sel_row-opti IS NOT INITIAL AND c_sel_row-sign IS INITIAL.
      c_sel_row-sign = 'I'.
    ENDIF.

    TRY.
        c_sel_row-option_icon = lcl_appl=>m_option_icons[ sign = c_sel_row-sign option = c_sel_row-opti ]-icon_name.
      CATCH cx_sy_itab_line_not_found.                  "#EC NO_HANDLER
    ENDTRY.

    IF c_sel_row-sign IS NOT INITIAL.
      READ TABLE c_sel_row-range ASSIGNING FIELD-SYMBOL(<range>) INDEX 1.
      IF sy-subrc NE 0.
        APPEND INITIAL LINE TO c_sel_row-range ASSIGNING <range>.
      ENDIF.
      MOVE-CORRESPONDING c_sel_row TO <range>.
      IF c_sel_row-opti NE 'BT' AND c_sel_row-opti NE 'NB' .
        CLEAR c_sel_row-high.
      ENDIF.
      IF c_sel_row-int_type = 'D' OR c_sel_row-int_type = 'T' .
        DO 2 TIMES.
          ASSIGN COMPONENT  COND string( WHEN sy-index = 1 THEN 'LOW' ELSE 'HIGH'  ) OF STRUCTURE <range> TO FIELD-SYMBOL(<field>).
          IF <field> IS INITIAL.
            CONTINUE.
          ENDIF.

          IF c_sel_row-int_type = 'D'.
            CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
              EXPORTING
                date_external            = <field>
              IMPORTING
                date_internal            = <field>
              EXCEPTIONS
                date_external_is_invalid = 1
                OTHERS                   = 2.
          ELSE.
            REPLACE ALL OCCURRENCES OF ':' IN <field> WITH ''.
          ENDIF.
        ENDDO.
      ENDIF.
    ENDIF.
    c_sel_row-more_icon = COND #( WHEN c_sel_row-range IS INITIAL THEN icon_enter_more    ELSE icon_display_more  ).

    IF c_sel_row-receiver IS BOUND AND c_sel_row-inherited IS INITIAL.
      c_sel_row-inherited = icon_businav_value_chain.
    ENDIF.

  ENDMETHOD.

  METHOD on_f4.

    DATA: return_tab TYPE STANDARD TABLE OF ddshretval,
          lt_objec   TYPE TABLE OF objec,
          ls_objec   TYPE objec,
          l_otype    TYPE otype,
          l_plvar    TYPE plvar,
          l_multiple TYPE xfeld,
          l_clear    TYPE xfeld.

    IF e_fieldname = 'LOW'.
      l_multiple = abap_true.
    ENDIF.

    READ TABLE mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) INDEX es_row_no-row_id.
    DATA(l_fname) =  <sel>-field_label.

    lcl_types=>mt_sel[] = mt_sel_tab[].
    IF <sel>-element = 'HROBJID'.
      READ TABLE mt_sel_tab INTO DATA(l_sel) WITH KEY field_label = 'OTYPE'.
      l_otype = l_sel-low.
      READ TABLE mt_sel_tab INTO l_sel WITH KEY field_label = 'PLVAR'.
      IF sy-subrc = 0 AND l_sel-low IS NOT INITIAL.
        l_plvar = l_sel-low.
      ELSE.
        CALL FUNCTION 'RH_GET_ACTIVE_WF_PLVAR'
          IMPORTING
            act_plvar       = l_plvar
          EXCEPTIONS
            no_active_plvar = 1
            OTHERS          = 2.
      ENDIF.
    ELSEIF <sel>-element = 'PERSNO'.
      l_otype = 'P'.
    ENDIF.

    IF l_otype IS NOT INITIAL.
      CALL FUNCTION 'RH_OBJID_REQUEST'
        EXPORTING
          plvar            = l_plvar
          otype            = l_otype
          seark_begda      = sy-datum
          seark_endda      = sy-datum
          dynpro_repid     = sy-repid
          dynpro_dynnr     = sy-dynnr
          set_mode         = l_multiple
        IMPORTING
          sel_object       = ls_objec
        TABLES
          sel_hrobject_tab = lt_objec
        EXCEPTIONS
          OTHERS           = 6.
      IF sy-subrc = 0.
        l_clear = abap_true.
        LOOP AT lt_objec INTO ls_objec.
          IF e_fieldname = 'LOW'.
            set_value( EXPORTING i_field = <sel>-field_label i_low = ls_objec-objid i_clear = l_clear ).
            CLEAR l_clear.
          ELSE.
            set_value( EXPORTING i_field = <sel>-field_label i_high = ls_objec-objid i_clear = l_clear ).
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.

      CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
        EXPORTING
          tabname           = mo_debugger->m_tabname
          fieldname         = l_fname
          callback_program  = sy-repid
          callback_form     = 'CALLBACK_F4_SEL' "callback_method - doesn't work for local class
          multiple_choice   = l_multiple
        TABLES
          return_tab        = return_tab
        EXCEPTIONS
          field_not_found   = 1
          no_help_for_field = 2
          inconsistent_help = 3
          no_values_found   = 4
          OTHERS            = 5.

      IF sy-subrc = 0 AND lines( return_tab ) > 0.
        ASSIGN er_event_data->m_data->* TO FIELD-SYMBOL(<itab>).
        CLEAR <sel>-range.
        l_clear = abap_true.
        LOOP AT return_tab ASSIGNING FIELD-SYMBOL(<ret>) WHERE fieldname = l_fname.
          IF e_fieldname = 'LOW'.
            set_value( EXPORTING i_field = <sel>-field_label i_low = <ret>-fieldval i_clear = l_clear ).
            CLEAR l_clear.
          ELSE.
            set_value( EXPORTING i_field = <sel>-field_label i_high = <ret>-fieldval ).
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
    er_event_data->m_event_handled = abap_true.
    raise_selection_done( ).

  ENDMETHOD.

  METHOD on_grid_button_click.

    DATA: l_tabfield TYPE rstabfield,
          ls_opt     TYPE rsoptions VALUE 'XXXXXXXXXX',
          lv_sign    TYPE raldb_sign,
          lv_option  TYPE raldb_opti.

    READ TABLE mt_sel_tab INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).
    CASE es_col_id.
      WHEN 'OPTION_ICON'. "edit select logical expression type
        CALL FUNCTION 'SELECT_OPTION_OPTIONS'
          EXPORTING
            selctext     = 'nnnn'
            option_list  = ls_opt
          IMPORTING
            sign         = lv_sign
            option       = lv_option
          EXCEPTIONS
            delete_line  = 1
            not_executed = 2
            illegal_sign = 3
            OTHERS       = 4.
        IF sy-subrc = 0.
          <tab>-sign = lv_sign.
          <tab>-opti = lv_option.
        ELSEIF sy-subrc = 1.
          CLEAR: <tab>-low, <tab>-high,<tab>-sign, <tab>-opti, <tab>-range.
        ENDIF.
      WHEN 'MORE_ICON'. "edit ranges
        l_tabfield-tablename = mo_debugger->m_tabname.
        l_tabfield-fieldname = <tab>-field_label.

        CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
          EXPORTING
            title             = 'title'
            text              = 'text'
            tab_and_field     = l_tabfield
          TABLES
            range             = <tab>-range
          EXCEPTIONS
            no_range_tab      = 1
            cancelled         = 2
            internal_error    = 3
            invalid_fieldname = 4
            OTHERS            = 5.
        IF sy-subrc = 0.
          READ TABLE <tab>-range INDEX 1 INTO DATA(l_range).
          MOVE-CORRESPONDING l_range TO <tab>.
          IF <tab>-opti NE 'BT'.
            CLEAR <tab>-high.
          ENDIF.
        ENDIF.
    ENDCASE.
    update_sel_row( CHANGING c_sel_row = <tab> ).
    RAISE EVENT selection_done.

  ENDMETHOD.

  METHOD on_data_changed.

    DATA: l_start TYPE i,
          lv_time TYPE sy-uzeit.

    FIELD-SYMBOLS: <field> TYPE any.

    LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<ls_cells>).
      READ TABLE mt_sel_tab INDEX <ls_cells>-row_id ASSIGNING FIELD-SYMBOL(<tab>).
      ASSIGN COMPONENT <ls_cells>-fieldname OF STRUCTURE <tab> TO <field>.
      READ TABLE mo_debugger->mt_alv_catalog WITH KEY fieldname = <tab>-field_label INTO DATA(l_cat).

      IF <field> IS NOT INITIAL AND <ls_cells>-value IS INITIAL.
        READ TABLE <tab>-range INTO DATA(l_second) INDEX 2.
        IF sy-subrc = 0.
          IF ( <ls_cells>-fieldname = 'LOW' AND <tab>-high IS INITIAL ) OR  ( <ls_cells>-fieldname = 'HIGH' AND <tab>-low IS INITIAL  ).
            DELETE <tab>-range INDEX 1.
          ELSE.
            CLEAR l_second.
          ENDIF.
        ENDIF.
      ENDIF.

      IF l_cat-convexit = 'ALPHA' AND NOT  <ls_cells>-value CA '+*'.
        <ls_cells>-value = |{ <ls_cells>-value ALPHA = IN }|.
        l_start = 128 - l_cat-dd_outlen.
        <ls_cells>-value = <ls_cells>-value+l_start(l_cat-dd_outlen).
      ENDIF.

      IF <ls_cells>-value IS NOT INITIAL.
        IF <tab>-int_type = 'D'.
          DATA: lv_date TYPE sy-datum.
          CALL FUNCTION 'CONVERT_DATE_INPUT'
            EXPORTING
              input                     = <ls_cells>-value
              plausibility_check        = abap_true
            IMPORTING
              output                    = lv_date
            EXCEPTIONS
              plausibility_check_failed = 1
              wrong_format_in_input     = 2
              OTHERS                    = 3.

          IF sy-subrc = 0.
            <ls_cells>-value = |{ lv_date DATE = USER }|.
          ENDIF.
        ELSEIF <tab>-int_type = 'T'.
          CALL FUNCTION 'CONVERT_TIME_INPUT'
            EXPORTING
              input                     = <ls_cells>-value
            IMPORTING
              output                    = lv_time
            EXCEPTIONS
              plausibility_check_failed = 1
              wrong_format_in_input     = 2
              OTHERS                    = 3.
          <ls_cells>-value = lv_time+0(2) && ':' && lv_time+2(2) && ':' && lv_time+4(2).
        ENDIF.
      ENDIF.
    ENDLOOP.
    CHECK sy-subrc = 0.

    IF l_second IS INITIAL.
      <field> = <ls_cells>-value.
      er_data_changed->modify_cell( EXPORTING i_row_id = <ls_cells>-row_id i_fieldname = <ls_cells>-fieldname i_value = <ls_cells>-value ).
    ELSE.
      <tab>-low = l_second-low.
      er_data_changed->modify_cell( EXPORTING i_row_id = <ls_cells>-row_id i_fieldname = 'LOW' i_value = l_second-low ).
      IF l_second-high CO '0 '.
        CLEAR l_second-high.
      ENDIF.
      <tab>-high = l_second-high.
      er_data_changed->modify_cell( EXPORTING i_row_id = <ls_cells>-row_id i_fieldname = 'HIGH' i_value = l_second-high ).

      <tab>-opti = l_second-opti.
      er_data_changed->modify_cell( EXPORTING i_row_id = <ls_cells>-row_id i_fieldname = 'OPTI' i_value = l_second-opti ).
      <tab>-sign = l_second-sign.
      er_data_changed->modify_cell( EXPORTING i_row_id = <ls_cells>-row_id i_fieldname = 'SIGN' i_value = l_second-sign ).
    ENDIF.

    update_sel_row( CHANGING c_sel_row = <tab> ).
    lcl_alv_common=>refresh( EXPORTING i_obj = mo_sel_alv i_layout = ms_layout ).
    raise_selection_done( ).

  ENDMETHOD.

  METHOD on_data_changed_finished.

    CHECK e_modified IS NOT INITIAL.
    RAISE EVENT selection_done.

  ENDMETHOD.

  METHOD handle_context_menu_request.

    DATA: ls_func TYPE ui_func,
          lt_func TYPE ui_functions.

    DATA(l_index) = lcl_alv_common=>get_selected( mo_sel_alv ).

    IF l_index IS NOT INITIAL.
      READ TABLE mt_sel_tab INTO DATA(l_sel) INDEX l_index.
    ENDIF.

    e_object->get_functions( IMPORTING fcodes = DATA(lt_fcodes) ). "Inactivate all standard functions

    LOOP AT lt_fcodes INTO DATA(ls_fcode) WHERE fcode NE '&OPTIMIZE'.
      ls_func = ls_fcode-fcode.
      APPEND ls_func TO lt_func.
    ENDLOOP.

    e_object->hide_functions( lt_func ).
    e_object->add_separator( ).

    IF l_sel-range[]  IS NOT INITIAL OR l_index IS INITIAL.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'SEL_CLEAR'
          text  = 'Clear Select-Options'.
    ENDIF.

    IF l_sel-receiver IS NOT INITIAL OR l_index IS INITIAL.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'DELR'
          text  = 'Delete receiver'.
    ENDIF.

  ENDMETHOD.

  METHOD handle_user_command.

    DATA: lv_sel_width TYPE i.

    IF e_ucomm = 'SEL_OFF'. "Hide select-options alv

      mo_debugger->m_visible = ''.

      lv_sel_width = 0.
      CALL METHOD mo_debugger->mo_splitter->get_column_width
        EXPORTING
          id                = 1
        IMPORTING
          result            = mo_debugger->mo_sel_width
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3.

      CALL METHOD mo_debugger->mo_splitter->set_column_width
        EXPORTING
          id    = 1
          width = lv_sel_width.
      mo_debugger->mo_alv->set_toolbar_interactive( ).
      RETURN.
    ENDIF.

    IF e_ucomm = 'SEL_CLEAR' OR e_ucomm = 'DELR'. "clear all selections
      mo_sel_alv->get_selected_rows( IMPORTING et_index_rows = DATA(lt_sel_rows) ).

      LOOP AT lt_sel_rows INTO DATA(l_row).
        READ TABLE mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) INDEX l_row-index.
        IF e_ucomm = 'SEL_CLEAR'.
          CLEAR : <sel>-low, <sel>-high, <sel>-sign, <sel>-opti, <sel>-range.
        ELSEIF e_ucomm = 'DELR'.
          IF <sel>-receiver IS NOT INITIAL.
            <sel>-receiver->shut_down( ).
            FREE <sel>-receiver.
            CLEAR <sel>-receiver.
            CLEAR <sel>-inherited.
          ENDIF.
        ENDIF.
        update_sel_row( CHANGING c_sel_row = <sel> ).
      ENDLOOP.
      RAISE EVENT selection_done.
    ENDIF.

    lcl_alv_common=>refresh( mo_debugger->mo_alv ).
    RAISE EVENT selection_done.

  ENDMETHOD.                           "handle_user_command

ENDCLASS.

CLASS lcl_appl IMPLEMENTATION.

  METHOD init_icons_table.

    m_option_icons = VALUE #(
     ( sign = space option = space  icon_name = icon_led_inactive )
     ( sign = 'I'   option = 'EQ'   icon_name = icon_equal_green )
     ( sign = 'I'   option = 'NE'   icon_name = icon_not_equal_green )
     ( sign = 'I'   option = 'LT'   icon_name = icon_less_green )
     ( sign = 'I'   option = 'LE'   icon_name = icon_less_equal_green )
     ( sign = 'I'   option = 'GT'   icon_name = icon_greater_green )
     ( sign = 'I'   option = 'GE'   icon_name = icon_greater_equal_green )
     ( sign = 'I'   option = 'CP'   icon_name = icon_pattern_include_green )
     ( sign = 'I'   option = 'NP'   icon_name = icon_pattern_exclude_green )
     ( sign = 'I'   option = 'BT'   icon_name = icon_interval_include_green )
     ( sign = 'I'   option = 'NB'   icon_name = icon_interval_exclude_green )
     ( sign = 'E'   option = 'EQ'   icon_name = icon_equal_red )
     ( sign = 'E'   option = 'NE'   icon_name = icon_not_equal_red )
     ( sign = 'E'   option = 'LT'   icon_name = icon_less_red )
     ( sign = 'E'   option = 'LE'   icon_name = icon_less_equal_red )
     ( sign = 'E'   option = 'GT'   icon_name = icon_greater_red )
     ( sign = 'E'   option = 'GE'   icon_name = icon_greater_equal_red )
     ( sign = 'E'   option = 'CP'   icon_name = icon_pattern_include_red )
     ( sign = 'E'   option = 'NP'   icon_name = icon_pattern_exclude_red )
     ( sign = 'E'   option = 'BT'   icon_name = icon_interval_include_red )
     ( sign = 'E'   option = 'NB'   icon_name = icon_interval_exclude_red ) ).

  ENDMETHOD.

  METHOD init_lang.
*    SELECT c~spras t~sptxt INTO CORRESPONDING FIELDS OF TABLE mt_lang
*      FROM t002c AS c
*      INNER JOIN t002t AS t
*      ON c~spras = t~sprsl
*      WHERE t~spras = sy-langu
*      ORDER BY c~ladatum DESCENDING c~lauzeit DESCENDING.
  ENDMETHOD.

  METHOD check_mermaid.

    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING
        clskey        = 'ZCL_WD_GUI_MERMAID_JS_DIAGRAM '
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_interface  = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.

    IF sy-subrc = 0.
      is_mermaid_active = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD open_int_table.

    DATA r_tab TYPE REF TO data.
    IF it_ref IS BOUND.
      r_tab = it_ref.
    ELSE.
      GET REFERENCE OF it_tab INTO r_tab.
    ENDIF.
    APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    <obj>-alv_viewer = NEW #(  i_additional_name = iv_name ir_tab = r_tab io_window = io_window ).
    <obj>-alv_viewer->mo_sel->raise_selection_done( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_rtti_tree IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).
    mo_debugger = i_debugger.

    cl_salv_tree=>factory(
      EXPORTING
        r_container = i_cont
      IMPORTING
        r_salv_tree = tree
      CHANGING
        t_table     = tree_table ).

    DATA(lo_setting) =  tree->get_tree_settings( ).
    lo_setting->set_hierarchy_header( i_header ).
    lo_setting->set_hierarchy_size( 30 ).
    lo_setting->set_hierarchy_icon( CONV #( icon_tree ) ).

    DATA(lo_columns) = tree->get_columns( ).
    lo_columns->set_optimize( abap_true ).

    lo_columns->get_column( 'VALUE' )->set_short_text( 'Value' ).
    lo_columns->get_column( 'FULLNAME' )->set_visible( abap_false ).
    lo_columns->get_column( 'PATH' )->set_visible( abap_false ).
    lo_columns->get_column( 'TYPENAME' )->set_short_text( 'Type' ).
    lo_columns->get_column( 'TYPENAME' )->set_medium_text( 'Absolute Type' ).

    add_buttons( i_type ).

    DATA(lo_event) = tree->get_event( ) .
    SET HANDLER hndl_double_click
                hndl_user_command FOR lo_event.

    m_globals = '01'.
    tree->display( ).

  ENDMETHOD.

  METHOD add_buttons.

    DATA(lo_functions) = tree->get_functions( ).
    lo_functions->set_all( ).

    lo_functions->set_group_layout( abap_false ).
    lo_functions->set_group_aggregation( abap_false ).
    lo_functions->set_group_print( abap_false ).

    CHECK mo_debugger IS NOT INITIAL AND iv_type = 'L'.

    lo_functions->add_function(
      name     = 'INITIALS'
      icon     = CONV #( icon_start_viewer )
      text     = 'Initials'
      tooltip  = 'Show/hide initial values'
      position = if_salv_c_function_position=>right_of_salv_functions ).

    lo_functions->add_function(
      name     = 'LOCALS'
      icon     = CONV #( icon_foreign_trade )
      text     = 'Locals'
      tooltip  = 'Show/hide locals variables'
      position = if_salv_c_function_position=>right_of_salv_functions ).

    lo_functions->add_function(
      name     = 'GLOBALS'
      icon     = CONV #( icon_foreign_trade )
      text     = 'Globals'
      tooltip  = 'Show/hide global variables'
      position = if_salv_c_function_position=>right_of_salv_functions ).

    lo_functions->add_function(
      name     = 'SYST'
      icon     = CONV #( icon_foreign_trade )
      text     = 'SYST'
      tooltip  = 'Show/hide SY sructure'
      position = if_salv_c_function_position=>right_of_salv_functions ).

    lo_functions->add_function(
      name     = 'CLASS_DATA'
      icon     = CONV #( icon_oo_class_attribute )
      text     = 'CLASS-DATA'
      tooltip  = 'Show/hide Class-Data variables (global)'
      position = if_salv_c_function_position=>right_of_salv_functions ).

    lo_functions->add_function(
      name     = 'LDB'
      icon     = CONV #( icon_biw_report_view )
      text     = 'LDB'
      tooltip  = 'Show/hide Local Data Base variables (global)'
      position = if_salv_c_function_position=>right_of_salv_functions ).

    lo_functions->add_function(
      name     = 'REFRESH'
      icon     = CONV #( icon_refresh )
      text     = ''
      tooltip  = 'Refresh'
      position = if_salv_c_function_position=>left_of_salv_functions ).

  ENDMETHOD.

  METHOD clear.

    tree->get_nodes( )->delete_all( ).

    CLEAR: m_globals_key,
           m_locals_key,
           m_syst_key,
           m_ldb_key,
           m_class_key,
           mt_vars,
           mt_classes_leaf.

  ENDMETHOD.

  METHOD traverse.

    ASSIGN ir_up->* TO FIELD-SYMBOL(<new>).
    IF <new> IS INITIAL AND m_hide IS NOT INITIAL.
      me->del_variable( CONV #( is_var-name )  ).
      RETURN.
    ENDIF.

    CASE io_type_descr->kind.
      WHEN c_kind-struct.
        IF iv_struc_name IS SUPPLIED.
          e_root_key = traverse_struct( io_type_descr  = io_type_descr
                                        iv_parent_key  = iv_parent_key
                                        iv_rel         = iv_rel
                                        is_var         = is_var
                                        ir_up          = ir_up
                                        iv_parent_name = iv_parent_name
                                        iv_struc_name  = iv_struc_name ).
        ELSE.
          e_root_key = traverse_struct( io_type_descr  = io_type_descr
                                        iv_parent_key  = iv_parent_key
                                        iv_rel         = iv_rel
                                        is_var         = is_var
                                        ir_up          = ir_up
                                        iv_parent_name = iv_parent_name ).
        ENDIF.

      WHEN c_kind-table.
        e_root_key = traverse_table( io_type_descr  = io_type_descr
                                     iv_parent_key  = iv_parent_key
                                     iv_rel         = iv_rel
                                     is_var         = is_var
                                     ir_up          = ir_up
                                     iv_parent_name = iv_parent_name ).
      WHEN c_kind-elem.
        e_root_key = traverse_elem( io_type_descr  = io_type_descr
                                    iv_parent_key  = iv_parent_key
                                    iv_rel         = iv_rel
                                    is_var         = is_var
                                    iv_parent_name = iv_parent_name ).

    ENDCASE.

  ENDMETHOD.

  METHOD traverse_struct.

    DATA: lt_component    TYPE abap_component_tab,
          lo_struct_descr TYPE REF TO cl_abap_structdescr,
          ls_tree         TYPE ts_table,
          lv_text         TYPE lvc_value,
          l_key           TYPE salv_de_node_key,
          l_rel           TYPE salv_de_node_relation,
          lv_icon         TYPE salv_de_tree_image.

    ASSIGN is_var-ref->* TO FIELD-SYMBOL(<new_value>).
    l_rel = iv_rel.
    lo_struct_descr ?= io_type_descr.
    ls_tree-ref =  ir_up.
    IF is_var-instance NE '{A:initial}'.
      ls_tree-typename = lo_struct_descr->absolute_name.
      REPLACE FIRST OCCURRENCE OF '\TYPE=' IN ls_tree-typename+0(6) WITH ''.
      IF ls_tree-typename+0(1) = '%'.
        ls_tree-typename = |{ lo_struct_descr->type_kind }({ lo_struct_descr->length / 2 })|.
      ENDIF.
    ENDIF.

    ls_tree-kind = lo_struct_descr->type_kind.

    IF m_icon IS INITIAL.
      lv_icon = icon_structure.
    ELSE.
      lv_icon = m_icon.
    ENDIF.

    lv_text = is_var-short.
    ls_tree-fullname = is_var-name.
    ls_tree-path = is_var-path.

    "own new method
    IF is_var-cl_leaf IS NOT INITIAL.

      add_obj_nodes( EXPORTING is_var = is_var ).

      READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf INTO DATA(ls_leaf).
      IF sy-subrc = 0.
        l_key = ls_leaf-key.
      ENDIF.
    ELSE.
      l_key = iv_parent_key.
    ENDIF.

    IF l_key IS INITIAL.
      l_key = iv_parent_key.
      l_rel = iv_rel.
    ENDIF.

    IF  ( iv_struc_name IS SUPPLIED AND iv_struc_name IS NOT INITIAL ) OR iv_struc_name IS NOT SUPPLIED.
      IF lv_text IS NOT INITIAL.


        DATA(lt_nodes) = tree->get_nodes( )->get_all_nodes( ).
        LOOP AT lt_nodes INTO DATA(ls_nodes).
          DATA(lr_row) = ls_nodes-node->get_data_row( ).
          DATA ls_row TYPE ts_table.
          ls_row = lr_row->*.
          IF ls_row-fullname = is_var-name.
            DATA(l_node) = ls_nodes-node.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF l_node IS NOT INITIAL.
          READ TABLE mt_vars WITH KEY name = is_var-name INTO DATA(l_var).
          IF sy-subrc = 0.
            IF l_node IS NOT INITIAL.
              TRY.
                  FIELD-SYMBOLS: <old_value> TYPE any.
                  ASSIGN l_var-ref->* TO <old_value>.
                  IF sy-subrc = 0.
                    IF is_var-type = l_var-type.
                      RETURN.
                    ELSE.
                      l_key = l_var-key.
                      l_rel = if_salv_c_node_relation=>next_sibling.
                      DELETE mt_vars WHERE name = is_var-name.
                    ENDIF.
                  ENDIF.
                CATCH cx_root.
                  DELETE mt_vars WHERE name = is_var-name.
              ENDTRY.

            ENDIF.
          ENDIF.
          "RETURN.
        ENDIF.
      ENDIF.

      e_root_key = tree->get_nodes( )->add_node(
             related_node   = l_key
             relationship   = l_rel
             data_row       = ls_tree
             collapsed_icon = lv_icon
             expanded_icon  = lv_icon
             text           = lv_text
             folder         = abap_false )->get_key( ).

      APPEND INITIAL LINE TO mt_vars ASSIGNING FIELD-SYMBOL(<vars>).
      <vars>-key = e_root_key.
      <vars>-stack = mo_debugger->mo_window->mt_stack[ 1 ]-stacklevel.
      <vars>-step  = mo_debugger->m_step - mo_debugger->m_step_delta.
      <vars>-program   = mo_debugger->mo_window->m_prg-program.
      <vars>-eventtype = mo_debugger->mo_window->m_prg-eventtype.
      <vars>-eventname = mo_debugger->mo_window->m_prg-eventname.
      <vars>-leaf  = m_leaf.
      <vars>-name  = is_var-name.
      <vars>-short = is_var-short.
      <vars>-ref  = ir_up.
      <vars>-cl_leaf = is_var-cl_leaf.
      <vars>-type = lo_struct_descr->absolute_name.
      <vars>-path = is_var-path.
    ENDIF.

    IF l_rel = if_salv_c_node_relation=>next_sibling AND l_node IS NOT INITIAL.
      IF l_node IS NOT INITIAL.

        l_node->delete( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD traverse_elem.

    DATA: lo_elem_descr TYPE REF TO cl_abap_elemdescr,
          ls_tree       TYPE ts_table,
          lv_text       TYPE lvc_value,
          lv_icon       TYPE salv_de_tree_image,
          l_key         TYPE salv_de_node_key,
          l_rel         TYPE salv_de_node_relation.

    lo_elem_descr ?= io_type_descr.
    ls_tree-ref = is_var-ref.
    l_rel = iv_rel.

    IF is_var-instance NE '{A:initial}'.
      ls_tree-typename = lo_elem_descr->absolute_name.
      REPLACE FIRST OCCURRENCE OF '\TYPE=' IN ls_tree-typename WITH ''.
      IF ls_tree-typename+0(1) = '%'.
        ls_tree-typename = |{ lo_elem_descr->type_kind }({ lo_elem_descr->length / 2 })|.
      ENDIF.
    ENDIF.

    ls_tree-kind = lo_elem_descr->type_kind.

    ASSIGN is_var-ref->* TO FIELD-SYMBOL(<new_value>).
    IF iv_value IS SUPPLIED.
      ls_tree-value = iv_value.
    ELSE.
      IF <new_value> IS NOT INITIAL.
        ls_tree-value = <new_value>.
      ENDIF.
    ENDIF.

    CASE lo_elem_descr->type_kind.
      WHEN 'D'.
        lv_icon = icon_date.
      WHEN 'T'.
        lv_icon = icon_bw_time_sap.
      WHEN 'C'.
        lv_icon = icon_wd_input_field.
      WHEN 'P'.
        lv_icon = icon_increase_decimal.
      WHEN 'g'.
        lv_icon = icon_text_act.
      WHEN 'N' OR 'I'.
        lv_icon = icon_pm_order.
      WHEN OTHERS.
        lv_icon = icon_element.
    ENDCASE.

    lv_text = is_var-short.
    ls_tree-fullname = is_var-name."is_var-path.
    ls_tree-path = is_var-path.

    "own new method
    IF is_var-cl_leaf IS NOT INITIAL.

      add_obj_nodes( EXPORTING is_var = is_var ).

      READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf INTO DATA(ls_leaf).
      IF sy-subrc = 0.
        l_key = ls_leaf-key.
      ENDIF.
    ELSE.
      l_key = iv_parent_key.
    ENDIF.

    IF l_key IS INITIAL.
      l_key = iv_parent_key.
      l_rel = iv_rel.
    ENDIF.

    DATA(lt_nodes) = tree->get_nodes( )->get_all_nodes( ).
    LOOP AT lt_nodes INTO DATA(ls_nodes).
      DATA(lv_name) = ls_nodes-node->get_text( ).
      DATA(lr_row) = ls_nodes-node->get_data_row( ).
      DATA ls_row TYPE ts_table.
      ls_row = lr_row->*.
      IF ls_row-fullname = is_var-name.
        DATA(l_node) = ls_nodes-node.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF mo_debugger->m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.

    IF l_node IS NOT INITIAL.
      READ TABLE mt_vars WITH KEY name = is_var-name INTO DATA(l_var).
      IF sy-subrc = 0.
        TRY.
            FIELD-SYMBOLS: <old_value> TYPE any.
            ASSIGN l_var-ref->* TO <old_value>.
            IF sy-subrc = 0.
              IF is_var-type = l_var-type.
                IF <old_value> NE <new_value>.
                  l_key = l_var-key.
                  l_rel = if_salv_c_node_relation=>next_sibling.
                  DELETE mt_vars WHERE name = is_var-name.
                ELSE.
                  IF ( <new_value> IS INITIAL AND m_hide IS NOT INITIAL ).
                  ELSE.
                    RETURN.
                  ENDIF.
                ENDIF.
              ELSE.
                l_key = l_var-key.
                l_rel = if_salv_c_node_relation=>next_sibling.
                DELETE mt_vars WHERE name = is_var-name.
              ENDIF.
            ENDIF.
          CATCH cx_root.
            DELETE mt_vars WHERE name = is_var-name.
        ENDTRY.
      ENDIF.
    ENDIF.

    DATA(lo_nodes) = tree->get_nodes( ).

    TRY.
        CALL METHOD lo_nodes->add_node
          EXPORTING
            related_node   = l_key
            relationship   = l_rel
            data_row       = ls_tree
            collapsed_icon = lv_icon
            expanded_icon  = lv_icon
            text           = lv_text
            folder         = abap_false
          RECEIVING
            node           = DATA(lo_node).

        IF sy-subrc = 0.
          e_root_key = lo_node->get_key( ).

          APPEND INITIAL LINE TO mt_vars ASSIGNING FIELD-SYMBOL(<vars>).
          <vars>-stack = mo_debugger->mo_window->mt_stack[ 1 ]-stacklevel.
          <vars>-step = mo_debugger->m_step - mo_debugger->m_step_delta.
          <vars>-program = mo_debugger->mo_window->m_prg-program.
          <vars>-eventtype = mo_debugger->mo_window->m_prg-eventtype.
          <vars>-eventname = mo_debugger->mo_window->m_prg-eventname.
          <vars>-leaf = m_leaf.
          <vars>-name = is_var-name.
          <vars>-short = is_var-short.
          <vars>-key = e_root_key.
          <vars>-ref = is_var-ref.
          <vars>-cl_leaf = is_var-cl_leaf.
          <vars>-type = lo_elem_descr->absolute_name.
          <vars>-path = is_var-path.

          IF l_rel = if_salv_c_node_relation=>next_sibling AND l_node IS NOT INITIAL.
            IF l_node IS NOT INITIAL.
              l_node->delete( ).
            ENDIF.
          ENDIF.
        ENDIF.
      CATCH cx_salv_msg.
    ENDTRY.

  ENDMETHOD.

  METHOD traverse_obj.

    DATA: ls_tree TYPE ts_table,
          lv_text TYPE lvc_value,
          lv_icon TYPE salv_de_tree_image,
          l_key   TYPE salv_de_node_key,
          l_rel   TYPE salv_de_node_relation.

    READ TABLE mt_vars WITH KEY name = is_var-name INTO DATA(l_var).
    IF mo_debugger->m_debug IS NOT INITIAL.BREAK-POINT.ENDIF.
    IF sy-subrc = 0.
      DATA(lo_nodes) = tree->get_nodes( ).
      DATA(l_node) =  lo_nodes->get_node( l_var-key ).

      IF l_var-ref = ir_up.
        RETURN.
      ENDIF.

    ELSE.
      l_rel = iv_rel.
    ENDIF.


    lv_icon = icon_oo_object.
    lv_text = is_var-short.
    ls_tree-fullname = is_var-name.
    ls_tree-path = is_var-path.

    "own new method
    IF is_var-cl_leaf IS NOT INITIAL.

      add_obj_nodes( EXPORTING is_var = is_var ).

      READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf INTO DATA(ls_leaf).
      IF sy-subrc = 0.
        l_key = ls_leaf-key.
      ENDIF.
    ENDIF.

    IF l_key IS INITIAL.
      l_key = iv_parent_key.
      l_rel = iv_rel.
    ENDIF.

    e_root_key = tree->get_nodes( )->add_node(
     related_node   = l_key
     relationship   = l_rel
     data_row       = ls_tree
     collapsed_icon = lv_icon
     expanded_icon  = lv_icon
     text           = lv_text
     folder         = abap_false )->get_key( ).

    APPEND INITIAL LINE TO mt_vars ASSIGNING FIELD-SYMBOL(<vars>).
    <vars>-stack = mo_debugger->mo_window->mt_stack[ 1 ]-stacklevel.
    <vars>-step = mo_debugger->m_step - mo_debugger->m_step_delta.
    <vars>-program = mo_debugger->mo_window->m_prg-program.
    <vars>-eventtype = mo_debugger->mo_window->m_prg-eventtype.
    <vars>-eventname = mo_debugger->mo_window->m_prg-eventname.
    <vars>-leaf = m_leaf.
    <vars>-name = is_var-name.
    <vars>-short = is_var-short.
    <vars>-key = e_root_key.
    <vars>-cl_leaf = is_var-cl_leaf.
    <vars>-path = is_var-path.

    IF l_node IS NOT INITIAL.
      l_node->delete( ).
    ENDIF.

  ENDMETHOD.

  METHOD traverse_table.

    DATA: lo_table_descr TYPE REF TO cl_abap_tabledescr,
          ls_tree        TYPE ts_table,
          lv_text        TYPE lvc_value,
          lv_icon        TYPE salv_de_tree_image,
          l_key          TYPE salv_de_node_key,
          l_rel          TYPE salv_de_node_relation.

    FIELD-SYMBOLS: <tab> TYPE ANY TABLE.

    ASSIGN ir_up->* TO <tab>.
    DATA(lines) = lines( <tab> ).
    ls_tree-ref = ir_up.
    l_key = iv_parent_key.

    lo_table_descr ?= io_type_descr.

    ls_tree-fullname = |{ is_var-short } ({ lines })|.
    ls_tree-kind = lo_table_descr->type_kind.
    IF is_var-instance NE '{A:initial}'.

      READ TABLE mo_debugger->mo_window->mt_source WITH KEY include = mo_debugger->ms_stack-include INTO DATA(ls_source).
      READ TABLE ls_source-tt_tabs WITH KEY name = is_var-short INTO DATA(ls_tab).
      IF sy-subrc <> 0.
        ls_tree-typename = replace( val = lo_table_descr->absolute_name sub = '\TYPE=' with = '' ).
      ELSE.
        ls_tree-typename = ls_tab-type.
      ENDIF.
    ENDIF.
    lv_icon = icon_view_table.

    IF is_var-name IS NOT INITIAL.
      lv_text = ls_tree-fullname.
    ELSE.
      lv_text = ls_tree-typename.
    ENDIF.

    l_rel = iv_rel.
    ASSIGN ir_up->* TO FIELD-SYMBOL(<new_value>).
    IF mo_debugger->m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.

    READ TABLE mt_vars WITH KEY name = is_var-name INTO DATA(l_var).
    DATA(lt_nodes) = tree->get_nodes( )->get_all_nodes( ).
    LOOP AT lt_nodes INTO DATA(ls_nodes).
      DATA(lr_row) = ls_nodes-node->get_data_row( ).
      DATA ls_row TYPE ts_table.
      ls_row = lr_row->*.
      IF ls_row-fullname = is_var-name.
        DATA(l_node) = ls_nodes-node.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF l_node IS NOT INITIAL.
      TRY.
          FIELD-SYMBOLS: <old_value> TYPE any.
          ASSIGN l_var-ref->* TO <old_value>.
          IF sy-subrc = 0.
            IF <old_value> NE <new_value>.
              l_key = l_var-key.
              l_rel = if_salv_c_node_relation=>next_sibling.
              DELETE mt_vars WHERE name = is_var-name.
            ELSE.
              IF ( <new_value> IS INITIAL AND m_hide IS NOT INITIAL ).
                me->del_variable( CONV #( is_var-name )  ).
              ENDIF.
            ENDIF.
          ENDIF.

          IF <new_value> IS INITIAL AND m_hide IS NOT INITIAL.
            me->del_variable( CONV #( is_var-name ) ).
            RETURN.
          ENDIF.
        CATCH cx_root.
          me->del_variable( CONV #( is_var-name )  ).
      ENDTRY.
    ELSE.

      IF <new_value> IS INITIAL AND m_hide IS NOT INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    IF is_var-cl_leaf IS NOT INITIAL.

      add_obj_nodes( EXPORTING is_var = is_var ).

      READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf INTO DATA(ls_leaf).
      IF sy-subrc = 0.
        l_key = ls_leaf-key.
      ENDIF.
    ELSE.
      l_key = iv_parent_key.
    ENDIF.

    READ TABLE mt_vars WITH KEY name = iv_parent_name TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.

      ls_tree-fullname = is_var-name.
      e_root_key =
        tree->get_nodes( )->add_node(
          related_node   = l_key
          relationship   = iv_rel
          collapsed_icon = lv_icon
          expanded_icon  = lv_icon
          data_row       = ls_tree
          text           = lv_text
          folder         = abap_true
        )->get_key( ).

      APPEND INITIAL LINE TO mt_vars ASSIGNING FIELD-SYMBOL(<vars>).
      <vars>-stack = mo_debugger->mo_window->mt_stack[ 1 ]-stacklevel.
      <vars>-leaf = m_leaf.
      <vars>-name = is_var-name.
      <vars>-program = mo_debugger->mo_window->m_prg-program.
      <vars>-eventtype = mo_debugger->mo_window->m_prg-eventtype.
      <vars>-eventname = mo_debugger->mo_window->m_prg-eventname.
      <vars>-short = is_var-short.
      <vars>-key = e_root_key.
      <vars>-ref = ir_up.
      <vars>-step = mo_debugger->m_step - mo_debugger->m_step_delta.
      <vars>-cl_leaf = is_var-cl_leaf.
      <vars>-path = is_var-path.

      IF l_rel = if_salv_c_node_relation=>next_sibling AND l_node IS NOT INITIAL.
        IF l_node IS NOT INITIAL.
          l_node->delete( ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD add_node.

    main_node_key =
          tree->get_nodes( )->add_node(
            related_node   = ''
            collapsed_icon = iv_icon
            expanded_icon = iv_icon
            relationship   = if_salv_c_node_relation=>last_child
            row_style = if_salv_c_tree_style=>intensified
            text           = CONV #( iv_name )
            folder         = abap_true
          )->get_key( ).

    CASE iv_name.
      WHEN 'Locals'.
        m_locals_key = main_node_key.
      WHEN 'Globals'.
        m_globals_key = main_node_key.
      WHEN 'LDB'.
        m_ldb_key = main_node_key.

      WHEN 'Class-data global variables'.
        m_class_key = main_node_key.

      WHEN 'System variables'.
        m_syst_key = main_node_key.
    ENDCASE.

  ENDMETHOD.

  METHOD add_obj_nodes.

    DATA lt_match TYPE match_result_tab.
    FIND ALL OCCURRENCES OF  '-' IN is_var-name RESULTS lt_match. "Only first level of instance should be here
    IF lines( lt_match ) > 1.
      RETURN.
    ENDIF.

    DATA lv_text TYPE lvc_value.
    DATA lv_node_key TYPE salv_de_node_key.
    DATA lv_icon TYPE salv_de_tree_image.

    CASE is_var-cl_leaf.
      WHEN 1.
        lv_icon = icon_led_green.
        lv_text = 'Public'.
      WHEN 2.
        lv_icon = icon_led_red.
        lv_text = 'Private'.
      WHEN 3.
        lv_icon = icon_led_yellow.
        lv_text = 'Protected'.
    ENDCASE.

    READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf ASSIGNING FIELD-SYMBOL(<class>).
    IF sy-subrc NE 0.

      READ TABLE mt_vars WITH KEY path = is_var-parent INTO DATA(ls_var).
      lv_node_key =
        tree->get_nodes( )->add_node(
          related_node   = ls_var-key
          relationship   = if_salv_c_node_relation=>last_child
          collapsed_icon = lv_icon
          expanded_icon  = lv_icon
          text           = lv_text
          folder         = abap_true
        )->get_key( ).

      APPEND INITIAL LINE TO mt_classes_leaf ASSIGNING <class>.
      <class>-name = is_var-parent.
      <class>-key = lv_node_key.
      <class>-type = is_var-cl_leaf.
    ENDIF.

  ENDMETHOD.

  METHOD delete_node.

    DATA(lo_nodes) = tree->get_nodes( ).
    DATA(l_node) =  lo_nodes->get_node( iv_key ).
    IF l_node IS NOT INITIAL.
      l_node->delete( ).

    ENDIF.

  ENDMETHOD.

  METHOD display.

    DATA(lo_columns) = tree->get_columns( ).
    lo_columns->get_column( 'KIND' )->set_visible( abap_false ).

    DATA(lo_nodes) = tree->get_nodes( ).
    DATA(lt_nodes) =  lo_nodes->get_all_nodes( ).


    DATA lt_sub TYPE salv_t_nodes.
    LOOP AT lt_nodes INTO DATA(l_node).
      READ TABLE lt_sub WITH KEY node = l_node-node TRANSPORTING NO FIELDS. "expanding only first level nodes.
      IF sy-subrc NE 0.
        TRY.
            l_node-node->expand( ).
            lt_sub = l_node-node->get_subtree( ).
          CATCH cx_root.
        ENDTRY.
      ENDIF.
    ENDLOOP.
    tree->display( ).

  ENDMETHOD.

  METHOD hndl_user_command.

    CONSTANTS: c_mask TYPE x VALUE '01'.

    CASE e_salv_function.

      WHEN 'REFRESH'."
        m_refresh = abap_true.
        mo_debugger->run_script_hist( mo_debugger->m_hist_step ).
        mo_debugger->mo_tree_local->display( ).
        RETURN.

      WHEN 'INITIALS'."Show/hide empty variables
        m_hide = m_hide BIT-XOR c_mask.
        m_clear = abap_true.

      WHEN 'LOCALS'."Show/hide locals variables
        m_locals = m_locals BIT-XOR c_mask.
      WHEN 'GLOBALS'."Show/hide global variables
        m_globals = m_globals BIT-XOR c_mask.
      WHEN 'SYST'."Show/hide sy structure
        m_syst = m_syst BIT-XOR c_mask.
      WHEN 'CLASS_DATA'."Show/hide CLASS-DATA variables (globals)
        m_class_data = m_class_data BIT-XOR c_mask.
      WHEN 'LDB'."Show/hide LDB variables (globals)
        m_ldb = m_ldb BIT-XOR c_mask.
    ENDCASE.

    mo_debugger->m_update = abap_true.

    mo_debugger->mo_tree_local->clear( ).
    mo_debugger->mo_tree_exp->clear( ).
    mo_debugger->mo_tree_imp->clear( ).

    CLEAR mo_debugger->mo_window->m_debug_button.
    IF mo_debugger->m_hist_step = mo_debugger->m_step.
      CLEAR mo_debugger->is_history.
    ENDIF.
    IF e_salv_function NE 'TEST'.

      IF mo_debugger->is_history = abap_true.

        mo_debugger->run_script_hist( ).
      ELSE.
        mo_debugger->run_script( ).
        mo_debugger->hndl_script_buttons( mo_debugger->mv_stack_changed ).
      ENDIF.
      mo_debugger->show_step( ).
    ENDIF.

  ENDMETHOD.

  METHOD hndl_double_click.

    DATA(lo_nodes) = tree->get_nodes( ).
    DATA(l_node) =  lo_nodes->get_node( node_key ).
    DATA r_row TYPE REF TO data.

    r_row = l_node->get_data_row( ).
    ASSIGN r_row->* TO FIELD-SYMBOL(<row>).
    ASSIGN COMPONENT 'REF' OF STRUCTURE <row> TO FIELD-SYMBOL(<ref>).
    ASSIGN COMPONENT 'KIND' OF STRUCTURE <row> TO FIELD-SYMBOL(<kind>).
    ASSIGN COMPONENT 'FULLNAME' OF STRUCTURE <row> TO FIELD-SYMBOL(<fullname>).
    ASSIGN COMPONENT 'PATH' OF STRUCTURE <row> TO FIELD-SYMBOL(<path>).

    IF <fullname> IS NOT INITIAL.
      READ TABLE mo_debugger->mt_selected_var WITH KEY name =  <fullname> TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        DELETE mo_debugger->mt_selected_var WHERE name = <fullname>.
        l_node->set_row_style( if_salv_c_tree_style=>default ).
      ELSE.
        l_node->set_row_style( if_salv_c_tree_style=>emphasized_b ).
        APPEND INITIAL LINE TO mo_debugger->mt_selected_var ASSIGNING FIELD-SYMBOL(<sel>).
        <sel>-name = <fullname>.
        <sel>-is_sel = abap_true.
      ENDIF.

      CASE <kind>.
        WHEN cl_abap_datadescr=>typekind_table.
          lcl_appl=>open_int_table( iv_name = <fullname> it_ref = <ref> io_window = mo_debugger->mo_window ).
        WHEN cl_abap_datadescr=>typekind_string.
          NEW lcl_text_viewer( <ref> ).
      ENDCASE.
    ENDIF.

  ENDMETHOD.

  METHOD del_variable.

    IF mo_debugger->m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.
    DATA(lt_hist) = mo_debugger->mt_vars_hist.
    SORT lt_hist BY step DESCENDING.
    LOOP AT lt_hist INTO DATA(ls_hist) WHERE name = iv_full_name.
      IF ls_hist-del IS INITIAL.
        CLEAR: ls_hist-ref, ls_hist-first.
        ls_hist-del = abap_true.
        ls_hist-step = mo_debugger->m_hist_step - 1.
        INSERT ls_hist INTO mo_debugger->mt_vars_hist INDEX 1.
      ENDIF.
    ENDLOOP.

    DATA(lo_nodes) = tree->get_nodes( ).
    READ TABLE mo_debugger->mt_state WITH KEY name = iv_full_name ASSIGNING FIELD-SYMBOL(<var>).
    IF sy-subrc = 0.

      TRY.
          DATA(l_node) =  lo_nodes->get_node( <var>-key ).
        CATCH cx_salv_msg.
      ENDTRY.

      DELETE mt_vars WHERE name = iv_full_name.
      DELETE mt_classes_leaf WHERE name = iv_full_name.
      IF i_state = abap_true.
        DELETE mo_debugger->mt_state WHERE name = iv_full_name.
      ENDIF.

      DATA(l_nam) = iv_full_name && '-'.
      DELETE mt_vars WHERE name CS l_nam.
      DELETE mt_classes_leaf WHERE name  CS l_nam.
      IF i_state = abap_true.
        DELETE mo_debugger->mt_state WHERE name CS l_nam.
      ENDIF.
      TRY.
          IF l_node IS NOT INITIAL.
            l_node->delete( ).
          ENDIF.
        CATCH cx_salv_msg.
      ENDTRY.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_dragdrop IMPLEMENTATION.

  METHOD drag.

    DATA(dataobj) = NEW lcl_dd_data( ).
    dataobj->m_row = e_row-index.
    dataobj->m_column = e_column.
    e_dragdropobj->object = dataobj.

  ENDMETHOD.

  METHOD drop."It should be refactored someday...

    DATA: ls_row          TYPE lcl_types=>t_sel_row,
          lv_set_receiver.

    LOOP AT lcl_appl=>mt_obj INTO DATA(lo).
      "to
      IF lo-alv_viewer->mo_sel IS BOUND.
        IF e_dragdropobj->droptargetctrl = lo-alv_viewer->mo_sel->mo_sel_alv.
          DATA(lo_to) = lo-alv_viewer->mo_sel.
        ENDIF.
      ENDIF.

      "from tab
      IF lo-alv_viewer->mo_alv = e_dragdropobj->dragsourcectrl.
        DATA(lo_from_tab) = lo-alv_viewer.
        CONTINUE.
      ENDIF.

      IF e_dragdropobj->dragsourcectrl = lo-alv_viewer->mo_sel->mo_sel_alv.
        DATA(lo_from_sel) = lo-alv_viewer->mo_sel.
        lo-alv_viewer->mo_sel->mo_sel_alv->get_selected_rows( IMPORTING et_index_rows = DATA(lt_sel_rows) ).
        lo-alv_viewer->mo_sel->mo_sel_alv->get_selected_cells( IMPORTING et_cell = DATA(lt_sel_cells) ).
      ENDIF.
    ENDLOOP.

    IF lo_from_tab IS BOUND." tab to select
      FIELD-SYMBOLS: <f_tab>   TYPE STANDARD TABLE,
                     <f_field> TYPE any.
      lo_from_tab->mo_alv->get_selected_cells( IMPORTING et_cell = lt_sel_cells ).
      lo_from_tab->mo_alv->get_selected_columns( IMPORTING et_index_columns = DATA(lt_sel_col) ).

      LOOP AT lt_sel_col INTO DATA(l_col).
        TRY.
            lo_from_tab->mt_alv_catalog[ fieldname = l_col-fieldname ]-style = cl_gui_alv_grid=>mc_style_button.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
        READ TABLE lo_from_tab->mo_column_emitters WITH KEY column = l_col ASSIGNING FIELD-SYMBOL(<emitter>).
        IF sy-subrc NE 0.
          APPEND INITIAL LINE TO lo_from_tab->mo_column_emitters ASSIGNING <emitter>.
          <emitter>-column = l_col.
          <emitter>-emitter = NEW #( ).
        ENDIF.
      ENDLOOP.

      IF sy-subrc = 0.
        lv_set_receiver = abap_true.
        CALL METHOD lo_from_tab->mo_alv->set_frontend_fieldcatalog EXPORTING it_fieldcatalog = lo_from_tab->mt_alv_catalog.
      ENDIF.

      TRY.
          ASSIGN lo_from_tab->mr_table->* TO <f_tab>.
          READ TABLE lo_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to_tab>) INDEX e_row.
          LOOP AT lt_sel_cells INTO DATA(l_cell).
            IF sy-tabix = 1.
              DATA(l_colname) = l_cell-col_id-fieldname.
            ENDIF.
            READ TABLE <f_tab> INDEX l_cell-row_id ASSIGNING FIELD-SYMBOL(<f_str>).
            ASSIGN COMPONENT l_colname OF STRUCTURE <f_str> TO <f_field>.
            IF sy-subrc = 0.
              IF lv_set_receiver IS NOT INITIAL.
                IF <to_tab>-receiver IS BOUND.
                  <to_tab>-receiver->shut_down( ).
                ENDIF.
                CREATE OBJECT <to_tab>-receiver
                  EXPORTING
                    io_transmitter = <emitter>-emitter
                    i_from_field   = CONV #( lt_sel_cells[ 1 ]-col_id )
                    i_to_field     = <to_tab>-field_label
                    io_sel_to      = lo_to
                    io_tab_from    = lo_from_tab.
                SET HANDLER <to_tab>-receiver->on_grid_button_click FOR lo_from_tab->mo_alv.
              ENDIF.

              IF <to_tab>-range IS INITIAL.
                <to_tab>-low = <f_field>.
              ENDIF.
              IF NOT line_exists( <to_tab>-range[ low = <f_field> ] ).
                APPEND VALUE #( sign = 'I' opti = 'EQ' low = <f_field>  ) TO <to_tab>-range.
              ENDIF.
            ENDIF.
          ENDLOOP.
          lo_to->update_sel_row( CHANGING c_sel_row = <to_tab> ).
        CATCH cx_sy_itab_line_not_found.                "#EC NO_HANDLER
      ENDTRY.
    ENDIF.

    "select to select
    IF lo_from_sel NE lo_to.
      IF lt_sel_rows[] IS INITIAL.
        DELETE lt_sel_cells WHERE col_id NE 'FIELD_LABEL'.
        LOOP AT lt_sel_cells INTO DATA(l_sel).
          APPEND INITIAL LINE TO lt_sel_rows ASSIGNING FIELD-SYMBOL(<row>).
          <row>-index = l_sel-row_id-index.
        ENDLOOP.
      ENDIF.

      LOOP AT lt_sel_rows ASSIGNING <row>.
        READ TABLE lo_from_sel->mt_sel_tab ASSIGNING FIELD-SYMBOL(<from_tab>) INDEX <row>-index.
        IF lines( lt_sel_rows ) = 1.
          READ TABLE lo_to->mt_sel_tab ASSIGNING <to_tab> INDEX e_row.
        ELSE.
          READ TABLE lo_to->mt_sel_tab ASSIGNING <to_tab> WITH KEY field_label = <from_tab>-field_label.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.
        ENDIF.
        MOVE-CORRESPONDING <from_tab> TO ls_row.
        MOVE-CORRESPONDING ls_row TO <to_tab>.
        <from_tab>-emitter = icon_workflow_external_event.
        <to_tab>-inherited = icon_businav_value_chain.
        IF <from_tab>-transmitter IS INITIAL.
          CREATE OBJECT <from_tab>-transmitter.
        ENDIF.
        IF <to_tab>-receiver IS NOT INITIAL.
          <to_tab>-receiver->shut_down( ). "receiver clearing
          FREE <to_tab>-receiver.
        ENDIF.
        CREATE OBJECT <to_tab>-receiver
          EXPORTING
            io_transmitter = <from_tab>-transmitter
            io_sel_to      = lo_to
            i_to_field     = <to_tab>-field_label.
      ENDLOOP.
    ENDIF.

    DATA(lo_alv) = CAST cl_gui_alv_grid( e_dragdropobj->dragsourcectrl ).
    lcl_alv_common=>refresh( EXPORTING i_obj = lo_alv ).

    lo_alv ?= e_dragdropobj->droptargetctrl.
    lo_to->raise_selection_done( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_source_parser IMPLEMENTATION.

  METHOD parse_tokens.

    DATA: lr_scan       TYPE REF TO cl_ci_scan,
          lv_prev       TYPE string,
          lv_change     TYPE string,
          lt_split      TYPE TABLE OF string,
          lo_scan       TYPE REF TO cl_ci_scan,
          lo_statement  TYPE REF TO if_ci_kzn_statement_iterator,
          lo_procedure  TYPE REF TO if_ci_kzn_statement_iterator,
          ls_token      TYPE lcl_window=>ts_kword,
          ls_calculated TYPE lcl_window=>ts_calculated,
          ls_composed   TYPE lcl_window=>ts_composing,
          lt_tokens     TYPE lcl_window=>tt_kword,
          lt_calculated TYPE lcl_window=>tt_calculated,
          lt_composed   TYPE lcl_window=>tt_composed,
          ls_call       TYPE lcl_window=>ts_calls,
          ls_call_line  TYPE lcl_window=>ts_calls_line,
          ls_tabs       TYPE lcl_window=>ts_int_tabs,
          lt_tabs       TYPE lcl_window=>tt_tabs,
          lv_eventtype  TYPE string,
          lv_eventname  TYPE string,
          ls_param      TYPE lcl_window=>ts_params,
          lv_par        TYPE char1,
          lv_type       TYPE char1,
          lv_class      TYPE xfeld,
          lv_cl_name    TYPE string,
          lv_preferred  TYPE xfeld.

    "CLEAR mv_step.

    READ TABLE io_debugger->mo_window->mt_source WITH KEY include = iv_program INTO DATA(ls_source).
    IF sy-subrc <> 0.

      ls_source-source = cl_ci_source_include=>create( p_name = iv_program ).
      lo_scan = NEW cl_ci_scan( p_include = ls_source-source ).

      ls_source-include = iv_program.

      lo_statement = cl_cikzn_scan_iterator_factory=>get_statement_iterator( ciscan = lo_scan ).
      lo_procedure = cl_cikzn_scan_iterator_factory=>get_procedure_iterator( ciscan = lo_scan ).

      TRY.
          lo_statement->next( ).
        CATCH cx_scan_iterator_reached_end.
          EXIT.
      ENDTRY.

      DATA(lt_kw) = lo_statement->get_keyword( ).

      DATA(token) = lo_statement->get_token( offset = 2 ).

      lo_procedure->statement_index = lo_statement->statement_index.
      lo_procedure->statement_type = lo_statement->statement_type.

      DATA(lv_max) = lines( lo_scan->statements ).
      DO.
        CLEAR ls_token-tt_calls.
        TRY.
            lo_procedure->next( ).
          CATCH cx_scan_iterator_reached_end.
        ENDTRY.

        lt_kw = lo_procedure->get_keyword( ).

        ls_token-name = lt_kw.
        ls_token-index = lo_procedure->statement_index.
        READ TABLE lo_scan->statements INDEX lo_procedure->statement_index INTO DATA(ls_statement).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        READ TABLE lo_scan->tokens INDEX ls_statement-from INTO DATA(l_token).
        ls_token-line = ls_calculated-line = ls_composed-line = l_token-row.

        DATA lv_new TYPE xfeld.

        IF lt_kw = 'CLASS'.
          lv_class = abap_true.
        ENDIF.

        IF lt_kw = 'FORM' OR lt_kw = 'METHOD' OR lt_kw = 'METHODS' OR lt_kw = 'CLASS-METHODS'.
          ls_tabs-eventtype = lv_eventtype = ls_param-event =  lt_kw.

          CLEAR lv_eventname.
          IF lt_kw = 'FORM'.
            CLEAR: lv_class, ls_param-class.
          ELSE.
            ls_tabs-eventtype = lv_eventtype = ls_param-event =  'METHOD'.
          ENDIF.
        ENDIF.

        IF lt_kw = 'ENDFORM' OR lt_kw = 'ENDMETHOD'.
          CLEAR: lv_eventtype, lv_eventname, ls_tabs.
          IF ls_param-param IS INITIAL. "No params - save empty row if no params
            READ TABLE ls_source-t_params WITH KEY event = ls_param-event name = ls_param-name TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              CLEAR ls_param-type.
              APPEND ls_param TO ls_source-t_params.
            ENDIF.
          ENDIF.
        ENDIF.

        CLEAR lv_prev.
        IF lt_kw = 'ASSIGN' OR lt_kw = 'ADD' OR lt_kw = 'SUBTRACT' .
          DATA(lv_count) = 0.
        ENDIF.
        CLEAR: lv_new, ls_token-to_evname, ls_token-to_evtype .


        WHILE 1 = 1.
          IF lt_kw IS INITIAL.
            EXIT.
          ENDIF.
          CLEAR lv_change.
          token = lo_procedure->get_token( offset = sy-index ).

          IF ( token CS '(' AND ( NOT token CS ')' ) ) OR token CS '->' OR token CS '=>'."can be method call
            ls_call-name = token.
            ls_call-event = 'METHOD'.
            REPLACE ALL OCCURRENCES OF '(' IN ls_call-name WITH ''.
            FIND FIRST OCCURRENCE OF '->' IN  ls_call-name.
            IF sy-subrc = 0.
              SPLIT ls_call-name  AT '->' INTO TABLE lt_split.
              ls_call-name = lt_split[ 2 ].
            ENDIF.

            FIND FIRST OCCURRENCE OF '=>' IN  ls_call-name.
            IF sy-subrc = 0.
              SPLIT ls_call-name  AT '=>' INTO TABLE lt_split.
              ls_call-name = lt_split[ 2 ].
            ENDIF.
            ls_token-to_evname = ls_call-name.
            ls_token-to_evtype = ls_call-event = 'METHOD'.
            IF lv_new = abap_true.
              ls_call-name =  ls_token-to_evname = 'CONSTRUCTOR'.
            ENDIF.
          ENDIF.

          IF sy-index = 1 AND ls_token-name = token.
            CONTINUE.
          ENDIF.

          IF sy-index = 2 AND ( lt_kw = 'DATA' OR lt_kw = 'PARAMETERS' ).
            WRITE: 'var =', token.
            ls_tabs-name = token.
          ENDIF.

          IF sy-index = 2 AND lt_kw = 'PERFORM'.
            ls_token-to_evname = ls_call-name = token.
            ls_token-to_evtype = ls_call-event = 'FORM'.
          ENDIF.

          IF sy-index = 2 AND lv_class = abap_true AND ls_param-class IS INITIAL.
            ls_call_line-class = ls_param-class = token.
          ENDIF.

          IF sy-index = 2 AND lv_eventtype IS NOT INITIAL AND lv_eventname IS INITIAL.
            ls_tabs-eventname = lv_eventname = ls_param-name =  token.

            MOVE-CORRESPONDING ls_tabs TO ls_call_line.
            ls_call_line-index = lo_procedure->statement_index + 1.
            "methods in definition should be overwrited by Implementation section
            READ TABLE ls_source-tt_calls_line WITH KEY eventname = ls_call_line-eventname eventtype = ls_call_line-eventtype ASSIGNING FIELD-SYMBOL(<call_line>).
            IF sy-subrc = 0.
              <call_line> = ls_call_line.
            ELSE.
              APPEND ls_call_line TO ls_source-tt_calls_line.
            ENDIF.

          ENDIF.

          IF token = ''.
            CLEAR ls_call.
            CASE lt_kw.
              WHEN 'COMPUTE'.
                IF  NOT lv_prev CO '0123456789.+-/* '.
                  ls_composed-composing = lv_prev.
                  APPEND  ls_composed TO lt_composed.
                ENDIF.
              WHEN 'CLEAR' OR 'SORT' OR 'CONDENSE'."no logic
              WHEN 'FORM'.
                IF ls_param-name IS NOT INITIAL.
                  APPEND ls_param TO ls_source-t_params.
                  CLEAR ls_param.
                ENDIF.
            ENDCASE.
            EXIT.
          ENDIF.

          IF token = 'USING' OR token = 'IMPORTING'.
            ls_param-type = 'I'.
            CLEAR: lv_type, lv_par.
          ELSEIF token = 'CHANGING' OR token = 'EXPORTING' OR token = 'RETURNING'.

            IF ls_param-param IS NOT INITIAL.
              APPEND ls_param TO ls_source-t_params.
              CLEAR: lv_type, lv_par, ls_param-param.
            ENDIF.

            ls_param-type = 'E'.
            CLEAR: lv_type, lv_par.
          ELSEIF token = 'OPTIONAL' OR token = 'PREFERRED'.
            CONTINUE.
          ELSEIF token = 'PARAMETER'.
            lv_preferred = abap_true.
            CONTINUE.
          ENDIF.

          IF lv_preferred = abap_true.
            READ TABLE ls_source-t_params WITH KEY event = 'METHOD' name = ls_param-name param = token ASSIGNING FIELD-SYMBOL(<param>).
            IF sy-subrc = 0.
              <param>-preferred = abap_true.
            ENDIF.

            CLEAR lv_preferred.
            CONTINUE.
          ENDIF.

          IF token <> 'CHANGING' AND token <> 'EXPORTING' AND token <> 'RETURNING' AND token <> 'IMPORTING' AND token <> 'USING'.
            IF lt_kw = 'FORM' OR lt_kw = 'METHODS' OR lt_kw = 'CLASS-METHODS'.
              IF lv_par = abap_true AND lv_type IS INITIAL AND  token NE 'TYPE'.

                APPEND ls_param TO ls_source-t_params.
                CLEAR: lv_par, ls_param-param.
              ENDIF.

              IF lv_par IS INITIAL AND sy-index > 3.
                ls_param-param = token.
                lv_par = abap_true.
                CONTINUE.
              ENDIF.
              IF lv_par = abap_true AND lv_type IS INITIAL AND token = 'TYPE'.
                lv_type = abap_true.
                CONTINUE.
              ENDIF.
              IF lv_par = abap_true AND lv_type = abap_true.

                APPEND ls_param TO ls_source-t_params.
                CLEAR: lv_type, lv_par, ls_param-param.
              ENDIF.
            ENDIF.
          ENDIF.

          DATA lv_temp TYPE char30.
          lv_temp = token.

          IF lv_temp+0(5) = 'DATA('.
            SHIFT lv_temp LEFT BY 5 PLACES.
            REPLACE ALL OCCURRENCES OF ')' IN lv_temp WITH ''.
          ENDIF.

          IF lv_temp+0(6) = '@DATA('.
            SHIFT lv_temp LEFT BY 6 PLACES.
            REPLACE ALL OCCURRENCES OF ')' IN lv_temp WITH ''.
          ENDIF.

          IF lv_temp+0(13) = 'FIELD-SYMBOL('.
            SHIFT lv_temp LEFT BY 13 PLACES.
            REPLACE ALL OCCURRENCES OF ')' IN lv_temp WITH ''.
          ENDIF.

          IF token = 'NEW'.
            lv_new = abap_true.
          ENDIF.

          FIND FIRST OCCURRENCE OF '->' IN token.
          IF sy-subrc = 0.
            CLEAR lv_new.
          ENDIF.

          CASE lt_kw.
            WHEN 'DATA' OR 'PARAMETERS'.
              IF (  lv_prev = 'OF' ) AND lv_temp <> 'TABLE' AND lv_temp <> 'OF'.
                ls_tabs-type = lv_temp.
                APPEND ls_tabs TO lt_tabs.
              ENDIF.

            WHEN 'COMPUTE'.
              IF lv_temp CA '=' AND lv_new IS INITIAL..
                lv_change = lv_prev.
              ENDIF.

              IF ( lv_prev = '=' OR lv_prev CA '+-/*' ) AND lv_temp <> 'NEW'.
                IF NOT lv_temp  CA '()' .
                  IF NOT lv_temp  CO '0123456789. '.
                    ls_composed-composing = lv_temp.
                    APPEND  ls_composed TO lt_composed.
                    IF ls_call IS NOT INITIAL.
                      ls_call-outer = lv_temp.
                      READ TABLE ls_token-tt_calls WITH KEY event = ls_call-event name = ls_call-name outer = ls_call-outer TRANSPORTING  NO FIELDS.
                      IF sy-subrc <> 0.
                        APPEND ls_call TO ls_token-tt_calls.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.

            WHEN 'PERFORM' .

              IF  lv_temp = 'USING' OR lv_temp = 'CHANGING' .
                CLEAR lv_prev.
              ENDIF.

              IF  lv_prev = 'USING' OR lv_prev = 'CHANGING' .

                IF NOT lv_temp  CA '()' .
                  IF NOT lv_temp  CO '0123456789. '.
                    ls_call-outer = lv_temp.
                    READ TABLE ls_token-tt_calls WITH KEY event = ls_call-event name = ls_call-name outer = ls_call-outer TRANSPORTING  NO FIELDS.
                    IF sy-subrc <> 0.
                      APPEND ls_call TO ls_token-tt_calls.
                    ENDIF.
                    lv_change = lv_temp.
                  ENDIF.
                ENDIF.
              ENDIF.

            WHEN 'CREATE' OR 'CALL'.
              DATA: lv_import TYPE xfeld,
                    lv_export.

              IF lv_prev = 'FUNCTION' AND lt_kw = 'CALL'.
                ls_token-to_evtype =   ls_call-event = 'FUNCTION'.
                ls_token-to_evname =  ls_call-name = token.
                REPLACE ALL OCCURRENCES OF '''' IN  ls_token-to_evname WITH ''.
              ENDIF.

              IF token = 'EXPORTING' OR token = 'CHANGING' OR token = 'TABLES'.
                lv_export = abap_true.
                CLEAR lv_import.
                CONTINUE.

              ELSEIF token = 'IMPORTING'.
                lv_import = abap_true.
                CLEAR lv_export.
                CONTINUE.

              ENDIF.

              IF lv_prev = 'OBJECT'.
                "WRITE : 'value', lv_temp.
*          CONTINUE.
              ENDIF.

              IF  lv_prev = '='.
                IF NOT lv_temp  CA '()'.
                  IF NOT lv_temp  CO '0123456789. '.
                    IF lv_import = abap_true.
                      ls_call-outer = lv_temp.
                      READ TABLE ls_token-tt_calls WITH KEY event = ls_call-event name = ls_call-name outer = ls_call-outer TRANSPORTING  NO FIELDS.
                      IF sy-subrc <> 0.
                        APPEND ls_call TO ls_token-tt_calls.
                      ENDIF.
                      ls_calculated-calculated = lv_temp.
                      APPEND  ls_calculated TO lt_calculated.
                    ELSEIF lv_export = abap_true.
                      ls_call-outer = lv_temp.
                      READ TABLE ls_token-tt_calls WITH KEY event = ls_call-event name = ls_call-name outer = ls_call-outer TRANSPORTING  NO FIELDS.
                      IF sy-subrc <> 0.
                        APPEND ls_call TO ls_token-tt_calls.
                      ENDIF.
                      ls_composed-composing = lv_temp.
                      APPEND  ls_composed TO lt_composed.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ELSE.
                IF NOT lv_temp  CO '0123456789. ' AND lv_temp <> '=' AND ( lv_import = abap_true OR lv_export = abap_true ).
                  ls_call-inner = lv_temp.
                ENDIF.
              ENDIF.

            WHEN 'CLEAR' OR 'SORT'.
              lv_change = lv_temp.
            WHEN  'CONDENSE'.

              IF lv_temp <> 'NO-GAPS'.
                lv_change = lv_temp.
              ENDIF.
            WHEN 'ASSIGN' OR 'UNASSIGN'.
              ADD 1 TO lv_count.
              IF lv_count <> 2.
                lv_change = lv_temp.
              ENDIF.
            WHEN 'ADD' OR 'SUBTRACT'.
              ADD 1 TO lv_count.
              IF lv_count = 1.
                IF  NOT lv_temp CO '0123456789.() '.
                  ls_composed-composing = lv_temp.
                  APPEND  ls_composed TO lt_composed.
                ENDIF.
              ENDIF.
              IF lv_count = 3.
                lv_change = lv_temp.
              ENDIF.
            WHEN 'READ'.
              IF lv_prev =  'INTO' OR lv_prev =  'ASSIGNING'.
                lv_change = lv_temp.
              ENDIF.

            WHEN 'SELECT'.
              IF  ( lv_prev =  'INTO' OR lv_prev =  '(' ) AND ( lv_temp <> 'TABLE' AND lv_temp <> '('  AND lv_temp <> ')' AND  lv_temp <> ',' ).
                lv_change = lv_temp.
              ENDIF.

            WHEN OTHERS.

          ENDCASE.
          IF ls_call-event = 'METHOD'.
            IF token = 'EXPORTING' OR token = 'CHANGING' OR token = 'TABLES'.
              lv_export = abap_true.
              CLEAR lv_import.
              CONTINUE.

            ELSEIF token = 'IMPORTING'.
              lv_import = abap_true.
              CLEAR lv_export.
              CONTINUE.
            ENDIF.

            IF  lv_temp = 'USING' OR lv_temp = 'CHANGING' .
              CLEAR lv_prev.
            ENDIF.

            IF  lv_prev = 'USING' OR lv_prev = 'CHANGING' .

              IF NOT lv_temp  CA '()' .
                IF NOT lv_temp  CO '0123456789. '.
                  ls_call-outer = lv_temp.
                  READ TABLE ls_token-tt_calls WITH KEY event = ls_call-event name = ls_call-name outer = ls_call-outer TRANSPORTING  NO FIELDS.
                  IF sy-subrc <> 0.
                    APPEND ls_call TO ls_token-tt_calls.
                  ENDIF.
                  lv_change = lv_temp.
                ENDIF.
              ENDIF.
            ENDIF.

            IF  lv_prev = '='.
              IF NOT lv_temp  CA '()'.
                IF NOT lv_temp  CO '0123456789. '.
                  IF lv_import = abap_true.
                    ls_call-outer = lv_temp.
                    READ TABLE ls_token-tt_calls WITH KEY event = ls_call-event name = ls_call-name outer = ls_call-outer TRANSPORTING  NO FIELDS.
                    IF sy-subrc <> 0.
                      APPEND ls_call TO ls_token-tt_calls.
                    ENDIF.

                    ls_calculated-calculated = lv_temp.
                    APPEND  ls_calculated TO lt_calculated.
                  ELSEIF lv_export = abap_true.
                    ls_call-outer = lv_temp.
                    READ TABLE ls_token-tt_calls WITH KEY event = ls_call-event name = ls_call-name outer = ls_call-outer TRANSPORTING  NO FIELDS.
                    IF sy-subrc <> 0.
                      APPEND ls_call TO ls_token-tt_calls.
                    ENDIF.
                    ls_composed-composing = lv_temp.
                    APPEND  ls_composed TO lt_composed.
                  ENDIF.
                ENDIF.
              ENDIF.
            ELSE.
              IF NOT lv_temp  CO '0123456789. ' AND lv_temp <> '=' AND ( lv_import = abap_true OR lv_export = abap_true ).
                ls_call-inner = lv_temp.
              ENDIF.
            ENDIF.

          ENDIF.

          IF lv_temp = '(' .
            lv_prev = lv_temp.
            CONTINUE.
          ENDIF.

          IF  NOT lv_temp  CA '()'.
            IF lv_temp <> 'TABLE' AND lv_temp <> 'NEW'  AND lv_prev <> '('.
              IF  lt_kw <> 'PERFORM'.
                lv_prev = lv_temp.
              ELSEIF token = 'USING' OR token = 'CHANGING'.
                lv_prev = lv_temp.
              ENDIF.
            ENDIF.
          ENDIF.

          IF lv_change IS NOT INITIAL.
            ls_calculated-calculated = lv_change.
            APPEND ls_calculated TO lt_calculated.

            IF lv_change+0(1) = '<'.

              SPLIT lv_change AT '-' INTO TABLE lt_split.
              lv_change = lt_split[ 1 ].
              IF lv_eventtype IS INITIAL. "Global fs
                READ TABLE io_debugger->mo_window->mt_globals_set WITH KEY program = iv_program ASSIGNING FIELD-SYMBOL(<globals_set>).
                IF sy-subrc <> 0.
                  APPEND INITIAL LINE TO io_debugger->mo_window->mt_globals_set ASSIGNING <globals_set>.
                  <globals_set>-program = iv_program.
                ENDIF.
                READ TABLE  <globals_set>-mt_fs WITH KEY name = lv_change TRANSPORTING NO FIELDS.
                IF sy-subrc <> 0.
                  APPEND INITIAL LINE TO  <globals_set>-mt_fs ASSIGNING FIELD-SYMBOL(<gl_fs>).
                  <gl_fs>-name = lv_change.
                ENDIF.

              ELSE."local fs
                READ TABLE io_debugger->mo_window->mt_locals_set
                 WITH KEY program = iv_program eventtype = lv_eventtype eventname = lv_eventname
                 ASSIGNING FIELD-SYMBOL(<locals_set>).
                IF sy-subrc <> 0.
                  APPEND INITIAL LINE TO io_debugger->mo_window->mt_locals_set ASSIGNING <locals_set>.
                  <locals_set>-program = iv_program.
                  <locals_set>-eventname = lv_eventname.
                  <locals_set>-eventtype = lv_eventtype.
                ENDIF.
                READ TABLE <locals_set>-mt_fs WITH KEY name = lv_change TRANSPORTING NO FIELDS.
                IF sy-subrc <> 0.
                  APPEND INITIAL LINE TO <locals_set>-mt_fs ASSIGNING FIELD-SYMBOL(<loc_fs>).
                  <loc_fs>-name = lv_change.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDWHILE.
        ls_token-from = ls_statement-from.
        ls_token-to = ls_statement-to.
        APPEND ls_token TO lt_tokens.
        IF lo_procedure->statement_index = lv_max.
          EXIT.
        ENDIF.

      ENDDO.

      "Fill keyword links for perform

      LOOP AT lt_tokens ASSIGNING FIELD-SYMBOL(<s_token>) WHERE tt_calls IS NOT INITIAL.

        READ TABLE <s_token>-tt_calls INDEX 1 INTO ls_call.
        DATA(lv_index) = 0.
        LOOP AT ls_source-t_params INTO ls_param WHERE event = ls_call-event AND name = ls_call-name .
          ADD 1 TO lv_index.
          READ TABLE <s_token>-tt_calls INDEX lv_index ASSIGNING FIELD-SYMBOL(<call>).
          IF sy-subrc = 0.
            <call>-inner = ls_param-param.
            IF ls_param-type = 'I'.
              <call>-type = '>'.
            ELSE.
              <call>-type = '<'.
            ENDIF.
          ENDIF.
        ENDLOOP.

      ENDLOOP.

      "clear value(var) to var.
      LOOP AT ls_source-t_params ASSIGNING <param>.
        REPLACE ALL OCCURRENCES OF 'VALUE(' IN <param>-param WITH ''.
        REPLACE ALL OCCURRENCES OF ')' IN <param>-param WITH ''.
      ENDLOOP.

      ls_source-scan = lo_scan.
      ls_source-t_keywords = lt_tokens.
      ls_source-t_calculated = lt_calculated.
      ls_source-t_composed = lt_composed.
      ls_source-tt_tabs = lt_tabs.
      APPEND ls_source TO io_debugger->mo_window->mt_source.

    ENDIF.

  ENDMETHOD.


ENDCLASS.

CLASS lcl_mermaid IMPLEMENTATION.

  METHOD constructor.

    DATA lv_text TYPE text100.

    super->constructor( ).

    mo_debugger = io_debugger.
    mv_type = iv_type.

    CHECK lcl_appl=>is_mermaid_active = abap_true.

    CASE mv_type.
      WHEN 'DIAG'.
        lv_text = 'Calls flow'.
      WHEN 'SMART'.
        lv_text = 'Calculations sequence'.
    ENDCASE.

    IF mo_box IS INITIAL.
      mo_box = create( i_name = lv_text i_width = 1000 i_hight = 300 ).
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
           END OF lty_entity.

    DATA: lv_mm_string TYPE string,
          lv_name      TYPE string,
          lt_entities  TYPE TABLE OF lty_entity,
          ls_entity    TYPE lty_entity,
          lv_ind1      TYPE i,
          lv_ind2      TYPE i,
          lt_parts     TYPE TABLE OF string,
          ls_step      LIKE LINE OF mo_debugger->mt_steps.

    DATA(lt_copy) = mo_debugger->mt_steps.
    LOOP AT lt_copy ASSIGNING FIELD-SYMBOL(<copy>).
      CLEAR <copy>-time.
    ENDLOOP.

    SORT lt_copy BY line.
    DELETE ADJACENT DUPLICATES FROM lt_copy.
    SORT lt_copy BY step.

    LOOP AT lt_copy ASSIGNING <copy>.
      IF <copy>-eventtype = 'METHOD'.
        SPLIT <copy>-program AT '=' INTO TABLE lt_parts.
        <copy>-eventname = ls_entity-name = |"{ lt_parts[ 1 ] }->{ <copy>-eventname }"|.
        ls_entity-event = <copy>-eventtype.

      ELSEIF <copy>-eventtype = 'FUNCTION'.
        <copy>-eventname = ls_entity-name = |"{ <copy>-eventtype }:{ <copy>-eventname }"|.
      ELSE.
        <copy>-eventname = ls_entity-name = |"{ <copy>-program }:{ <copy>-eventname }"|.
      ENDIF.

      COLLECT ls_entity INTO lt_entities.
    ENDLOOP.

    CLEAR ls_step.

    IF iv_direction IS INITIAL.
      lv_mm_string = |graph TD\n |.
    ELSE.
      lv_mm_string = |graph { iv_direction }\n |.
    ENDIF.

    LOOP AT lt_copy INTO DATA(ls_step2).
      IF ls_step IS INITIAL.
        ls_step = ls_step2.
        CONTINUE.
      ENDIF.
      IF ls_step2-stacklevel > ls_step-stacklevel.

        READ TABLE lt_entities WITH KEY name = ls_step-eventname TRANSPORTING NO FIELDS.
        lv_ind1 = sy-tabix.
        READ TABLE lt_entities WITH KEY name = ls_step2-eventname TRANSPORTING NO FIELDS.
        lv_ind2 = sy-tabix.
        lv_mm_string = |{ lv_mm_string }{ lv_ind1 }({ ls_step-eventname }) --> { lv_ind2 }({ ls_step2-eventname })\n|.
      ENDIF.
      ls_step = ls_step2.
    ENDLOOP.

    open_mermaid( lv_mm_string ).

  ENDMETHOD.

  METHOD magic_search.

    DATA: lv_add         TYPE xfeld,
          lv_mm_string   TYPE string,
          lv_sub         TYPE string,
          lv_form        TYPE string,
          lv_direction   TYPE string,
          lv_box_s       TYPE string,
          lv_box_e       TYPE string,
          lv_ind2        TYPE i,
          lv_start       TYPE i,
          lv_end         TYPE i,
          lv_bool        TYPE string,
          lv_block_first TYPE i,
          lv_els_before  TYPE i.

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

    DATA: ls_line       TYPE ts_line,
          lt_lines      TYPE STANDARD TABLE OF ts_line,
          ls_prev_stack TYPE ts_line,
          lv_opened     TYPE i.

    Clear mo_debugger->mo_window->mt_watch.

    LOOP AT mo_debugger->mt_steps INTO DATA(ls_step).
      READ TABLE mo_debugger->mo_window->mt_source WITH KEY include = ls_step-include INTO DATA(ls_source).
      READ TABLE ls_source-t_keywords WITH KEY line = ls_step-line INTO DATA(ls_keyword).
      LOOP AT ls_keyword-tt_calls INTO DATA(ls_call).

        READ TABLE mo_debugger->mt_selected_var WITH KEY name = ls_call-outer TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO  mo_debugger->mt_selected_var ASSIGNING FIELD-SYMBOL(<selected>).
          <selected>-name = ls_call-outer.
        ENDIF.

        READ TABLE mo_debugger->mt_selected_var WITH KEY name = ls_call-inner TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO  mo_debugger->mt_selected_var ASSIGNING <selected>.
          <selected>-name = ls_call-inner.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    DATA(lt_steps) = mo_debugger->mt_steps.

    "deleting empty cycles.
    DATA: lv_prev     LIKE LINE OF lt_steps,
          lv_prev_key TYPE string.

    READ TABLE mo_debugger->mo_window->mt_source WITH KEY include = ls_step-include INTO ls_source.

    LOOP AT lt_steps ASSIGNING FIELD-SYMBOL(<step>).
      DATA(lv_ind) = sy-tabix.
      READ TABLE ls_source-t_keywords WITH KEY line = <step>-line INTO DATA(ls_key).
      IF lv_prev IS NOT INITIAL.
        IF ls_key-name = 'ENDDO' AND lv_prev_key = 'DO'.
          <step>-first = 'D'."to delete
          READ TABLE lt_steps INDEX lv_ind - 1 ASSIGNING FIELD-SYMBOL(<step_prev>).
          <step_prev>-first = 'D'.
        ENDIF.
      ENDIF.
      lv_prev = <step>.
      lv_prev_key = ls_key-name.
    ENDLOOP.

    DELETE lt_steps WHERE first = 'D'.


    SORT lt_steps BY step DESCENDING.

    "collecting dependents variables
    LOOP AT lt_steps INTO ls_step.

      READ TABLE mo_debugger->mo_window->mt_source WITH KEY include = ls_step-include INTO ls_source.

      LOOP AT ls_source-t_calculated INTO DATA(ls_calculated) WHERE line = ls_step-line.
        READ TABLE mo_debugger->mt_selected_var WITH KEY name = ls_calculated-calculated TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO  mo_debugger->mt_selected_var ASSIGNING <selected>.
          <selected>-name = ls_calculated-calculated.
        ENDIF.
        LOOP AT ls_source-t_composed INTO DATA(ls_composed) WHERE line = ls_step-line.
          READ TABLE mo_debugger->mt_selected_var WITH KEY name = ls_composed-composing TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            APPEND INITIAL LINE TO  mo_debugger->mt_selected_var ASSIGNING <selected>.
            <selected>-name = ls_composed-composing.
          ENDIF.
        ENDLOOP.
        "adding returning values
        LOOP AT ls_source-t_params INTO DATA(lv_param).
          READ TABLE mo_debugger->mt_selected_var WITH KEY name = lv_param-param TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND INITIAL LINE TO  mo_debugger->mt_selected_var ASSIGNING <selected>.
            <selected>-name = lv_param-param.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      READ TABLE ls_source-t_keywords WITH KEY line = ls_step-line INTO ls_keyword.
      LOOP AT ls_keyword-tt_calls INTO ls_call.

        READ TABLE mo_debugger->mt_selected_var WITH KEY name = ls_call-outer TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO  mo_debugger->mt_selected_var ASSIGNING <selected>.
          <selected>-name = ls_call-inner.
        ENDIF.
      ENDLOOP.

    ENDLOOP.
    SORT mo_debugger->mt_selected_var.
    DELETE ADJACENT DUPLICATES FROM mo_debugger->mt_selected_var.

    "collecting watchpoints
    CLEAR mo_debugger->mo_window->mt_coverage.

    LOOP AT  lt_steps INTO ls_step.

      READ TABLE mo_debugger->mo_window->mt_source WITH KEY include = ls_step-include INTO ls_source.
      READ TABLE ls_source-t_keywords WITH KEY line = ls_step-line INTO ls_key.

      CLEAR ls_line-cond.
      IF ls_key-name = 'IF' OR ls_key-name = 'ELSE' OR ls_key-name = 'ENDIF' OR ls_key-name = 'ELSEIF' OR
         ls_key-name = 'CASE' OR ls_key-name = 'WHEN' OR ls_key-name = 'ENDCASE' OR
          ls_key-name = 'DO' OR ls_key-name = 'ENDDO'  OR ls_key-name = 'LOOP'  OR ls_key-name = 'ENDLOOP' OR ls_key-name = 'WHILE' OR ls_key-name = 'ENDWHILE'.
        APPEND INITIAL LINE TO mo_debugger->mo_window->mt_watch ASSIGNING FIELD-SYMBOL(<watch>).

        <watch>-program = ls_step-program.
        <watch>-line = ls_line-line = ls_step-line.

        INSERT ls_line INTO lt_lines INDEX 1 ASSIGNING FIELD-SYMBOL(<line>).
        <line>-cond = ls_key-name.
        <line>-event = ls_step-eventname.
        <line>-stack = ls_step-stacklevel.
        <line>-include = ls_step-include.
      ENDIF.

      LOOP AT  ls_source-t_calculated INTO ls_calculated WHERE line = ls_step-line.

        LOOP AT ls_source-t_composed INTO ls_composed WHERE line = ls_step-line.
          READ TABLE mo_debugger->mt_selected_var WITH KEY name = ls_composed-composing TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            APPEND INITIAL LINE TO  mo_debugger->mt_selected_var ASSIGNING <selected>.
            <selected>-name = ls_composed-composing.
          ENDIF.
        ENDLOOP.

        READ TABLE mo_debugger->mt_selected_var WITH KEY name = ls_calculated-calculated TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.

          APPEND INITIAL LINE TO mo_debugger->mo_window->mt_watch ASSIGNING <watch>.
          <watch>-program = ls_step-program.
          <watch>-line = ls_line-line = ls_step-line.

          "should be commented for Smart debugger
*          LOOP AT lt_lines ASSIGNING <line> WHERE line = ls_line-line AND event = ls_step-eventname AND stack = ls_step-stacklevel .
*            <line>-del = abap_true.
*          ENDLOOP.

          ls_line-event = ls_step-eventname.
          ls_line-stack = ls_step-stacklevel.
          ls_line-include = ls_step-include.
          INSERT ls_line INTO lt_lines INDEX 1.
        ENDIF.
       EXIT.
      ENDLOOP.

    ENDLOOP.
    

    DELETE lt_lines WHERE del = abap_true.

    "getting code texts and calls params
    LOOP AT lt_lines ASSIGNING <line>.
      lv_ind = sy-tabix.

      READ TABLE mo_debugger->mo_window->mt_source WITH KEY include = <line>-include INTO ls_source.
      READ TABLE ls_source-t_keywords WITH KEY line = <line>-line INTO ls_keyword.
      LOOP AT ls_source-scan->tokens FROM ls_keyword-from TO ls_keyword-to INTO DATA(ls_token).
        IF ls_token-str = 'USING' OR ls_token-str = 'EXPORTING' OR ls_token-str = 'IMPORTING' OR ls_token-str = 'CHANGING'.
          EXIT.
        ENDIF.
        IF <line>-code IS INITIAL.
          <line>-code = ls_token-str.
        ELSE.
          <line>-code = |{  <line>-code } { ls_token-str }|.
        ENDIF.
      ENDLOOP.

      LOOP AT ls_keyword-tt_calls INTO ls_call.
        IF sy-tabix <> 1.
          <line>-arrow = |{ <line>-arrow }, |.
        ENDIF.
        <line>-arrow  = |{ <line>-arrow  } { ls_call-outer } { ls_call-type } { ls_call-inner }|.
        <line>-subname = ls_call-name.
        REPLACE ALL OCCURRENCES OF '''' IN <line>-subname WITH ''.
      ENDLOOP.
    ENDLOOP.

    "check subform execution steps existance and if/case structures build

    DATA: if_depth   TYPE i,
          when_count TYPE i.
    LOOP AT lt_lines ASSIGNING <line> WHERE code <> 'DO' AND code <> 'ENDDO' AND code <> 'WHILE' AND code <> 'ENDWHILE' AND code <> 'LOOP' AND code <> 'ENDLOOP' .
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
        READ TABLE mt_if INDEX if_depth ASSIGNING <if>.
      ENDIF.

      IF <line>-cond = 'WHEN'.
        ADD 1 TO when_count.
      ENDIF.

      IF <line>-cond = 'ELSE' OR <line>-cond = 'ELSEIF'.

        <line>-els_before = lv_els_before.
        <line>-els_after = <line>-ind.
        DATA(lv_counter) = <line>-ind + 1.
        DO.
          READ TABLE lt_lines INDEX lv_counter INTO ls_line.
          IF sy-subrc <> 0.
            CLEAR <line>-els_after.
            EXIT.
          ENDIF.

          IF ls_line-cond = 'ELSE' OR ls_line-cond = 'ELSEIF'.
            CLEAR <line>-els_after.
            EXIT.
          ELSEIF  ls_line-cond <> 'DO' AND ls_line-cond <> 'ENDDO' AND ls_line-cond <> 'WHILE' AND ls_line-cond <> 'ENDWHILE' AND ls_line-cond <> 'LOOP' AND ls_line-cond <> 'ENDLOOP'.
            <line>-els_after = lv_counter.
            EXIT.
          ELSE.
            ADD 1 TO lv_counter.

          ENDIF.
        ENDDO.
        IF when_count = 1.
          <if>-if_ind = lv_els_before.
          CLEAR <line>-els_before.
        ENDIF.
      ENDIF.

      IF <line>-cond = 'WHEN'.

        <line>-els_before = lv_els_before.
        <line>-els_after = <line>-ind.
        lv_counter = <line>-ind + 1.
        DO.
          READ TABLE lt_lines INDEX lv_counter INTO ls_line.
          IF sy-subrc <> 0.
            CLEAR <line>-els_after.
            EXIT.
          ENDIF.

          IF ls_line-cond = 'WHEN'.
            CLEAR <line>-els_after.
            EXIT.
          ELSEIF  ls_line-cond <> 'DO' AND ls_line-cond <> 'ENDDO' AND ls_line-cond <> 'WHILE' AND ls_line-cond <> 'ENDWHILE' AND ls_line-cond <> 'LOOP' AND ls_line-cond <> 'ENDLOOP'.
            <line>-els_after = lv_counter.
            EXIT.
          ELSE.
            ADD 1 TO lv_counter.

          ENDIF.
        ENDDO.
        IF when_count = 1.
          <if>-if_ind = lv_els_before.
          CLEAR <line>-els_before.
        ENDIF.
      ENDIF.

      IF <line>-cond <> 'ELSE' AND <line>-cond <> 'ELSEIF' AND <line>-cond <> 'WHEN'.
        lv_els_before = <line>-ind.
      ELSE.
        CLEAR   lv_els_before.
      ENDIF.

      READ TABLE lt_lines WITH KEY event = <line>-subname TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CLEAR <line>-arrow.
      ENDIF.
    ENDLOOP.

    IF mt_if IS INITIAL AND ms_if-if_ind IS NOT INITIAL.
      INSERT ms_if INTO mt_if INDEX 1.
    ENDIF.

    IF lines( lt_lines ) > 0.
      IF lt_lines[ lines( lt_lines ) ]-arrow IS NOT INITIAL.
        CLEAR lt_lines[ lines( lt_lines ) ]-arrow .
      ENDIF.
    ENDIF.

    "creating mermaid code
    CHECK lt_lines IS NOT INITIAL.

    IF iv_direction IS INITIAL.
      IF lines( lt_lines ) < 100.
        lv_direction = 'LR'.
      ELSE.
        lv_direction = 'TB'.
      ENDIF.
    ELSE.
      lv_direction = iv_direction.
    ENDIF.

    lv_mm_string = |graph { lv_direction }\n |.

    LOOP AT lt_lines INTO ls_line WHERE cond <> 'ELSE' AND cond <> 'ELSEIF' AND  cond <> 'WHEN'.
      lv_ind = sy-tabix.

      IF ls_line-cond IS INITIAL.
        lv_box_s = '('.
        lv_box_e = ')'.
      ELSE.
        lv_box_s = '{'.
        lv_box_e = '}'.
      ENDIF.

      IF ls_prev_stack IS INITIAL.
        ls_prev_stack = ls_line.
      ENDIF.

      IF ( ls_prev_stack-stack > ls_line-stack OR ls_prev_stack-event <> ls_line-event ) AND lv_opened > 0 AND lv_sub IS INITIAL.
        IF ls_prev_stack-stack = ls_line-stack AND ls_prev_stack-event <> ls_line-event.
          DATA(lv_times) = 1.
        ELSE.
          lv_times = ls_prev_stack-stack - ls_line-stack.
        ENDIF.

        DO lv_times TIMES.
          lv_mm_string = |{ lv_mm_string } end\n|.
          SUBTRACT 1 FROM lv_opened.
          IF lv_opened = 0.
            EXIT.
          ENDIF.
        ENDDO.

      ENDIF.
      DATA: lv_name TYPE string.
      IF    ls_line-cond = 'LOOP' OR ls_line-cond = 'DO' OR ls_line-cond = 'WHILE' OR ls_line-arrow IS NOT INITIAL .
        IF ls_line-arrow IS NOT INITIAL.
          lv_mm_string = |{ lv_mm_string }{ lv_ind }{ lv_box_s }"{ ls_line-code }"{ lv_box_e }\n|.
          ls_prev_stack = ls_line.

        ENDIF.

        IF strlen( ls_line-code ) > 50.
          lv_name = ls_line-code+0(50).
        ELSE.
          lv_name = ls_line-code.
        ENDIF.
        REPLACE ALL OCCURRENCES OF `PERFORM` IN lv_name WITH `FORM` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF `CALL FUNCTION` IN lv_name WITH `FUNCTION` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF `CALL METHOD` IN lv_name WITH `METHOD` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF `-` IN lv_name WITH `~` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF ` ` IN lv_name WITH `&nbsp;` IN CHARACTER MODE.

        lv_mm_string = |{ lv_mm_string } subgraph S{ lv_ind }["{ lv_name }"]\n  direction { lv_direction }\n|.
        ADD 1 TO lv_opened.
        lv_start = lv_ind.
        CONTINUE.
      ENDIF.

      IF ls_line-cond = 'ENDLOOP' OR ls_line-cond = 'ENDDO' OR ls_line-cond = 'ENDWHILE'.
        SUBTRACT 1 FROM lv_opened.
        lv_mm_string = |{ lv_mm_string } end\n|.
        CONTINUE.
      ENDIF.

      lv_mm_string = |{ lv_mm_string }{ lv_ind }{ lv_box_s }"{ ls_line-code }"{ lv_box_e }\n|.
      ls_prev_stack = ls_line.

    ENDLOOP.

    DO lv_opened TIMES.
      lv_mm_string = |{ lv_mm_string } end\n|.
      SUBTRACT 1 FROM lv_opened.
    ENDDO.


    DATA: if_ind      TYPE i.
    CLEAR ls_prev_stack.
    LOOP AT lt_lines INTO ls_line WHERE cond <> 'LOOP' AND cond <> 'DO' AND cond <> 'WHILE' AND cond <> 'ENDLOOP' AND cond <> 'ENDDO' AND cond <> 'ENDWHILE'.

      IF ls_line-cond = 'IF' OR ls_line-cond = 'CASE' .
        ADD 1 TO if_ind.
        READ TABLE mt_if INDEX if_ind INTO ms_if.
      ENDIF.


      IF ls_prev_stack IS INITIAL.
        IF ls_line-cond = 'WHEN' OR ls_line-cond = 'ELSE' OR ls_line-cond = 'ELSEIF'.
          ls_prev_stack = lt_lines[ <if>-if_ind ].
        ELSE.
          ls_prev_stack = ls_line.
          CONTINUE.
        ENDIF.

      ENDIF.

      IF ls_line-cond = 'ELSE' OR ls_line-cond = 'ELSEIF' OR ls_line-cond = 'WHEN'.
        lv_bool = '|' && ls_line-code && '|'.
        IF ls_line-els_after IS NOT INITIAL.
          lv_mm_string = |{ lv_mm_string }{ ms_if-if_ind }-->{ lv_bool }{ ls_line-els_after }\n|.
          DATA(lv_diff) = ms_if-end_ind - ls_line-els_after.
          DATA(lv_last_els) = ls_line-els_after.
          IF ls_line-cond <> 'WHEN' AND ls_line-cond <> 'ELSEIF'  AND  lv_diff > 1 AND ls_line-els_after <> ms_if-end_ind.
            lv_mm_string = |{ lv_mm_string }{  ls_line-els_after }-->{ ms_if-end_ind }\n|.
          ENDIF.
        ELSE.
          lv_mm_string = |{ lv_mm_string }{ ms_if-if_ind }-->{ lv_bool }{ ms_if-end_ind }\n|.
        ENDIF.

        IF ls_line-els_before IS NOT INITIAL AND ls_line-els_before <> ms_if-if_ind.
          lv_mm_string = |{ lv_mm_string }{ ls_line-els_before }-->{ ms_if-end_ind }\n|.
        ENDIF.

        IF lt_lines[ ls_line-ind + 1 ]-cond <> 'ENDIF' AND lt_lines[ ls_line-ind + 1 ]-cond <> 'ENDCASE'.
          CLEAR ls_prev_stack.
        ENDIF.
        CONTINUE.
      ENDIF.

      IF   ls_prev_stack-cond NE 'ELSE' AND ls_prev_stack-cond NE 'ELSEIF' AND ls_prev_stack-cond NE 'WHEN' AND NOT ( lv_last_els = ls_line-ind ).

        lv_mm_string = |{ lv_mm_string }{ ls_prev_stack-ind }-->{ lv_sub }{ ls_line-ind }\n|.

        IF ls_line-arrow IS NOT INITIAL.
          lv_sub = '|"' && ls_line-arrow && '"|'.
        ELSE.
          CLEAR lv_sub.
        ENDIF.

      ENDIF.

      ls_prev_stack = ls_line.

      IF ls_line-cond = 'ENDIF' OR ls_line-cond = 'ENDCASE'.
        DELETE mt_if INDEX if_ind.
        SUBTRACT 1 FROM if_ind.
        READ TABLE mt_if INDEX if_ind INTO ms_if.
      ENDIF.

    ENDLOOP.

    open_mermaid( lv_mm_string ).

  ENDMETHOD.

  METHOD add_toolbar_buttons.

    DATA: lt_button TYPE ttb_button,
          lt_events TYPE cntl_simple_events,
          ls_events LIKE LINE OF lt_events.

    lt_button  = VALUE #(
     ( function = 'TD' icon = CONV #( icon_view_expand_vertical ) quickinfo = 'Vertical' text = '' )
     ( function = 'LR' icon = CONV #( icon_view_expand_horizontal ) quickinfo = 'Horizontal' text = '' )
     ( butn_type = 3  )
     ( function = 'TEXT' icon = CONV #( icon_wd_caption ) quickinfo = 'Mermaid Diagram text' text = '' )
                    ).

    mo_toolbar->add_button_group( lt_button ).

*   Register events
    ls_events-eventid = cl_gui_toolbar=>m_id_function_selected.
    ls_events-appl_event = space.
    APPEND ls_events TO lt_events.

    mo_toolbar->set_registered_events( events = lt_events ).
    SET HANDLER me->hnd_toolbar FOR mo_toolbar.

  ENDMETHOD.

  METHOD hnd_toolbar.


    IF fcode = 'TEXT'.
      DATA: lv_mm_string TYPE string,
            lv_ref       TYPE REF TO data.
      lv_mm_string = mo_diagram->get_source_code_string( ).
      GET REFERENCE OF lv_mm_string INTO lv_ref.
      NEW lcl_text_viewer( lv_ref ).

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
  ENDMETHOD.

ENDCLASS.
*</SCRIPT:SCRIPT_CLASS>
*</SCRIPT:PERSISTENT>
