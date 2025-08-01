*  &---------------------------------------------------------------------*
*  & Smart  Debugger (Project ARIADNA - Advanced Reverse Ingeneering Abap Debugger with New Analytycs )
*  & Multi-windows program for viewing all objects and data structures in debug
*  &---------------------------------------------------------------------*
*  & version: beta 0.9.400
*  & Git https://github.com/ysichov/SDDE
*  & RU description - https://ysychov.wordpress.com/2020/07/27/abap-simple-debugger-data-explorer/
*  & EN description - https://github.com/ysichov/SDDE/wiki

*  & Written by Yurii Sychov
*  & e-mail:   ysichov@gmail.com
*  & blog:     https://ysychov.wordpress.com/blog/
*  & LinkedIn: https://www.linkedin.com/in/ysychov/
*  &---------------------------------------------------------------------*

*  & External resources
*  & Inspired by
*  & https://habr.com/ru/articles/504908/
*  & https://github.com/larshp/ABAP-Object-Visualizer - Abap Object Visualizer
*  & https://github.com/ysichov/SDE_abapgit - Simple Data Explorer
*  & https://gist.github.com/AtomKrieg/7f4ec2e2f49b82def162e85904b7e25b - data object visualizer

CLASS lcl_data_receiver DEFINITION DEFERRED.
CLASS lcl_data_transmitter DEFINITION DEFERRED.
CLASS lcl_rtti_tree DEFINITION DEFERRED.
CLASS lcl_window DEFINITION DEFERRED.
CLASS lcl_table_viewer DEFINITION DEFERRED.

CLASS lcl_source_parcer DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: get_fs_var IMPORTING iv_program TYPE string iv_type TYPE string iv_name TYPE string
              RETURNING VALUE(rt_vars) TYPE tpda_scr_locals_it.
ENDCLASS.

CLASS lcl_source_parcer IMPLEMENTATION.
  METHOD get_fs_var. "get local field symbols variables

    DATA gr_scan TYPE REF TO cl_ci_scan.
    DATA gr_source TYPE REF TO cl_ci_source_include.
    DATA gr_statement TYPE REF TO if_ci_kzn_statement_iterator.
    DATA gr_procedure TYPE REF TO if_ci_kzn_statement_iterator.

    gr_source = cl_ci_source_include=>create( p_name = CONV #( iv_program ) ).
    gr_Scan = NEW cl_ci_scan( p_include = gr_source ).

    gr_statement = cl_cikzn_scan_iterator_factory=>get_statement_iterator( ciscan = gr_scan ).
    gr_procedure = cl_cikzn_scan_iterator_factory=>get_procedure_iterator( ciscan = gr_scan ).

    DO.
      TRY.
          gr_statement->next( ).
        CATCH cx_scan_iterator_reached_end.
          EXIT.
      ENDTRY.

      DATA(gt_kw) = gr_statement->get_keyword( ).

      IF gt_kw = iv_type .

        DATA(token) = gr_statement->get_token( offset =  2 ).
        IF token NE iv_name.
          CONTINUE.
        ENDIF.

        gr_procedure->statement_index = gr_statement->statement_index.
        gr_procedure->statement_type = gr_statement->statement_type.

        DO.
          TRY.
              gr_procedure->next( ).
            CATCH cx_scan_iterator_reached_end.
              ULINE.
              EXIT.
          ENDTRY.

          gt_kw = gr_procedure->get_keyword( ).
          IF gt_kw = 'FIELD-SYMBOLS'.
            token = gr_procedure->get_token( offset =  2 ).
            APPEND INITIAL LINE TO rt_vars ASSIGNING FIELD-SYMBOL(<ls_var>).
            <ls_var>-name = token.
          ELSE.
            WHILE 1 = 1.
              token = gr_procedure->get_token( offset =  sy-index ).
              IF strlen( token ) > 13.
                IF token+0(13) =  'FIELD-SYMBOL('.
                  SHIFT token LEFT UP TO '<' .
                  REPLACE ALL OCCURRENCES OF ')' IN token WITH ''.
                  APPEND INITIAL LINE TO rt_vars ASSIGNING <ls_var>.
                  <ls_var>-name = token.
                ENDIF.
              ENDIF.

              IF token = ''.
                EXIT.
              ENDIF.

            ENDWHILE.
          ENDIF.

        ENDDO.

      ENDIF.
    ENDDO.
  ENDMETHOD.
ENDCLASS.

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
             stackpointer TYPE tpda_stack_pointer,
             stacklevel   TYPE tpda_stack_level,
             program      TYPE tpda_program,
             include      TYPE tpda_include,
             line         TYPE tpda_sc_line,
             eventtype    TYPE tpda_event_type,
             eventname    TYPE tpda_event,
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

           END OF t_step_counter.

    CLASS-DATA: m_option_icons     TYPE TABLE OF sign_option_icon_s,
                mt_lang            TYPE TABLE OF t_lang,
                mt_obj             TYPE TABLE OF t_obj, "main object table
                m_ctrl_box_handler TYPE REF TO lcl_box_handler,
                c_dragdropalv      TYPE REF TO cl_dragdrop.

    CLASS-METHODS:
      init_icons_table,

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

    METHODS: "constructor,
      create IMPORTING i_width       TYPE i
                       i_hight       TYPE i
                       i_name        TYPE text100 OPTIONAL
             RETURNING VALUE(ro_box) TYPE REF TO cl_gui_dialogbox_container,

      on_box_close FOR EVENT close OF cl_gui_dialogbox_container IMPORTING sender,
      constructor IMPORTING i_additional_name TYPE string OPTIONAL.

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
      i_obj->set_frontend_layout( i_layout ) .
    ENDIF.
    i_obj->refresh_table_display( EXPORTING is_stable = l_stable i_soft_refresh = i_soft  ).
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

CLASS lcl_debugger_script DEFINITION INHERITING FROM  cl_tpda_script_class_super.

  PUBLIC SECTION.
    TYPES: BEGIN OF t_obj,
             name TYPE string,
             obj  TYPE string,
           END OF t_obj.

    DATA: mt_obj            TYPE TABLE OF t_obj,
          mt_compo          TYPE TABLE OF scompo,
          mt_locals         TYPE tpda_scr_locals_it,
          mt_loc_fs         TYPE tpda_scr_locals_it, "locals Field Symbols
          mt_globals        TYPE tpda_scr_globals_it,

          mt_ret_exp        TYPE tpda_scr_locals_it,
          m_counter         TYPE i,
          mt_steps          TYPE  TABLE OF lcl_appl=>t_step_counter, "source code steps
          mt_var_step       TYPE  TABLE OF lcl_appl=>var_table_h,
          mt_del_vars       TYPE STANDARD TABLE OF lcl_appl=>var_table,
          m_step            TYPE i,
          m_is_find         TYPE xfeld,
          m_stop_stack      TYPE i,
          m_debug           TYPE x,
          is_step           TYPE xfeld,

          ms_stack_prev     TYPE   lcl_appl=>t_stack,
          ms_stack          TYPE   lcl_appl=>t_stack,

          is_history        TYPE xfeld,
          m_hist_step       TYPE i,
          m_step_delta      TYPE i,
          mt_vars_hist_view TYPE STANDARD TABLE OF lcl_appl=>var_table,
          mt_vars_hist      TYPE STANDARD TABLE OF lcl_appl=>var_table,

          mt_state          TYPE STANDARD TABLE OF lcl_appl=>var_table,

          mt_classes_types  TYPE TABLE OF lcl_appl=>t_classes_types,

          mo_window         TYPE REF TO lcl_window,
          mv_f7_stop        TYPE xfeld,
          m_f6_level        TYPE i,
          m_target_stack    TYPE i,

          mo_tree_imp       TYPE REF TO lcl_rtti_tree,
          mo_tree_local     TYPE REF TO lcl_rtti_tree,
          mo_tree_exp       TYPE REF TO lcl_rtti_tree,
          mv_selected_var   TYPE string,
          m_ref_val         TYPE REF TO data,
          mv_stack_changed  TYPE xfeld,
          m_variable        TYPE REF TO data,
          mt_new_string     TYPE TABLE OF  string,
          m_quick           TYPE tpda_scr_quick_info.

    METHODS: prologue  REDEFINITION,
      init    REDEFINITION,
      script  REDEFINITION,
      end     REDEFINITION,

      run_script_new,
      run_script_hist IMPORTING iv_step TYPE i OPTIONAL
                      EXPORTING es_stop TYPE xfeld
                      ,
      show_variables CHANGING it_var TYPE lcl_appl=>t_var_table RETURNING VALUE(rv_stop) TYPE xfeld,
      save_hist IMPORTING
                  iv_name        TYPE clike
                  iv_fullname    TYPE string
                  iv_type        TYPE string
                  iv_cl_leaf     TYPE int4
                  iv_parent_name TYPE string
                  ir_up          TYPE any OPTIONAL
                  i_instance     TYPE string OPTIONAL,

      f5,
      f6,
      f7,
      f8,
      hndl_script_buttons IMPORTING iv_stack_changed TYPE xfeld,
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

      read_class_globals,
      get_form_parameters IMPORTING i_prg TYPE tpda_scr_prg_info.

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
        i_ref          TYPE xfeld OPTIONAL.

    METHODS traverse_struct
      IMPORTING io_type_descr  TYPE REF TO cl_abap_typedescr
                iv_name        TYPE clike
                iv_fullname    TYPE string OPTIONAL
                iv_type        TYPE string
                i_cl_leaf      TYPE int4
                ir_up          TYPE  REF TO data OPTIONAL
                iv_parent_name TYPE string OPTIONAL
                iv_struc_name  TYPE string OPTIONAL
                i_instance     TYPE string OPTIONAL.

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
             objname  TYPE string,
           END OF ts_table.

    TYPES tt_table TYPE STANDARD TABLE OF ts_table
          WITH NON-UNIQUE DEFAULT KEY.

    DATA: main_node_key   TYPE salv_de_node_key,
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
          m_SYST_key      TYPE salv_de_node_key,
          m_ldb_key       TYPE salv_de_node_key,
          m_icon          TYPE salv_de_tree_image,
          mt_vars         TYPE STANDARD TABLE OF lcl_appl=>var_table,
          mt_classes_leaf TYPE TABLE OF t_classes_leaf,
          m_no_refresh    TYPE xfeld,
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

    METHODS save_stack_vars IMPORTING iv_step TYPE i.

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

CLASS lcl_window DEFINITION INHERITING FROM lcl_popup.
  PUBLIC SECTION.

    TYPES: BEGIN OF ts_table,
             ref      TYPE REF TO data,
             kind(1),
             value    TYPE string,
             typename TYPE abap_abstypename,
             fullname TYPE string,
           END OF ts_table.

    TYPES tt_table TYPE STANDARD TABLE OF ts_table
          WITH NON-UNIQUE DEFAULT KEY.

    DATA: m_history              TYPE x,
          m_visualization        TYPE x,
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
          m_hist_depth           TYPE i,
          m_start_Stack          TYPE i.

    METHODS: constructor IMPORTING i_debugger TYPE REF TO lcl_debugger_script i_additional_name TYPE string OPTIONAL,
      add_toolbar_buttons,
      hnd_toolbar FOR EVENT function_selected OF cl_gui_toolbar IMPORTING fcode,
      set_program IMPORTING iv_program TYPE program.

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
    lcl_appl=>init_icons_table( ).

    mo_window = NEW lcl_window( me ).

    mo_tree_imp = NEW lcl_rtti_tree( i_header = 'Importing parameters' i_type = 'I' i_cont = mo_window->mo_importing_container
                                     i_debugger = me  ).

    mo_tree_local = NEW lcl_rtti_tree( i_header =  'Variables' i_type = 'L'
                     i_cont = mo_window->mo_locals_container
                     i_debugger = me ).

    mo_tree_exp = NEW lcl_rtti_tree( i_header = 'Exporting & Returning parameters' i_type = 'E' i_cont = mo_window->mo_exporting_container
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
          lo_elem   TYPE REF TO cl_abap_elemdescr..

    FIELD-SYMBOLS: <string>   TYPE tpda_sys_symbstring,
                   <lv_value> TYPE any.

    CALL METHOD cl_tpda_script_data_descr=>get_quick_info
      EXPORTING
        p_var_name   = i_name
      RECEIVING
        p_symb_quick = DATA(quick).

    ASSIGN quick-quickdata TO <lv_value>.
    lr_string ?= <lv_value>.
    ASSIGN lr_string->* TO <string>.

    CALL METHOD cl_abap_complexdescr=>describe_by_name
      EXPORTING
        p_name      = quick-abstypename
      RECEIVING
        p_descr_ref = DATA(lo_type).

    lo_elem ?= lo_type.

    CREATE DATA lr_struc TYPE HANDLE lo_elem.
    ASSIGN lr_struc->* TO FIELD-SYMBOL(<new_string>).

    e_string = <string>-valstring.
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

    lcl_rtti=>create_struc_handle( EXPORTING i_tname = CONV #( replace( val = quick-abstypename sub = '/TYPE=' with = '' )  ) IMPORTING e_handle = lo_new_type ).
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
                           r_obj = lr_struc ).

        WHEN cl_tpda_script_data_descr=>mt_tab.
          FIELD-SYMBOLS: <new_table> TYPE ANY TABLE.
          ASSIGN COMPONENT comp-compname OF STRUCTURE <new_deep> TO <new_table>.
          GET REFERENCE OF <new_table> INTO r_data.
          get_table( EXPORTING i_name = |{ i_name }-{ comp-compname }|
                     CHANGING c_obj = r_data ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_form_parameters.

    TYPES: BEGIN OF t_param,
             form TYPE string,
             name TYPE string,
             type TYPE char1,
           END OF t_param.

    DATA: gr_scan  TYPE REF TO cl_ci_scan,
          lt_param TYPE TABLE OF t_param,
          ls_param TYPE t_param,
          lv_par   TYPE char1,
          lv_type  TYPE char1.
    DATA(gr_source) = cl_ci_source_include=>create( p_name = CONV #( i_prg-include ) ).
    CREATE OBJECT gr_scan EXPORTING p_include = gr_source.

    LOOP AT gr_scan->tokens INTO DATA(l_token) WHERE str = 'FORM' .
      READ TABLE gr_scan->statements INTO DATA(l_statement) WITH KEY from =  sy-tabix.

      LOOP AT gr_scan->tokens FROM l_statement-from TO l_statement-to INTO l_token.
        IF sy-tabix = l_statement-from.
          CONTINUE.
        ENDIF.
        IF sy-tabix = l_statement-from + 1.
          ls_param-form = l_token-str.
          CONTINUE.
        ENDIF.
        IF l_token-str = 'USING'.
          ls_param-type = 'I'.
          CLEAR: lv_type, lv_par.
          CONTINUE.
        ELSEIF l_token-str = 'CHANGING'.
          IF ls_param-name IS NOT INITIAL.
            APPEND ls_param TO lt_param.
            CLEAR: lv_type, lv_par, ls_param-name.
          ENDIF.
          ls_param-type = 'E'.
          CLEAR: lv_type, lv_par.
          CONTINUE.
        ENDIF.
        IF lv_par = abap_true AND lv_type IS INITIAL AND l_token-str NE 'TYPE'.
          APPEND ls_param TO lt_param.
          CLEAR: lv_par, ls_param-name.
        ENDIF.

        IF lv_par IS INITIAL.
          ls_param-name = l_token-str.
          lv_par = abap_true.
          CONTINUE.
        ENDIF.
        IF lv_par = abap_true AND lv_type IS INITIAL AND l_token-str = 'TYPE'.
          lv_type = abap_true.
          CONTINUE.
        ENDIF.
        IF lv_par = abap_true AND lv_type = abap_true.
          APPEND ls_param TO lt_param.
          CLEAR: lv_type, lv_par, ls_param-name.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    IF ls_param-name IS NOT INITIAL.
      APPEND ls_param TO lt_param.
    ENDIF.

    LOOP AT lt_param INTO ls_param WHERE form = i_prg-event-eventname.
      READ TABLE mt_locals WITH KEY name = ls_param-name ASSIGNING FIELD-SYMBOL(<loc>).
      IF sy-subrc = 0.
        IF ls_param-type = 'I'.
          <loc>-parkind = '1'.
        ELSEIF ls_param-type = 'E'.
          <loc>-parkind = '2'.
        ENDIF.
      ENDIF.
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

    lo_table_descr ?= cl_tpda_script_data_descr=>factory(  i_name ).
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
                         CHANGING c_obj = r_data ).
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

          mo_tree_local->del_variable( EXPORTING iv_full_name =  i_name i_state = 'X' ).
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
          lo_table_descr ?= cl_tpda_script_data_descr=>factory( i_name ).
          table_clone = lo_table_descr->elem_clone( ).
          ASSIGN table_clone->* TO FIELD-SYMBOL(<f>).

          "check header area
          DATA td       TYPE sydes_desc.
          DESCRIBE FIELD <f> INTO td.

          READ TABLE td-names INTO DATA(l_names) INDEX 1.
          IF sy-subrc = 0.
            TRY.
                CALL METHOD cl_tpda_script_data_descr=>get_quick_info
                  EXPORTING
                    p_var_name   = |{ i_name }-{ l_names-name }|
                  RECEIVING
                    p_symb_quick = DATA(l_quick).

                lo_tabl ?= cl_abap_typedescr=>describe_by_data( <f> ).
                lo_struc ?= lo_tabl->get_table_line_type( ).
                CREATE DATA r_header TYPE HANDLE lo_struc.
                ASSIGN r_header->* TO FIELD-SYMBOL(<header>).
*                TRY.
*                    LOOP AT td-names INTO l_names.
*                      CLEAR r_elem.
*                      r_elem = create_simple_var( EXPORTING i_name = |{ i_name }-{ l_names-name }| ).
*                      IF r_elem IS NOT INITIAL.
*                        ASSIGN r_elem->* TO FIELD-SYMBOL(<elem>).
*                        ASSIGN COMPONENT l_names-name OF STRUCTURE <header> TO FIELD-SYMBOL(<to>).
*                        <to> = <elem>.
*                      ENDIF.
*                    ENDLOOP.
*                  CATCH cx_root.
*                ENDTRY.

                traverse( io_type_descr = cl_abap_typedescr=>describe_by_data_ref( r_header )
                          iv_name = l_name
                          iv_fullname = i_name
                          iv_type = iv_type
                          iv_parent_name = i_parent_name
                          i_instance = i_instance
                          i_cl_leaf = i_cl_leaf
                          ir_up = r_header ).

                l_name = l_name && '[]'.
                l_full_name = i_name && '[]'.
              CATCH cx_tpda_varname .
            ENDTRY.
          ENDIF.
          GET REFERENCE OF <f> INTO lr_struc.

          traverse( io_type_descr = cl_abap_typedescr=>describe_by_data_ref( lr_struc )
                    iv_name = l_name
                    iv_fullname = l_full_name
                    iv_type = iv_type
                    i_instance = i_instance
                    iv_parent_name = i_parent_name
                    i_cl_leaf = i_cl_leaf
                    ir_up = lr_struc ).

        ELSEIF m_quick-typid = 'l'. "data ref

          DATA: ls_info TYPE tpda_scr_quick_info.

          FIELD-SYMBOLS: <ls_symobjref> TYPE tpda_sys_symbdatref.
          ls_info = cl_tpda_script_data_descr=>get_quick_info( i_name ).
          ASSIGN ls_info-quickdata->* TO <ls_symobjref>.

          transfer_variable( EXPORTING i_name = CONV #( <ls_symobjref>-instancename )
                                       iv_type = iv_type
                                       i_shortname = i_name
                                       i_parent_name = i_parent_name
                                       i_cl_leaf = i_cl_leaf
                                       i_instance = <ls_symobjref>-instancename ).

        ELSEIF m_quick-typid = 'r'. "reference

          FIELD-SYMBOLS: <symobjref> TYPE tpda_sys_symbobjref.
          ASSIGN m_quick-quickdata->* TO <symobjref>.

          save_hist( EXPORTING
                     iv_fullname = i_name
                     iv_name = i_shortname
                     iv_parent_name = i_parent_name
                     iv_type = iv_type
                     iv_cl_leaf = i_cl_leaf
                     i_instance = <symobjref>-instancename ).

          create_reference( EXPORTING i_name = l_name
                                       i_type = iv_type
                                       i_shortname = l_name
                                       i_parent = i_parent_name
                                       i_quick = m_quick ).

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

            traverse( io_type_descr = cl_abap_typedescr=>describe_by_data_ref( lr_struc )
                      iv_name = l_name
                      iv_fullname = i_name
                      iv_type = iv_type
                      iv_parent_name = i_parent_name
                      i_instance = i_instance
                      i_cl_leaf = i_cl_leaf
                      ir_up = lr_struc ).
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
          traverse( io_type_descr = cl_abap_typedescr=>describe_by_data_ref( m_variable )
          iv_name = l_name
                    iv_type = iv_type
                    iv_fullname = i_name
                    iv_parent_name = i_parent_name
                    i_instance = i_instance
                    i_cl_leaf = i_cl_leaf
                    ir_up = m_variable ).
        ELSE.
          lr_struc = create_simple_var( i_name ).
          ASSIGN lr_struc->* TO FIELD-SYMBOL(<new_elem>).

          traverse( io_type_descr = cl_abap_typedescr=>describe_by_data_ref( lr_struc )
                    iv_name = l_name
                    iv_fullname = i_name
                    iv_type = iv_type
                    iv_parent_name = i_parent_name
                    ir_up = lr_struc
                    i_cl_leaf = i_cl_leaf
                    i_instance = i_instance ).
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

            transfer_variable( EXPORTING
                            i_name =  |{ <ls_symobjref>-instancename  }-{ <ls_attribute>-name }|
                            i_shortname = <ls_attribute>-name
                            iv_type = i_type
                            i_instance = <ls_symobjref>-instancename
                            i_cl_leaf  = <ls_attribute>-acckind
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
    run_script_new( ).
    hndl_script_buttons( mv_stack_changed ).
  ENDMETHOD.

  METHOD run_script_hist.

    DATA: lt_hist      LIKE mt_vars_hist_view,
          lv_hist_step TYPE i.
    IF m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.
    is_history = abap_true.

    IF iv_step IS NOT INITIAL.
      lv_hist_step = iv_step.
      READ TABLE mt_steps WITH KEY step = iv_step INTO DATA(ls_Steps).

    ELSE.
      IF ( mo_window->m_debug_button = 'F6BEG' OR mo_window->m_debug_button = 'F6END' ) AND m_target_stack IS INITIAL.
        READ TABLE mt_steps INTO ls_steps INDEX m_hist_step.
        m_target_stack = ls_Steps-stacklevel.
      ENDIF.

      IF mo_window->m_direction IS NOT INITIAL AND m_hist_step = 1 AND mo_window->m_debug_button IS NOT INITIAL.
        es_stop = abap_true.
      ENDIF.

      IF mo_window->m_direction IS INITIAL AND m_hist_step = m_step AND mo_window->m_debug_button IS NOT INITIAL.
        es_stop = abap_true.
      ENDIF.

      IF mo_window->m_direction IS NOT INITIAL AND m_hist_step > 1 AND mo_window->m_debug_button IS NOT INITIAL.
        SUBTRACT 1 FROM m_hist_step.
      ENDIF.

      IF mo_window->m_direction IS INITIAL AND m_hist_step < m_step AND mo_window->m_debug_button IS NOT INITIAL.
        ADD 1  TO m_hist_step.
      ENDIF.

      lv_hist_step = m_hist_step.


      READ TABLE mt_steps INTO ls_steps WITH KEY step =  m_hist_step.
      IF mo_window->m_visualization IS NOT INITIAL.
        mo_window->set_program( CONV #( ls_steps-include ) ).
        mo_window->set_program_line( ls_steps-line ).
      ENDIF.

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
      es_Stop = abap_true.
    ENDIF.

    IF mo_window->m_debug_button = 'F6' AND m_stop_stack = ls_stack-stacklevel.

      es_Stop = abap_true.
      CLEAR m_stop_stack.
    ENDIF.

    IF ( mo_window->m_debug_button = 'F6BEG' AND ls_steps-first = abap_true AND m_target_stack = ls_stack-stacklevel ) OR
       ( mo_window->m_debug_button = 'F6END' AND ls_steps-last = abap_true  AND m_target_stack = ls_stack-stacklevel ).
      CLEAR m_target_stack.
      es_Stop = abap_true.
    ENDIF.

    IF iv_step IS INITIAL.
      READ TABLE mo_window->mt_breaks WITH KEY inclnamesrc = ls_steps-include linesrc = ls_steps-line INTO DATA(ls_break).
      IF sy-subrc = 0.
        IF m_debug IS NOT INITIAL.BREAK-POINT.ENDIF.
        es_Stop = abap_true.
      ENDIF.
    ENDIF.

    mo_tree_local->m_no_refresh = 'X'.
    mo_tree_exp->m_no_refresh = 'X'.
    mo_tree_imp->m_no_refresh = 'X'.

    IF  iv_step IS NOT INITIAL.
      es_stop = abap_true.
    ENDIF.

    IF es_Stop = abap_true.

      "history state find refactoring
      DATA(lt_vars_hist) = mt_vars_hist.
      SORT lt_vars_hist BY step ASCENDING first DESCENDING.

      mo_tree_local->clear( ).
      mo_tree_exp->clear( ).
      mo_tree_imp->clear( ).

      CLEAR lt_hist.
      IF m_debug IS NOT INITIAL.BREAK-POINT.ENDIF.

      LOOP AT lt_vars_hist INTO DATA(ls_hist) WHERE step <= lv_hist_step.
        IF m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.
        IF  mo_tree_local->m_globals IS INITIAL AND ls_hist-leaf = 'GLOBAL' OR ls_hist-program <> ls_Steps-program.
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
          ELSE.
            APPEND INITIAL LINE TO lt_hist ASSIGNING <hist>.
            <hist> = ls_hist.
          ENDIF.
        ELSE.
          DELETE lt_hist WHERE name = ls_hist-name.
        ENDIF.
      ENDLOOP.

      SORT lt_hist BY name.

      LOOP AT lt_hist ASSIGNING FIELD-SYMBOL(<state>).
        CLEAR <state>-done.
      ENDLOOP.
      IF m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.
      IF lt_hist IS NOT INITIAL.
        show_variables( CHANGING it_var = lt_hist ).
      ENDIF.

    ENDIF.

    mo_tree_local->m_no_refresh = 'X'.
    mo_tree_exp->m_no_refresh = 'X'.
    mo_tree_imp->m_no_refresh = 'X'.
  ENDMETHOD.

  METHOD run_script_new.

    IF m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.

    DATA: lv_type TYPE string.
    ADD 1 TO m_counter.
    TRY.
        cl_tpda_script_abapdescr=>get_abap_src_info( IMPORTING p_prg_info  = mo_window->m_prg ).

        DATA(lt_stack) = cl_tpda_script_abapdescr=>get_abap_stack( ).

        READ TABLE mo_window->mt_stack INDEX 1 INTO ms_stack_prev.

        MOVE-CORRESPONDING lt_stack TO mo_window->mt_stack.
        READ TABLE mo_window->mt_stack INDEX 1 INTO ms_stack.

        CALL METHOD cl_tpda_script_bp_services=>get_all_bps RECEIVING p_bps_it = mo_window->mt_breaks.

        IF is_step = abap_true.
          ADD 1 TO m_step.
          m_hist_step = m_step.

          APPEND INITIAL LINE TO mt_steps ASSIGNING FIELD-SYMBOL(<step>).
          MOVE-CORRESPONDING ms_stack TO <step>.
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

          mo_tree_local->save_stack_vars( lv_step ).
          mo_tree_exp->save_stack_vars( lv_step ).
          mo_tree_imp->save_stack_vars( lv_step ).

          mo_tree_local->clear( ).
          mo_tree_exp->clear( ).
          mo_tree_imp->clear( ).
          DELETE mt_state WHERE leaf NE 'GLOBAL'. "AND leaf NE 'SYST'.

        ELSE.
          CLEAR mv_stack_changed.
          m_step_delta = 1.
        ENDIF.
      CATCH cx_tpda_src_info.
    ENDTRY.

    mo_tree_local->m_prg_info = mo_window->m_prg.

    IF mo_tree_local->m_globals IS NOT INITIAL OR mo_tree_local->m_ldb IS NOT INITIAL.
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

    IF mv_stack_changed = abap_true OR is_history = abap_true.

      IF mo_tree_local->m_locals IS NOT INITIAL.
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

        SORT mt_locals.

        mt_loc_fs = lcl_source_parcer=>get_fs_var( iv_program = CONV #( mo_window->m_prg-program )
                                                   iv_type = CONV #( mo_window->m_prg-eventtype )
                                                   iv_name = CONV #( mo_window->m_prg-eventname ) ).

        IF mo_window->m_prg-event-eventtype = 'FORM'.
          get_form_parameters( mo_window->m_prg ).
        ENDIF.
      ENDIF.

    ENDIF.

    IF mo_tree_local->m_globals IS NOT INITIAL OR
       mo_tree_local->m_ldb IS NOT INITIAL.
      CALL METHOD cl_tpda_script_data_descr=>globals RECEIVING p_globals_it = mt_globals.
      SORT mt_globals.

      LOOP AT mt_globals ASSIGNING FIELD-SYMBOL(<global>).
        READ TABLE mt_compo WITH KEY name = <global>-name TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
          <global>-parisval = 'L'.
        ENDIF.
      ENDLOOP.
    ENDIF.

    mo_tree_imp->m_prg_info = mo_window->m_prg.
    IF mo_window->m_visualization IS NOT INITIAL.
      mo_window->set_program( CONV #( mo_window->m_prg-include ) ).
      mo_window->set_program_line( mo_window->m_prg-line ).
      IF mv_stack_changed = abap_true.
        mo_window->show_stack( ).
      ENDIF.
    ENDIF.

    IF mo_tree_local->m_locals IS NOT INITIAL.
      LOOP AT mt_locals INTO DATA(ls_local).

        "CHECK NOT ls_local-name CA '[]'.

        CASE ls_local-parkind.
          WHEN 0.
            lv_type = 'LOCAL'.
          WHEN 1.
            lv_type = 'IMP'.
          WHEN OTHERS.
            lv_type = 'EXP'.
        ENDCASE.

        transfer_variable( EXPORTING i_name =  ls_local-name iv_type = lv_type ).

      ENDLOOP.

      LOOP AT mt_loc_fs INTO ls_local.

        CHECK NOT ls_local-name CA '[]'.

        transfer_variable( EXPORTING i_name =  ls_local-name iv_type = 'LOCAL' ).

      ENDLOOP.
    ENDIF.

    IF mo_tree_local->m_globals IS NOT INITIAL.
      LOOP AT mt_globals INTO DATA(ls_global)  WHERE parisval NE 'L'.
        transfer_variable( EXPORTING i_name =  ls_global-name iv_type = 'GLOBAL' ).
      ENDLOOP.
      IF sy-subrc <> 0.
        CLEAR mo_tree_local->m_globals_key.
        DELETE mo_tree_local->mt_vars WHERE leaf = 'GLOBAL'.
        DELETE mt_state WHERE leaf = 'GLOBAL'.
        mo_tree_local->clear( ).
      ENDIF.
    ENDIF.
    IF mo_tree_local->m_syst IS NOT INITIAL.

      transfer_variable( EXPORTING i_name =  'SYST' iv_type = 'SYST' ).
    ELSE.
      DELETE mo_tree_local->mt_vars WHERE leaf = 'SYST'.
      DELETE mt_state WHERE leaf = 'SYST'.
      "mo_tree_local->clear( ).
    ENDIF.


    IF mo_tree_local->m_ldb IS NOT INITIAL.
      LOOP AT mt_globals INTO ls_global WHERE parisval = 'L'.
        transfer_variable( EXPORTING i_name =  ls_global-name iv_type = 'LDB' ).
      ENDLOOP.
    ENDIF.

    IF mo_tree_local->m_class_data IS NOT INITIAL.
      read_class_globals( ).
    ENDIF.

    IF mv_stack_changed = abap_true.
      mo_tree_local->save_stack_vars( m_step ).
      mo_tree_exp->save_stack_vars( m_step ).
      mo_tree_imp->save_stack_vars( m_step ).
    ENDIF.

    LOOP AT mt_state ASSIGNING FIELD-SYMBOL(<state>).
      CLEAR <state>-done.
    ENDLOOP.

    mo_tree_local->m_no_refresh = 'X'.
    mo_tree_exp->m_no_refresh = 'X'.
    mo_tree_imp->m_no_refresh = 'X'.
    CLEAR mo_window->m_show_step.

  ENDMETHOD.

  METHOD show_variables.

    FIELD-SYMBOLS: <hist> TYPE any,
                   <new>  TYPE any.

    DATA: l_rel   TYPE salv_de_node_relation,
          lv_key  TYPE salv_de_node_key,
          lo_tree TYPE REF TO  lcl_rtti_tree,
          is_skip TYPE xfeld.

    IF mo_tree_local->m_clear = abap_true.
      mo_tree_local->clear( ).
      CLEAR mo_tree_local->m_clear.
    ENDIF.

    mo_tree_imp->m_leaf   =  'IMP'.
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
      "mo_tree_local->clear( ).
    ENDIF.

    IF mo_tree_local->m_class_key IS NOT INITIAL AND mo_tree_local->m_class_data IS INITIAL.
      mo_tree_local->delete_node( mo_tree_local->m_class_key ).
      DELETE mo_tree_local->mt_vars WHERE leaf = 'CLASS'.
      DELETE mt_state WHERE leaf = 'CLASS'.
      "mo_tree_local->clear( ).
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
    IF m_debug IS NOT INITIAL. BREAK-POINT.ENDIF.
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

      IF <var>-name = mv_selected_var.

        IF m_ref_val IS BOUND.
          ASSIGN m_ref_val->* TO <hist>.
          ASSIGN <var>-ref->* TO <new>.
          IF <new> <> <hist>.

            m_ref_val = <var>-ref.
            rv_stop = abap_true.
          ENDIF.
        ELSE.
          m_ref_val = <var>-ref.
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
        io_type_descr = cl_abap_typedescr=>describe_by_data_ref( <var>-ref )
        iv_parent_key = lv_key
        iv_rel  = l_rel
        is_var = <var>
        ir_up = <var>-ref
        iv_parent_name = CONV #( <var>-name ) ).
      ELSE.
        lo_tree->traverse_obj(
        iv_parent_key = lv_key
        iv_rel  = l_rel
        is_var = <var>
        ir_up = <var>-ref
        iv_parent_name = CONV #( <var>-name ) ).
      ENDIF.

    ENDLOOP.

    IF is_skip = abap_true.
      CLEAR is_skip.
      show_variables( CHANGING it_var = it_var ).
    ENDIF.

  ENDMETHOD.

  METHOD hndl_script_buttons.
    IF m_debug = abap_true. BREAK-POINT. ENDIF.

    IF m_is_find = abap_true.
      show_step( ).
      me->break( ).
      CLEAR m_is_find.
      RETURN.
    ENDIF.

    IF mo_window->m_debug_button = 'F5'.
      show_step( ).
      me->break( ).

    ELSEIF mo_window->m_debug_button = 'F6'.
      IF m_f6_level IS NOT INITIAL AND m_f6_level = ms_stack-stacklevel OR mo_window->m_history IS INITIAL.
        CLEAR m_f6_level.
        show_step( ).
        me->break( ).
      ELSE.
        IF mo_window->m_history IS NOT INITIAL.
          f5( ).
        ENDIF.
      ENDIF.

    ELSEIF mo_window->m_debug_button = 'F6END'.
      IF mo_window->m_prg-flag_eoev IS NOT INITIAL AND m_target_stack = ms_stack-stacklevel.
        show_step( ).
        me->break( ).
      ELSE.
        f5( ).
      ENDIF.
    ELSEIF mo_window->m_debug_button = 'F7'.

      IF m_target_stack = ms_stack-stacklevel.
        CLEAR m_target_stack.
        show_step( ).
        me->break( ).
      ELSE.
        f5( ).
      ENDIF.

    ELSEIF mo_window->m_debug_button IS NOT INITIAL.
      READ TABLE mo_window->mt_breaks WITH KEY inclnamesrc = mo_window->m_prg-include linesrc = mo_window->m_prg-line INTO DATA(gs_break).
      IF sy-subrc = 0.
        show_step( ).
        me->break( ).
      ELSE.

        IF mo_window->m_debug_button = 'F6BEG' AND m_target_stack = ms_Stack-stacklevel.
          show_step( ).
          me->break( ).
        ELSE.
          IF mo_window->m_history IS NOT INITIAL.
            IF ms_Stack-stacklevel = mo_window->m_hist_depth +  mo_window->m_start_stack.
              f6( ).
            ELSE.
              f5( ).
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      show_step( ).
      me->break( ).
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

    IF ( mo_window->m_debug_button = 'F6BEG' OR mo_window->m_debug_button = 'F6END' ) AND m_target_stack IS INITIAL.
      m_target_stack = stack-stacklevel.

    ENDIF.

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
    IF mv_f7_stop = abap_true.
      CLEAR m_counter.
      me->run_script_new( ).

      hndl_script_buttons( mv_stack_changed ).
      m_is_find = abap_true.
    ENDIF.
    IF m_counter < 1000."very deep history - to stop
      me->run_script_new( ).
      hndl_script_buttons( mv_stack_changed ).
    ELSE.
      CLEAR m_counter.
      show_step( ).
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
    me->run_script_new( ).

    hndl_script_buttons( mv_stack_changed ).
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
    me->run_script_new( ).
    hndl_script_buttons( mv_stack_changed ).
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
    me->run_script_new( ).
    hndl_script_buttons( mv_stack_changed ).
  ENDMETHOD.

  METHOD get_obj_index.

    FIND FIRST OCCURRENCE OF '*' IN iv_name MATCH OFFSET DATA(lv_offset).
    e_index =  iv_name+0(lv_offset).

  ENDMETHOD.

  METHOD show_step.
    "run_Script_hist( m_hist_step ).
    show_variables( CHANGING it_var = mt_state ).
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

        save_hist( EXPORTING
        iv_fullname = CONV #( ls_prog-name )
        iv_name = CONV #( ls_prog-name )
        iv_parent_name = ''
        iv_type = 'CLASS'
        iv_cl_leaf = 0
        i_instance = CONV #(  ls_prog-name ) ).


        LOOP AT REFc->attributes INTO DATA(ls_atr).
          transfer_variable( EXPORTING i_name =  CONV #( |{ ls_prog-name }=>{ ls_Atr-name }| )
                             i_shortname = CONV #( ls_Atr-name )
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

    READ TABLE mt_state
         WITH KEY name = lv_full_name
                  program = mo_window->mt_stack[ 1 ]-program
          ASSIGNING FIELD-SYMBOL(<state>).

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

    <state>-step = m_step - m_step_delta.
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
      <state>-type = lo_elem->absolute_name.

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
          IF mv_selected_var = <state>-name.
            m_is_find = abap_true.
          ENDIF.
        ENDIF.

      ENDIF.
    ELSE. "main node without data
      READ TABLE mt_vars_hist WITH KEY name = <state>-name INTO lv_hist.
      IF sy-subrc <> 0.
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
          traverse_struct( io_type_descr = io_type_descr
                           iv_name = iv_name
                           iv_fullname = iv_fullname
                           iv_type = iv_type
                           ir_up = ir_up
                           iv_parent_name = iv_parent_name
                           i_instance = i_instance
                           i_cl_leaf = i_cl_leaf
                           iv_struc_name = iv_struc_name ).
        ELSE.
          traverse_struct( io_type_descr = io_type_descr
                           iv_name = iv_name
                           iv_fullname = iv_fullname
                           iv_type = iv_type
                           ir_up = ir_up
                           i_instance = i_instance
                           i_cl_leaf = i_cl_leaf
                           iv_parent_name = iv_parent_name ).
        ENDIF.

      WHEN c_kind-elem.
        traverse_elem( iv_name = iv_name
                       iv_fullname = iv_fullname
                       iv_type = iv_type
                       ir_up = ir_up
                       i_instance = i_instance
                       i_cl_leaf = i_cl_leaf
                       iv_parent_name = iv_parent_name ).

      WHEN c_kind-table.

        traverse_elem( iv_name = iv_name
                       iv_fullname = iv_fullname
                       iv_type = iv_type
                       ir_up = ir_up
                       i_instance = i_instance
                       i_cl_leaf = i_cl_leaf
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
        save_hist( EXPORTING
          ir_up = ir_up
          iv_fullname = iv_fullname
          iv_name = iv_name
          iv_type = iv_type
          iv_parent_name = iv_parent_name
          iv_cl_leaf = i_cl_leaf
          i_instance = i_instance ).

      ENDIF.
    ENDIF.

    lt_component = lo_struct_descr->get_components( ).

    LOOP AT lt_component INTO ls_component.

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

        save_hist( EXPORTING
                   iv_fullname = lv_string
                   iv_name = ls_component-name
                   iv_parent_name = iv_fullname
                   iv_type = iv_type
                   iv_cl_leaf = i_cl_leaf
                   i_instance = <symobjref>-instancename ).

        create_reference( EXPORTING i_name = lv_string
                                    i_type = iv_type
                                    i_shortname = ls_component-name
                                    i_quick = l_quick ).

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
        traverse( io_type_descr = ls_component-type
          iv_name = ls_component-name
          iv_fullname = lv_string
          iv_type = iv_type
          ir_up = lr_new_struc
          iv_parent_name = lv_parent
          iv_struc_name = ls_component-name
          i_cl_leaf = i_cl_leaf
          i_instance = i_instance ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD traverse_elem.

    save_hist( EXPORTING
                ir_up = ir_up
                iv_fullname = iv_fullname
                iv_name = iv_name
                iv_parent_name = iv_parent_name
                iv_type = iv_type
                iv_cl_leaf = i_cl_leaf
                i_instance = i_instance ).

  ENDMETHOD.

ENDCLASS.                    "lcl_debugger_script IMPLEMENTATION

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
    m_history =  m_zcode = m_visualization = '01'.
    m_hist_depth = 9.

    mo_box = create( i_name = 'SDDE Simple Debugger Data Explorer beta v. 0.8' i_width = 1400 i_hight = 400 ).
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

    mo_splitter->set_row_height( id = 1  height = '3' ).
    mo_splitter->set_row_height( id = 2  height = '70' ).

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

*    CREATE OBJECT mo_splitter_steps ##FM_SUBRC_OK
*      EXPORTING
*        parent  = mo_tables_container
*        rows    = 1
*        columns = 3
*      EXCEPTIONS
*        OTHERS  = 1.

    "mo_splitter_steps->set_column_width( EXPORTING id = 1 width = '40' ).

*    mo_splitter_steps->get_container(
*            EXPORTING
*              row       = 1
*              column    = 1
*            RECEIVING
*              container = mo_stack_container ).

*    mo_splitter_steps->get_container(
*         EXPORTING
*           row       = 1
*           column    = 2
*         RECEIVING
*           container = mo_steps_container ).
*
*    mo_splitter_steps->get_container(
*         EXPORTING
*           row       = 1
*           column    = 3
*         RECEIVING
*           container = mo_hist_container ).
*
*mo_steps_container->set_visible( abap_false ).
*mo_steps_container->set_enable( abap_false ).
*
*mo_hist_container->set_visible( abap_false ).
*mo_hist_container->set_enable( abap_false ).

    CREATE OBJECT mo_splitter_code ##FM_SUBRC_OK
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

    CREATE OBJECT mo_splitter_var ##FM_SUBRC_OK
      EXPORTING
        parent  = mo_variables_container
        rows    = 2
        columns = 1
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter_var->set_row_height( id = 1  height = '66' ).

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
     ( function = 'VIS'  icon = CONV #( icon_flight ) quickinfo = 'Visualization ON' text = 'Visualization ON' )
     ( function = 'HIST' icon = CONV #( icon_graduate ) quickinfo = 'History On' text = 'History On' )
     ( function = 'DEPTH' icon = CONV #( icon_next_hierarchy_level ) quickinfo = 'History depth level' text = |Depth { m_hist_depth }| )
     ( function = 'CODE' icon = CONV #( icon_customer_warehouse ) quickinfo = 'Only Z' text = 'Only Z' )
     ( butn_type = 3  )
     ( function = 'F5' icon = CONV #( icon_debugger_step_into ) quickinfo = 'Step into' text = 'Step into' )
     ( function = 'F6' icon = CONV #( icon_debugger_step_over ) quickinfo = 'Step over' text = 'Step over' )
     ( function = 'F7' icon = CONV #( icon_debugger_step_out ) quickinfo = 'Step out' text = 'Step out' )
     ( function = 'F8' icon = CONV #( icon_debugger_continue ) quickinfo = 'to the next Breakpoint' text = 'Continue' )
     ( butn_type = 3  )
     ( function = 'F6BEG' icon = CONV #( icon_release ) quickinfo = 'Start of block' text = 'Start of block' )
     ( function = 'F6END' icon = CONV #( icon_outgoing_org_unit ) quickinfo = 'End of block' text = 'End of block' )
     ( butn_type = 3  )
     ( function = 'DIRECTION' icon = CONV #( icon_column_right ) quickinfo = 'Forward' text = 'Forward' )
     ( function = 'CLEARVAR' icon = CONV #( icon_select_detail ) quickinfo = 'Select variable to scan' text = 'Select variable to scan' )
     ( function = 'DEBUG' icon = CONV #( icon_tools ) quickinfo = 'Debug' text = 'Debug' )
     ( function = 'STEPS' icon = CONV #( icon_next_step ) quickinfo = 'Steps' text = 'Steps' )
     ( function = 'HISTORY' icon = CONV #( icon_history ) quickinfo = 'Variables history' text = 'History' )
     ( function = 'INFO' icon = CONV #( icon_information ) quickinfo = 'Documentation' text = '' )
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
    DATA gr_scan TYPE REF TO cl_ci_scan.
    DATA(gr_source) = cl_ci_source_include=>create( p_name = iv_program ).

    CREATE OBJECT gr_scan EXPORTING p_include = gr_source.
    mo_code_viewer->set_text( table = gr_source->lines  ).
  ENDMETHOD.

  METHOD set_program_line.
    TYPES: lntab TYPE STANDARD TABLE OF i.
    DATA lt_lines TYPE lntab.

    APPEND INITIAL LINE TO lt_lines ASSIGNING FIELD-SYMBOL(<line>).
    <line> = iv_line.
    mo_code_viewer->set_marker( EXPORTING marker_number = 7  marker_lines = lt_lines ).

    CLEAR lt_lines.

    LOOP AT mt_breaks INTO DATA(ls_break) WHERE inclnamesrc = m_prg-include.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <line>.
      <line> = ls_break-linesrc.
    ENDLOOP.
    mo_code_viewer->set_marker( EXPORTING marker_number = 9  marker_lines = lt_lines ).
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
    r_container = mo_tables_container
  IMPORTING
  r_salv_table = mo_salv_stack
  CHANGING
  t_table = mt_stack ).

      DATA:  lo_column  TYPE REF TO cl_salv_column.

      DATA(lo_columns) = mo_salv_stack->get_columns( ).
      lo_columns->set_optimize( 'X' ).

      lo_column ?= lo_columns->get_column( 'STACKPOINTER' ).
      lo_column->set_output_length( '5' ).

      lo_column ?= lo_columns->get_column( 'STACKLEVEL' ).
      lo_column->set_output_length( '5' ).
      mo_salv_stack->display( ).
    ELSE.
      mo_salv_stack->refresh( ).
    ENDIF.

*    IF mo_salv_steps IS INITIAL.
*
*      cl_salv_table=>factory(
*  EXPORTING
*    r_container = mo_steps_container
*  IMPORTING
*  r_salv_table = mo_salv_steps
*  CHANGING
*  t_table = mo_debugger->mt_steps ).
*
*      mo_salv_steps->display( ).
*    ELSE.
*      mo_salv_steps->refresh( ).
*    ENDIF.
*
*    IF mo_salv_hist IS INITIAL.
*
*      cl_salv_table=>factory(
*  EXPORTING
*    r_container = mo_hist_container
*  IMPORTING
*  r_salv_table = mo_salv_hist
*  CHANGING
*  t_table = mo_debugger->mt_vars_hist ).
*
*      mo_salv_hist->display( ).
*    ELSE.
*      mo_salv_hist->refresh( ).
*    ENDIF.

  ENDMETHOD.

  METHOD hnd_toolbar.
    CONSTANTS: c_mask TYPE x VALUE '01'.
    m_debug_button = fcode.
    CASE fcode.

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
          mo_toolbar->set_button_info( EXPORTING fcode =  'F5'  text = 'Step into' quickinfo = 'Step into' ).
          mo_toolbar->set_button_info( EXPORTING fcode =  'F8'  text = 'Continue' quickinfo = 'to the next Breakpoint' ).
          mo_toolbar->set_button_visible( visible = abap_true fcode = 'F6' ).
          mo_toolbar->set_button_visible( visible = abap_true fcode = 'F7' ).
        ELSE.
          mo_toolbar->set_button_info( EXPORTING fcode =  'DIRECTION' icon = CONV #( icon_column_left ) text = 'Backward' quickinfo = 'Backward' ).
          mo_toolbar->set_button_info( EXPORTING fcode =  'F5'  text = 'Step back' quickinfo = 'Step back' ).
          mo_toolbar->set_button_info( EXPORTING fcode =  'F8'  text = 'to the previous Breakpoint' ).
          mo_toolbar->set_button_visible( visible = abap_false fcode = 'F6' ).
          mo_toolbar->set_button_visible( visible = abap_false fcode = 'F7' ).

        ENDIF.

      WHEN 'DEPTH'.
        IF m_hist_depth < 9.
          ADD 1 TO m_hist_depth.
        ELSE.
          CLEAR m_hist_depth.
        ENDIF.
        mo_toolbar->set_button_info( EXPORTING fcode =  'DEPTH'  text = |Depth { m_hist_depth }|  ).

      WHEN 'HIST'.
        m_history = m_history BIT-XOR c_mask.
        IF m_history IS INITIAL.
          mo_toolbar->set_button_info( EXPORTING fcode =  'HIST' icon = CONV #( icon_red_xcircle ) text = 'History OFF' ).
        ELSE.
          mo_toolbar->set_button_info( EXPORTING fcode =  'HIST' icon = CONV #( icon_graduate ) text = 'History ON' ).
        ENDIF.

      WHEN 'CODE'.
        m_zcode = m_zcode BIT-XOR c_mask.
        IF m_zcode IS INITIAL.
          mo_toolbar->set_button_info( EXPORTING fcode =  'CODE'  text = 'Z & Standard' ).
        ELSE.
          mo_toolbar->set_button_info( EXPORTING fcode =  'CODE'  text = 'Only Z code' ).
        ENDIF.

      WHEN 'CLEARVAR'.
        CLEAR: mo_debugger->mv_selected_var,
               mo_debugger->m_ref_val.
        mo_toolbar->set_button_info( EXPORTING icon = CONV #( icon_select_detail ) fcode =  'CLEARVAR'  text = 'Select variable to scan' ).

      WHEN 'DEBUG'."activate break_points
        mo_debugger->m_debug = mo_debugger->m_debug BIT-XOR c_mask.

      WHEN 'INFO'.
        DATA(l_url) = 'https://ysychov.wordpress.com/2020/07/27/abap-simple-debugger-data-explorer/'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.

      WHEN 'STEPS'.

        lcl_appl=>open_int_table( iv_name = 'Steps'   it_tab =  mo_debugger->mt_steps io_window = mo_debugger->mo_window ).

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
              <hist>-value = ls_vars-ref->*.
            ENDIF.
          ENDIF.
        ENDLOOP.
        lcl_appl=>open_int_table( iv_name = |mt_vars_hist - History({ lines( lt_hist ) })| it_tab =  lt_hist io_window = mo_debugger->mo_window  ).

    ENDCASE.

    IF m_direction IS INITIAL AND mo_debugger->m_hist_step = mo_debugger->m_step.
      READ TABLE mt_stack INDEX 1 INTO DATA(ls_stack).
      CASE fcode.
        WHEN 'F5' OR 'F6END' OR 'F6BEG'.
          mo_debugger->f5( ).

        WHEN 'F6'.
          IF m_history IS INITIAL.
            mo_debugger->f6( ).
          ELSE.

            mo_debugger->m_f6_level = ls_stack-stacklevel.
            mo_debugger->f5( ).
          ENDIF.

        WHEN 'F7'.
          IF m_history IS INITIAL.
            mo_debugger->f7( ).
          ELSE.
            mo_debugger->f5( ).
          ENDIF.

        WHEN 'F8'.
          CLEAR m_counter.
          IF m_history IS INITIAL.
            mo_debugger->f8( ).
          ELSE.
            m_start_stack = ls_stack-stacklevel.
            IF ls_stack-stacklevel = m_start_stack + m_hist_depth.
              mo_debugger->f6( ).
            ELSE.
              mo_debugger->f5( ).
            ENDIF.
          ENDIF.

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
              mo_Debugger->mo_tree_imp->display( ).
              mo_Debugger->mo_tree_local->display( ).
              mo_Debugger->mo_tree_exp->display( ).
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
    cl_abap_typedescr=>describe_by_name( EXPORTING p_name          = i_tname
                                         RECEIVING p_descr_ref     = DATA(lo_descr)
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
    DATA: mo_viewer  TYPE REF TO lcl_table_viewer,
          mo_sel_alv TYPE REF TO cl_gui_alv_grid,
          mt_fcat    TYPE lvc_t_fcat,
          mt_sel_tab TYPE TABLE OF lcl_types=>selection_display_s,
          ms_layout  TYPE lvc_s_layo.

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
    mo_box = create( i_name = 'text' i_width = 200 i_hight = 100 ).
    CREATE OBJECT mo_splitter ##FM_SUBRC_OK
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
    lo_sel_to->set_value( i_field = m_to_field i_low = <f_field>  ).
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
      lcl_rtti=>create_table_by_name( EXPORTING i_tname = m_tabname CHANGING c_table = mr_table  ).
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

    CREATE OBJECT mo_splitter ##FM_SUBRC_OK
      EXPORTING
        parent  = mo_box
        rows    = 1
        columns = 2
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter->set_column_mode(  mode = mo_splitter->mode_absolute ).
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
      DATA(ls_f4) = VALUE  lvc_s_f4( register   = abap_true chngeafter = abap_true fieldname  = <catalog>-fieldname ).
      INSERT ls_f4 INTO TABLE lt_f4.
    ENDLOOP.

    mo_alv->register_f4_for_fields( it_f4 = lt_f4 ).
    mo_alv->set_frontend_fieldcatalog( EXPORTING it_fieldcatalog = mt_alv_catalog ).

    LOOP AT mt_alv_catalog ASSIGNING FIELD-SYMBOL(<cat>) WHERE scrtext_l IS INITIAL.
      lcl_alv_common=>translate_field(  CHANGING c_fld = <cat> ).
    ENDLOOP.

    mo_alv->set_frontend_fieldcatalog( EXPORTING  it_fieldcatalog = mt_alv_catalog ).
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
        ASSIGN COMPONENT 'STEP' OF STRUCTURE <tab> TO FIELD-SYMBOL(<step>).
        mo_window->mo_debugger->run_script_hist( <step> ).
        READ TABLE   mo_window->mo_debugger->mt_steps INTO DATA(ls_step) WITH KEY step = <step>.
        mo_window->set_program( CONV #( ls_Step-include ) ).
        mo_window->set_program_line( ls_Step-line ).

        mo_window->mo_Debugger->mo_tree_imp->display( ).
        mo_window->mo_Debugger->mo_tree_local->display( ).
        mo_window->mo_Debugger->mo_tree_exp->display( ).
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

      mo_splitter->set_column_width( EXPORTING id    = 1 width = lv_sel_width ).
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
      mo_sel->set_value( i_field = 'SPRSL' i_low = m_lang  ).
    ENDIF.

    CALL METHOD mo_alv->set_frontend_fieldcatalog EXPORTING it_fieldcatalog = it_fields[].
    lcl_alv_common=>refresh( mo_alv ).
    IF mo_sel IS BOUND.
      IF  e_ucomm = 'HIDE' OR e_ucomm = 'SHOW' OR e_ucomm = 'UPDATE' .
        mo_sel->update_sel_tab( ).
      ENDIF.
      lcl_alv_common=>refresh( mo_sel->mo_sel_alv ).
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
      mo_sel->mo_viewer->handle_user_command( 'SHOW' ).
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

    mo_viewer = io_viewer.
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
    mo_viewer->mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = mo_viewer->mt_alv_catalog ).
    LOOP AT mo_viewer->mt_alv_catalog INTO DATA(l_catalog) WHERE domname NE 'MANDT'.
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
      lcl_alv_common=>translate_field( EXPORTING i_lang = mo_viewer->m_lang CHANGING c_fld = l_catalog ).
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
          langu             = mo_viewer->m_lang
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

    IF c_sel_row-low CA  '*%+&'.
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
            CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL' ##FM_SUBRC_OK
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
          tabname           = mo_viewer->m_tabname
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
    DATA:
      l_tabfield TYPE rstabfield,
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
        l_tabfield-tablename = mo_viewer->m_tabname.
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
      READ TABLE mo_viewer->mt_alv_catalog WITH KEY fieldname = <tab>-field_label INTO DATA(l_cat).

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
    lcl_alv_common=>refresh( EXPORTING i_obj = mo_sel_alv i_layout = ms_layout  ).
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
      mo_viewer->m_visible = ''.

      lv_sel_width = 0.
      CALL METHOD mo_viewer->mo_splitter->get_column_width ##FM_SUBRC_OK
        EXPORTING
          id                = 1
        IMPORTING
          result            = mo_viewer->mo_sel_width
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3.

      CALL METHOD mo_viewer->mo_splitter->set_column_width
        EXPORTING
          id    = 1
          width = lv_sel_width.
      mo_viewer->mo_alv->set_toolbar_interactive( ).
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

    lcl_alv_common=>refresh( mo_viewer->mo_alv ).
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
      EXPORTING r_container = i_cont
      IMPORTING r_salv_tree = tree
      CHANGING t_table = tree_table ).

    DATA(lo_setting) =  tree->get_tree_settings( ).
    lo_setting->set_hierarchy_header( i_header ).
    lo_setting->set_hierarchy_size( 30 ).
    lo_setting->set_hierarchy_icon( CONV #( icon_tree ) ).

    DATA(lo_columns) = tree->get_columns( ).
    lo_columns->set_optimize( abap_true ).

    lo_columns->get_column( 'VALUE' )->set_short_text( 'Value' ).
    lo_columns->get_column( 'FULLNAME' )->set_visible( '' ).
    lo_columns->get_column( 'TYPENAME' )->set_short_text( 'Type' ).
    lo_columns->get_column( 'TYPENAME' )->set_medium_text( 'Absolute Type' ).
    lo_columns->get_column( 'OBJNAME' )->set_short_text( 'Int. obj.' ).
    lo_columns->get_column( 'OBJNAME' )->set_medium_text( 'Int. obj. name' ).

    add_buttons( i_type ).

    tree->get_nodes( )->expand_all( ).
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
           mt_classes_leaf,
           m_no_refresh.

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
          e_root_key = traverse_struct( io_type_descr = io_type_descr
                                        iv_parent_key = iv_parent_key
                                        iv_rel  = iv_rel
                                        is_var = is_var
                                        ir_up = ir_up
                                        iv_parent_name = iv_parent_name
                                        iv_struc_name = iv_struc_name ).
        ELSE.
          e_root_key = traverse_struct( io_type_descr = io_type_descr
                                     iv_parent_key = iv_parent_key
                                     iv_rel  = iv_rel
                                     is_var = is_var
                                     ir_up = ir_up
                                     iv_parent_name = iv_parent_name ).
        ENDIF.

      WHEN c_kind-table.
        e_root_key = traverse_table( io_type_descr = io_type_descr
                                     iv_parent_key = iv_parent_key
                                     iv_rel  = iv_rel
                                     is_var = is_var
                                     ir_up = ir_up
                                      iv_parent_name = iv_parent_name ).
      WHEN c_kind-elem.
        e_root_key = traverse_elem( io_type_descr = io_type_descr
                                    iv_parent_key = iv_parent_key
                                    iv_rel  = iv_rel
                                    is_var = is_var
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
    ls_tree-fullname = is_Var-name.
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

        READ TABLE mt_vars WITH KEY name = is_var-name INTO DATA(l_var).
        IF sy-subrc = 0.
          TRY.
              DATA(lo_nodes) = tree->get_nodes( ).
              DATA(l_node) =  lo_nodes->get_node( l_var-key ).

              ASSIGN l_var-ref->* TO FIELD-SYMBOL(<old_value>).

              IF is_var-type = l_var-type.
                RETURN.
              ELSE.
                l_key = l_var-key.
                l_rel = if_salv_c_node_relation=>next_sibling.
                DELETE mt_vars WHERE name = is_var-name.
              ENDIF.

            CATCH cx_root.
              DELETE mt_vars WHERE name = is_var-name.
          ENDTRY.

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

    READ TABLE mt_vars WITH KEY name = is_var-name INTO DATA(l_var).
    IF sy-subrc = 0.
      TRY.
          DATA(lo_nodes) = tree->get_nodes( ).
          DATA(l_node) =  lo_nodes->get_node( l_var-key ).

          ASSIGN l_var-ref->* TO FIELD-SYMBOL(<old_value>).

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

        CATCH cx_root.
          DELETE mt_vars WHERE name = is_var-name.
      ENDTRY.

    ENDIF.

    lo_nodes = tree->get_nodes( ).

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
    IF sy-subrc = 0.
      READ TABLE mo_debugger->mt_state WITH KEY parent = is_var-name TRANSPORTING NO FIELDS.
      IF sy-subrc NE 0.
        DATA(lo_nodes) = tree->get_nodes( ).
        DATA(l_node) =  lo_nodes->get_node( l_var-key ).

        l_key = l_var-key.
        l_rel = if_salv_c_node_relation=>next_sibling.
        DELETE mt_vars WHERE name = is_var-name.
        DELETE mt_classes_leaf WHERE name = is_var-name.
      ELSE.
        RETURN.
      ENDIF.
    ELSE.
      l_rel = iv_rel.
    ENDIF.

    lv_icon = icon_oo_object.
    lv_text = is_var-short.
    ls_tree-fullname = is_var-name.
    ls_tree-path = is_var-path.
    ls_tree-objname = is_var-instance.

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

  METHOD save_stack_vars.

    LOOP AT mo_debugger->mt_state INTO DATA(vars).

      READ TABLE mo_debugger->mt_var_step WITH KEY name = vars-name step = iv_step TRANSPORTING NO FIELDS  .
      IF sy-subrc NE 0.

        APPEND INITIAL LINE TO mo_debugger->mt_var_step ASSIGNING FIELD-SYMBOL(<step>).
        MOVE-CORRESPONDING vars TO <step>.
        <step>-step = iv_step.
      ENDIF.
    ENDLOOP.
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
      ls_tree-typename = replace( val = lo_table_descr->absolute_name sub = '\TYPE=' with = ''   ).
    ENDIF.
    lv_icon = icon_view_table.

    IF is_var-name IS NOT INITIAL.
      lv_text = ls_tree-fullname.
    ELSE.
      lv_text = ls_tree-typename.
    ENDIF.

    l_rel = iv_rel.
    ASSIGN ir_up->* TO FIELD-SYMBOL(<new_value>).
    READ TABLE mt_vars WITH KEY name = is_var-name INTO DATA(l_var).
    IF sy-subrc = 0.
      TRY.
          DATA(lo_nodes) = tree->get_nodes( ).
          DATA(l_node) =  lo_nodes->get_node( l_var-key ).

          ASSIGN l_var-ref->* TO FIELD-SYMBOL(<old_value>).
          IF is_var-type = l_var-type.
            IF <old_value> NE <new_value>.
              l_key = l_var-key.
              l_rel = if_salv_c_node_relation=>next_sibling.
              DELETE mt_vars WHERE name = is_var-name.
            ELSE.
              IF ( <new_value> IS INITIAL AND m_hide IS NOT INITIAL ).
                me->del_variable( CONV #( is_var-name )  ).
              ENDIF.
            ENDIF.
          ELSE.
            me->del_variable( CONV #( is_var-name )  ).
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
    IF mo_debugger->m_debug  IS NOT INITIAL. BREAK-POINT.  ENDIF.
    DATA(lo_nodes) = tree->get_nodes( ).
    DATA(l_node) =  lo_nodes->get_node( iv_key ).
    IF l_node IS NOT INITIAL.
      l_node->delete( ).
    ENDIF.
  ENDMETHOD.

  METHOD display.

    DATA(lo_columns) = tree->get_columns( ).
    "lo_columns->set_optimize( ).
    lo_columns->get_column( 'KIND' )->set_visible( abap_false ).
    DATA(lo_nodes) = tree->get_nodes( ).
    DATA(lt_nodes) =  lo_nodes->get_all_nodes( ).

    "expanding only first level nodes.
    DATA lt_sub TYPE salv_t_nodes.
    LOOP AT lt_nodes INTO DATA(l_node).
      READ TABLE lt_sub WITH KEY node = l_node-node TRANSPORTING NO FIELDS.
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
        mo_debugger->run_script_new( ).
        mo_debugger->hndl_script_buttons( mo_debugger->mv_stack_changed ).
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

    CLEAR mo_debugger->mo_window->m_debug_button.
    IF mo_debugger->m_hist_step = mo_debugger->m_step.
      CLEAR mo_debugger->is_history.
    ENDIF.
    IF e_salv_function NE 'TEST'.

      IF mo_debugger->is_history = abap_true.

        mo_debugger->run_script_hist( ).
      ELSE.
        mo_debugger->run_script_new( ).
        mo_debugger->hndl_script_buttons( mo_debugger->mv_stack_changed ).
      ENDIF.
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
    mo_debugger->mv_selected_var = <fullname>.
    CLEAR mo_debugger->m_ref_val.

    mo_debugger->mo_window->mo_toolbar->set_button_info( EXPORTING fcode =  'CLEARVAR' icon = CONV #( icon_select_detail ) text = |'Clear { <path> }| ).

    CASE <kind>.
      WHEN cl_abap_datadescr=>typekind_table.
        lcl_appl=>open_int_table( iv_name = <fullname> it_ref = <ref> io_window = mo_debugger->mo_window ).
      WHEN cl_abap_datadescr=>typekind_string.
        NEW lcl_text_viewer( <ref> ).
    ENDCASE.

  ENDMETHOD.

  METHOD del_variable.
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

    READ TABLE mo_Debugger->mt_state WITH KEY name = iv_full_name ASSIGNING FIELD-SYMBOL(<var>).
    IF sy-subrc = 0.

      DATA(lo_nodes) = tree->get_nodes( ).
      TRY.
          DATA(l_node) =  lo_nodes->get_node( <var>-key ).
        CATCH cx_salv_msg.
      ENDTRY.

      IF  NOT ( ( mo_debugger->mo_window->m_direction IS NOT INITIAL AND mo_debugger->m_hist_step > 1 AND mo_debugger->mo_window->m_debug_button IS NOT INITIAL ) OR
         ( mo_debugger->mo_window->m_direction IS INITIAL AND mo_debugger->m_hist_step < mo_debugger->m_step AND mo_debugger->mo_window->m_debug_button IS NOT INITIAL ) ).

        <var>-step = mo_debugger->m_step.
        APPEND <var> TO mo_debugger->mt_del_vars.

      ENDIF.

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
      lo_from_tab->mo_alv->get_selected_cells( IMPORTING et_cell = lt_sel_cells  ).
      lo_from_tab->mo_alv->get_selected_columns( IMPORTING et_index_columns = DATA(lt_sel_col)  ).

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
