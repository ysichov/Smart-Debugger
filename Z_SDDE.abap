*&---------------------------------------------------------------------*
*& Simple  Debugger Data Explorer (Project ARIADNA Part 1)
*& Multi-windows program for viewing all objects and data structures in debug
*&---------------------------------------------------------------------*
*& version: beta 0.1.67.74
*& Git https://github.com/ysichov/SDDE
*& RU description
*& EN description

*& Written by Yurii Sychov
*& e-mail:   ysichov@gmail.com
*& skype:    ysichov
*& blog:     https://ysychov.wordpress.com/blog/
*& LinkedIn: https://www.linkedin.com/in/ysychov/
*&---------------------------------------------------------------------*

*& External resources
*& https://github.com/bizhuka/eui - ALV listboxes
*& https://github.com/ysichov/SDE_abapgit - Simple Data Explorer
*& https://github.com/larshp/ABAP-Object-Visualizer - Abap Object Visualizer
*& https://gist.github.com/AtomKrieg/7f4ec2e2f49b82def162e85904b7e25b - data object visualizer

CLASS lcl_data_receiver DEFINITION DEFERRED.
CLASS lcl_data_transmitter DEFINITION DEFERRED.
CLASS lcl_rtti_tree DEFINITION DEFERRED.
CLASS lcl_table_viewer DEFINITION DEFERRED.

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

           BEGIN OF t_obj,
             name       TYPE string,
             alv_viewer TYPE REF TO lcl_table_viewer,
           END OF t_obj,

           BEGIN OF t_lang,
             spras(4),
             sptxt    TYPE sptxt,
           END OF t_lang  .

    CLASS-DATA: m_option_icons     TYPE TABLE OF sign_option_icon_s,
                mt_lang            TYPE TABLE OF t_lang,
                mt_obj             TYPE TABLE OF t_obj, "main object table
                m_ctrl_box_handler TYPE REF TO lcl_box_handler,
                c_dragdropalv      TYPE REF TO cl_dragdrop.

    CLASS-METHODS:
      init_icons_table,
      init_lang,
      suppress_run_button,
      exit.
ENDCLASS.

CLASS lcl_popup DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA m_counter TYPE i.
    DATA: mo_box            TYPE REF TO cl_gui_dialogbox_container,
          mo_splitter       TYPE REF TO cl_gui_splitter_container,
          mo_parent         TYPE REF TO cl_gui_container,
          m_additional_name TYPE string.

    METHODS: constructor IMPORTING i_tname           TYPE any OPTIONAL
                                   ir_tab            TYPE REF TO data OPTIONAL
                                   i_additional_name TYPE string OPTIONAL,
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
    l_top  = l_left =  10 + 10 * ( m_counter DIV 5 ) +  ( m_counter MOD 5 ) * 50.

    CREATE OBJECT ro_box
      EXPORTING
        width                       = i_width
        height                      = i_hight
        top                         = l_top
        left                        = l_left
        caption                     = i_name
        "style                       = 1
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
    CONSTANTS: c_white(4) TYPE x VALUE '00000001', "white background
               c_grey(4)  TYPE x VALUE '00000003', "gray background
               c_green(4) TYPE x VALUE '00000216', "green +underline
               c_blue(4)  TYPE x VALUE '00000209', " blue font +underline
               c_bold(4)  TYPE x VALUE '00000020'.

    TYPES: BEGIN OF t_tabfields.
             INCLUDE TYPE   dfies.
             TYPES: empty   TYPE xfeld,
             is_text TYPE xfeld,
           END OF t_tabfields.

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
           END OF t_obj.

    DATA mt_obj TYPE TABLE OF t_obj.

    DATA: go_tree TYPE REF TO lcl_rtti_tree.
    METHODS: prologue  REDEFINITION,
      init    REDEFINITION,
      script  REDEFINITION,
      end     REDEFINITION,
      script2,
      f5,
      f6,
      f7,
      f8,
      break2.

  PRIVATE SECTION.
    METHODS: transfer_variable IMPORTING i_name      TYPE string
                                         i_shortname TYPE string OPTIONAL
                                         i_new_node  TYPE salv_de_node_key OPTIONAL,
      create_simple_var IMPORTING i_name        TYPE string
                        RETURNING VALUE(er_var) TYPE REF TO data,
      create_simple_string IMPORTING i_name          TYPE string
                           RETURNING VALUE(e_string) TYPE string,
      create_struc         IMPORTING  i_name          TYPE string
                           RETURNING  VALUE(er_struc) TYPE REF TO data
                           EXCEPTIONS type_not_found,
      get_deep_struc       IMPORTING i_name TYPE string
                                     r_obj  TYPE REF TO data.

ENDCLASS.

CLASS lcl_rtti_tree DEFINITION FINAL INHERITING FROM lcl_popup.
  PUBLIC SECTION.

    TYPES: BEGIN OF ts_table,
             ref      TYPE REF TO data,

             kind(1),
             value    TYPE string,
             fullname TYPE string,
             typename TYPE abap_abstypename,
           END OF ts_table.

    TYPES tt_table TYPE STANDARD TABLE OF ts_table
          WITH NON-UNIQUE DEFAULT KEY.

    DATA: m_variable   TYPE REF TO data,

          m_object     TYPE REF TO object,
          m_correction TYPE i,
          m_hide       TYPE x,
          mo_debugger  TYPE REF TO lcl_debugger_script.
    METHODS constructor
      IMPORTING
        i_header TYPE clike DEFAULT 'View'.

    METHODS add_variable
      IMPORTING
        iv_root_name TYPE string
        iv_key       TYPE salv_de_node_key OPTIONAL
      CHANGING
        io_var       TYPE any  .

    METHODS clear.

    METHODS add_node
      IMPORTING
        iv_name TYPE lvc_value.

    METHODS add_obj_nodes
      IMPORTING
        iv_name          TYPE lvc_value
        iv_full          TYPE string OPTIONAL
        it_attr          TYPE tpda_script_object_attribut_it
        i_new_node       TYPE salv_de_node_key OPTIONAL
      EXPORTING
        ev_public_key    TYPE salv_de_node_key
        ev_protected_key TYPE salv_de_node_key
        ev_private_key   TYPE salv_de_node_key.

    METHODS add_obj_var
      IMPORTING
                iv_name       TYPE lvc_value
                iv_full       TYPE string OPTIONAL
                iv_value      TYPE string OPTIONAL
                iv_key        TYPE salv_de_node_key OPTIONAL
                iv_icon       TYPE  salv_de_tree_image OPTIONAL
      RETURNING VALUE(er_key) TYPE salv_de_node_key.


    METHODS display IMPORTING io_debugger TYPE REF TO lcl_debugger_script OPTIONAL.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_kind,
                 struct LIKE cl_abap_typedescr=>kind_struct VALUE cl_abap_typedescr=>kind_struct,
                 table  LIKE cl_abap_typedescr=>kind_table VALUE cl_abap_typedescr=>kind_table,
                 elem   LIKE cl_abap_typedescr=>kind_elem VALUE cl_abap_typedescr=>kind_elem,
                 class  LIKE cl_abap_typedescr=>kind_class VALUE cl_abap_typedescr=>kind_class,
                 intf   LIKE cl_abap_typedescr=>kind_intf VALUE cl_abap_typedescr=>kind_intf,
                 ref    LIKE cl_abap_typedescr=>kind_ref VALUE cl_abap_typedescr=>kind_ref,
               END OF c_kind.

    DATA tree TYPE REF TO cl_salv_tree.
    DATA tree_table TYPE tt_table.
    DATA main_node_key TYPE salv_de_node_key.

    METHODS traverse
      IMPORTING
        io_type_descr TYPE REF TO cl_abap_typedescr
        iv_parent_key TYPE salv_de_node_key
        iv_name       TYPE clike
        ir_up         TYPE REF TO data OPTIONAL.

    METHODS traverse_struct
      IMPORTING
        io_type_descr TYPE REF TO cl_abap_typedescr
        iv_parent_key TYPE salv_de_node_key
        iv_name       TYPE clike
        ir_up         TYPE REF TO data OPTIONAL.

    METHODS traverse_elem
      IMPORTING
        io_type_descr TYPE REF TO cl_abap_typedescr
        iv_parent_key TYPE salv_de_node_key
        iv_name       TYPE clike
        iv_value      TYPE any OPTIONAL
        ir_up         TYPE REF TO data OPTIONAL.

    METHODS traverse_table
      IMPORTING
        io_type_descr TYPE REF TO cl_abap_typedescr
        iv_parent_key TYPE salv_de_node_key
        iv_name       TYPE clike
        ir_up         TYPE REF TO data OPTIONAL.

    METHODS: hndl_double_click FOR EVENT double_click OF cl_salv_events_tree IMPORTING node_key,
      hndl_user_command FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function .
ENDCLASS.

CLASS lcl_debugger_script IMPLEMENTATION.
  METHOD prologue.
*** generate abap_source (source handler for ABAP)
    super->prologue( ).
  ENDMETHOD.                    "prolog

  METHOD init.
    lcl_appl=>init_lang( ).
    lcl_appl=>init_icons_table( ).
    go_tree = NEW lcl_rtti_tree( 'view' ).
  ENDMETHOD.                    "init

  METHOD create_simple_var.

    DATA: lo_descr      TYPE REF TO cl_tpda_script_data_descr,
          lr_symbsimple TYPE REF TO tpda_sys_symbsimple.

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
        p_name      = quick-abstypename
      RECEIVING
        p_descr_ref = DATA(lo_type).

    DATA: lo_elem TYPE REF TO cl_abap_elemdescr.
    lo_elem ?= lo_type.
    CREATE DATA er_var TYPE HANDLE lo_elem.
    ASSIGN er_var->* TO FIELD-SYMBOL(<new_elem>).
    <new_elem> = <simple>-valstring.
  ENDMETHOD.

  METHOD create_simple_string.
    DATA lr_string TYPE REF TO tpda_sys_symbstring.
    FIELD-SYMBOLS: <string>   TYPE tpda_sys_symbstring,
                   <lv_value> TYPE any.

    DATA: lr_struc      TYPE REF TO data,
          lo_descr      TYPE REF TO cl_tpda_script_data_descr,
          lr_symbsimple TYPE REF TO tpda_sys_symbsimple.

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

    DATA: lo_elem TYPE REF TO cl_abap_elemdescr.
    lo_elem ?= lo_type.

    CREATE DATA lr_struc TYPE HANDLE lo_elem.
    ASSIGN lr_struc->* TO FIELD-SYMBOL(<new_string>).

    e_string = <string>-valstring.
  ENDMETHOD.

  METHOD create_struc.
    DATA: lo_new_type   TYPE REF TO cl_abap_structdescr,
          lo_new_elem   TYPE REF TO cl_abap_elemdescr,
          ls_comp       TYPE abap_componentdescr,
          lt_components TYPE abap_component_tab,
          lr_symbsimple TYPE REF TO tpda_sys_symbsimple.

    DATA: lo_struc_descr TYPE REF TO cl_tpda_script_structdescr,
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

        CALL METHOD cl_abap_complexdescr=>describe_by_name
          EXPORTING
            p_name         = quick-abstypename
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

    LOOP AT comp_full INTO DATA(comp).
      ASSIGN comp-symbquick-quickdata TO <lv_value>.
      lr_symbsimple ?= <lv_value>.
      ASSIGN COMPONENT comp-compname OF STRUCTURE <new_struc> TO FIELD-SYMBOL(<new>).
      <new> = lr_symbsimple->valstring.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_deep_struc.
    DATA: lr_struc       TYPE REF TO data,
          lo_struc_descr TYPE REF TO cl_tpda_script_structdescr,
          comp_full      TYPE  tpda_scr_struct_comp_it,
          comp_it        TYPE tpda_script_struc_componentsit,
          lo_table_descr TYPE REF TO cl_tpda_script_tabledescr,
          table_clone    TYPE REF TO data,
          lr_symbsimple  TYPE REF TO tpda_sys_symbsimple,
          lr_symbstring  TYPE REF TO tpda_sys_symbstring,
          lr_symbstruc   TYPE REF TO tpda_sys_symbstruct,
          lr_tab         TYPE REF TO    tpda_sys_symbtab.

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
          TRY.
              lr_symbstruc ?= <lv_value>.
              get_deep_struc( EXPORTING i_name = |{ i_name }-{ comp-compname }|
                               r_obj = lr_struc ).
            CATCH cx_root.
          ENDTRY.

        WHEN cl_tpda_script_data_descr=>mt_tab.
          TRY.
              ASSIGN COMPONENT comp-compname OF STRUCTURE <new_deep> TO <new>.
              lo_table_descr ?= cl_tpda_script_data_descr=>factory( |{ i_name }-{ comp-compname }| ).
              table_clone = lo_table_descr->elem_clone( ).
              ASSIGN table_clone->* TO FIELD-SYMBOL(<f>).
              <new> = <f>.
            CATCH cx_root.
          ENDTRY.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD transfer_variable.
    DATA: ls_obj LIKE LINE OF mt_obj.

    DATA: lr_struc       TYPE REF TO data,
          lo_struc_descr TYPE REF TO cl_tpda_script_structdescr,
          comp_full      TYPE  tpda_scr_struct_comp_it,
          comp_it        TYPE tpda_script_struc_componentsit,
          lo_table_descr TYPE REF TO cl_tpda_script_tabledescr,
          table_clone    TYPE REF TO data,
          lr_symbsimple  TYPE REF TO tpda_sys_symbsimple,
          lr_symbstring  TYPE REF TO tpda_sys_symbstring,
          lr_symbstruc   TYPE REF TO tpda_sys_symbstruct,
          lr_tab         TYPE REF TO    tpda_sys_symbtab.

    FIELD-SYMBOLS: <lv_value> TYPE any,
                   <simple>   TYPE tpda_sys_symbsimple,
                   <string>   TYPE tpda_sys_symbstring.

    TRY.
        CALL METHOD cl_tpda_script_data_descr=>get_quick_info
          EXPORTING
            p_var_name   = i_name
          RECEIVING
            p_symb_quick = DATA(quick).
      CATCH cx_tpda_varname .
    ENDTRY.

    TRY.
        ASSIGN quick-quickdata->* TO <lv_value>.

        IF quick-typid = 'h'."internal table
          lo_table_descr ?= cl_tpda_script_data_descr=>factory( i_name ).
          table_clone = lo_table_descr->elem_clone( ).
          ASSIGN table_clone->* TO FIELD-SYMBOL(<f>).
          go_tree->add_variable( EXPORTING iv_root_name = i_name iv_key = i_new_node CHANGING io_var =  <f>  ).

        ELSEIF quick-typid = 'r'. "reference

          DATA: lv_public_key    TYPE salv_de_node_key,
                lv_protected_key TYPE salv_de_node_key,
                lv_private_key   TYPE salv_de_node_key.

          FIELD-SYMBOLS: <ls_symobjref> TYPE tpda_sys_symbobjref.
          ASSIGN quick-quickdata->* TO <ls_symobjref>.
          IF <ls_symobjref>-instancename <> '{O:initial}'.

            READ TABLE mt_obj WITH KEY name = <ls_symobjref>-instancename TRANSPORTING NO FIELDS.
            IF sy-subrc = 0.
              DATA l_name TYPE string.
              IF i_shortname IS NOT INITIAL.
                l_name = i_shortname.
              ELSE.
                l_name = i_name.
              ENDIF.
              go_tree->add_obj_var( EXPORTING iv_name = CONV #( l_name )
                                              iv_full = <ls_symobjref>-instancename
                                                iv_key = i_new_node ).
              RETURN.
            ENDIF.
            ls_obj-name = <ls_symobjref>-instancename.
            COLLECT ls_obj INTO mt_obj.

            DATA: lv_name TYPE string.
            IF i_shortname IS SUPPLIED.
              lv_name = i_shortname.
            ELSE.
              lv_name = i_name.
            ENDIF.

            TRY.
                CALL METHOD cl_tpda_script_data_descr=>get_quick_info
                  EXPORTING
                    p_var_name   = <ls_symobjref>-instancename
                  RECEIVING
                    p_symb_quick = quick.

                DATA: lo_object     TYPE REF TO cl_tpda_script_objectdescr,
                      lo_descr      TYPE REF TO cl_tpda_script_data_descr,
                      lt_attributes TYPE tpda_script_object_attribut_it.

                lo_descr = cl_tpda_script_data_descr=>factory( <ls_symobjref>-instancename ).
                ls_obj-name = <ls_symobjref>-instancename.
                COLLECT ls_obj INTO mt_obj.
                lo_object ?= lo_descr.

                lt_attributes = lo_object->attributes( ).

                go_tree->add_obj_nodes( EXPORTING iv_name = CONV #( lv_name )
                                           iv_full = <ls_symobjref>-instancename
                                           i_new_node = i_new_node
                                           it_attr = lt_attributes
                                 IMPORTING ev_public_key = lv_public_key
                                           ev_protected_key = lv_protected_key
                                           ev_private_key = lv_private_key ).


                LOOP AT lt_attributes ASSIGNING FIELD-SYMBOL(<ls_attribute>).

                  lo_descr = cl_tpda_script_data_descr=>factory( |{ <ls_symobjref>-instancename }-{ <ls_attribute>-name }| ).
                  DATA(ls_info) = cl_tpda_script_data_descr=>get_quick_info( |{ <ls_symobjref>-instancename }-{ <ls_attribute>-name }| ).

                  CASE ls_info-metatype.
                    WHEN cl_tpda_script_data_descr=>mt_simple.
                      lr_struc = create_simple_var( |{ <ls_symobjref>-instancename }-{ <ls_attribute>-name }| ).
                      ASSIGN lr_struc->* TO FIELD-SYMBOL(<new_elem>).
                      go_tree->add_variable( EXPORTING iv_root_name = <ls_attribute>-name
                         iv_key = SWITCH #( <ls_attribute>-acckind WHEN '1' THEN lv_public_key WHEN '3' THEN lv_protected_key WHEN '2' THEN lv_private_key )
                          CHANGING io_var =  <new_elem> ).
                    WHEN cl_tpda_script_data_descr=>mt_struct.
                      lr_struc = create_struc(  EXPORTING i_name = |{ <ls_symobjref>-instancename }-{ <ls_attribute>-name }| ).
                      IF lr_struc IS NOT INITIAL.
                        ASSIGN lr_struc->* TO FIELD-SYMBOL(<new_struc>).
                        go_tree->add_variable( EXPORTING iv_root_name = <ls_attribute>-name
                          iv_key = SWITCH #( <ls_attribute>-acckind WHEN '1' THEN lv_public_key WHEN '3' THEN lv_protected_key WHEN '2' THEN lv_private_key )
                            CHANGING io_var =  <new_struc> ).
                      ENDIF.
                    WHEN cl_tpda_script_data_descr=>mt_string.
                      DATA(new_string) = create_simple_string( |{ <ls_symobjref>-instancename }-{ <ls_attribute>-name }| ).
                      go_tree->add_variable( EXPORTING iv_root_name = <ls_attribute>-name
                        iv_key = SWITCH #( <ls_attribute>-acckind WHEN '1' THEN lv_public_key WHEN '3' THEN lv_protected_key WHEN '2' THEN lv_private_key )
                          CHANGING io_var =  new_string ).
                    WHEN cl_tpda_script_data_descr=>mt_tab.
                      lo_table_descr ?= cl_tpda_script_data_descr=>factory( |{ <ls_symobjref>-instancename }-{ <ls_attribute>-name }| ).
                      table_clone = lo_table_descr->elem_clone( ).
                      ASSIGN table_clone->* TO <f>.
                      go_tree->add_variable( EXPORTING iv_root_name = <ls_attribute>-name
                                             iv_key = SWITCH #( <ls_attribute>-acckind WHEN '1' THEN lv_public_key WHEN '3' THEN lv_protected_key WHEN '2' THEN lv_private_key )
                                               CHANGING io_var =  <f> ).
                    WHEN OTHERS.
*

                      READ TABLE mt_obj WITH KEY name = |{ <ls_symobjref>-instancename }-{ <ls_attribute>-name }| TRANSPORTING NO FIELDS.
                      IF sy-subrc NE 0.
                        transfer_variable( EXPORTING i_name = |{ <ls_symobjref>-instancename }-{ <ls_attribute>-name }|
                                                     i_shortname = <ls_attribute>-name
                                                     i_new_node = SWITCH #( <ls_attribute>-acckind WHEN '1' THEN lv_public_key WHEN '3' THEN lv_protected_key WHEN '2' THEN lv_private_key ) ).
                      ENDIF.
                  ENDCASE.
                ENDLOOP.
              CATCH cx_tpda_varname .
            ENDTRY.
          ELSE.
            IF i_new_node IS NOT INITIAL.
              IF i_shortname IS NOT INITIAL.
                l_name = i_shortname.
              ELSE.
                l_name = i_name.
              ENDIF.
              go_tree->add_obj_var( EXPORTING iv_name = CONV #( l_name )
                                              iv_value = <ls_symobjref>-instancename
                                                iv_key = i_new_node ).
              RETURN.
            ENDIF.
          ENDIF.

        ELSEIF quick-typid = 'v' OR quick-typid = 'u' ."deep structure or structure

          DATA deep_ref TYPE REF TO cl_abap_typedescr.
          CALL METHOD cl_abap_complexdescr=>describe_by_name
            EXPORTING
              p_name         = quick-abstypename
            RECEIVING
              p_descr_ref    = deep_ref
            EXCEPTIONS
              type_not_found = 1.

          IF sy-subrc = 0.
            DATA: lo_deep_handle TYPE REF TO cl_abap_datadescr.
            lo_deep_handle ?= deep_ref.
            CREATE DATA lr_struc TYPE HANDLE lo_deep_handle.

            get_deep_struc( EXPORTING i_name = i_name r_obj = lr_struc ).

            ASSIGN lr_struc->* TO FIELD-SYMBOL(<new_deep>).

            go_tree->add_variable( EXPORTING iv_root_name = i_name CHANGING io_var =  <new_deep> ).
          ELSE.
            go_tree->add_obj_var( EXPORTING iv_name = CONV #( i_name ) iv_value  ='error' iv_icon = CONV #( icon_status_critical ) ).
          ENDIF.
        ELSEIF quick-typid = 'g'."string
          new_string = create_simple_string( i_name ).
          go_tree->add_variable( EXPORTING iv_root_name = i_name CHANGING io_var =  new_string ).
        ELSE.
          lr_struc = create_simple_var( i_name ).
          ASSIGN lr_struc->* TO <new_elem>.
          go_tree->add_variable( EXPORTING iv_root_name = i_name CHANGING io_var =  <new_elem> ).
        ENDIF.
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD script.
    script2( ).
  ENDMETHOD.

  METHOD script2.
    go_tree->clear( ).
    CALL METHOD cl_tpda_script_data_descr=>locals RECEIVING p_locals_it = DATA(locals).

    go_tree->add_node( 'locals' ).

    LOOP AT locals INTO DATA(ls_local).
      transfer_variable( ls_local-name ).
    ENDLOOP.

    CALL METHOD cl_tpda_script_data_descr=>globals RECEIVING p_globals_it = DATA(globals).

    go_tree->add_node( 'globals' ).
    transfer_variable( 'SYST' ).
    LOOP AT globals INTO DATA(ls_global).
      transfer_variable( ls_global-name ).
    ENDLOOP.

    go_tree->add_node( 'extra data' ).

    CALL METHOD abap_source->program
      RECEIVING
        p_prg = DATA(l_program).

    go_tree->add_variable( EXPORTING iv_root_name = 'current program' CHANGING io_var =  l_program ).

    TRY.
        cl_tpda_script_abapdescr=>get_abap_src_info(
           IMPORTING
             p_prg_info  = DATA(l_prg)
             p_dynp_info = DATA(l_dyn) ).

        go_tree->add_variable( EXPORTING iv_root_name = 'code line' CHANGING io_var =  l_prg-line ).
        go_tree->add_variable( EXPORTING iv_root_name = 'event' CHANGING io_var =  l_prg-eventname ).
        go_tree->add_variable( EXPORTING iv_root_name = 'include' CHANGING io_var =  l_prg-include ).

      CATCH cx_tpda_src_info.
    ENDTRY.

    go_tree->display( me ).
    me->break( ).
  ENDMETHOD.                    "script
  METHOD end.
*** insert your code which shall be executed at the end of the scripting (before trace is saved)
*** here
  ENDMETHOD.                    "end

  METHOD f5.
    CLEAR mt_obj[].
    TRY.
        CALL METHOD debugger_controller->debug_step
          EXPORTING
            p_command = cl_tpda_script_debugger_ctrl=>debug_step_into.
      CATCH cx_tpda_scr_rtctrl_status .
      CATCH cx_tpda_scr_rtctrl .
    ENDTRY.
    me->script2( ).
  ENDMETHOD.

  METHOD f6.
    CLEAR mt_obj[].
    TRY.
        CALL METHOD debugger_controller->debug_step
          EXPORTING
            p_command = cl_tpda_script_debugger_ctrl=>debug_step_over.
      CATCH cx_tpda_scr_rtctrl_status .
      CATCH cx_tpda_scr_rtctrl .
    ENDTRY.
    me->script2( ).
  ENDMETHOD.

  METHOD f7.
    CLEAR mt_obj[].
    TRY.
        CALL METHOD debugger_controller->debug_step
          EXPORTING
            p_command = cl_tpda_script_debugger_ctrl=>debug_step_out.
      CATCH cx_tpda_scr_rtctrl_status .
      CATCH cx_tpda_scr_rtctrl .
    ENDTRY.
    me->script2( ).
  ENDMETHOD.

  METHOD f8.
    CLEAR mt_obj[].
    TRY.
        CALL METHOD debugger_controller->debug_step
          EXPORTING
            p_command = cl_tpda_script_debugger_ctrl=>debug_continue.
      CATCH cx_tpda_scr_rtctrl_status .
      CATCH cx_tpda_scr_rtctrl .
    ENDTRY.
    me->script2( ).
  ENDMETHOD.

  METHOD break2.
    me->break( ).
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

CLASS lcl_sel_opt DEFINITION DEFERRED.

CLASS lcl_rtti IMPLEMENTATION.
  METHOD create_struc_handle.
    DATA: ls_comp       TYPE abap_componentdescr,
          lt_components TYPE abap_component_tab,
          lt_field_info TYPE TABLE OF dfies.

    lcl_ddic=>get_text_table( EXPORTING i_tname = i_tname IMPORTING e_tab = DATA(l_texttab) ).

    cl_abap_typedescr=>describe_by_name( EXPORTING p_name          = i_tname
                                      RECEIVING p_descr_ref     = DATA(lo_descr)
                                      EXCEPTIONS type_not_found = 1 ).
    IF sy-subrc = 0.
      e_handle ?= lo_descr.
    ELSE.
      RETURN.
    ENDIF.

    IF l_texttab IS NOT INITIAL.
      DATA(lo_texttab)  = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( l_texttab ) ).
      LOOP AT e_handle->components INTO DATA(l_descr).
        ls_comp-name = l_descr-name.
        ls_comp-type ?= e_handle->get_component_type( ls_comp-name ).
        APPEND ls_comp TO lt_components.
      ENDLOOP.

      LOOP AT lo_texttab->components INTO l_descr.

        CALL FUNCTION 'DDIF_FIELDINFO_GET'
          EXPORTING
            tabname        = l_texttab
            fieldname      = l_descr-name
            langu          = sy-langu
          TABLES
            dfies_tab      = lt_field_info
          EXCEPTIONS
            not_found      = 1
            internal_error = 2
            OTHERS         = 3.

        CHECK sy-subrc = 0.
        IF lt_field_info[ 1 ]-keyflag = abap_false.
          ls_comp-name =  l_texttab && '_' && l_descr-name.
          ls_comp-type ?= lo_texttab->get_component_type( l_descr-name ).
          APPEND: ls_comp TO lt_components,
                  ls_comp TO e_t_comp.

        ENDIF.
      ENDLOOP.
      e_handle  = cl_abap_structdescr=>create( lt_components ).
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
          m_texttabname      TYPE tabname,
          mo_alv             TYPE REF TO cl_gui_alv_grid,
          mo_sel             TYPE REF TO lcl_sel_opt,
          mr_table           TYPE REF TO data,
          mr_text_table      TYPE REF TO data,
          mo_sel_parent      TYPE REF TO cl_gui_container,
          mo_alv_parent      TYPE REF TO cl_gui_container,
          mt_alv_catalog     TYPE lvc_t_fcat,
          mt_text_components TYPE abap_component_tab,
          mt_fields          TYPE TABLE OF t_elem,
          mo_column_emitters TYPE TABLE OF t_column_emitter,
          mo_sel_width       TYPE i,
          m_visible,
          m_std_tbar         TYPE x,
          m_show_empty       TYPE i.

    METHODS:
      constructor IMPORTING i_tname           TYPE any OPTIONAL
                            i_additional_name TYPE string OPTIONAL
                            ir_tab            TYPE REF TO data OPTIONAL,
      get_where RETURNING VALUE(c_where) TYPE string,
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
      on_menu_request FOR EVENT context_menu_request OF cl_gui_alv_grid IMPORTING e_object,
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
       container = mo_parent ).

    SET HANDLER on_box_close FOR mo_box.

    CREATE OBJECT mo_text
      EXPORTING
        parent                 = mo_parent
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

    DATA ct_data TYPE  tline.
    FIELD-SYMBOLS <str> TYPE string.
    ASSIGN ir_str->* TO <str>.
    DATA lt_string TYPE TABLE OF char255.
    SPLIT <str> AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_string.
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
    LOOP AT lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
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
      DELETE lcl_appl=>mt_obj INDEX lv_tabix.
    ENDIF.
  ENDMETHOD.                    "ON_BOX_CLOSE
ENDCLASS.               "lcl_box_handler

CLASS lcl_table_viewer IMPLEMENTATION.
  METHOD constructor.
    super->constructor( i_additional_name = i_additional_name ).
    m_lang = sy-langu.
    mo_sel_width = 0.
    m_tabname = i_tname.
    create_popup( ).

    IF ir_tab IS NOT BOUND.
      lcl_rtti=>create_table_by_name( EXPORTING i_tname = m_tabname CHANGING c_table = mr_table  ).
    ELSE.
      mr_table = ir_tab.
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
                  on_menu_request
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
    DATA: lt_field_info TYPE TABLE OF dfies.
    DATA: l_dd04 TYPE dd04v.

    READ TABLE mt_fields INTO DATA(l_field) WITH KEY field = c_fld-fieldname.

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
      MOVE-CORRESPONDING l_dd04 TO c_fld.
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

  METHOD on_menu_request.
    DATA: l_smenu TYPE REF TO cl_ctmenu.
  ENDMETHOD.

  METHOD handle_tab_toolbar.
    IF m_visible IS INITIAL.
      DATA(lt_toolbar) = VALUE ttb_button(
       ( function = 'SEL_ON' icon = icon_arrow_left quickinfo = 'Show Select-Options'  butn_type = 0 )
       ( butn_type = 3 ) ).
    ENDIF.
    APPEND VALUE #( function = 'TECH' icon = icon_wd_caption quickinfo = 'Tech names'  butn_type = 0 ) TO lt_toolbar.

    LOOP AT lcl_appl=>mt_lang INTO DATA(lang).
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
    DATA: lr_struc       TYPE REF TO data,
          lr_field       TYPE REF TO data,
          lr_table_descr TYPE REF TO cl_abap_structdescr,
          lr_data_descr  TYPE REF TO cl_abap_datadescr,
          it_tabdescr    TYPE abap_compdescr_tab,
          l_replace      TYPE string,
          l_texttab      TYPE tabname,
          lr_temp        TYPE REF TO data,
          lo_str         TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <tab>   TYPE STANDARD TABLE,
                   <struc> TYPE any,
                   <field> TYPE any.
    ASSIGN mr_table->* TO <tab>.
    CREATE DATA lr_temp LIKE LINE OF <tab>.

    "read table <tab> index 1 ASSIGNING <struc>.
    "get REFERENCE OF <struc> into lr_temp.
    ASSIGN lr_temp->* TO <struc>.

    TRY.
        lr_table_descr ?= cl_abap_typedescr=>describe_by_data_ref( lr_temp ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    it_tabdescr[] = lr_table_descr->components[].
    lcl_ddic=>get_text_table( EXPORTING i_tname = i_tname IMPORTING e_tab = l_texttab ).
    l_replace = l_texttab && '_'.

    LOOP AT it_tabdescr INTO DATA(ls) WHERE type_kind NE 'h'." WHERE name NE 'MANDT' AND name NE 'CLIENT'.
      DATA(l_ind) = sy-tabix.

      ASSIGN COMPONENT ls-name OF STRUCTURE <struc> TO <field>.
      GET REFERENCE OF <field> INTO lr_field.
      lr_data_descr ?= cl_abap_typedescr=>describe_by_data_ref( lr_field ).
      DATA: l_name TYPE string.
      l_name = lr_data_descr->absolute_name.
      REPLACE ALL OCCURRENCES OF '\TYPE=' IN l_name WITH ''.

      APPEND VALUE #( field = ls-name elem = l_name ) TO mt_fields.

      DATA: l_dd04 TYPE dd04v.
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
    FIELD-SYMBOLS: <f_tab>  TYPE STANDARD TABLE.
    CHECK es_row_no-row_id IS NOT INITIAL.
    ASSIGN mr_table->* TO  <f_tab>.
    READ TABLE <f_tab> INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).
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
                  <sel>-name = <fields>-scrtext_l.
                  IF <sel>-name IS INITIAL.
                    <sel>-name = <fields>-reptext.
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

  METHOD get_where."dynamic where clause
    DATA: lt_where TYPE rsds_twhere,
          lt_range TYPE rsds_trange.

    IF  mo_sel IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_range ASSIGNING FIELD-SYMBOL(<tabl>).
      <tabl>-tablename = m_tabname.
      LOOP AT mo_sel->mt_sel_tab INTO DATA(ls_tab) WHERE range IS NOT INITIAL.
        APPEND INITIAL LINE TO <tabl>-frange_t ASSIGNING FIELD-SYMBOL(<t_range>).
        IF sy-subrc = 0.
          <t_range>-fieldname = ls_tab-field_label.
          <t_range>-selopt_t  = ls_tab-range.
        ENDIF.
      ENDLOOP.

      CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
        EXPORTING
          field_ranges  = lt_range
        IMPORTING
          where_clauses = lt_where.

      LOOP AT lt_where INTO DATA(ls_where) WHERE tablename = m_tabname.
        LOOP AT ls_where-where_tab INTO DATA(l_where).
          CONDENSE l_where-line.
          c_where = |{ c_where } { l_where-line }|.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

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
    lcl_alv_common=>refresh( mo_sel_alv ).
    RAISE EVENT selection_done.
    DATA: ls_row TYPE lcl_types=>t_sel_row.
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
      IF c_sel_row-int_type = 'D'.
        DO 2 TIMES.
          ASSIGN COMPONENT  COND string( WHEN sy-index = 1 THEN 'LOW' ELSE 'HIGH'  ) OF STRUCTURE <range> TO FIELD-SYMBOL(<field>).
          IF <field> IS INITIAL.
            CONTINUE.
          ENDIF.

          CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL' ##FM_SUBRC_OK
            EXPORTING
              date_external            = <field>
            IMPORTING
              date_internal            = <field>
            EXCEPTIONS
              date_external_is_invalid = 1
              OTHERS                   = 2.
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
    DATA: l_start TYPE i.
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
          DATA: lv_time TYPE sy-uzeit.
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

  METHOD init_lang.
    SELECT c~spras t~sptxt INTO CORRESPONDING FIELDS OF TABLE mt_lang
      FROM t002c AS c
      INNER JOIN t002t AS t
      ON c~spras = t~sprsl
      WHERE t~spras = sy-langu
      ORDER BY c~ladatum DESCENDING c~lauzeit DESCENDING.
  ENDMETHOD.

  METHOD suppress_run_button.
    DATA itab TYPE TABLE OF sy-ucomm.

    itab = VALUE #( ( 'ONLI' ) ( 'WB_EXEC' ) ).
    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = sy-pfkey
      TABLES
        p_exclude = itab.
  ENDMETHOD.

  METHOD exit.
    DATA: l_answer.
    DESCRIBE TABLE lcl_appl=>mt_obj.
    IF sy-tfill NE 0.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar       = 'Exit'
          text_question  = 'Do you want to exit?'
        IMPORTING
          answer         = l_answer
        EXCEPTIONS
          text_not_found = 1
          OTHERS         = 2.
      IF l_answer = '1'.
        LEAVE PROGRAM.
      ELSE.
        CALL SCREEN 101.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_rtti_tree IMPLEMENTATION.
  METHOD constructor.

    super->constructor( ).
    m_hide = '01'.
    mo_box = create( i_name = 'text' i_width = 800 i_hight = 300 ).
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
       container = mo_parent ).

    SET HANDLER on_box_close FOR mo_box.

    cl_salv_tree=>factory(
         EXPORTING r_container = mo_parent
         IMPORTING r_salv_tree = tree
         CHANGING t_table = tree_table ).

    DATA(lo_setting) =  tree->get_tree_settings( ).
    lo_setting->set_hierarchy_header( i_header ).
    lo_setting->set_hierarchy_size( 80 ).
    lo_setting->set_hierarchy_icon( CONV #( icon_tree ) ).

    DATA(lo_columns) = tree->get_columns( ).
    lo_columns->set_optimize( abap_true ).
    lo_columns->set_optimize( abap_false ).
    lo_columns->get_column( 'VALUE' )->set_short_text( 'Value' ).
    lo_columns->get_column( 'VALUE' )->set_output_length( 40 ).
    lo_columns->get_column( 'FULLNAME' )->set_short_text( 'Full Name' ).
    lo_columns->get_column( 'FULLNAME' )->set_output_length( 40 ).

    lo_columns->get_column( 'TYPENAME' )->set_short_text( 'Type' ).
    "lo_columns->get_column( 'FULLNAME' )->set_visible( '' ).

    DATA(lo_functions) = tree->get_functions( ).
    lo_functions->set_all( ).

    lo_functions->add_function(
       name     = 'INITIALS'
       icon     = CONV #( icon_start_viewer )
       text     = 'Initials'
       tooltip  = 'Show/hide initial values'
       position = if_salv_c_function_position=>right_of_salv_functions ).

    lo_functions->add_function(
       name     = 'F5'
       icon     = CONV #( icon_debugger_step_into )
       text     = ''
       tooltip  = 'Step into'
       position = if_salv_c_function_position=>left_of_salv_functions ).

    lo_functions->add_function(
       name     = 'F6'
       icon     = CONV #( icon_debugger_step_over )
       text     = ''
       tooltip  = 'Step over'
       position = if_salv_c_function_position=>left_of_salv_functions ).

    lo_functions->add_function(
       name     = 'F7'
       icon     = CONV #( icon_debugger_step_out )
       text     = ''
       tooltip  = 'Step out'
       position = if_salv_c_function_position=>left_of_salv_functions ).

    lo_functions->add_function(
       name     = 'F8'
       icon     = CONV #( icon_debugger_continue )
       text     = ''
       tooltip  = 'Continue'
       position = if_salv_c_function_position=>left_of_salv_functions ).

    tree->get_nodes( )->expand_all( ).
    DATA(lo_event) = tree->get_event( ) .
    SET HANDLER hndl_double_click
                hndl_user_command FOR lo_event.

  ENDMETHOD.

  METHOD clear.
    m_correction = m_correction + lines( tree_table ).
    tree->get_nodes( )->delete_all( ).
  ENDMETHOD.

  METHOD add_node.
    main_node_key =
          tree->get_nodes( )->add_node(
            related_node   = ''
            relationship   = if_salv_c_node_relation=>last_child
            row_style = if_salv_c_tree_style=>emphasized_b
            text           = iv_name
            folder         = abap_true
          )->get_key( ).
  ENDMETHOD.

  METHOD add_obj_nodes.

    DATA l_new_node TYPE salv_de_node_key.
    DATA lv_text TYPE lvc_value.
    DATA lv_node_key TYPE salv_de_node_key.
    DATA lv_icon TYPE salv_de_tree_image.
    DATA         ls_tree TYPE ts_table.

    CLEAR: ev_public_key,
           ev_protected_key,
           ev_private_key.

    lv_icon = icon_oo_object.
    lv_text = iv_name.

    IF i_new_node IS SUPPLIED AND i_new_node IS NOT INITIAL.
      l_new_node = i_new_node.
    ELSE.
      l_new_node =  main_node_key.
    ENDIF.

    IF iv_full IS SUPPLIED.
      ls_tree-fullname = iv_full.
    ENDIF.

    lv_node_key = tree->get_nodes( )->add_node(
      related_node   = l_new_node
      relationship   = if_salv_c_node_relation=>last_child
      collapsed_icon = lv_icon
      expanded_icon  = lv_icon
      data_row       = ls_tree
      text           = lv_text
      folder         = abap_false )->get_key( ).

    lv_icon = icon_led_green.

    READ TABLE it_attr WITH KEY acckind = '1' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ev_public_key =
        tree->get_nodes( )->add_node(
          related_node   = lv_node_key
          relationship   = if_salv_c_node_relation=>last_child
          collapsed_icon = lv_icon
          expanded_icon  = lv_icon
          text           = 'Public'
          folder         = abap_true
        )->get_key( ).
    ENDIF.

    READ TABLE it_attr WITH KEY acckind = '3' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lv_icon = icon_led_yellow.
      ev_protected_key =
        tree->get_nodes( )->add_node(
          related_node   = lv_node_key
          relationship   = if_salv_c_node_relation=>last_child
          collapsed_icon = lv_icon
          expanded_icon  = lv_icon
          text           = 'Protected'
          folder         = abap_true
        )->get_key( ).
    ENDIF.

    READ TABLE it_attr WITH KEY acckind = '2' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lv_icon = icon_led_red.
      ev_private_key =
        tree->get_nodes( )->add_node(
          related_node   = lv_node_key
          relationship   = if_salv_c_node_relation=>last_child
          collapsed_icon = lv_icon
          expanded_icon  = lv_icon
          text           = 'Private'
          folder         = abap_true
        )->get_key( ).
    ENDIF.
  ENDMETHOD.

  METHOD add_obj_var.

    DATA: lv_icon TYPE salv_de_tree_image,
          ls_tree TYPE ts_table,
          l_key   TYPE salv_de_node_key.

    IF iv_icon IS NOT SUPPLIED.
      lv_icon = icon_oo_class.
    ELSE.
      lv_icon = iv_icon.
    ENDIF.

    IF iv_full IS SUPPLIED.
      ls_tree-fullname = iv_full.
    ENDIF.

    IF iv_value IS SUPPLIED.
      ls_tree-value = iv_value.
    ENDIF.

    IF iv_key IS SUPPLIED.
      l_key = iv_key.
    ELSE.
      l_key = main_node_key.
    ENDIF.

    IF l_key IS INITIAL.
      l_key = main_node_key.
    ENDIF.
    IF iv_name IS INITIAL.
      BREAK-POINT.
    ENDIF.

    er_key = tree->get_nodes( )->add_node(
      related_node   = l_key
      relationship   = if_salv_c_node_relation=>last_child
      collapsed_icon = lv_icon
      expanded_icon  = lv_icon
      data_row       = ls_tree
      text           = iv_name
      folder         = abap_false
    )->get_key( ).
  ENDMETHOD.

  METHOD display.
    mo_debugger = io_debugger.
    DATA(lo_columns) = tree->get_columns( ).
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
    IF mo_debugger IS NOT INITIAL.
      mo_debugger->break2( ).
    ENDIF.
  ENDMETHOD.

  METHOD hndl_user_command.
    FIELD-SYMBOLS: <row>   LIKE LINE OF tree_table,
                   <value> TYPE any.
    CONSTANTS: c_mask TYPE x VALUE '01'.
    CASE e_salv_function.
      WHEN 'INITIALS'."Show/hide empty variables
        m_hide = m_hide BIT-XOR c_mask.
      WHEN 'F5'.
        mo_debugger->f5( ).
      WHEN 'F6'.
        mo_debugger->f6( ).
      WHEN 'F7'.
        mo_debugger->f7( ).
      WHEN 'F8'.
        mo_debugger->f8( ).
    ENDCASE.
  ENDMETHOD.

  METHOD hndl_double_click.
    DATA(ls_hier) = tree_table[ node_key - m_correction ].

    CASE ls_hier-kind.
      WHEN cl_abap_datadescr=>typekind_table.
        APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
        <obj>-alv_viewer = NEW #(  i_additional_name = CONV #( ls_hier-fullname ) ir_tab = ls_hier-ref  ).
        <obj>-alv_viewer->mo_sel->raise_selection_done( ).
      WHEN cl_abap_datadescr=>typekind_string.
        DATA(lo_text) = NEW lcl_text_viewer( ls_hier-ref ).
    ENDCASE.
  ENDMETHOD.

  METHOD add_variable.
    DATA: lr_new   TYPE REF TO data,
          lr_struc TYPE REF TO data,
          l_name   TYPE string,
          l_key    TYPE salv_de_node_key.
    FIELD-SYMBOLS: <new>      TYPE any,
                   <tab_from> TYPE ANY TABLE,
                   <tab_to>   TYPE STANDARD TABLE.
    l_name = iv_root_name.
    DESCRIBE FIELD io_var TYPE DATA(lv_type) COMPONENTS DATA(lv_components).
    IF lv_type NE cl_abap_typedescr=>typekind_table.
      CREATE DATA lr_new LIKE io_var.
      ASSIGN lr_new->*  TO <new>.
      <new> = io_var.
      IF <new> IS INITIAL AND m_hide IS NOT INITIAL.
        RETURN.
      ENDIF.
    ELSE.
      ASSIGN io_var TO <tab_from>.
      CREATE DATA lr_struc LIKE LINE OF <tab_from>.
      ASSIGN lr_struc->* TO FIELD-SYMBOL(<ls_record>).
      CREATE DATA lr_new LIKE STANDARD TABLE OF <ls_record>.
      ASSIGN lr_new->* TO <tab_to>.
      <tab_to> = <tab_from>.
      IF <tab_to> IS INITIAL AND m_hide IS NOT INITIAL.
        RETURN.
      ENDIF.
    ENDIF.
    m_variable = lr_new.

    DATA td       TYPE sydes_desc.
    DESCRIBE FIELD io_var INTO td.
    IF td-types[ 1 ]-type = 'r'.
      m_object = io_var.
    ENDIF.

    IF iv_key IS SUPPLIED AND iv_key IS NOT INITIAL.
      l_key = iv_key.
    ELSE.
      l_key = main_node_key.
    ENDIF.

    traverse(
      io_type_descr = cl_abap_typedescr=>describe_by_data_ref( m_variable )
      iv_parent_key = l_key
      iv_name = l_name
      ir_up = m_variable ).
  ENDMETHOD.

  METHOD traverse.
    "FIELD-SYMBOLS: <struc> type any.
    CASE io_type_descr->kind.
      WHEN c_kind-struct. traverse_struct( io_type_descr = io_type_descr iv_parent_key = iv_parent_key iv_name = iv_name ir_up = ir_up ).
      WHEN c_kind-table.  traverse_table( io_type_descr = io_type_descr iv_parent_key = iv_parent_key iv_name = iv_name ir_up = ir_up ).
      WHEN c_kind-elem.   traverse_elem( io_type_descr = io_type_descr iv_parent_key = iv_parent_key  iv_name = iv_name  ir_up = ir_up ).
      WHEN c_kind-class.
      WHEN c_kind-intf.
      WHEN c_kind-ref.
    ENDCASE.
  ENDMETHOD.

  METHOD traverse_struct.
    DATA lt_component TYPE abap_component_tab.
    DATA ls_component LIKE LINE OF lt_component.
    DATA lo_struct_descr TYPE REF TO cl_abap_structdescr.
    DATA ls_tree TYPE ts_table.
    DATA lv_text TYPE lvc_value.
    DATA lv_node_key TYPE salv_de_node_key.
    DATA lo_nodes TYPE REF TO cl_salv_nodes.
    DATA lv_icon TYPE salv_de_tree_image.
    DATA lr_struc      TYPE REF TO data.

    lo_struct_descr ?= io_type_descr.
    ls_tree-ref =  ir_up."m_variable.
    ls_tree-typename = lo_struct_descr->absolute_name.
    REPLACE FIRST OCCURRENCE OF '\TYPE=' IN ls_tree-typename+0(6) WITH ''.
    IF ls_tree-typename+0(1) = '%'.
      ls_tree-typename = |{ lo_struct_descr->type_kind }({ lo_struct_descr->length / 2 })|.
    ENDIF.

    ls_tree-kind = lo_struct_descr->type_kind.
    lv_icon = icon_structure.
    IF iv_name IS NOT INITIAL.
      lv_text = iv_name.
    ELSE.
      lv_text = ls_tree-typename.
    ENDIF.

    IF lv_text IS NOT INITIAL.
      lv_node_key =
        tree->get_nodes( )->add_node(
          related_node   = iv_parent_key
          relationship   = if_salv_c_node_relation=>last_child
          collapsed_icon = lv_icon
          expanded_icon  = lv_icon
          data_row       = ls_tree
          text           = lv_text
          folder         = abap_true
        )->get_key( ).
    ENDIF.

    lt_component = lo_struct_descr->get_components( ).
    LOOP AT lt_component INTO ls_component. "WHERE name IS NOT INITIAL.
      DATA: lr_new_struc TYPE REF TO data.
      ASSIGN ir_up->* TO FIELD-SYMBOL(<up>).
      IF ls_component-name IS INITIAL.
        lr_new_struc = ir_up.
      ELSE.
        ASSIGN COMPONENT ls_component-name OF STRUCTURE <up> TO FIELD-SYMBOL(<new>).
        GET REFERENCE OF <new> INTO lr_new_struc.
      ENDIF.

      traverse(
        io_type_descr = ls_component-type
        iv_parent_key = lv_node_key
        iv_name = ls_component-name
        ir_up = lr_new_struc ).
    ENDLOOP.
  ENDMETHOD.

  METHOD traverse_elem.
    DATA lo_elem_descr TYPE REF TO cl_abap_elemdescr.
    DATA ls_tree TYPE ts_table.
    DATA lv_text TYPE lvc_value.
    DATA lv_icon TYPE salv_de_tree_image.

    lo_elem_descr ?= io_type_descr.
    ls_tree-ref = ir_up."m_variable.

    ls_tree-typename = lo_elem_descr->absolute_name.
    REPLACE FIRST OCCURRENCE OF '\TYPE=' IN ls_tree-typename WITH ''.
    IF ls_tree-typename+0(1) = '%'.
      ls_tree-typename = |{ lo_elem_descr->type_kind }({ lo_elem_descr->length / 2 })|.
    ENDIF.

    ls_tree-kind = lo_elem_descr->type_kind.
    IF iv_value IS SUPPLIED.
      ls_tree-value = iv_value.
    ELSE.
      ASSIGN ir_up->* TO FIELD-SYMBOL(<var>).
      IF <var> IS NOT INITIAL.
        ls_tree-value = <var>.
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

    lv_text = iv_name.

    IF ls_tree-value IS INITIAL AND m_hide IS NOT INITIAL.
      RETURN.
    ENDIF.
    tree->get_nodes( )->add_node(
      related_node   = iv_parent_key
      relationship   = if_salv_c_node_relation=>last_child
      data_row       = ls_tree
      collapsed_icon = lv_icon
      expanded_icon  = lv_icon
      text           = lv_text
      folder         = abap_false ).
  ENDMETHOD.

  METHOD traverse_table.
    DATA lo_table_descr TYPE REF TO cl_abap_tabledescr.
    DATA ls_tree TYPE ts_table.
    DATA lv_text TYPE lvc_value.
    DATA lv_node_key TYPE salv_de_node_key.
    DATA lo_nodes TYPE REF TO cl_salv_nodes.
    DATA lv_icon TYPE salv_de_tree_image.
    DATA lt_key TYPE abap_table_keydescr_tab.
    DATA ls_key LIKE LINE OF lt_key.
    DATA lv_key_node_key TYPE salv_de_node_key.
    DATA ls_key_component LIKE LINE OF ls_key-components.
    DATA lo_struct_descr TYPE REF TO cl_abap_structdescr.
    DATA lt_view TYPE abap_component_view_tab.
    DATA ls_view LIKE LINE OF lt_view.

    FIELD-SYMBOLS: <tab> TYPE ANY TABLE.
    ASSIGN ir_up->* TO <tab>.
    DATA(lines) = lines( <tab> ).
    ls_tree-ref = ir_up.

    lo_table_descr ?= io_type_descr.

    ls_tree-fullname = |{ iv_name } ({ lines })|.
    ls_tree-kind = lo_table_descr->type_kind.
    ls_tree-typename = replace(  val = lo_table_descr->absolute_name sub = '\TYPE=' with = ''   ).

    lv_icon = icon_view_table.

    IF iv_name IS NOT INITIAL.
      lv_text = ls_tree-fullname.
    ELSE.
      lv_text = ls_tree-typename.
    ENDIF.

    IF lines > 0 OR  m_hide IS INITIAL.
      lv_node_key =
        tree->get_nodes( )->add_node(
          related_node   = iv_parent_key
          relationship   = if_salv_c_node_relation=>last_child
          collapsed_icon = lv_icon
          expanded_icon  = lv_icon
          data_row       = ls_tree
          text           = lv_text
          folder         = abap_true
        )->get_key( ).
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
    lcl_alv_common=>refresh( EXPORTING i_obj = lo_alv i_soft = abap_true ).

    lo_alv ?= e_dragdropobj->droptargetctrl.
    lo_to->raise_selection_done( ).
  ENDMETHOD.
ENDCLASS.
