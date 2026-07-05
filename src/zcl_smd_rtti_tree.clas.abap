class ZCL_SMD_RTTI_TREE definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF t_classes_leaf,
             name TYPE string,
             type TYPE char1,
             key  TYPE salv_de_node_key,
           END OF t_classes_leaf .
  types:
    BEGIN OF ts_table,
             ref      TYPE REF TO data,
             kind(1),
             value    TYPE string,
             typename TYPE abap_abstypename,
             fullname TYPE string,
             path     TYPE string,
             instance TYPE string,
           END OF ts_table .
  types:
    tt_table TYPE STANDARD TABLE OF ts_table
          WITH NON-UNIQUE DEFAULT KEY .

  data MAIN_NODE_KEY type SALV_DE_NODE_KEY .
  data M_REFRESH type XFELD .
  data M_LEAF type STRING .
  data M_CLEAR type FLAG .
  data M_LOCALS type X .
  data M_GLOBALS type X .
  data M_SYST type X .
  data M_CLASS_DATA type X .
  data M_LDB type X .
  data M_LOCALS_KEY type SALV_DE_NODE_KEY .
  data M_GLOBALS_KEY type SALV_DE_NODE_KEY .
  data M_CLASS_KEY type SALV_DE_NODE_KEY .
  data M_SYST_KEY type SALV_DE_NODE_KEY .
  data M_LDB_KEY type SALV_DE_NODE_KEY .
  data M_ICON type SALV_DE_TREE_IMAGE .
  data:
    mt_vars         TYPE STANDARD TABLE OF zcl_smd_appl=>var_table .
  data:
    mt_classes_leaf TYPE TABLE OF t_classes_leaf .
  data M_PRG_INFO type TPDA_SCR_PRG_INFO .
  data MO_DEBUGGER type ref to ZCL_SMD_DEBUGGER_SCRIPT .
  data M_TREE type ref to CL_SALV_TREE .

  methods CONSTRUCTOR
    importing
      !I_HEADER type CLIKE default 'View'
      !I_TYPE type XFELD optional
      !I_CONT type ref to CL_GUI_CONTAINER optional
      !I_DEBUGGER type ref to ZCL_SMD_DEBUGGER_SCRIPT optional .
  methods DEL_VARIABLE
    importing
      !I_FULL_NAME type STRING
      !I_STATE type XFELD optional .
  methods CLEAR .
  methods ADD_BUTTONS
    importing
      !I_TYPE type XFELD .
  methods ADD_NODE
    importing
      !I_NAME type STRING
      !I_ICON type SALV_DE_TREE_IMAGE optional .
  methods ADD_OBJ_NODES
    importing
      !IS_VAR type ZCL_SMD_APPL=>VAR_TABLE
    returning
      value(E_ROOT_KEY) type SALV_DE_NODE_KEY .
  methods DELETE_NODE
    importing
      !I_KEY type SALV_DE_NODE_KEY .
  methods DISPLAY
    importing
      !IO_DEBUGGER type ref to ZCL_SMD_DEBUGGER_SCRIPT optional .
  methods TRAVERSE
    importing
      !IO_TYPE_DESCR type ref to CL_ABAP_TYPEDESCR
      !I_PARENT_KEY type SALV_DE_NODE_KEY
      !I_REL type SALV_DE_NODE_RELATION
      !IS_VAR type ZCL_SMD_APPL=>VAR_TABLE
      !IR_UP type ref to DATA optional
      !I_PARENT_CALCULATED type STRING optional
      !I_STRUC_NAME type STRING optional
    returning
      value(E_ROOT_KEY) type SALV_DE_NODE_KEY .
  methods TRAVERSE_STRUCT
    importing
      !IO_TYPE_DESCR type ref to CL_ABAP_TYPEDESCR
      !I_PARENT_KEY type SALV_DE_NODE_KEY
      !I_REL type SALV_DE_NODE_RELATION
      !IS_VAR type ZCL_SMD_APPL=>VAR_TABLE
      !IR_UP type ref to DATA optional
      !I_PARENT_CALCULATED type STRING optional
      !I_STRUC_NAME type STRING optional
    returning
      value(E_ROOT_KEY) type SALV_DE_NODE_KEY .
  methods TRAVERSE_ELEM
    importing
      !IO_TYPE_DESCR type ref to CL_ABAP_TYPEDESCR
      !I_PARENT_KEY type SALV_DE_NODE_KEY
      !I_REL type SALV_DE_NODE_RELATION
      !IS_VAR type ZCL_SMD_APPL=>VAR_TABLE
      !I_VALUE type ANY optional
      !I_PARENT_CALCULATED type STRING optional
    returning
      value(E_ROOT_KEY) type SALV_DE_NODE_KEY .
  methods TRAVERSE_OBJ
    importing
      !I_PARENT_KEY type SALV_DE_NODE_KEY
      !I_REL type SALV_DE_NODE_RELATION
      !IS_VAR type ZCL_SMD_APPL=>VAR_TABLE
      !I_VALUE type ANY optional
      !IR_UP type ref to DATA optional
      !I_PARENT_CALCULATED type STRING optional
    returning
      value(E_ROOT_KEY) type SALV_DE_NODE_KEY .
  methods TRAVERSE_TABLE
    importing
      !IO_TYPE_DESCR type ref to CL_ABAP_TYPEDESCR
      !I_PARENT_KEY type SALV_DE_NODE_KEY
      !I_REL type SALV_DE_NODE_RELATION
      !IS_VAR type ZCL_SMD_APPL=>VAR_TABLE
      !IR_UP type ref to DATA optional
      !I_PARENT_CALCULATED type STRING optional
    returning
      value(E_ROOT_KEY) type SALV_DE_NODE_KEY .
protected section.
private section.

  constants:
    BEGIN OF c_kind,
                 struct LIKE cl_abap_typedescr=>kind_struct VALUE cl_abap_typedescr=>kind_struct,
                 table  LIKE cl_abap_typedescr=>kind_table VALUE cl_abap_typedescr=>kind_table,
                 elem   LIKE cl_abap_typedescr=>kind_elem VALUE cl_abap_typedescr=>kind_elem,
                 class  LIKE cl_abap_typedescr=>kind_class VALUE cl_abap_typedescr=>kind_class,
                 intf   LIKE cl_abap_typedescr=>kind_intf VALUE cl_abap_typedescr=>kind_intf,
                 ref    LIKE cl_abap_typedescr=>kind_ref VALUE cl_abap_typedescr=>kind_ref,
               END OF c_kind .
  data TREE_TABLE type TT_TABLE .

  methods HNDL_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_SALV_EVENTS_TREE
    importing
      !NODE_KEY .
  methods HNDL_USER_COMMAND
    for event ADDED_FUNCTION of CL_SALV_EVENTS
    importing
      !E_SALV_FUNCTION .
ENDCLASS.



CLASS ZCL_SMD_RTTI_TREE IMPLEMENTATION.


  method ADD_BUTTONS.


    DATA(o_functions) = m_tree->get_functions( ).
    o_functions->set_all( ).

    o_functions->set_group_layout( abap_false ).
    o_functions->set_group_aggregation( abap_false ).
    o_functions->set_group_print( abap_false ).

    CHECK mo_debugger IS NOT INITIAL AND i_type = 'L'.

    o_functions->add_function(
      name     = 'INITIALS'
      icon     = CONV #( icon_start_viewer )
      text     = 'Initials'
      tooltip  = 'Show/hide initial values'
      position = if_salv_c_function_position=>right_of_salv_functions ).

    o_functions->add_function(
      name     = 'LOCALS'
      icon     = CONV #( icon_foreign_trade )
      text     = 'Locals'
      tooltip  = 'Show/hide locals variables'
      position = if_salv_c_function_position=>right_of_salv_functions ).

    o_functions->add_function(
      name     = 'GLOBALS'
      icon     = CONV #( icon_foreign_trade )
      text     = 'Globals'
      tooltip  = 'Show/hide global variables'
      position = if_salv_c_function_position=>right_of_salv_functions ).

    o_functions->add_function(
      name     = 'SYST'
      icon     = CONV #( icon_foreign_trade )
      text     = 'SYST'
      tooltip  = 'Show/hide SY sructure'
      position = if_salv_c_function_position=>right_of_salv_functions ).

    o_functions->add_function(
      name     = 'CLASS_DATA'
      icon     = CONV #( icon_oo_class_attribute )
      text     = 'CLASS-DATA'
      tooltip  = 'Show/hide Class-Data variables (global)'
      position = if_salv_c_function_position=>right_of_salv_functions ).

    o_functions->add_function(
      name     = 'LDB'
      icon     = CONV #( icon_biw_report_view )
      text     = 'LDB'
      tooltip  = 'Show/hide Local Data Base variables (global)'
      position = if_salv_c_function_position=>right_of_salv_functions ).

    o_functions->add_function(
      name     = 'REFRESH'
      icon     = CONV #( icon_refresh )
      text     = ''
      tooltip  = 'Refresh'
      position = if_salv_c_function_position=>left_of_salv_functions ).


  endmethod.


  method ADD_NODE.


    main_node_key =
          m_tree->get_nodes( )->add_node(
            related_node   = ''
            collapsed_icon = i_icon
            expanded_icon = i_icon
            relationship   = if_salv_c_node_relation=>last_child
            row_style = if_salv_c_tree_style=>intensified
            text           = CONV #( i_name )
            folder         = abap_true
          )->get_key( ).

    CASE i_name.
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


  endmethod.


  method ADD_OBJ_NODES.


    DATA match TYPE match_result_tab.
    FIND ALL OCCURRENCES OF  '-' IN is_var-name RESULTS match. "Only first level of instance should be here
    IF lines( match ) > 1.
      RETURN.
    ENDIF.

    DATA text TYPE lvc_value.
    DATA node_key TYPE salv_de_node_key.
    DATA icon TYPE salv_de_tree_image.

    CASE is_var-cl_leaf.
      WHEN 1.
        icon = icon_led_green.
        text = 'Public'.
      WHEN 2.
        icon = icon_led_red.
        text = 'Private'.
      WHEN 3.
        icon = icon_led_yellow.
        text = 'Protected'.
    ENDCASE.

    READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf ASSIGNING FIELD-SYMBOL(<class>).
    IF sy-subrc NE 0.

      READ TABLE mt_vars WITH KEY path = is_var-parent INTO DATA(var).
      node_key =
        m_tree->get_nodes( )->add_node(
          related_node   = var-key
          relationship   = if_salv_c_node_relation=>last_child
          collapsed_icon = icon
          expanded_icon  = icon
          text           = text
          folder         = abap_true
        )->get_key( ).

      APPEND INITIAL LINE TO mt_classes_leaf ASSIGNING <class>.
      <class>-name = is_var-parent.
      <class>-key = node_key.
      <class>-type = is_var-cl_leaf.
    ENDIF.


  endmethod.


  method CLEAR.


    m_tree->get_nodes( )->delete_all( ).

    CLEAR: m_globals_key,
           m_locals_key,
           m_syst_key,
           m_ldb_key,
           m_class_key,
           mt_vars,
           mt_classes_leaf.


  endmethod.


  method CONSTRUCTOR.


    super->constructor( ).
    mo_debugger = i_debugger.

    cl_salv_tree=>factory(
      EXPORTING
        r_container = i_cont
      IMPORTING
        r_salv_tree = m_tree
      CHANGING
        t_table     = tree_table ).

    DATA(o_setting) =  m_tree->get_tree_settings( ).
    o_setting->set_hierarchy_header( i_header ).
    o_setting->set_hierarchy_size( 30 ).
    o_setting->set_hierarchy_icon( CONV #( icon_tree ) ).

    DATA(o_columns) = m_tree->get_columns( ).
    o_columns->set_optimize( abap_true ).

    o_columns->get_column( 'VALUE' )->set_short_text( 'Value' ).
    o_columns->get_column( 'INSTANCE' )->set_short_text( 'Instance' ).
    o_columns->get_column( 'FULLNAME' )->set_visible( abap_false ).
    o_columns->get_column( 'PATH' )->set_visible( abap_false ).
    o_columns->get_column( 'TYPENAME' )->set_short_text( 'Type' ).
    o_columns->get_column( 'TYPENAME' )->set_medium_text( 'Absolute Type' ).

    add_buttons( i_type ).

    DATA(o_event) = m_tree->get_event( ) .
    SET HANDLER hndl_double_click
                hndl_user_command FOR o_event.

    m_globals = '01'.
    m_tree->display( ).


  endmethod.


  method DELETE_NODE.


    DATA(o_nodes) = m_tree->get_nodes( ).
    DATA(o_node) =  o_nodes->get_node( i_key ).
    IF o_node IS NOT INITIAL.
      o_node->delete( ).

    ENDIF.


  endmethod.


  method DEL_VARIABLE.


    IF mo_debugger->m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.
    DATA(vars_hist) = mo_debugger->mt_vars_hist.
    SORT vars_hist BY step DESCENDING.
    LOOP AT vars_hist INTO DATA(hist) WHERE name = i_full_name.
      IF hist-del IS INITIAL.
        CLEAR: hist-ref, hist-first.
        hist-del = abap_true.
        hist-step = mo_debugger->m_hist_step - 1.
        INSERT hist INTO mo_debugger->mt_vars_hist INDEX 1.
      ENDIF.
    ENDLOOP.

    DATA(o_nodes) = m_tree->get_nodes( ).
    READ TABLE mo_debugger->mt_state WITH KEY name = i_full_name ASSIGNING FIELD-SYMBOL(<var>).
    IF sy-subrc = 0.

      TRY.
          DATA(o_node) =  o_nodes->get_node( <var>-key ).
        CATCH cx_salv_msg.
      ENDTRY.

      DELETE mt_vars WHERE name = i_full_name.
      DELETE mt_classes_leaf WHERE name = i_full_name.
      IF i_state = abap_true.
        DELETE mo_debugger->mt_state WHERE name = i_full_name.
      ENDIF.

      DATA(nam) = i_full_name && '-'.
      DELETE mt_vars WHERE name CS nam.
      DELETE mt_classes_leaf WHERE name  CS nam.
      IF i_state = abap_true.
        DELETE mo_debugger->mt_state WHERE name CS nam.
      ENDIF.
      TRY.
          IF o_node IS NOT INITIAL.
            o_node->delete( ).
          ENDIF.
        CATCH cx_salv_msg.
      ENDTRY.
    ENDIF.


  endmethod.


  method DISPLAY.


    DATA(o_columns) = m_tree->get_columns( ).
    o_columns->get_column( 'KIND' )->set_visible( abap_false ).

    DATA(o_nodes) = m_tree->get_nodes( ).
    DATA(nodes) =  o_nodes->get_all_nodes( ).


    DATA sub TYPE salv_t_nodes.
    LOOP AT nodes INTO DATA(node).
      READ TABLE sub WITH KEY node = node-node TRANSPORTING NO FIELDS. "expanding only first level nodes.
      IF sy-subrc NE 0.
        TRY.
            node-node->expand( ).
            sub = node-node->get_subtree( ).
          CATCH cx_root.
        ENDTRY.
      ENDIF.
    ENDLOOP.
    m_tree->display( ).


  endmethod.


  method HNDL_DOUBLE_CLICK.


    DATA(o_nodes) = m_tree->get_nodes( ).
    DATA(o_node) =  o_nodes->get_node( node_key ).
    DATA r_row TYPE REF TO data.

    r_row = o_node->get_data_row( ).
    ASSIGN r_row->* TO FIELD-SYMBOL(<row>).
    ASSIGN COMPONENT 'REF' OF STRUCTURE <row> TO FIELD-SYMBOL(<ref>).
    ASSIGN COMPONENT 'KIND' OF STRUCTURE <row> TO FIELD-SYMBOL(<kind>).
    ASSIGN COMPONENT 'FULLNAME' OF STRUCTURE <row> TO FIELD-SYMBOL(<fullname>).
    ASSIGN COMPONENT 'PATH' OF STRUCTURE <row> TO FIELD-SYMBOL(<path>).

    IF <fullname> IS NOT INITIAL.
      READ TABLE mo_debugger->mt_selected_var WITH KEY name =  <fullname> TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        DELETE mo_debugger->mt_selected_var WHERE name = <fullname>.
        o_node->set_row_style( if_salv_c_tree_style=>default ).
      ELSE.
        o_node->set_row_style( if_salv_c_tree_style=>emphasized_b ).
        APPEND INITIAL LINE TO mo_debugger->mt_selected_var ASSIGNING FIELD-SYMBOL(<sel>).
        <sel>-name = <fullname>.
        <sel>-is_sel = abap_true.
      ENDIF.

      CASE <kind>.
        WHEN cl_abap_datadescr=>typekind_table.
          zcl_smd_appl=>open_int_table( i_name = <fullname> it_ref = <ref> io_window = mo_debugger->mo_window ).
        WHEN cl_abap_datadescr=>typekind_string.
          NEW zcl_smd_text_viewer( <ref> ).
      ENDCASE.
    ENDIF.


  endmethod.


  method HNDL_USER_COMMAND.


    CONSTANTS: c_mask TYPE x VALUE '01'.

    CASE e_salv_function.

      WHEN 'REFRESH'."
        m_refresh = abap_true.
        mo_debugger->run_script_hist( mo_debugger->m_hist_step ).
        mo_debugger->mo_tree_local->display( ).
        RETURN.

      WHEN 'INITIALS'."Show/hide empty variables
        mo_debugger->m_hide = mo_debugger->m_hide BIT-XOR c_mask.
        m_clear = abap_true.

      WHEN 'LOCALS'."Show/hide locals variables
        m_locals = m_locals BIT-XOR c_mask.
        m_refresh = abap_true.
      WHEN 'GLOBALS'."Show/hide global variables
        m_globals = m_globals BIT-XOR c_mask.
        m_refresh = abap_true.
      WHEN 'SYST'."Show/hide sy structure
        m_syst = m_syst BIT-XOR c_mask.
        m_refresh = abap_true.
      WHEN 'CLASS_DATA'."Show/hide CLASS-DATA variables (globals)
        m_class_data = m_class_data BIT-XOR c_mask.

      WHEN 'LDB'."Show/hide LDB variables (globals)
        m_ldb = m_ldb BIT-XOR c_mask.
    ENDCASE.

    m_refresh = abap_true.
    mo_debugger->mo_tree_local->clear( ).
    mo_debugger->mo_tree_exp->clear( ).
    mo_debugger->mo_tree_imp->clear( ).

    "mo_debugger->run_script_hist( mo_debugger->m_hist_step ).
    "mo_debugger->mo_tree_local->display( ).

    "RETURN.

    mo_debugger->m_update = abap_true.

    mo_debugger->mo_tree_local->display( ).

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


  endmethod.


  method TRAVERSE.


    ASSIGN ir_up->* TO FIELD-SYMBOL(<new>).
    IF <new> IS INITIAL AND mo_debugger->m_hide IS NOT INITIAL.
      me->del_variable( CONV #( is_var-name )  ).
      RETURN.
    ENDIF.

    CASE io_type_descr->kind.
      WHEN c_kind-struct.
        IF i_struc_name IS SUPPLIED.
          e_root_key = traverse_struct( io_type_descr  = io_type_descr
                                        i_parent_key  = i_parent_key
                                        i_rel         = i_rel
                                        is_var         = is_var
                                        ir_up          = ir_up
                                        i_parent_calculated = i_parent_calculated
                                        i_struc_name  = i_struc_name ).
        ELSE.
          e_root_key = traverse_struct( io_type_descr  = io_type_descr
                                        i_parent_key  = i_parent_key
                                        i_rel         = i_rel
                                        is_var         = is_var
                                        ir_up          = ir_up
                                        i_parent_calculated = i_parent_calculated ).
        ENDIF.

      WHEN c_kind-table.
        e_root_key = traverse_table( io_type_descr  = io_type_descr
                                     i_parent_key  = i_parent_key
                                     i_rel         = i_rel
                                     is_var         = is_var
                                     ir_up          = ir_up
                                     i_parent_calculated = i_parent_calculated ).
      WHEN c_kind-elem.
        e_root_key = traverse_elem( io_type_descr  = io_type_descr
                                    i_parent_key  = i_parent_key
                                    i_rel         = i_rel
                                    is_var         = is_var
                                    i_parent_calculated = i_parent_calculated ).

    ENDCASE.


  endmethod.


  method TRAVERSE_ELEM.


    DATA: o_elem_descr TYPE REF TO cl_abap_elemdescr,
          tree         TYPE ts_table,
          text         TYPE lvc_value,
          icon         TYPE salv_de_tree_image,
          key          TYPE salv_de_node_key,
          rel          TYPE salv_de_node_relation.

    o_elem_descr ?= io_type_descr.
    tree-ref = is_var-ref.
    rel = i_rel.

    IF is_var-instance NE '{A:initial}'.
      tree-typename = o_elem_descr->absolute_name.
      REPLACE FIRST OCCURRENCE OF '\TYPE=' IN tree-typename WITH ''.
      IF tree-typename+0(1) = '%'.
        tree-typename = |{ o_elem_descr->type_kind }({ o_elem_descr->length / 2 })|.
      ENDIF.
    ENDIF.

    tree-kind = o_elem_descr->type_kind.

    ASSIGN is_var-ref->* TO FIELD-SYMBOL(<new_value>).
    IF i_value IS SUPPLIED.
      tree-value = i_value.
    ELSE.
      IF <new_value> IS NOT INITIAL.
        tree-value = <new_value>.
      ENDIF.
    ENDIF.

    CASE o_elem_descr->type_kind.
      WHEN 'D'.
        icon = icon_date.
      WHEN 'T'.
        icon = icon_bw_time_sap.
      WHEN 'C'.
        icon = icon_wd_input_field.
      WHEN 'P'.
        icon = icon_increase_decimal.
      WHEN 'g'.
        icon = icon_text_act.
      WHEN 'N' OR 'I'.
        icon = icon_pm_order.
      WHEN OTHERS.
        icon = icon_element.
    ENDCASE.

    text = is_var-short.
    tree-fullname = is_var-name."is_var-path.
    tree-path = is_var-path.

    "own new method
    IF is_var-cl_leaf IS NOT INITIAL.

      add_obj_nodes( EXPORTING is_var = is_var ).

      READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf INTO DATA(leaf).
      IF sy-subrc = 0.
        key = leaf-key.
      ENDIF.
    ELSE.
      key = i_parent_key.
    ENDIF.

    IF key IS INITIAL.
      key = i_parent_key.
      rel = i_rel.
    ENDIF.

    DATA(nodes) = m_tree->get_nodes( )->get_all_nodes( ).
    LOOP AT nodes INTO DATA(node).
      DATA(name) = node-node->get_text( ).
      DATA(lr_row) = node-node->get_data_row( ).
      FIELD-SYMBOLS <row> TYPE ts_table.
      ASSIGN lr_row->* TO <row>.
      IF <row>-fullname = is_var-name.
        DATA(o_node) = node-node.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF mo_debugger->m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.

    IF o_node IS NOT INITIAL.
      READ TABLE mt_vars WITH KEY name = is_var-name INTO DATA(var).
      IF sy-subrc = 0.
        TRY.
            FIELD-SYMBOLS: <old_value> TYPE any.
            ASSIGN var-ref->* TO <old_value>.
            IF sy-subrc = 0.
              IF is_var-type = var-type.
                IF <old_value> NE <new_value>.
                  key = var-key.
                  rel = if_salv_c_node_relation=>next_sibling.
                  DELETE mt_vars WHERE name = is_var-name.
                ELSE.
                  IF ( <new_value> IS INITIAL AND mo_debugger->m_hide IS NOT INITIAL ).
                  ELSE.
                    RETURN.
                  ENDIF.
                ENDIF.
              ELSE.
                key = var-key.
                rel = if_salv_c_node_relation=>next_sibling.
                DELETE mt_vars WHERE name = is_var-name.
              ENDIF.
            ENDIF.
          CATCH cx_root.
            DELETE mt_vars WHERE name = is_var-name.
        ENDTRY.
      ENDIF.
    ENDIF.

    DATA(o_nodes) = m_tree->get_nodes( ).

    TRY.
        CALL METHOD o_nodes->add_node
          EXPORTING
            related_node   = key
            relationship   = rel
            data_row       = tree
            collapsed_icon = icon
            expanded_icon  = icon
            text           = text
            folder         = abap_false
          RECEIVING
            node           = o_node.

        IF sy-subrc = 0.
          e_root_key = o_node->get_key( ).

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
          <vars>-type = o_elem_descr->absolute_name.
          <vars>-path = is_var-path.

          IF rel = if_salv_c_node_relation=>next_sibling AND o_node IS NOT INITIAL.
            IF o_node IS NOT INITIAL.
              o_node->delete( ).
            ENDIF.
          ENDIF.
        ENDIF.
      CATCH cx_salv_msg.
    ENDTRY.


  endmethod.


  method TRAVERSE_OBJ.


    DATA: tree TYPE ts_table,
          text TYPE lvc_value,
          icon TYPE salv_de_tree_image,
          key  TYPE salv_de_node_key,
          rel  TYPE salv_de_node_relation.

    READ TABLE mt_vars WITH KEY name = is_var-name INTO DATA(var).
    IF mo_debugger->m_debug IS NOT INITIAL.BREAK-POINT.ENDIF.
    IF sy-subrc = 0.
      DATA(o_nodes) = m_tree->get_nodes( ).
      DATA(o_node) =  o_nodes->get_node( var-key ).

      IF var-ref = ir_up.
        RETURN.
      ENDIF.

    ELSE.
      rel = i_rel.
    ENDIF.

    icon = icon_oo_object.
    text = is_var-short.
    tree-fullname = is_var-name.
    tree-path = is_var-path.

    DATA(string) = is_var-instance.
    DATA: split TYPE TABLE OF string.

    IF is_var-instance IS NOT INITIAL.
      string = is_var-instance.
      REPLACE ALL OCCURRENCES OF REGEX '[*{}]' IN string WITH ''.
      REPLACE ALL OCCURRENCES OF '\CLASS' IN string WITH ''.
      REPLACE ALL OCCURRENCES OF '\PROGRAM' IN string WITH ''.
      SPLIT string AT '=' INTO TABLE split.
      tree-instance = |{ split[ lines( split ) ] }({ split[ 1 ] })|.
    ENDIF.
    "own new method
    IF is_var-cl_leaf IS NOT INITIAL.

      add_obj_nodes( EXPORTING is_var = is_var ).

      READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf INTO DATA(leaf).
      IF sy-subrc = 0.
        key = leaf-key.
      ENDIF.
    ENDIF.

    IF key IS INITIAL.
      key = i_parent_key.
      rel = i_rel.
    ENDIF.

    e_root_key = m_tree->get_nodes( )->add_node(
     related_node   = key
     relationship   = rel
     data_row       = tree
     collapsed_icon = icon
     expanded_icon  = icon
     text           = text
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

    IF o_node IS NOT INITIAL.
      o_node->delete( ).
    ENDIF.


  endmethod.


  method TRAVERSE_STRUCT.


    DATA: component      TYPE abap_component_tab,
          o_struct_descr TYPE REF TO cl_abap_structdescr,
          tree           TYPE ts_table,
          text           TYPE lvc_value,
          key            TYPE salv_de_node_key,
          rel            TYPE salv_de_node_relation,
          icon           TYPE salv_de_tree_image.

    ASSIGN is_var-ref->* TO FIELD-SYMBOL(<new_value>).
    rel = i_rel.
    o_struct_descr ?= io_type_descr.
    tree-ref =  ir_up.
    IF is_var-instance NE '{A:initial}'.
      "ls_tree-typename = o_struct_descr->absolute_name.
      "REPLACE FIRST OCCURRENCE OF '\TYPE=' IN tree-typename+0(6) WITH ''.
      DATA: split TYPE TABLE OF string.
      SPLIT o_struct_descr->absolute_name AT '\TYPE=' INTO TABLE split.
      tree-typename = split[ lines( split ) ].

      IF tree-typename+0(1) = '%'.
        tree-typename = |{ o_struct_descr->type_kind }({ o_struct_descr->length / 2 })|.
      ENDIF.
    ENDIF.

    tree-kind = o_struct_descr->type_kind.

    IF m_icon IS INITIAL.
      icon = icon_structure.
    ELSE.
      icon = m_icon.
    ENDIF.

    text = is_var-short.
    tree-fullname = is_var-name.
    tree-path = is_var-path.

    "own new method
    IF is_var-cl_leaf IS NOT INITIAL.

      add_obj_nodes( EXPORTING is_var = is_var ).

      READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf INTO DATA(leaf).
      IF sy-subrc = 0.
        key = leaf-key.
      ENDIF.
    ELSE.
      key = i_parent_key.
    ENDIF.

    IF key IS INITIAL.
      key = i_parent_key.
      rel = i_rel.
    ENDIF.

    IF  ( i_struc_name IS SUPPLIED AND i_struc_name IS NOT INITIAL ) OR i_struc_name IS NOT SUPPLIED.
      IF text IS NOT INITIAL.

        DATA(nodes) = m_tree->get_nodes( )->get_all_nodes( ).
        LOOP AT nodes INTO DATA(node).
          DATA(lr_row) = node-node->get_data_row( ).
          FIELD-SYMBOLS <row> TYPE ts_table.
          ASSIGN lr_row->* TO <row>.
          IF <row>-fullname = is_var-name.
            DATA(o_node) = node-node.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF o_node IS NOT INITIAL.
          READ TABLE mt_vars WITH KEY name = is_var-name INTO DATA(var).
          IF sy-subrc = 0.
            IF o_node IS NOT INITIAL.
              TRY.
                  FIELD-SYMBOLS: <old_value> TYPE any.
                  ASSIGN var-ref->* TO <old_value>.
                  IF sy-subrc = 0.
                    IF is_var-type = var-type.
                      RETURN.
                    ELSE.
                      key = var-key.
                      rel = if_salv_c_node_relation=>next_sibling.
                      DELETE mt_vars WHERE name = is_var-name.
                    ENDIF.
                  ENDIF.
                CATCH cx_root.
                  DELETE mt_vars WHERE name = is_var-name.
              ENDTRY.

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      e_root_key = m_tree->get_nodes( )->add_node(
             related_node   = key
             relationship   = rel
             data_row       = tree
             collapsed_icon = icon
             expanded_icon  = icon
             text           = text
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
      <vars>-path = is_var-path.
      <vars>-type = o_struct_descr->absolute_name.

    ENDIF.

    IF rel = if_salv_c_node_relation=>next_sibling AND o_node IS NOT INITIAL.
      IF o_node IS NOT INITIAL.

        o_node->delete( ).
      ENDIF.
    ENDIF.


  endmethod.


  method TRAVERSE_TABLE.


    DATA: o_table_descr TYPE REF TO cl_abap_tabledescr,
          tree          TYPE ts_table,
          text          TYPE lvc_value,
          icon          TYPE salv_de_tree_image,
          key           TYPE salv_de_node_key,
          rel           TYPE salv_de_node_relation.

    FIELD-SYMBOLS: <tab> TYPE ANY TABLE.

    ASSIGN ir_up->* TO <tab>.
    DATA(lines) = lines( <tab> ).
    tree-ref = ir_up.
    key = i_parent_key.

    o_table_descr ?= io_type_descr.

    tree-fullname = |{ is_var-short } ({ lines })|.
    tree-kind = o_table_descr->type_kind.
    IF is_var-instance NE '{A:initial}'.

      READ TABLE mo_debugger->mo_window->mt_source WITH KEY include = mo_debugger->ms_stack-include INTO DATA(source).
      READ TABLE source-tt_tabs WITH KEY name = is_var-short INTO DATA(tab).
      IF sy-subrc <> 0.
        DATA: split TYPE TABLE OF string.
        SPLIT o_table_descr->absolute_name AT '\TYPE=' INTO TABLE split.
        tree-typename = split[ lines( split ) ].
      ELSE.
        tree-typename = tab-type.
      ENDIF.
    ENDIF.
    icon = icon_view_table.

    IF is_var-name IS NOT INITIAL.
      text = tree-fullname.
    ELSE.
      text = tree-typename.
    ENDIF.

    rel = i_rel.
    ASSIGN ir_up->* TO FIELD-SYMBOL(<new_value>).
    IF mo_debugger->m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.

    READ TABLE mt_vars WITH KEY name = is_var-name INTO DATA(var).
    DATA(nodes) = m_tree->get_nodes( )->get_all_nodes( ).
    LOOP AT nodes INTO DATA(node).
      DATA(lr_row) = node-node->get_data_row( ).
      FIELD-SYMBOLS <row> TYPE ts_table.
      ASSIGN lr_row->* TO <row>.
      IF <row>-fullname = is_var-name.
        DATA(o_node) = node-node.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF o_node IS NOT INITIAL.
      TRY.
          FIELD-SYMBOLS: <old_value> TYPE any.
          ASSIGN var-ref->* TO <old_value>.
          IF sy-subrc = 0.
            IF <old_value> NE <new_value>.
              key = var-key.
              rel = if_salv_c_node_relation=>next_sibling.
              DELETE mt_vars WHERE name = is_var-name.
            ELSE.
              IF ( <new_value> IS INITIAL AND mo_Debugger->m_hide IS NOT INITIAL ).
                me->del_variable( CONV #( is_var-name )  ).
              ENDIF.
            ENDIF.
          ENDIF.

          IF <new_value> IS INITIAL AND mo_debugger->m_hide IS NOT INITIAL.
            me->del_variable( CONV #( is_var-name ) ).
            RETURN.
          ENDIF.
        CATCH cx_root.
          me->del_variable( CONV #( is_var-name )  ).
      ENDTRY.
    ELSE.

      IF <new_value> IS INITIAL AND mo_Debugger->m_hide IS NOT INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    IF is_var-cl_leaf IS NOT INITIAL.

      add_obj_nodes( EXPORTING is_var = is_var ).

      READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf INTO DATA(leaf).
      IF sy-subrc = 0.
        key = leaf-key.
      ENDIF.
    ELSE.
      key = i_parent_key.
    ENDIF.

    READ TABLE mt_vars WITH KEY name = i_parent_calculated TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.

      tree-fullname = is_var-name.
      e_root_key =
        m_tree->get_nodes( )->add_node(
          related_node   = key
          relationship   = i_rel
          collapsed_icon = icon
          expanded_icon  = icon
          data_row       = tree
          text           = text
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

      IF rel = if_salv_c_node_relation=>next_sibling AND o_node IS NOT INITIAL.
        IF o_node IS NOT INITIAL.
          o_node->delete( ).
        ENDIF.
      ENDIF.
    ENDIF.


  endmethod.
ENDCLASS.
