class ZCL_SMD_DEBUGGER_SCRIPT definition
  public
  inheriting from CL_TPDA_SCRIPT_CLASS_SUPER
  create public .

public section.

  types:
    BEGIN OF t_obj,
             name TYPE string,
             obj  TYPE string,
           END OF t_obj .
  types:
    BEGIN OF t_sel_var,
             name   TYPE string,
             is_sel TYPE xfeld,
             refval TYPE REF TO data,
           END OF t_sel_var .

  data:
    mt_obj            TYPE TABLE OF t_obj .
  data:
    mt_compo          TYPE TABLE OF scompo .
  data MT_LOCALS type TPDA_SCR_LOCALS_IT .
  data MT_GLOBALS type TPDA_SCR_GLOBALS_IT .
  data MT_RET_EXP type TPDA_SCR_LOCALS_IT .
  data M_HIDE type X .
  data M_COUNTER type I .
  data:
    mt_steps          TYPE  TABLE OF zcl_smd_appl=>t_step_counter .
  data:
    mt_var_step       TYPE  TABLE OF zcl_smd_appl=>var_table_h .
  data M_STEP type I .
  data M_IS_FIND type XFELD .
  data M_STOP_STACK type I .
  data M_DEBUG type X .
  data M_REFRESH type XFELD .
  data M_UPDATE type XFELD .
  data IS_STEP type XFELD .
  data MS_STACK_PREV type ZCL_SMD_APPL=>T_STACK .
  data MS_STACK type ZCL_SMD_APPL=>T_STACK .
  data IS_HISTORY type XFELD .
  data M_HIST_STEP type I .
  data M_STEP_DELTA type I .
  data:
    mt_vars_hist_view TYPE STANDARD TABLE OF zcl_smd_appl=>var_table .
  data:
    mt_vars_hist      TYPE STANDARD TABLE OF zcl_smd_appl=>var_table .
  data:
    mt_state          TYPE STANDARD TABLE OF zcl_smd_appl=>var_table .
  data MV_RECURSE type I .
  data:
    mt_classes_types  TYPE TABLE OF zcl_smd_appl=>t_classes_types .
  data MO_WINDOW type ref to ZCL_SMD_WINDOW .
  data MV_F7_STOP type XFELD .
  data M_F6_LEVEL type I .
  data M_TARGET_STACK type I .
  data MO_TREE_IMP type ref to ZCL_SMD_RTTI_TREE .
  data MO_TREE_LOCAL type ref to ZCL_SMD_RTTI_TREE .
  data MO_TREE_EXP type ref to ZCL_SMD_RTTI_TREE .
  data:
    mt_selected_var   TYPE TABLE OF t_sel_var .
  data MV_STACK_CHANGED type XFELD .
  data M_VARIABLE type ref to DATA .
  data:
    mt_new_string     TYPE TABLE OF  string .
  data M_QUICK type TPDA_SCR_QUICK_INFO .
  data:
    mr_statements     TYPE RANGE OF string .

  methods RUN_SCRIPT .
  methods RUN_SCRIPT_HIST
    importing
      !I_STEP type I optional
    exporting
      !ES_STOP type XFELD .
  methods SHOW_VARIABLES
    changing
      !IT_VAR type ZCL_SMD_APPL=>VARIABLES
    returning
      value(STOP) type XFELD .
  methods SET_SELECTED_VARS .
  methods SAVE_HIST
    importing
      !I_NAME type CLIKE
      !I_FULLNAME type STRING
      !I_TYPE type STRING
      !I_CL_LEAF type INT4
      !I_PARENT_CALCULATED type STRING
      !IR_UP type ANY optional
      !I_INSTANCE type STRING optional .
  methods F5
    returning
      value(STOP) type XFELD .
  methods F6
    returning
      value(STOP) type XFELD .
  methods F7
    returning
      value(STOP) type XFELD .
  methods F8
    returning
      value(STOP) type XFELD .
  methods MAKE_STEP .
  methods HNDL_SCRIPT_BUTTONS
    importing
      !I_STACK_CHANGED type XFELD
    returning
      value(STOP) type XFELD .
  methods GET_OBJ_INDEX
    importing
      !I_NAME type ANY
    returning
      value(E_INDEX) type STRING .
  methods CREATE_REFERENCE
    importing
      !I_NAME type STRING
      !I_TYPE type STRING
      !I_SHORTNAME type STRING optional
      !I_QUICK type TPDA_SCR_QUICK_INFO
      !I_PARENT type STRING optional
    returning
      value(E_ROOT_KEY) type SALV_DE_NODE_KEY .
  methods SHOW_STEP .

  methods PROLOGUE
    redefinition .
  methods INIT
    redefinition .
  methods SCRIPT
    redefinition .
  methods END
    redefinition .
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

  methods TRANSFER_VARIABLE
    importing
      !I_NAME type STRING
      !I_TYPE type STRING
      !I_SHORTNAME type STRING optional
      !I_VALUE type STRING optional
      !I_PARENT_CALCULATED type STRING optional
      !I_CL_LEAF type INT4 optional
      !I_INSTANCE type STRING optional .
  methods CREATE_SIMPLE_VAR
    importing
      !I_NAME type STRING
    returning
      value(ER_VAR) type ref to DATA .
  methods CREATE_SIMPLE_STRING
    importing
      !I_NAME type STRING
    returning
      value(E_STRING) type STRING .
  methods CREATE_STRUC
    importing
      !I_NAME type STRING
    returning
      value(ER_STRUC) type ref to DATA
    exceptions
      TYPE_NOT_FOUND .
  methods CREATE_STRUC2
    importing
      !I_NAME type STRING
      !I_SHORTNAME type STRING optional .
  methods GET_CLASS_NAME
    importing
      !I_NAME type STRING
    returning
      value(E_NAME) type STRING .
  methods GET_DEEP_STRUC
    importing
      !I_NAME type STRING
      !R_OBJ type ref to DATA .
  methods GET_TABLE
    importing
      !I_NAME type STRING
    changing
      !C_OBJ type ref to DATA .
  methods READ_CLASS_GLOBALS .
  methods TRAVERSE
    importing
      !IO_TYPE_DESCR type ref to CL_ABAP_TYPEDESCR
      !I_NAME type CLIKE
      !I_FULLNAME type STRING optional
      !I_TYPE type STRING
      !IR_UP type ref to DATA optional
      !I_PARENT_CALCULATED type STRING optional
      !I_STRUC_NAME type STRING optional
      !I_INSTANCE type STRING optional
      !I_CL_LEAF type INT4
      !I_REF type XFELD optional
      !I_SUFFIX type STRING optional .
  methods TRAVERSE_STRUCT
    importing
      !IO_TYPE_DESCR type ref to CL_ABAP_TYPEDESCR
      !I_NAME type CLIKE
      !I_FULLNAME type STRING optional
      !I_TYPE type STRING
      !I_CL_LEAF type INT4
      !IR_UP type ref to DATA optional
      !I_PARENT_CALCULATED type STRING optional
      !I_STRUC_NAME type STRING optional
      !I_INSTANCE type STRING optional
      !I_SUFFIX type STRING optional .
  methods TRAVERSE_ELEM
    importing
      !I_NAME type CLIKE
      !I_FULLNAME type STRING optional
      !I_TYPE type STRING
      !I_VALUE type ANY optional
      !IR_UP type ref to DATA optional
      !I_PARENT_CALCULATED type STRING optional
      !I_CL_LEAF type INT4
      !I_INSTANCE type STRING optional .
ENDCLASS.



CLASS ZCL_SMD_DEBUGGER_SCRIPT IMPLEMENTATION.


  method CREATE_REFERENCE.


    DATA: obj        LIKE LINE OF mt_obj,
          lr_struc   TYPE REF TO data,
          o_object   TYPE REF TO cl_tpda_script_objectdescr,
          o_descr    TYPE REF TO cl_tpda_script_data_descr,
          attributes TYPE tpda_script_object_attribut_it.

    FIELD-SYMBOLS: <symobjref> TYPE tpda_sys_symbobjref.
    ASSIGN i_quick-quickdata->* TO <symobjref>.
    IF <symobjref>-instancename <> '{O:initial}'.

      obj-name = i_name.
      obj-obj = <symobjref>-instancename.
      COLLECT obj INTO mt_obj.

      TRY.
          o_descr = cl_tpda_script_data_descr=>factory( <symobjref>-instancename ).
          o_object ?= o_descr.

          attributes = o_object->attributes( ).
          DELETE attributes WHERE instantiation = 1.
          SORT attributes BY acckind name.

          DATA(name) = o_object->classname( ).
          DATA(obj_ind) =  get_obj_index( <symobjref>-instancename ).

          READ TABLE mt_classes_types WITH KEY full = obj_ind TRANSPORTING NO FIELDS.
          IF sy-subrc NE 0.
            LOOP AT attributes ASSIGNING FIELD-SYMBOL(<attribute>).
              AT NEW acckind.
                APPEND INITIAL LINE TO mt_classes_types ASSIGNING FIELD-SYMBOL(<cl_type>).
                <cl_type>-name = i_name.
                <cl_type>-full = obj_ind.
                <cl_type>-type = <attribute>-acckind.
              ENDAT.
            ENDLOOP.
          ENDIF.

          DATA: parent TYPE string.
          IF i_parent IS NOT INITIAL.
            parent = |{ i_parent }-{ i_name }|.
          ELSE.
            parent = i_name.
          ENDIF.

          LOOP AT attributes ASSIGNING <attribute>.

            transfer_variable( EXPORTING i_name        = |{ <symobjref>-instancename  }-{ <attribute>-name }|
                                         i_shortname   = <attribute>-name
                                         i_type       = i_type
                                         i_instance    = <symobjref>-instancename
                                         i_cl_leaf     = <attribute>-acckind
                                         i_parent_calculated = parent ).

            READ TABLE mt_state WITH KEY path = |{ parent  }-{ <attribute>-name }| ASSIGNING FIELD-SYMBOL(<state>).
            IF sy-subrc = 0.
              <state>-cl_leaf = <attribute>-acckind.
            ENDIF.
          ENDLOOP.
        CATCH cx_tpda_varname.
      ENDTRY.
    ENDIF.


  endmethod.


  method CREATE_SIMPLE_STRING.


    DATA: lr_string TYPE REF TO tpda_sys_symbstring,
          lr_struc  TYPE REF TO data,
          o_elem    TYPE REF TO cl_abap_elemdescr,
          depth     TYPE i VALUE 0.

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


  endmethod.


  method CREATE_SIMPLE_VAR.


    DATA: lr_symbsimple TYPE REF TO tpda_sys_symbsimple,
          o_elem        TYPE REF TO cl_abap_elemdescr.

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
        p_descr_ref    = DATA(o_type)
      EXCEPTIONS
        type_not_found = 1.

    o_elem ?= o_type.
    CREATE DATA er_var TYPE HANDLE o_elem.
    ASSIGN er_var->* TO FIELD-SYMBOL(<new_elem>).
    <new_elem> = <simple>-valstring.


  endmethod.


  method CREATE_STRUC.


    DATA: o_new_type    TYPE REF TO cl_abap_structdescr,
          comp_descr    TYPE abap_componentdescr,
          components    TYPE abap_component_tab,
          lr_symbsimple TYPE REF TO tpda_sys_symbsimple,
          o_struc_descr TYPE REF TO cl_tpda_script_structdescr,
          comp_full     TYPE  tpda_scr_struct_comp_it,
          comp_it       TYPE tpda_script_struc_componentsit.

    FIELD-SYMBOLS: <lv_value> TYPE any,
                   <simple>   TYPE tpda_sys_symbsimple.

    CLEAR er_struc.
    CALL METHOD cl_tpda_script_data_descr=>get_quick_info
      EXPORTING
        p_var_name   = i_name
      RECEIVING
        p_symb_quick = DATA(quick).

    o_struc_descr ?= cl_tpda_script_data_descr=>factory( i_name ).
    o_struc_descr->components( IMPORTING p_components_it = comp_it p_components_full_it = comp_full ).

    zcl_smd_rtti=>create_struc_handle( EXPORTING i_tname = CONV #( replace( val = quick-abstypename sub = '/TYPE=' with = '' ) ) IMPORTING e_handle = o_new_type ).
    IF o_new_type IS NOT INITIAL.
      CREATE DATA er_struc TYPE HANDLE o_new_type.
    ELSE.

      LOOP AT comp_full INTO DATA(comp).
        comp_descr-name = comp-compname.

        ASSIGN comp-symbquick-quickdata TO <lv_value>.
        lr_symbsimple ?= <lv_value>.
        ASSIGN lr_symbsimple->* TO <simple>.

        CALL METHOD cl_tpda_script_data_descr=>get_quick_info
          EXPORTING
            p_var_name   = |{ i_name }-{ comp-compname }|
          RECEIVING
            p_symb_quick = DATA(quick_sub).

        CALL METHOD cl_abap_complexdescr=>describe_by_name
          EXPORTING
            p_name         = quick_sub-abstypename
          RECEIVING
            p_descr_ref    = DATA(o_type)
          EXCEPTIONS
            type_not_found = 1.

        IF sy-subrc = 0.
          comp_descr-type ?= o_type.
          APPEND comp_descr TO components.
        ENDIF.
      ENDLOOP.
      o_new_type  = cl_abap_structdescr=>create( components ).
      CREATE DATA er_struc TYPE HANDLE o_new_type.
    ENDIF.

    ASSIGN er_struc->* TO FIELD-SYMBOL(<new_struc>).

    LOOP AT comp_full INTO comp.
      ASSIGN comp-symbquick-quickdata TO <lv_value>.
      lr_symbsimple ?= <lv_value>.
      ASSIGN COMPONENT comp-compname OF STRUCTURE <new_struc> TO FIELD-SYMBOL(<new>).
      <new> = lr_symbsimple->valstring.
    ENDLOOP.


  endmethod.


  method CREATE_STRUC2.


    DATA: o_struc_descr TYPE REF TO cl_tpda_script_structdescr,
          components    TYPE abap_component_tab,
          comp_full     TYPE  tpda_scr_struct_comp_it,
          comp_descr    TYPE abap_componentdescr,
          comp_it       TYPE tpda_script_struc_componentsit,
          structdescr   TYPE REF TO cl_abap_structdescr,
          r_data        TYPE REF TO data.

    FIELD-SYMBOLS: <str> TYPE any.

    o_struc_descr ?= cl_tpda_script_data_descr=>factory( i_name ).
    o_struc_descr->components( IMPORTING p_components_it = comp_it p_components_full_it = comp_full ).

    LOOP AT comp_it INTO DATA(comp).
      comp_descr-name = comp-compname.
      IF comp-typid = 'u'.
        r_data = create_struc( EXPORTING i_name = |{ comp-longname }| ).
      ELSE.
        r_data = create_simple_var( EXPORTING i_name = |{ comp-longname }| ).
      ENDIF.
      ASSIGN r_data->* TO FIELD-SYMBOL(<item>).

      CALL METHOD cl_abap_complexdescr=>describe_by_data
        EXPORTING
          p_data      = <item>
        RECEIVING
          p_descr_ref = DATA(o_type).

      comp_descr-type ?= o_type.
      APPEND comp_descr TO components.
    ENDLOOP.

    structdescr = cl_abap_structdescr=>create( components ).
    CREATE DATA r_data TYPE HANDLE structdescr.
    ASSIGN r_data->* TO <str>.

    get_deep_struc( EXPORTING i_name = i_name r_obj = r_data ).
    ASSIGN r_data->* TO FIELD-SYMBOL(<new_deep>).


  endmethod.


  method END.
 "dummy method

  endmethod.


  method F5.


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
      stop = abap_true.
      m_is_find = abap_true.
    ENDIF.
    IF m_counter >= 50000."very deep history - to stop
      CLEAR m_counter.
      stop = abap_true.
    ENDIF.

    IF m_counter MOD 10000 = 0.
      show_step( ).
    ENDIF.

    IF mo_window->m_debug_button = 'F5'.
      stop = abap_true.
    ENDIF.


  endmethod.


  method F6.


    TRY.
        CALL METHOD debugger_controller->debug_step
          EXPORTING
            p_command = cl_tpda_script_debugger_ctrl=>debug_step_over.
        is_step = abap_true.
      CATCH cx_tpda_scr_rtctrl_status .
      CATCH cx_tpda_scr_rtctrl .
    ENDTRY.


  endmethod.


  method F7.


    TRY.
        CALL METHOD debugger_controller->debug_step
          EXPORTING
            p_command = cl_tpda_script_debugger_ctrl=>debug_step_out.
        is_step = abap_true.
      CATCH cx_tpda_scr_rtctrl_status .
      CATCH cx_tpda_scr_rtctrl .
    ENDTRY.


  endmethod.


  method F8.


    TRY.
        CALL METHOD debugger_controller->debug_step
          EXPORTING
            p_command = cl_tpda_script_debugger_ctrl=>debug_continue.
        is_step = abap_true.
      CATCH cx_tpda_scr_rtctrl_status .
      CATCH cx_tpda_scr_rtctrl .
    ENDTRY.


  endmethod.


  method GET_CLASS_NAME.


    DATA: o_object TYPE REF TO cl_tpda_script_objectdescr,
          o_descr  TYPE REF TO cl_tpda_script_data_descr.

    FIELD-SYMBOLS: <symobjref> TYPE tpda_sys_symbobjref.

    TRY.
        CALL METHOD cl_tpda_script_data_descr=>get_quick_info
          EXPORTING
            p_var_name   = i_name
          RECEIVING
            p_symb_quick = DATA(quick).

        ASSIGN quick-quickdata->* TO <symobjref>.
        IF <symobjref>-instancename <> '{O:initial}'.

          o_descr = cl_tpda_script_data_descr=>factory( <symobjref>-instancename ).
          o_object ?= o_descr.

          e_name = o_object->classname( ).
        ENDIF.
      CATCH cx_tpda_varname .
    ENDTRY.


  endmethod.


  method GET_DEEP_STRUC.


    DATA: lr_struc      TYPE REF TO data,
          o_struc_descr TYPE REF TO cl_tpda_script_structdescr,
          comp_full     TYPE  tpda_scr_struct_comp_it,
          comp_it       TYPE tpda_script_struc_componentsit,
          lr_symbsimple TYPE REF TO tpda_sys_symbsimple,
          lr_symbstring TYPE REF TO tpda_sys_symbstring,
          lr_symbstruc  TYPE REF TO tpda_sys_symbstruct,
          r_data        TYPE REF TO data.

    FIELD-SYMBOLS: <lv_value> TYPE any.

    ASSIGN r_obj->* TO FIELD-SYMBOL(<new_deep>).
    o_struc_descr ?= cl_tpda_script_data_descr=>factory( i_name ).
    o_struc_descr->components( IMPORTING p_components_it = comp_it p_components_full_it = comp_full ).

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


  endmethod.


  method GET_OBJ_INDEX.


    FIND FIRST OCCURRENCE OF '*' IN i_name MATCH OFFSET DATA(offset).
    e_index =  i_name+0(offset).


  endmethod.


  method GET_TABLE.
 "construct deep tables

    DATA: r_data        TYPE REF TO data,
          o_table_descr TYPE REF TO cl_tpda_script_tabledescr,
          table_clone   TYPE REF TO data,
          o_tabl        TYPE REF TO cl_abap_tabledescr,
          o_struc       TYPE REF TO cl_abap_structdescr,
          r_struc       TYPE REF TO data.

    FIELD-SYMBOLS: <f>         TYPE ANY TABLE,
                   <new_table> TYPE ANY TABLE.

    ASSIGN c_obj->* TO <new_table>.
    o_tabl ?= cl_abap_typedescr=>describe_by_data( <new_table> ).

    TRY.
        o_struc ?= o_tabl->get_table_line_type( ).


        CREATE DATA r_data TYPE HANDLE o_struc.
        ASSIGN r_data->* TO FIELD-SYMBOL(<new_line>).

        o_table_descr ?= cl_tpda_script_data_descr=>factory( i_name ).
        table_clone = o_table_descr->elem_clone( ).
        ASSIGN table_clone->* TO <f>.
        DATA: count TYPE i.

        LOOP AT <f> ASSIGNING FIELD-SYMBOL(<fs>).
          count = sy-tabix.
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
              o_struc ?= cl_abap_typedescr=>describe_by_data( <fs> ).
              READ TABLE o_struc->components INDEX sy-index INTO DATA(comp_descr).
              GET REFERENCE OF <to> INTO r_data.
              get_table( EXPORTING i_name = |{ i_name }[ { count } ]-{ comp_descr-name }|
                         CHANGING  c_obj  = r_data ).
            ENDIF.
          ENDDO.
          INSERT <new_line> INTO TABLE <new_table>.
        ENDLOOP.
      CATCH cx_root.
        "DATA(cnt) = o_table_descr->linecnt( ).
        "DO cnt TIMES.
        "  r_struc = create_struc( i_name = |{ i_name }[{ sy-index }]| ).
        "  ASSIGN r_struc->* TO <new_line>.
        "  INSERT <new_line> INTO TABLE <new_table>.
        "ENDDO.
    ENDTRY.


  endmethod.


  method HNDL_SCRIPT_BUTTONS.


    IF m_is_find = abap_true.
      stop = abap_true.
      CLEAR m_is_find.
      RETURN.
    ENDIF.

    IF mo_window->m_debug_button = 'F5'.
      stop = abap_true.

    ELSEIF mo_window->m_debug_button = 'F6'.
      IF m_f6_level IS NOT INITIAL AND m_f6_level = ms_stack-stacklevel OR mo_window->m_history IS INITIAL.
        CLEAR m_f6_level.
        stop = abap_true.
      ENDIF.

    ELSEIF mo_window->m_debug_button = 'F6END'.
      IF mo_window->m_prg-flag_eoev IS NOT INITIAL AND m_target_stack = ms_stack-stacklevel.
        stop = abap_true.
      ENDIF.
    ELSEIF mo_window->m_debug_button = 'F7'.

      IF m_target_stack = ms_stack-stacklevel.
        CLEAR m_target_stack.
        stop = abap_true.
      ENDIF.

    ELSEIF mo_window->m_debug_button IS NOT INITIAL.
      READ TABLE mo_window->mt_breaks WITH KEY inclnamesrc = mo_window->m_prg-include linesrc = mo_window->m_prg-line INTO DATA(gs_break).
      IF sy-subrc = 0.
        stop = abap_true.
      ELSE.

        IF mo_window->m_debug_button = 'F6BEG' AND m_target_stack = ms_stack-stacklevel.
          stop = abap_true.
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
      stop = abap_true.
    ENDIF.


  endmethod.


  method INIT.


    CONSTANTS: c_mask TYPE x VALUE '01'.

    is_step = abap_on.
    zcl_smd_appl=>check_mermaid( ).
    zcl_smd_appl=>init_lang( ).
    zcl_smd_appl=>init_icons_table( ).

    mo_window = NEW Zcl_smd_window( me ).

    mo_tree_imp = NEW zcl_smd_rtti_tree( i_header   = 'Importing parameters'
                                     i_type     = 'I'
                                     i_cont     = mo_window->mo_importing_container
                                     i_debugger = me ).

    mo_tree_local = NEW zcl_smd_rtti_tree( i_header   = 'Variables'
                                       i_type     = 'L'
                                       i_cont     = mo_window->mo_locals_container
                                       i_debugger = me ).

    mo_tree_exp = NEW zcl_smd_rtti_tree( i_header   = 'Exporting & Returning parameters'
                                     i_type     = 'E'
                                     i_cont     = mo_window->mo_exporting_container
                                     i_debugger = me ).

    mo_tree_local->m_locals = mo_tree_local->m_locals BIT-XOR c_mask.


  endmethod.


  method MAKE_STEP.


    DATA: stop TYPE xfeld.

    READ TABLE mo_window->mt_stack INDEX 1 INTO DATA(stack).
    IF mo_window->m_debug_button = 'F6' AND mo_window->m_history IS NOT INITIAL.
      m_f6_level = stack-stacklevel.
    ENDIF.

    WHILE stop IS INITIAL.

      CASE mo_window->m_debug_button.

        WHEN 'F5' OR 'F6END' OR 'F6BEG'.
          stop = f5( ).
        WHEN 'F6'.
          IF mo_window->m_history IS INITIAL.
            stop = f6( ).
          ELSE.
            stop = f5( ).
          ENDIF.

        WHEN 'F7'.
          IF mo_window->m_history IS INITIAL.
            stop = f7( ).
          ELSE.
            stop = f5( ).
          ENDIF.

        WHEN 'F8'.
          IF mo_window->m_history IS INITIAL.
            stop = f8( ).
          ELSE.

            IF stack-stacklevel = mo_window->m_start_stack + mo_window->m_hist_depth.
              stop = f6( ).
            ELSE.
              stop = f5( ).
            ENDIF.
          ENDIF.

      ENDCASE.
      run_script( ).
      stop = hndl_script_buttons( mv_stack_changed ).
      READ TABLE mo_window->mt_stack INDEX 1 INTO stack.

    ENDWHILE.
    show_step( ).
    me->break( ).


  endmethod.


  method PROLOGUE.

    super->prologue( ).

  endmethod.


  method READ_CLASS_GLOBALS.


    DATA: compo_tmp TYPE TABLE OF scompo,
          class     TYPE seu_name.

    mo_tree_local->m_leaf = 'Class-data global variables'.
    IF mo_tree_local->m_class_data IS NOT INITIAL.

      "global classes
      CALL METHOD cl_tpda_script_abapdescr=>get_loaded_programs
        IMPORTING
          p_progs_it = DATA(progs).

      DELETE progs WHERE sys = abap_true.

      LOOP AT progs INTO DATA(prog) WHERE name+30(2) = 'CP' AND ( name+0(1) = 'Z' OR name+0(1) = 'Y' ) .

        CLEAR prog-name+30(2).
        REPLACE ALL OCCURRENCES OF '=' IN prog-name WITH ''.

        DATA refc TYPE REF TO cl_abap_objectdescr.
        CALL METHOD cl_abap_classdescr=>describe_by_name
          EXPORTING
            p_name         = prog-name
          RECEIVING
            p_descr_ref    = DATA(ref)
          EXCEPTIONS
            type_not_found = 1
            OTHERS         = 2.

        refc ?= ref.

        READ TABLE mt_obj WITH KEY name = class TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
          APPEND INITIAL LINE TO mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
          <obj>-name = class.
        ENDIF.

        save_hist( EXPORTING i_fullname    = CONV #( prog-name )
                             i_name        = CONV #( prog-name )
                             i_parent_calculated = ''
                             i_type        = 'CLASS'
                             i_cl_leaf     = 0
                             i_instance     = CONV #( prog-name ) ).


        LOOP AT refc->attributes INTO DATA(atr).
          transfer_variable( EXPORTING i_name =  CONV #( |{ prog-name }=>{ atr-name }| )
                             i_shortname = CONV #( atr-name )
                             i_parent_calculated = CONV #( prog-name )
                              i_type = 'CLASS' ).
        ENDLOOP.
      ENDLOOP.

    ENDIF.

    compo_tmp = mt_compo.
    DELETE compo_tmp WHERE  type NE 'D'.


  endmethod.


  method RUN_SCRIPT.


    DATA: type TYPE string.
    ADD 1 TO m_counter.
    TRY.
        cl_tpda_script_abapdescr=>get_abap_src_info( IMPORTING p_prg_info = mo_window->m_prg ).
        DATA(stack) = cl_tpda_script_abapdescr=>get_abap_stack( ).
        READ TABLE mo_window->mt_stack INDEX 1 INTO ms_stack_prev.

        MOVE-CORRESPONDING stack TO mo_window->mt_stack.
        READ TABLE mo_window->mt_stack INDEX 1 ASSIGNING FIELD-SYMBOL(<stack>).
        <stack>-step = m_step.
        ms_stack = <stack>.

        CALL METHOD cl_tpda_script_bp_services=>get_all_bps RECEIVING p_bps_it = mo_window->mt_breaks.

        IF is_step = abap_true.
          ADD 1 TO m_step.
          m_hist_step = m_step.
          GET TIME.
          "add missed ELSE/ENDIF/ENDCASE
          IF m_step > 2.
            READ TABLE mt_steps INDEX m_step - 1 INTO DATA(one_step).

            IF ms_stack-line > one_step-line.
              READ TABLE mo_window->mt_source WITH KEY include = ms_stack-include INTO DATA(source).
              READ TABLE source-t_keytokens WITH KEY line = ms_stack-line INTO DATA(key).
              READ TABLE source-t_keytokens INDEX sy-tabix - 1 INTO key.
              READ TABLE source-t_keytokens WITH KEY line = one_step-line INTO DATA(key_prev).

              IF key_prev-name <> 'DO' AND key_prev-name <> 'LOOP' AND key_prev-name <> 'WHILE'.

                IF key-name = 'ELSE' OR key-name = 'ENDIF' OR key-name = 'ENDCASE'.

                  APPEND INITIAL LINE TO mt_steps ASSIGNING FIELD-SYMBOL(<step>).
                  MOVE-CORRESPONDING ms_stack TO <step>.
                  <step>-line = key-line.
                  <step>-step = m_step.
                  ADD 1 TO m_step.
                ENDIF.
              ENDIF.
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

          DATA: step TYPE i.
          step = m_step - 1.
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
      DATA: optimize TYPE xfeld.
      READ TABLE mo_window->mt_source WITH KEY include = ms_stack_prev-include INTO source.
      IF sy-subrc = 0.
        READ TABLE source-t_keytokens WITH KEY line = ms_stack_prev-line INTO DATA(oper).
        IF mv_stack_changed IS INITIAL.
          IF oper-name = 'COMPUTE' OR oper-name = 'SELECT' OR oper-name = 'CLEAR' OR  oper-name = 'LOOP' OR oper-name = 'SORT'
             OR oper-name = 'DELETE' OR oper-name = 'READ' OR  oper-name = 'CONCATENATE' OR oper-name = 'CONDENSE'
             OR oper-name = 'APPEND' OR oper-name = 'MODIFY' OR  oper-name = 'CREATE' OR oper-name = 'SHIFT'
             OR oper-name = 'ASSIGN' OR oper-name = 'UNASSIGN' OR oper-name = 'TRANSLATE' OR  oper-name = 'REPLACE'
             OR  oper-name = 'ADD' OR  oper-name = 'SUBTRACT'.
            optimize = abap_true.

            IF oper-name = 'UNASSIGN'.
              mo_tree_local->clear( ).
              mo_tree_exp->clear( ).
              mo_tree_imp->clear( ).
              DELETE mt_state WHERE leaf NE 'GLOBAL'.
            ENDIF.

          ENDIF.
        ENDIF.
      ELSE.
        zcl_smd_source_parser=>parse_tokens( i_program = mo_window->m_prg-include io_debugger = me ).
        READ TABLE mo_window->mt_source WITH KEY include = ms_stack-include INTO source.
      ENDIF.
    ENDIF.

    IF mo_window->m_varhist IS NOT INITIAL.
      IF mo_tree_local->m_globals IS NOT INITIAL AND mo_tree_local->m_ldb IS NOT INITIAL.
        DATA: name(40),
              inc       TYPE TABLE OF  d010inc.
        name = abap_source->program( ).

        CALL FUNCTION 'RS_PROGRAM_INDEX'
          EXPORTING
            pg_name      = name
          TABLES
            compo        = mt_compo
            inc          = inc
          EXCEPTIONS
            syntax_error = 1
            OTHERS       = 2.
      ENDIF.

      IF mv_stack_changed = abap_true.
        IF mo_tree_local->m_locals IS NOT INITIAL.
          READ TABLE mo_window->mt_locals_set WITH KEY program = ms_stack-program
                                                       eventname = ms_stack-eventname
                                                       eventtype = ms_stack-eventtype
             INTO DATA(local_set).

          IF sy-subrc = 0 AND local_set-loc_fill = abap_true.
            mt_locals = local_set-locals_tab.
          ELSE.

            CALL METHOD cl_tpda_script_data_descr=>locals RECEIVING p_locals_it = mt_locals.

            IF ms_stack-eventtype = 'METHOD'.
              APPEND INITIAL LINE TO mt_locals ASSIGNING FIELD-SYMBOL(<loc>).
              <loc>-name = 'ME'.
            ENDIF.
            IF ms_stack-eventtype = 'FUNCTION'.
              DATA: fname              TYPE rs38l_fnam,
                    exception_list     TYPE TABLE OF  rsexc,
                    export_parameter   TYPE TABLE OF  rsexp,
                    import_parameter   TYPE TABLE OF  rsimp,
                    changing_parameter TYPE TABLE OF    rscha,
                    tables_parameter   TYPE TABLE OF    rstbl.

              fname = ms_stack-eventname.
              CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE'
                EXPORTING
                  funcname           = fname
                TABLES
                  exception_list     = exception_list
                  export_parameter   = export_parameter
                  import_parameter   = import_parameter
                  changing_parameter = changing_parameter
                  tables_parameter   = tables_parameter
                EXCEPTIONS
                  error_message      = 1
                  function_not_found = 2
                  invalid_name       = 3
                  OTHERS             = 4.
              IF sy-subrc = 0.
                LOOP AT export_parameter INTO DATA(exp).
                  APPEND INITIAL LINE TO mt_locals ASSIGNING <loc>.
                  <loc>-name = exp-parameter.
                  <loc>-parkind = 2.
                ENDLOOP.
                LOOP AT import_parameter INTO DATA(imp).
                  APPEND INITIAL LINE TO mt_locals ASSIGNING <loc>.
                  <loc>-name = imp-parameter.
                  <loc>-parkind = 1.
                ENDLOOP.
                LOOP AT changing_parameter INTO DATA(change).
                  APPEND INITIAL LINE TO mt_locals ASSIGNING <loc>.
                  <loc>-name = change-parameter.
                  <loc>-parkind = 2.
                ENDLOOP.
                LOOP AT tables_parameter INTO DATA(table).
                  APPEND INITIAL LINE TO mt_locals ASSIGNING <loc>.
                  <loc>-name = table-parameter.
                  <loc>-parkind = 2.
                ENDLOOP.
              ENDIF.
            ENDIF.

            IF mo_window->m_prg-event-eventtype = 'FORM'.
              LOOP AT source-t_params INTO DATA(params) WHERE name = mo_window->m_prg-event-eventname AND class IS INITIAL.
                READ TABLE mt_locals WITH KEY name = params-param ASSIGNING FIELD-SYMBOL(<local>).
                IF sy-subrc = 0.
                  IF params-type = 'I'.
                    <local>-parkind = '1'.
                  ELSEIF params-type = 'E'.
                    <local>-parkind = '2'.
                  ENDIF.
                ENDIF.
              ENDLOOP.
              "get_form_parameters( i_prg = mo_window->m_prg i_form = ms_stack-eventname ).
            ENDIF.

            SORT mt_locals.

            local_set-program = ms_stack-program.
            local_set-eventname = ms_stack-eventname.
            local_set-eventtype = ms_stack-eventtype.
            local_set-loc_fill = abap_true.
            local_set-locals_tab = mt_locals.
            APPEND local_set TO mo_window->mt_locals_set.
          ENDIF.
        ENDIF.

        IF ( mo_tree_local->m_globals IS NOT INITIAL OR  mo_tree_local->m_ldb IS NOT INITIAL ) AND ms_stack_prev-program <> ms_stack-program.

          READ TABLE mo_window->mt_globals_set WITH KEY program = ms_stack-program INTO DATA(global_set).

          IF sy-subrc = 0 AND global_set-glob_fill = abap_true.
            mt_globals = global_set-globals_tab.
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
            global_set-program = ms_stack-program.
            global_set-globals_tab = mt_globals.
            global_set-glob_fill = abap_true.
            APPEND global_set TO mo_window->mt_globals_set.
          ENDIF.
        ENDIF.

        IF mo_tree_local->m_class_data IS NOT INITIAL.
          read_class_globals( ).
        ENDIF.

      ENDIF.

      DATA: lr_names TYPE RANGE OF string,
            temp     TYPE char30.

      DATA(globals) = mt_globals.
      DATA(locals) = mt_locals.

      IF optimize = abap_true AND m_update IS INITIAL.

        LOOP AT source-t_calculated INTO DATA(param) WHERE line = ms_stack_prev-line.
          temp = param-name.
          lr_names = VALUE #( BASE lr_names ( sign = 'I' option = 'EQ' low = temp ) ).
        ENDLOOP.

        DELETE lr_names WHERE low = 'CORRESPONDING'. "to refactor
        IF sy-subrc = 0.
          DELETE globals WHERE name NOT IN lr_names.
          DELETE locals WHERE name NOT IN lr_names.
        ENDIF.

      ENDIF.

      IF mo_tree_local->m_locals IS NOT INITIAL.
        "BREAK-POINT.
        LOOP AT locals INTO DATA(local).

          CASE local-parkind.
            WHEN 0.
              type = 'LOCAL'.
            WHEN 1.
              type = 'IMP'.
            WHEN OTHERS.
              type = 'EXP'.
          ENDCASE.

          transfer_variable( EXPORTING i_name = local-name i_type = type ).
        ENDLOOP.

        READ TABLE mo_window->mt_locals_set
         WITH KEY program = ms_stack-program eventtype = ms_stack-eventtype eventname = ms_stack-eventname
         INTO DATA(locals_set).
        LOOP AT locals_set-mt_fs INTO DATA(fs).
          transfer_variable( EXPORTING i_name = fs-name i_type = 'LOCAL' ).
        ENDLOOP.
      ENDIF.

      IF mo_tree_local->m_globals IS NOT INITIAL.

        LOOP AT globals INTO DATA(global)  WHERE parisval NE 'L'.
          transfer_variable( EXPORTING i_name = global-name i_type = 'GLOBAL' ).
        ENDLOOP.
        READ TABLE mo_window->mt_globals_set WITH KEY program = ms_stack-program INTO DATA(globals_set).
        LOOP AT globals_set-mt_fs INTO fs.
          transfer_variable( EXPORTING i_name = fs-name i_type = 'GLOBAL' ).
        ENDLOOP.

      ENDIF.
    ENDIF.

    IF mo_tree_local->m_syst IS NOT INITIAL.
      transfer_variable( EXPORTING i_name = 'SYST' i_type = 'SYST' ).
    ELSE.
      DELETE mo_tree_local->mt_vars WHERE leaf = 'SYST'.
      DELETE mt_state WHERE leaf = 'SYST'.
    ENDIF.

    IF mo_tree_local->m_ldb IS NOT INITIAL.
      LOOP AT globals INTO global WHERE parisval = 'L'.
        transfer_variable( EXPORTING i_name = global-name i_type = 'LDB' ).
      ENDLOOP.
    ENDIF.

    LOOP AT mt_state ASSIGNING FIELD-SYMBOL(<state>).
      CLEAR <state>-done.
    ENDLOOP.

    "check dependents variables.
    IF mt_selected_var IS NOT INITIAL.
      READ TABLE source-t_keytokens WITH KEY line = ms_stack_prev-line INTO DATA(keyword).
      LOOP AT keyword-tt_calls INTO DATA(call) WHERE event = 'FORM' AND name =  ms_stack-eventname.
        READ TABLE mt_selected_var WITH KEY name = call-outer TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO mt_selected_var ASSIGNING FIELD-SYMBOL(<selected>).
          <selected>-name = call-inner.
        ENDIF.
      ENDLOOP.
    ENDIF.

    CLEAR: mo_window->m_show_step.
    mo_tree_imp->m_prg_info = mo_window->m_prg.


  endmethod.


  method RUN_SCRIPT_HIST.


    DATA: vars_history LIKE mt_vars_hist_view,
          hist_step    TYPE i,
          old_step     TYPE i.

    CLEAR mv_recurse.
    is_history = abap_true.
    IF m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.
    IF i_step IS NOT INITIAL.
      hist_step = i_step.
      READ TABLE mt_steps WITH KEY step = i_step INTO DATA(steps).

    ELSE.
      IF ( mo_window->m_debug_button = 'F6BEG' OR mo_window->m_debug_button = 'F6END' ) AND m_target_stack IS INITIAL.
        READ TABLE mt_steps INTO steps INDEX m_hist_step.
        m_target_stack = steps-stacklevel.
      ENDIF.

      IF mo_window->m_direction IS NOT INITIAL AND m_hist_step = 1 AND mo_window->m_debug_button IS NOT INITIAL.
        es_stop = abap_true.
      ENDIF.

      IF mo_window->m_direction IS INITIAL AND m_hist_step = m_step AND mo_window->m_debug_button IS NOT INITIAL.
        es_stop = abap_true.
      ENDIF.

      old_step = m_hist_step.
      IF mo_window->m_direction IS NOT INITIAL AND m_hist_step > 1 AND mo_window->m_debug_button IS NOT INITIAL.
        SUBTRACT 1 FROM m_hist_step.
      ENDIF.

      IF mo_window->m_direction IS INITIAL AND m_hist_step < m_step AND mo_window->m_debug_button IS NOT INITIAL.
        ADD 1  TO m_hist_step.
      ENDIF.

      hist_step = m_hist_step.

      READ TABLE mt_steps INTO steps WITH KEY step =  m_hist_step.
      READ TABLE mt_steps INTO DATA(step_old) WITH KEY step =  old_step.

      IF steps-stacklevel <> step_old-stacklevel.
        m_refresh = abap_true.
      ENDIF.

      mo_window->set_program( steps-include ).
      mo_window->set_program_line( steps-line ).

      IF ( mo_window->m_debug_button = 'F6BEG' OR mo_window->m_debug_button = 'F6END' ) AND m_target_stack =  steps-stacklevel.
        CLEAR m_target_stack.
        es_stop = abap_true.
      ENDIF.

      READ TABLE mo_window->mt_stack INTO DATA(stack) INDEX 1.

      MOVE-CORRESPONDING stack TO mo_window->m_prg.

      IF mo_window->m_debug_button = 'F6' AND m_stop_stack IS INITIAL.
        m_stop_stack = stack-stacklevel.
      ENDIF.

    ENDIF.

    IF mo_window->m_debug_button = 'F5'.
      es_stop = abap_true.
    ENDIF.

    IF mo_window->m_debug_button = 'F6' AND m_stop_stack = stack-stacklevel.

      es_stop = abap_true.
      CLEAR m_stop_stack.
    ENDIF.

    IF ( mo_window->m_debug_button = 'F6BEG' AND steps-first = abap_true AND m_target_stack = stack-stacklevel ) OR
       ( mo_window->m_debug_button = 'F6END' AND steps-last = abap_true  AND m_target_stack = stack-stacklevel ).
      CLEAR m_target_stack.
      es_stop = abap_true.
    ENDIF.

    IF i_step IS INITIAL.
      READ TABLE mo_window->mt_breaks WITH KEY inclnamesrc = steps-include linesrc = steps-line INTO DATA(break).
      IF sy-subrc = 0.

        es_stop = abap_true.
      ENDIF.
    ENDIF.

    IF  i_step IS NOT INITIAL.
      es_stop = abap_true.
    ENDIF.

    IF es_stop = abap_true.

      "history state find refactoring
      DATA(vars_hist) = mt_vars_hist.
      SORT vars_hist BY step ASCENDING first DESCENDING.

      CLEAR vars_history.

      LOOP AT mt_steps INTO DATA(hist_steps) WHERE step <= hist_step.
        IF hist_steps-stacklevel < steps-stacklevel.
          DELETE vars_history WHERE leaf = 'LOCAL'.
        ENDIF.
        LOOP AT vars_hist INTO DATA(hist) WHERE step = hist_steps-step.
          IF m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.
          IF  mo_tree_local->m_globals IS INITIAL AND hist-leaf = 'GLOBAL' OR hist-program <> steps-program.
            CONTINUE.
          ENDIF.
          IF  mo_tree_local->m_class_data IS INITIAL AND hist-leaf = 'CLASS'.
            CONTINUE.
          ENDIF.
          IF ( hist-leaf = 'LOCAL' OR hist-leaf = 'IMP' OR hist-leaf = 'EXP' ) AND hist-stack <> steps-stacklevel.
            CONTINUE.
          ENDIF.

          IF hist-step = hist_step AND hist-first IS INITIAL.
            CONTINUE.
          ENDIF.

          IF hist-del IS INITIAL.
            READ TABLE vars_history WITH KEY name = hist-name ASSIGNING FIELD-SYMBOL(<hist>).
            IF sy-subrc = 0.
              <hist> = hist.
              CLEAR <hist>-done.
            ELSE.
              "check initial.
              IF m_hide IS NOT INITIAL.
                ASSIGN hist-ref->* TO FIELD-SYMBOL(<new>).
                IF <new> IS NOT INITIAL.
                  APPEND INITIAL LINE TO vars_history ASSIGNING <hist>.
                  <hist> = hist.
                  CLEAR <hist>-done.
                ENDIF.
              ELSE.
                APPEND INITIAL LINE TO vars_history ASSIGNING <hist>.
                <hist> = hist.
                CLEAR <hist>-done.
              ENDIF.
            ENDIF.
          ELSE.
            IF m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.
            mo_tree_local->clear( ).
            mo_tree_exp->clear( ).
            mo_tree_imp->clear( ).
          ENDIF.
        ENDLOOP.
      ENDLOOP.
      IF m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.

      SORT vars_history BY name.

      IF step_old-stacklevel <> steps-stacklevel OR m_refresh = abap_true.
        mo_tree_local->clear( ).
        mo_tree_exp->clear( ).
        mo_tree_imp->clear( ).
      ENDIF.


      IF vars_history IS NOT INITIAL.
        show_variables( CHANGING it_var = vars_history ).

        set_selected_vars( ).
        CLEAR m_refresh.
      ENDIF.

    ENDIF.


  endmethod.


  method SAVE_HIST.


    DATA: add        TYPE xfeld,
          add_hist   TYPE xfeld,
          name2(100),
          full_name  TYPE string.

    CHECK m_hist_step = m_step AND mo_window->m_direction IS INITIAL.
    IF ir_up IS SUPPLIED.
      ASSIGN ir_up->* TO FIELD-SYMBOL(<ir_up>).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

    IF i_instance IS INITIAL.
      full_name = i_fullname.
    ELSE.
      IF i_parent_calculated IS INITIAL.
        IF i_name IS NOT INITIAL.
          full_name = i_name.
        ELSE.
          full_name = i_fullname.
        ENDIF.
      ELSE.
        IF i_fullname+0(3) = '{O:'.
          full_name = i_fullname.
        ELSE.
          full_name =  |{ i_parent_calculated }-{ i_name }|.
        ENDIF.
      ENDIF.
    ENDIF.
    IF i_instance IS INITIAL.
      READ TABLE mt_state
           WITH KEY name = full_name
                    program = mo_window->mt_stack[ 1 ]-program
            ASSIGNING FIELD-SYMBOL(<state>).
    ELSE.
      READ TABLE mt_state
          WITH KEY name = full_name
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
      <state>-name = full_name.

      IF i_name IS NOT INITIAL.
        <state>-short = i_name.
      ELSE.
        <state>-short = i_fullname.
      ENDIF.

      <state>-leaf = i_type.
      <state>-is_appear = abap_true.
      <state>-parent = i_parent_calculated.
      <state>-instance = i_instance.

      IF i_parent_calculated IS NOT INITIAL.
        IF i_name IS NOT INITIAL.
          <state>-path =  |{ i_parent_calculated }-{ i_name }|.
        ELSE.
          <state>-path =  |{ i_parent_calculated }-{ i_fullname }|.
        ENDIF.
      ELSE.
        IF i_instance IS INITIAL.
          <state>-path = i_fullname.
        ELSE.
          IF i_parent_calculated IS INITIAL.
            IF i_name IS NOT INITIAL.
              <state>-path = i_name.
            ELSE.
              <state>-path = i_fullname.
            ENDIF.
          ELSE.

            IF i_name IS NOT INITIAL.
              <state>-path =  |{ i_parent_calculated }-{ i_name }|.
            ELSE.
              <state>-path =  |{ i_parent_calculated }-{ i_fullname }|.
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

      DATA(o_elem) = cl_abap_typedescr=>describe_by_data_ref( <state>-ref ).

      name2 = i_fullname.

      IF name2+0(2) NE '{O'.

        IF <state>-leaf NE 'GLOBAL' AND <state>-leaf NE 'CLASS'.
          READ TABLE mt_vars_hist_view
           WITH KEY stack = <state>-stack
                    name = i_fullname
                    eventtype = <state>-eventtype
                    eventname = <state>-eventname
                    INTO DATA(hist).
        ELSE.
          READ TABLE mt_vars_hist_view
           WITH KEY name = i_fullname
                    INTO hist.
        ENDIF.

        IF sy-subrc NE 0.
          add_hist = add = abap_on.
        ELSE.
          ASSIGN hist-ref->* TO FIELD-SYMBOL(<hist>).

          IF <hist> NE <ir_up>.
            add_hist = add = abap_on.
          ENDIF.
        ENDIF.
      ELSE.
        READ TABLE mt_vars_hist_view WITH KEY name = <state>-name INTO hist.
        IF sy-subrc = 0.
          ASSIGN hist-ref->* TO <hist>.
          IF <hist> NE <ir_up>.
            add_hist = add = abap_on.
          ENDIF.
        ELSE.
          add_hist = add = abap_on.
        ENDIF.

      ENDIF.
      IF mv_stack_changed = abap_true.
        add = abap_on.
      ENDIF.

      o_elem = cl_abap_typedescr=>describe_by_data_ref( <state>-ref ).
      IF <state>-type IS INITIAL.
        <state>-type = o_elem->absolute_name.
      ENDIF.

      IF add = abap_on.

        CLEAR <state>-first.

        IF  ms_stack_prev-stacklevel IS INITIAL OR
         ms_stack-stacklevel > ms_stack_prev-stacklevel.
          <state>-first = 'X'.
        ENDIF.

        <state>-cl_leaf = i_cl_leaf.
        INSERT <state> INTO mt_vars_hist_view INDEX 1.

        IF  add_hist = abap_true.
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

      READ TABLE mt_vars_hist WITH KEY name = <state>-name INTO DATA(var_hist).
      IF sy-subrc = 0.
        IF <state>-instance <> var_hist-instance.
          <state>-del = abap_true.
          INSERT <state> INTO mt_vars_hist INDEX 1.
        ENDIF.
      ELSE.
        <state>-first = 'X'.
        INSERT <state> INTO mt_vars_hist INDEX 1.
      ENDIF.
    ENDIF.


  endmethod.


  method SCRIPT.


    run_script( ).
    show_step( ).
    me->break( ).


  endmethod.


  method SET_SELECTED_VARS.


    DATA(nodes) = mo_tree_local->m_tree->get_nodes( )->get_all_nodes( ).
    LOOP AT nodes INTO DATA(node).
      DATA(name) = node-node->get_text( ).
      READ TABLE mt_selected_var WITH KEY name = name TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        node-node->set_row_style( if_salv_c_tree_style=>emphasized_b ).
      ENDIF.
    ENDLOOP.


  endmethod.


  method SHOW_STEP.


    show_variables( CHANGING it_var = mt_state ).
    set_selected_vars( ).
    mo_window->set_program( mo_window->m_prg-include ).
    mo_window->set_program_line( mo_window->m_prg-line ).
    mo_window->show_stack( ).
    mo_tree_imp->display( ).
    mo_tree_local->display( ).
    mo_tree_exp->display( ).
    IF mo_window->m_debug_button NE 'F5'.
      mo_window->m_show_step = abap_true.
    ENDIF.


  endmethod.


  method SHOW_VARIABLES.


    FIELD-SYMBOLS: <hist> TYPE any,
                   <new>  TYPE any.

    DATA: rel     TYPE salv_de_node_relation,
          key     TYPE salv_de_node_key,
          o_tree  TYPE REF TO  zcl_smd_rtti_tree,
          is_skip TYPE xfeld.
    ADD 1 TO mv_recurse.
    IF mo_tree_local->m_clear = abap_true.
      mo_tree_local->clear( ).
      CLEAR mo_tree_local->m_clear.
    ENDIF.

    READ TABLE it_var WITH KEY del = abap_true TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      mo_tree_local->clear( ).
      mo_tree_exp->clear( ).
      mo_tree_imp->clear( ).
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
      READ TABLE mo_tree_local->mt_vars WITH KEY name = 'SYST' INTO DATA(var).
      IF sy-subrc = 0.
        mo_tree_local->delete_node( var-key ).
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

    rel = if_salv_c_node_relation=>last_child.

    LOOP AT it_var ASSIGNING FIELD-SYMBOL(<var>) WHERE done = abap_false.

      CASE <var>-leaf.

        WHEN 'LOCAL'.
          IF mo_tree_local->m_locals IS NOT INITIAL.
            mo_tree_local->m_leaf =  'LOCAL'.
            IF mo_tree_local->m_locals_key IS INITIAL.
              mo_tree_local->add_node( i_name = 'Locals' i_icon = CONV #( icon_life_events ) ).
            ELSE.
              mo_tree_local->main_node_key = mo_tree_local->m_locals_key.
            ENDIF.
          ELSE.
            CONTINUE.
          ENDIF.

        WHEN 'GLOBAL'.
          IF mo_tree_local->m_globals IS NOT INITIAL.
            mo_tree_local->m_leaf =  'GLOBAL'.
            IF mo_tree_local->m_globals_key IS INITIAL.
              mo_tree_local->add_node( i_name = 'Globals' i_icon = CONV #( icon_life_events ) ).
            ELSE.
              mo_tree_local->main_node_key = mo_tree_local->m_globals_key.
            ENDIF.
          ELSE.
            CONTINUE.
          ENDIF.

        WHEN 'LDB'.
          IF mo_tree_local->m_ldb IS NOT INITIAL.
            mo_tree_local->m_leaf =  'LDB'.
            IF mo_tree_local->m_ldb_key IS INITIAL.
              mo_tree_local->add_node( i_name = 'LDB' i_icon = CONV #( icon_life_events ) ).
            ELSE.
              mo_tree_local->main_node_key = mo_tree_local->m_ldb_key.
            ENDIF.
          ELSE.
            CONTINUE.
          ENDIF.

        WHEN 'SYST'.
          IF mo_tree_local->m_syst IS NOT INITIAL.
            mo_tree_local->m_leaf =  'SYST'.
            IF mo_tree_local->m_syst_key IS INITIAL.
              mo_tree_local->add_node( i_name = 'System variables' i_icon = CONV #( icon_life_events ) ).
            ENDIF.
          ELSE.
            CONTINUE.
          ENDIF.
        WHEN 'CLASS'.
          IF mo_tree_local->m_class_data IS NOT INITIAL.
            mo_tree_local->m_leaf =  'CLASS'.
            IF mo_tree_local->m_class_key IS INITIAL.
              mo_tree_local->add_node( i_name = 'Class-data global variables' i_icon = CONV #( icon_life_events ) ).
            ENDIF.
          ELSE.
            CONTINUE.
          ENDIF.
      ENDCASE.

      READ TABLE mt_selected_var WITH KEY name = <var>-name ASSIGNING FIELD-SYMBOL(<sel>).
      IF sy-subrc = 0.

        IF <sel>-refval IS BOUND.
          ASSIGN <sel>-refval->* TO <hist>.
          ASSIGN <var>-ref->* TO <new>.

          IF <new> <> <hist>.
            <sel>-refval = <var>-ref.
            stop = abap_true.
          ENDIF.
        ELSE.
          <sel>-refval = <var>-ref.
        ENDIF.
      ENDIF.

      CASE <var>-leaf.
        WHEN 'IMP'.
          o_tree = mo_tree_imp.
        WHEN 'EXP'.
          o_tree = mo_tree_exp.
        WHEN OTHERS.
          o_tree = mo_tree_local.
      ENDCASE.

      IF <var>-parent IS NOT INITIAL.
        READ TABLE o_tree->mt_vars WITH KEY path = <var>-parent TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          <var>-done = abap_true.
        ELSE.
          IF m_hide IS INITIAL.

            is_skip = abap_true.
            CONTINUE.
          ENDIF.
        ENDIF.
      ELSE.
        <var>-done = abap_true.
      ENDIF.

      READ TABLE o_tree->mt_vars WITH KEY path = <var>-parent INTO var.
      IF sy-subrc = 0.
        key = var-key.
      ELSE.
        key = o_tree->main_node_key.
      ENDIF.

      IF <var>-ref IS NOT INITIAL.
        o_tree->traverse(
          io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( <var>-ref )
          i_parent_key  = key
          i_rel         = rel
          is_var         = <var>
          ir_up          = <var>-ref
          i_parent_calculated = CONV #( <var>-name ) ).
      ELSE.
        o_tree->traverse_obj(
          i_parent_key  = key
          i_rel         = rel
          is_var         = <var>
          ir_up          = <var>-ref
          i_parent_calculated = CONV #( <var>-name ) ).
      ENDIF.

    ENDLOOP.

    IF is_skip = abap_true.
      CLEAR is_skip.
      IF mv_recurse < 5.
        show_variables( CHANGING it_var = it_var ).
      ENDIF.
      set_selected_vars( ).
    ENDIF.


  endmethod.


  method TRANSFER_VARIABLE.


    DATA: lr_struc      TYPE REF TO data,
          o_table_descr TYPE REF TO cl_tpda_script_tabledescr,
          table_clone   TYPE REF TO data,
          name          TYPE string,
          full_name     TYPE string,
          o_deep_handle TYPE REF TO cl_abap_datadescr,
          deep_ref      TYPE REF TO cl_abap_typedescr,
          o_tabl        TYPE REF TO cl_abap_tabledescr,
          o_struc       TYPE REF TO cl_abap_structdescr,
          r_header      TYPE REF TO data,
          r_elem        TYPE REF TO data.

    DATA: len TYPE i.
    FIELD-SYMBOLS: <lv_value> TYPE any.

    full_name = i_name.

    IF i_name NE '{A:initial}'.
      TRY.
          CALL METHOD cl_tpda_script_data_descr=>get_quick_info
            EXPORTING
              p_var_name   = i_name
            RECEIVING
              p_symb_quick = m_quick.
        CATCH cx_tpda_varname .

          mo_tree_local->del_variable( EXPORTING i_full_name = i_name i_state = 'X' ).
          RETURN.
      ENDTRY.
    ELSE.
      m_quick-typid = 'g'.
    ENDIF.

    IF i_shortname IS NOT INITIAL.
      name = i_shortname.
    ELSE.
      name = i_name.
    ENDIF.

    TRY.
        IF i_name NE '{A:initial}'.
          ASSIGN m_quick-quickdata->* TO <lv_value>.
        ENDIF.

        IF m_quick-typid = 'h'."internal table
          READ TABLE mo_window->mt_source WITH KEY include = ms_stack-include INTO DATA(source).
          READ TABLE source-tt_tabs WITH KEY name = i_name INTO DATA(var).


          o_table_descr ?= cl_tpda_script_data_descr=>factory( i_name ).

*          DATA(comp_tpda) = o_table_descr->components( ).
*
*          DATA: comp TYPE abap_component_tab,
*                comp_descr TYPE abap_componentdescr.
*
*          LOOP AT comp_tpda INTO DATA(comp_descr_tpda).
*            REPLACE ALL OCCURRENCES OF '\TYPE-POOL=ABAP\TYPE=' IN comp_descr_tpda-abstypename WITH ''.
*            REPLACE ALL OCCURRENCES OF '\TYPE=' IN comp_descr_tpda-abstypename WITH ''.
*            REPLACE ALL OCCURRENCES OF '\TYPE-POOL=' IN comp_descr_tpda-abstypename WITH ''.
*
*            IF comp_descr_tpda-abstypename+0(3) = '%_T' OR
*              comp_descr_tpda-abstypename+0(11) = '\INTERFACE=' OR
*              comp_descr_tpda-abstypename+0(7) = '\CLASS='.
*              DATA(old_generation) = abap_true.
*              EXIT.
*            ENDIF.
*
*            DATA(o_descr) = cl_abap_typedescr=>describe_by_name( comp_descr_tpda-abstypename ).
*            IF o_descr IS INSTANCE OF cl_abap_elemdescr.
*              DATA(o_elem) = CAST cl_abap_elemdescr( o_descr ).
*              CLEAR comp_descr.
*              comp_descr-name = comp_descr_tpda-compname.
*              comp_descr-type = o_elem.
*              APPEND comp_descr TO comp.
*            ENDIF.
*          ENDLOOP.
*
*          old_generation = abap_true.
*          IF old_generation IS INITIAL.
*            "--- создаём структуру на основе component_tab
*            DATA(o_struct) = cl_abap_structdescr=>create( comp ).
*
*            "--- создаём таблицу этой структуры
*            DATA(o_table)  = cl_abap_tabledescr=>create( o_struct ).
*
*            "--- создаём реальный объект таблицы
*            DATA lr_table TYPE REF TO data.
*            CREATE DATA lr_table TYPE HANDLE o_table.
*
*            ASSIGN lr_table->* TO FIELD-SYMBOL(<lt_dyn>).
*          ENDIF.

          table_clone = o_table_descr->elem_clone( ).


          "ASSIGN table_clone->* TO FIELD-SYMBOL(<f>).

*          IF old_generation IS INITIAL.
*            MOVE-CORRESPONDING <f> TO <lt_dyn>.
*          ELSE.
          ASSIGN table_clone->* TO FIELD-SYMBOL(<lt_dyn>).
*          ENDIF.

          "check header area
          DATA td       TYPE sydes_desc.
          DESCRIBE FIELD <lt_dyn> INTO td.

          READ TABLE td-names INTO DATA(names) INDEX 1.
          IF sy-subrc = 0.
            TRY.
                CALL METHOD cl_tpda_script_data_descr=>get_quick_info
                  EXPORTING
                    p_var_name   = |{ i_name }-{ names-name }|
                  RECEIVING
                    p_symb_quick = DATA(quick).

                o_tabl ?= cl_abap_typedescr=>describe_by_data( <lt_dyn> ).

                o_struc ?= o_tabl->get_table_line_type( ).
                CREATE DATA r_header TYPE HANDLE o_struc.
                ASSIGN r_header->* TO FIELD-SYMBOL(<header>).

                traverse( io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( r_header )
                          i_name        = name
                          i_fullname    = i_name
                          i_type        = i_type
                          i_parent_calculated = i_parent_calculated
                          i_instance     = i_instance
                          i_cl_leaf      = i_cl_leaf
                          ir_up          = r_header ).

                name = name && '[]'.
                full_name = i_name && '[]'.
              CATCH cx_tpda_varname .
            ENDTRY.
          ENDIF.
          GET REFERENCE OF <lt_dyn> INTO lr_struc.

          traverse( io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( lr_struc )
                    i_name        = name
                    i_fullname    = full_name
                    i_type        = i_type
                    i_instance     = i_instance
                    i_parent_calculated = i_parent_calculated
                    i_cl_leaf      = i_cl_leaf
                    ir_up          = lr_struc ).

        ELSEIF m_quick-typid = 'l'. "data ref

          DATA: info TYPE tpda_scr_quick_info.

          FIELD-SYMBOLS: <symbdatref> TYPE tpda_sys_symbdatref.
          info = cl_tpda_script_data_descr=>get_quick_info( i_name ).
          ASSIGN info-quickdata->* TO <symbdatref>.

          " Check if the referenced object exists
          IF <symbdatref>-instancename IS NOT INITIAL AND
             <symbdatref>-instancename <> '{R:initial}' AND
             <symbdatref>-instancename <> '{A:initial}'.

            TRY." Try to get info about the referenced object
                DATA(ref_info) = cl_tpda_script_data_descr=>get_quick_info( <symbdatref>-instancename  ).

                " Handle string references specially
                IF ref_info-typid = 'g'. "string
                  " Create a string variable directly
                  APPEND INITIAL LINE TO mt_new_string ASSIGNING FIELD-SYMBOL(<m_string_ref>).
                  <m_string_ref> = create_simple_string( <symbdatref>-instancename ).
                  GET REFERENCE OF <m_string_ref> INTO m_variable.

                  traverse( io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( m_variable )
                            i_name        = name
                            i_type        = i_type
                            i_fullname    = i_name
                            i_parent_calculated = i_parent_calculated
                            i_instance     = i_instance
                            i_cl_leaf      = i_cl_leaf
                            ir_up          = m_variable ).
                ELSE.
                  " Handle other reference types as before
                  transfer_variable( EXPORTING i_name =  <symbdatref>-instancename
                                               i_type = i_type
                                               i_shortname = i_name
                                               i_parent_calculated = i_parent_calculated
                                               i_cl_leaf = i_cl_leaf
                                               i_instance = <symbdatref>-instancename ).
                ENDIF.
              CATCH cx_tpda_varname.
                " Handle error - show as unresolved reference
                APPEND INITIAL LINE TO mt_new_string ASSIGNING <m_string_ref>.
                <m_string_ref> = |Unresolved reference: { <symbdatref>-instancename }|.
                GET REFERENCE OF <m_string_ref> INTO m_variable.

                traverse( io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( m_variable )
                          i_name        = name
                          i_type        = i_type
                          i_fullname    = i_name
                          i_parent_calculated = i_parent_calculated
                          i_instance     = i_instance
                          i_cl_leaf      = i_cl_leaf
                          ir_up          = m_variable ).
            ENDTRY.
          ENDIF.

        ELSEIF m_quick-typid = 'r'. "reference
          FIELD-SYMBOLS: <symobjref> TYPE tpda_sys_symbobjref.
          ASSIGN m_quick-quickdata->* TO <symobjref>.

          save_hist( EXPORTING i_fullname    = i_name
                               i_name        = i_shortname
                               i_parent_calculated = i_parent_calculated
                               i_type        = i_type
                               i_cl_leaf     = i_cl_leaf
                               i_instance     = <symobjref>-instancename ).

          create_reference( EXPORTING i_name      = name
                                      i_type      = i_type
                                      i_shortname = name
                                      i_parent    = i_parent_calculated
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
            o_deep_handle ?= deep_ref.
            CREATE DATA lr_struc TYPE HANDLE o_deep_handle.
            get_deep_struc( EXPORTING i_name = i_name r_obj = lr_struc ).
            ASSIGN lr_struc->* TO FIELD-SYMBOL(<new_deep>).

            traverse( io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( lr_struc )
                      i_name        = name
                      i_fullname    = i_name
                      i_type        = i_type
                      i_parent_calculated = i_parent_calculated
                      i_instance     = i_instance
                      i_cl_leaf      = i_cl_leaf
                      ir_up          = lr_struc ).
          ELSE.
            create_struc2( EXPORTING i_name = i_name i_shortname = name ).
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
                    i_name        = name
                    i_type        = i_type
                    i_fullname    = i_name
                    i_parent_calculated = i_parent_calculated
                    i_instance     = i_instance
                    i_cl_leaf      = i_cl_leaf
                    ir_up          = m_variable ).
        ELSE.
          lr_struc = create_simple_var( i_name ).
          ASSIGN lr_struc->* TO FIELD-SYMBOL(<new_elem>).

          traverse( io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( lr_struc )
                    i_name        = name
                    i_fullname    = i_name
                    i_type        = i_type
                    i_parent_calculated = i_parent_calculated
                    ir_up          = lr_struc
                    i_cl_leaf      = i_cl_leaf
                    i_instance     = i_instance ).
        ENDIF.
      CATCH cx_root.
    ENDTRY.


  endmethod.


  method TRAVERSE.


    "create new data
    DATA: lr_new   TYPE REF TO data,
          lr_struc TYPE REF TO data.

    FIELD-SYMBOLS: <new>      TYPE any,
                   <tab_from> TYPE ANY TABLE,
                   <tab_to>   TYPE STANDARD TABLE,
                   <ir_up>    TYPE any.
    ASSIGN ir_up->* TO <ir_up>.
    DESCRIBE FIELD ir_up TYPE DATA(type).
    IF type NE cl_abap_typedescr=>typekind_table.
      CREATE DATA lr_new LIKE <ir_up>.
      ASSIGN lr_new->*  TO <new>.
      ASSIGN ir_up->* TO <new>.
      GET REFERENCE OF <new> INTO lr_new.
    ELSE.
      ASSIGN ir_up->* TO <tab_from>.
      CREATE DATA lr_struc LIKE LINE OF <tab_from>.
      ASSIGN lr_struc->* TO FIELD-SYMBOL(<record>).
      CREATE DATA lr_new LIKE STANDARD TABLE OF <record>.
      ASSIGN lr_new->* TO <tab_to>.
      <tab_to> = <tab_from>.
    ENDIF.
    GET REFERENCE OF <new> INTO m_variable.

    DATA td TYPE sydes_desc.
    DESCRIBE FIELD ir_up INTO td.

    m_variable = lr_new.

    CASE io_type_descr->kind.
      WHEN c_kind-struct.
        IF i_struc_name IS SUPPLIED.
          traverse_struct( io_type_descr  = io_type_descr
                           i_name        = i_name
                           i_fullname    = i_fullname
                           i_type        = i_type
                           ir_up          = ir_up
                           i_parent_calculated = i_parent_calculated
                           i_instance     = i_instance
                           i_cl_leaf      = i_cl_leaf
                           i_struc_name  = i_struc_name
                           i_suffix       = i_suffix ).
        ELSE.
          traverse_struct( io_type_descr  = io_type_descr
                           i_name        = i_name
                           i_fullname    = i_fullname
                           i_type        = i_type
                           ir_up          = ir_up
                           i_instance     = i_instance
                           i_cl_leaf      = i_cl_leaf
                           i_parent_calculated = i_parent_calculated ).
        ENDIF.

      WHEN c_kind-elem.
        traverse_elem( i_name        = i_name
                       i_fullname    = i_fullname
                       i_type        = i_type
                       ir_up          = ir_up
                       i_instance     = i_instance
                       i_cl_leaf      = i_cl_leaf
                       i_parent_calculated = i_parent_calculated ).

      WHEN c_kind-table.
        traverse_elem( i_name        = i_name
                       i_fullname    = i_fullname
                       i_type        = i_type
                       ir_up          = ir_up
                       i_instance     = i_instance
                       i_cl_leaf      = i_cl_leaf
                       i_parent_calculated = i_parent_calculated ).
    ENDCASE.


  endmethod.


  method TRAVERSE_ELEM.


    save_hist( EXPORTING ir_up          = ir_up
                         i_fullname    = i_fullname
                         i_name        = i_name
                         i_parent_calculated = i_parent_calculated
                         i_type        = i_type
                         i_cl_leaf     = i_cl_leaf
                         i_instance     = i_instance ).


  endmethod.


  method TRAVERSE_STRUCT.


    DATA: component       TYPE abap_component_tab,
          comp_descronent LIKE LINE OF component,
          o_struct_descr  TYPE REF TO cl_abap_structdescr,
          string          TYPE string,
          parent          TYPE string.

    o_struct_descr ?= io_type_descr.

    IF  ( i_struc_name IS SUPPLIED AND i_struc_name IS NOT INITIAL ) OR i_struc_name IS NOT SUPPLIED.
      IF i_name IS NOT INITIAL.
        save_hist( EXPORTING ir_up          = ir_up
                             i_fullname    = i_fullname
                             i_name        = i_name
                             i_type        = i_type
                             i_parent_calculated = i_parent_calculated
                             i_cl_leaf     = i_cl_leaf
                             i_instance     = i_instance ).

      ENDIF.
    ENDIF.

    component = o_struct_descr->get_components( ).

    LOOP AT component INTO comp_descronent.
      IF comp_descronent-name IS INITIAL AND comp_descronent-suffix IS NOT INITIAL.
        DATA(suffix) =  comp_descronent-suffix.
      ENDIF.

      IF i_suffix IS NOT INITIAL.
        comp_descronent-name = comp_descronent-name && i_suffix.
      ENDIF.
      DATA: lr_new_struc TYPE REF TO data.
      ASSIGN ir_up->* TO FIELD-SYMBOL(<up>).
      IF comp_descronent-name IS INITIAL.
        lr_new_struc = ir_up.
      ELSE.
        ASSIGN COMPONENT comp_descronent-name OF STRUCTURE <up> TO FIELD-SYMBOL(<new>).
        GET REFERENCE OF <new> INTO lr_new_struc.
      ENDIF.

      IF comp_descronent-name IS NOT INITIAL.
        string = |{ i_fullname }-{ comp_descronent-name }|.
      ELSE.
        string = i_fullname.
      ENDIF.

      TRY.
          CALL METHOD cl_tpda_script_data_descr=>get_quick_info
            EXPORTING
              p_var_name   = string
            RECEIVING
              p_symb_quick = DATA(quick).
        CATCH cx_tpda_varname .
      ENDTRY.

      IF quick-typid = 'r'.
        DATA: lr_variable TYPE REF TO data. "need to refaktor
        lr_variable = m_variable.

        FIELD-SYMBOLS: <symobjref> TYPE tpda_sys_symbobjref.
        ASSIGN quick-quickdata->* TO <symobjref>.

        save_hist( EXPORTING i_fullname    = string
                             i_name        = comp_descronent-name
                             i_parent_calculated = i_fullname
                             i_type        = i_type
                             i_cl_leaf     = i_cl_leaf
                             i_instance     = <symobjref>-instancename ).

        create_reference( EXPORTING i_name      = string
                                    i_type      = i_type
                                    i_shortname = comp_descronent-name
                                    i_quick     = quick ).

        m_variable = lr_variable.
      ELSE.
        IF i_name IS NOT INITIAL.
          IF i_parent_calculated IS NOT INITIAL.
            parent = |{ i_parent_calculated }-{ i_name }|.
          ELSE.
            parent = i_name.
          ENDIF.
        ELSE.
          parent = i_parent_calculated.
        ENDIF.
        traverse( io_type_descr  = comp_descronent-type
                  i_name        = comp_descronent-name
                  i_fullname    = string
                  i_type        = i_type
                  ir_up          = lr_new_struc
                  i_parent_calculated = parent
                  i_struc_name  = comp_descronent-name
                  i_cl_leaf      = i_cl_leaf
                  i_instance     = i_instance
                  i_suffix       = suffix ).
      ENDIF.
    ENDLOOP.


  endmethod.
ENDCLASS.
