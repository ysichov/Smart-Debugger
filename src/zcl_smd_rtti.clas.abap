class ZCL_SMD_RTTI definition
  public
  create public .

public section.

  class-methods CREATE_TABLE_BY_NAME
    importing
      !I_TNAME type TABNAME
    changing
      !C_TABLE type ref to DATA .
  class-methods CREATE_STRUC_HANDLE
    importing
      !I_TNAME type TABNAME
    exporting
      !E_T_COMP type ABAP_COMPONENT_TAB
      !E_HANDLE type ref to CL_ABAP_STRUCTDESCR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SMD_RTTI IMPLEMENTATION.


  method CREATE_STRUC_HANDLE.

    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = i_tname
                                         RECEIVING  p_descr_ref    = DATA(o_descr)
                                         EXCEPTIONS type_not_found = 1 ).
    IF sy-subrc = 0.
      e_handle ?= o_descr.
    ELSE.
      RETURN.
    ENDIF.


  endmethod.


  method CREATE_TABLE_BY_NAME.


    DATA: o_new_tab  TYPE REF TO cl_abap_tabledescr,
          o_new_type TYPE REF TO cl_abap_structdescr.

    create_struc_handle( EXPORTING i_tname = i_tname IMPORTING e_handle = o_new_type ).
    o_new_tab = cl_abap_tabledescr=>create(
      p_line_type  = o_new_type
      p_table_kind = cl_abap_tabledescr=>tablekind_std
      p_unique     = abap_false ).
    CREATE DATA c_table TYPE HANDLE o_new_tab.  "Create a New table type

  endmethod.
ENDCLASS.
