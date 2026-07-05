class ZCL_SMD_DDIC definition
  public
  create public .

public section.

  class-methods GET_TEXT_TABLE
    importing
      !I_TNAME type TABNAME
    exporting
      !E_TAB type TABNAME .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SMD_DDIC IMPLEMENTATION.


  method GET_TEXT_TABLE.

    CALL FUNCTION 'DDUT_TEXTTABLE_GET'
      EXPORTING
        tabname   = i_tname
      IMPORTING
        texttable = e_tab.

  endmethod.
ENDCLASS.
