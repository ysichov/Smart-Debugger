class ZCL_SMD_COMMON definition
  public
  create public .

public section.

  constants:
    c_white(4) TYPE x value '00000001' ##NO_TEXT.

  class-methods REFRESH
    importing
      !I_OBJ type ref to CL_GUI_ALV_GRID
      !I_LAYOUT type LVC_S_LAYO optional
      !I_SOFT type CHAR01 optional .
  class-methods TRANSLATE_FIELD
    importing
      !I_LANG type DDLANGUAGE optional
    changing
      !C_FLD type LVC_S_FCAT .
  class-methods GET_SELECTED
    importing
      !I_OBJ type ref to CL_GUI_ALV_GRID
    returning
      value(E_INDEX) type I .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SMD_COMMON IMPLEMENTATION.


  method GET_SELECTED.


    i_obj->get_selected_cells( IMPORTING et_cell = DATA(sel_cells) ).
    IF lines( sel_cells ) > 0.
      e_index = sel_cells[ 1 ]-row_id.
    ELSE.
      i_obj->get_selected_rows( IMPORTING et_index_rows = DATA(sel_rows) ).
      IF lines( sel_rows ) > 0.
        e_index = sel_rows[ 1 ]-index.
      ENDIF.
    ENDIF.


  endmethod.


  method REFRESH.


    DATA stable TYPE lvc_s_stbl.
    stable = 'XX'.
    IF i_layout IS SUPPLIED.
      i_obj->set_frontend_layout( i_layout ).
    ENDIF.
    i_obj->refresh_table_display( EXPORTING is_stable = stable i_soft_refresh = i_soft ).


  endmethod.


  method TRANSLATE_FIELD.


    DATA: field_info TYPE TABLE OF dfies.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = c_fld-tabname
        fieldname      = c_fld-fieldname
        langu          = i_lang
      TABLES
        dfies_tab      = field_info
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.

    IF sy-subrc = 0.
      READ TABLE field_info INDEX 1 INTO DATA(info).
      IF info-scrtext_l IS INITIAL AND info-scrtext_m IS INITIAL AND info-scrtext_s IS INITIAL.
        IF info-fieldtext IS NOT INITIAL.
          MOVE info-fieldtext TO: c_fld-reptext, c_fld-scrtext_l, c_fld-scrtext_m, c_fld-scrtext_s .
        ELSE.
          MOVE info-fieldname TO: c_fld-reptext, c_fld-scrtext_l, c_fld-scrtext_m, c_fld-scrtext_s .
        ENDIF.
      ELSE.
        c_fld-scrtext_l = info-scrtext_l.
        c_fld-scrtext_m = info-scrtext_m.
        c_fld-scrtext_s = info-scrtext_s.
        IF info-reptext IS NOT INITIAL.
          c_fld-reptext   = info-reptext.
        ENDIF.
      ENDIF.
    ENDIF.


  endmethod.
ENDCLASS.
