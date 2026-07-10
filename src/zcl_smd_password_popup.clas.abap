CLASS zcl_smd_password_popup DEFINITION
  PUBLIC
  INHERITING FROM zcl_smd_popup
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS get_password
      RETURNING VALUE(rv_password) TYPE string.
  PRIVATE SECTION.
    DATA mo_text TYPE REF TO cl_gui_textedit.
    DATA mv_closed TYPE abap_bool.
    METHODS on_close
      FOR EVENT close OF cl_gui_dialogbox_container
      IMPORTING sender.
ENDCLASS.

CLASS zcl_smd_password_popup IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mo_box = create( i_name = 'AI password' i_width = 500 i_hight = 150 ).
    IF mo_box IS NOT BOUND.
      mv_closed = abap_true.
      RETURN.
    ENDIF.

    SET HANDLER on_close FOR mo_box.
    CREATE OBJECT mo_text
      EXPORTING
        parent = mo_box
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      mv_closed = abap_true.
      RETURN.
    ENDIF.

    mo_text->set_toolbar_mode( 0 ).
    mo_text->set_statusbar_mode( 0 ).
    mo_text->set_focus( mo_box ).
    cl_gui_cfw=>flush( ).
  ENDMETHOD.

  METHOD get_password.
    WHILE mv_closed IS INITIAL.
      WAIT UP TO 1 SECONDS.
    ENDWHILE.

    DATA lt_text TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    mo_text->get_text_as_stream( IMPORTING text = lt_text ).
    READ TABLE lt_text INDEX 1 INTO rv_password.
  ENDMETHOD.

  METHOD on_close.
    mv_closed = abap_true.
    sender->free( ).
  ENDMETHOD.

ENDCLASS.
