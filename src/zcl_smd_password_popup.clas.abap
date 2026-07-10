class ZCL_SMD_PASSWORD_POPUP definition
  public
  inheriting from ZCL_SMD_POPUP
  final
  create public .

public section.

  data MO_TEXT type ref to CL_GUI_TEXTEDIT .

  methods CONSTRUCTOR
    importing
      !IR_PASSWORD type ref to STRING
      !IR_OPEN type ref to XFELD .
  methods ON_BOX_CLOSE
    redefinition .
protected section.
private section.

  data MR_PASSWORD type ref to STRING .
  data MR_OPEN type ref to XFELD .
ENDCLASS.



CLASS ZCL_SMD_PASSWORD_POPUP IMPLEMENTATION.


  method CONSTRUCTOR.

    super->constructor( ).
    mr_password = ir_password.
    mr_open = ir_open.

    mo_box = create(
      i_name  = 'AI key password'
      i_width = 450
      i_hight = 80 ).

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
      RETURN.
    ENDIF.

    mo_text->set_toolbar_mode( toolbar_mode = 0 ).
    mo_text->set_statusbar_mode( statusbar_mode = 0 ).
    mo_text->set_focus( mo_box ).
    cl_gui_cfw=>flush( ).

  endmethod.


  method ON_BOX_CLOSE.

    DATA lt_text TYPE STANDARD TABLE OF char255.
    DATA lv_password TYPE string.

    IF mo_text IS BOUND.
      mo_text->get_text_as_r3table(
        IMPORTING
          table = lt_text
        EXCEPTIONS
          OTHERS = 1 ).

      READ TABLE lt_text INDEX 1 INTO DATA(lv_line).
      IF sy-subrc = 0.
        lv_password = lv_line.
      ENDIF.
    ENDIF.

    IF mr_password IS BOUND.
      mr_password->* = lv_password.
    ENDIF.

    IF mr_open IS BOUND.
      CLEAR mr_open->*.
    ENDIF.

    super->on_box_close( sender ).

  endmethod.
ENDCLASS.
