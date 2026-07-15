class ZCL_SMD_TEXT_VIEWER definition
  public
  inheriting from ZCL_SMD_POPUP
  final
  create public .

public section.

  data MO_TEXT type ref to CL_GUI_TEXTEDIT .

  methods CONSTRUCTOR
    importing
      !IR_STR type ref to DATA .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SMD_TEXT_VIEWER IMPLEMENTATION.


  method CONSTRUCTOR.

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
      RETURN.
    ENDIF.

    mo_text->set_readonly_mode( ).
    FIELD-SYMBOLS <str> TYPE string.
    ASSIGN ir_str->* TO <str>.
    DATA string TYPE TABLE OF char255.

    "work on a copy: shifting the referenced string directly would truncate
    "the debugger-held value of the variable being displayed
    DATA(text) = <str>.
    WHILE strlen( text ) > 255.
      APPEND text+0(255) TO string.
      SHIFT text LEFT BY 255 PLACES.
    ENDWHILE.

    APPEND text TO string.
    mo_text->set_text_as_r3table( string ).
    CALL METHOD cl_gui_cfw=>flush.
    mo_text->set_focus( mo_box ).


  endmethod.
ENDCLASS.
