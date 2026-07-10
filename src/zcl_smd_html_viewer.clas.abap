class ZCL_SMD_HTML_VIEWER definition
  public
  inheriting from ZCL_SMD_POPUP
  final
  create public .

public section.

  data MO_HTML type ref to CL_GUI_HTML_VIEWER .

  methods CONSTRUCTOR
    importing
      !I_MARKDOWN type STRING
      !I_TITLE type TEXT100 optional .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SMD_HTML_VIEWER IMPLEMENTATION.


  method CONSTRUCTOR.

    DATA lt_html TYPE STANDARD TABLE OF w3html.
    DATA ls_html TYPE w3html.
    DATA lv_html TYPE string.
    DATA lv_offset TYPE i.
    DATA lv_url TYPE c LENGTH 255.

    super->constructor( ).
    mo_box = create( i_name = i_title i_width = 900 i_hight = 500 ).

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

    CREATE OBJECT mo_html
      EXPORTING
        parent = mo_variables_container
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      on_box_close( mo_box ).
      RETURN.
    ENDIF.

    lv_html = zcl_smd_markdown_html=>to_html( i_markdown ).

    WHILE lv_offset < strlen( lv_html ).
      CLEAR ls_html.
      ls_html-line = substring(
        val = lv_html
        off = lv_offset
        len = nmin( val1 = 255 val2 = strlen( lv_html ) - lv_offset ) ).
      APPEND ls_html TO lt_html.
      lv_offset = lv_offset + 255.
    ENDWHILE.

    mo_html->load_data(
      EXPORTING
        type         = 'text'
        subtype      = 'html'
      IMPORTING
        assigned_url = lv_url
      CHANGING
        data_table   = lt_html
      EXCEPTIONS
        OTHERS       = 1 ).

    mo_html->show_url(
      EXPORTING
        url = lv_url
      EXCEPTIONS
        OTHERS = 1 ).

    cl_gui_cfw=>flush( ).

  endmethod.
ENDCLASS.
