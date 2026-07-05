class ZCL_SMD_APPL definition
  public
  create public .

public section.

  types:
    BEGIN OF selection_display,
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
        transmitter TYPE REF TO Zcl_smd_transmitter,
        receiver    TYPE REF TO Zcl_smd_receiver,
        color       TYPE lvc_t_scol,
        style       TYPE lvc_t_styl,
      END OF selection_display .
  types:
    BEGIN OF t_sel_row,
        sign        TYPE tvarv_sign,
        opti        TYPE tvarv_opti,
        option_icon TYPE aqadh_type_of_icon,
        low         TYPE string, "aqadh_range_value,
        high        TYPE string, "aqadh_range_value,
        more_icon   TYPE aqadh_type_of_icon,
        range       TYPE aqadh_t_ranges,
      END OF t_sel_row .
  types:
    BEGIN OF sign_option_icon_s,
             sign          TYPE tvarv_sign,
             option        TYPE tvarv_opti,
             icon_name(64) TYPE c,
             icon          TYPE aqadh_type_of_icon,
           END OF sign_option_icon_s .
  types:
    BEGIN OF var_table,
             step          TYPE i,
             stack         TYPE i,
             program(40)   TYPE c,
             eventtype(30) TYPE c,
             eventname(61) TYPE c,
             first         TYPE xfeld,
             is_appear     TYPE xfeld,
             del           TYPE xfeld,
             leaf          TYPE string,
             name(1000)               ,
             path          TYPE string,
             short         TYPE string,
             key           TYPE salv_de_node_key,
             parent        TYPE string,
             cl_leaf       TYPE int4,
             ref           TYPE REF TO data,
             type          TYPE string,
             instance      TYPE string,
             objname       TYPE string,
             done          TYPE xfeld,
           END OF var_table .
  types:
    variables TYPE STANDARD TABLE OF var_table WITH NON-UNIQUE DEFAULT KEY .
  types:
    BEGIN OF var_table_temp,
             step          TYPE i,
             stack         TYPE i,
             eventtype(30) TYPE c,
             eventname(61) TYPE c,
             name          TYPE string,
             value         TYPE string,
             first         TYPE xfeld,
             is_appear     TYPE xfeld,
             del           TYPE xfeld,
             program(40)   TYPE c,
             leaf          TYPE string,
             path          TYPE string,
             type          TYPE string,
             instance      TYPE string,
             objname       TYPE string,
             ref           TYPE REF TO data,
           END OF var_table_temp .
  types:
    BEGIN OF var_table_h,
             step          TYPE i,
             program(40)   TYPE c,
             eventtype(30) TYPE c,
             eventname(61) TYPE c,
             leaf          TYPE string,
             name          TYPE string,
             path          TYPE string,
             parent        TYPE string,
             short         TYPE string,
             cl_leaf       TYPE int4,  "?
             ref           TYPE REF TO data,
             tree          TYPE REF TO zcl_smd_rtti_tree,
             time          LIKE sy-uname,
           END OF var_table_h .
  types:
    BEGIN OF t_obj,
             name       TYPE string,
             alv_viewer TYPE REF TO zcl_smd_table_viewer,
           END OF t_obj .
  types:
    BEGIN OF t_popup,
             parent TYPE REF TO cl_gui_dialogbox_container,
             child  TYPE REF TO cl_gui_dialogbox_container,
           END OF t_popup .
  types:
    BEGIN OF t_classes_types,
             name TYPE string,
             full TYPE string,
             type TYPE char1,
             key  TYPE salv_de_node_key,
           END OF t_classes_types .
  types:
    BEGIN OF t_lang,
             spras(4),
             sptxt    TYPE sptxt,
           END OF t_lang .
  types:
    BEGIN OF t_stack,
             step       TYPE i,
             "stackpointer TYPE tpda_stack_pointer,
             stacklevel TYPE tpda_stack_level,
             line       TYPE tpda_sc_line,
             eventtype  TYPE tpda_event_type,
             eventname  TYPE tpda_event,
             program    TYPE tpda_program,
             include    TYPE tpda_include,
           END OF t_stack .
  types:
    BEGIN OF t_step_counter,
             step       TYPE i,
             stacklevel TYPE tpda_stack_level,
             line       TYPE tpda_sc_line,
             eventtype  TYPE string,
             eventname  TYPE string,
             first      TYPE xfeld,
             last       TYPE xfeld,
             program    TYPE tpda_program,
             include    TYPE tpda_include,
             time       LIKE sy-uzeit,
           END OF t_step_counter .
  types:
    tt_steps TYPE STANDARD TABLE OF t_step_counter WITH EMPTY KEY .

  class-data:
    m_option_icons    TYPE TABLE OF sign_option_icon_s .
  class-data:
    mt_lang           TYPE TABLE OF t_lang .
  class-data:
    mt_obj            TYPE TABLE OF t_obj .
  class-data:
    mt_popups         TYPE TABLE OF t_popup .
  class-data C_DRAGDROPALV type ref to CL_DRAGDROP .
  class-data IS_MERMAID_ACTIVE type XFELD .
  class-data:
    mt_sel TYPE TABLE OF selection_display .

  class-methods INIT_ICONS_TABLE .
  class-methods INIT_LANG .
  class-methods CHECK_MERMAID .
  class-methods OPEN_INT_TABLE
    importing
      !IT_TAB type ANY TABLE optional
      !IT_REF type ref to DATA optional
      !I_NAME type STRING
      !IO_WINDOW type ref to ZCL_SMD_WINDOW .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SMD_APPL IMPLEMENTATION.


  method CHECK_MERMAID.


    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING
        clskey        = 'ZCL_WD_GUI_MERMAID_JS_DIAGRAM '
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_interface  = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.

    IF sy-subrc = 0.
      is_mermaid_active = abap_true.
    ENDIF.


  endmethod.


  method INIT_ICONS_TABLE.


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


  endmethod.


  method INIT_LANG.

*    SELECT c~spras t~sptxt INTO CORRESPONDING FIELDS OF TABLE mt_lang
*      FROM t002c AS c
*      INNER JOIN t002t AS t
*      ON c~spras = t~sprsl
*      WHERE t~spras = sy-langu
*      ORDER BY c~ladatum DESCENDING c~lauzeit DESCENDING.

  endmethod.


  method OPEN_INT_TABLE.


    DATA r_tab TYPE REF TO data.
    IF it_ref IS BOUND.
      r_tab = it_ref.
    ELSE.
      GET REFERENCE OF it_tab INTO r_tab.
    ENDIF.
    APPEND INITIAL LINE TO zcl_smd_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    <obj>-alv_viewer = NEW #(  i_additional_name = i_name ir_tab = r_tab io_window = io_window ).
    <obj>-alv_viewer->mo_sel->raise_selection_done( ).


  endmethod.
ENDCLASS.
