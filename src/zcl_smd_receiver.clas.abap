class ZCL_SMD_RECEIVER definition
  public
  create public .

public section.

  data MO_TRANSMITTER type ref to ZCL_SMD_TRANSMITTER .
  data O_TAB_FROM type ref to ZCL_SMD_TABLE_VIEWER .
  data O_SEL_TO type ref to ZCL_SMD_SEL_OPT .
  data M_FROM_FIELD type LVC_FNAME .
  data M_TO_FIELD type LVC_FNAME .

  methods CONSTRUCTOR
    importing
      !IO_TRANSMITTER type ref to ZCL_SMD_TRANSMITTER optional
      !IO_TAB_FROM type ref to ZCL_SMD_TABLE_VIEWER optional
      !IO_SEL_TO type ref to ZCL_SMD_SEL_OPT optional
      !I_FROM_FIELD type LVC_FNAME optional
      !I_TO_FIELD type LVC_FNAME optional .
  methods SHUT_DOWN .
  methods UPDATE
    for event DATA_CHANGED of ZCL_SMD_TRANSMITTER
    importing
      !E_ROW .
  methods UPDATE_COL
    for event COL_CHANGED of ZCL_SMD_TRANSMITTER
    importing
      !E_COLUMN .
  methods ON_GRID_BUTTON_CLICK
    for event BUTTON_CLICK of CL_GUI_ALV_GRID
    importing
      !ES_COL_ID
      !ES_ROW_NO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SMD_RECEIVER IMPLEMENTATION.


  method CONSTRUCTOR.


    o_sel_to = io_sel_to.
    m_from_field =  i_from_field.
    m_to_field =  i_to_field.
    o_tab_from = io_tab_from.
    mo_transmitter = io_transmitter.

    IF mo_transmitter IS NOT INITIAL.
      IF o_tab_from IS INITIAL.
        SET HANDLER me->update FOR io_transmitter.
      ELSE.
        SET HANDLER me->update_col FOR io_transmitter.
      ENDIF.
    ELSE.
      SET HANDLER me->update FOR ALL INSTANCES.
    ENDIF.


  endmethod.


  method ON_GRID_BUTTON_CLICK.


    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.

    CHECK m_from_field = es_col_id-fieldname.
    ASSIGN o_tab_from->mr_table->* TO <table>.
    READ TABLE <table> INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<row>).
    ASSIGN COMPONENT es_col_id-fieldname OF STRUCTURE <row> TO  FIELD-SYMBOL(<field>).
    CHECK o_sel_to IS NOT INITIAL.
    o_sel_to->set_value( i_field = m_to_field i_low = <field> ).
    o_sel_to->raise_selection_done( ).


  endmethod.


  method SHUT_DOWN.


    IF mo_transmitter IS NOT INITIAL.
      SET HANDLER me->update FOR mo_transmitter  ACTIVATION space.
    ELSE.
      SET HANDLER me->update FOR ALL INSTANCES  ACTIVATION space.
    ENDIF.
    CLEAR o_sel_to.


  endmethod.


  method UPDATE.


    DATA: updated.

    READ TABLE o_sel_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to>) WITH KEY field_label = m_to_field.
    IF <to>-range[] = e_row-range[].
      updated = abap_true."so as not to have an infinite event loop
    ENDIF.
    MOVE-CORRESPONDING e_row TO <to>.
    IF <to>-transmitter IS BOUND AND updated IS INITIAL.
      <to>-transmitter->emit( EXPORTING e_row = e_row ).
    ENDIF.
    o_sel_to->raise_selection_done( ).


  endmethod.


  method UPDATE_COL.


    DATA: updated,
          sel_row   TYPE zcl_smd_appl=>t_sel_row.

    FIELD-SYMBOLS: <tab>   TYPE STANDARD TABLE,
                   <field> TYPE any.

    CHECK o_sel_to IS NOT INITIAL.
    READ TABLE o_sel_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to>) WITH KEY field_label = m_to_field.
    DATA(old_range) = <to>-range.
    CLEAR: <to>-sign, <to>-opti, <to>-low, <to>-high, <to>-range.
    ASSIGN o_tab_from->mr_table->* TO <tab>.

    LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<row>).
      ASSIGN COMPONENT e_column OF STRUCTURE <row> TO <field>.
      IF line_exists( <to>-range[ low = <field> ] ).
        APPEND VALUE #( sign = 'I' opti = 'EQ' low = <field> ) TO <to>-range.
      ENDIF.
    ENDLOOP.

    IF sy-subrc NE 0." empty column
      APPEND VALUE #( sign = 'I' opti = 'EQ' low = '' ) TO <to>-range.
    ENDIF.

    LOOP AT <to>-range ASSIGNING FIELD-SYMBOL(<sel>).
      <to>-low = <sel>-low.
      o_sel_to->update_sel_row( CHANGING c_sel_row = <to> ).
      EXIT.
    ENDLOOP.

    MOVE-CORRESPONDING <to> TO sel_row.
    IF <to>-range = old_range.
      updated = abap_true."so as not to have an infinite event loop
    ENDIF.
    IF <to>-transmitter IS BOUND AND updated IS INITIAL.
      <to>-transmitter->emit( EXPORTING e_row = sel_row ).
      o_sel_to->raise_selection_done( ).
    ENDIF.


  endmethod.
ENDCLASS.
