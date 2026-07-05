class ZCL_SMD_DRAGDROP definition
  public
  create public .

public section.

  class-methods DRAG
    for event ONDRAG of CL_GUI_ALV_GRID
    importing
      !E_COLUMN
      !E_DRAGDROPOBJ
      !E_ROW .
  class-methods DROP
    for event ONDROP of CL_GUI_ALV_GRID
    importing
      !E_DRAGDROPOBJ
      !E_ROW .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SMD_DRAGDROP IMPLEMENTATION.


  method DRAG.


    DATA(dataobj) = NEW zcl_smd_dd_data( ).
    dataobj->m_row = e_row-index.
    dataobj->m_column = e_column.
    e_dragdropobj->object = dataobj.


  endmethod.


  method DROP.
"It should be refactored someday...

    DATA: row          TYPE zcl_smd_appl=>t_sel_row,
          set_receiver.

    LOOP AT zcl_smd_appl=>mt_obj INTO DATA(lo).
      "to
      IF lo-alv_viewer->mo_sel IS BOUND.
        IF e_dragdropobj->droptargetctrl = lo-alv_viewer->mo_sel->mo_sel_alv.
          DATA(o_to) = lo-alv_viewer->mo_sel.
        ENDIF.
      ENDIF.

      "from tab
      IF lo-alv_viewer->mo_alv = e_dragdropobj->dragsourcectrl.
        DATA(o_from_tab) = lo-alv_viewer.
        CONTINUE.
      ENDIF.

      IF e_dragdropobj->dragsourcectrl = lo-alv_viewer->mo_sel->mo_sel_alv.
        DATA(o_from_sel) = lo-alv_viewer->mo_sel.
        lo-alv_viewer->mo_sel->mo_sel_alv->get_selected_rows( IMPORTING et_index_rows = DATA(sel_rows) ).
        lo-alv_viewer->mo_sel->mo_sel_alv->get_selected_cells( IMPORTING et_cell = DATA(sel_cells) ).
      ENDIF.
    ENDLOOP.

    IF o_from_tab IS BOUND." tab to select
      FIELD-SYMBOLS: <table> TYPE STANDARD TABLE,
                     <field> TYPE any.
      o_from_tab->mo_alv->get_selected_cells( IMPORTING et_cell = sel_cells ).
      o_from_tab->mo_alv->get_selected_columns( IMPORTING et_index_columns = DATA(sel_col) ).

      LOOP AT sel_col INTO DATA(col).
        TRY.
            o_from_tab->mt_alv_catalog[ fieldname = col-fieldname ]-style = cl_gui_alv_grid=>mc_style_button.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
        READ TABLE o_from_tab->mo_column_emitters WITH KEY column = col ASSIGNING FIELD-SYMBOL(<emitter>).
        IF sy-subrc NE 0.
          APPEND INITIAL LINE TO o_from_tab->mo_column_emitters ASSIGNING <emitter>.
          <emitter>-column = col.
          <emitter>-emitter = NEW #( ).
        ENDIF.
      ENDLOOP.

      IF sy-subrc = 0.
        set_receiver = abap_true.
        CALL METHOD o_from_tab->mo_alv->set_frontend_fieldcatalog EXPORTING it_fieldcatalog = o_from_tab->mt_alv_catalog.
      ENDIF.

      TRY.
          ASSIGN o_from_tab->mr_table->* TO <table>.
          READ TABLE o_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to_tab>) INDEX e_row.
          LOOP AT sel_cells INTO DATA(cell).
            IF sy-tabix = 1.
              DATA(colname) = cell-col_id-fieldname.
            ENDIF.
            READ TABLE <table> INDEX cell-row_id ASSIGNING FIELD-SYMBOL(<str>).
            ASSIGN COMPONENT colname OF STRUCTURE <str> TO <field>.
            IF sy-subrc = 0.
              IF set_receiver IS NOT INITIAL.
                IF <to_tab>-receiver IS BOUND.
                  <to_tab>-receiver->shut_down( ).
                ENDIF.
                CREATE OBJECT <to_tab>-receiver
                  EXPORTING
                    io_transmitter = <emitter>-emitter
                    i_from_field   = CONV #( sel_cells[ 1 ]-col_id )
                    i_to_field     = <to_tab>-field_label
                    io_sel_to      = o_to
                    io_tab_from    = o_from_tab.
                SET HANDLER <to_tab>-receiver->on_grid_button_click FOR o_from_tab->mo_alv.
              ENDIF.

              IF <to_tab>-range IS INITIAL.
                <to_tab>-low = <field>.
              ENDIF.
              IF NOT line_exists( <to_tab>-range[ low = <field> ] ).
                APPEND VALUE #( sign = 'I' opti = 'EQ' low = <field>  ) TO <to_tab>-range.
              ENDIF.
            ENDIF.
          ENDLOOP.
          o_to->update_sel_row( CHANGING c_sel_row = <to_tab> ).
        CATCH cx_sy_itab_line_not_found.                "#EC NO_HANDLER
      ENDTRY.
    ENDIF.

    "select to select
    IF o_from_sel NE o_to.
      IF sel_rows[] IS INITIAL.
        DELETE sel_cells WHERE col_id NE 'FIELD_LABEL'.
        LOOP AT sel_cells INTO DATA(sel).
          APPEND INITIAL LINE TO sel_rows ASSIGNING FIELD-SYMBOL(<row>).
          <row>-index = sel-row_id-index.
        ENDLOOP.
      ENDIF.

      LOOP AT sel_rows ASSIGNING <row>.
        READ TABLE o_from_sel->mt_sel_tab ASSIGNING FIELD-SYMBOL(<from_tab>) INDEX <row>-index.
        IF lines( sel_rows ) = 1.
          READ TABLE o_to->mt_sel_tab ASSIGNING <to_tab> INDEX e_row.
        ELSE.
          READ TABLE o_to->mt_sel_tab ASSIGNING <to_tab> WITH KEY field_label = <from_tab>-field_label.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.
        ENDIF.
        MOVE-CORRESPONDING <from_tab> TO row.
        MOVE-CORRESPONDING row TO <to_tab>.
        <from_tab>-emitter = icon_workflow_external_event.
        <to_tab>-inherited = icon_businav_value_chain.
        IF <from_tab>-transmitter IS INITIAL.
          CREATE OBJECT <from_tab>-transmitter.
        ENDIF.
        IF <to_tab>-receiver IS NOT INITIAL.
          <to_tab>-receiver->shut_down( ). "receiver clearing
          FREE <to_tab>-receiver.
        ENDIF.
        CREATE OBJECT <to_tab>-receiver
          EXPORTING
            io_transmitter = <from_tab>-transmitter
            io_sel_to      = o_to
            i_to_field     = <to_tab>-field_label.
      ENDLOOP.
    ENDIF.

    DATA(o_alv) = CAST cl_gui_alv_grid( e_dragdropobj->dragsourcectrl ).
    zcl_smd_common=>refresh( EXPORTING i_obj = o_alv ).

    o_alv ?= e_dragdropobj->droptargetctrl.
    o_to->raise_selection_done( ).


  endmethod.
ENDCLASS.
