class ZCL_SMD_SEL_OPT definition
  public
  create public .

public section.

  data MO_DEBUGGER type ref to ZCL_SMD_TABLE_VIEWER .
  data MO_SEL_ALV type ref to CL_GUI_ALV_GRID .
  data MT_FCAT type LVC_T_FCAT .
  data:
    mt_sel_tab  TYPE TABLE OF zcl_smd_appl=>selection_display .
  data MS_LAYOUT type LVC_S_LAYO .

  events SELECTION_DONE .

  methods CONSTRUCTOR
    importing
      !IO_VIEWER type ref to ZCL_SMD_TABLE_VIEWER
      !IO_CONTAINER type ref to CL_GUI_CONTAINER .
  methods RAISE_SELECTION_DONE .
  methods UPDATE_SEL_TAB .
  methods SET_VALUE
    importing
      !I_FIELD type ANY
      !I_LOW type ANY optional
      !I_HIGH type ANY optional
      !I_CLEAR type XFELD default ABAP_TRUE .
  methods UPDATE_SEL_ROW
    changing
      !C_SEL_ROW type ZCL_SMD_APPL=>SELECTION_DISPLAY .
protected section.
private section.

  methods INIT_FCAT
    importing
      !I_DD_HANDLE type I .
  methods HANDLE_SEL_TOOLBAR
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT .
  methods ON_F4
    for event ONF4 of CL_GUI_ALV_GRID
    importing
      !ER_EVENT_DATA
      !ES_ROW_NO
      !E_FIELDNAME .
  methods ON_GRID_BUTTON_CLICK
    for event BUTTON_CLICK of CL_GUI_ALV_GRID
    importing
      !ES_COL_ID
      !ES_ROW_NO .
  methods ON_DATA_CHANGED
    for event DATA_CHANGED of CL_GUI_ALV_GRID
    importing
      !ER_DATA_CHANGED .
  methods ON_DATA_CHANGED_FINISHED
    for event DATA_CHANGED_FINISHED of CL_GUI_ALV_GRID
    importing
      !E_MODIFIED .
  methods HANDLE_USER_COMMAND
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods HANDLE_DOUBLECLICK
    for event DOUBLE_CLICK of CL_GUI_ALV_GRID
    importing
      !ES_ROW_NO
      !E_COLUMN .
  methods HANDLE_CONTEXT_MENU_REQUEST
    for event CONTEXT_MENU_REQUEST of CL_GUI_ALV_GRID
    importing
      !E_OBJECT .
ENDCLASS.



CLASS ZCL_SMD_SEL_OPT IMPLEMENTATION.


  method CONSTRUCTOR.

    DATA: effect     TYPE i,
          handle_alv TYPE i.

    mo_debugger = io_viewer.
    mo_sel_alv = NEW #( i_parent = io_container ).
    update_sel_tab( ).
    CREATE OBJECT zcl_smd_appl=>c_dragdropalv.
    effect =  cl_dragdrop=>copy. " + cl_dragdrop=>move.

    CALL METHOD zcl_smd_appl=>c_dragdropalv->add
      EXPORTING
        flavor     = 'Line'
        dragsrc    = abap_true
        droptarget = abap_true
        effect     = effect.

    CALL METHOD zcl_smd_appl=>c_dragdropalv->get_handle IMPORTING handle = handle_alv.
    ms_layout-s_dragdrop-col_ddid = handle_alv.
    init_fcat( handle_alv ).
    ms_layout-cwidth_opt = abap_true.
    ms_layout-col_opt = abap_true.
    ms_layout-ctab_fname = 'COLOR'.
    ms_layout-stylefname = 'STYLE'.

    "fields for F4 event handling
    DATA(gt_f4) = VALUE  lvc_t_f4( register   = abap_true chngeafter = abap_true
                             ( fieldname  = 'LOW'  )
                             ( fieldname  = 'HIGH'  ) ).

    mo_sel_alv->register_f4_for_fields( it_f4 = gt_f4 ).
    mo_sel_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
    mo_sel_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    SET HANDLER handle_user_command
                handle_sel_toolbar
                zcl_smd_dragdrop=>drag
                zcl_smd_dragdrop=>drop
                on_data_changed
                on_data_changed_finished
                on_grid_button_click
                handle_context_menu_request
                handle_doubleclick
                on_f4 FOR mo_sel_alv.

    CALL METHOD mo_sel_alv->set_table_for_first_display
      EXPORTING
        i_save          = abap_true
        i_default       = abap_true
        is_layout       = ms_layout
      CHANGING
        it_outtab       = mt_sel_tab[]
        it_fieldcatalog = mt_fcat.

    mo_sel_alv->set_toolbar_interactive( ).


  endmethod.


  method HANDLE_CONTEXT_MENU_REQUEST.


    DATA: func  TYPE ui_func,
          funcs TYPE ui_functions.

    DATA(index) = zcl_smd_common=>get_selected( mo_sel_alv ).

    IF index IS NOT INITIAL.
      READ TABLE mt_sel_tab INTO DATA(sel) INDEX index.
    ENDIF.

    e_object->get_functions( IMPORTING fcodes = DATA(fcodes) ). "Inactivate all standard functions

    LOOP AT fcodes INTO DATA(fcode) WHERE fcode NE '&OPTIMIZE'.
      func = fcode-fcode.
      APPEND func TO funcs.
    ENDLOOP.

    e_object->hide_functions( funcs ).
    e_object->add_separator( ).

    IF sel-range[]  IS NOT INITIAL OR index IS INITIAL.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'SEL_CLEAR'
          text  = 'Clear Select-Options'.
    ENDIF.

    IF sel-receiver IS NOT INITIAL OR index IS INITIAL.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'DELR'
          text  = 'Delete receiver'.
    ENDIF.


  endmethod.


  method HANDLE_DOUBLECLICK.


    DATA: it_bdcdata TYPE TABLE OF  bdcdata.

    CHECK es_row_no-row_id IS NOT INITIAL.

    READ TABLE mt_sel_tab INDEX es_row_no-row_id INTO DATA(sel).
    APPEND VALUE #( program = 'SAPLSD_ENTRY' dynpro = '1000' dynbegin = abap_true ) TO it_bdcdata.
    APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WB_DISPLAY' ) TO it_bdcdata.

    IF e_column = 'ELEMENT'.
      SET PARAMETER ID 'DTYP' FIELD sel-element.
      APPEND VALUE #( fnam = 'RSRD1-DDTYPE' fval = abap_true ) TO it_bdcdata.
      CALL TRANSACTION 'SE11' USING it_bdcdata MODE 'E'.
    ELSEIF e_column = 'DOMAIN'.
      SET PARAMETER ID 'DOM' FIELD sel-domain.
      APPEND VALUE #( fnam = 'RSRD1-DOMA' fval = abap_true ) TO it_bdcdata.
      CALL TRANSACTION 'SE11' USING it_bdcdata MODE 'E'.
    ELSE.
      CALL FUNCTION 'DOCU_CALL'
        EXPORTING
          id                = 'DE'
          langu             = mo_debugger->m_lang
          object            = sel-element
          typ               = 'E'
          displ             = abap_true
          displ_mode        = 3
          use_sec_langu     = abap_true
          display_shorttext = abap_true.
    ENDIF.


  endmethod.


  method HANDLE_SEL_TOOLBAR.


    e_object->mt_toolbar[] = VALUE #( butn_type = 0 disabled = ''
     ( function = 'SEL_OFF' icon = icon_arrow_right    quickinfo = 'Hide' )
     ( function = 'SEL_CLEAR' icon = icon_delete_row    quickinfo = 'Clear Select-Options' ) ).


  endmethod.


  method HANDLE_USER_COMMAND.


    DATA: sel_width TYPE i.

    IF e_ucomm = 'SEL_OFF'. "Hide select-options alv

      mo_debugger->m_visible = ''.

      sel_width = 0.
      CALL METHOD mo_debugger->mo_splitter->get_column_width
        EXPORTING
          id                = 1
        IMPORTING
          result            = mo_debugger->mo_sel_width
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3.

      CALL METHOD mo_debugger->mo_splitter->set_column_width
        EXPORTING
          id    = 1
          width = sel_width.
      mo_debugger->mo_alv->set_toolbar_interactive( ).
      RETURN.
    ENDIF.

    IF e_ucomm = 'SEL_CLEAR' OR e_ucomm = 'DELR'. "clear all selections
      mo_sel_alv->get_selected_rows( IMPORTING et_index_rows = DATA(sel_rows) ).

      LOOP AT sel_rows INTO DATA(row).
        READ TABLE mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) INDEX row-index.
        IF e_ucomm = 'SEL_CLEAR'.
          CLEAR : <sel>-low, <sel>-high, <sel>-sign, <sel>-opti, <sel>-range.
        ELSEIF e_ucomm = 'DELR'.
          IF <sel>-receiver IS NOT INITIAL.
            <sel>-receiver->shut_down( ).
            FREE <sel>-receiver.
            CLEAR <sel>-receiver.
            CLEAR <sel>-inherited.
          ENDIF.
        ENDIF.
        update_sel_row( CHANGING c_sel_row = <sel> ).
      ENDLOOP.
      RAISE EVENT selection_done.
    ENDIF.

    zcl_smd_common=>refresh( mo_debugger->mo_alv ).
    RAISE EVENT selection_done.


  endmethod.


  method INIT_FCAT.


    mt_fcat = VALUE #(
     ( fieldname = 'IND'         coltext = '№'  outputlen = 3 style = '00000003' )
     ( fieldname = 'FIELD_LABEL' coltext = 'Label'  outputlen = 30 dragdropid = i_dd_handle )
     ( fieldname = 'SIGN'        coltext = 'SIGN'   tech = abap_true )
     ( fieldname = 'OPTI'        coltext = 'Option' tech = abap_true )
     ( fieldname = 'OPTION_ICON' coltext = 'Option' icon = abap_true outputlen = 4 style = cl_gui_alv_grid=>mc_style_button )
     ( fieldname = 'LOW'         coltext = 'From data' edit = abap_true lowercase = abap_true outputlen = 45 style = cl_gui_alv_grid=>mc_style_f4 col_opt = abap_true  )
     ( fieldname = 'HIGH'        coltext = 'To data' edit = abap_true lowercase = abap_true outputlen = 45 style = cl_gui_alv_grid=>mc_style_f4  col_opt = abap_true )
     ( fieldname = 'MORE_ICON'   coltext = 'Range' icon = abap_true  style = cl_gui_alv_grid=>mc_style_button  )
     ( fieldname = 'RANGE'   tech = abap_true  )
     ( fieldname = 'INHERITED'   coltext = 'Inh.' icon = abap_true outputlen = 4 seltext = 'Inherited' style = '00000003')
     ( fieldname = 'EMITTER'    coltext = 'Emit.' icon = abap_true outputlen = 4 seltext = 'Emitter' style = '00000003')
     ( fieldname = 'NAME' coltext = 'Field name'  outputlen = 60 style = '00000003')
     ( fieldname = 'ELEMENT' coltext = 'Data element'  outputlen = 15 style = '00000209' )
     ( fieldname = 'DOMAIN'  coltext = 'Domain'  outputlen = 15 style = '00000209' )
     ( fieldname = 'DATATYPE' coltext = 'Type'  outputlen = 5 style = '00000003')
     ( fieldname = 'LENGTH' coltext = 'Length'  outputlen = 5 style = '00000003')
     ( fieldname = 'TRANSMITTER'   tech = abap_true  )
     ( fieldname = 'RECEIVER'    tech = abap_true  )
     ( fieldname = 'COLOR'    tech = abap_true  ) ).


  endmethod.


  method ON_DATA_CHANGED.


    DATA: start TYPE i,
          time  TYPE sy-uzeit.

    FIELD-SYMBOLS: <field> TYPE any.

    LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<cells>).
      READ TABLE mt_sel_tab INDEX <cells>-row_id ASSIGNING FIELD-SYMBOL(<tab>).
      ASSIGN COMPONENT <cells>-fieldname OF STRUCTURE <tab> TO <field>.
      READ TABLE mo_debugger->mt_alv_catalog WITH KEY fieldname = <tab>-field_label INTO DATA(cat).

      IF <field> IS NOT INITIAL AND <cells>-value IS INITIAL.
        READ TABLE <tab>-range INTO DATA(second) INDEX 2.
        IF sy-subrc = 0.
          IF ( <cells>-fieldname = 'LOW' AND <tab>-high IS INITIAL ) OR  ( <cells>-fieldname = 'HIGH' AND <tab>-low IS INITIAL  ).
            DELETE <tab>-range INDEX 1.
          ELSE.
            CLEAR second.
          ENDIF.
        ENDIF.
      ENDIF.

      IF cat-convexit = 'ALPHA' AND NOT  <cells>-value CA '+*'.
        <cells>-value = |{ <cells>-value ALPHA = IN }|.
        start = 128 - cat-dd_outlen.
        <cells>-value = <cells>-value+start(cat-dd_outlen).
      ENDIF.

      IF <cells>-value IS NOT INITIAL.
        IF <tab>-int_type = 'D'.
          DATA: date TYPE sy-datum.
          CALL FUNCTION 'CONVERT_DATE_INPUT'
            EXPORTING
              input                     = <cells>-value
              plausibility_check        = abap_true
            IMPORTING
              output                    = date
            EXCEPTIONS
              plausibility_check_failed = 1
              wrong_format_in_input     = 2
              OTHERS                    = 3.

          IF sy-subrc = 0.
            <cells>-value = |{ date DATE = USER }|.
          ENDIF.
        ELSEIF <tab>-int_type = 'T'.
          CALL FUNCTION 'CONVERT_TIME_INPUT'
            EXPORTING
              input                     = <cells>-value
            IMPORTING
              output                    = time
            EXCEPTIONS
              plausibility_check_failed = 1
              wrong_format_in_input     = 2
              OTHERS                    = 3.
          <cells>-value = time+0(2) && ':' && time+2(2) && ':' && time+4(2).
        ENDIF.
      ENDIF.
    ENDLOOP.
    CHECK sy-subrc = 0.

    IF second IS INITIAL.
      <field> = <cells>-value.
      er_data_changed->modify_cell( EXPORTING i_row_id = <cells>-row_id i_fieldname = <cells>-fieldname i_value = <cells>-value ).
    ELSE.
      <tab>-low = second-low.
      er_data_changed->modify_cell( EXPORTING i_row_id = <cells>-row_id i_fieldname = 'LOW' i_value = second-low ).
      IF second-high CO '0 '.
        CLEAR second-high.
      ENDIF.
      <tab>-high = second-high.
      er_data_changed->modify_cell( EXPORTING i_row_id = <cells>-row_id i_fieldname = 'HIGH' i_value = second-high ).

      <tab>-opti = second-opti.
      er_data_changed->modify_cell( EXPORTING i_row_id = <cells>-row_id i_fieldname = 'OPTI' i_value = second-opti ).
      <tab>-sign = second-sign.
      er_data_changed->modify_cell( EXPORTING i_row_id = <cells>-row_id i_fieldname = 'SIGN' i_value = second-sign ).
    ENDIF.

    update_sel_row( CHANGING c_sel_row = <tab> ).
    zcl_smd_common=>refresh( EXPORTING i_obj = mo_sel_alv i_layout = ms_layout ).
    raise_selection_done( ).


  endmethod.


  method ON_DATA_CHANGED_FINISHED.


    CHECK e_modified IS NOT INITIAL.
    RAISE EVENT selection_done.


  endmethod.


  method ON_F4.


    DATA: return_tab TYPE STANDARD TABLE OF ddshretval,
          objects    TYPE TABLE OF objec,
          objec      TYPE objec,
          otype      TYPE otype,
          plvar      TYPE plvar,
          multiple   TYPE xfeld,
          clear      TYPE xfeld.

    IF e_fieldname = 'LOW'.
      multiple = abap_true.
    ENDIF.

    READ TABLE mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) INDEX es_row_no-row_id.
    DATA(fname) =  <sel>-field_label.

    zcl_smd_appl=>mt_sel[] = mt_sel_tab[].
    IF <sel>-element = 'HROBJID'.
      READ TABLE mt_sel_tab INTO DATA(sel) WITH KEY field_label = 'OTYPE'.
      otype = sel-low.
      READ TABLE mt_sel_tab INTO sel WITH KEY field_label = 'PLVAR'.
      IF sy-subrc = 0 AND sel-low IS NOT INITIAL.
        plvar = sel-low.
      ELSE.
        CALL FUNCTION 'RH_GET_ACTIVE_WF_PLVAR'
          IMPORTING
            act_plvar       = plvar
          EXCEPTIONS
            no_active_plvar = 1
            OTHERS          = 2.
      ENDIF.
    ELSEIF <sel>-element = 'PERSNO'.
      otype = 'P'.
    ENDIF.

    IF otype IS NOT INITIAL.
      CALL FUNCTION 'RH_OBJID_REQUEST'
        EXPORTING
          plvar            = plvar
          otype            = otype
          seark_begda      = sy-datum
          seark_endda      = sy-datum
          dynpro_repid     = sy-repid
          dynpro_dynnr     = sy-dynnr
          set_mode         = multiple
        IMPORTING
          sel_object       = objec
        TABLES
          sel_hrobject_tab = objects
        EXCEPTIONS
          OTHERS           = 6.
      IF sy-subrc = 0.
        clear = abap_true.
        LOOP AT objects INTO objec.
          IF e_fieldname = 'LOW'.
            set_value( EXPORTING i_field = <sel>-field_label i_low = objec-objid i_clear = clear ).
            CLEAR clear.
          ELSE.
            set_value( EXPORTING i_field = <sel>-field_label i_high = objec-objid i_clear = clear ).
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.

      CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
        EXPORTING
          tabname           = mo_debugger->m_tabname
          fieldname         = fname
          callback_program  = sy-repid
          callback_form     = 'CALLBACK_F4_SEL' "callback_method - doesn't work for local class
          multiple_choice   = multiple
        TABLES
          return_tab        = return_tab
        EXCEPTIONS
          field_not_found   = 1
          no_help_for_field = 2
          inconsistent_help = 3
          no_values_found   = 4
          OTHERS            = 5.

      IF sy-subrc = 0 AND lines( return_tab ) > 0.
        ASSIGN er_event_data->m_data->* TO FIELD-SYMBOL(<itab>).
        CLEAR <sel>-range.
        clear = abap_true.
        LOOP AT return_tab ASSIGNING FIELD-SYMBOL(<ret>) WHERE fieldname = fname.
          IF e_fieldname = 'LOW'.
            set_value( EXPORTING i_field = <sel>-field_label i_low = <ret>-fieldval i_clear = clear ).
            CLEAR clear.
          ELSE.
            set_value( EXPORTING i_field = <sel>-field_label i_high = <ret>-fieldval ).
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
    er_event_data->m_event_handled = abap_true.
    raise_selection_done( ).


  endmethod.


  method ON_GRID_BUTTON_CLICK.


    DATA: tabfield TYPE rstabfield,
          opt      TYPE rsoptions VALUE 'XXXXXXXXXX',
          sign     TYPE raldb_sign,
          option   TYPE raldb_opti.

    READ TABLE mt_sel_tab INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).
    CASE es_col_id.
      WHEN 'OPTION_ICON'. "edit select logical expression type
        CALL FUNCTION 'SELECT_OPTION_OPTIONS'
          EXPORTING
            selctext     = 'nnnn'
            option_list  = opt
          IMPORTING
            sign         = sign
            option       = option
          EXCEPTIONS
            delete_line  = 1
            not_executed = 2
            illegal_sign = 3
            OTHERS       = 4.
        IF sy-subrc = 0.
          <tab>-sign = sign.
          <tab>-opti = option.
        ELSEIF sy-subrc = 1.
          CLEAR: <tab>-low, <tab>-high,<tab>-sign, <tab>-opti, <tab>-range.
        ENDIF.
      WHEN 'MORE_ICON'. "edit ranges
        tabfield-tablename = mo_debugger->m_tabname.
        tabfield-fieldname = <tab>-field_label.

        CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
          EXPORTING
            title             = 'title'
            text              = 'text'
            tab_and_field     = tabfield
          TABLES
            range             = <tab>-range
          EXCEPTIONS
            no_range_tab      = 1
            cancelled         = 2
            internal_error    = 3
            invalid_fieldname = 4
            OTHERS            = 5.
        IF sy-subrc = 0.
          READ TABLE <tab>-range INDEX 1 INTO DATA(range).
          MOVE-CORRESPONDING range TO <tab>.
          IF <tab>-opti NE 'BT'.
            CLEAR <tab>-high.
          ENDIF.
        ENDIF.
    ENDCASE.
    update_sel_row( CHANGING c_sel_row = <tab> ).
    RAISE EVENT selection_done.


  endmethod.


  method RAISE_SELECTION_DONE.


    DATA: row TYPE zcl_smd_appl=>t_sel_row.

    zcl_smd_common=>refresh( mo_sel_alv ).
    RAISE EVENT selection_done.
    LOOP AT mt_sel_tab  ASSIGNING FIELD-SYMBOL(<sel>).
      IF <sel>-transmitter IS NOT INITIAL.
        MOVE-CORRESPONDING <sel> TO row.
        <sel>-transmitter->emit( e_row = row ).
      ENDIF.
    ENDLOOP.


  endmethod.


  method SET_VALUE.


    READ TABLE mt_sel_tab ASSIGNING FIELD-SYMBOL(<to>) WITH KEY field_label = i_field.
    CHECK sy-subrc = 0.
    IF i_low IS SUPPLIED.
      IF i_clear IS INITIAL.
        APPEND VALUE #( sign = 'I' opti = 'EQ' low = i_low high = i_high ) TO <to>-range.
      ELSE.
        CLEAR:  <to>-opti, <to>-sign,<to>-range.
        IF i_low IS SUPPLIED.
          <to>-low = i_low.
        ENDIF.
        IF i_high IS SUPPLIED.
          <to>-high = i_high.
        ENDIF.
        update_sel_row( CHANGING c_sel_row = <to> ).
      ENDIF.
    ELSE.
      CLEAR:  <to>-opti, <to>-sign.
      <to>-high = i_high.
      update_sel_row( CHANGING c_sel_row = <to> ).
    ENDIF.
    IF <to>-transmitter IS BOUND.
      DATA: row TYPE zcl_smd_appl=>t_sel_row.
      MOVE-CORRESPONDING <to> TO row.
      <to>-transmitter->emit( EXPORTING e_row = row ).
    ENDIF.


  endmethod.


  method UPDATE_SEL_ROW.
 "select patterns rules

    IF c_sel_row-high IS INITIAL AND c_sel_row-opti = 'BT'.
      CLEAR c_sel_row-opti.
    ENDIF.

    IF c_sel_row-low IS NOT INITIAL AND c_sel_row-opti IS INITIAL.
      c_sel_row-sign = 'I'.
      c_sel_row-opti = 'EQ'.
    ENDIF.

    IF c_sel_row-high IS NOT INITIAL AND c_sel_row-opti NE 'NB' .
      c_sel_row-opti = 'BT'.
    ENDIF.

    IF c_sel_row-sign IS INITIAL AND c_sel_row-opti IS INITIAL.
      CLEAR: c_sel_row-low, c_sel_row-low.
    ENDIF.

    IF c_sel_row-low CA  '*%+&' AND c_sel_row-opti <> 'NP'.
      c_sel_row-sign = 'I'.
      c_sel_row-opti = 'CP'.
    ENDIF.

    IF c_sel_row-opti IS NOT INITIAL AND c_sel_row-sign IS INITIAL.
      c_sel_row-sign = 'I'.
    ENDIF.

    TRY.
        c_sel_row-option_icon = zcl_smd_appl=>m_option_icons[ sign = c_sel_row-sign option = c_sel_row-opti ]-icon_name.
      CATCH cx_sy_itab_line_not_found.                  "#EC NO_HANDLER
    ENDTRY.

    IF c_sel_row-sign IS NOT INITIAL.
      READ TABLE c_sel_row-range ASSIGNING FIELD-SYMBOL(<range>) INDEX 1.
      IF sy-subrc NE 0.
        APPEND INITIAL LINE TO c_sel_row-range ASSIGNING <range>.
      ENDIF.
      MOVE-CORRESPONDING c_sel_row TO <range>.
      IF c_sel_row-opti NE 'BT' AND c_sel_row-opti NE 'NB' .
        CLEAR c_sel_row-high.
      ENDIF.
      IF c_sel_row-int_type = 'D' OR c_sel_row-int_type = 'T' .
        DO 2 TIMES.
          ASSIGN COMPONENT  COND string( WHEN sy-index = 1 THEN 'LOW' ELSE 'HIGH'  ) OF STRUCTURE <range> TO FIELD-SYMBOL(<field>).
          IF <field> IS INITIAL.
            CONTINUE.
          ENDIF.

          IF c_sel_row-int_type = 'D'.
            CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
              EXPORTING
                date_external            = <field>
              IMPORTING
                date_internal            = <field>
              EXCEPTIONS
                date_external_is_invalid = 1
                OTHERS                   = 2.
          ELSE.
            REPLACE ALL OCCURRENCES OF ':' IN <field> WITH ''.
          ENDIF.
        ENDDO.
      ENDIF.
    ENDIF.
    c_sel_row-more_icon = COND #( WHEN c_sel_row-range IS INITIAL THEN icon_enter_more    ELSE icon_display_more  ).

    IF c_sel_row-receiver IS BOUND AND c_sel_row-inherited IS INITIAL.
      c_sel_row-inherited = icon_businav_value_chain.
    ENDIF.


  endmethod.


  method UPDATE_SEL_TAB.


    IF mt_sel_tab[] IS NOT INITIAL.
      DATA(sel_tab_copy) = mt_sel_tab.
    ENDIF.
    CLEAR mt_sel_tab[].
    mo_debugger->mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = mo_debugger->mt_alv_catalog ).
    LOOP AT mo_debugger->mt_alv_catalog INTO DATA(catalog) WHERE domname NE 'MANDT'.
      DATA(ind) = sy-tabix.
      APPEND INITIAL LINE TO mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel_tab>).
      READ TABLE sel_tab_copy INTO DATA(copy) WITH KEY field_label = catalog-fieldname.

      IF sy-subrc = 0.
        MOVE-CORRESPONDING copy TO <sel_tab>.
      ELSE.
        <sel_tab>-option_icon = icon_led_inactive.
        <sel_tab>-more_icon = icon_enter_more.
      ENDIF.

      <sel_tab>-ind = ind.
      <sel_tab>-field_label = catalog-fieldname.
      <sel_tab>-int_type = catalog-inttype.
      <sel_tab>-element = catalog-rollname.
      <sel_tab>-domain =  catalog-domname.
      <sel_tab>-datatype = catalog-datatype.
      <sel_tab>-length = catalog-outputlen.
      zcl_smd_common=>translate_field( EXPORTING i_lang = mo_debugger->m_lang CHANGING c_fld = catalog ).
      <sel_tab>-name = catalog-scrtext_l.
    ENDLOOP.


  endmethod.
ENDCLASS.
