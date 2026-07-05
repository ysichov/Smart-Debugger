class ZCL_SMD_TABLE_VIEWER definition
  public
  inheriting from ZCL_SMD_POPUP
  create public .

public section.

  types:
    BEGIN OF t_elem,
        field TYPE fieldname,
        elem  TYPE ddobjname,
      END OF t_elem .

  data M_LANG type DDLANGUAGE .
  data M_TABNAME type TABNAME .
  data MO_ALV type ref to CL_GUI_ALV_GRID .
  data MO_SEL type ref to ZCL_SMD_SEL_OPT .
  data MR_TABLE type ref to DATA .
  data MO_SEL_PARENT type ref to CL_GUI_CONTAINER .
  data MO_ALV_PARENT type ref to CL_GUI_CONTAINER .
  data MT_ALV_CATALOG type LVC_T_FCAT .
  data:
    mt_fields      TYPE TABLE OF t_elem .
  data MO_SEL_WIDTH type I .
  data M_VISIBLE type C .
  data M_STD_TBAR type X .
  data M_SHOW_EMPTY type I .
  "data MO_WINDOW type ref to CL_ACE_WINDOW . "TODO

  methods CONSTRUCTOR
    importing
      !I_TNAME type ANY optional
      !I_ADDITIONAL_NAME type STRING optional
      !IR_TAB type ref to DATA optional.
      "!IO_WINDOW type ref to CL_ACE_WINDOW .
  methods REFRESH_TABLE
    for event SELECTION_DONE of ZCL_SMD_SEL_OPT .
protected section.
private section.

  methods CREATE_POPUP .
  methods CREATE_ALV .
  methods CREATE_SEL_ALV .
  methods SET_HEADER .
  methods CREATE_FIELD_CAT
    importing
      !I_TNAME type TABNAME
    returning
      value(ET_CATALOG) type LVC_T_FCAT .
  methods TRANSLATE_FIELD
    importing
      !I_LANG type DDLANGUAGE
    changing
      !C_FLD type LVC_S_FCAT .
  methods HANDLE_TAB_TOOLBAR
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT .
  methods BEFORE_USER_COMMAND
    for event BEFORE_USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods HANDLE_USER_COMMAND
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods HANDLE_DOUBLECLICK
    for event DOUBLE_CLICK of CL_GUI_ALV_GRID
    importing
      !ES_ROW_NO
      !E_COLUMN .
  methods ON_TABLE_CLOSE
    for event CLOSE of CL_GUI_DIALOGBOX_CONTAINER
    importing
      !SENDER .
ENDCLASS.



CLASS ZCL_SMD_TABLE_VIEWER IMPLEMENTATION.


  method BEFORE_USER_COMMAND.


    CASE e_ucomm.
      WHEN '&INFO'.
        DATA(url) = 'https://ysychov.wordpress.com/2020/02/10/simple-data-explorer/'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = url.
    ENDCASE.


  endmethod.


  method CONSTRUCTOR.


    DATA: comp_descr   TYPE abap_componentdescr,
          comp_notab   TYPE abap_component_tab,
          comp_tab2str TYPE abap_component_tab,
          comp_str     TYPE abap_component_tab,
          s            TYPE string,
          data         TYPE REF TO data.

    DATA: notab   TYPE REF TO data,
          tab2str TYPE REF TO data.

    DATA: handle_notab   TYPE REF TO cl_abap_structdescr,
          handle_tab2str TYPE REF TO cl_abap_structdescr,
          o_new_tab      TYPE REF TO cl_abap_tabledescr.

    FIELD-SYMBOLS: <notab>   TYPE STANDARD TABLE,
                   <tab2str> TYPE STANDARD TABLE,
                   <any_tab> TYPE ANY TABLE,
                   <temptab> TYPE ANY TABLE.

    super->constructor( i_additional_name = i_additional_name ).
    "mo_window = io_window. "TODO
    m_lang = sy-langu.
    mo_sel_width = 0.
    m_tabname = i_tname.
    create_popup( ).

    IF ir_tab IS NOT BOUND.
      zcl_smd_rtti=>create_table_by_name( EXPORTING i_tname = m_tabname CHANGING c_table = mr_table ).
    ELSE.
      FIELD-SYMBOLS:<any> TYPE any.
      ASSIGN ir_tab->* TO <any>.
      DATA o_tabl  TYPE REF TO cl_abap_tabledescr.
      DATA o_struc TYPE REF TO cl_abap_structdescr.
      o_tabl ?= cl_abap_typedescr=>describe_by_data( <any> ).
      TRY.
          o_struc ?= o_tabl->get_table_line_type( ).
          ASSIGN ir_tab->* TO <any_tab>.
          TRY.
              LOOP AT o_struc->components INTO DATA(comp).

                IF comp-type_kind NE 'h'.
                  comp_descr-name = comp-name.
                  comp_descr-type ?= o_struc->get_component_type( comp-name ).
                  APPEND comp_descr TO comp_notab.
                  APPEND comp_descr TO comp_tab2str.
                ELSE.
                  comp_descr-name = comp-name.
                  comp_descr-type ?= cl_abap_typedescr=>describe_by_data( s ).
                  APPEND comp_descr TO comp_tab2str.
                  APPEND comp_descr TO comp_str.

                  comp_descr-name = comp-name && '_REF'.
                  comp_descr-type ?= cl_abap_typedescr=>describe_by_data( data ).
                  APPEND comp_descr TO comp_tab2str.
                ENDIF.
              ENDLOOP.
            CATCH cx_sy_move_cast_error.
          ENDTRY.

          TRY.
              handle_notab  = cl_abap_structdescr=>create( comp_notab ).
              handle_tab2str  = cl_abap_structdescr=>create( comp_tab2str ).

              o_new_tab = cl_abap_tabledescr=>create(
                p_line_type  = handle_notab
                p_table_kind = cl_abap_tabledescr=>tablekind_std
                p_unique     = abap_false ).

              CREATE DATA notab TYPE HANDLE o_new_tab.

              o_new_tab = cl_abap_tabledescr=>create(
                p_line_type  = handle_tab2str
                p_table_kind = cl_abap_tabledescr=>tablekind_std
                p_unique     = abap_false ).

              CREATE DATA tab2str TYPE HANDLE o_new_tab.

              ASSIGN notab->* TO <notab>.
              MOVE-CORRESPONDING <any_tab> TO <notab>.
              ASSIGN tab2str->* TO <tab2str>.
              MOVE-CORRESPONDING <notab> TO <tab2str>.

              LOOP AT <any_tab> ASSIGNING FIELD-SYMBOL(<old_struc>).
                READ TABLE <tab2str> ASSIGNING FIELD-SYMBOL(<new_struc>) INDEX sy-tabix.
                LOOP AT comp_str INTO comp_descr.
                  ASSIGN COMPONENT comp_descr-name OF STRUCTURE <new_struc> TO FIELD-SYMBOL(<field>).
                  ASSIGN COMPONENT comp_descr-name OF STRUCTURE <old_struc> TO <temptab>.
                  <field> = | { icon_view_table } [{ lines( <temptab> ) }] |.
                  ASSIGN COMPONENT comp_descr-name  OF STRUCTURE <old_struc> TO <field>.
                  ASSIGN COMPONENT |{ comp_descr-name }_REF| OF STRUCTURE <new_struc> TO FIELD-SYMBOL(<ref>).
                  GET REFERENCE OF <field> INTO <ref>.
                ENDLOOP.
              ENDLOOP.

              GET REFERENCE OF <tab2str> INTO mr_table.
            CATCH cx_root.
              mr_table = ir_tab.
          ENDTRY.
        CATCH cx_sy_move_cast_error.  "no structure
          comp_descr-name = 'FIELD'.
          comp_descr-type ?= cl_abap_typedescr=>describe_by_data( s ).
          APPEND comp_descr TO comp_tab2str.

          handle_tab2str  = cl_abap_structdescr=>create( comp_tab2str ).
          o_new_tab = cl_abap_tabledescr=>create(
            p_line_type  = handle_tab2str
            p_table_kind = cl_abap_tabledescr=>tablekind_std
            p_unique     = abap_false ).

          CREATE DATA tab2str TYPE HANDLE o_new_tab.
          ASSIGN tab2str->* TO <tab2str>.
          ASSIGN ir_tab->* TO <any_tab>.

          LOOP AT <any_tab> ASSIGNING <old_struc>.
            APPEND INITIAL LINE TO <tab2str> ASSIGNING <new_struc>.
            ASSIGN COMPONENT 'FIELD' OF STRUCTURE <new_struc> TO <field>.
            <field> = <old_struc>.
          ENDLOOP.
          GET REFERENCE OF <tab2str> INTO mr_table.
      ENDTRY.
    ENDIF.

    create_alv( ).
    create_sel_alv( ).
    mo_alv->set_focus( mo_alv ).


  endmethod.


  method CREATE_ALV.


    DATA: layout TYPE lvc_s_layo,
          effect TYPE i,
          f4s    TYPE lvc_t_f4.

    FIELD-SYMBOLS: <table>   TYPE table.

    mo_alv = NEW #( i_parent = mo_alv_parent ).
    mt_alv_catalog = create_field_cat( m_tabname ).

    IF mt_alv_catalog IS INITIAL.
      RETURN. "todo show tables without structure
    ENDIF.

    ASSIGN mr_table->* TO <table>.
    set_header( ).
    layout-cwidth_opt = abap_true.
    layout-sel_mode = 'D'.



    SET HANDLER   before_user_command
                  handle_user_command
                  handle_tab_toolbar
                  handle_doubleclick
                  FOR mo_alv.

    CALL METHOD mo_alv->set_table_for_first_display
      EXPORTING
        i_save          = abap_true
        i_default       = abap_true
        is_layout       = layout
      CHANGING
        it_fieldcatalog = mt_alv_catalog
        it_outtab       = <table>.

    mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = mt_alv_catalog ).
    LOOP AT mt_alv_catalog ASSIGNING FIELD-SYMBOL(<catalog>).
      CLEAR <catalog>-key.
      DATA(f4) = VALUE lvc_s_f4( register = abap_true chngeafter = abap_true fieldname = <catalog>-fieldname ).
      INSERT f4 INTO TABLE f4s.
    ENDLOOP.

    mo_alv->register_f4_for_fields( it_f4 = f4s ).
    mo_alv->set_frontend_fieldcatalog( EXPORTING it_fieldcatalog = mt_alv_catalog ).

    LOOP AT mt_alv_catalog ASSIGNING FIELD-SYMBOL(<cat>) WHERE scrtext_l IS INITIAL.
      zcl_smd_common=>translate_field( CHANGING c_fld = <cat> ).
    ENDLOOP.

    mo_alv->set_frontend_fieldcatalog( EXPORTING it_fieldcatalog = mt_alv_catalog ).
    me->handle_user_command( EXPORTING e_ucomm = 'TECH' ).
    me->handle_user_command( EXPORTING e_ucomm = 'SHOW' ).
    mo_alv->set_toolbar_interactive( ).


  endmethod.


  method CREATE_FIELD_CAT.


    DATA: lr_field       TYPE REF TO data,
          lr_table_descr TYPE REF TO cl_abap_structdescr,
          lr_data_descr  TYPE REF TO cl_abap_datadescr,
          it_tabdescr    TYPE abap_compdescr_tab,
          texttab        TYPE tabname,
          lr_temp        TYPE REF TO data,
          name           TYPE string,
          dd04           TYPE dd04v.

    FIELD-SYMBOLS: <tab>   TYPE STANDARD TABLE,
                   <struc> TYPE any,
                   <field> TYPE any.

    ASSIGN mr_table->* TO <tab>.
    CREATE DATA lr_temp LIKE LINE OF <tab>.
    ASSIGN lr_temp->* TO <struc>.

    TRY.
        lr_table_descr ?= cl_abap_typedescr=>describe_by_data_ref( lr_temp ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    it_tabdescr[] = lr_table_descr->components[].
    zcl_smd_ddic=>get_text_table( EXPORTING i_tname = i_tname IMPORTING e_tab = texttab ).

    LOOP AT it_tabdescr INTO DATA(ls)
       WHERE type_kind NE 'h'
         AND type_kind NE 'l'.
      DATA(ind) = sy-tabix.

      ASSIGN COMPONENT ls-name OF STRUCTURE <struc> TO <field>.
      GET REFERENCE OF <field> INTO lr_field.
      lr_data_descr ?= cl_abap_typedescr=>describe_by_data_ref( lr_field ).
      name = lr_data_descr->absolute_name.
      REPLACE ALL OCCURRENCES OF '\TYPE=' IN name WITH ''.
      APPEND VALUE #( field = ls-name elem = name ) TO mt_fields.

      CLEAR dd04.
      CALL FUNCTION 'DDIF_DTEL_GET'
        EXPORTING
          name          = CONV ddobjname( name )
          langu         = m_lang
        IMPORTING
          dd04v_wa      = dd04
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.

      APPEND INITIAL LINE TO et_catalog ASSIGNING FIELD-SYMBOL(<catalog>).

      <catalog>-col_pos = ind.
      <catalog>-style = zcl_smd_common=>c_white.
      <catalog>-fieldname = ls-name.
      <catalog>-f4availabl = abap_true.

      IF dd04 IS INITIAL.
        <catalog>-scrtext_s = <catalog>-scrtext_m = <catalog>-scrtext_l = <catalog>-reptext = <catalog>-fieldname = ls-name.
      ELSE.
        MOVE-CORRESPONDING dd04 TO <catalog>.
      ENDIF.
    ENDLOOP.


  endmethod.


  method CREATE_POPUP.


    mo_box = create( i_width = 800 i_hight = 150 ).
    "save new popup ref
    "TODO
*    APPEND INITIAL LINE TO lcl_appl=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>).
*    <popup>-parent = mo_window->mo_box.
*    <popup>-child = mo_box.

    SET HANDLER on_box_close FOR mo_box.

    CREATE OBJECT mo_splitter
      EXPORTING
        parent  = mo_box
        rows    = 1
        columns = 2
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter->set_column_mode( mode = mo_splitter->mode_absolute ).
    mo_splitter->set_column_width( id = 1 width = mo_sel_width ).

    CALL METHOD:
     mo_splitter->get_container(  EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = mo_sel_parent ),

      mo_splitter->get_container
       EXPORTING
        row       = 1
        column    = 2
       RECEIVING
        container = mo_alv_parent.

    SET HANDLER on_box_close FOR mo_box.


  endmethod.


  method CREATE_SEL_ALV.


    IF mo_sel IS INITIAL.
      mo_sel     = NEW #( io_viewer = me io_container = mo_sel_parent ).
      SET HANDLER refresh_table FOR mo_sel.
    ELSE.
      mo_sel->update_sel_tab( ).
    ENDIF.


  endmethod.


  method HANDLE_DOUBLECLICK.


    DATA: o_table_descr TYPE REF TO cl_tpda_script_tabledescr,
          table_clone   TYPE REF TO data.
    FIELD-SYMBOLS: <table>  TYPE STANDARD TABLE.

    CHECK es_row_no-row_id IS NOT INITIAL.
    ASSIGN mr_table->* TO  <table>.
    READ TABLE <table> INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<row>).
    ASSIGN COMPONENT e_column-fieldname  OF STRUCTURE <row> TO FIELD-SYMBOL(<val>).

    CASE e_column-fieldname.
      WHEN 'VALUE'.
        IF sy-subrc = 0.
          IF <val> = 'Table'.
            ASSIGN COMPONENT 'REF'  OF STRUCTURE <row> TO FIELD-SYMBOL(<ref>).
            "lcl_appl=>open_int_table( EXPORTING i_name = CONV #( e_column-fieldname ) it_ref = <ref> io_window = mo_window ). "TODO
          ENDIF.
        ELSE.
          TRY.
              o_table_descr ?= cl_tpda_script_data_descr=>factory( |{ m_additional_name }[ { es_row_no-row_id } ]-{ e_column-fieldname }| ).
              table_clone = o_table_descr->elem_clone( ).
              "todo  lcl_appl=>open_int_table( EXPORTING i_name = |{ m_additional_name }[ { es_row_no-row_id } ]-{ e_column-fieldname }| it_ref = table_clone io_window = mo_window ).
            CATCH cx_sy_move_cast_error.
          ENDTRY.
        ENDIF.
      WHEN 'STEP'.
      "todo  MOVE-CORRESPONDING <row> TO mo_window->m_prg.
      "todo  MOVE-CORRESPONDING <row> TO mo_window->mo_debugger->ms_stack.

        "todo mo_window->show_coverage( ).
        "todo mo_window->mo_debugger->show_step( ).
      WHEN OTHERS. "check if it is an embedded table.
        TRY.
            o_table_descr ?= cl_tpda_script_data_descr=>factory( |{ m_additional_name }[ { es_row_no-row_id } ]-{ e_column-fieldname }| ).
            table_clone = o_table_descr->elem_clone( ).
            "todo lcl_appl=>open_int_table( EXPORTING i_name = |{ m_additional_name }[ { es_row_no-row_id } ]-{ e_column-fieldname }| it_ref = table_clone io_window = mo_window ).
          CATCH cx_sy_move_cast_error.
        ENDTRY.
    ENDCASE.


  endmethod.


  method HANDLE_TAB_TOOLBAR.


    IF m_visible IS INITIAL.
      DATA(toolbar) = VALUE ttb_button(
       ( function = 'SEL_ON' icon = icon_arrow_left quickinfo = 'Show Select-Options'  butn_type = 0 )
       ( butn_type = 3 ) ).
    ENDIF.

    APPEND VALUE #( function = 'TECH' icon = icon_wd_caption quickinfo = 'Tech names'  butn_type = 0 ) TO toolbar.

"TODO
*    LOOP AT lcl_appl=>mt_lang INTO DATA(lang).
*      IF sy-tabix > 10.
*        EXIT.
*      ENDIF.
*      APPEND VALUE #( function = lang-spras icon = icon_foreign_trade quickinfo = lang-sptxt butn_type = 0 text = lang-sptxt ) TO toolbar.
*    ENDLOOP.

    toolbar = VALUE ttb_button( BASE toolbar
     ( function = 'SHOW'  icon = icon_list  quickinfo = 'Show empty columns'   butn_type = 0  )
     ( function = 'TBAR' icon = COND #( WHEN m_std_tbar IS INITIAL THEN icon_column_right ELSE icon_column_left )
        quickinfo = COND #( WHEN m_std_tbar IS INITIAL THEN 'Show standard ALV function'  ELSE 'Hide standard ALV function') )
     ( butn_type = 3 ) ).

    IF m_std_tbar IS INITIAL.
      e_object->mt_toolbar =  toolbar.
    ELSE.
      e_object->mt_toolbar =  toolbar = VALUE ttb_button( BASE toolbar ( LINES OF e_object->mt_toolbar ) ).
    ENDIF.


  endmethod.


  method HANDLE_USER_COMMAND.


    DATA: it_fields  TYPE lvc_t_fcat,
          clause(45),
          sel_width  TYPE i.

    FIELD-SYMBOLS: <table>  TYPE STANDARD  TABLE.
    ASSIGN mr_table->* TO <table>.
    mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = it_fields[] ).
    IF e_ucomm = 'SEL_ON' AND m_visible IS INITIAL.
      create_sel_alv( ).
      m_visible = abap_true.
      IF mo_sel_width = 0.
        sel_width = 500.
      ELSE.
        sel_width = mo_sel_width.
      ENDIF.

      mo_splitter->set_column_width( EXPORTING id = 1 width = sel_width ).
      mo_alv->set_toolbar_interactive( ).
      RETURN.
    ELSEIF e_ucomm = 'TBAR'.
      m_std_tbar = BIT-NOT  m_std_tbar.
    ELSE.
      IF e_ucomm = 'SHOW'.
        IF m_show_empty IS INITIAL.
          m_show_empty = 1.
        ELSE.
          CLEAR m_show_empty.
        ENDIF.
      ENDIF.

      LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<fields>) WHERE domname NE 'MANDT'.
        <fields>-col_pos = sy-tabix.
        CASE e_ucomm.

          WHEN 'SHOW'.
            IF m_show_empty = abap_false.
              <fields>-no_out = ' '.
            ELSE.
              clause = |{ <fields>-fieldname } IS NOT INITIAL|.
              LOOP AT <table> ASSIGNING FIELD-SYMBOL(<line>)  WHERE (clause).
                EXIT.
              ENDLOOP.
              IF sy-subrc NE 0.
                <fields>-no_out = abap_true.
              ENDIF.
            ENDIF.

          WHEN 'TECH'. "technical field name
            <fields>-scrtext_l = <fields>-scrtext_m = <fields>-scrtext_s =  <fields>-reptext = <fields>-fieldname.

          WHEN OTHERS. "header names translation
            "TODO
*            IF line_exists( lcl_appl=>mt_lang[ spras = e_ucomm ] ).
*              translate_field( EXPORTING i_lang = CONV #( e_ucomm )  CHANGING c_fld = <fields> ).
*              IF mo_sel IS BOUND.
*                READ TABLE mo_sel->mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) WITH KEY field_label = <fields>-fieldname.
*                IF sy-subrc = 0.
*                  IF <fields>-scrtext_l IS NOT INITIAL.
*                    <sel>-name = <fields>-scrtext_l.
*                  ENDIF.
*                  IF <sel>-name IS INITIAL.
*                    IF <fields>-reptext IS NOT INITIAL.
*                      <sel>-name = <fields>-reptext.
*                    ENDIF.
*                  ENDIF.
*                ENDIF.
*              ENDIF.
*            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDIF.

"TODO
*    IF line_exists( lcl_appl=>mt_lang[ spras = e_ucomm ] ).
*      m_lang = e_ucomm.
*      set_header( ).
*      mo_sel->set_value( i_field = 'SPRSL' i_low = m_lang ).
*    ENDIF.

    CALL METHOD mo_alv->set_frontend_fieldcatalog EXPORTING it_fieldcatalog = it_fields[].

    zcl_smd_common=>refresh( mo_alv ).
    IF mo_sel IS BOUND.
      IF  e_ucomm = 'HIDE' OR e_ucomm = 'SHOW' OR e_ucomm = 'UPDATE' .
        mo_sel->update_sel_tab( ).
      ENDIF.
      zcl_smd_common=>refresh( mo_sel->mo_sel_alv ).
      mo_sel->mo_sel_alv->refresh_table_display(  ).
    ENDIF.


  endmethod.


  method ON_TABLE_CLOSE.

    DATA: tabix LIKE sy-tabix.
    sender->free( ).

    "Free Memory TODO
*    LOOP AT lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>) WHERE alv_viewer IS NOT INITIAL.
*      IF <obj>-alv_viewer->mo_box = sender.
*        tabix = sy-tabix.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
    IF sy-subrc = 0.
*      FREE <obj>-alv_viewer->mr_table.
*      FREE <obj>-alv_viewer->mo_alv.
*
*      FREE <obj>-alv_viewer.
*      IF tabix NE 0.
*        DELETE lcl_appl=>mt_obj INDEX tabix.
*      ENDIF.
    ENDIF.

  endmethod.


  method REFRESH_TABLE.


    DATA: row    TYPE zcl_smd_sel_opt=>t_sel_row,
          filter TYPE lvc_t_filt.

    CLEAR filter.
    set_header( ).

    LOOP AT mo_sel->mt_sel_tab  ASSIGNING FIELD-SYMBOL(<sel>).
      LOOP AT <sel>-range INTO DATA(range).
        APPEND VALUE #( fieldname = <sel>-field_label
                              low = range-low
                             high = range-high
                             sign = range-sign
                           option = range-opti ) TO filter.
      ENDLOOP.
    ENDLOOP.

    IF mo_sel->mt_sel_tab IS NOT INITIAL.
      CALL METHOD mo_alv->set_filter_criteria
        EXPORTING
          it_filter = filter.
      zcl_smd_common=>refresh( mo_sel->mo_sel_alv ).
      zcl_smd_common=>refresh( mo_alv ).
      mo_sel->mo_debugger->handle_user_command( 'SHOW' ).
    ENDIF.

  endmethod.


  method SET_HEADER.


    DATA: text       TYPE as4text,
          header(80) TYPE c.

    SELECT SINGLE ddtext INTO text
      FROM dd02t
     WHERE tabname = m_tabname
       AND ddlanguage = m_lang.

    header = |{ m_tabname } - { text } { m_additional_name }|.
    mo_box->set_caption( header ).


  endmethod.


  method TRANSLATE_FIELD.


    DATA: dd04 TYPE dd04v.

    READ TABLE mt_fields INTO DATA(field) WITH KEY field = c_fld-fieldname.
    CHECK field-elem IS NOT INITIAL.
    CLEAR dd04.

    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING
        name          = CONV ddobjname( field-elem )
        langu         = i_lang
      IMPORTING
        dd04v_wa      = dd04
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc = 0.
      IF dd04-reptext IS NOT INITIAL.
        MOVE-CORRESPONDING dd04 TO c_fld.
      ENDIF.
    ENDIF.


  endmethod.
ENDCLASS.
