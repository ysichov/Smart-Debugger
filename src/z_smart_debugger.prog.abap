  REPORT z_ace. " ACE - Abap Code Explorer
  " & Multi-windows program for ABAP code analysis
  " &----------------------------------------------------------------------
  " & version: beta 0.5
  " & Git https://github.com/ysichov/ACE

  " & Written by Yurii Sychov
  " & e-mail:   ysichov@gmail.com
  " & blog:     https://ysychov.wordpress.com/blog/
  " & LinkedIn: https://www.linkedin.com/in/ysychov/
  " &----------------------------------------------------------------------

  " & External resources
  " & https://github.com/WegnerDan/abapMermaid
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (29) TEXT-002 FOR FIELD p_prog.
    SELECTION-SCREEN POSITION 35.
    PARAMETERS: p_prog  TYPE progname MATCHCODE OBJECT progname MODIF ID prg OBLIGATORY.
    SELECTION-SCREEN COMMENT (70) TEXT-001 FOR FIELD p_prog.
  SELECTION-SCREEN END OF LINE.
  PARAMETERS: p_dest   TYPE text255 MEMORY ID dest,
              p_model  TYPE text255 MEMORY ID model,
              p_apikey TYPE text255 MEMORY ID api.

  CLASS lcl_ace_ai DEFINITION DEFERRED.
  CLASS lcl_ace_data_receiver DEFINITION DEFERRED.
  CLASS lcl_ace_data_transmitter DEFINITION DEFERRED.
  CLASS lcl_ace_rtti_tree DEFINITION DEFERRED.
  CLASS lcl_ace_window DEFINITION DEFERRED.
  CLASS lcl_ace_table_viewer DEFINITION DEFERRED.
  CLASS lcl_ace_mermaid DEFINITION DEFERRED.

  CLASS lcl_ace_appl DEFINITION.

    PUBLIC SECTION.

      TYPES:
        BEGIN OF selection_display_s,
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
          transmitter TYPE REF TO lcl_ace_data_transmitter,
          receiver    TYPE REF TO lcl_ace_data_receiver,
          color       TYPE lvc_t_scol,
          style       TYPE lvc_t_styl,
        END OF selection_display_s,

        BEGIN OF t_sel_row,
          sign        TYPE tvarv_sign,
          opti        TYPE tvarv_opti,
          option_icon TYPE aqadh_type_of_icon,
          low         TYPE string, "aqadh_range_value,
          high        TYPE string, "aqadh_range_value,
          more_icon   TYPE aqadh_type_of_icon,
          range       TYPE aqadh_t_ranges,
        END OF t_sel_row.

      TYPES: BEGIN OF sign_option_icon_s,
               sign          TYPE tvarv_sign,
               option        TYPE tvarv_opti,
               icon_name(64) TYPE c,
               icon          TYPE aqadh_type_of_icon,
             END OF sign_option_icon_s,

             BEGIN OF var_table,
               step          TYPE i,
               stack         TYPE i,
               program(40)   TYPE c,
               eventtype(30) TYPE c,
               eventname(61) TYPE c,
               first         TYPE boolean,
               i_appear      TYPE boolean,
               del           TYPE boolean,
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
               done          TYPE boolean,
             END OF var_table,

             t_var_table TYPE STANDARD TABLE OF var_table WITH NON-UNIQUE DEFAULT KEY,

             BEGIN OF var_table_temp,
               step          TYPE i,
               stack         TYPE i,
               eventtype(30) TYPE c,
               eventname(61) TYPE c,
               name          TYPE string,
               value         TYPE string,
               first         TYPE boolean,
               i_appear      TYPE boolean,
               del           TYPE boolean,
               program(40)   TYPE c,
               leaf          TYPE string,
               path          TYPE string,
               type          TYPE string,
               instance      TYPE string,
               objname       TYPE string,
               ref           TYPE REF TO data,
             END OF var_table_temp,

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
               tree          TYPE REF TO lcl_ace_rtti_tree,
               time          LIKE sy-uname,
             END OF var_table_h,

             BEGIN OF t_obj,
               name       TYPE string,
               alv_viewer TYPE REF TO lcl_ace_table_viewer,
             END OF t_obj,

             BEGIN OF t_popup,
               parent TYPE REF TO cl_gui_dialogbox_container,
               child  TYPE REF TO cl_gui_dialogbox_container,
             END OF t_popup,

             BEGIN OF t_classes_types,
               name TYPE string,
               full TYPE string,
               type TYPE char1,
               key  TYPE salv_de_node_key,
             END OF t_classes_types,

             BEGIN OF t_lang,
               spras(4),
               sptxt    TYPE sptxt,
             END OF t_lang,

             BEGIN OF t_stack,
               step       TYPE i,
               stacklevel TYPE tpda_stack_level,
               line       TYPE tpda_sc_line,
               program    TYPE tpda_program,
               eventtype  TYPE tpda_event_type,
               eventname  TYPE tpda_event,
               prg        TYPE program,
               include    TYPE tpda_include,
             END OF t_stack,

             BEGIN OF t_step_counter,
               step       TYPE i,
               stacklevel TYPE tpda_stack_level,
               line       TYPE tpda_sc_line,
               eventtype  TYPE string,
               eventname  TYPE string,
               class      TYPE string,

               first      TYPE boolean,
               last       TYPE boolean,
               program    TYPE tpda_program,
               include    TYPE tpda_include,
               time       LIKE sy-uzeit,
             END OF t_step_counter,

             BEGIN OF ts_calls,
               class TYPE string,
               event TYPE string,
               type  TYPE string,
               name  TYPE string,
               outer TYPE string,
               inner TYPE string,
             END OF ts_calls,
             tt_calls TYPE STANDARD TABLE OF ts_calls WITH NON-UNIQUE KEY outer,

             BEGIN OF ts_kword,
               program  TYPE string,
               include  TYPE string,
               index    TYPE i,
               line     TYPE i,
               v_line   TYPE i, "virtual line in code Mix
               sub      TYPE boolean, "subcode: class/form...
               name     TYPE string,
               from     TYPE i,
               to       TYPE i,
               tt_calls TYPE tt_calls,
               "to_prog   TYPE string,
               "to_class  TYPE string,
               "to_evtype TYPE string,
               "to_evname TYPE string,
             END OF ts_kword,

             BEGIN OF ts_tree,
               kind(1),
               value   TYPE string,
               param   TYPE string,
               include TYPE program,
             END OF ts_tree,

             BEGIN OF ts_call,
               include TYPE string,
               ev_name TYPE string,
             END OF ts_call.


      CLASS-DATA: m_option_icons   TYPE TABLE OF sign_option_icon_s,
                  mt_lang          TYPE TABLE OF t_lang,
                  mt_obj           TYPE TABLE OF t_obj, "main object table
                  mt_popups        TYPE TABLE OF t_popup, "dependents popups

                  mo_dragdropalv   TYPE REF TO cl_dragdrop,
                  i_mermaid_active TYPE boolean.

      CLASS-DATA: mt_sel TYPE TABLE OF selection_display_s.

      CLASS-METHODS:
        init_icons_table,
        init_lang,
        check_mermaid,
        open_int_table IMPORTING it_tab    TYPE ANY TABLE OPTIONAL
                                 it_ref    TYPE REF TO data OPTIONAL
                                 i_name    TYPE string
                                 io_window TYPE REF TO lcl_ace_window.

  ENDCLASS.
  CLASS lcl_ace_popup DEFINITION.

    PUBLIC SECTION.
      CLASS-DATA m_counter              TYPE i.
      DATA: m_additional_name      TYPE string,
            mo_box                 TYPE REF TO cl_gui_dialogbox_container,
            mo_splitter            TYPE REF TO cl_gui_splitter_container,
            mo_splitter_imp_exp    TYPE REF TO cl_gui_splitter_container,
            mo_variables_container TYPE REF TO cl_gui_container,
            mo_tables_container    TYPE REF TO cl_gui_container,
            mo_mermaid             TYPE REF TO lcl_ace_mermaid.

      METHODS: constructor IMPORTING i_additional_name TYPE string OPTIONAL,
        create IMPORTING i_width       TYPE i
                         i_hight       TYPE i
                         i_name        TYPE text100 OPTIONAL
               RETURNING VALUE(ro_box) TYPE REF TO cl_gui_dialogbox_container,
        on_box_close FOR EVENT close OF cl_gui_dialogbox_container IMPORTING sender.

  ENDCLASS.


  CLASS lcl_ace_popup IMPLEMENTATION.

    METHOD constructor.
      m_additional_name = i_additional_name.

    ENDMETHOD.

    METHOD create.

      DATA: l_top  TYPE i,
            l_left TYPE i.

      ADD 1 TO m_counter.
      l_top  = l_left =  50 -  5 * ( m_counter DIV 5 ) - ( m_counter MOD 5 ) * 5.
      CREATE OBJECT ro_box
        EXPORTING
          width                       = i_width
          height                      = i_hight
          top                         = l_top
          left                        = l_left
          caption                     = i_name
          lifetime                    = 2
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          event_already_registered    = 6
          error_regist_event          = 7
          OTHERS                      = 8.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

    ENDMETHOD.

    METHOD on_box_close.
      LOOP AT lcl_ace_appl=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>) WHERE parent = sender .
        <popup>-child->free( ).
        CLEAR <popup>-child.
      ENDLOOP.
      IF sy-subrc <> 0.
        DELETE  lcl_ace_appl=>mt_popups WHERE child = sender.
      ENDIF.
      DELETE lcl_ace_appl=>mt_popups WHERE child IS INITIAL.
      sender->free( ).
      CLEAR mo_box.


    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_ace_ddic DEFINITION.

    PUBLIC SECTION.
      CLASS-METHODS: get_text_table IMPORTING i_tname TYPE tabname
                                    EXPORTING e_tab   TYPE tabname.
  ENDCLASS.

  CLASS lcl_ace_ddic IMPLEMENTATION.

    METHOD get_text_table.
      CALL FUNCTION 'DDUT_TEXTTABLE_GET'
        EXPORTING
          tabname   = i_tname
        IMPORTING
          texttable = e_tab.
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_ace_dragdrop_data DEFINITION."drag&drop data

    PUBLIC  SECTION.
      DATA: m_row    TYPE i,
            m_column TYPE lvc_s_col.

  ENDCLASS.

  CLASS lcl_ace_dragdrop DEFINITION.

    PUBLIC SECTION.
      CLASS-METHODS:
        drag FOR EVENT ondrag OF cl_gui_alv_grid IMPORTING e_dragdropobj e_row e_column ,
        drop FOR EVENT ondrop OF cl_gui_alv_grid IMPORTING e_dragdropobj e_row.

  ENDCLASS.

  CLASS lcl_ace_alv_common DEFINITION.

    PUBLIC SECTION.
      CONSTANTS: c_white(4) TYPE x VALUE '00000001'. "white background

      CLASS-METHODS:
        refresh IMPORTING i_obj TYPE REF TO cl_gui_alv_grid i_layout TYPE lvc_s_layo OPTIONAL i_soft TYPE char01 OPTIONAL,
        translate_field IMPORTING i_lang TYPE ddlanguage OPTIONAL CHANGING c_fld TYPE lvc_s_fcat,
        get_selected IMPORTING i_obj TYPE REF TO cl_gui_alv_grid RETURNING VALUE(e_index) TYPE i.

  ENDCLASS.

  CLASS lcl_ace_alv_common IMPLEMENTATION.

    METHOD refresh.

      DATA l_stable TYPE lvc_s_stbl.
      l_stable = 'XX'.
      IF i_layout IS SUPPLIED.
        i_obj->set_frontend_layout( i_layout ).
      ENDIF.
      i_obj->refresh_table_display( EXPORTING is_stable = l_stable i_soft_refresh = i_soft ).

    ENDMETHOD.

    METHOD translate_field.

      DATA: fields_info TYPE TABLE OF dfies.

      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname        = c_fld-tabname
          fieldname      = c_fld-fieldname
          langu          = i_lang
        TABLES
          dfies_tab      = fields_info
        EXCEPTIONS
          not_found      = 1
          internal_error = 2
          OTHERS         = 3.

      IF sy-subrc = 0.
        READ TABLE fields_info INDEX 1 INTO DATA(l_info).
        IF l_info-scrtext_l IS INITIAL AND l_info-scrtext_m IS INITIAL AND l_info-scrtext_s IS INITIAL.
          IF l_info-fieldtext IS NOT INITIAL.
            MOVE l_info-fieldtext TO: c_fld-reptext, c_fld-scrtext_l, c_fld-scrtext_m, c_fld-scrtext_s .
          ELSE.
            MOVE l_info-fieldname TO: c_fld-reptext, c_fld-scrtext_l, c_fld-scrtext_m, c_fld-scrtext_s .
          ENDIF.
        ELSE.
          c_fld-scrtext_l = l_info-scrtext_l.
          c_fld-scrtext_m = l_info-scrtext_m.
          c_fld-scrtext_s = l_info-scrtext_s.
          IF l_info-reptext IS NOT INITIAL.
            c_fld-reptext   = l_info-reptext.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDMETHOD.

    METHOD get_selected.

      i_obj->get_selected_cells( IMPORTING et_cell = DATA(sel_cells) ).
      IF lines( sel_cells ) > 0.
        e_index = sel_cells[ 1 ]-row_id.
      ELSE.
        i_obj->get_selected_rows( IMPORTING et_index_rows = DATA(sel_rows) ).
        IF lines( sel_rows ) > 0.
          e_index = sel_rows[ 1 ]-index.
        ENDIF.
      ENDIF.

    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_ace_rtti DEFINITION.

    PUBLIC SECTION.
      CLASS-METHODS:
        create_table_by_name IMPORTING i_tname TYPE tabname
                             CHANGING  c_table TYPE REF TO data,

        create_struc_handle IMPORTING i_tname  TYPE tabname
                            EXPORTING e_t_comp TYPE abap_component_tab
                                      e_handle TYPE REF TO cl_abap_structdescr.

  ENDCLASS.

  CLASS lcl_ace DEFINITION DEFERRED.


  CLASS lcl_ace_source_parser DEFINITION.

    PUBLIC SECTION.

      "CLASS-DATA: mv_step TYPE i.
      CLASS-METHODS: parse_tokens IMPORTING i_program TYPE program
                                            i_main TYPE boolean OPTIONAL
                                            i_include TYPE program
                                            io_debugger TYPE REF TO lcl_ace
                                            i_class TYPE string OPTIONAL
                                            i_evname TYPE string OPTIONAL,
        parse_call IMPORTING i_program TYPE program
                             i_include TYPE program
                             i_index TYPE i i_stack TYPE i
                             i_e_name TYPE string
                             i_e_type TYPE string
                             i_class TYPE string OPTIONAL
                             io_debugger TYPE REF TO lcl_ace,

        parse_class IMPORTING key TYPE lcl_ace_appl=>ts_kword
                              i_include TYPE program
                              i_call  TYPE lcl_ace_appl=>ts_calls
                              i_stack   TYPE i
                              io_debugger TYPE REF TO lcl_ace,

        parse_screen IMPORTING key TYPE lcl_ace_appl=>ts_kword
                               i_stack   TYPE i
                               i_call  TYPE lcl_ace_appl=>ts_calls
                               io_debugger TYPE REF TO lcl_ace,

        code_execution_scanner IMPORTING i_program TYPE program
                                         i_include TYPE program
                                         i_evname TYPE string OPTIONAL i_evtype TYPE string OPTIONAL
          i_stack TYPE i OPTIONAL io_debugger TYPE REF TO lcl_ace.


  ENDCLASS.

  CLASS lcl_ace DEFINITION.

    PUBLIC SECTION.
      TYPES: BEGIN OF t_obj,
               name TYPE string,
               obj  TYPE string,
             END OF t_obj,

             BEGIN OF t_sel_var,
               name   TYPE string,
               i_sel  TYPE boolean,
               refval TYPE REF TO data,
             END OF t_sel_var,

             BEGIN OF ts_if,
               if_ind      TYPE i,
               end_ind     TYPE i,
               before_else TYPE i,
             END OF ts_if,
             tt_if TYPE STANDARD TABLE OF ts_if WITH EMPTY KEY,

             BEGIN OF ts_line,
               cond       TYPE string,
               program    TYPE string,
               include    TYPE string,
               line       TYPE i,
               ind        TYPE i,
               class      TYPE string,
               ev_name    TYPE string,
               ev_type    TYPE string,
               stack      TYPE i,
               code       TYPE string,
               arrow      TYPE string,
               subname    TYPE string,
               del        TYPE flag,
               els_before TYPE i,
               els_after  TYPE i,
             END OF ts_line,
             tt_line TYPE TABLE OF ts_line WITH EMPTY KEY.

      DATA: mv_prog           TYPE prog,
            mv_dest           TYPE text255,
            mv_model          TYPE text255,
            mv_apikey         TYPE text255,
            mt_obj            TYPE TABLE OF t_obj,
            mt_compo          TYPE TABLE OF scompo,
            mt_locals         TYPE tpda_scr_locals_it,
            mt_globals        TYPE tpda_scr_globals_it,
            mt_ret_exp        TYPE tpda_scr_locals_it,
            m_counter         TYPE i,
            mt_steps          TYPE  TABLE OF lcl_ace_appl=>t_step_counter WITH NON-UNIQUE KEY line eventtype eventname, "source code steps
            mt_var_step       TYPE  TABLE OF lcl_ace_appl=>var_table_h,
            m_step            TYPE i,
            m_i_find          TYPE boolean,
            m_stop_stack      TYPE i,
            m_debug           TYPE x,
            m_refresh         TYPE boolean, "to refactor
            m_update          TYPE boolean,
            i_step            TYPE boolean,
            ms_stack_prev     TYPE   lcl_ace_appl=>t_stack,
            ms_stack          TYPE   lcl_ace_appl=>t_stack,
            i_history         TYPE boolean,
            m_hist_step       TYPE i,
            m_step_delta      TYPE i,
            mt_vars_hist_view TYPE STANDARD TABLE OF lcl_ace_appl=>var_table,
            mt_vars_hist      TYPE STANDARD TABLE OF lcl_ace_appl=>var_table,
            mt_state          TYPE STANDARD TABLE OF lcl_ace_appl=>var_table,
            mv_recurse        TYPE i,
            mt_classes_types  TYPE TABLE OF lcl_ace_appl=>t_classes_types,
            mo_window         TYPE REF TO lcl_ace_window,
            mv_f7_stop        TYPE boolean,
            m_f6_level        TYPE i,
            m_target_stack    TYPE i,
            mo_tree_local     TYPE REF TO lcl_ace_rtti_tree,
            mt_selected_var   TYPE TABLE OF t_sel_var,
            mv_stack_changed  TYPE boolean,
            m_variable        TYPE REF TO data,
            mt_new_string     TYPE TABLE OF  string,
            m_quick           TYPE tpda_scr_quick_info,
            mr_statements     TYPE RANGE OF string,
            ms_if             TYPE ts_if,
            mt_if             TYPE tt_if.

      METHODS:
        constructor IMPORTING i_prog   TYPE prog
                              i_dest   TYPE text255
                              i_model  TYPE text255
                              i_apikey TYPE text255,


        hndl_script_buttons IMPORTING i_stack_changed TYPE boolean
                            RETURNING VALUE(rv_stop)  TYPE boolean,
        show,
        get_code_flow RETURNING VALUE(results) TYPE tt_line,
        get_code_mix.

    PRIVATE SECTION.

      CONSTANTS: BEGIN OF c_kind,
                   struct LIKE cl_abap_typedescr=>kind_struct VALUE cl_abap_typedescr=>kind_struct,
                   table  LIKE cl_abap_typedescr=>kind_table VALUE cl_abap_typedescr=>kind_table,
                   elem   LIKE cl_abap_typedescr=>kind_elem VALUE cl_abap_typedescr=>kind_elem,
                   class  LIKE cl_abap_typedescr=>kind_class VALUE cl_abap_typedescr=>kind_class,
                   intf   LIKE cl_abap_typedescr=>kind_intf VALUE cl_abap_typedescr=>kind_intf,
                   ref    LIKE cl_abap_typedescr=>kind_ref VALUE cl_abap_typedescr=>kind_ref,
                 END OF c_kind.


  ENDCLASS.

  CLASS lcl_ace_mermaid DEFINITION INHERITING FROM lcl_ace_popup FRIENDS  lcl_ace.

    PUBLIC SECTION.

      DATA: mo_viewer       TYPE REF TO lcl_ace,
            mo_mm_container TYPE REF TO cl_gui_container,
            mo_mm_toolbar   TYPE REF TO cl_gui_container,
            mo_toolbar      TYPE REF TO cl_gui_toolbar,
            mo_diagram      TYPE REF TO object,
            mv_type         TYPE string,
            mv_direction    TYPE ui_func.

      METHODS: constructor IMPORTING io_debugger TYPE REF TO lcl_ace
                                     i_type      TYPE string,

        steps_flow IMPORTING i_direction TYPE ui_func OPTIONAL,
        magic_search IMPORTING i_direction TYPE ui_func OPTIONAL,
        add_toolbar_buttons,
        hnd_toolbar FOR EVENT function_selected OF cl_gui_toolbar IMPORTING fcode,
        open_mermaid IMPORTING i_mm_string TYPE string,
        refresh  .

  ENDCLASS.


  CLASS lcl_ace_ai_api DEFINITION.

    PUBLIC SECTION.

      METHODS:      constructor IMPORTING
                                  i_dest   TYPE text255
                                  i_model  TYPE text255
                                  i_apikey TYPE text255 ,
        call_openai   IMPORTING i_prompt TYPE string RETURNING VALUE(rv_answer) TYPE string,


        build_request
          IMPORTING
            i_prompt  TYPE string
          EXPORTING
            e_payload TYPE string ,

        send_request
          IMPORTING
            i_payload  TYPE string
          EXPORTING
            e_response TYPE string
            e_error    TYPE boolean,
        output
          IMPORTING
                    i_prompt         TYPE string
                    i_content        TYPE string
          RETURNING VALUE(rv_answer) TYPE string.

    PRIVATE SECTION.
      DATA mv_api_key TYPE string .
      DATA mv_dest TYPE text255 .
      DATA mv_model TYPE string .

  ENDCLASS.

  CLASS lcl_ace_ai_api IMPLEMENTATION.

    METHOD constructor.

      mv_dest = i_dest.
      mv_model = i_model.
      mv_api_key = i_apikey.

    ENDMETHOD.


    METHOD call_openai.
      DATA: prompt   TYPE string,
            payload  TYPE string,
            response TYPE string.

      "Build payload
      CALL METHOD build_request
        EXPORTING
          i_prompt  = i_prompt
        IMPORTING
          e_payload = payload.

      CALL METHOD me->send_request
        EXPORTING
          i_payload  = payload
        IMPORTING
          e_response = response
          e_error    = DATA(error).

      IF  error IS NOT INITIAL.
        rv_answer =  response.
      ELSE.
        rv_answer = output(
          EXPORTING
            i_prompt  = i_prompt
            i_content =  response ).
      ENDIF.
    ENDMETHOD.

    METHOD build_request.

      DATA:  payload TYPE string.

      payload = |{ '{ "model": "' && p_model && '", "messages": [{ "role": "user", "content": "' && i_prompt &&  '" }], "max_tokens": 10000 } ' }|.

      e_payload =  payload.
    ENDMETHOD.

    METHOD send_request.

      DATA: o_http_client TYPE REF TO if_http_client,
            response_body TYPE string,
            header        TYPE string.

      CALL METHOD cl_http_client=>create_by_destination
        EXPORTING
          destination              = p_dest
        IMPORTING
          client                   = o_http_client
        EXCEPTIONS
          argument_not_found       = 1
          destination_not_found    = 2
          destination_no_authority = 3
          plugin_not_active        = 4
          internal_error           = 5
          OTHERS                   = 13.
      IF sy-subrc = 2.
        e_response = 'Destination not found. Please check it in SM59 transaction'.
        e_error = abap_true.
        RETURN.
      ELSEIF sy-subrc <> 0.
        e_response = |cl_http_client=>create_by_destination error №' { sy-subrc }|.
        e_error = abap_true.
        RETURN.
      ENDIF.

      "mv_api_key = 'lmstudio'. "any name for local LLMs or secret key for external
      mv_api_key = p_apikey.
      "set request header
      o_http_client->request->set_header_field( name = 'Content-Type' value = 'application/json' ).
      o_http_client->request->set_header_field( name = 'Authorization' value = |Bearer { mv_api_key }| ).

      o_http_client->request->set_method('POST').

      "set payload
      o_http_client->request->set_cdata( i_payload ).

      CALL METHOD o_http_client->send
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5.
      IF sy-subrc = 0.
        CALL METHOD o_http_client->receive
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            OTHERS                     = 4.
        "Get response
        IF sy-subrc <> 0.
          response_body = o_http_client->response->get_data( ).
          e_response =  response_body.
        ELSE.
          response_body = o_http_client->response->get_data( ).
          IF  response_body IS NOT INITIAL.
            e_response =  response_body.
          ELSE.
            e_response = 'Call was succeesful, but got no response'.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDMETHOD.

    METHOD output.

      DATA: text(1000) TYPE c,
            string     TYPE string,
            content    TYPE string,
            reasoning  TYPE string.

      TYPES: BEGIN OF lty_s_message,
               role              TYPE string,
               content           TYPE string,
               reasoning_content TYPE string,
             END           OF lty_s_message,
             lty_t_message TYPE STANDARD TABLE OF lty_s_message WITH NON-UNIQUE DEFAULT KEY,
             BEGIN OF lty_s_choice,
               index         TYPE string,
               message       TYPE lty_s_message,
               logprobs      TYPE string,
               finish_reason TYPE string,
             END      OF lty_s_choice,
             BEGIN OF lty_s_base_chatgpt_res,
               id      TYPE string,
               object  TYPE string,
               created TYPE string,
               model   TYPE string,
               choices TYPE TABLE OF lty_s_choice WITH NON-UNIQUE DEFAULT KEY,
             END OF lty_s_base_chatgpt_res.

      DATA response TYPE lty_s_base_chatgpt_res.

      DATA:  binary TYPE xstring.

      DATA: o_x2c TYPE REF TO cl_abap_conv_in_ce.
      o_x2c = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
      binary = i_content.
      o_x2c->convert( EXPORTING input =  binary
                       IMPORTING data  =  string ).

      /ui2/cl_json=>deserialize( EXPORTING json =  string CHANGING data = response ).

      IF  response-choices IS NOT INITIAL.
        content = response-choices[ 1 ]-message-content.
        reasoning = response-choices[ 1 ]-message-reasoning_content.
      ELSE.
        content =  string.
        cl_abap_browser=>show_html(  html_string =  content title = 'Error (' ).
        RETURN.
      ENDIF.

      rv_answer =  content.

    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_ace_rtti_tree DEFINITION FINAL.

    PUBLIC SECTION.

      TYPES tt_table TYPE STANDARD TABLE OF lcl_ace_appl=>ts_tree
            WITH NON-UNIQUE DEFAULT KEY.

      DATA: main_node_key TYPE salv_de_node_key,
            m_prg_info    TYPE tpda_scr_prg_info,
            mo_viewer     TYPE REF TO lcl_ace,
            mo_tree       TYPE REF TO cl_salv_tree.

      METHODS constructor IMPORTING i_header   TYPE clike DEFAULT 'View'
                                    i_type     TYPE boolean OPTIONAL
                                    i_cont     TYPE REF TO cl_gui_container OPTIONAL
                                    i_debugger TYPE REF TO lcl_ace OPTIONAL.

      METHODS clear.

      METHODS add_buttons IMPORTING i_type TYPE boolean.
      METHODS add_node
        IMPORTING
                  i_name         TYPE string
                  i_rel          TYPE salv_de_node_key OPTIONAL
                  i_icon         TYPE salv_de_tree_image OPTIONAL
                  i_tree         TYPE lcl_ace_appl=>ts_tree OPTIONAL
        RETURNING VALUE(rv_node) TYPE salv_de_node_key.

      METHODS delete_node IMPORTING i_key TYPE salv_de_node_key.
      METHODS display IMPORTING io_debugger TYPE REF TO lcl_ace OPTIONAL.


    PRIVATE SECTION.
      CONSTANTS: BEGIN OF c_kind,
                   struct LIKE cl_abap_typedescr=>kind_struct VALUE cl_abap_typedescr=>kind_struct,
                   table  LIKE cl_abap_typedescr=>kind_table VALUE cl_abap_typedescr=>kind_table,
                   elem   LIKE cl_abap_typedescr=>kind_elem VALUE cl_abap_typedescr=>kind_elem,
                   class  LIKE cl_abap_typedescr=>kind_class VALUE cl_abap_typedescr=>kind_class,
                   intf   LIKE cl_abap_typedescr=>kind_intf VALUE cl_abap_typedescr=>kind_intf,
                   ref    LIKE cl_abap_typedescr=>kind_ref VALUE cl_abap_typedescr=>kind_ref,
                 END OF c_kind.

      DATA: tree_table TYPE tt_table.


      METHODS: hndl_double_click FOR EVENT double_click OF cl_salv_events_tree IMPORTING node_key,
        hndl_user_command FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function.

  ENDCLASS.

  CLASS lcl_ace_ai DEFINITION INHERITING FROM lcl_ace_popup.

    PUBLIC SECTION.
      DATA: mo_ai_box               TYPE REF TO cl_gui_dialogbox_container,
            mo_ai_splitter          TYPE REF TO cl_gui_splitter_container,
            mo_ai_toolbar_container TYPE REF TO cl_gui_container,
            mo_ai_toolbar           TYPE REF TO cl_gui_toolbar,
            mo_prompt_container     TYPE REF TO cl_gui_container,
            mo_answer_container     TYPE REF TO cl_gui_container,
            mo_prompt_text          TYPE REF TO cl_gui_textedit,
            mo_answer_text          TYPE REF TO cl_gui_textedit,
            mv_prompt               TYPE string,
            mv_answer               TYPE string.

      METHODS:  constructor IMPORTING i_source  TYPE sci_include
                                      io_parent TYPE REF TO cl_gui_dialogbox_container,
        add_ai_toolbar_buttons,
        hnd_ai_toolbar FOR EVENT function_selected OF cl_gui_toolbar IMPORTING fcode.

  ENDCLASS.

  CLASS lcl_ace_ai IMPLEMENTATION.

    METHOD constructor.
      super->constructor( ).

      mo_ai_box = create( i_name = 'ACE: Abap Code Explorer - AI chat' i_width = 1400 i_hight = 400 ).
      CREATE OBJECT mo_ai_splitter
        EXPORTING
          parent  = mo_ai_box
          rows    = 3
          columns = 1
        EXCEPTIONS
          OTHERS  = 1.

      "save new popup ref
      APPEND INITIAL LINE TO lcl_ace_appl=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>).
      <popup>-parent = io_parent.
      <popup>-child = mo_ai_box.

      SET HANDLER on_box_close FOR mo_ai_box.

      mo_ai_splitter->get_container(
           EXPORTING
             row       = 1
             column    = 1
           RECEIVING
             container = mo_ai_toolbar_container ).

      mo_ai_splitter->get_container(
        EXPORTING
          row       = 2
          column    = 1
        RECEIVING
          container = mo_prompt_container ).

      mo_ai_splitter->get_container(
        EXPORTING
          row       = 3
          column    = 1
        RECEIVING
          container = mo_answer_container  ).

      mo_ai_splitter->set_row_height( id = 1 height = '3' ).

      mo_ai_splitter->set_row_sash( id    = 1
                                    type  = 0
                                    value = 0 ).


      SET HANDLER on_box_close FOR mo_ai_box.


      CREATE OBJECT mo_prompt_text
        EXPORTING
          parent                 = mo_prompt_container
        EXCEPTIONS
          error_cntl_create      = 1
          error_cntl_init        = 2
          error_cntl_link        = 3
          error_dp_create        = 4
          gui_type_not_supported = 5
          OTHERS                 = 6.
      IF sy-subrc <> 0.
        on_box_close( mo_box ).
      ENDIF.

      CREATE OBJECT mo_answer_text
        EXPORTING
          parent                 = mo_answer_container
        EXCEPTIONS
          error_cntl_create      = 1
          error_cntl_init        = 2
          error_cntl_link        = 3
          error_dp_create        = 4
          gui_type_not_supported = 5
          OTHERS                 = 6.
      IF sy-subrc <> 0.
        on_box_close( mo_box ).
      ENDIF.

      mo_answer_text->set_readonly_mode( ).

      CREATE OBJECT mo_ai_toolbar EXPORTING parent = mo_ai_toolbar_container.
      add_ai_toolbar_buttons( ).
      mo_ai_toolbar->set_visible( 'X' ).

      "set prompt
      DATA string TYPE TABLE OF char255.

      APPEND INITIAL LINE TO string ASSIGNING FIELD-SYMBOL(<str>).
      <str> = 'Explain please the meaning of this ABAP code and provide a code review'.
      mv_prompt = <str>.
      APPEND INITIAL LINE TO string ASSIGNING <str>.


      LOOP AT i_source INTO DATA(line).
        APPEND INITIAL LINE TO string ASSIGNING <str>.
        <str> = line.
        mv_prompt = mv_prompt && <str>.
      ENDLOOP.

      mo_prompt_text->set_text_as_r3table( string ).
      cl_gui_control=>set_focus( mo_ai_box ).

    ENDMETHOD.

    METHOD add_ai_toolbar_buttons.

      DATA: button TYPE ttb_button,
            events TYPE cntl_simple_events,
            event  LIKE LINE OF events.

      button  = VALUE #(
       ( function = 'AI' icon = CONV #( icon_manikin_unknown_gender ) quickinfo = 'Ask AI' text = 'Ask AI' ) ).

      mo_ai_toolbar->add_button_group( button ).

*   Register events
      event-eventid = cl_gui_toolbar=>m_id_function_selected.
      event-appl_event = space.
      APPEND event TO events.

      mo_ai_toolbar->set_registered_events( events = events ).
      SET HANDLER me->hnd_ai_toolbar FOR mo_ai_toolbar.

    ENDMETHOD.

    METHOD hnd_ai_toolbar.

      DATA:  prompt TYPE string.

      CASE fcode.

        WHEN 'AI'.

          DATA(o_ai) = NEW lcl_ace_ai_api( i_model = p_model i_dest = p_dest i_apikey = p_apikey ).

          DATA text TYPE TABLE OF char255.
          CALL METHOD mo_prompt_text->get_text_as_stream
            IMPORTING
              text = text.
          CLEAR mv_prompt.
          LOOP AT text INTO DATA(line).
            CONCATENATE mv_prompt  line
                        "cl_abap_char_utilities=>newline
                   INTO mv_prompt.
          ENDLOOP.

          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN mv_prompt WITH ''.
          REPLACE ALL OCCURRENCES OF '#' IN mv_prompt WITH ''.
          REPLACE ALL OCCURRENCES OF '"' IN mv_prompt WITH ''''.
          DO 50 TIMES.
            REPLACE ALL OCCURRENCES OF '/' IN mv_prompt WITH ''.
          ENDDO.
          REPLACE ALL OCCURRENCES OF REGEX '[[:cntrl:]]' IN mv_prompt WITH ' '.

          mv_answer = o_ai->call_openai( mv_prompt ).
          mo_answer_text->set_textstream( mv_answer ).

      ENDCASE.

    ENDMETHOD.

  ENDCLASS.


  CLASS lcl_ace_window DEFINITION INHERITING FROM lcl_ace_popup .

    PUBLIC SECTION.

      TYPES: BEGIN OF ts_table,
               ref      TYPE REF TO data,
               kind(1),
               value    TYPE string,
               typename TYPE abap_abstypename,
               fullname TYPE string,
             END OF ts_table,

             BEGIN OF ts_calls,
               class TYPE string,
               event TYPE string,
               type  TYPE string,
               name  TYPE string,
               outer TYPE string,
               inner TYPE string,
             END OF ts_calls,
             tt_calls TYPE STANDARD TABLE OF ts_calls WITH NON-UNIQUE KEY outer,

             BEGIN OF ts_calls_line,
               program   TYPE string,
               include   TYPE string,
               class     TYPE string,
               eventtype TYPE string,
               meth_type TYPE i,
               eventname TYPE string,
               index     TYPE i,
             END OF ts_calls_line,
             tt_calls_line TYPE STANDARD TABLE OF ts_calls_line WITH NON-UNIQUE EMPTY KEY,

             BEGIN OF ts_vars,
               program   TYPE string,
               include   TYPE string,
               eventtype TYPE string,
               eventname TYPE string,
               line      TYPE i,
               name      TYPE string,
               type      TYPE string,
               icon      TYPE salv_de_tree_image,
             END OF ts_vars,

             BEGIN OF ts_event,
               program    TYPE program,
               include    TYPE program,
               type       TYPE string,
               stmnt_type TYPE string,
               stmnt_from TYPE string,
               stmnt_to   TYPE string,
               name       TYPE string,
               line       TYPE i,
             END OF ts_event,

             BEGIN OF ts_var,
               program TYPE string,
               include TYPE string,
               line    TYPE i,
               name    TYPE string,
               type    TYPE string,
             END OF ts_var,

             BEGIN OF ts_refvar,
               program TYPE string,
               name    TYPE string,
               class   TYPE string,
             END OF ts_refvar,

             tt_kword      TYPE STANDARD TABLE OF lcl_ace_appl=>ts_kword WITH EMPTY KEY,
             tt_vars       TYPE STANDARD TABLE OF ts_vars WITH EMPTY KEY,
             tt_calculated TYPE STANDARD TABLE OF ts_var WITH EMPTY KEY,
             tt_composed   TYPE STANDARD TABLE OF ts_var WITH EMPTY KEY,
             tt_events     TYPE STANDARD TABLE OF ts_event WITH EMPTY KEY,

             tt_refvar     TYPE STANDARD TABLE OF ts_refvar WITH EMPTY KEY,

             BEGIN OF ts_params,
               class     TYPE string,
               event     TYPE string,
               name      TYPE string,
               param     TYPE string,
               type      TYPE char1,
               preferred TYPE char1,
             END OF ts_params,
             tt_params TYPE STANDARD TABLE OF ts_params WITH KEY class event name param,

             BEGIN OF ts_int_tabs,
               eventtype TYPE string,
               eventname TYPE string,
               name      TYPE string,
               type      TYPE string,
             END OF ts_int_tabs,
             tt_tabs TYPE STANDARD TABLE OF ts_int_tabs WITH EMPTY KEY,

             BEGIN OF ts_prog,
               program    TYPE program,
               include    TYPE program,
               source_tab TYPE sci_include,
               scan       TYPE REF TO cl_ci_scan,
               t_keywords TYPE tt_kword,

               t_vars     TYPE tt_vars,
               selected   TYPE boolean,
             END OF ts_prog,
             tt_progs TYPE STANDARD TABLE OF ts_prog WITH EMPTY KEY,

             BEGIN OF ts_source,
               tt_progs      TYPE tt_progs,
               t_events      TYPE tt_events,
               t_calculated  TYPE tt_calculated,
               t_composed    TYPE tt_composed,
               t_params      TYPE tt_params,
               tt_tabs       TYPE tt_tabs,
               tt_calls_line TYPE tt_calls_line,
               tt_refvar     TYPE tt_refvar,
             END OF ts_source,

             BEGIN OF ts_locals,
               program    TYPE tpda_program,
               eventtype  TYPE tpda_event_type,
               eventname  TYPE tpda_event,
               loc_fill   TYPE boolean,
               locals_tab TYPE tpda_scr_locals_it,
               mt_fs      TYPE tpda_scr_locals_it,
             END OF ts_locals,

             BEGIN OF ts_globals,
               program     TYPE tpda_program,
               glob_fill   TYPE boolean,
               globals_tab TYPE tpda_scr_globals_it,
               mt_fs       TYPE tpda_scr_locals_it,
             END OF ts_globals,

             BEGIN OF ts_watch,
               program TYPE string,
               line    TYPE i,
             END OF ts_watch,
             tt_watch TYPE STANDARD  TABLE OF ts_watch WITH EMPTY KEY,

             BEGIN OF ts_bpoint,
               program TYPE string,
               include TYPE string,
               line    TYPE i,
               type    TYPE char1,
               del     TYPE char1,
             END OF ts_bpoint,
             tt_bpoints TYPE STANDARD TABLE OF ts_bpoint WITH EMPTY KEY.

      TYPES tt_table TYPE STANDARD TABLE OF ts_table
            WITH NON-UNIQUE DEFAULT KEY.

      DATA: m_version              TYPE x, " 0 - alpha, 01 - beta
            m_history              TYPE x,
            m_visualization        TYPE x,
            m_varhist              TYPE x,
            m_zcode                TYPE x,
            m_direction            TYPE x,
            m_prg                  TYPE tpda_scr_prg_info,
            m_debug_button         LIKE sy-ucomm,
            m_show_step            TYPE boolean,
            mt_bpoints             TYPE tt_bpoints,
            mo_viewer              TYPE REF TO lcl_ace,
            mo_splitter_code       TYPE REF TO cl_gui_splitter_container,
            mo_splitter_var        TYPE REF TO cl_gui_splitter_container,
            mo_splitter_steps      TYPE REF TO cl_gui_splitter_container,
            mo_toolbar_container   TYPE REF TO cl_gui_container,
            mo_importing_container TYPE REF TO cl_gui_container,
            mo_locals_container    TYPE REF TO cl_gui_container,
            mo_exporting_container TYPE REF TO cl_gui_container,
            mo_code_container      TYPE REF TO cl_gui_container,
            mo_imp_exp_container   TYPE REF TO cl_gui_container,
            mo_editor_container    TYPE REF TO cl_gui_container,
            mo_steps_container     TYPE REF TO cl_gui_container,
            mo_stack_container     TYPE REF TO cl_gui_container,
            mo_hist_container      TYPE REF TO cl_gui_container,
            mo_code_viewer         TYPE REF TO cl_gui_abapedit,
            mt_stack               TYPE TABLE OF lcl_ace_appl=>t_stack,
            mo_toolbar             TYPE REF TO cl_gui_toolbar,
            mo_salv_stack          TYPE REF TO cl_salv_table,
            mo_salv_steps          TYPE REF TO cl_salv_table,
            mo_salv_hist           TYPE REF TO cl_salv_table,
            mt_breaks              TYPE tpda_bp_persistent_it,
            mt_watch               TYPE tt_watch,
            mt_coverage            TYPE tt_watch,
            mt_calls               TYPE TABLE OF lcl_ace_appl=>ts_call,
            m_hist_depth           TYPE i,
            m_start_stack          TYPE i,
            mt_source              TYPE STANDARD  TABLE OF ts_source,
            ms_sources             TYPE ts_source,
            mt_params              TYPE STANDARD  TABLE OF ts_params,
            mt_locals_set          TYPE STANDARD TABLE OF ts_locals,
            mt_globals_set         TYPE STANDARD TABLE OF ts_globals.

      METHODS: constructor IMPORTING i_debugger TYPE REF TO lcl_ace i_additional_name TYPE string OPTIONAL,
        add_toolbar_buttons,
        hnd_toolbar FOR EVENT function_selected OF cl_gui_toolbar IMPORTING fcode,
        set_program IMPORTING i_include TYPE program,
        set_program_line IMPORTING i_line LIKE sy-index OPTIONAL,
        set_mixprog_line IMPORTING i_line LIKE sy-index OPTIONAL,
        create_code_viewer,
        show_stack,
        show_coverage,
        on_stack_double_click FOR EVENT double_click OF cl_salv_events_table IMPORTING row column,
        on_editor_double_click  FOR EVENT dblclick OF cl_gui_abapedit IMPORTING sender,
        on_editor_border_click  FOR EVENT border_click OF cl_gui_abapedit IMPORTING line cntrl_pressed_set shift_pressed_set.

  ENDCLASS.

  CLASS lcl_ace IMPLEMENTATION.



    METHOD constructor.

      CONSTANTS: c_mask TYPE x VALUE '01'.

      mv_prog = i_prog.
      mv_dest = i_dest.
      mv_model = i_model.
      mv_apikey = i_apikey.

      i_step = abap_on.
      lcl_ace_appl=>check_mermaid( ).
      lcl_ace_appl=>init_lang( ).
      lcl_ace_appl=>init_icons_table( ).

      mo_window = NEW lcl_ace_window( me ).


      mo_tree_local = NEW lcl_ace_rtti_tree( i_header   = 'Objects & Code Flow'
                                         i_type     = 'L'
                                         i_cont     = mo_window->mo_locals_container
                                         i_debugger = me ).


      show( ).

    ENDMETHOD.

    METHOD hndl_script_buttons.

*      IF m_i_find = abap_true.
*        rv_stop = abap_true.
*        CLEAR m_i_find.
*        RETURN.
*      ENDIF.
*
*      IF mo_window->m_debug_button = 'F5'.
*        rv_stop = abap_true.
*
*      ELSEIF mo_window->m_debug_button = 'F6'.
*        IF m_f6_level IS NOT INITIAL AND m_f6_level = ms_stack-stacklevel OR mo_window->m_history IS INITIAL.
*          CLEAR m_f6_level.
*          rv_stop = abap_true.
*        ENDIF.
*
*      ELSEIF mo_window->m_debug_button = 'F6END'.
*        IF mo_window->m_prg-flag_eoev IS NOT INITIAL AND m_target_stack = ms_stack-stacklevel.
*          rv_stop = abap_true.
*        ENDIF.
*      ELSEIF mo_window->m_debug_button = 'F7'.
*
*        IF m_target_stack = ms_stack-stacklevel.
*          CLEAR m_target_stack.
*          rv_stop = abap_true.
*        ENDIF.
*
*      ELSEIF mo_window->m_debug_button IS NOT INITIAL.
*        READ TABLE mo_window->mt_breaks WITH KEY inclnamesrc = mo_window->m_prg-include linesrc = mo_window->m_prg-line INTO DATA(gs_break).
*        IF sy-subrc = 0.
*          rv_stop = abap_true.
*        ELSE.
*
*          IF mo_window->m_debug_button = 'F6BEG' AND m_target_stack = ms_stack-stacklevel.
*            rv_stop = abap_true.
*          ELSE.
*            IF mo_window->m_history IS NOT INITIAL.
*              IF ms_stack-stacklevel = mo_window->m_hist_depth +  mo_window->m_start_stack.
*                "f6( )."to refactor
*              ELSE.
*                "f5( )."to refactor
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ELSE.
*        rv_stop = abap_true.
*      ENDIF.

    ENDMETHOD.

    METHOD show.

      DATA: tree        TYPE lcl_ace_appl=>ts_tree,
            cl_name     TYPE string,
            icon        TYPE salv_de_tree_image,
            forms_rel   TYPE salv_de_node_key,
            classes_rel TYPE salv_de_node_key,
            events_rel  TYPE salv_de_node_key,
            globals_rel TYPE salv_de_node_key,
            splits_prg  TYPE TABLE OF string,
            splits_incl TYPE TABLE OF string.

      IF  mo_window->m_prg-include IS INITIAL.
        mo_window->m_prg-program =  mo_window->m_prg-include = mv_prog.
      ENDIF.
      mo_window->set_program( CONV #( mo_window->m_prg-include ) ).
      IF mo_window->m_prg-include <> 'Code_Flow_Mix'.
        mo_window->show_coverage( ).
      ENDIF.
      IF  mo_window->m_prg-line IS INITIAL AND mo_window->mt_stack IS NOT INITIAL.
        mo_window->m_prg-line = mo_window->mt_stack[ 1 ]-line.
      ENDIF.
      mo_window->set_program_line( 1 ).

      READ TABLE mo_window->ms_sources-tt_progs WITH KEY program = mo_window->m_prg-program INTO DATA(prog).

      mo_window->show_stack( ).
      mo_tree_local->clear( ).
      SPLIT mo_window->m_prg-program AT '=' INTO TABLE splits_prg.
      CHECK splits_prg IS NOT INITIAL.
      tree-kind = 'F'.

      CASE mo_window->m_prg-eventtype.
        WHEN 'FUNCTION'.

          mo_tree_local->main_node_key = mo_tree_local->add_node( i_name = CONV #( mo_window->m_prg-eventname ) i_icon = CONV #( icon_folder ) i_tree = tree ).
        WHEN OTHERS.
          mo_tree_local->main_node_key = mo_tree_local->add_node( i_name = CONV #( splits_prg[ 1 ] ) i_icon = CONV #( icon_folder )  i_tree = tree ).
      ENDCASE.

      "global variable.
      CLEAR tree.
      LOOP AT prog-t_vars INTO DATA(var) WHERE eventtype IS INITIAL .
        IF globals_rel IS INITIAL.
          globals_rel = mo_tree_local->add_node( i_name = 'Global Vars' i_icon = CONV #( icon_header ) i_rel = mo_tree_local->main_node_key ).
        ENDIF.
        tree-value = var-line.
        tree-param = var-name.
        mo_tree_local->add_node( i_name = var-name i_icon = var-icon i_rel = globals_rel i_tree = tree ).
      ENDLOOP.

      "Virtual Start event - first executable step
      READ TABLE mo_window->mo_viewer->mt_steps INDEX 1 INTO DATA(step).
      IF step-line IS NOT INITIAL AND step-program = mo_window->m_prg-program.
        IF events_rel IS INITIAL.
          tree-kind = 'F'.
          events_rel = mo_tree_local->add_node( i_name = 'Events' i_icon = CONV #( icon_folder ) i_rel = mo_tree_local->main_node_key i_tree = tree ).
        ENDIF.
        tree-value = step-line.
        tree-kind = 'E'.
        tree-include = step-include.
        mo_tree_local->add_node( i_name = 'Code Flow start line' i_icon = CONV #( icon_oo_event ) i_rel = events_rel i_tree = tree ).
      ENDIF.

      CLEAR tree.
      LOOP AT mo_window->ms_sources-t_events INTO DATA(event).
        IF events_rel IS INITIAL.
          tree-kind = 'F'.
          events_rel = mo_tree_local->add_node( i_name = 'Events' i_icon = CONV #( icon_folder ) i_rel = mo_tree_local->main_node_key i_tree = tree ).
        ENDIF.
        tree-include = event-include.
        tree-value = event-line.
        mo_tree_local->add_node( i_name = event-name i_icon = CONV #( icon_oo_event ) i_rel = events_rel i_tree = tree ).
      ENDLOOP.

      "mo_tree_local->add_node( i_name = 'Code Flow' i_icon = CONV #( icon_enhanced_bo ) i_rel = mo_tree_local->main_node_key ).

      SORT mo_window->ms_sources-tt_calls_line BY program class eventtype meth_type eventname .
      LOOP AT mo_window->ms_sources-tt_calls_line INTO DATA(subs). "WHERE include IS NOT INITIAL.
        SPLIT subs-include AT '=' INTO TABLE splits_incl.
        READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = subs-include INTO prog.
        READ TABLE prog-t_keywords WITH KEY index = subs-index INTO DATA(keyword).
        DATA(form_name) = subs-eventname.

        IF subs-eventtype = 'FORM'.
          IF subs-program = splits_prg[ 1 ].
            IF forms_rel IS INITIAL.
              tree-kind = 'F'.
              forms_rel = mo_tree_local->add_node( i_name = 'Subroutines' i_icon = CONV #( icon_folder ) i_rel = mo_tree_local->main_node_key i_tree = tree ).
            ENDIF.
            CLEAR tree.
            tree-value = keyword-line.
            tree-include = subs-include.
            DATA(event_node) = mo_tree_local->add_node( i_name =  form_name i_icon = CONV #( icon_biw_info_source_ina ) i_rel =  forms_rel i_tree = tree ).

            CLEAR tree.
            LOOP AT mo_window->ms_sources-t_params INTO DATA(param) WHERE event = 'FORM' AND name = subs-eventname  AND param IS NOT INITIAL.

              CASE param-type.
                WHEN 'I'.
                  icon = icon_parameter_import.
                WHEN 'E'.
                  icon = icon_parameter_export.
              ENDCASE.
              tree-param = param-param.
              mo_tree_local->add_node( i_name =  param-param i_icon = icon i_rel =  event_node i_tree = tree ).

            ENDLOOP.
            CLEAR tree.

          ENDIF.

        ELSEIF subs-eventtype = 'METHOD'.
          CHECK subs-include IS NOT INITIAL.
          IF subs-class = splits_prg[ 1 ] OR subs-program = splits_prg[ 1 ].
            IF subs-class = splits_prg[ 1 ].

              DATA(last) = splits_incl[ lines( splits_incl ) ].
              IF last+0(2) <> 'CM'.
                CONTINUE.
              ENDIF.

            ENDIF.
            IF classes_rel IS INITIAL.
              IF subs-class = splits_prg[ 1 ].
              ELSE.
                tree-kind = 'F'.
                classes_rel = mo_tree_local->add_node( i_name = 'Local Classes' i_icon = CONV #( icon_folder ) i_rel = mo_tree_local->main_node_key i_tree = tree ).
              ENDIF.
            ENDIF.
            IF cl_name <> subs-class.
              tree-kind = 'F'.
              IF classes_rel IS NOT INITIAL.
                DATA(class_rel) = mo_tree_local->add_node( i_name = subs-class i_icon = CONV #( icon_folder ) i_rel = classes_rel i_tree = tree ).
              ELSE.
                class_rel = mo_tree_local->main_node_key.
              ENDIF.
              cl_name = subs-class.
            ENDIF.
            CASE subs-meth_type.
              WHEN 1.
                icon = icon_led_green.
              WHEN 2.
                icon = icon_led_yellow.
              WHEN 3.
                icon = icon_led_red.
              WHEN OTHERS.
                IF subs-eventname = 'CONSTRUCTOR'.
                  icon = icon_tools.
                ENDIF.
            ENDCASE.
            CLEAR tree.
            tree-kind = 'M'.
            tree-value = keyword-line.
            tree-include = subs-include.

            event_node = mo_tree_local->add_node( i_name =  form_name i_icon = icon i_rel =  class_rel i_tree = tree ).
            CLEAR tree-value.
            LOOP AT mo_window->ms_sources-t_params INTO param WHERE class = subs-class AND event = 'METHOD' AND name = subs-eventname  AND param IS NOT INITIAL.

              CASE param-type.
                WHEN 'I'.
                  icon = icon_parameter_import.
                WHEN 'E'.
                  icon = icon_parameter_export.
              ENDCASE.
              tree-param = param-param.

              mo_tree_local->add_node( i_name =  param-param i_icon = icon i_rel =  event_node i_tree = tree ).
            ENDLOOP.
            CLEAR tree.
          ENDIF.

        ELSEIF subs-eventtype = 'FUNCTION'.
          DATA: fname              TYPE rs38l_fnam,
                exception_list     TYPE TABLE OF  rsexc,
                export_parameter   TYPE TABLE OF  rsexp,
                import_parameter   TYPE TABLE OF  rsimp,
                changing_parameter TYPE TABLE OF    rscha,
                tables_parameter   TYPE TABLE OF    rstbl.

*          CLEAR tree.
*          tree-kind = 'F'.
*          DATA(func_rel) = mo_tree_local->add_node( i_name = subs-eventname i_icon = CONV #( icon_folder ) i_rel = mo_tree_local->main_node_key i_tree = tree ).
*          CLEAR tree.

          fname = ms_stack-eventname.
          CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE'
            EXPORTING
              funcname           = fname
            TABLES
              exception_list     = exception_list
              export_parameter   = export_parameter
              import_parameter   = import_parameter
              changing_parameter = changing_parameter
              tables_parameter   = tables_parameter
            EXCEPTIONS
              error_message      = 1
              function_not_found = 2
              invalid_name       = 3
              OTHERS             = 4.
          IF sy-subrc = 0.
            LOOP AT import_parameter INTO DATA(imp).
              mo_tree_local->add_node( i_name =  CONV #( imp-parameter ) i_icon = CONV #( icon_parameter_import ) i_rel =  mo_tree_local->main_node_key  i_tree = tree ).
            ENDLOOP.

            LOOP AT export_parameter INTO DATA(exp).
              mo_tree_local->add_node( i_name =  CONV #( exp-parameter ) i_icon = CONV #( icon_parameter_export ) i_rel =  mo_tree_local->main_node_key  i_tree = tree ).
            ENDLOOP.
            LOOP AT changing_parameter INTO DATA(change).
              mo_tree_local->add_node( i_name =  CONV #( change-parameter ) i_icon = CONV #( icon_parameter_changing ) i_rel =  mo_tree_local->main_node_key  i_tree = tree ).
            ENDLOOP.
            LOOP AT tables_parameter INTO DATA(table).
              mo_tree_local->add_node( i_name =  CONV #( table-parameter ) i_icon = CONV #( icon_parameter_table ) i_rel =  mo_tree_local->main_node_key  i_tree = tree ).
            ENDLOOP.
          ENDIF.
        ENDIF.

      ENDLOOP.

      mo_tree_local->display( ).

    ENDMETHOD.

    METHOD get_code_flow.

      DATA: add         TYPE boolean,
            sub         TYPE string,
            form        TYPE string,
            direction   TYPE string,
            ind2        TYPE i,
            start       TYPE i,
            end         TYPE i,
            bool        TYPE string,
            block_first TYPE i,
            els_before  TYPE i.

      DATA: line      TYPE ts_line,
            pre_stack TYPE ts_line,
            opened    TYPE i.

      READ TABLE mo_window->ms_sources-tt_progs INDEX 1 INTO DATA(prog).
      DATA(lt_selected_var) = mt_selected_var.

      DATA(steps) = mt_steps.
      SORT steps BY line eventtype eventname.
      DELETE ADJACENT DUPLICATES FROM steps.
      SORT steps BY step.
      DATA: yes TYPE xfeld.
      LOOP AT steps INTO DATA(step).
        READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
        READ TABLE prog-t_keywords WITH KEY line = step-line INTO DATA(keyword).
        LOOP AT keyword-tt_calls INTO DATA(call).

          READ TABLE lt_selected_var WITH KEY name = call-outer TRANSPORTING NO FIELDS.
          IF sy-subrc = 0 OR mt_selected_var IS INITIAL.
            yes = abap_true.
          ENDIF.

          READ TABLE lt_selected_var WITH KEY name = call-inner TRANSPORTING NO FIELDS.
          IF sy-subrc = 0 OR mt_selected_var IS INITIAL.
            yes = abap_true.
          ENDIF.
        ENDLOOP.
        IF yes = abap_true.
          LOOP AT keyword-tt_calls INTO call.
            READ TABLE lt_selected_var WITH KEY name = call-outer TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO  lt_selected_var ASSIGNING FIELD-SYMBOL(<selected>).
              <selected>-name = call-outer.
            ENDIF.

            READ TABLE lt_selected_var WITH KEY name = call-inner TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO  lt_selected_var ASSIGNING <selected>.
              <selected>-name = call-inner.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.

      "deleting empty cycles.
      DATA: prev    LIKE LINE OF mt_steps,
            pre_key TYPE string.

      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.

      LOOP AT steps ASSIGNING FIELD-SYMBOL(<step>).
        DATA(ind) = sy-tabix.
        READ TABLE prog-t_keywords WITH KEY line = <step>-line INTO DATA(key).
        IF prev IS NOT INITIAL.
          IF ( key-name = 'ENDDO' OR key-name = 'ENDWHILE' OR key-name = 'ENDLOOP' OR key-name = 'ENDIF' )  AND
             ( pre_key = 'DO' OR pre_key = 'LOOP'  OR pre_key = 'WHILE'  OR pre_key = 'IF' ).
            <step>-first = 'D'."to delete
            READ TABLE mt_steps INDEX ind - 1 ASSIGNING FIELD-SYMBOL(<step_prev>).
            <step_prev>-first = 'D'.
          ENDIF.
        ENDIF.
        prev = <step>.
        pre_key = key-name.
      ENDLOOP.

      DELETE steps WHERE first = 'D'.

      SORT steps BY step DESCENDING.

      "collecting dependents variables
      LOOP AT steps INTO step.

        READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.

        LOOP AT mo_window->ms_sources-t_calculated INTO DATA(calculated_var) WHERE line = step-line.
          READ TABLE lt_selected_var WITH KEY name = calculated_var-name TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
*          APPEND INITIAL LINE TO  mo_viewer->mt_selected_var ASSIGNING <selected>.
*          <selected>-name = calculated_var-name.

            LOOP AT mo_window->ms_sources-t_composed INTO DATA(composed_var) WHERE line = step-line.
              READ TABLE lt_selected_var WITH KEY name = composed_var-name TRANSPORTING NO FIELDS.
              IF sy-subrc <> 0.
                APPEND INITIAL LINE TO  lt_selected_var ASSIGNING <selected>.
                <selected>-name = composed_var-name.
              ENDIF.
            ENDLOOP.
          ENDIF.
          "adding returning values
*        LOOP AT source-t_params INTO DATA(param).
*          READ TABLE mo_viewer->mt_selected_var WITH KEY name = param-param TRANSPORTING NO FIELDS.
*          IF sy-subrc <> 0.
*            APPEND INITIAL LINE TO  mo_viewer->mt_selected_var ASSIGNING <selected>.
*            <selected>-name = param-param.
*          ENDIF.
*        ENDLOOP.
        ENDLOOP.

        READ TABLE prog-t_keywords WITH KEY line = step-line INTO keyword.
        LOOP AT keyword-tt_calls INTO call.

          READ TABLE lt_selected_var WITH KEY name = call-outer TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            APPEND INITIAL LINE TO  lt_selected_var ASSIGNING <selected>.
            <selected>-name = call-inner.
          ENDIF.
        ENDLOOP.

      ENDLOOP.
      SORT lt_selected_var.
      DELETE ADJACENT DUPLICATES FROM lt_selected_var.

      "collecting watchpoints
      CLEAR mo_window->mt_coverage.

      LOOP AT  steps INTO step.

        READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
        READ TABLE prog-t_keywords WITH KEY line = step-line INTO key.

        CLEAR line-cond.
        IF key-name = 'IF' OR key-name = 'ELSE' OR key-name = 'ENDIF' OR key-name = 'ELSEIF' OR
           key-name = 'CASE' OR key-name = 'WHEN' OR key-name = 'ENDCASE' OR
            key-name = 'DO' OR key-name = 'ENDDO'  OR key-name = 'LOOP'  OR key-name = 'ENDLOOP' OR key-name = 'WHILE' OR key-name = 'ENDWHILE'.
          APPEND INITIAL LINE TO mo_window->mt_watch ASSIGNING FIELD-SYMBOL(<watch>).

          <watch>-program = step-program.
          <watch>-line = line-line = step-line.

          INSERT line INTO results INDEX 1 ASSIGNING FIELD-SYMBOL(<line>).
          <line>-cond = key-name.
          <line>-ev_name = step-eventname.
          <line>-stack = step-stacklevel.
          <line>-include = step-include.
          <line>-class = step-class.
        ENDIF.

        CLEAR ind.
        LOOP AT  mo_window->ms_sources-t_calculated INTO calculated_var WHERE line = step-line.
          ADD 1 TO ind.
          LOOP AT mo_window->ms_sources-t_composed INTO composed_var WHERE line = step-line.
            READ TABLE lt_selected_var WITH KEY name = composed_var-name TRANSPORTING NO FIELDS.
            IF sy-subrc = 0.
              APPEND INITIAL LINE TO  lt_selected_var ASSIGNING <selected>.
              <selected>-name = composed_var-name.
            ENDIF.
          ENDLOOP.

          READ TABLE lt_selected_var WITH KEY name = calculated_var-name TRANSPORTING NO FIELDS.
          IF sy-subrc = 0 OR mt_selected_var IS INITIAL.

            APPEND INITIAL LINE TO mo_window->mt_watch ASSIGNING <watch>.
            <watch>-program = step-program.
            <watch>-line = line-line = step-line.

            "should be commented for Smart debugger
*          LOOP AT lines ASSIGNING <line> WHERE line = line-line AND event = step-eventname AND stack = step-stacklevel .
*            <line>-del = abap_true.
*          ENDLOOP.
            IF ind = 1.

              IF key-name <> 'PUBLIC' AND key-name <> 'ENDCLASS' AND  key-name <> 'ENDFORM' AND key-name <> 'FORM' AND
                key-name <> 'METHOD' AND key-name <> 'METHODS' AND key-name <> 'ENDMETHOD' AND key-name <> 'MODULE' .
                line-ev_name = step-eventname.
                line-stack = step-stacklevel.
                line-include = step-include.
                line-class = step-class.
                line-ev_type = step-eventtype.
                INSERT line INTO results INDEX 1.
              ENDIF.

            ENDIF.
          ENDIF.

        ENDLOOP.
        "if no variable - whole CodeMix flow
        IF sy-subrc <> 0 AND mt_selected_var IS INITIAL.
*          IF key-name <> 'ENDMETHOD' AND key-name <> 'ENDMODULE' AND  key-name <> 'ENDFORM' AND
*             key-name <> 'METHOD' AND key-name <> 'MODULE' AND  key-name <> 'FORM'.

          IF key-name <> 'PUBLIC' AND key-name <> 'ENDCLASS' AND  key-name <> 'ENDFORM' AND
            key-name <> 'METHOD' AND key-name <> 'METHODS' AND key-name <> 'MODULE' AND  key-name <> 'FORM'.
            READ TABLE results WITH KEY line = step-line include = step-include ev_type = step-eventtype ev_name = step-eventname TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              line-line = step-line.
              line-ev_name = step-eventname.
              line-stack = step-stacklevel.
              line-include = step-include.
              line-ev_type = step-eventtype.
              line-class = step-class.
              INSERT line INTO results INDEX 1.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.

      DELETE results WHERE del = abap_true.

      "getting code texts and calls params
      LOOP AT results ASSIGNING <line>.
        ind = sy-tabix.

        READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = <line>-include INTO prog.
        READ TABLE prog-t_keywords WITH KEY line = <line>-line INTO keyword.
        LOOP AT prog-scan->tokens FROM keyword-from TO keyword-to INTO DATA(token).
          IF token-str = 'USING' OR token-str = 'EXPORTING' OR token-str = 'IMPORTING' OR token-str = 'CHANGING'.
            EXIT.
          ENDIF.
          IF <line>-code IS INITIAL.
            <line>-code = token-str.
          ELSE.
            <line>-code = |{  <line>-code } { token-str }|.
          ENDIF.
        ENDLOOP.

        IF keyword-tt_calls IS NOT INITIAL.
          SORT keyword-tt_calls BY outer.
          DELETE ADJACENT DUPLICATES FROM keyword-tt_calls.
          LOOP AT keyword-tt_calls INTO call.
            <line>-subname = call-name.
            CHECK call-outer IS NOT INITIAL AND call-inner IS NOT INITIAL.
            IF sy-tabix <> 1.
              <line>-arrow = |{ <line>-arrow }, |.
            ENDIF.
            <line>-arrow  = |{ <line>-arrow  } { call-outer } { call-type } { call-inner }|.

            REPLACE ALL OCCURRENCES OF '''' IN <line>-subname WITH ''.
            REPLACE ALL OCCURRENCES OF '"' IN  <line>-code WITH ''.
          ENDLOOP.
        ENDIF.
        REPLACE ALL OCCURRENCES OF '''' IN <line>-subname WITH ''.
        REPLACE ALL OCCURRENCES OF '(' IN <line>-arrow WITH ''.
        REPLACE ALL OCCURRENCES OF ')' IN <line>-arrow WITH ''.
        REPLACE ALL OCCURRENCES OF '(' IN <line>-subname WITH ''.
        REPLACE ALL OCCURRENCES OF ')' IN <line>-subname WITH ''.
      ENDLOOP.

      "check subform execution steps existance and if/case structures build

      DATA: if_depth   TYPE i,
            when_count TYPE i.
      LOOP AT results ASSIGNING <line> WHERE code <> 'DO' AND code <> 'ENDDO' AND code <> 'WHILE' AND code <> 'ENDWHILE' AND code <> 'LOOP' AND code <> 'ENDLOOP' .
        <line>-ind = sy-tabix.

        FIELD-SYMBOLS: <if> TYPE ts_if.
        IF <line>-cond = 'IF' OR  <line>-cond = 'CASE'.
          ADD 1 TO if_depth.
          CLEAR when_count.
          APPEND INITIAL LINE TO mt_if  ASSIGNING <if>.
          <if>-if_ind = <line>-ind.

        ENDIF.

        IF <line>-cond = 'ENDIF' OR <line>-cond = 'ENDCASE'.
          IF <if> IS ASSIGNED.
            <if>-end_ind = <line>-ind.
            SUBTRACT 1 FROM if_depth.
            LOOP AT mt_if  ASSIGNING <if> WHERE end_ind = 0.
            ENDLOOP.
          ENDIF.
        ENDIF.

        IF <line>-cond = 'WHEN'.
          ADD 1 TO when_count.
        ENDIF.

        IF <line>-cond = 'ELSE' OR <line>-cond = 'ELSEIF'.

          <line>-els_before = els_before.
          <line>-els_after = <line>-ind.
          DATA(counter) = <line>-ind + 1.
          DO.
            READ TABLE results INDEX counter INTO line.
            IF sy-subrc <> 0.
              CLEAR <line>-els_after.
              EXIT.
            ENDIF.

            IF line-cond = 'ELSE' OR line-cond = 'ELSEIF'.
              CLEAR <line>-els_after.
              EXIT.
            ELSEIF  line-cond <> 'DO' AND line-cond <> 'ENDDO' AND line-cond <> 'WHILE' AND line-cond <> 'ENDWHILE' AND line-cond <> 'LOOP' AND line-cond <> 'ENDLOOP'.
              <line>-els_after = counter.
              EXIT.
            ELSE.
              ADD 1 TO counter.

            ENDIF.
          ENDDO.
          IF when_count = 1. "to refactor
*          <if>-if_ind = els_before.
*          CLEAR <line>-els_before.
          ENDIF.
        ENDIF.

        IF <line>-cond = 'WHEN'.

          <line>-els_before = els_before.
          <line>-els_after = <line>-ind.
          counter = <line>-ind + 1.
          DO.
            READ TABLE results INDEX counter INTO line.
            IF sy-subrc <> 0.
              CLEAR <line>-els_after.
              EXIT.
            ENDIF.

            IF line-cond = 'WHEN'.
              CLEAR <line>-els_after.
              EXIT.
            ELSEIF  line-cond <> 'DO' AND line-cond <> 'ENDDO' AND line-cond <> 'WHILE' AND line-cond <> 'ENDWHILE' AND line-cond <> 'LOOP' AND line-cond <> 'ENDLOOP'.
              <line>-els_after = counter.
              EXIT.
            ELSE.
              ADD 1 TO counter.

            ENDIF.
          ENDDO.
          IF when_count = 1.
            IF <if> IS ASSIGNED. "to refactor
              <if>-if_ind = els_before.
            ENDIF.
            CLEAR <line>-els_before.
          ENDIF.
        ENDIF.

        IF <line>-cond <> 'ELSE' AND <line>-cond <> 'ELSEIF' AND <line>-cond <> 'WHEN'.
          els_before = <line>-ind.
        ELSE.
          CLEAR   els_before.
        ENDIF.

        "READ TABLE results WITH KEY ev_type = <line>-subname TRANSPORTING NO FIELDS.
        " IF sy-subrc <> 0.
        "  CLEAR <line>-arrow.
        " ENDIF.
      ENDLOOP.

      IF mt_if IS INITIAL AND ms_if-if_ind IS NOT INITIAL.
        INSERT ms_if INTO mt_if INDEX 1.
      ENDIF.

      IF lines( results ) > 0.
        IF results[ lines( results ) ]-arrow IS NOT INITIAL.
          CLEAR results[ lines( results ) ]-arrow .
        ENDIF.
      ENDIF.


    ENDMETHOD.

    METHOD get_code_mix.

      DATA: flow_lines TYPE sci_include,
            splits     TYPE TABLE OF string,

            ind        TYPE i,
            prev_line  TYPE ts_line.

      DATA(lines) = get_code_flow( ).
      LOOP AT mo_window->ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<prog_mix>).
        CLEAR <prog_mix>-selected.
      ENDLOOP.
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = 'Code_Flow_Mix' ASSIGNING <prog_mix>.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO mo_window->ms_sources-tt_progs ASSIGNING <prog_mix>.
        INSERT INITIAL LINE INTO mo_window->mt_stack INDEX 1 ASSIGNING FIELD-SYMBOL(<stack_mix>).
        <prog_mix>-include = <stack_mix>-program = <stack_mix>-include = 'Code_Flow_Mix'.
      ENDIF.

      LOOP AT lines INTO DATA(line).
        READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = line-include INTO DATA(prog).
        READ TABLE prog-t_keywords WITH KEY line = line-line INTO DATA(keyword).

        APPEND INITIAL LINE TO <prog_mix>-t_keywords ASSIGNING FIELD-SYMBOL(<keyword_mix>).
        <keyword_mix> = keyword.
        <keyword_mix>-include = line-include.
        <keyword_mix>-program = line-program.

        DATA(from_row) = prog-scan->tokens[ keyword-from ]-row.
        DATA(to_row) = prog-scan->tokens[ keyword-to ]-row.
        DATA(spaces) = repeat( val = | | occ = ( line-stack - 1 ) * 3 ).
        DATA(dashes) = repeat( val = |-| occ = ( line-stack ) ).
        IF prev_line-ev_name <> line-ev_name OR prev_line-ev_type <> line-ev_type. "new event
          SPLIT line-include AT '=' INTO TABLE splits.

          APPEND INITIAL LINE TO flow_lines ASSIGNING FIELD-SYMBOL(<flow>).
          ind  = sy-tabix.
          IF line-class IS INITIAL.
            <flow> =  |"{ dashes } { line-ev_type } { line-ev_name } in { splits[ 1 ] }|.
          ELSE.
            <flow> =  |"{ dashes } { line-ev_type } { line-ev_name } in { line-class }|.
          ENDIF.
        ENDIF.

        <keyword_mix>-v_line = ind + 1.

        LOOP AT prog-source_tab FROM from_row TO to_row INTO DATA(source_line).
          APPEND INITIAL LINE TO flow_lines ASSIGNING <flow>.
          ind = sy-tabix.
          <flow> = |{ spaces }{ source_line }|.
        ENDLOOP.
        prev_line = line.
      ENDLOOP.

      mo_window->mo_code_viewer->set_text( table = flow_lines ).
      <prog_mix>-source_tab = flow_lines.
      <prog_mix>-selected = abap_true.
      mo_window->m_prg-include = 'Code_Flow_Mix'.
      mo_window->set_mixprog_line( ).
      mo_window->show_stack( ).
      mo_window->mo_box->set_caption( |Code Mix: { lines( lines ) } statements| ).
    ENDMETHOD.

  ENDCLASS.                    "lcl_ace IMPLEMENTATION

  CLASS lcl_ace_window IMPLEMENTATION.

    METHOD constructor.

      DATA:  text TYPE char100.
      text = i_debugger->mv_prog.

      super->constructor( ).
      mo_viewer = i_debugger.
      m_history = m_varhist =  m_zcode  = '01'.
      m_hist_depth = 9.

      mo_box = create( i_name =  text i_width = 1300 i_hight = 350 ).
      SET HANDLER on_box_close FOR mo_box.
      CREATE OBJECT mo_splitter
        EXPORTING
          parent  = mo_box
          rows    = 3
          columns = 1
        EXCEPTIONS
          OTHERS  = 1.

      mo_splitter->get_container(
        EXPORTING
          row       = 2
          column    = 1
        RECEIVING
          container = mo_code_container ).

      mo_splitter->get_container(
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = mo_toolbar_container ).

      mo_splitter->get_container(
        EXPORTING
          row       = 3
          column    = 1
        RECEIVING
          container = mo_tables_container ).

      mo_splitter->set_row_height( id = 1 height = '4' ).
      mo_splitter->set_row_height( id = 2 height = '70' ).

      mo_splitter->set_row_sash( id    = 1
                                 type  = 0
                                 value = 0 ).

      CREATE OBJECT mo_splitter_code
        EXPORTING
          parent  = mo_code_container
          rows    = 1
          columns = 2
        EXCEPTIONS
          OTHERS  = 1.

      mo_splitter_code->get_container(
        EXPORTING
          row       = 1
          column    = 2
        RECEIVING
          container = mo_editor_container ).

      mo_splitter_code->get_container(
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = mo_locals_container ).

      mo_splitter_code->set_column_width( EXPORTING id = 1 width = '25' ).

      SET HANDLER on_box_close FOR mo_box.

      CREATE OBJECT mo_toolbar EXPORTING parent = mo_toolbar_container.
      add_toolbar_buttons( ).
      mo_toolbar->set_visible( 'X' ).
      create_code_viewer( ).

    ENDMETHOD.

    METHOD add_toolbar_buttons.

      DATA: button TYPE ttb_button,
            events TYPE cntl_simple_events,
            event  LIKE LINE OF events.

      button  = VALUE #(
       ( function = 'RUN' icon = CONV #( icon_execute_object ) quickinfo = 'Run report' )
       ( COND #( WHEN mo_viewer->mv_dest IS NOT INITIAL
        THEN VALUE #( function = 'AI' icon = CONV #( icon_manikin_unknown_gender ) quickinfo = 'Ask AI' text = 'Ask AI' ) ) )

       ( COND #( WHEN lcl_ace_appl=>i_mermaid_active = abap_true
        THEN VALUE #( function = 'CALLS' icon = CONV #( icon_workflow_process ) quickinfo = ' Calls Flow' text = 'Diagrams' ) ) )
       ( function = 'CODEMIX' icon = CONV #( icon_wizard ) quickinfo = 'Calculations flow sequence' text = 'CodeMix' )
       ( function = 'CODE' icon = CONV #( icon_customer_warehouse ) quickinfo = 'Only Z' text = 'Only Z' )
       ( function = 'DEPTH' icon = CONV #( icon_next_hierarchy_level ) quickinfo = 'History depth level' text = |Depth { m_hist_depth }| )
       "( function = 'COVERAGE' icon = CONV #( icon_wizard ) quickinfo = 'Coverage ' text = 'Coverage' )
       ( butn_type = 3  )
       ( function = 'STEPS' icon = CONV #( icon_next_step ) quickinfo = 'Steps table' text = 'Steps' )
       ( butn_type = 3  )
       ( function = 'INFO' icon = CONV #( icon_bw_gis ) quickinfo = 'Documentation' text = '' )
                      ).

      mo_toolbar->add_button_group( button ).

*   Register events
      event-eventid = cl_gui_toolbar=>m_id_function_selected.
      event-appl_event = space.
      APPEND event TO events.

      mo_toolbar->set_registered_events( events = events ).
      SET HANDLER me->hnd_toolbar FOR mo_toolbar.

    ENDMETHOD.

    METHOD set_program.

      lcl_ace_source_parser=>parse_tokens( i_program = i_include i_include = i_include io_debugger = mo_viewer ).
      SORT ms_sources-t_params.
      DELETE ADJACENT DUPLICATES FROM ms_sources-t_params.
      IF mo_viewer->m_step IS INITIAL.
        lcl_ace_source_parser=>code_execution_scanner( i_program = i_include i_include = i_include io_debugger = mo_viewer ).
      ENDIF.

      LOOP AT ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<prog>).
        CLEAR <prog>-selected.
      ENDLOOP.

      READ TABLE ms_sources-tt_progs WITH KEY include = i_include ASSIGNING <prog>.
      IF sy-subrc = 0.

        <prog>-selected = abap_true.
        mo_code_viewer->set_text( table = <prog>-source_tab ).
      ENDIF.
    ENDMETHOD.

    METHOD set_program_line.

      TYPES: lntab TYPE STANDARD TABLE OF i.
      DATA: lines    TYPE lntab,
            line_num TYPE i.

      mo_code_viewer->remove_all_marker( 2 ).
      mo_code_viewer->remove_all_marker( 4 ).
      mo_code_viewer->remove_all_marker( 7 ).

*    "session breakpoints
      CALL METHOD cl_abap_debugger=>read_breakpoints
        EXPORTING
          main_program         = mo_viewer->mo_window->m_prg-program
        IMPORTING
          breakpoints_complete = DATA(points)
        EXCEPTIONS
          c_call_error         = 1
          generate             = 2
          wrong_parameters     = 3
          OTHERS               = 4.

      LOOP AT points INTO DATA(point) WHERE  include = m_prg-include.
        APPEND INITIAL LINE TO lines ASSIGNING FIELD-SYMBOL(<line>).
        <line> = point-line.

        APPEND INITIAL LINE TO mt_bpoints ASSIGNING FIELD-SYMBOL(<point>).
        MOVE-CORRESPONDING point TO <point>.
        <point>-type = 'S'.
      ENDLOOP.
      mo_code_viewer->set_marker( EXPORTING marker_number = 2 marker_lines = lines ).

*    "exernal breakpoints
      CALL METHOD cl_abap_debugger=>read_breakpoints
        EXPORTING
          main_program         = mo_viewer->mo_window->m_prg-include
          flag_other_session   = abap_true
        IMPORTING
          breakpoints_complete = points
        EXCEPTIONS
          c_call_error         = 1
          generate             = 2
          wrong_parameters     = 3
          OTHERS               = 4.

      CLEAR lines.

      LOOP AT points INTO point. "WHERE inclnamesrc = m_prg-include.
        APPEND INITIAL LINE TO lines ASSIGNING <line>.
        <line> = point-line.

        APPEND INITIAL LINE TO mt_bpoints ASSIGNING <point>.
        MOVE-CORRESPONDING point TO <point>.
        <point>-type = 'E'.
      ENDLOOP.
      mo_code_viewer->set_marker( EXPORTING marker_number = 4 marker_lines = lines ).

*    "watchpoints or coverage
*    CLEAR lines.
*    LOOP AT mt_watch INTO DATA(watch).
*      APPEND INITIAL LINE TO lines ASSIGNING <line>.
*      <line> = watch-line.
*    ENDLOOP.
*
*    "coverage
*    LOOP AT mt_coverage INTO DATA(coverage).
*      APPEND INITIAL LINE TO lines ASSIGNING <line>.
*      <line> = coverage-line.
*    ENDLOOP.

      IF i_line IS NOT INITIAL.

        IF i_line IS NOT INITIAL.
          line_num = i_line.
        ELSE.
          line_num = m_prg-line.
        ENDIF.

        CLEAR lines.
        "blue arrow - current line
        APPEND INITIAL LINE TO lines ASSIGNING <line>.
        <line> = i_line.

        mo_code_viewer->set_marker( EXPORTING marker_number = 7 marker_lines = lines ).
        mo_code_viewer->select_lines( EXPORTING from_line = i_line to_line = i_line ).
      ENDIF.
      mo_code_viewer->clear_line_markers( 'S' ).
      mo_code_viewer->draw( ).

    ENDMETHOD.

    METHOD set_mixprog_line.

      TYPES: lntab TYPE STANDARD TABLE OF i.
      DATA: lines    TYPE lntab,
            line_num TYPE i,
            flag     TYPE boolean,
            programs TYPE TABLE OF program.

      mo_code_viewer->remove_all_marker( 2 ).
      mo_code_viewer->remove_all_marker( 4 ).

      LOOP AT mo_viewer->mo_window->ms_sources-tt_progs INTO DATA(prog) WHERE include <> 'Code_Flow_Mix'.
        COLLECT prog-program INTO programs.
      ENDLOOP.

      READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY include = 'Code_Flow_Mix' INTO prog.

      flag = abap_true.
      DO 2 TIMES.

        LOOP AT programs INTO DATA(program).
*    "session breakpoints
          CALL METHOD cl_abap_debugger=>read_breakpoints
            EXPORTING
              main_program         = program
              flag_other_session   = flag
            IMPORTING
              breakpoints_complete = DATA(points)
            EXCEPTIONS
              c_call_error         = 1
              generate             = 2
              wrong_parameters     = 3
              OTHERS               = 4.

          LOOP AT points INTO DATA(point).
            CLEAR lines.
            READ TABLE prog-t_keywords WITH KEY include = point-include line = point-line INTO DATA(keyword).
            IF sy-subrc = 0.
              APPEND INITIAL LINE TO lines ASSIGNING FIELD-SYMBOL(<line>).
              <line> = keyword-v_line.

              APPEND INITIAL LINE TO mt_bpoints ASSIGNING FIELD-SYMBOL(<point>).
              MOVE-CORRESPONDING point TO <point>.

              IF flag IS INITIAL.
                <point>-type = 'S'.
              ELSE.
                <point>-type = 'E'.
              ENDIF.
            ENDIF.

          ENDLOOP.

        ENDLOOP.
        IF flag IS NOT INITIAL.
          mo_code_viewer->set_marker( EXPORTING marker_number = 4 marker_lines = lines )."external
        ELSE.
          mo_code_viewer->set_marker( EXPORTING marker_number = 2 marker_lines = lines )."Session
        ENDIF.
        CLEAR flag.
      ENDDO.

      IF i_line IS NOT INITIAL.

        IF i_line IS NOT INITIAL.
          line_num = i_line.
        ELSE.
          line_num = m_prg-line.
        ENDIF.

        CLEAR lines.
        "blue arrow - current line
        APPEND INITIAL LINE TO lines ASSIGNING <line>.
        <line> = i_line.

        mo_code_viewer->set_marker( EXPORTING marker_number = 7 marker_lines = lines ).
        mo_code_viewer->select_lines( EXPORTING from_line = i_line to_line = i_line ).
      ENDIF.
      mo_code_viewer->clear_line_markers( 'S' ).
      mo_code_viewer->draw( ).

    ENDMETHOD.

    METHOD create_code_viewer.

      DATA: events TYPE cntl_simple_events,
            event  TYPE cntl_simple_event.

      CHECK mo_code_viewer IS INITIAL.

      CREATE OBJECT mo_code_viewer
        EXPORTING
          parent           = mo_editor_container
          max_number_chars = 100.

      mo_code_viewer->init_completer( ).
      mo_code_viewer->upload_properties(
        EXCEPTIONS
          dp_error_create  = 1
          dp_error_general = 2
          dp_error_send    = 3
          OTHERS           = 4 ).

      event-eventid    = cl_gui_textedit=>event_double_click.
      APPEND event TO events.

      mo_code_viewer->set_registered_events( events ).
      mo_code_viewer->register_event_border_click( ).
      mo_code_viewer->register_event_break_changed( ).

      SET HANDLER on_editor_double_click FOR mo_code_viewer.
      SET HANDLER on_editor_border_click FOR mo_code_viewer.

      mo_code_viewer->set_statusbar_mode( statusbar_mode = cl_gui_abapedit=>true ).
      mo_code_viewer->create_document( ).
      mo_code_viewer->set_readonly_mode( 1 ).

    ENDMETHOD.

    METHOD show_stack.

      IF mo_salv_stack IS INITIAL.

        cl_salv_table=>factory(
          EXPORTING
            r_container  = mo_tables_container
          IMPORTING
            r_salv_table = mo_salv_stack
          CHANGING
            t_table      = mt_stack ).

        DATA:  o_column  TYPE REF TO cl_salv_column.

        DATA(o_columns) = mo_salv_stack->get_columns( ).
        "o_columns->set_optimize( 'X' ).

        o_column ?= o_columns->get_column( 'STEP' ).
        o_column->set_output_length( '3' ).
        o_column->set_short_text( 'STEP' ).

        o_column ?= o_columns->get_column( 'STACKLEVEL' ).
        o_column->set_output_length( '5' ).

        o_column ?= o_columns->get_column( 'PROGRAM' ).
        o_column->set_output_length( '20' ).
        o_column->set_long_text( 'Program/Class' ).
        o_column->set_medium_text( 'Program/Class' ).

        o_column ?= o_columns->get_column( 'INCLUDE' ).
        o_column->set_output_length( '40' ).

        o_column ?= o_columns->get_column( 'EVENTTYPE' ).
        o_column->set_output_length( '20' ).

        o_column ?= o_columns->get_column( 'EVENTNAME' ).
        o_column->set_output_length( '30' ).

        DATA(o_event) =  mo_salv_stack->get_event( ).

        " Event double click
        SET HANDLER on_stack_double_click FOR o_event.
        mo_salv_stack->display( ).
      ELSE.
        mo_salv_stack->refresh( ).
      ENDIF.

    ENDMETHOD.

    METHOD show_coverage.

      DATA: split TYPE TABLE OF string.
      CLEAR: mt_watch, mt_coverage. "mt_stack.
      "CHECK mt_stack IS INITIAL.
      LOOP AT mo_viewer->mt_steps INTO DATA(step).

        READ TABLE mt_stack WITH KEY include = step-include TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO mt_stack ASSIGNING FIELD-SYMBOL(<stack>).
          MOVE-CORRESPONDING step TO <stack>.


          SPLIT <stack>-program  AT '=' INTO TABLE split.
          <stack>-prg = <stack>-program.
          <stack>-program = split[ 1 ].
        ENDIF.

        IF step-include <> mo_viewer->mo_window->m_prg-include.
          CONTINUE.
        ENDIF.

        "APPEND INITIAL LINE TO mt_coverage ASSIGNING FIELD-SYMBOL(<coverage>).
        "<coverage>-line = step-line.
      ENDLOOP.

      SORT mt_coverage.
      DELETE ADJACENT DUPLICATES FROM mt_coverage.

    ENDMETHOD.

    METHOD on_stack_double_click.

      READ TABLE mo_viewer->mo_window->mt_stack INDEX row INTO DATA(stack).
      "only for coverage stack selection should work.
      "CHECK mo_viewer->mo_window->mt_coverage IS NOT INITIAL.

      MOVE-CORRESPONDING stack TO mo_viewer->mo_window->m_prg.
      MOVE-CORRESPONDING stack TO mo_viewer->ms_stack.

      mo_viewer->mo_window->m_prg-program = stack-prg.

      "show_coverage( ).
      mo_viewer->show( ).
      CASE stack-eventtype.
        WHEN 'FUNCTION'.
          mo_viewer->mo_window->mo_box->set_caption( |FM: { stack-eventname }| ).
        WHEN OTHERS.
          mo_viewer->mo_window->mo_box->set_caption( |{ stack-program } : { stack-eventname }| ).
      ENDCASE.
    ENDMETHOD.

    METHOD on_editor_double_click.
      sender->get_selection_pos( IMPORTING from_line = DATA(fr_line) from_pos = DATA(fr_pos) to_line = DATA(to_line) to_pos = DATA(to_pos) ).

    ENDMETHOD.

    METHOD on_editor_border_click.

      DATA: type      TYPE char1,
            program   TYPE program,
            include   TYPE program,
            code_line TYPE i.

      IF cntrl_pressed_set IS INITIAL.
        type = 'S'.
      ELSE.
        type = 'E'.
      ENDIF.
      IF m_prg-include = 'Code_Flow_Mix'.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY include =  'Code_Flow_Mix' INTO DATA(prog_mix).
        READ TABLE prog_mix-t_keywords WITH KEY v_line = line  INTO DATA(keyword).
        program = keyword-program.
        include = keyword-include.
        code_line = keyword-line.
      ELSE.
        program = m_prg-program.
        include = m_prg-include.
        code_line = line.
      ENDIF.
      LOOP AT mt_bpoints ASSIGNING FIELD-SYMBOL(<point>) WHERE line = code_line AND include = include.
        type = <point>-type.

        CALL FUNCTION 'RS_DELETE_BREAKPOINT'
          EXPORTING
            index        = code_line
            mainprog     = program
            program      = include
            bp_type      = type
          EXCEPTIONS
            not_executed = 1
            OTHERS       = 2.

        IF sy-subrc = 0.
          <point>-del = abap_true.
        ENDIF.
      ENDLOOP.

      IF sy-subrc <> 0. "create
        CALL FUNCTION 'RS_SET_BREAKPOINT'
          EXPORTING
            index        = code_line
            program      = include
            mainprogram  = program
            bp_type      = type
          EXCEPTIONS
            not_executed = 1
            OTHERS       = 2.

      ENDIF.
      DELETE mt_bpoints WHERE del IS NOT INITIAL.

      IF m_prg-include = 'Code_Flow_Mix'.
        set_mixprog_line( ).
      ELSE.
        set_program_line( ).
      ENDIF.

    ENDMETHOD.

    METHOD hnd_toolbar.

      CONSTANTS: c_mask TYPE x VALUE '01'.
      FIELD-SYMBOLS: <any> TYPE any.
      m_debug_button = fcode.
      READ TABLE mt_stack INDEX 1 INTO DATA(stack).
      CASE fcode.

        WHEN 'AI'.

          READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY selected = abap_true INTO DATA(prog).
          NEW lcl_ace_ai( i_source = prog-source_tab io_parent =  mo_viewer->mo_window->mo_box ).

        WHEN 'RUN'.

          DATA: lt_source TYPE STANDARD TABLE OF text255,
                lv_prog   TYPE progname VALUE 'Z_SMART_DEBUGGER_SCRIPT'.

          READ REPORT lv_prog INTO lt_source.
          DELETE lt_source INDEX 2.
          IF sy-subrc = 0.
            CALL FUNCTION 'CLPB_EXPORT'
              TABLES
                data_tab   = lt_source
              EXCEPTIONS
                clpb_error = 1
                OTHERS     = 2.

          ENDIF.
          lv_prog = mo_viewer->mv_prog.
          SELECT COUNT(*) INTO @DATA(count) FROM reposrc WHERE progname = @lv_prog AND subc = '1'.

          IF count = 1.
            SUBMIT (lv_prog) VIA SELECTION-SCREEN AND RETURN.
          ENDIF.

        WHEN 'DEPTH'.
          IF m_hist_depth < 9.
            ADD 1 TO m_hist_depth.
          ELSE.
            m_hist_depth = 1.
          ENDIF.

          CLEAR: mo_viewer->mt_steps, mo_viewer->m_step, mo_viewer->mo_window->mt_stack, mo_viewer->mo_window->mt_calls.
          READ TABLE mo_viewer->mo_window->ms_sources-tt_progs INDEX 1 INTO DATA(source).
          lcl_ace_source_parser=>code_execution_scanner( i_program = source-include i_include = source-include io_debugger = mo_viewer ).
          mo_viewer->mo_window->show_coverage( ).
          mo_viewer->mo_window->show_stack( ).
          IF mo_mermaid IS NOT INITIAL.
            mo_mermaid->refresh( ).
          ENDIF.

          mo_toolbar->set_button_info( EXPORTING fcode = 'DEPTH' text = |Depth { m_hist_depth }| ).

          IF mo_viewer->mo_window->m_prg-include = 'Code_Flow_Mix'. "refresh this
            mo_viewer->get_code_mix( ).
            mo_viewer->mo_window->show_stack( ).
          ENDIF.

        WHEN 'CALLS'.
          IF mo_mermaid IS INITIAL.
            mo_mermaid = NEW lcl_ace_mermaid( io_debugger = mo_viewer i_type =  'CALLS' ).
          ELSE.
            IF mo_mermaid->mo_box IS INITIAL.
              mo_mermaid = NEW lcl_ace_mermaid( io_debugger = mo_viewer i_type =  'CALLS' ).
            ENDIF.
          ENDIF.

        WHEN 'CODEMIX'.

          mo_viewer->get_code_mix( ).
          mo_viewer->mo_window->show_stack( ).


*      WHEN 'COVERAGE'.
*        show_coverage( ).
*        mo_viewer->show( ).

        WHEN 'CODE'.
          m_zcode = m_zcode BIT-XOR c_mask.
          CLEAR: mo_viewer->mt_steps, mo_viewer->m_step, mo_viewer->mo_window->mt_calls.
          READ TABLE mo_viewer->mo_window->ms_sources-tt_progs INDEX 1 INTO source.
          lcl_ace_source_parser=>code_execution_scanner( i_program = source-include i_include = source-include io_debugger = mo_viewer ).
          IF m_zcode IS INITIAL.
            mo_toolbar->set_button_info( EXPORTING fcode = 'CODE' text = 'Z & Standard' ).
          ELSE.
            mo_toolbar->set_button_info( EXPORTING fcode = 'CODE' text = 'Only Z code' ).
          ENDIF.
          mo_viewer->mo_window->show_coverage( ).
          mo_viewer->mo_window->show_stack( ).
          IF mo_mermaid IS NOT INITIAL.
            mo_mermaid->refresh( ).
          ENDIF.

          IF mo_viewer->mo_window->m_prg-include = 'Code_Flow_Mix'. "refresh this
            mo_viewer->get_code_mix( ).
            mo_viewer->mo_window->show_stack( ).
          ENDIF.

        WHEN 'INFO'.
          DATA(l_url) = 'https://ysychov.wordpress.com/2020/07/27/abap-simple-debugger-data-explorer/'.
          CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.

          l_url = 'https://github.com/ysichov/Smart-Debugger'.
          CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.

        WHEN 'STEPS'.

          lcl_ace_appl=>open_int_table( i_name = 'Steps' it_tab = mo_viewer->mt_steps io_window = mo_viewer->mo_window ).

      ENDCASE.

    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_ace_sel_opt DEFINITION DEFERRED.

  CLASS lcl_ace_rtti IMPLEMENTATION.

    METHOD create_struc_handle.
      cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = i_tname
                                           RECEIVING  p_descr_ref    = DATA(o_descr)
                                           EXCEPTIONS type_not_found = 1 ).
      IF sy-subrc = 0.
        e_handle ?= o_descr.
      ELSE.
        RETURN.
      ENDIF.

    ENDMETHOD.

    METHOD create_table_by_name.

      DATA: o_new_tab  TYPE REF TO cl_abap_tabledescr,
            o_new_type TYPE REF TO cl_abap_structdescr.

      create_struc_handle( EXPORTING i_tname = i_tname IMPORTING e_handle = o_new_type ).
      o_new_tab = cl_abap_tabledescr=>create(
        p_line_type  = o_new_type
        p_table_kind = cl_abap_tabledescr=>tablekind_std
        p_unique     = abap_false ).
      CREATE DATA c_table TYPE HANDLE o_new_tab.  "Create a New table type
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_ace_data_transmitter DEFINITION.

    PUBLIC SECTION.
      EVENTS: data_changed EXPORTING VALUE(e_row) TYPE lcl_ace_appl=>t_sel_row,
        col_changed EXPORTING VALUE(e_column) TYPE lvc_fname.
      METHODS: emit IMPORTING e_row TYPE lcl_ace_appl=>t_sel_row,
        emit_col IMPORTING e_column TYPE lvc_fname.

  ENDCLASS.

  CLASS lcl_ace_data_transmitter IMPLEMENTATION.

    METHOD  emit.
      RAISE EVENT data_changed EXPORTING e_row = e_row.

    ENDMETHOD.

    METHOD emit_col.
      RAISE EVENT col_changed EXPORTING e_column = e_column.
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_ace_data_receiver DEFINITION.

    PUBLIC SECTION.
      DATA: mo_transmitter TYPE REF TO lcl_ace_data_transmitter,
            o_tab_from     TYPE REF TO lcl_ace_table_viewer,
            o_sel_to       TYPE REF TO lcl_ace_sel_opt,
            m_from_field   TYPE lvc_fname,
            m_to_field     TYPE lvc_fname.
      METHODS: constructor
        IMPORTING io_transmitter TYPE REF TO lcl_ace_data_transmitter OPTIONAL
                  io_tab_from    TYPE REF TO lcl_ace_table_viewer OPTIONAL
                  io_sel_to      TYPE REF TO lcl_ace_sel_opt OPTIONAL
                  i_from_field   TYPE lvc_fname OPTIONAL
                  i_to_field     TYPE lvc_fname OPTIONAL,
        shut_down,
        update FOR EVENT data_changed OF lcl_ace_data_transmitter IMPORTING e_row,
        update_col FOR EVENT col_changed OF lcl_ace_data_transmitter IMPORTING e_column,
        on_grid_button_click
          FOR EVENT button_click OF cl_gui_alv_grid
          IMPORTING
            es_col_id
            es_row_no.

  ENDCLASS.

  CLASS lcl_ace_sel_opt DEFINITION.

    PUBLIC SECTION.
      DATA: mo_viewer  TYPE REF TO lcl_ace_table_viewer,
            mo_sel_alv TYPE REF TO cl_gui_alv_grid,
            mt_fcat    TYPE lvc_t_fcat,
            mt_sel_tab TYPE TABLE OF lcl_ace_appl=>selection_display_s,
            ms_layout  TYPE lvc_s_layo.

      EVENTS: selection_done.
      METHODS:
        constructor IMPORTING io_viewer TYPE REF TO lcl_ace_table_viewer io_container TYPE REF TO cl_gui_container,
        raise_selection_done,
        update_sel_tab,
        set_value IMPORTING  i_field TYPE any i_low TYPE any OPTIONAL i_high TYPE any OPTIONAL i_clear TYPE boolean DEFAULT abap_true ,
        update_sel_row CHANGING c_sel_row TYPE lcl_ace_appl=>selection_display_s.

    PRIVATE SECTION.
      METHODS:
        init_fcat IMPORTING i_dd_handle TYPE i,
        handle_sel_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
        on_f4 FOR EVENT onf4 OF cl_gui_alv_grid IMPORTING e_fieldname es_row_no er_event_data,
        on_grid_button_click FOR EVENT button_click OF cl_gui_alv_grid
          IMPORTING
            es_col_id
            es_row_no,
        on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING  er_data_changed,
        on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified,
        handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
        handle_doubleclick FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_column es_row_no,
        handle_context_menu_request FOR EVENT context_menu_request OF cl_gui_alv_grid IMPORTING e_object.

  ENDCLASS.

  CLASS lcl_ace_table_viewer DEFINITION INHERITING FROM lcl_ace_popup.

    PUBLIC SECTION.
      TYPES: BEGIN OF t_column_emitter,
               column  TYPE lvc_fname,
               emitter TYPE REF TO lcl_ace_data_transmitter,
             END OF t_column_emitter,
             BEGIN OF t_elem,
               field TYPE fieldname,
               elem  TYPE ddobjname,
             END OF t_elem.

      DATA: m_lang             TYPE ddlanguage,
            m_tabname          TYPE tabname,
            mo_alv             TYPE REF TO cl_gui_alv_grid,
            mo_sel             TYPE REF TO lcl_ace_sel_opt,
            mr_table           TYPE REF TO data,
            mo_sel_parent      TYPE REF TO cl_gui_container,
            mo_alv_parent      TYPE REF TO cl_gui_container,
            mt_alv_catalog     TYPE lvc_t_fcat,
            mt_fields          TYPE TABLE OF t_elem,
            mo_column_emitters TYPE TABLE OF t_column_emitter,
            mo_sel_width       TYPE i,
            m_visible,
            m_std_tbar         TYPE x,
            m_show_empty       TYPE i,
            mo_window          TYPE REF TO lcl_ace_window.

      METHODS:
        constructor IMPORTING i_tname           TYPE any OPTIONAL
                              i_additional_name TYPE string OPTIONAL
                              ir_tab            TYPE REF TO data OPTIONAL
                              io_window         TYPE REF TO lcl_ace_window,
        refresh_table FOR EVENT selection_done OF lcl_ace_sel_opt.

    PRIVATE SECTION.
      METHODS:
        create_popup,
        create_alv,
        create_sel_alv,
        set_header,
        create_field_cat IMPORTING i_tname           TYPE tabname
                         RETURNING VALUE(et_catalog) TYPE lvc_t_fcat,
        translate_field IMPORTING i_lang TYPE ddlanguage CHANGING c_fld TYPE lvc_s_fcat,
        handle_tab_toolbar  FOR EVENT toolbar OF cl_gui_alv_grid  IMPORTING e_object,
        before_user_command FOR EVENT before_user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
        handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
        handle_doubleclick FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_column es_row_no,
        on_table_close FOR EVENT close OF cl_gui_dialogbox_container IMPORTING sender.

  ENDCLASS.

  CLASS lcl_ace_text_viewer DEFINITION FINAL INHERITING FROM lcl_ace_popup.

    PUBLIC SECTION.
      DATA: mo_text     TYPE REF TO cl_gui_textedit.
      METHODS: constructor IMPORTING ir_str TYPE REF TO data.
  ENDCLASS.

  CLASS lcl_ace_text_viewer IMPLEMENTATION.

    METHOD constructor.
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
      ENDIF.

      mo_text->set_readonly_mode( ).
      FIELD-SYMBOLS <str> TYPE string.
      ASSIGN ir_str->* TO <str>.
      DATA string TYPE TABLE OF char255.

      WHILE strlen( <str> ) > 255.
        APPEND <str>+0(255) TO string.
        SHIFT <str> LEFT BY 255 PLACES.
      ENDWHILE.

      APPEND <str> TO string.
      mo_text->set_text_as_r3table( string ).
      CALL METHOD cl_gui_cfw=>flush.
      mo_text->set_focus( mo_box ).

    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_ace_data_receiver IMPLEMENTATION.

    METHOD constructor.

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

    ENDMETHOD.

    METHOD shut_down.

      IF mo_transmitter IS NOT INITIAL.
        SET HANDLER me->update FOR mo_transmitter  ACTIVATION space.
      ELSE.
        SET HANDLER me->update FOR ALL INSTANCES  ACTIVATION space.
      ENDIF.
      CLEAR o_sel_to.

    ENDMETHOD.

    METHOD on_grid_button_click.

      FIELD-SYMBOLS: <f_tab>   TYPE STANDARD TABLE.

      CHECK m_from_field = es_col_id-fieldname.
      ASSIGN o_tab_from->mr_table->* TO <f_tab>.
      READ TABLE <f_tab> INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).
      ASSIGN COMPONENT es_col_id-fieldname OF STRUCTURE <tab> TO  FIELD-SYMBOL(<f_field>).
      CHECK o_sel_to IS NOT INITIAL.
      o_sel_to->set_value( i_field = m_to_field i_low = <f_field> ).
      o_sel_to->raise_selection_done( ).

    ENDMETHOD.

    METHOD  update.

      DATA: l_updated.

      READ TABLE o_sel_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to>) WITH KEY field_label = m_to_field.
      IF <to>-range[] = e_row-range[].
        l_updated = abap_true."so as not to have an infinite event loop
      ENDIF.
      MOVE-CORRESPONDING e_row TO <to>.
      IF <to>-transmitter IS BOUND AND l_updated IS INITIAL.
        <to>-transmitter->emit( EXPORTING e_row = e_row ).
      ENDIF.
      o_sel_to->raise_selection_done( ).

    ENDMETHOD.

    METHOD update_col.

      DATA: l_updated,
            sel_row   TYPE lcl_ace_appl=>t_sel_row.

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
        l_updated = abap_true."so as not to have an infinite event loop
      ENDIF.
      IF <to>-transmitter IS BOUND AND l_updated IS INITIAL.
        <to>-transmitter->emit( EXPORTING e_row = sel_row ).
        o_sel_to->raise_selection_done( ).
      ENDIF.

    ENDMETHOD.

  ENDCLASS.

*CLASS lcl_box_handler IMPLEMENTATION.
*
*   METHOD on_table_close.
*    DATA:  tabix LIKE sy-tabix.
*    sender->free( ).
*
*    "Free Memory
*    LOOP AT lcl_ace_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>) WHERE alv_viewer IS NOT INITIAL.
*      IF <obj>-alv_viewer->mo_box = sender.
*         tabix = sy-tabix.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
*    IF sy-subrc = 0.
*      FREE <obj>-alv_viewer->mr_table.
*      FREE <obj>-alv_viewer->mo_alv.
*
*      "shutdown receivers.
*      IF <obj>-alv_viewer->mo_sel IS NOT INITIAL.
*        LOOP AT <obj>-alv_viewer->mo_sel->mt_sel_tab INTO DATA(l_sel).
*          IF l_sel-receiver IS BOUND.
*            l_sel-receiver->shut_down( ).
*          ENDIF.
*        ENDLOOP.
*      ENDIF.
*      FREE <obj>-alv_viewer.
*      IF  tabix NE 0.
*        DELETE lcl_ace_appl=>mt_obj INDEX  tabix.
*      ENDIF.
*    ENDIF.
*  ENDMETHOD.                    "ON_BOX_CLOSE
*
*ENDCLASS.               "lcl_box_handler

  CLASS lcl_ace_table_viewer IMPLEMENTATION.

    METHOD constructor.

      DATA: comp         TYPE abap_componentdescr,
            comp_notab   TYPE abap_component_tab,
            comp_tab2str TYPE abap_component_tab,
            comp_str     TYPE abap_component_tab,
            str          TYPE string,
            data         TYPE REF TO data.

      DATA: l_notab   TYPE REF TO data,
            l_tab2str TYPE REF TO data.

      DATA: handle_notab   TYPE REF TO cl_abap_structdescr,
            handle_tab2str TYPE REF TO cl_abap_structdescr,
            o_new_tab      TYPE REF TO cl_abap_tabledescr.

      FIELD-SYMBOLS: <notab>   TYPE STANDARD TABLE,
                     <tab2str> TYPE STANDARD TABLE,
                     <any_tab> TYPE ANY TABLE,
                     <temptab> TYPE ANY TABLE.

      super->constructor( i_additional_name = i_additional_name ).
      mo_window = io_window.
      m_lang = sy-langu.
      mo_sel_width = 0.
      m_tabname = i_tname.
      create_popup( ).

      IF ir_tab IS NOT BOUND.
        lcl_ace_rtti=>create_table_by_name( EXPORTING i_tname = m_tabname CHANGING c_table = mr_table ).
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
                LOOP AT o_struc->components INTO DATA(component).

                  IF component-type_kind NE 'h'.
                    comp-name = component-name.
                    comp-type ?= o_struc->get_component_type( component-name ).
                    APPEND comp TO comp_notab.
                    APPEND comp TO comp_tab2str.
                  ELSE.
                    comp-name = component-name.
                    comp-type ?= cl_abap_typedescr=>describe_by_data(  str ).
                    APPEND comp TO comp_tab2str.
                    APPEND comp TO comp_str.

                    comp-name = component-name && '_REF'.
                    comp-type ?= cl_abap_typedescr=>describe_by_data(  data ).
                    APPEND comp TO comp_tab2str.
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

                CREATE DATA l_notab TYPE HANDLE o_new_tab.

                o_new_tab = cl_abap_tabledescr=>create(
                  p_line_type  = handle_tab2str
                  p_table_kind = cl_abap_tabledescr=>tablekind_std
                  p_unique     = abap_false ).

                CREATE DATA l_tab2str TYPE HANDLE o_new_tab.

                ASSIGN l_notab->* TO <notab>.
                MOVE-CORRESPONDING <any_tab> TO <notab>.
                ASSIGN l_tab2str->* TO <tab2str>.
                MOVE-CORRESPONDING <notab> TO <tab2str>.

                LOOP AT <any_tab> ASSIGNING FIELD-SYMBOL(<old_struc>).
                  READ TABLE <tab2str> ASSIGNING FIELD-SYMBOL(<new_struc>) INDEX sy-tabix.
                  LOOP AT comp_str INTO comp.
                    ASSIGN COMPONENT comp-name OF STRUCTURE <new_struc> TO FIELD-SYMBOL(<field>).
                    ASSIGN COMPONENT comp-name OF STRUCTURE <old_struc> TO <temptab>.
                    <field> = | { icon_view_table } [{ lines( <temptab> ) }] |.
                    ASSIGN COMPONENT comp-name  OF STRUCTURE <old_struc> TO <field>.
                    ASSIGN COMPONENT |{ comp-name }_REF| OF STRUCTURE <new_struc> TO FIELD-SYMBOL(<ref>).
                    GET REFERENCE OF <field> INTO <ref>.
                  ENDLOOP.
                ENDLOOP.

                GET REFERENCE OF <tab2str> INTO mr_table.
              CATCH cx_root.
                mr_table = ir_tab.
            ENDTRY.
          CATCH cx_sy_move_cast_error.  "no structure
            comp-name = 'FIELD'.
            comp-type ?= cl_abap_typedescr=>describe_by_data( str ).
            APPEND comp TO comp_tab2str.

            handle_tab2str  = cl_abap_structdescr=>create( comp_tab2str ).
            o_new_tab = cl_abap_tabledescr=>create(
              p_line_type  = handle_tab2str
              p_table_kind = cl_abap_tabledescr=>tablekind_std
              p_unique     = abap_false ).

            CREATE DATA l_tab2str TYPE HANDLE o_new_tab.
            ASSIGN l_tab2str->* TO <tab2str>.
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

    ENDMETHOD.

    METHOD create_popup.

      mo_box = create( i_width = 800 i_hight = 150 ).

      "save new popup ref
      APPEND INITIAL LINE TO lcl_ace_appl=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>).
      <popup>-parent = mo_window->mo_box.
      <popup>-child = mo_box.

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


      SET HANDLER on_table_close FOR mo_box.

    ENDMETHOD.

    METHOD create_alv.

      DATA: layout TYPE lvc_s_layo,
            effect TYPE i,
            f4s    TYPE lvc_t_f4.

      FIELD-SYMBOLS: <f_tab>   TYPE table.

      mo_alv = NEW #( i_parent = mo_alv_parent ).
      mt_alv_catalog = create_field_cat( m_tabname ).

      IF mt_alv_catalog IS INITIAL.
        RETURN. "todo show tables without structure
      ENDIF.

      ASSIGN mr_table->* TO <f_tab>.
      set_header( ).
      layout-cwidth_opt = abap_true.
      layout-sel_mode = 'D'.
      CREATE OBJECT lcl_ace_appl=>mo_dragdropalv.
      effect = cl_dragdrop=>move + cl_dragdrop=>copy.

      CALL METHOD lcl_ace_appl=>mo_dragdropalv->add
        EXPORTING
          flavor     = 'Line' ##NO_TEXT
          dragsrc    = abap_true
          droptarget = abap_true
          effect     = effect.

      CALL METHOD lcl_ace_appl=>mo_dragdropalv->get_handle IMPORTING handle = DATA(handle_alv).
      layout-s_dragdrop-grid_ddid = handle_alv.

      SET HANDLER   before_user_command
                    handle_user_command
                    handle_tab_toolbar
                    handle_doubleclick
                    lcl_ace_dragdrop=>drag
                    FOR mo_alv.

      CALL METHOD mo_alv->set_table_for_first_display
        EXPORTING
          i_save          = abap_true
          i_default       = abap_true
          is_layout       = layout
        CHANGING
          it_fieldcatalog = mt_alv_catalog
          it_outtab       = <f_tab>.

      mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = mt_alv_catalog ).
      LOOP AT mt_alv_catalog ASSIGNING FIELD-SYMBOL(<catalog>).
        CLEAR <catalog>-key.
        DATA(f4) = VALUE lvc_s_f4( register = abap_true chngeafter = abap_true fieldname = <catalog>-fieldname ).
        INSERT f4 INTO TABLE f4s.
      ENDLOOP.

      mo_alv->register_f4_for_fields( it_f4 = f4s ).
      mo_alv->set_frontend_fieldcatalog( EXPORTING it_fieldcatalog = mt_alv_catalog ).

      LOOP AT mt_alv_catalog ASSIGNING FIELD-SYMBOL(<cat>) WHERE scrtext_l IS INITIAL.
        lcl_ace_alv_common=>translate_field( CHANGING c_fld = <cat> ).
      ENDLOOP.

      mo_alv->set_frontend_fieldcatalog( EXPORTING it_fieldcatalog = mt_alv_catalog ).
      me->handle_user_command( EXPORTING e_ucomm = 'TECH' ).
      me->handle_user_command( EXPORTING e_ucomm = 'SHOW' ).
      mo_alv->set_toolbar_interactive( ).

    ENDMETHOD.

    METHOD translate_field.

      DATA: l_dd04 TYPE dd04v.

      READ TABLE mt_fields INTO DATA(l_field) WITH KEY field = c_fld-fieldname.
      CHECK l_field-elem IS NOT INITIAL.
      CLEAR l_dd04.

      CALL FUNCTION 'DDIF_DTEL_GET'
        EXPORTING
          name          = CONV ddobjname( l_field-elem )
          langu         = i_lang
        IMPORTING
          dd04v_wa      = l_dd04
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.

      IF sy-subrc = 0.
        IF l_dd04-reptext IS NOT INITIAL.
          MOVE-CORRESPONDING l_dd04 TO c_fld.
        ENDIF.
      ENDIF.

    ENDMETHOD.

    METHOD create_sel_alv.

      IF mo_sel IS INITIAL.
        mo_sel     = NEW #( io_viewer = me io_container = mo_sel_parent ).
        SET HANDLER refresh_table FOR mo_sel.
      ELSE.
        mo_sel->update_sel_tab( ).
      ENDIF.

    ENDMETHOD.

    METHOD set_header.

      DATA: text       TYPE as4text,
            header(80) TYPE c.

      SELECT SINGLE ddtext INTO  text
        FROM dd02t
       WHERE tabname = m_tabname
         AND ddlanguage = m_lang.

      header = |{ m_tabname } - {  text } { m_additional_name }|.
      mo_box->set_caption(  header ).

    ENDMETHOD.

    METHOD handle_tab_toolbar.

      IF m_visible IS INITIAL.
        DATA(toolbar) = VALUE ttb_button(
         ( function = 'SEL_ON' icon = icon_arrow_left quickinfo = 'Show Select-Options'  butn_type = 0 )
         ( butn_type = 3 ) ).
      ENDIF.

      APPEND VALUE #( function = 'TECH' icon = icon_wd_caption quickinfo = 'Tech names'  butn_type = 0 ) TO toolbar.

      LOOP AT lcl_ace_appl=>mt_lang INTO DATA(lang).
        IF sy-tabix > 10.
          EXIT.
        ENDIF.
        APPEND VALUE #( function = lang-spras icon = icon_foreign_trade quickinfo = lang-sptxt butn_type = 0 text = lang-sptxt ) TO toolbar.
      ENDLOOP.

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

    ENDMETHOD.

    METHOD create_field_cat.

      DATA: lr_field       TYPE REF TO data,
            lr_table_descr TYPE REF TO cl_abap_structdescr,
            lr_data_descr  TYPE REF TO cl_abap_datadescr,
            it_tabdescr    TYPE abap_compdescr_tab,
            l_texttab      TYPE tabname,
            lr_temp        TYPE REF TO data,
            l_name         TYPE string,
            l_dd04         TYPE dd04v.

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
      lcl_ace_ddic=>get_text_table( EXPORTING i_tname = i_tname IMPORTING e_tab = l_texttab ).

      LOOP AT it_tabdescr INTO DATA(ls)
         WHERE type_kind NE 'h'
           AND type_kind NE 'l'.
        DATA(l_ind) = sy-tabix.

        ASSIGN COMPONENT ls-name OF STRUCTURE <struc> TO <field>.
        GET REFERENCE OF <field> INTO lr_field.
        lr_data_descr ?= cl_abap_typedescr=>describe_by_data_ref( lr_field ).
        l_name = lr_data_descr->absolute_name.
        REPLACE ALL OCCURRENCES OF '\TYPE=' IN l_name WITH ''.
        APPEND VALUE #( field = ls-name elem = l_name ) TO mt_fields.

        CLEAR l_dd04.
        CALL FUNCTION 'DDIF_DTEL_GET'
          EXPORTING
            name          = CONV ddobjname( l_name )
            langu         = m_lang
          IMPORTING
            dd04v_wa      = l_dd04
          EXCEPTIONS
            illegal_input = 1
            OTHERS        = 2.

        APPEND INITIAL LINE TO et_catalog ASSIGNING FIELD-SYMBOL(<catalog>).

        <catalog>-col_pos = l_ind.
        <catalog>-style = lcl_ace_alv_common=>c_white.
        <catalog>-fieldname = ls-name.
        <catalog>-f4availabl = abap_true.

        IF l_dd04 IS INITIAL.
          <catalog>-scrtext_s = <catalog>-scrtext_m = <catalog>-scrtext_l = <catalog>-reptext = <catalog>-fieldname = ls-name.
        ELSE.
          MOVE-CORRESPONDING l_dd04 TO <catalog>.
        ENDIF.
      ENDLOOP.

    ENDMETHOD.

    METHOD handle_doubleclick.

*    DATA: o_table_descr TYPE REF TO cl_tpda_script_tabledescr,
*          table_clone    TYPE REF TO data.
*    FIELD-SYMBOLS: <f_tab>  TYPE STANDARD TABLE.
*
*    CHECK es_row_no-row_id IS NOT INITIAL.
*    ASSIGN mr_table->* TO  <f_tab>.
*    READ TABLE <f_tab> INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).
*    ASSIGN COMPONENT e_column-fieldname  OF STRUCTURE <tab> TO FIELD-SYMBOL(<val>).
*
*    CASE e_column-fieldname.
*      WHEN 'VALUE'.
*        IF sy-subrc = 0.
*          IF <val> = 'Table'.
*            ASSIGN COMPONENT 'REF'  OF STRUCTURE <tab> TO FIELD-SYMBOL(<ref>).
*            lcl_ace_appl=>open_int_table( EXPORTING i_name = CONV #( e_column-fieldname ) it_ref = <ref> io_window = mo_window ).
*          ENDIF.
*        ELSE.
*          TRY.
*              o_table_descr ?= cl_tpda_script_data_descr=>factory( |{ m_additional_name }[ { es_row_no-row_id } ]-{ e_column-fieldname }| ).
*              table_clone = o_table_descr->elem_clone( ).
*              lcl_ace_appl=>open_int_table( EXPORTING i_name = |{ m_additional_name }[ { es_row_no-row_id } ]-{ e_column-fieldname }| it_ref = table_clone io_window = mo_window ).
*            CATCH cx_sy_move_cast_error.
*          ENDTRY.
*        ENDIF.
*      WHEN 'STEP'.
*        MOVE-CORRESPONDING <tab> TO mo_window->m_prg.
*        MOVE-CORRESPONDING <tab> TO mo_window->mo_viewer->ms_stack.
*
*        mo_window->show_coverage( ).
*        mo_window->mo_viewer->show( ).
*      WHEN OTHERS. "check if it is an embedded table.
*        TRY.
*            o_table_descr ?= cl_tpda_script_data_descr=>factory( |{ m_additional_name }[ { es_row_no-row_id } ]-{ e_column-fieldname }| ).
*            table_clone = o_table_descr->elem_clone( ).
*            lcl_ace_appl=>open_int_table( EXPORTING i_name = |{ m_additional_name }[ { es_row_no-row_id } ]-{ e_column-fieldname }| it_ref = table_clone io_window = mo_window ).
*          CATCH cx_sy_move_cast_error.
*        ENDTRY.
*    ENDCASE.
*
    ENDMETHOD.

    METHOD on_table_close.
      DATA:  tabix LIKE sy-tabix.
      CALL METHOD sender->free
        EXCEPTIONS
          cntl_error = 1
          OTHERS     = 2.

      "Free Memory
      LOOP AT lcl_ace_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>) WHERE alv_viewer IS NOT INITIAL.
        IF <obj>-alv_viewer->mo_box = sender.
          tabix = sy-tabix.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF sy-subrc = 0.
        FREE <obj>-alv_viewer->mr_table.
        FREE <obj>-alv_viewer->mo_alv.

        "shutdown receivers.
        IF <obj>-alv_viewer->mo_sel IS NOT INITIAL.
          LOOP AT <obj>-alv_viewer->mo_sel->mt_sel_tab INTO DATA(l_sel).
            IF l_sel-receiver IS BOUND.
              l_sel-receiver->shut_down( ).
            ENDIF.
          ENDLOOP.
        ENDIF.
        FREE <obj>-alv_viewer.
        IF  tabix NE 0.
          DELETE lcl_ace_appl=>mt_obj INDEX  tabix.
        ENDIF.
      ENDIF.

    ENDMETHOD.

    METHOD before_user_command.

      CASE e_ucomm.
        WHEN '&INFO'.
          DATA(l_url) = 'https://ysychov.wordpress.com/2020/02/10/simple-data-explorer/'.
          CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.
      ENDCASE.

    ENDMETHOD.

    METHOD handle_user_command.

      DATA: it_fields  TYPE lvc_t_fcat,
            clause(45),
            sel_width  TYPE i.

      FIELD-SYMBOLS: <f_tab>  TYPE STANDARD  TABLE.
      ASSIGN mr_table->* TO <f_tab>.
      mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = it_fields[] ).
      IF e_ucomm = 'SEL_ON' AND m_visible IS INITIAL.
        create_sel_alv( ).
        m_visible = abap_true.
        IF mo_sel_width = 0.
          sel_width = 500.
        ELSE.
          sel_width = mo_sel_width.
        ENDIF.

        mo_splitter->set_column_width( EXPORTING id = 1 width =  sel_width ).
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
                LOOP AT <f_tab> ASSIGNING FIELD-SYMBOL(<f_line>)  WHERE (clause).
                  EXIT.
                ENDLOOP.
                IF sy-subrc NE 0.
                  <fields>-no_out = abap_true.
                ENDIF.
              ENDIF.

            WHEN 'TECH'. "technical field name
              <fields>-scrtext_l = <fields>-scrtext_m = <fields>-scrtext_s =  <fields>-reptext = <fields>-fieldname.

            WHEN OTHERS. "header names translation
              IF line_exists( lcl_ace_appl=>mt_lang[ spras = e_ucomm ] ).
                translate_field( EXPORTING i_lang = CONV #( e_ucomm )  CHANGING c_fld = <fields> ).
                IF mo_sel IS BOUND.
                  READ TABLE mo_sel->mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) WITH KEY field_label = <fields>-fieldname.
                  IF sy-subrc = 0.
                    IF <fields>-scrtext_l IS NOT INITIAL.
                      <sel>-name = <fields>-scrtext_l.
                    ENDIF.
                    IF <sel>-name IS INITIAL.
                      IF <fields>-reptext IS NOT INITIAL.
                        <sel>-name = <fields>-reptext.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
          ENDCASE.
        ENDLOOP.
      ENDIF.

      IF line_exists( lcl_ace_appl=>mt_lang[ spras = e_ucomm ] ).
        m_lang = e_ucomm.
        set_header( ).
        mo_sel->set_value( i_field = 'SPRSL' i_low = m_lang ).
      ENDIF.

      CALL METHOD mo_alv->set_frontend_fieldcatalog EXPORTING it_fieldcatalog = it_fields[].

      lcl_ace_alv_common=>refresh( mo_alv ).
      IF mo_sel IS BOUND.
        IF  e_ucomm = 'HIDE' OR e_ucomm = 'SHOW' OR e_ucomm = 'UPDATE' .
          mo_sel->update_sel_tab( ).
        ENDIF.
        lcl_ace_alv_common=>refresh( mo_sel->mo_sel_alv ).
        mo_sel->mo_sel_alv->refresh_table_display(  ).
      ENDIF.

    ENDMETHOD.                           "handle_user_command

    METHOD refresh_table.

      DATA: row    TYPE lcl_ace_appl=>t_sel_row,
            filter TYPE lvc_t_filt.

      CLEAR filter.
      set_header( ).

      LOOP AT mo_sel->mt_sel_tab  ASSIGNING FIELD-SYMBOL(<sel>).
        IF <sel>-transmitter IS NOT INITIAL.
          MOVE-CORRESPONDING <sel> TO row.
          <sel>-transmitter->emit( e_row = row ).
        ENDIF.
        LOOP AT <sel>-range INTO DATA(l_range).
          APPEND VALUE #( fieldname = <sel>-field_label
                                low = l_range-low
                               high = l_range-high
                               sign = l_range-sign
                             option = l_range-opti ) TO filter.
        ENDLOOP.
      ENDLOOP.

      IF mo_sel->mt_sel_tab IS NOT INITIAL.
        CALL METHOD mo_alv->set_filter_criteria
          EXPORTING
            it_filter = filter.
        lcl_ace_alv_common=>refresh( mo_sel->mo_sel_alv ).
        lcl_ace_alv_common=>refresh( mo_alv ).
        mo_sel->mo_viewer->handle_user_command( 'SHOW' ).
        LOOP AT mo_column_emitters INTO DATA(l_emit).
          l_emit-emitter->emit_col( l_emit-column ).
        ENDLOOP.
      ENDIF.
    ENDMETHOD.
  ENDCLASS.

  CLASS lcl_ace_sel_opt IMPLEMENTATION.
    METHOD constructor.
      DATA: effect     TYPE i,
            handle_alv TYPE i.

      mo_viewer = io_viewer.
      mo_sel_alv = NEW #( i_parent = io_container ).
      update_sel_tab( ).
      CREATE OBJECT lcl_ace_appl=>mo_dragdropalv.
      effect =  cl_dragdrop=>copy. " + cl_dragdrop=>move.

      CALL METHOD lcl_ace_appl=>mo_dragdropalv->add
        EXPORTING
          flavor     = 'Line'
          dragsrc    = abap_true
          droptarget = abap_true
          effect     = effect.

      CALL METHOD lcl_ace_appl=>mo_dragdropalv->get_handle IMPORTING handle = handle_alv.
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
                  lcl_ace_dragdrop=>drag
                  lcl_ace_dragdrop=>drop
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

    ENDMETHOD.

    METHOD init_fcat.

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

    ENDMETHOD.

    METHOD raise_selection_done.

      DATA: row TYPE lcl_ace_appl=>t_sel_row.

      lcl_ace_alv_common=>refresh( mo_sel_alv ).
      RAISE EVENT selection_done.
      LOOP AT mt_sel_tab  ASSIGNING FIELD-SYMBOL(<sel>).
        IF <sel>-transmitter IS NOT INITIAL.
          MOVE-CORRESPONDING <sel> TO row.
          <sel>-transmitter->emit( e_row = row ).
        ENDIF.
      ENDLOOP.

    ENDMETHOD.

    METHOD update_sel_tab.

      IF mt_sel_tab[] IS NOT INITIAL.
        DATA(sel_tab_copy) = mt_sel_tab.
      ENDIF.
      CLEAR mt_sel_tab[].
      mo_viewer->mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = mo_viewer->mt_alv_catalog ).
      LOOP AT mo_viewer->mt_alv_catalog INTO DATA(l_catalog) WHERE domname NE 'MANDT'.
        DATA(ind) = sy-tabix.
        APPEND INITIAL LINE TO mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel_tab>).
        READ TABLE sel_tab_copy INTO DATA(copy) WITH KEY field_label = l_catalog-fieldname.

        IF sy-subrc = 0.
          MOVE-CORRESPONDING copy TO <sel_tab>.
        ELSE.
          <sel_tab>-option_icon = icon_led_inactive.
          <sel_tab>-more_icon = icon_enter_more.
        ENDIF.

        <sel_tab>-ind =  ind.
        <sel_tab>-field_label = l_catalog-fieldname.
        <sel_tab>-int_type = l_catalog-inttype.
        <sel_tab>-element = l_catalog-rollname.
        <sel_tab>-domain =  l_catalog-domname.
        <sel_tab>-datatype = l_catalog-datatype.
        <sel_tab>-length = l_catalog-outputlen.
        lcl_ace_alv_common=>translate_field( EXPORTING i_lang = mo_viewer->m_lang CHANGING c_fld = l_catalog ).
        <sel_tab>-name = l_catalog-scrtext_l.
      ENDLOOP.

    ENDMETHOD.

    METHOD handle_sel_toolbar.

      e_object->mt_toolbar[] = VALUE #( butn_type = 0 disabled = ''
       ( function = 'SEL_OFF' icon = icon_arrow_right    quickinfo = 'Hide' )
       ( function = 'SEL_CLEAR' icon = icon_delete_row    quickinfo = 'Clear Select-Options' ) ).

    ENDMETHOD.

    METHOD set_value.

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
        DATA: row TYPE lcl_ace_appl=>t_sel_row.
        MOVE-CORRESPONDING <to> TO row.
        <to>-transmitter->emit( EXPORTING e_row = row ).
      ENDIF.

    ENDMETHOD.

    METHOD handle_doubleclick.

      DATA: it_bdcdata TYPE TABLE OF  bdcdata.

      CHECK es_row_no-row_id IS NOT INITIAL.

      READ TABLE mt_sel_tab INDEX es_row_no-row_id INTO DATA(l_sel).
      APPEND VALUE #( program = 'SAPLSD_ENTRY' dynpro = '1000' dynbegin = abap_true ) TO it_bdcdata.
      APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WB_DISPLAY' ) TO it_bdcdata.

      IF e_column = 'ELEMENT'.
        SET PARAMETER ID 'DTYP' FIELD l_sel-element.
        APPEND VALUE #( fnam = 'RSRD1-DDTYPE' fval = abap_true ) TO it_bdcdata.
        CALL TRANSACTION 'SE11' USING it_bdcdata MODE 'E'.
      ELSEIF e_column = 'DOMAIN'.
        SET PARAMETER ID 'DOM' FIELD l_sel-domain.
        APPEND VALUE #( fnam = 'RSRD1-DOMA' fval = abap_true ) TO it_bdcdata.
        CALL TRANSACTION 'SE11' USING it_bdcdata MODE 'E'.
      ELSE.
        CALL FUNCTION 'DOCU_CALL'
          EXPORTING
            id                = 'DE'
            langu             = mo_viewer->m_lang
            object            = l_sel-element
            typ               = 'E'
            displ             = abap_true
            displ_mode        = 3
            use_sec_langu     = abap_true
            display_shorttext = abap_true.
      ENDIF.

    ENDMETHOD.

    METHOD update_sel_row. "select patterns rules

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
          c_sel_row-option_icon = lcl_ace_appl=>m_option_icons[ sign = c_sel_row-sign option = c_sel_row-opti ]-icon_name.
        CATCH cx_sy_itab_line_not_found.                "#EC NO_HANDLER
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
                  date_external           = <field>
                IMPORTING
                  date_internal           = <field>
                EXCEPTIONS
                  date_external_i_invalid = 1
                  OTHERS                  = 2.
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

    ENDMETHOD.

    METHOD on_f4.

      DATA: return_tab TYPE STANDARD TABLE OF ddshretval,
            objects    TYPE TABLE OF objec,
            objec      TYPE objec,
            l_otype    TYPE otype,
            l_plvar    TYPE plvar,
            l_multiple TYPE boolean,
            l_clear    TYPE boolean.

      IF e_fieldname = 'LOW'.
        l_multiple = abap_true.
      ENDIF.

      READ TABLE mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) INDEX es_row_no-row_id.
      DATA(l_fname) =  <sel>-field_label.

      lcl_ace_appl=>mt_sel[] = mt_sel_tab[].
      IF <sel>-element = 'HROBJID'.
        READ TABLE mt_sel_tab INTO DATA(l_sel) WITH KEY field_label = 'OTYPE'.
        l_otype = l_sel-low.
        READ TABLE mt_sel_tab INTO l_sel WITH KEY field_label = 'PLVAR'.
        IF sy-subrc = 0 AND l_sel-low IS NOT INITIAL.
          l_plvar = l_sel-low.
        ELSE.
          CALL FUNCTION 'RH_GET_ACTIVE_WF_PLVAR'
            IMPORTING
              act_plvar       = l_plvar
            EXCEPTIONS
              no_active_plvar = 1
              OTHERS          = 2.
        ENDIF.
      ELSEIF <sel>-element = 'PERSNO'.
        l_otype = 'P'.
      ENDIF.

      IF l_otype IS NOT INITIAL.
        CALL FUNCTION 'RH_OBJID_REQUEST'
          EXPORTING
            plvar            = l_plvar
            otype            = l_otype
            seark_begda      = sy-datum
            seark_endda      = sy-datum
            dynpro_repid     = sy-repid
            dynpro_dynnr     = sy-dynnr
            set_mode         = l_multiple
          IMPORTING
            sel_object       = objec
          TABLES
            sel_hrobject_tab = objects
          EXCEPTIONS
            OTHERS           = 6.
        IF sy-subrc = 0.
          l_clear = abap_true.
          LOOP AT objects INTO objec.
            IF e_fieldname = 'LOW'.
              set_value( EXPORTING i_field = <sel>-field_label i_low = objec-objid i_clear = l_clear ).
              CLEAR l_clear.
            ELSE.
              set_value( EXPORTING i_field = <sel>-field_label i_high = objec-objid i_clear = l_clear ).
            ENDIF.
          ENDLOOP.
        ENDIF.
      ELSE.

        CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
          EXPORTING
            tabname           = mo_viewer->m_tabname
            fieldname         = l_fname
            callback_program  = sy-repid
            callback_form     = 'CALLBACK_F4_SEL' "callback_method - doesn't work for local class
            multiple_choice   = l_multiple
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
          l_clear = abap_true.
          LOOP AT return_tab ASSIGNING FIELD-SYMBOL(<ret>) WHERE fieldname = l_fname.
            IF e_fieldname = 'LOW'.
              set_value( EXPORTING i_field = <sel>-field_label i_low = <ret>-fieldval i_clear = l_clear ).
              CLEAR l_clear.
            ELSE.
              set_value( EXPORTING i_field = <sel>-field_label i_high = <ret>-fieldval ).
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
      er_event_data->m_event_handled = abap_true.
      raise_selection_done( ).

    ENDMETHOD.

    METHOD on_grid_button_click.

      DATA: l_tabfield TYPE rstabfield,
            opt        TYPE rsoptions VALUE 'XXXXXXXXXX',
            sign       TYPE raldb_sign,
            option     TYPE raldb_opti.

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
            <tab>-sign =  sign.
            <tab>-opti =  option.
          ELSEIF sy-subrc = 1.
            CLEAR: <tab>-low, <tab>-high,<tab>-sign, <tab>-opti, <tab>-range.
          ENDIF.
        WHEN 'MORE_ICON'. "edit ranges
          l_tabfield-tablename = mo_viewer->m_tabname.
          l_tabfield-fieldname = <tab>-field_label.

          CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
            EXPORTING
              title             = 'title'
              text              = 'text'
              tab_and_field     = l_tabfield
            TABLES
              range             = <tab>-range
            EXCEPTIONS
              no_range_tab      = 1
              cancelled         = 2
              internal_error    = 3
              invalid_fieldname = 4
              OTHERS            = 5.
          IF sy-subrc = 0.
            READ TABLE <tab>-range INDEX 1 INTO DATA(l_range).
            MOVE-CORRESPONDING l_range TO <tab>.
            IF <tab>-opti NE 'BT'.
              CLEAR <tab>-high.
            ENDIF.
          ENDIF.
      ENDCASE.
      update_sel_row( CHANGING c_sel_row = <tab> ).
      RAISE EVENT selection_done.

    ENDMETHOD.

    METHOD on_data_changed.

      DATA: l_start TYPE i,
            time    TYPE sy-uzeit.

      FIELD-SYMBOLS: <field> TYPE any.

      LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<ls_cells>).
        READ TABLE mt_sel_tab INDEX <ls_cells>-row_id ASSIGNING FIELD-SYMBOL(<tab>).
        ASSIGN COMPONENT <ls_cells>-fieldname OF STRUCTURE <tab> TO <field>.
        READ TABLE mo_viewer->mt_alv_catalog WITH KEY fieldname = <tab>-field_label INTO DATA(l_cat).

        IF <field> IS NOT INITIAL AND <ls_cells>-value IS INITIAL.
          READ TABLE <tab>-range INTO DATA(l_second) INDEX 2.
          IF sy-subrc = 0.
            IF ( <ls_cells>-fieldname = 'LOW' AND <tab>-high IS INITIAL ) OR  ( <ls_cells>-fieldname = 'HIGH' AND <tab>-low IS INITIAL  ).
              DELETE <tab>-range INDEX 1.
            ELSE.
              CLEAR l_second.
            ENDIF.
          ENDIF.
        ENDIF.

        IF l_cat-convexit = 'ALPHA' AND NOT  <ls_cells>-value CA '+*'.
          <ls_cells>-value = |{ <ls_cells>-value ALPHA = IN }|.
          l_start = 128 - l_cat-dd_outlen.
          <ls_cells>-value = <ls_cells>-value+l_start(l_cat-dd_outlen).
        ENDIF.

        IF <ls_cells>-value IS NOT INITIAL.
          IF <tab>-int_type = 'D'.
            DATA:  date TYPE sy-datum.
            CALL FUNCTION 'CONVERT_DATE_INPUT'
              EXPORTING
                input                     = <ls_cells>-value
                plausibility_check        = abap_true
              IMPORTING
                output                    = date
              EXCEPTIONS
                plausibility_check_failed = 1
                wrong_format_in_input     = 2
                OTHERS                    = 3.

            IF sy-subrc = 0.
              <ls_cells>-value = |{  date DATE = USER }|.
            ENDIF.
          ELSEIF <tab>-int_type = 'T'.
            CALL FUNCTION 'CONVERT_TIME_INPUT'
              EXPORTING
                input                     = <ls_cells>-value
              IMPORTING
                output                    = time
              EXCEPTIONS
                plausibility_check_failed = 1
                wrong_format_in_input     = 2
                OTHERS                    = 3.
            <ls_cells>-value =  time+0(2) && ':' &&  time+2(2) && ':' &&  time+4(2).
          ENDIF.
        ENDIF.
      ENDLOOP.
      CHECK sy-subrc = 0.

      IF l_second IS INITIAL.
        <field> = <ls_cells>-value.
        er_data_changed->modify_cell( EXPORTING i_row_id = <ls_cells>-row_id i_fieldname = <ls_cells>-fieldname i_value = <ls_cells>-value ).
      ELSE.
        <tab>-low = l_second-low.
        er_data_changed->modify_cell( EXPORTING i_row_id = <ls_cells>-row_id i_fieldname = 'LOW' i_value = l_second-low ).
        IF l_second-high CO '0 '.
          CLEAR l_second-high.
        ENDIF.
        <tab>-high = l_second-high.
        er_data_changed->modify_cell( EXPORTING i_row_id = <ls_cells>-row_id i_fieldname = 'HIGH' i_value = l_second-high ).

        <tab>-opti = l_second-opti.
        er_data_changed->modify_cell( EXPORTING i_row_id = <ls_cells>-row_id i_fieldname = 'OPTI' i_value = l_second-opti ).
        <tab>-sign = l_second-sign.
        er_data_changed->modify_cell( EXPORTING i_row_id = <ls_cells>-row_id i_fieldname = 'SIGN' i_value = l_second-sign ).
      ENDIF.

      update_sel_row( CHANGING c_sel_row = <tab> ).
      lcl_ace_alv_common=>refresh( EXPORTING i_obj = mo_sel_alv i_layout = ms_layout ).
      raise_selection_done( ).

    ENDMETHOD.

    METHOD on_data_changed_finished.

      CHECK e_modified IS NOT INITIAL.
      RAISE EVENT selection_done.

    ENDMETHOD.

    METHOD handle_context_menu_request.

      DATA: func  TYPE ui_func,
            funcs TYPE ui_functions.

      DATA(l_index) = lcl_ace_alv_common=>get_selected( mo_sel_alv ).

      IF l_index IS NOT INITIAL.
        READ TABLE mt_sel_tab INTO DATA(l_sel) INDEX l_index.
      ENDIF.

      e_object->get_functions( IMPORTING fcodes = DATA(fcodes) ). "Inactivate all standard functions

      LOOP AT fcodes INTO DATA(fcode) WHERE fcode NE '&OPTIMIZE'.
        func = fcode-fcode.
        APPEND func TO funcs.
      ENDLOOP.

      e_object->hide_functions( funcs ).
      e_object->add_separator( ).

      IF l_sel-range[]  IS NOT INITIAL OR l_index IS INITIAL.
        CALL METHOD e_object->add_function
          EXPORTING
            fcode = 'SEL_CLEAR'
            text  = 'Clear Select-Options'.
      ENDIF.

      IF l_sel-receiver IS NOT INITIAL OR l_index IS INITIAL.
        CALL METHOD e_object->add_function
          EXPORTING
            fcode = 'DELR'
            text  = 'Delete receiver'.
      ENDIF.

    ENDMETHOD.

    METHOD handle_user_command.

      DATA:  sel_width TYPE i.

      IF e_ucomm = 'SEL_OFF'. "Hide select-options alv

        mo_viewer->m_visible = ''.

        sel_width = 0.
        CALL METHOD mo_viewer->mo_splitter->get_column_width
          EXPORTING
            id                = 1
          IMPORTING
            result            = mo_viewer->mo_sel_width
          EXCEPTIONS
            cntl_error        = 1
            cntl_system_error = 2
            OTHERS            = 3.

        CALL METHOD mo_viewer->mo_splitter->set_column_width
          EXPORTING
            id    = 1
            width = sel_width.
        mo_viewer->mo_alv->set_toolbar_interactive( ).
        RETURN.
      ENDIF.

      IF e_ucomm = 'SEL_CLEAR' OR e_ucomm = 'DELR'. "clear all selections
        mo_sel_alv->get_selected_rows( IMPORTING et_index_rows = DATA(sel_rows) ).

        LOOP AT sel_rows INTO DATA(l_row).
          READ TABLE mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) INDEX l_row-index.
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

      lcl_ace_alv_common=>refresh( mo_viewer->mo_alv ).
      RAISE EVENT selection_done.

    ENDMETHOD.                           "handle_user_command

  ENDCLASS.

  CLASS lcl_ace_appl IMPLEMENTATION.

    METHOD init_icons_table.

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

    ENDMETHOD.

    METHOD init_lang.
      ULINE.
*    SELECT c~spras t~sptxt INTO CORRESPONDING FIELDS OF TABLE mt_lang
*      FROM t002c AS c
*      INNER JOIN t002t AS t
*      ON c~spras = t~sprsl
*      WHERE t~spras = sy-langu
*      ORDER BY c~ladatum DESCENDING c~lauzeit DESCENDING.
    ENDMETHOD.

    METHOD check_mermaid.

      CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
        EXPORTING
          clskey        = 'ZCL_WD_GUI_MERMAID_JS_DIAGRAM '
        EXCEPTIONS
          not_specified = 1
          not_existing  = 2
          i_interface   = 3
          no_text       = 4
          inconsistent  = 5
          OTHERS        = 6.

      IF sy-subrc = 0.
        i_mermaid_active = abap_true.
      ENDIF.

    ENDMETHOD.

    METHOD open_int_table.

      DATA r_tab TYPE REF TO data.
      IF it_ref IS BOUND.
        r_tab = it_ref.
      ELSE.
        GET REFERENCE OF it_tab INTO r_tab.
      ENDIF.
      APPEND INITIAL LINE TO lcl_ace_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
      <obj>-alv_viewer = NEW #(  i_additional_name = i_name ir_tab = r_tab io_window = io_window ).
      <obj>-alv_viewer->mo_sel->raise_selection_done( ).

    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_ace_rtti_tree IMPLEMENTATION.

    METHOD constructor.

      super->constructor( ).
      mo_viewer = i_debugger.

      cl_salv_tree=>factory(
        EXPORTING
          r_container = i_cont
        IMPORTING
          r_salv_tree = mo_tree
        CHANGING
          t_table     = tree_table ).

      DATA(o_setting) =  mo_tree->get_tree_settings( ).
      o_setting->set_hierarchy_header( i_header ).
      o_setting->set_hierarchy_size( 30 ).
      o_setting->set_hierarchy_icon( CONV #( icon_tree ) ).

      DATA(o_columns) = mo_tree->get_columns( ).
      o_columns->set_optimize( abap_true ).

      o_columns->get_column( 'VALUE' )->set_visible( abap_false ).
      o_columns->get_column( 'PARAM' )->set_visible( abap_false ).
      o_columns->get_column( 'INCLUDE' )->set_visible( abap_false ).

      add_buttons( i_type ).

      DATA(o_event) = mo_tree->get_event( ) .
      SET HANDLER hndl_double_click
                  hndl_user_command FOR o_event.

      mo_tree->display( ).

    ENDMETHOD.

    METHOD add_buttons.

      DATA(o_functions) = mo_tree->get_functions( ).
      o_functions->set_all( ).

      o_functions->set_group_layout( abap_false ).
      o_functions->set_group_aggregation( abap_false ).
      o_functions->set_group_print( abap_false ).

      CHECK mo_viewer IS NOT INITIAL AND i_type = 'L'.

      o_functions->add_function(
        name     = 'REFRESH'
        icon     = CONV #( icon_refresh )
        text     = ''
        tooltip  = 'Refresh'
        position = if_salv_c_function_position=>left_of_salv_functions ).

    ENDMETHOD.

    METHOD clear.

      mo_tree->get_nodes( )->delete_all( ).

    ENDMETHOD.


    METHOD add_node.

      DATA style TYPE salv_de_constant.
      IF i_tree-kind = 'F' OR i_tree-value IS NOT INITIAL.
        style = if_salv_c_tree_style=>intensified.
      ENDIF.

      rv_node =
            mo_tree->get_nodes( )->add_node(
              related_node   = i_rel
              collapsed_icon = i_icon
              expanded_icon  = i_icon
              relationship   = if_salv_c_node_relation=>last_child
              data_row       = i_tree
              row_style      = style
              text           = CONV #( i_name )
              folder         = abap_true
            )->get_key( ).

    ENDMETHOD.

    METHOD delete_node.

      DATA(o_nodes) = mo_tree->get_nodes( ).
      DATA(l_node) =  o_nodes->get_node( i_key ).
      IF l_node IS NOT INITIAL.
        l_node->delete( ).

      ENDIF.

    ENDMETHOD.

    METHOD display.

      DATA(o_columns) = mo_tree->get_columns( ).
      o_columns->get_column( 'KIND' )->set_visible( abap_false ).

      DATA(o_nodes) = mo_tree->get_nodes( ).
      DATA(nodes) =  o_nodes->get_all_nodes( ).

      DATA sub TYPE salv_t_nodes.

      LOOP AT nodes INTO DATA(l_node).

        DATA r_row TYPE REF TO data.

        r_row = l_node-node->get_data_row( ).
        ASSIGN r_row->* TO FIELD-SYMBOL(<row>).
        ASSIGN COMPONENT 'KIND' OF STRUCTURE <row> TO FIELD-SYMBOL(<kind>).

        IF <kind> = 'F'.

          TRY.
              l_node-node->expand( ).
              sub = l_node-node->get_subtree( ).
            CATCH cx_root.
          ENDTRY.
        ENDIF.
      ENDLOOP.
      mo_tree->display( ).

    ENDMETHOD.

    METHOD hndl_user_command.

      CONSTANTS: c_mask TYPE x VALUE '01'.

      CASE e_salv_function.

        WHEN 'REFRESH'."
          mo_viewer->mo_tree_local->display( ).
          RETURN.

      ENDCASE.


    ENDMETHOD.

    METHOD hndl_double_click.

      DATA(o_nodes) = mo_tree->get_nodes( ).
      DATA(o_node) =  o_nodes->get_node( node_key ).
      DATA r_row TYPE REF TO data.

      r_row = o_node->get_data_row( ).
      ASSIGN r_row->* TO FIELD-SYMBOL(<row>).
      ASSIGN COMPONENT 'KIND' OF STRUCTURE <row> TO FIELD-SYMBOL(<kind>).
      ASSIGN COMPONENT 'VALUE' OF STRUCTURE <row> TO FIELD-SYMBOL(<value>).
      ASSIGN COMPONENT 'PARAM' OF STRUCTURE <row> TO FIELD-SYMBOL(<param>).
      ASSIGN COMPONENT 'INCLUDE' OF STRUCTURE <row> TO FIELD-SYMBOL(<include>).

      IF <include> IS NOT INITIAL.
        mo_viewer->mo_window->set_program( CONV #( <include> ) ).
      ENDIF.

      mo_viewer->mo_window->set_program_line( CONV #( <value> ) ).

      IF <param> IS NOT INITIAL.
        READ TABLE mo_viewer->mt_selected_var WITH KEY name =  <param> TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          DELETE mo_viewer->mt_selected_var WHERE name = <param>.
          o_node->set_row_style( if_salv_c_tree_style=>default ).
        ELSE.
          o_node->set_row_style( if_salv_c_tree_style=>emphasized_b ).
          APPEND INITIAL LINE TO mo_viewer->mt_selected_var ASSIGNING FIELD-SYMBOL(<sel>).
          <sel>-name = <param>.
          <sel>-i_sel = abap_true.
        ENDIF.
      ENDIF.

    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_ace_dragdrop IMPLEMENTATION.

    METHOD drag.

      DATA(dataobj) = NEW lcl_ace_dragdrop_data( ).
      dataobj->m_row = e_row-index.
      dataobj->m_column = e_column.
      e_dragdropobj->object = dataobj.

    ENDMETHOD.

    METHOD drop."It should be refactored someday...

      DATA: row          TYPE lcl_ace_appl=>t_sel_row,
            set_receiver.

      LOOP AT lcl_ace_appl=>mt_obj INTO DATA(lo).
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
        FIELD-SYMBOLS: <f_tab>   TYPE STANDARD TABLE,
                       <f_field> TYPE any.
        o_from_tab->mo_alv->get_selected_cells( IMPORTING et_cell = sel_cells ).
        o_from_tab->mo_alv->get_selected_columns( IMPORTING et_index_columns = DATA(sel_col) ).

        LOOP AT sel_col INTO DATA(l_col).
          TRY.
              o_from_tab->mt_alv_catalog[ fieldname = l_col-fieldname ]-style = cl_gui_alv_grid=>mc_style_button.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.
          READ TABLE o_from_tab->mo_column_emitters WITH KEY column = l_col ASSIGNING FIELD-SYMBOL(<emitter>).
          IF sy-subrc NE 0.
            APPEND INITIAL LINE TO o_from_tab->mo_column_emitters ASSIGNING <emitter>.
            <emitter>-column = l_col.
            <emitter>-emitter = NEW #( ).
          ENDIF.
        ENDLOOP.

        IF sy-subrc = 0.
          set_receiver = abap_true.
          CALL METHOD o_from_tab->mo_alv->set_frontend_fieldcatalog EXPORTING it_fieldcatalog = o_from_tab->mt_alv_catalog.
        ENDIF.

        TRY.
            ASSIGN o_from_tab->mr_table->* TO <f_tab>.
            READ TABLE o_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to_tab>) INDEX e_row.
            LOOP AT sel_cells INTO DATA(l_cell).
              IF sy-tabix = 1.
                DATA(l_colname) = l_cell-col_id-fieldname.
              ENDIF.
              READ TABLE <f_tab> INDEX l_cell-row_id ASSIGNING FIELD-SYMBOL(<f_str>).
              ASSIGN COMPONENT l_colname OF STRUCTURE <f_str> TO <f_field>.
              IF sy-subrc = 0.
                IF  set_receiver IS NOT INITIAL.
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
                  <to_tab>-low = <f_field>.
                ENDIF.
                IF NOT line_exists( <to_tab>-range[ low = <f_field> ] ).
                  APPEND VALUE #( sign = 'I' opti = 'EQ' low = <f_field>  ) TO <to_tab>-range.
                ENDIF.
              ENDIF.
            ENDLOOP.
            o_to->update_sel_row( CHANGING c_sel_row = <to_tab> ).
          CATCH cx_sy_itab_line_not_found.              "#EC NO_HANDLER
        ENDTRY.
      ENDIF.

      "select to select
      IF o_from_sel NE o_to.
        IF sel_rows[] IS INITIAL.
          DELETE sel_cells WHERE col_id NE 'FIELD_LABEL'.
          LOOP AT sel_cells INTO DATA(l_sel).
            APPEND INITIAL LINE TO sel_rows ASSIGNING FIELD-SYMBOL(<row>).
            <row>-index = l_sel-row_id-index.
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
      lcl_ace_alv_common=>refresh( EXPORTING i_obj = o_alv ).

      o_alv ?= e_dragdropobj->droptargetctrl.
      o_to->raise_selection_done( ).

    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_ace_source_parser IMPLEMENTATION.

    METHOD parse_tokens.

      DATA: lr_scan         TYPE REF TO cl_ci_scan,
            prev            TYPE string,
            change          TYPE string,
            split           TYPE TABLE OF string,
            o_scan          TYPE REF TO cl_ci_scan,
            o_statement     TYPE REF TO if_ci_kzn_statement_iterator,
            o_procedure     TYPE REF TO if_ci_kzn_statement_iterator,
            token           TYPE lcl_ace_appl=>ts_kword,
            calculated      TYPE lcl_ace_window=>ts_var,
            composed        TYPE lcl_ace_window=>ts_var,
            tokens          TYPE lcl_ace_window=>tt_kword,
            calculated_vars TYPE lcl_ace_window=>tt_calculated,
            composed_vars   TYPE lcl_ace_window=>tt_composed,
            call            TYPE lcl_ace_window=>ts_calls,
            call_line       TYPE lcl_ace_window=>ts_calls_line,
            tab             TYPE lcl_ace_window=>ts_int_tabs,
            tabs            TYPE lcl_ace_window=>tt_tabs,
            variable        TYPE lcl_ace_window=>ts_vars,
            eventtype       TYPE string,
            eventname       TYPE string,
            param           TYPE lcl_ace_window=>ts_params,
            par             TYPE char1,
            type            TYPE char1,
            ref             TYPE boolean,
            class           TYPE boolean,
            cl_name         TYPE string,
            preferred       TYPE boolean,
            method_type     TYPE i,
            class_name      TYPE string,
            main_prog       TYPE program.

      IF i_main = abap_true.
        main_prog = i_program.
      ELSE.
        main_prog = i_include.
      ENDIF.

      READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = main_prog INTO DATA(prog).
      IF sy-subrc <> 0.

        DATA(o_source) = cl_ci_source_include=>create( p_name = i_include ).

        prog-source_tab = o_source->lines.
        o_scan = NEW cl_ci_scan( p_include = o_source ).

        "get_events.
        LOOP AT o_scan->structures INTO DATA(struc) WHERE type = 'E'.
          READ TABLE io_debugger->mo_window->ms_sources-t_events WITH KEY program = i_program stmnt_type = struc-stmnt_type ASSIGNING FIELD-SYMBOL(<event>).
          IF sy-subrc <> 0.
            APPEND INITIAL LINE TO io_debugger->mo_window->ms_sources-t_events ASSIGNING <event>.
            <event>-program = i_program.
          ENDIF.
          MOVE-CORRESPONDING struc TO <event>.
          <event>-include = i_include.
        ENDLOOP.

        prog-include = i_include.

        o_statement = cl_cikzn_scan_iterator_factory=>get_statement_iterator( ciscan = o_scan ).
        o_procedure = cl_cikzn_scan_iterator_factory=>get_procedure_iterator( ciscan = o_scan ).

        "methods in definition should be overwritten by Implementation section
        IF i_class IS NOT INITIAL.
          class = abap_true.
          call_line-class = param-class = i_class.
        ENDIF.

        DATA(kw) = o_statement->get_keyword( ).

        DATA(word) = o_statement->get_token( offset = 2 ).

        o_procedure->statement_index = o_statement->statement_index.
        o_procedure->statement_type = o_statement->statement_type.

        DATA(max) = lines( o_scan->statements ).


        DO.
          CLEAR token-tt_calls.

          TRY.
              o_procedure->next( ).
            CATCH cx_scan_iterator_reached_end.
          ENDTRY.
          kw = o_procedure->get_keyword( ).

          token-name = kw.
          token-index = o_procedure->statement_index.
          READ TABLE o_scan->statements INDEX o_procedure->statement_index INTO DATA(statement).
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

          READ TABLE o_scan->tokens INDEX statement-from INTO DATA(l_token).
          token-line = calculated-line = composed-line = l_token-row.
          token-program = i_program.
          READ TABLE o_scan->levels  INDEX statement-level INTO DATA(level).
          "IF level-type <> 'D'. "Define Macros
          IF i_include <> level-name. "includes will be processed separately
            lcl_ace_source_parser=>parse_tokens( i_program = CONV #( token-program ) i_include = CONV #( level-name ) io_debugger = io_debugger ).
            "ENDIF.

            token-include = level-name.

          ELSE.
            token-include = i_include.
            "ENDIF.


            calculated-program = composed-program = i_include.

            DATA  new TYPE boolean.

            IF kw = 'CLASS'.
              class = abap_true.
            ENDIF.

            IF kw = 'FORM' OR kw = 'METHOD' OR kw = 'METHODS' OR kw = 'CLASS-METHODS' OR kw = 'MODULE'.
              variable-eventtype = tab-eventtype =  eventtype = param-event =  kw.

              CLEAR  eventname.
              IF kw = 'FORM'.
                CLEAR:  class, param-class.
              ELSEIF kw = 'MODULE'.
                CLEAR:  class, param-class.
                tab-eventtype =  eventtype = param-event =  'MODULE'.
              ELSE.
                tab-eventtype =  eventtype = param-event =  'METHOD'.
              ENDIF.
            ENDIF.

            IF kw = 'ENDCLASS'.
              call_line-class = param-class = ''.
            ENDIF.
            IF kw = 'ENDFORM' OR kw = 'ENDMETHOD' OR kw = 'ENDMODULE'.
              CLEAR:  eventtype,  eventname, tabs, variable, token-sub.
              IF param-param IS INITIAL. "No params - save empty row if no params
                READ TABLE io_debugger->mo_window->ms_sources-t_params WITH KEY event = param-event name = param-name TRANSPORTING NO FIELDS.
                IF sy-subrc <> 0.
                  CLEAR param-type.
                  APPEND param TO io_debugger->mo_window->ms_sources-t_params.
                ENDIF.
              ENDIF.
            ENDIF.

            CLEAR  prev.
            IF kw = 'ASSIGN' OR kw = 'ADD' OR kw = 'SUBTRACT' .
              DATA(count) = 0.
            ENDIF.
            CLEAR new.

            IF eventname IS  NOT INITIAL OR class IS NOT INITIAL AND eventtype <> 'EVENT'.
              token-sub = abap_true.
            ENDIF.

            WHILE 1 = 1.
              IF kw IS INITIAL.
                EXIT.
              ENDIF.
              CLEAR  change.
              word = o_procedure->get_token( offset = sy-index ).

              IF word = 'DEFERRED'.
                CLEAR: class, call_line.
              ENDIF.

              IF ( word CS '(' AND ( NOT word CS ')' ) AND word <> '#(' AND word <> '=>' )  OR word CS '->'."can be method call

                IF call-event = 'METHOD' AND call-name IS NOT INITIAL.
                  APPEND call TO token-tt_calls.
                  CLEAR: call-event, call-type, call-name, call-outer, call-inner.
                ENDIF.

                call-name = word.
                call-event = 'METHOD'.
                REPLACE ALL OCCURRENCES OF '(' IN call-name WITH ''.
                FIND FIRST OCCURRENCE OF '->' IN  call-name.
                IF sy-subrc = 0.
                  SPLIT call-name  AT '->' INTO TABLE split.
                  IF split[ 1 ] <> ')'."to refactor
                    call-class = split[ 1 ].
                  ENDIF.
                  call-name = split[ 2 ].
                ENDIF.

                FIND FIRST OCCURRENCE OF '=>' IN  call-name.
                IF sy-subrc = 0.
                  SPLIT call-name  AT '=>' INTO TABLE split.
                  IF split[ 1 ] <> ')'."to refactor
                    call-class = split[ 1 ].
                  ENDIF.
                  call-name = split[ 2 ].
                ENDIF.

                IF call-class = 'ME' AND i_class IS NOT INITIAL.
                  call-class  =  i_class.
                ENDIF.

                IF call-class IS INITIAL AND i_class IS NOT INITIAL.
                  call-class  =  i_class.
                ENDIF.

                "token-to_evname = call-name.
                call-event = 'METHOD'.
                IF  new = abap_true.
                  call-class = call-name.
                  call-name = 'CONSTRUCTOR'.
                ENDIF.
                IF  new = abap_true.

                  call_line-class = call-class.
                  call_line-eventname = call-name.
                  call_line-eventtype = 'METHOD'.

                  READ TABLE calculated_vars WITH KEY line = l_token-row program = i_include INTO DATA(calc).
                  IF sy-subrc = 0.
                    APPEND INITIAL LINE TO  io_debugger->mo_window->ms_sources-tt_refvar ASSIGNING FIELD-SYMBOL(<refvar>).
                    <refvar>-name = calc-name.
                    <refvar>-class = call-class.
                    call-class = call-class.
                  ENDIF.
                ENDIF.

                READ TABLE io_debugger->mo_window->ms_sources-tt_refvar WITH KEY name = call-class INTO DATA(refvar).
                IF sy-subrc = 0.
                  call-class = refvar-class.
                ENDIF.

                "token-to_class = call-class.
              ENDIF.

              IF word = '#('.
                CLEAR new.
              ENDIF.

              IF sy-index = 1 AND token-name = word.
                CONTINUE.
              ENDIF.

              IF sy-index = 2 AND ( kw = 'DATA' OR kw = 'PARAMETERS' ).
                tab-name = word.

              ENDIF.

              IF sy-index = 2 AND kw = 'PERFORM'.
                call-name = word.
                call-event = 'FORM'.
              ENDIF.

              IF sy-index = 2 AND  class = abap_true AND param-class IS INITIAL.
                call_line-class = word.
                param-class = word.
              ENDIF.

              IF sy-index = 2 AND  kw = 'CLASS'.
                class_name = word.
              ENDIF.

              IF sy-index = 2 AND  eventtype IS NOT INITIAL AND  eventname IS INITIAL.
                variable-eventname = tab-eventname =  eventname = param-name = word.

                MOVE-CORRESPONDING tab TO call_line.
                call_line-index = o_procedure->statement_index + 1.
                "IF call_line-class IS INITIAL.
                call_line-class = class_name.
                "ENDIF.

                "methods in definition should be overwritten by Implementation section
                READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
                 WITH KEY class = call_line-class eventname = call_line-eventname eventtype = call_line-eventtype ASSIGNING FIELD-SYMBOL(<call_line>).
                IF sy-subrc = 0.
                  <call_line>-index = call_line-index.
                  <call_line>-include = token-include.
                ELSE.
                  IF i_class IS INITIAL.
                    call_line-program = i_program.
                    call_line-include = token-include.
                  ELSE.
                    call_line-include = token-include.
                  ENDIF.
                  call_line-meth_type = method_type.
                  APPEND call_line TO io_debugger->mo_window->ms_sources-tt_calls_line.
                ENDIF.

              ENDIF.

              IF word = ''.
                IF call IS NOT INITIAL.
                  APPEND call TO token-tt_calls.
                ENDIF.
                CLEAR call.
                CASE kw.
                  WHEN 'COMPUTE'.
                    IF  NOT  prev CO '0123456789.+-/* '.
                      composed-name =  prev.
                      APPEND  composed TO composed_vars.
                    ENDIF.
                  WHEN 'CLEAR' OR 'SORT' OR 'CONDENSE'."no logic
                  WHEN 'FORM'.
                    IF param-name IS NOT INITIAL.
                      APPEND param TO io_debugger->mo_window->ms_sources-t_params.
                      CLEAR param.
                    ENDIF.
                ENDCASE.
                EXIT.
              ENDIF.

              IF word = 'REF'.
                ref = abap_true.
              ENDIF.

              IF word = 'USING' OR word = 'IMPORTING'.
                param-type = 'I'.
                CLEAR:  type,  par.
              ELSEIF word = 'CHANGING' OR word = 'EXPORTING' OR word = 'RETURNING'.

                IF param-param IS NOT INITIAL.
                  APPEND param TO io_debugger->mo_window->ms_sources-t_params.
                  CLEAR:  type,  par, param-param.
                ENDIF.

                param-type = 'E'.
                CLEAR:  type,  par.
              ELSEIF word = 'OPTIONAL' OR word = 'PREFERRED' OR word = 'REF' OR word = 'TO'.
                CONTINUE.
              ELSEIF word = 'PARAMETER'.
                preferred = abap_true.
                CONTINUE.
              ENDIF.

              IF  preferred = abap_true.
                READ TABLE io_debugger->mo_window->ms_sources-t_params WITH KEY event = 'METHOD' name = param-name param = word ASSIGNING FIELD-SYMBOL(<param>).
                IF sy-subrc = 0.
                  <param>-preferred = abap_true.
                ENDIF.

                CLEAR  preferred.
                CONTINUE.
              ENDIF.

              IF word <> 'CHANGING' AND word <> 'EXPORTING' AND word <> 'RETURNING' AND word <> 'IMPORTING' AND word <> 'USING'.
                IF kw = 'FORM' OR kw = 'METHODS' OR kw = 'CLASS-METHODS'.
                  IF  par = abap_true AND  type IS INITIAL AND word NE 'TYPE'.

                    APPEND param TO io_debugger->mo_window->ms_sources-t_params.
                    CLEAR:  par, param-param.
                  ENDIF.

                  IF  par IS INITIAL AND sy-index > 3.
                    param-param = word.
                    par = abap_true.
                    CONTINUE.
                  ENDIF.
                  IF  par = abap_true AND  type IS INITIAL AND word = 'TYPE'.
                    type = abap_true.
                    CONTINUE.
                  ENDIF.
                  IF  par = abap_true AND  type = abap_true.
                    REPLACE ALL OCCURRENCES OF 'VALUE(' IN param-param WITH ''.
                    REPLACE ALL OCCURRENCES OF ')' IN param-param WITH ''.
                    APPEND param TO io_debugger->mo_window->ms_sources-t_params.
                    CLEAR:  type,  par, param-param.
                  ENDIF.
                ENDIF.
              ENDIF.

              DATA  temp TYPE char30.
              temp = word.

              IF  temp+0(5) = 'DATA('.
                SHIFT  temp LEFT BY 5 PLACES.
                REPLACE ALL OCCURRENCES OF ')' IN  temp WITH ''.
              ENDIF.

              IF  temp+0(6) = '@DATA('.
                SHIFT  temp LEFT BY 6 PLACES.
                REPLACE ALL OCCURRENCES OF ')' IN  temp WITH ''.
              ENDIF.

              IF  temp+0(13) = 'FIELD-SYMBOL('.
                SHIFT  temp LEFT BY 13 PLACES.
                REPLACE ALL OCCURRENCES OF ')' IN  temp WITH ''.
              ENDIF.

              IF word = 'NEW'.
                new = abap_true.

              ENDIF.

              FIND FIRST OCCURRENCE OF '->' IN word.
              IF sy-subrc = 0.
                CLEAR  new.
              ENDIF.

              CASE kw.

                WHEN 'PUBLIC'.
                  method_type = 1.

                WHEN 'PROTECTED'.
                  method_type = 2.

                WHEN 'PRIVATE'.
                  method_type = 3.

                WHEN 'DATA' OR 'PARAMETERS'.
                  IF (   prev = 'OF' ) AND  temp <> 'TABLE' AND  temp <> 'OF'.
                    tab-type =  temp.
                    APPEND tab TO tabs.

                    variable-name = tab-name.
                    variable-type = tab-type.
                    variable-line = l_token-row.
                    variable-icon = icon_table_settings.
                    APPEND variable TO prog-t_vars.
                  ENDIF.

                  IF (   prev = 'TYPE' ) AND  temp <> 'TABLE' AND  temp <> 'OF'.
                    variable-name = tab-name.
                    variable-type = temp.
                    variable-line = l_token-row.

                    CASE variable-type.
                      WHEN 'D'.
                        variable-icon = icon_date.
                      WHEN 'T'.
                        variable-icon = icon_bw_time_sap.
                      WHEN 'C'.
                        variable-icon = icon_wd_input_field.
                      WHEN 'P'.
                        variable-icon = icon_increase_decimal.
                      WHEN 'STRING'.
                        variable-icon = icon_text_act.
                      WHEN 'N' OR 'I'.
                        variable-icon = icon_pm_order.
                      WHEN OTHERS.
                        variable-icon = icon_element.
                    ENDCASE.
                    IF ref IS NOT INITIAL.
                      variable-icon = icon_oo_class.
                      CLEAR ref.
                    ENDIF.
                    APPEND variable TO prog-t_vars.

                  ENDIF.

                WHEN 'COMPUTE'.
                  IF  temp CA '=' AND  new IS INITIAL..
                    change =  prev.
                  ENDIF.

                  IF (  prev = '=' OR  prev CA '+-/*' ) AND  temp <> 'NEW'.
                    IF NOT  temp  CA '()' .
                      IF NOT  temp  CO '0123456789. '.
                        composed-name =  temp.
                        APPEND  composed TO composed_vars.
                        IF call IS NOT INITIAL.
                          call-outer =  temp.
                          READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                          IF sy-subrc <> 0.
                            APPEND call TO token-tt_calls.
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                WHEN 'PERFORM' .

                  IF   temp = 'USING' OR  temp = 'CHANGING' .
                    CLEAR  prev.
                  ENDIF.

                  IF   prev = 'USING' OR  prev = 'CHANGING' .

                    IF NOT  temp  CA '()' .
                      IF NOT  temp  CO '0123456789. '.
                        call-outer =  temp.
                        READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                        IF sy-subrc <> 0.
                          APPEND call TO token-tt_calls.
                        ENDIF.
                        change =  temp.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                WHEN 'CREATE' OR 'CALL'.
                  DATA: import TYPE boolean,
                        export.

                  IF  prev = 'FUNCTION' AND kw = 'CALL'.
                    call_line-eventtype = call-event = 'FUNCTION'.
                    call_line-eventname = call-name = word.
                    "REPLACE ALL OCCURRENCES OF '''' IN  token-to_evname WITH ''.
                    REPLACE ALL OCCURRENCES OF '''' IN  call_line-eventname WITH ''.

                    READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY eventtype = call_line-eventtype eventname = call_line-eventname TRANSPORTING NO FIELDS.
                    IF sy-subrc <> 0.
                      CLEAR call_line-class.
                      APPEND call_line TO io_debugger->mo_window->ms_sources-tt_calls_line.
                    ENDIF.

                  ENDIF.

                  IF  prev = 'SCREEN' AND kw = 'CALL'.
                    APPEND INITIAL LINE TO token-tt_calls ASSIGNING FIELD-SYMBOL(<call>).

                    <call>-event = 'SCREEN'.
                    <call>-name = temp.
                    token-program = i_program.
                  ENDIF.

                  IF word = 'EXPORTING' OR word = 'CHANGING' OR word = 'TABLES'.
                    export = abap_true.
                    CLEAR  import.
                    CONTINUE.

                  ELSEIF word = 'IMPORTING'.
                    import = abap_true.
                    CLEAR  export.
                    CONTINUE.

                  ENDIF.

                  IF  prev = 'OBJECT'.
                    READ TABLE prog-t_vars WITH KEY icon = icon_oo_class name = word INTO DATA(var).
                    IF sy-subrc = 0.
                      "token-to_class = var-type.
                      "token-to_evtype = 'METHOD'.
                      "token-to_evname = 'CONSTRUCTOR'.
                    ENDIF.

                    "WRITE : 'value',  temp.
                  ENDIF.

                  IF   prev = '='.
                    IF NOT  temp  CA '()'.
                      IF NOT  temp  CO '0123456789. '.
                        IF  import = abap_true.
                          call-outer =  temp.
                          READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                          IF sy-subrc <> 0.
                            APPEND call TO token-tt_calls.
                          ENDIF.
                          calculated-name =  temp.
                          APPEND  calculated TO calculated_vars.
                        ELSEIF  export = abap_true.
                          call-outer =  temp.
                          READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                          IF sy-subrc <> 0.
                            APPEND call TO token-tt_calls.
                          ENDIF.
                          composed-name =  temp.
                          APPEND  composed TO composed_vars.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  ELSE.
                    IF NOT  temp  CO '0123456789. ' AND  temp <> '=' AND (  import = abap_true OR  export = abap_true ).
                      call-inner =  temp.
                    ENDIF.
                  ENDIF.

                WHEN 'CLEAR' OR 'SORT'.
                  change =  temp.
                WHEN  'CONDENSE'.

                  IF  temp <> 'NO-GAPS'.
                    change =  temp.
                  ENDIF.
                WHEN 'ASSIGN' OR 'UNASSIGN'.
                  ADD 1 TO  count.
                  IF  count <> 2.
                    change =  temp.
                  ENDIF.
                WHEN 'ADD' OR 'SUBTRACT'.
                  ADD 1 TO  count.
                  IF  count = 1.
                    IF  NOT  temp CO '0123456789.() '.
                      composed-name =  temp.
                      APPEND  composed TO composed_vars.
                    ENDIF.
                  ENDIF.
                  IF  count = 3.
                    change =  temp.
                  ENDIF.
                WHEN 'READ'.
                  IF  prev =  'INTO' OR  prev =  'ASSIGNING'.
                    change =  temp.
                  ENDIF.

                WHEN 'SELECT'.
                  IF  (  prev =  'INTO' OR  prev =  '(' ) AND (  temp <> 'TABLE' AND  temp <> '('  AND  temp <> ')' AND   temp <> ',' ).
                    change =  temp.
                  ENDIF.

                WHEN OTHERS.

              ENDCASE.
              IF call-event = 'METHOD'.
                IF word = 'EXPORTING' OR word = 'CHANGING' OR word = 'TABLES'.
                  export = abap_true.
                  CLEAR  import.
                  CONTINUE.

                ELSEIF word = 'IMPORTING'.
                  import = abap_true.
                  CLEAR  export.
                  CONTINUE.
                ENDIF.

                IF   temp = 'USING' OR  temp = 'CHANGING' .
                  CLEAR  prev.
                ENDIF.

                IF   prev = 'USING' OR  prev = 'CHANGING' .

                  IF NOT  temp  CA '()' .
                    IF NOT  temp  CO '0123456789. '.
                      call-outer =  temp.
                      READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                      IF sy-subrc <> 0.
                        APPEND call TO token-tt_calls.
                      ENDIF.
                      change =  temp.
                    ENDIF.
                  ENDIF.
                ENDIF.

                IF   prev = '='.
                  IF NOT  temp  CA '()'.
                    IF NOT  temp  CO '0123456789. '.
                      IF  import = abap_true.
                        call-outer =  temp.
                        READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                        IF sy-subrc <> 0.
                          APPEND call TO token-tt_calls.
                        ENDIF.

                        calculated-name =  temp.
                        APPEND  calculated TO calculated_vars.
                      ELSEIF  export = abap_true.
                        call-outer =  temp.
                        READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                        IF sy-subrc <> 0.
                          APPEND call TO token-tt_calls.
                        ENDIF.
                        composed-name =  temp.
                        APPEND  composed TO composed_vars.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ELSE.
                  IF NOT  temp  CO '0123456789. ' AND  temp <> '=' AND temp <> ')' AND (  import = abap_true OR  export = abap_true ).
                    call-inner =  temp.
                  ENDIF.
                ENDIF.

              ENDIF.

              IF  temp = '(' .
                prev =  temp.
                CONTINUE.
              ENDIF.

              IF  NOT  temp  CA '()'.
                IF  temp <> 'TABLE' AND  temp <> 'NEW'  AND  prev <> '('.
                  IF  kw <> 'PERFORM'.
                    prev =  temp.
                  ELSEIF word = 'USING' OR word = 'CHANGING'.
                    prev =  temp.
                  ENDIF.
                ENDIF.
              ENDIF.

              IF  change IS NOT INITIAL.
                calculated-name =  change.
                APPEND calculated TO calculated_vars.

                IF  change+0(1) = '<'.

                  SPLIT  change AT '-' INTO TABLE split.
                  change = split[ 1 ].
                  IF  eventtype IS INITIAL. "Global fs
                    READ TABLE io_debugger->mo_window->mt_globals_set WITH KEY program = i_include ASSIGNING FIELD-SYMBOL(<globals_set>).
                    IF sy-subrc <> 0.
                      APPEND INITIAL LINE TO io_debugger->mo_window->mt_globals_set ASSIGNING <globals_set>.
                      <globals_set>-program = i_include.
                    ENDIF.
                    READ TABLE  <globals_set>-mt_fs WITH KEY name =  change TRANSPORTING NO FIELDS.
                    IF sy-subrc <> 0.
                      APPEND INITIAL LINE TO  <globals_set>-mt_fs ASSIGNING FIELD-SYMBOL(<gl_fs>).
                      <gl_fs>-name =  change.
                    ENDIF.

                  ELSE."local fs
                    READ TABLE io_debugger->mo_window->mt_locals_set
                     WITH KEY program = i_include eventtype =  eventtype eventname =  eventname
                     ASSIGNING FIELD-SYMBOL(<locals_set>).
                    IF sy-subrc <> 0.
                      APPEND INITIAL LINE TO io_debugger->mo_window->mt_locals_set ASSIGNING <locals_set>.
                      <locals_set>-program = i_include.
                      <locals_set>-eventname =  eventname.
                      <locals_set>-eventtype =  eventtype.
                    ENDIF.
                    READ TABLE <locals_set>-mt_fs WITH KEY name =  change TRANSPORTING NO FIELDS.
                    IF sy-subrc <> 0.
                      APPEND INITIAL LINE TO <locals_set>-mt_fs ASSIGNING FIELD-SYMBOL(<loc_fs>).
                      <loc_fs>-name =  change.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.

            ENDWHILE.
            token-from = statement-from.
            token-to = statement-to.
*            IF i_class IS INITIAL.
*              token-to_prog = i_include.
*            ENDIF.
            "check class names

*            IF token-to_class IS INITIAL AND token-to_evname <> 'CONSTRUCTOR'. "to refactor
*              READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line INTO call_line WITH KEY eventname = token-to_evname  eventtype = token-to_evtype .
*              IF sy-subrc = 0.
*                token-to_class = call_line-class.
*              ENDIF.
*            ENDIF.
*            IF token-to_class IS NOT INITIAL. "check ref variable
*              READ TABLE prog-t_vars WITH KEY name = token-to_class icon = icon_oo_class INTO var.
*              IF sy-subrc = 0.
*                token-to_class = var-type.
*              ENDIF.
*            ENDIF.

            "SORT token-tt_calls.
            "DELETE ADJACENT DUPLICATES FROM token-tt_calls.

            APPEND token TO tokens.
            IF kw = 'ENDCLASS'.
              CLEAR: token-sub, class.
            ENDIF.
            "ELSE.
            "lcl_ace_source_parser=>parse_tokens( i_program = CONV #( token-include ) i_include = CONV #( token-include ) io_debugger = io_debugger ).
            "ENDIF.
          ENDIF.

          IF o_procedure->statement_index =  max.
            EXIT.
          ENDIF.

        ENDDO.

        "Fill keyword links for calls

        LOOP AT tokens ASSIGNING FIELD-SYMBOL(<s_token>) WHERE tt_calls IS NOT INITIAL.

          READ TABLE <s_token>-tt_calls INDEX 1 INTO call.
          DATA(index) = 0.
          LOOP AT io_debugger->mo_window->ms_sources-t_params INTO param WHERE event = call-event AND name = call-name .
            ADD 1 TO  index.
            READ TABLE <s_token>-tt_calls INDEX  index ASSIGNING <call>.
            IF sy-subrc = 0.
              <call>-inner = param-param.
              IF param-type = 'I'.
                <call>-type = '>'.
              ELSE.
                <call>-type = '<'.
              ENDIF.
            ENDIF.
          ENDLOOP.

        ENDLOOP.

        "clear value(var) to var.
        LOOP AT io_debugger->mo_window->ms_sources-t_params ASSIGNING <param>.
          REPLACE ALL OCCURRENCES OF 'VALUE(' IN <param>-param WITH ''.
          REPLACE ALL OCCURRENCES OF ')' IN <param>-param WITH ''.
        ENDLOOP.

        APPEND LINES OF calculated_vars TO io_debugger->mo_window->ms_sources-t_calculated.
        APPEND LINES OF composed_vars TO io_debugger->mo_window->ms_sources-t_composed.

        io_debugger->mo_window->ms_sources-tt_tabs = tabs.
        DATA line LIKE LINE OF io_debugger->mo_window->ms_sources-tt_progs.
        prog-scan = o_scan.
        prog-t_keywords = tokens.
        prog-program = i_program.
        APPEND prog TO io_debugger->mo_window->ms_sources-tt_progs.

        "IF io_debugger->m_step IS INITIAL.
        "code_execution_scanner( i_program = i_include i_include = i_include io_debugger = io_debugger ).

        "Fill keyword links for calls
        LOOP AT io_debugger->mo_window->ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<prog>).
          LOOP AT prog-t_keywords ASSIGNING <s_token> WHERE tt_calls IS NOT INITIAL.

            READ TABLE <s_token>-tt_calls INDEX 1 INTO call.
            index = 0.
            LOOP AT io_debugger->mo_window->ms_sources-t_params INTO param WHERE event = call-event AND name = call-name .
              ADD 1 TO  index.
              READ TABLE <s_token>-tt_calls INDEX  index ASSIGNING <call>.
              IF sy-subrc = 0.
                <call>-inner = param-param.
                IF param-type = 'I'.
                  <call>-type = '>'.
                ELSE.
                  <call>-type = '<'.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDLOOP.
        ENDLOOP.

      ENDIF.

    ENDMETHOD.

    METHOD code_execution_scanner.
      "code execution scanner
      DATA: max       TYPE i,
            call_line TYPE lcl_ace_window=>ts_calls_line,
            program   TYPE program,
            include   TYPE program,
            prefix    TYPE string,
            event     TYPE string,
            stack     TYPE i,
            statement TYPE i,
            prog      TYPE lcl_ace_window=>ts_prog.

      SORT io_debugger->mo_window->ms_sources-tt_calls_line.

      READ TABLE io_debugger->mt_steps WITH KEY program = i_include eventname = i_evname eventtype = i_evtype TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        RETURN.
      ENDIF.

      stack =  i_stack + 1.
      CHECK  stack <=  io_debugger->mo_window->m_hist_depth.

      lcl_ace_source_parser=>parse_tokens( i_program = i_program i_include = i_include io_debugger = io_debugger ).
      READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = i_include ASSIGNING FIELD-SYMBOL(<prog>).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      DATA: structures LIKE <prog>-scan->structures.

      LOOP AT <prog>-scan->structures INTO DATA(structure) WHERE type = 'E' AND ( stmnt_type = '1' OR stmnt_type = '2' OR stmnt_type = '3' ) .
      ENDLOOP.

      IF sy-subrc = 0.
        structures = <prog>-scan->structures.
        DELETE structures WHERE type <> 'E'.
        LOOP AT structures  ASSIGNING FIELD-SYMBOL(<structure>) WHERE stmnt_type = 'g'.
          CLEAR <structure>-stmnt_type.
        ENDLOOP.
        SORT structures BY stmnt_type ASCENDING.
      ELSE.
        CLEAR  max.
        LOOP AT <prog>-scan->structures INTO DATA(str) WHERE type <> 'C' AND type <> 'R'.
          IF str-type = 'P' AND  str-stmnt_type = '?'.
            CONTINUE.
          ENDIF.
          IF  max < str-stmnt_to.
            max = str-stmnt_to.
            APPEND str TO structures.
          ENDIF.
        ENDLOOP.
      ENDIF.

      LOOP AT structures INTO str.

        IF str-type = 'E'.
          "get event name.
          READ TABLE io_debugger->mo_window->ms_sources-t_events WITH KEY program = i_program stmnt_type = str-stmnt_type ASSIGNING FIELD-SYMBOL(<event>).
          READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = <event>-include INTO prog.

          READ TABLE prog-scan->statements INDEX <event>-stmnt_from INTO DATA(command).
          READ TABLE prog-scan->levels INDEX command-level INTO DATA(level).
          CLEAR event.
          LOOP AT prog-scan->tokens FROM command-from TO command-to INTO DATA(word).
            IF event IS INITIAL.
              event = word-str.
            ELSE.
              event = |{ event } { word-str }|.
            ENDIF.
          ENDLOOP.

          <event>-name = event.
          <event>-line = word-row.

          statement = <event>-stmnt_from + 1.


        ELSE.
          statement = str-stmnt_from.
          prog = <prog>.
        ENDIF.

        READ TABLE prog-t_keywords WITH KEY index =  str-stmnt_from INTO DATA(key).
        lcl_ace_source_parser=>parse_tokens( i_program = CONV #( key-program ) i_include = CONV #( key-include ) io_debugger = io_debugger ).
        CHECK sy-subrc = 0.

        WHILE  statement <= str-stmnt_to.
          READ TABLE prog-t_keywords WITH KEY index =   statement INTO key.

          IF key-name = 'DATA' OR key-name = 'TYPES' OR key-name = 'CONSTANTS'
            OR key-name = 'PARAMETERS' OR key-name = 'INCLUDE' OR key-name = 'REPORT'
            OR key-name IS INITIAL OR sy-subrc <> 0 OR key-sub IS NOT INITIAL.

            ADD 1 TO  statement.
            CONTINUE.
          ENDIF.
          ADD 1 TO io_debugger->m_step.

          READ TABLE io_debugger->mt_steps WITH KEY line = key-line program = i_program include = key-include TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING FIELD-SYMBOL(<step>).

            <step>-step = io_debugger->m_step.
            <step>-line = key-line.
            IF i_evtype IS INITIAL.
              <step>-eventtype = 'EVENT'.
              <step>-eventname =  event.
            ELSE.
              <step>-eventtype = i_evtype.
              <step>-eventname = i_evname.
            ENDIF.
            <step>-stacklevel =  stack.
            <step>-program = i_program.
            <step>-include = key-include.
            IF  <step>-eventtype = 'METHOD'.

            ENDIF.
          ENDIF.

          LOOP AT key-tt_calls INTO DATA(call).
            IF call-name IS NOT INITIAL AND NOT ( call-event = 'METHOD' AND call-class IS INITIAL ).

              IF call-event = 'FORM'.
                READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY eventname = call-name eventtype = call-event INTO call_line.
                IF sy-subrc = 0.
                  lcl_ace_source_parser=>parse_call( EXPORTING i_index = call_line-index
                                                   i_e_name = call_line-eventname
                                                   i_e_type = call_line-eventtype
                                                   i_program = CONV #( call_line-program )
                                                   i_include = CONV #( call_line-include )
                                                   i_stack   =  stack
                                                   io_debugger = io_debugger ).
                ENDIF.
              ELSEIF call-event = 'FUNCTION'.
                DATA:  func TYPE rs38l_fnam.
                func = call-name.
                IF io_debugger->mo_window->m_zcode IS INITIAL OR
                 ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND (  func+0(1) = 'Z' OR  func+0(1) = 'Y' ) ) .

                  CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
                    CHANGING
                      funcname            = func
                      include             = include
                    EXCEPTIONS
                      function_not_exists = 1
                      include_not_exists  = 2
                      group_not_exists    = 3
                      no_selections       = 4
                      no_function_include = 5
                      OTHERS              = 6.

                  code_execution_scanner( i_program =  include i_include =  include i_stack =  stack i_evtype = call-event i_evname = call-name io_debugger = io_debugger ).
                ENDIF.
              ELSEIF call-event = 'METHOD'. "Method call
                parse_class( i_include = i_include i_call = call i_stack = stack io_debugger = io_debugger key = key ).
              ELSEIF call-event = 'SCREEN'. "Method call
                parse_screen( i_stack = stack i_call = call io_debugger = io_debugger key = key ).
              ENDIF.
            ENDIF.
          ENDLOOP.

          ADD 1 TO  statement.
        ENDWHILE.

      ENDLOOP.

    ENDMETHOD.


    METHOD parse_call.
      DATA: statement TYPE i,
            stack     TYPE i,
            include   TYPE progname,
            prefix    TYPE string,
            program   TYPE program.

      stack = i_stack + 1.
      CHECK  stack <= io_debugger->mo_window->m_hist_depth.

      READ TABLE io_debugger->mt_steps WITH KEY program = i_include eventname = i_e_name eventtype = i_e_type TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        RETURN.
      ENDIF.

      READ TABLE io_debugger->mo_window->mt_calls WITH KEY include  = i_include ev_name = i_e_name TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        EXIT.
      ELSE.
        APPEND INITIAL LINE TO io_debugger->mo_window->mt_calls ASSIGNING FIELD-SYMBOL(<method_call>).
        <method_call>-include = i_include.
        <method_call>-ev_name = i_e_name.
      ENDIF.

      DATA: cl_key        TYPE seoclskey,
            meth_includes TYPE seop_methods_w_include.
      cl_key = i_class.
      CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
        EXPORTING
          clskey                       = cl_key
        IMPORTING
          includes                     = meth_includes
        EXCEPTIONS
          _internal_class_not_existing = 1
          OTHERS                       = 2.

      IF lines( meth_includes ) IS INITIAL.
        statement = i_index.
      ELSE.
        statement = 1.
      ENDIF.

      IF i_include IS NOT INITIAL.
        READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = i_include INTO DATA(prog).
        IF sy-subrc <> 0.
          lcl_ace_source_parser=>parse_tokens( i_program = i_program i_include = i_include io_debugger = io_debugger ).
          READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = i_include INTO prog.

        ENDIF.


      ELSE.
        READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = i_program INTO prog.
        IF sy-subrc <> 0.
          lcl_ace_source_parser=>parse_tokens( i_program = i_program i_include = i_program io_debugger = io_debugger ).
          READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = i_include INTO prog.
        ENDIF.
      ENDIF.
      DATA(max) = lines( prog-scan->statements ).
      DO.
        IF  statement >  max.
          EXIT.
        ENDIF.
        READ TABLE prog-t_keywords WITH KEY index =   statement INTO DATA(key).
        IF sy-subrc <> 0.
          ADD 1 TO  statement.
          CONTINUE.
        ENDIF.
        IF key-name = 'DATA' OR key-name = 'TYPES' OR key-name = 'CONSTANTS' OR key-name IS INITIAL.
          ADD 1 TO  statement.
          CONTINUE.
        ENDIF.
        lcl_ace_source_parser=>parse_tokens( i_program = CONV #( key-program ) i_include = CONV #( key-include ) io_debugger = io_debugger ).

        READ TABLE io_debugger->mt_steps WITH KEY line = key-line program = i_program include = key-include TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          ADD 1 TO io_debugger->m_step.
          APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING FIELD-SYMBOL(<step>).

          <step>-step = io_debugger->m_step.
          <step>-line = key-line.
          <step>-eventname = i_e_name.
          <step>-eventtype = i_e_type.
          <step>-stacklevel =  stack.
          <step>-program = i_program.
          <step>-include = key-include.
          <step>-class   = i_class.
        ENDIF.
        LOOP AT key-tt_calls INTO DATA(call).
          IF call-name IS NOT INITIAL AND NOT ( call-event = 'METHOD' AND call-class IS INITIAL ).
            .
            IF call-event = 'FORM'.

              READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY eventname = call-name eventtype = call-event INTO DATA(call_line).
              IF sy-subrc = 0.
                lcl_ace_source_parser=>parse_call( EXPORTING i_index = call_line-index
                                                         i_e_name = call_line-eventname
                                                         i_e_type = call_line-eventtype
                                                         i_program = i_include
                                                         i_include = i_include
                                                         i_stack   =  stack
                                                         io_debugger = io_debugger ).
              ENDIF.

            ELSEIF call-event = 'FUNCTION'.
              DATA:  func TYPE rs38l_fnam.
              func = call-name.
              IF io_debugger->mo_window->m_zcode IS INITIAL OR
                ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND (  func+0(1) = 'Z' OR  func+0(1) = 'Y' ) ) .

                CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
                  CHANGING
                    funcname            = func
                    include             = include
                  EXCEPTIONS
                    function_not_exists = 1
                    include_not_exists  = 2
                    group_not_exists    = 3
                    no_selections       = 4
                    no_function_include = 5
                    OTHERS              = 6.

                code_execution_scanner( i_program =  include i_include =  include i_stack =  stack i_evtype = call-event i_evname = call-name io_debugger = io_debugger ).
              ENDIF.

            ELSEIF call-event = 'METHOD'. "Method call

              DATA inlude TYPE program.
              IF i_include IS INITIAL.
                include = i_program.
              ELSE.
                include = i_include.
              ENDIF.
              parse_class( i_include = include i_call = call i_stack = stack io_debugger = io_debugger key = key ).
            ELSEIF call-event = 'SCREEN'. "Method call
              parse_screen( i_stack = stack i_call = call io_debugger = io_debugger key = key ).

            ENDIF.

          ENDIF.
        ENDLOOP.

        IF key-name = 'ENDFORM' OR key-name = 'ENDMETHOD' OR key-name = 'ENDMODULE'.
          RETURN.
        ENDIF.

        ADD 1 TO  statement.
      ENDDO.
    ENDMETHOD.

    METHOD parse_class.

      DATA: cl_key        TYPE seoclskey,
            meth_includes TYPE seop_methods_w_include,
            prefix        TYPE string,
            program       TYPE program,
            include       TYPE progname.

      cl_key = i_call-class.

      CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
        EXPORTING
          clskey                       = cl_key
        IMPORTING
          includes                     = meth_includes
        EXCEPTIONS
          _internal_class_not_existing = 1
          OTHERS                       = 2.

      IF io_debugger->mo_window->m_zcode IS INITIAL OR
       ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND ( i_call-class+0(1) = 'Z' OR i_call-class+0(1) = 'Y' ) )
         OR meth_includes IS INITIAL.

        IF lines( meth_includes ) > 0.
          prefix = i_call-class && repeat( val = `=` occ = 30 - strlen( i_call-class ) ).
          program = prefix && 'CP'.
          include =  prefix && 'CU'.
          lcl_ace_source_parser=>parse_tokens( i_program = program i_include = include io_debugger = io_debugger i_class = i_call-class ).

          include =  prefix && 'CI'.
          lcl_ace_source_parser=>parse_tokens( i_program = program i_include = include io_debugger = io_debugger i_class = i_call-class ).

          include =  prefix && 'CO'.
          lcl_ace_source_parser=>parse_tokens( i_program = program i_include = include io_debugger = io_debugger i_class = i_call-class ).

          READ TABLE meth_includes[] WITH KEY cpdkey-cpdname = i_call-name INTO DATA(incl).                        .
          IF sy-subrc = 0.
            include = incl-incname.
            lcl_ace_source_parser=>parse_tokens( i_program = program i_include =  include io_debugger = io_debugger i_class = i_call-class i_evname = i_call-name ).
          ENDIF.
        ELSE.
          program = i_include.
        ENDIF.
        IF include IS INITIAL.

        ENDIF.

        READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY class = cl_key eventtype = 'METHOD' eventname = i_call-name INTO DATA(call_line).
        IF sy-subrc = 0.
          IF include IS INITIAL.
            include =  call_line-include.
          ENDIF.
          lcl_ace_source_parser=>parse_call( EXPORTING i_index = call_line-index
                                i_e_name = call_line-eventname
                                i_e_type = call_line-eventtype
                                i_program =  CONV #( include )
                                i_include =  CONV #( include )
                                i_class = call_line-class
                                i_stack   =  i_stack
                                io_debugger = io_debugger ).
        ENDIF.
      ENDIF.


    ENDMETHOD.

    METHOD parse_screen.

      DATA: stack    TYPE i,
            ftab     TYPE STANDARD TABLE OF d021s,
            scr_code TYPE STANDARD TABLE OF d022s,
            prog     TYPE progname,
            num(4)   TYPE n,
            fmnum    TYPE sychar04,
            code_str TYPE string,
            pbo      TYPE boolean,
            pai      TYPE boolean,
            split    TYPE TABLE OF string.

      stack = i_stack + 1.
      prog = key-program.
      fmnum = num = i_Call-name.

      CALL FUNCTION 'RS_IMPORT_DYNPRO'
        EXPORTING
          dyname = prog
          dynumb = fmnum
        TABLES
          ftab   = ftab
          pltab  = scr_code
        EXCEPTIONS
          OTHERS = 19.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      LOOP AT scr_code ASSIGNING FIELD-SYMBOL(<code>).
        CONDENSE <code>.
        FIND '"' IN <code> MATCH OFFSET DATA(pos).

        IF pos <> 0.
          <code> = <code>+0(pos).  " обрезаем до кавычки
        ENDIF.
      ENDLOOP.

      DELETE scr_code WHERE line+0(1) = '*' OR line+0(1) = '"' OR line IS INITIAL.

      LOOP AT scr_code INTO DATA(code).
        IF code_str IS INITIAL.
          code_str = code-line.
        ELSE.
          code_str = |{ code_str } { code-line }|.
        ENDIF.
      ENDLOOP.

      SPLIT code_str AT '.' INTO TABLE scr_code.

      "PBO
      LOOP AT scr_code INTO code.
        CONDENSE code-line.
        IF code-line = 'PROCESS BEFORE OUTPUT'.
          pbo = abap_true.
          CLEAR pai.
          APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING FIELD-SYMBOL(<step>).
          ADD 1 TO  io_debugger->m_step.
          <step>-step = io_debugger->m_step.
          <step>-line = key-line.
          <step>-eventname = i_call-name.
          <step>-eventtype = i_call-event.
          <step>-stacklevel =  stack.
          <step>-program = key-program.
          <step>-include = key-include.

          CONTINUE.
        ENDIF.
        IF code-line = 'PROCESS AFTER INPUT'.
          pai = abap_true.
          CLEAR pbo.
          CONTINUE.
        ENDIF.

        CHECK pbo IS NOT INITIAL.
        SPLIT code-line AT | | INTO TABLE split.
        CHECK split[ 1 ] = 'MODULE'.

        READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY program = key-program eventtype = 'MODULE' eventname = split[ 2 ] INTO DATA(call_line).
        IF sy-subrc = 0.
          lcl_ace_source_parser=>parse_call( EXPORTING i_index = call_line-index
                                i_e_name = call_line-eventname
                                i_e_type = call_line-eventtype
                                i_program =  CONV #( call_line-program )
                                i_include =  CONV #( call_line-include )
                                i_stack   =  stack
                                io_debugger = io_debugger ).
        ENDIF.

      ENDLOOP.

      "PAI
      LOOP AT scr_code INTO code.
        CONDENSE code-line.
        IF code-line = 'PROCESS AFTER INPUT'.
          CLEAR pbo.
          pai = abap_true.
          "READ TABLE io_debugger->mt_steps WITH KEY line = key-line program = key-program include = key-include TRANSPORTING NO FIELDS.
          "IF sy-subrc <> 0.
          APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING <step>.
          ADD 1 TO  io_debugger->m_step.
          <step>-step = io_debugger->m_step.
          <step>-line = key-line.
          <step>-eventname = i_call-name.
          <step>-eventtype = i_call-event. <step>-stacklevel =  stack.
          <step>-program = key-program.
          <step>-include = key-include.
          "ENDIF.
          CONTINUE.
        ENDIF.
        IF code-line = 'PROCESS BEFORE OUTPUT'.
          pbo = abap_true.
          CLEAR pai.
          CONTINUE.
        ENDIF.

        CHECK pai IS NOT INITIAL.
        SPLIT code-line AT | | INTO TABLE split.
        CHECK split[ 1 ] = 'MODULE'.

        READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY program = key-program eventtype = 'MODULE' eventname = split[ 2 ] INTO call_line.
        IF sy-subrc = 0.
          lcl_ace_source_parser=>parse_call( EXPORTING i_index = call_line-index
                                i_e_name = call_line-eventname
                                i_e_type = call_line-eventtype
                                i_program =  CONV #( call_line-program )
                                i_include =  CONV #( call_line-include )
                                i_stack   =  stack
                                io_debugger = io_debugger ).
        ENDIF.

      ENDLOOP.

    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_ace_mermaid IMPLEMENTATION.

    METHOD constructor.

      DATA  text TYPE text100.

      super->constructor( ).

      mo_viewer = io_debugger.
      mv_type = i_type.

      CHECK lcl_ace_appl=>i_mermaid_active = abap_true.

      CASE mv_type.
        WHEN 'CALLS'.
          text = 'Calls flow'.
        WHEN 'FLOW'.
          text = 'Calculations sequence'.
      ENDCASE.

      IF mo_box IS INITIAL.
        mo_box = create( i_name =  text i_width = 1000 i_hight = 300 ).

        "save new popup ref
        APPEND INITIAL LINE TO lcl_ace_appl=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>).
        <popup>-parent = mo_viewer->mo_window->mo_box.
        <popup>-child = mo_box.

        SET HANDLER on_box_close FOR mo_box.

        CREATE OBJECT mo_splitter
          EXPORTING
            parent  = mo_box
            rows    = 2
            columns = 1
          EXCEPTIONS
            OTHERS  = 1.

        mo_splitter->get_container(
          EXPORTING
            row       = 2
            column    = 1
          RECEIVING
            container = mo_mm_container ).

        mo_splitter->get_container(
          EXPORTING
            row       = 1
            column    = 1
          RECEIVING
            container = mo_mm_toolbar ).

        mo_splitter->set_row_height( id = 1 height = '3' ).
        mo_splitter->set_row_height( id = 2 height = '70' ).

        mo_splitter->set_row_sash( id    = 1
                                   type  = 0
                                   value = 0 ).

        CREATE OBJECT mo_toolbar EXPORTING parent = mo_mm_toolbar.
        add_toolbar_buttons( ).
        mo_toolbar->set_visible( 'X' ).
      ENDIF.
      CASE mv_type.
        WHEN 'CALLS'.
          steps_flow( ).
        WHEN 'FLOW'.
          magic_search( ).
      ENDCASE.

    ENDMETHOD.

    METHOD steps_flow.

      TYPES: BEGIN OF lty_entity,
               event TYPE string,
               name  TYPE string,
             END OF lty_entity,
             BEGIN OF t_ind,
               from TYPE i,
               to   TYPE i,
             END OF t_ind  .

      DATA: mm_string TYPE string,
            name      TYPE string,
            entities  TYPE TABLE OF lty_entity,
            entity    TYPE lty_entity,
            parts     TYPE TABLE OF string,
            step      LIKE LINE OF mo_viewer->mt_steps,
            ind       TYPE t_ind,
            indexes   TYPE TABLE OF t_ind.

      DATA(copy) = mo_viewer->mt_steps.

      LOOP AT copy ASSIGNING FIELD-SYMBOL(<copy>).
        IF <copy>-eventtype = 'METHOD'.
          READ TABLE mo_viewer->mo_window->ms_sources-tt_calls_line WITH KEY include = <copy>-include eventtype = 'METHOD' eventname = <copy>-eventname INTO DATA(call_line).
          <copy>-eventname = entity-name = |"{ call_line-class }->{ <copy>-eventname }"|.
          entity-event = <copy>-eventtype.

        ELSEIF <copy>-eventtype = 'FUNCTION'.
          <copy>-eventname = entity-name = |"{ <copy>-eventtype }:{ <copy>-eventname }"|.
        ELSEIF <copy>-eventtype = 'SCREEN'.
          <copy>-eventname = entity-name = |"CALL SCREEN { <copy>-eventname }"|.
        ELSEIF <copy>-eventtype = 'MODULE'.
          <copy>-eventname = entity-name = |"MODULE { <copy>-eventname }"|.
        ELSEIF <copy>-eventtype = 'FORM'.
          <copy>-eventname = entity-name = |"FORM { <copy>-eventname }"|.

        ELSE.
          <copy>-eventname = entity-name = |"{ <copy>-program }:{ <copy>-eventname }"|.
        ENDIF.

        COLLECT entity INTO entities.
      ENDLOOP.

      CLEAR step.

      IF i_direction IS INITIAL.
        mm_string = |graph TD\n |.
      ELSE.
        mm_string = |graph { i_direction }\n |.
      ENDIF.

      LOOP AT copy INTO DATA(step2).
        IF step IS INITIAL.
          step = step2.
          CONTINUE.
        ENDIF.
        IF step2-stacklevel > step-stacklevel.

          READ TABLE entities WITH KEY name = step-eventname TRANSPORTING NO FIELDS.
          ind-from = sy-tabix.
          READ TABLE entities WITH KEY name = step2-eventname TRANSPORTING NO FIELDS.
          ind-to = sy-tabix.
          READ TABLE indexes WITH KEY from = ind-from to = ind-to TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            "REPLACE ALL OCCURRENCES OF `-` IN step-eventname WITH `~` IN CHARACTER MODE.
            "REPLACE ALL OCCURRENCES OF `-` IN step2-eventname WITH `~` IN CHARACTER MODE.
            mm_string = |{  mm_string }{ ind-from }({ step-eventname }) --> { ind-to }({ step2-eventname })\n|.
            APPEND ind TO indexes.
          ENDIF.
        ENDIF.
        step = step2.
      ENDLOOP.
      mm_string = |{  mm_string }\n|.

      open_mermaid(  mm_string ).

    ENDMETHOD.

    METHOD magic_search.

      FIELD-SYMBOLS: <if> TYPE mo_viewer->ts_if.

      DATA: add         TYPE boolean,
            mm_string   TYPE string,
            sub         TYPE string,
            form        TYPE string,
            direction   TYPE string,
            box_s       TYPE string,
            box_e       TYPE string,
            ind2        TYPE i,
            start       TYPE i,
            end         TYPE i,
            bool        TYPE string,
            block_first TYPE i,
            els_before  TYPE i.

      DATA: line      TYPE mo_viewer->ts_line,
            pre_stack TYPE mo_viewer->ts_line,
            opened    TYPE i.

      DATA(lines) = mo_viewer->get_code_flow( ).

      "creating mermaid code
      CHECK lines IS NOT INITIAL.

      IF i_direction IS INITIAL.
        IF lines( lines ) < 100.
          direction = 'LR'.
        ELSE.
          direction = 'TB'.
        ENDIF.
      ELSE.
        direction = i_direction.
      ENDIF.

      mm_string = |graph {  direction }\n |.

      LOOP AT lines INTO line WHERE cond <> 'ELSE' AND cond <> 'ELSEIF' AND  cond <> 'WHEN'.
        DATA(ind) = sy-tabix.

        IF line-cond IS INITIAL.
          box_s = '('.
          box_e = ')'.
        ELSE.
          box_s = '{'.
          box_e = '}'.
        ENDIF.

        IF pre_stack IS INITIAL.
          pre_stack = line.
        ENDIF.

        IF ( pre_stack-stack > line-stack OR pre_stack-ev_name <> line-ev_name ) AND  opened > 0 AND  sub IS INITIAL.
          IF pre_stack-stack = line-stack AND pre_stack-ev_name <> line-ev_name.
            DATA(times) = 1.
          ELSE.
            times = pre_stack-stack - line-stack.
          ENDIF.

          DO  times TIMES.
            mm_string = |{  mm_string } end\n|.
            SUBTRACT 1 FROM  opened.
            IF  opened = 0.
              EXIT.
            ENDIF.
          ENDDO.

        ENDIF.
        DATA:  name TYPE string.
        IF    line-cond = 'LOOP' OR line-cond = 'DO' OR line-cond = 'WHILE' OR line-subname IS NOT INITIAL .

          "IF line-arrow IS NOT INITIAL.
          mm_string = |{  mm_string }{  ind }{  box_s }"{ line-code }"{  box_e }\n|.
          pre_stack = line.
          "ENDIF.

          IF strlen( line-code ) > 50.
            name = line-code+0(50).
          ELSE.
            name = line-code.
          ENDIF.
          REPLACE ALL OCCURRENCES OF `PERFORM` IN  name WITH `FORM` IN CHARACTER MODE.
          REPLACE ALL OCCURRENCES OF `CALL FUNCTION` IN  name WITH `FUNCTION` IN CHARACTER MODE.
          REPLACE ALL OCCURRENCES OF `CALL METHOD` IN  name WITH `METHOD` IN CHARACTER MODE.
          REPLACE ALL OCCURRENCES OF `-` IN  name WITH `~` IN CHARACTER MODE.
          REPLACE ALL OCCURRENCES OF ` ` IN  name WITH `&nbsp;` IN CHARACTER MODE.

          READ TABLE lines INDEX ind + 1 INTO DATA(line2).
          IF sy-subrc = 0 AND line-stack <> line2-stack.
            mm_string = |{  mm_string } subgraph S{  ind }["{  name }"]\n  direction {  direction }\n|.
            ADD 1 TO  opened.
            start =  ind.
            CONTINUE.
          ENDIF.

        ENDIF.

        IF line-cond = 'ENDLOOP' OR line-cond = 'ENDDO' OR line-cond = 'ENDWHILE'.
          SUBTRACT 1 FROM  opened.
          mm_string = |{  mm_string } end\n|.
          CONTINUE.
        ENDIF.

        mm_string = |{  mm_string }{  ind }{  box_s }"{ line-code }"{  box_e }\n|.
        pre_stack = line.

      ENDLOOP.

      DO  opened TIMES.
        mm_string = |{  mm_string } end\n|.
        SUBTRACT 1 FROM  opened.
      ENDDO.


      DATA: if_ind      TYPE i.
      CLEAR pre_stack.
      LOOP AT lines INTO line WHERE cond <> 'LOOP' AND cond <> 'DO' AND cond <> 'WHILE' AND cond <> 'ENDLOOP' AND cond <> 'ENDDO' AND cond <> 'ENDWHILE'.

        IF line-cond = 'IF' OR line-cond = 'CASE' .
          ADD 1 TO if_ind.
          READ TABLE mo_viewer->mt_if INDEX if_ind INTO mo_viewer->ms_if.
        ENDIF.


        IF pre_stack IS INITIAL.
          IF line-cond = 'WHEN' OR line-cond = 'ELSE' OR line-cond = 'ELSEIF'.
            IF <if> IS ASSIGNED.
              pre_stack = lines[ <if>-if_ind ].
            ELSE.
              CLEAR pre_stack.
            ENDIF.
          ELSE.
            pre_stack = line.

            IF line-arrow IS NOT INITIAL.
              sub = '|"' && line-arrow && '"|'.
            ELSE.
              CLEAR  sub.
            ENDIF.

            CONTINUE.
          ENDIF.

        ENDIF.

        IF line-cond = 'ELSE' OR line-cond = 'ELSEIF' OR line-cond = 'WHEN'.
          bool = '|' && line-code && '|'.
          IF line-els_after IS NOT INITIAL.
            mm_string = |{  mm_string }{ mo_viewer->ms_if-if_ind }-->{  bool }{ line-els_after }\n|.
            DATA(diff) = mo_viewer->ms_if-end_ind - line-els_after.
            DATA(last_els) = line-els_after.
*          IF line-cond <> 'WHEN' AND line-cond <> 'ELSEIF'  AND   diff > 1 AND line-els_after <> ms_if-end_ind.
*             mm_string = |{  mm_string }{  line-els_after }-->{ ms_if-end_ind }\n|.
*          ENDIF.
          ELSE.
            mm_string = |{  mm_string }{ mo_viewer->ms_if-if_ind }-->{  bool }{ mo_viewer->ms_if-end_ind }\n|.
          ENDIF.

          IF line-els_before IS NOT INITIAL AND line-els_before <> mo_viewer->ms_if-if_ind.
            mm_string = |{  mm_string }{ line-els_before }-->{ mo_viewer->ms_if-end_ind }\n|.
          ENDIF.

          IF lines[ line-ind + 1 ]-cond <> 'ENDIF' AND lines[ line-ind + 1 ]-cond <> 'ENDCASE'.
            CLEAR pre_stack.
          ENDIF.
          CONTINUE.
        ENDIF.

        IF   pre_stack-cond NE 'ELSE' AND pre_stack-cond NE 'ELSEIF' AND pre_stack-cond NE 'WHEN' AND NOT (  last_els = line-ind ).

          mm_string = |{  mm_string }{ pre_stack-ind }-->{  sub }{ line-ind }\n|.

          IF line-arrow IS NOT INITIAL.
            sub = '|"' && line-arrow && '"|'.
          ELSE.
            CLEAR  sub.
          ENDIF.

        ENDIF.

        pre_stack = line.

        IF line-cond = 'ENDIF' OR line-cond = 'ENDCASE' AND if_ind <> 0.
          DELETE mo_viewer->mt_if INDEX if_ind.
          SUBTRACT 1 FROM if_ind.
          READ TABLE mo_viewer->mt_if INDEX if_ind INTO mo_viewer->ms_if.
        ENDIF.

      ENDLOOP.
      mm_string = |{  mm_string }\n|.

      open_mermaid(  mm_string ).

    ENDMETHOD.

    METHOD add_toolbar_buttons.

      DATA: button TYPE ttb_button,
            events TYPE cntl_simple_events,
            event  LIKE LINE OF events.

      button  = VALUE #(
       ( function = 'TB' icon = CONV #( icon_view_expand_vertical ) quickinfo = 'Vertical' text = '' )
       ( function = 'LR' icon = CONV #( icon_view_expand_horizontal ) quickinfo = 'Horizontal' text = '' )
       ( butn_type = 3  )
       ( function = 'CALLS' icon = CONV #( icon_workflow_process ) quickinfo = 'Calls Flow' text = 'Calls Flow' )
       ( function = 'FLOW' icon = CONV #( icon_wizard ) quickinfo = 'Calculations flow sequence' text = 'Code Flow' )
       ( butn_type = 3  )
       ( function = 'TEXT' icon = CONV #( icon_wd_caption ) quickinfo = 'Mermaid Diagram text' text = '' )
                      ).

      mo_toolbar->add_button_group( button ).

*   Register events
      event-eventid = cl_gui_toolbar=>m_id_function_selected.
      event-appl_event = space.
      APPEND event TO events.

      mo_toolbar->set_registered_events( events = events ).
      SET HANDLER me->hnd_toolbar FOR mo_toolbar.

    ENDMETHOD.

    METHOD hnd_toolbar.

      IF fcode = 'TEXT'.
        DATA: mm_string TYPE string,
              ref       TYPE REF TO data.
        CALL METHOD mo_diagram->('GET_SOURCE_CODE_STRING') RECEIVING result = mm_string.
        GET REFERENCE OF  mm_string INTO  ref.
        NEW lcl_ace_text_viewer(  ref ).

        RETURN.
      ENDIF.

      IF fcode =  'LR' OR fcode =  'TB'.
        mv_direction = fcode.
      ELSE.
        mv_type = fcode.
      ENDIF.

      refresh( ).



    ENDMETHOD.

    METHOD open_mermaid.

      CHECK lcl_ace_appl=>i_mermaid_active = abap_true.

      TRY.
          IF mo_diagram IS INITIAL.
            CREATE OBJECT mo_diagram TYPE ('ZCL_WD_GUI_MERMAID_JS_DIAGRAM') EXPORTING parent = mo_mm_container hide_scrollbars = abap_false.
          ENDIF.
          CALL METHOD mo_diagram->('SET_SOURCE_CODE_STRING') EXPORTING source_code = i_mm_string.
          CALL METHOD mo_diagram->('DISPLAY').

        CATCH cx_root INTO DATA(error).
          MESSAGE error TYPE 'E'.
      ENDTRY.

    ENDMETHOD.

    METHOD refresh.

      CASE mv_type.
        WHEN 'CALLS'.
          steps_flow( mv_direction ).
        WHEN 'FLOW'.
          magic_search( mv_direction ).
      ENDCASE.

    ENDMETHOD.

  ENDCLASS.

  INITIALIZATION.

    "supressing F8 button
    DATA itab TYPE TABLE OF sy-ucomm.

    APPEND: 'ONLI' TO itab.
    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = sy-pfkey
      TABLES
        p_exclude = itab.

  AT SELECTION-SCREEN.

    SELECT COUNT( * ) FROM reposrc WHERE progname = p_prog.

    IF sy-dbcnt <> 0.
      DATA(gv_ace) = NEW lcl_ace( i_prog = p_prog i_dest = p_dest i_model = p_model i_apikey = p_apikey ).
    ELSE.
      MESSAGE 'Program is not found' TYPE 'E' DISPLAY LIKE 'I'.
    ENDIF.
