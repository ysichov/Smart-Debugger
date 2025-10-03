class ZCL_AI_API definition
  public
  final
  create public .

public section.

  data IV_PROMPT type I .

  methods CALL_OPENAI
    importing
      !IV_PROMPT type STRING
      !IV_TEMP type STRING optional .
  methods CONSTRUCTOR
    importing
      !IV_DEST type TEXT255
      !IV_MODEL type TEXT255
      !IV_APIKEY type TEXT255 .
protected section.
private section.

  data MV_API_KEY type STRING .
  data MV_DEST type TEXT255 .
  data MV_MODEL type STRING .

  methods BUILD_REQUEST
    importing
      !IV_PROMPT type STRING
    exporting
      !EV_PAYLOAD type STRING .
  methods SEND_REQUEST
    importing
      !IV_PAYLOAD type STRING
    exporting
      !EV_RESPONSE type STRING .
  methods OUTPUT
    importing
      !IV_PROMPT type STRING
      !IV_CONTENT type STRING .
ENDCLASS.



CLASS ZCL_AI_API IMPLEMENTATION.


  method BUILD_REQUEST.

    data: lv_payload type string.

    lv_payload = |{ '{ "model": "' && mv_model && '", "messages": [{ "role": "user", "content": "' && iv_prompt &&  '" }], "max_tokens": 1000 } ' }|.

    ev_payload = lv_payload.

  endmethod.


  method CALL_OPENAI.

    DATA: lv_prompt type string,
          lv_payload type string,
          lv_response type string.

    "Build payload
    build_request(
      EXPORTING
        iv_prompt  = iv_prompt
      IMPORTING
        ev_payload = lv_payload ).

    CALL METHOD me->send_request
      EXPORTING
        iv_payload  = lv_payload
      IMPORTING
        ev_response = lv_response.

    CALL METHOD me->output
      EXPORTING
        iv_prompt = iv_prompt
        iv_content = lv_response.

  endmethod.


  METHOD output.
    DATA: lv_text(1000) TYPE c,
          lv_string     TYPE string,
          lv_content    TYPE string,
          lv_reasoning  TYPE string.

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

    DATA ls_response TYPE lty_s_base_chatgpt_res.

    DATA: lv_binary TYPE xstring.

    DATA: lo_x2c TYPE REF TO cl_abap_conv_in_ce.
    lo_x2c = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
    lv_binary = iv_content.
    lo_x2c->convert( EXPORTING input = lv_binary
                     IMPORTING data  = lv_string ).

    /ui2/cl_json=>deserialize( EXPORTING json = lv_string CHANGING data = ls_response ).


    IF  ls_response-choices IS NOT INITIAL.
      lv_content = ls_response-choices[ 1 ]-message-content.
      lv_reasoning = ls_response-choices[ 1 ]-message-reasoning_content.
    ELSE.
      lv_content = lv_string.
      cl_abap_browser=>show_html( html_string = lv_content title = 'Error (' ).
      RETURN.
    ENDIF.

    cl_demo_output=>display(
     | PROMPT: { iv_prompt } { cl_abap_char_utilities=>cr_lf  } CONTENT: { lv_content } { cl_abap_char_utilities=>cr_lf } reasoning: { lv_reasoning } | ).
  ENDMETHOD.


  METHOD send_request.

    DATA: lo_http_client   TYPE REF TO if_http_client,
          lv_response_body TYPE string,
          lv_header        TYPE string.

    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING
        destination                = mv_dest
      IMPORTING
        client                     = lo_http_client
      EXCEPTIONS
        argument_not_found         = 1
        destination_not_found      = 2
        destination_no_authority   = 3
        plugin_not_active          = 4
        internal_error             = 5
        oa2c_set_token_error       = 6
        oa2c_missing_authorization = 7
        oa2c_invalid_config        = 8
        oa2c_invalid_parameters    = 9
        oa2c_invalid_scope         = 10
        oa2c_invalid_grant         = 11
        oa2c_secstore_adm          = 12
        OTHERS                     = 13.
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ENDIF.

MV_API_KEY = mv_api_key.

    "set request header
    lo_http_client->request->set_header_field( name = 'Content-Type' value = 'application/json' ).
    lo_http_client->request->set_header_field( name = 'Authorization' value = |Bearer { mv_api_key }| ).
    lo_http_client->request->set_method('POST').

    "set payload
    lo_http_client->request->set_cdata( iv_payload ).

    CALL METHOD lo_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.
    IF sy-subrc = 0.
      CALL METHOD lo_http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4.
      "Get response
      IF sy-subrc <> 0.
        lv_response_body = lo_http_client->response->get_data( ).
        ev_response = lv_response_body.
      ELSE.
        lv_response_body = lo_http_client->response->get_data( ).
        IF lv_response_body is not INITIAL.
          ev_response = lv_response_body.
        ELSE.
          ev_response = 'Call was succeesful, but got no response'.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    mv_dest = iv_dest.
    mv_model = iv_model.
    mv_api_key = iv_apikey.

  ENDMETHOD.
ENDCLASS.
