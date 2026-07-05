CLASS zcl_smd_ai_agent DEFINITION
  PUBLIC
  CREATE PRIVATE .


  PUBLIC SECTION.
    CONSTANTS c_provider TYPE string VALUE 'MISTRAL'.
    CONSTANTS c_keyname  TYPE string VALUE 'DEFAULT'.
    CONSTANTS c_model    TYPE text255 VALUE 'mistral-large-latest'.

    METHODS constructor
      IMPORTING
        !io_debugger TYPE REF TO zcl_smd_debugger_base.

    METHODS run
      IMPORTING
        !i_task        TYPE string
      EXPORTING
        !es_action     TYPE zif_smd_ai_agent_types=>ty_action
        !ev_text       TYPE string.

    METHODS execute_action
      IMPORTING
        !is_action     TYPE zif_smd_ai_agent_types=>ty_action
      RETURNING
        VALUE(rv_text) TYPE string.

    CLASS-METHODS create
      IMPORTING
        !io_debugger  TYPE REF TO zcl_smd_debugger_base
      RETURNING
        VALUE(ro_agent) TYPE REF TO zcl_smd_ai_agent.
  PROTECTED SECTION.
  PRIVATE SECTION.

TYPES tt_string TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    DATA mo_debugger TYPE REF TO zcl_smd_debugger_base.
    DATA mv_last_error TYPE string.
    DATA mv_last_tool_result TYPE string.

    CLASS-DATA gv_cached_password TYPE string.

    METHODS get_default_api_key
      RETURNING
        VALUE(rv_api_key) TYPE string.

    METHODS build_prompt
      IMPORTING
        !i_task          TYPE string
      RETURNING
        VALUE(rv_prompt) TYPE string.

    METHODS get_system_prompt
      RETURNING
        VALUE(rv_prompt) TYPE string.

    METHODS get_tools_json
      RETURNING
        VALUE(rv_json) TYPE string.

    METHODS get_plugin_tools_json
      RETURNING
        VALUE(rv_json) TYPE string.

    METHODS parse_tool_call
      IMPORTING
        !i_name          TYPE string
        !i_arguments     TYPE string
      RETURNING
        VALUE(rs_action) TYPE zif_smd_ai_agent_types=>ty_action.

    METHODS ensure_guard_breakpoint
      RETURNING
        VALUE(rv_text) TYPE string.

    METHODS execute_plugin_tool
      IMPORTING
        !is_action     TYPE zif_smd_ai_agent_types=>ty_action
      RETURNING
        VALUE(rv_text) TYPE string.

    METHODS read_variable
      IMPORTING
        !i_name        TYPE string
      RETURNING
        VALUE(rv_text) TYPE string.

    METHODS append_line
      IMPORTING
        !i_line TYPE string
      CHANGING
        !ct_lines TYPE tt_string.

    METHODS ask_password
      RETURNING
        VALUE(rv_password) TYPE string.
ENDCLASS.



CLASS ZCL_SMD_AI_AGENT IMPLEMENTATION.


  method APPEND_LINE.
    APPEND i_line TO ct_lines.
  endmethod.


METHOD build_prompt.

    DATA lt_lines TYPE tt_string.
    DATA lv_line TYPE string.
    DATA lv_newline TYPE string.

    lv_newline = cl_abap_char_utilities=>newline.

    append_line( EXPORTING i_line = |Task: { i_task }| CHANGING ct_lines = lt_lines ).
    append_line( EXPORTING i_line = `` CHANGING ct_lines = lt_lines ).

    IF mo_debugger IS BOUND.
      append_line(
        EXPORTING
          i_line = |Current step: { mo_debugger->m_step }, history step: { mo_debugger->m_hist_step }|
        CHANGING
          ct_lines = lt_lines ).

      lv_line = |Current stack: program={ mo_debugger->ms_stack-program } |.
      lv_line = lv_line && |include={ mo_debugger->ms_stack-include } |.
      lv_line = lv_line && |line={ mo_debugger->ms_stack-line } |.
      lv_line = lv_line && |event={ mo_debugger->ms_stack-eventtype } { mo_debugger->ms_stack-eventname }|.
      append_line( EXPORTING i_line = lv_line CHANGING ct_lines = lt_lines ).

      IF mv_last_tool_result IS NOT INITIAL.
        append_line( EXPORTING i_line = `` CHANGING ct_lines = lt_lines ).
        append_line( EXPORTING i_line = |Last confirmed tool result: { mv_last_tool_result }| CHANGING ct_lines = lt_lines ).
      ENDIF.

      IF mo_debugger->mo_window IS BOUND.
        lv_line = |Screen program: program={ mo_debugger->mo_window->m_prg-program } |.
        lv_line = lv_line && |include={ mo_debugger->mo_window->m_prg-include } |.
        lv_line = lv_line && |line={ mo_debugger->mo_window->m_prg-line }|.
        append_line( EXPORTING i_line = lv_line CHANGING ct_lines = lt_lines ).
      ENDIF.

      append_line( EXPORTING i_line = `` CHANGING ct_lines = lt_lines ).
      append_line( EXPORTING i_line = `Recent debugger steps:` CHANGING ct_lines = lt_lines ).

      DATA(lv_from_step) = mo_debugger->m_step - 50.
      IF lv_from_step < 0.
        lv_from_step = 0.
      ENDIF.

      LOOP AT mo_debugger->mt_steps INTO DATA(ls_step) WHERE step >= lv_from_step.
        append_line(
          EXPORTING
            i_line = |#{ ls_step-step } stack={ ls_step-stacklevel } { ls_step-program }/{ ls_step-include }:{ ls_step-line } { ls_step-eventtype } { ls_step-eventname }|
          CHANGING
            ct_lines = lt_lines ).
      ENDLOOP.

      append_line( EXPORTING i_line = `` CHANGING ct_lines = lt_lines ).
      append_line( EXPORTING i_line = `Current variables/state:` CHANGING ct_lines = lt_lines ).

      DATA lv_state_count TYPE i.
      LOOP AT mo_debugger->mt_state INTO DATA(ls_state).
        ADD 1 TO lv_state_count.
        IF lv_state_count > 120.
          append_line( EXPORTING i_line = `... state truncated ...` CHANGING ct_lines = lt_lines ).
          EXIT.
        ENDIF.

        append_line(
          EXPORTING
            i_line = |{ ls_state-leaf } { ls_state-name } = { ls_state-short } type={ ls_state-type } path={ ls_state-path }|
          CHANGING
            ct_lines = lt_lines ).
      ENDLOOP.

      append_line( EXPORTING i_line = `` CHANGING ct_lines = lt_lines ).
      append_line( EXPORTING i_line = `Variable history:` CHANGING ct_lines = lt_lines ).

      DATA lv_hist_count TYPE i.
      LOOP AT mo_debugger->mt_vars_hist INTO DATA(ls_hist).
        ADD 1 TO lv_hist_count.
        IF lv_hist_count > 120.
          append_line( EXPORTING i_line = `... variable history truncated ...` CHANGING ct_lines = lt_lines ).
          EXIT.
        ENDIF.

        append_line(
          EXPORTING
            i_line = |step={ ls_hist-step } stack={ ls_hist-stack } { ls_hist-leaf } { ls_hist-name } = { ls_hist-short } type={ ls_hist-type } path={ ls_hist-path }|
          CHANGING
            ct_lines = lt_lines ).
      ENDLOOP.

      IF mo_debugger->mt_selected_var IS NOT INITIAL.
        append_line( EXPORTING i_line = `` CHANGING ct_lines = lt_lines ).
        append_line( EXPORTING i_line = `Selected variables:` CHANGING ct_lines = lt_lines ).
        LOOP AT mo_debugger->mt_selected_var INTO DATA(ls_selected).
          append_line( EXPORTING i_line = ls_selected-name CHANGING ct_lines = lt_lines ).
        ENDLOOP.
      ENDIF.
    ENDIF.

    CONCATENATE LINES OF lt_lines INTO rv_prompt SEPARATED BY lv_newline.

  ENDMETHOD.


  method CONSTRUCTOR.
      mo_debugger = io_debugger.
  endmethod.


METHOD ensure_guard_breakpoint.

    CHECK mo_debugger IS BOUND.
    CHECK mo_debugger->mo_window IS BOUND.

    DATA(lv_include) = mo_debugger->mo_window->m_prg-include.
    DATA(lv_program) = mo_debugger->mo_window->m_prg-program.

    IF lv_include IS INITIAL.
      RETURN.
    ENDIF.

    IF lv_program IS INITIAL.
      lv_program = lv_include.
    ENDIF.

    READ TABLE mo_debugger->mo_window->mt_source WITH KEY include = lv_include INTO DATA(ls_source).
    IF sy-subrc <> 0 OR ls_source-source IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_line) = lines( ls_source-source->lines ).
    IF lv_line <= 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'RS_SET_BREAKPOINT'
      EXPORTING
        index       = lv_line
        program     = lv_include
        mainprogram = lv_program
        bp_type     = 'S'
      EXCEPTIONS
        not_executed = 1
        OTHERS       = 2.

    rv_text = COND string(
      WHEN sy-subrc = 0 THEN |Guard breakpoint: { lv_include }:{ lv_line }|
      ELSE |Guard breakpoint was not set: { lv_include }:{ lv_line }| ).

  ENDMETHOD.


METHOD execute_action.

    CASE is_action-tool.
      WHEN 'read_variable'.
        rv_text = read_variable( is_action-variable ).

      WHEN 'step_debugger'.
        rv_text = |Confirmed debugger step { is_action-command }. Intent: { is_action-reason }|.

      WHEN OTHERS.
        rv_text = execute_plugin_tool( is_action ).
    ENDCASE.

    mv_last_tool_result = rv_text.

  ENDMETHOD.


METHOD execute_plugin_tool.

    TRY.
        DATA(lo_tool) = zcl_ai_tool_factory=>get_tool( is_action-tool ).
        IF lo_tool IS NOT BOUND AND is_action-tool = 'set_breakpoint'.
          CREATE OBJECT lo_tool TYPE zcl_smd_aitool_breakpoint.
        ENDIF.

        IF lo_tool IS NOT BOUND.
          rv_text = |Unknown AI action: { is_action-tool }|.
          RETURN.
        ENDIF.

        DATA(ls_result) = lo_tool->execute( i_arguments = is_action-arguments ).
        IF ls_result-error_text IS NOT INITIAL.
          rv_text = ls_result-error_text.
        ELSE.
          rv_text = ls_result-xml_payload.
        ENDIF.
      CATCH cx_root INTO DATA(lx_root).
        rv_text = |AI action { is_action-tool } failed: { lx_root->get_text( ) }|.
    ENDTRY.

  ENDMETHOD.


  METHOD get_default_api_key.

    " TEMP: hardcoded password while the popup/keyname flow is being finalized.
    CONSTANTS c_temp_password TYPE string VALUE 'Developer001'.

    CLEAR: rv_api_key, mv_last_error.

    " 1) Try a key stored for the current user
    SELECT SINGLE username, secret
      FROM zaicode_apikey
      INTO @DATA(ls_key)
      WHERE username = @sy-uname
        AND provider = @c_provider
        AND keyname  = @c_keyname.

    IF sy-subrc <> 0.
      " 2) Fall back to any shared/default key for this provider, regardless of username
      SELECT SINGLE username, secret
        FROM zaicode_apikey
        INTO @ls_key
        WHERE provider = @c_provider
          AND keyname  = @c_keyname.
    ENDIF.

    IF sy-subrc <> 0 OR ls_key-secret IS INITIAL.
      mv_last_error = |No stored key for { c_provider } / { c_keyname } in ZAICODE_APIKEY|.
      RETURN.
    ENDIF.

    " Try with the cached/blank password first (works for keys without a password)
    TRY.
        rv_api_key = zcl_aicode_crypto=>decrypt(
          i_username = ls_key-username
          i_provider = c_provider
          i_name     = c_keyname
          i_password = gv_cached_password
          i_secret   = ls_key-secret ).
        RETURN.
      CATCH cx_sec_sxml_encrypt_error.
        CLEAR rv_api_key.
    ENDTRY.

    " TEMP: try the hardcoded password before bothering the user with a popup
    TRY.
        rv_api_key = zcl_aicode_crypto=>decrypt(
          i_username = ls_key-username
          i_provider = c_provider
          i_name     = c_keyname
          i_password = c_temp_password
          i_secret   = ls_key-secret ).
        gv_cached_password = c_temp_password.
        RETURN.
      CATCH cx_sec_sxml_encrypt_error.
        CLEAR rv_api_key.
    ENDTRY.

    " Ask for the password once per session and retry
    DATA(lv_password) = ask_password( ).
    IF lv_password IS INITIAL.
      mv_last_error = |Password required to decrypt { c_provider } / { c_keyname }.|.
      RETURN.
    ENDIF.

    TRY.
        rv_api_key = zcl_aicode_crypto=>decrypt(
          i_username = ls_key-username
          i_provider = c_provider
          i_name     = c_keyname
          i_password = lv_password
          i_secret   = ls_key-secret ).
        gv_cached_password = lv_password.
      CATCH cx_sec_sxml_encrypt_error.
        mv_last_error = |Cannot decrypt { c_provider } / { c_keyname }. Wrong password?|.
    ENDTRY.

  ENDMETHOD.


METHOD get_plugin_tools_json.

    rv_json = `[]`.

    TRY.
        DATA lt_tool_names TYPE string_table.
        APPEND 'set_breakpoint' TO lt_tool_names.

        rv_json = zcl_ai_tool_factory=>build_tools_json( it_tool_names = lt_tool_names ).
      CATCH cx_root.
        rv_json = `[]`.
    ENDTRY.

    IF strlen( rv_json ) <= 2.
      TRY.
          DATA lo_tool TYPE REF TO zif_ai_tool.
          CREATE OBJECT lo_tool TYPE zcl_smd_aitool_breakpoint.
          DATA(lv_schema) = lo_tool->get_schema( ).
          IF lv_schema IS NOT INITIAL.
            rv_json = `[` && lv_schema && `]`.
          ENDIF.
        CATCH cx_root.
          rv_json = `[]`.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


METHOD get_system_prompt.

    rv_prompt =
      'You are the Smart Debugger AI agent for ABAP. ' &&
      'Use only the debugger snapshot provided by the user prompt. ' &&
      'Find likely bugs, suspicious state transitions, wrong variable values, and useful next debug actions. ' &&
      'When you need debugger control, call exactly one tool. ' &&
      'Use read_variable when the exact runtime value of any ABAP variable, field, component, reference, or table expression is needed. ' &&
      'Use set_breakpoint with real TPDA include and 1-based source line numbers when stopping at a specific source line is useful; it still requires user confirmation before execution. ' &&
      'Never call F8/continue unless you explain why it is safe; prefer F5/F6/F7 for investigation. ' &&
      'Before continuing, assume a guard breakpoint exists at the end of the current include. ' &&
      'Do not reveal hidden chain-of-thought; write a concise analysis summary instead. ' &&
      'Answer in Russian unless the user asks otherwise. Structure the answer as: Intent, Analysis, Findings, Proposed action.'.

  ENDMETHOD.


METHOD get_tools_json.

    DATA(lv_json) =
      `[` &&
      `{ "type":"function", "function": {` &&
      `"name":"step_debugger",` &&
      `"description":"Request one debugger step.",` &&
      `"parameters": { "type":"object", "properties": {` &&
      `"command": { "type":"string", "enum":["F5","F6","F7","F8"],` &&
      `"description":"F5 into, F6 over, F7 out, F8 continue" },` &&
      `"reason": { "type":"string",` &&
      `"description":"Visible intent shown before confirmation" } },` &&
      `"required":["command","reason"],` &&
      `"additionalProperties":false } } },` &&
      `{ "type":"function", "function": {` &&
      `"name":"read_variable",` &&
      `"description":"Read a value from the ABAP debugger context.",` &&
      `"parameters": { "type":"object", "properties": {` &&
      `"variable": { "type":"string",` &&
      `"description":"ABAP debugger expression or variable name" },` &&
      `"reason": { "type":"string",` &&
      `"description":"Visible intent shown before confirmation" } },` &&
      `"required":["variable","reason"],` &&
      `"additionalProperties":false } } }`.

    DATA(lv_plugin_json) = get_plugin_tools_json( ).
    IF strlen( lv_plugin_json ) > 2.
      DATA(lv_len) = strlen( lv_plugin_json ) - 2.
      DATA(lv_plugin_body) = lv_plugin_json+1(lv_len).
      IF lv_plugin_body IS NOT INITIAL.
        lv_json = lv_json && `,` && lv_plugin_body.
      ENDIF.
    ENDIF.

    rv_json = lv_json && `]`.

  ENDMETHOD.


METHOD parse_tool_call.

    TYPES:
      BEGIN OF ty_step_args,
        command TYPE string,
        reason  TYPE string,
      END OF ty_step_args,
      BEGIN OF ty_break_args,
        program TYPE string,
        include TYPE string,
        line    TYPE i,
        reason  TYPE string,
      END OF ty_break_args,
      BEGIN OF ty_read_args,
        variable TYPE string,
        reason   TYPE string,
      END OF ty_read_args.

    rs_action-tool = i_name.
    rs_action-arguments = i_arguments.

    TRY.
        IF i_name = 'step_debugger'.
          DATA ls_step TYPE ty_step_args.
          /ui2/cl_json=>deserialize( EXPORTING json = i_arguments CHANGING data = ls_step ).
          rs_action-command = ls_step-command.
          rs_action-reason = ls_step-reason.

        ELSEIF i_name = 'set_breakpoint'.
          DATA ls_break TYPE ty_break_args.
          /ui2/cl_json=>deserialize( EXPORTING json = i_arguments CHANGING data = ls_break ).
          MOVE-CORRESPONDING ls_break TO rs_action.

        ELSEIF i_name = 'read_variable'.
          DATA ls_read TYPE ty_read_args.
          /ui2/cl_json=>deserialize( EXPORTING json = i_arguments CHANGING data = ls_read ).
          MOVE-CORRESPONDING ls_read TO rs_action.
        ENDIF.
      CATCH cx_root INTO DATA(lx_root).
        rs_action-reason = |Cannot parse tool call arguments: { lx_root->get_text( ) }|.
    ENDTRY.

  ENDMETHOD.


METHOD read_variable.

    DATA lr_simple TYPE REF TO tpda_sys_symbsimple.
    DATA lr_string TYPE REF TO tpda_sys_symbstring.

    FIELD-SYMBOLS: <quickdata> TYPE any,
                   <simple>    TYPE tpda_sys_symbsimple,
                   <string>    TYPE tpda_sys_symbstring.

    IF i_name IS INITIAL.
      rv_text = 'read_variable: variable is empty'.
      RETURN.
    ENDIF.

    IF mo_debugger IS BOUND.
      READ TABLE mo_debugger->mt_state INTO DATA(ls_state) WITH KEY name = i_name.
      IF sy-subrc <> 0.
        READ TABLE mo_debugger->mt_state INTO ls_state WITH KEY path = i_name.
      ENDIF.

      IF sy-subrc = 0.
        rv_text = |read_variable { i_name }: type={ ls_state-type }, value={ ls_state-short }, path={ ls_state-path }|.
        RETURN.
      ENDIF.
    ENDIF.

    TRY.
        DATA(ls_quick) = cl_tpda_script_data_descr=>get_quick_info( i_name ).
        ASSIGN ls_quick-quickdata TO <quickdata>.

        IF <quickdata> IS ASSIGNED.
          TRY.
              lr_string ?= <quickdata>.
              ASSIGN lr_string->* TO <string>.
              IF <string> IS ASSIGNED.
                rv_text = |read_variable { i_name }: type={ ls_quick-abstypename }, typid={ ls_quick-typid }, value={ <string>-valstring }|.
                RETURN.
              ENDIF.
            CATCH cx_sy_move_cast_error.
          ENDTRY.

          TRY.
              lr_simple ?= <quickdata>.
              ASSIGN lr_simple->* TO <simple>.
              IF <simple> IS ASSIGNED.
                rv_text = |read_variable { i_name }: type={ ls_quick-abstypename }, typid={ ls_quick-typid }, value={ <simple>-valstring }|.
                RETURN.
              ENDIF.
            CATCH cx_sy_move_cast_error.
          ENDTRY.

        ENDIF.

        rv_text = |read_variable { i_name }: type={ ls_quick-abstypename }, typid={ ls_quick-typid }, value is not directly printable|.

      CATCH cx_root INTO DATA(lx_root).
        rv_text = |read_variable { i_name } failed: { lx_root->get_text( ) }|.
    ENDTRY.

  ENDMETHOD.


METHOD run.

    CLEAR es_action.
    DATA(lv_api_key) = get_default_api_key( ).
    IF lv_api_key IS INITIAL.
      ev_text = |AI agent cannot start: { mv_last_error }|.
      RETURN.
    ENDIF.

    TRY.
        DATA(lv_guard) = ensure_guard_breakpoint( ).

        DATA(lo_llm) = NEW zcl_abapai_llm_client(
          i_model    = c_model
          i_apikey   = lv_api_key
          i_provider = c_provider ).

        lo_llm->set_temperature( '0.1' ).
        lo_llm->set_max_tokens( 4000 ).

        DATA(lo_context) = NEW zcl_ai_tool_context(
          io_llm        = lo_llm
          i_agents_path = `` ).

        zcl_ai_tool_factory=>initialize( lo_context ).

        DATA lt_tool_calls TYPE zcl_code_ai_api=>tt_tool_calls.

        DATA(lv_answer) = lo_llm->ask_with_tools(
          EXPORTING
            i_prompt        = build_prompt( i_task )
            i_system_prompt = get_system_prompt( )
            i_tools_json    = get_tools_json( )
          IMPORTING
            et_tool_calls   = lt_tool_calls ).

        READ TABLE lt_tool_calls INDEX 1 INTO DATA(ls_call).
        IF sy-subrc = 0.
          es_action = parse_tool_call(
            i_name      = ls_call-name
            i_arguments = ls_call-arguments ).
        ENDIF.

        ev_text =
          |Provider: { c_provider } / { c_model }| &&
          cl_abap_char_utilities=>newline &&
          |Time: { lo_llm->get_last_seconds( ) } sec, tokens in/out: { lo_llm->mv_last_tok_in }/{ lo_llm->mv_last_tok_out }| &&
          cl_abap_char_utilities=>newline &&
          lv_guard &&
          cl_abap_char_utilities=>newline &&
          cl_abap_char_utilities=>newline &&
          lv_answer.

        IF es_action-tool IS NOT INITIAL.
          ev_text = ev_text &&
            cl_abap_char_utilities=>newline &&
            cl_abap_char_utilities=>newline &&
            |Pending AI action: { es_action-tool } { es_action-command } { es_action-variable } { es_action-include }:{ es_action-line }| &&
            cl_abap_char_utilities=>newline &&
            |Intent: { es_action-reason }| &&
            cl_abap_char_utilities=>newline &&
            |Press AI again to confirm this action.|.
        ENDIF.

      CATCH cx_root INTO DATA(lx_root).
        ev_text = |AI agent failed: { lx_root->get_text( ) }|.
    ENDTRY.

  ENDMETHOD.
  METHOD create.
    ro_agent = NEW zcl_smd_ai_agent( io_debugger = io_debugger ).
  ENDMETHOD.
  METHOD ask_password.

    DATA lv_answer   TYPE c.
    DATA lv_valueout TYPE zaicode_apikey-secret.

    CALL FUNCTION 'POPUP_TO_GET_VALUE'
      EXPORTING
        fieldname            = 'SECRET'
        tabname               = 'ZAICODE_APIKEY'
        titel                 = 'Enter password for the stored AI API key'
        valuein               = ''
      IMPORTING
        answer                = lv_answer
        valueout              = lv_valueout
      EXCEPTIONS
        fieldname_not_found   = 1
        OTHERS                = 2.

    IF sy-subrc <> 0 OR lv_answer = 'C'.
      CLEAR rv_password.
      RETURN.
    ENDIF.

    rv_password = lv_valueout.

  ENDMETHOD.
ENDCLASS.
