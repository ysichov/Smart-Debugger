CLASS zcl_smd_ai_agent DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS c_provider TYPE string VALUE 'MISTRAL'.
    CONSTANTS c_keyname  TYPE string VALUE 'Default'.
    CONSTANTS c_model    TYPE text255 VALUE 'mistral-large-latest'.

    TYPES:
      BEGIN OF ty_action,
        tool    TYPE string,
        command TYPE string,
        program TYPE string,
        include TYPE string,
        line    TYPE i,
        variable TYPE string,
        reason  TYPE string,
      END OF ty_action.

    METHODS constructor
      IMPORTING
        !io_debugger TYPE REF TO zcl_smd_debugger_base.

    METHODS run
      IMPORTING
        !i_task        TYPE string
      EXPORTING
        !es_action     TYPE ty_action
      RETURNING
        VALUE(rv_text) TYPE string.

    METHODS execute_action
      IMPORTING
        !is_action     TYPE ty_action
      RETURNING
        VALUE(rv_text) TYPE string.

  PRIVATE SECTION.
    TYPES tt_string TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    DATA mo_debugger TYPE REF TO zcl_smd_debugger_base.
    DATA mv_last_error TYPE string.
    DATA mv_last_tool_result TYPE string.

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

    METHODS parse_tool_call
      IMPORTING
        !is_call         TYPE zcl_code_ai_api=>ty_tool_call
      RETURNING
        VALUE(rs_action) TYPE ty_action.

    METHODS ensure_guard_breakpoint
      RETURNING
        VALUE(rv_text) TYPE string.

    METHODS set_breakpoint
      IMPORTING
        !is_action     TYPE ty_action
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

ENDCLASS.

CLASS zcl_smd_ai_agent IMPLEMENTATION.

  METHOD constructor.
    mo_debugger = io_debugger.
  ENDMETHOD.

  METHOD run.

    CLEAR es_action.
    DATA(lv_api_key) = get_default_api_key( ).
    IF lv_api_key IS INITIAL.
      rv_text = |AI agent cannot start: { mv_last_error }|.
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
          es_action = parse_tool_call( ls_call ).
        ENDIF.

        rv_text =
          |Provider: { c_provider } / { c_model }| &&
          cl_abap_char_utilities=>newline &&
          |Time: { lo_llm->get_last_seconds( ) } sec, tokens in/out: { lo_llm->mv_last_tok_in }/{ lo_llm->mv_last_tok_out }| &&
          cl_abap_char_utilities=>newline &&
          lv_guard &&
          cl_abap_char_utilities=>newline &&
          cl_abap_char_utilities=>newline &&
          lv_answer.

        IF es_action-tool IS NOT INITIAL.
          rv_text = rv_text &&
            cl_abap_char_utilities=>newline &&
            cl_abap_char_utilities=>newline &&
            |Pending AI action: { es_action-tool } { es_action-command } { es_action-variable } { es_action-include }:{ es_action-line }| &&
            cl_abap_char_utilities=>newline &&
            |Intent: { es_action-reason }| &&
            cl_abap_char_utilities=>newline &&
            |Press AI again to confirm this action.|.
        ENDIF.

      CATCH cx_root INTO DATA(lx_root).
        rv_text = |AI agent failed: { lx_root->get_text( ) }|.
    ENDTRY.

  ENDMETHOD.

  METHOD execute_action.

    CASE is_action-tool.
      WHEN 'set_breakpoint'.
        rv_text = set_breakpoint( is_action ).

      WHEN 'read_variable'.
        rv_text = read_variable( is_action-variable ).

      WHEN 'step_debugger'.
        rv_text = |Confirmed debugger step { is_action-command }. Intent: { is_action-reason }|.

      WHEN OTHERS.
        rv_text = |Unknown AI action: { is_action-tool }|.
    ENDCASE.

    mv_last_tool_result = rv_text.

  ENDMETHOD.

  METHOD get_default_api_key.

    CLEAR: rv_api_key, mv_last_error.

    SELECT SINGLE secret
      FROM zaicode_apikey
      INTO @DATA(lv_secret)
      WHERE username = @sy-uname
        AND provider = @c_provider
        AND keyname  = @c_keyname.

    IF sy-subrc <> 0 OR lv_secret IS INITIAL.
      mv_last_error = |No stored key for { c_provider } / { c_keyname } in ZAICODE_APIKEY|.
      RETURN.
    ENDIF.

    TRY.
        rv_api_key = zcl_aicode_crypto=>decrypt(
          i_username = sy-uname
          i_provider = c_provider
          i_name     = c_keyname
          i_password = ''
          i_secret   = lv_secret ).
      CATCH cx_sec_sxml_encrypt_error.
        mv_last_error = |Cannot decrypt { c_provider } / { c_keyname }. The key probably has a password.|.
    ENDTRY.

  ENDMETHOD.

  METHOD build_prompt.

    DATA lt_lines TYPE tt_string.
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

      append_line(
        EXPORTING
          i_line = |Current stack: program={ mo_debugger->ms_stack-program } include={ mo_debugger->ms_stack-include } line={ mo_debugger->ms_stack-line } event={ mo_debugger->ms_stack-eventtype } { mo_debugger->ms_stack-eventname }|
        CHANGING
          ct_lines = lt_lines ).

      IF mv_last_tool_result IS NOT INITIAL.
        append_line( EXPORTING i_line = `` CHANGING ct_lines = lt_lines ).
        append_line( EXPORTING i_line = |Last confirmed tool result: { mv_last_tool_result }| CHANGING ct_lines = lt_lines ).
      ENDIF.

      IF mo_debugger->mo_window IS BOUND.
        append_line(
          EXPORTING
            i_line = |Screen program: program={ mo_debugger->mo_window->m_prg-program } include={ mo_debugger->mo_window->m_prg-include } line={ mo_debugger->mo_window->m_prg-line }|
          CHANGING
            ct_lines = lt_lines ).
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

  METHOD get_system_prompt.

    rv_prompt =
      `You are the Smart Debugger AI agent for ABAP. ` &&
      `Use only the debugger snapshot provided by the user prompt. ` &&
      `Find likely bugs, suspicious state transitions, wrong variable values, and useful next debug actions. ` &&
      `When you need debugger control, call exactly one tool. ` &&
      `Use read_variable when the exact runtime value of any ABAP variable, field, component, reference, or table expression is needed. ` &&
      `Never call F8/continue unless you explain why it is safe; prefer F5/F6/F7 for investigation. ` &&
      `Before continuing, assume a guard breakpoint exists at the end of the current include. ` &&
      `Do not reveal hidden chain-of-thought; write a concise analysis summary instead. ` &&
      `Answer in Russian unless the user asks otherwise. Structure the answer as: Intent, Analysis, Findings, Proposed action.`;

  ENDMETHOD.

  METHOD get_tools_json.

    rv_json =
      `[` &&
      `{ "type":"function", "function": { "name":"step_debugger", "description":"Request one debugger step. The user must confirm before execution.", "parameters": { "type":"object", "properties": { "command": { "type":"string", "enum":["F5","F6","F7","F8"], "description":"F5 step into, F6 step over, F7 step out, F8 continue to breakpoint" }, "reason": { "type":"string", "description":"Visible intent shown to the user before confirmation" } }, "required":["command","reason"], "additionalProperties":false } } },` &&
      `{ "type":"function", "function": { "name":"read_variable", "description":"Read the exact runtime value or type summary of any variable available in the current ABAP debugger context. The user must confirm before execution.", "parameters": { "type":"object", "properties": { "variable": { "type":"string", "description":"ABAP debugger expression, variable name, field path, component, reference, or table expression to inspect" }, "reason": { "type":"string", "description":"Visible intent shown to the user before confirmation" } }, "required":["variable","reason"], "additionalProperties":false } } },` &&
      `{ "type":"function", "function": { "name":"set_breakpoint", "description":"Request a session breakpoint. The user must confirm before execution.", "parameters": { "type":"object", "properties": { "program": { "type":"string" }, "include": { "type":"string" }, "line": { "type":"integer" }, "reason": { "type":"string", "description":"Visible intent shown to the user before confirmation" } }, "required":["include","line","reason"], "additionalProperties":false } } }` &&
      `]`.

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

    rs_action-tool = is_call-name.

    TRY.
        IF is_call-name = 'step_debugger'.
          DATA ls_step TYPE ty_step_args.
          /ui2/cl_json=>deserialize( EXPORTING json = is_call-arguments CHANGING data = ls_step ).
          rs_action-command = ls_step-command.
          rs_action-reason = ls_step-reason.

        ELSEIF is_call-name = 'set_breakpoint'.
          DATA ls_break TYPE ty_break_args.
          /ui2/cl_json=>deserialize( EXPORTING json = is_call-arguments CHANGING data = ls_break ).
          MOVE-CORRESPONDING ls_break TO rs_action.

        ELSEIF is_call-name = 'read_variable'.
          DATA ls_read TYPE ty_read_args.
          /ui2/cl_json=>deserialize( EXPORTING json = is_call-arguments CHANGING data = ls_read ).
          MOVE-CORRESPONDING ls_read TO rs_action.
        ENDIF.
      CATCH cx_root INTO DATA(lx_root).
        rs_action-reason = |Cannot parse tool call arguments: { lx_root->get_text( ) }|.
    ENDTRY.

  ENDMETHOD.

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

  METHOD set_breakpoint.

    DATA(lv_program) = COND program( WHEN is_action-program IS NOT INITIAL THEN is_action-program ELSE is_action-include ).

    CALL FUNCTION 'RS_SET_BREAKPOINT'
      EXPORTING
        index       = is_action-line
        program     = CONV program( is_action-include )
        mainprogram = lv_program
        bp_type     = 'S'
      EXCEPTIONS
        not_executed = 1
        OTHERS       = 2.

    rv_text = COND string(
      WHEN sy-subrc = 0 THEN |Breakpoint set: { lv_program }/{ is_action-include }:{ is_action-line }. Intent: { is_action-reason }|
      ELSE |Breakpoint failed: { lv_program }/{ is_action-include }:{ is_action-line }. Intent: { is_action-reason }| ).

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

  METHOD append_line.
    APPEND i_line TO ct_lines.
  ENDMETHOD.

ENDCLASS.
