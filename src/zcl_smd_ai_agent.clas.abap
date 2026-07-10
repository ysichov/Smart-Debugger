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
        !et_actions    TYPE zif_smd_ai_agent_types=>tt_action
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
    METHODS reset_last_tool_result.
    METHODS get_last_tool_result
      RETURNING VALUE(rv_text) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.

TYPES tt_string TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    DATA mo_debugger TYPE REF TO zcl_smd_debugger_base.
    DATA mv_last_error TYPE string.
    DATA mv_last_tool_result TYPE string.
    DATA mt_action_log TYPE tt_string.
    DATA mt_ai_breakpoints TYPE tt_string.

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

    METHODS get_action_log_text
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

    METHODS remember_ai_breakpoint
      IMPORTING
        !is_action TYPE zif_smd_ai_agent_types=>ty_action.

    METHODS forget_ai_breakpoint
      IMPORTING
        !is_action TYPE zif_smd_ai_agent_types=>ty_action.

    METHODS is_known_ai_breakpoint
      IMPORTING
        !is_action        TYPE zif_smd_ai_agent_types=>ty_action
      RETURNING
        VALUE(rv_known)   TYPE abap_bool.
METHODS validate_evidence
      IMPORTING is_action TYPE zif_smd_ai_agent_types=>ty_action
      EXPORTING ev_detail TYPE string
      RETURNING VALUE(rv_valid) TYPE abap_bool.
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

      IF mt_ai_breakpoints IS NOT INITIAL.
        append_line( EXPORTING i_line = `` CHANGING ct_lines = lt_lines ).
        append_line( EXPORTING i_line = `Known AI-set breakpoints:` CHANGING ct_lines = lt_lines ).
        LOOP AT mt_ai_breakpoints INTO DATA(lv_ai_breakpoint).
          append_line(
            EXPORTING
              i_line = |{ sy-tabix }. { lv_ai_breakpoint }|
            CHANGING
              ct_lines = lt_lines ).
        ENDLOOP.
      ENDIF.

      IF mo_debugger->mo_window IS BOUND.
        lv_line = |Screen program: program={ mo_debugger->mo_window->m_prg-program } |.
        lv_line = lv_line && |include={ mo_debugger->mo_window->m_prg-include } |.
        lv_line = lv_line && |line={ mo_debugger->mo_window->m_prg-line }|.
        append_line( EXPORTING i_line = lv_line CHANGING ct_lines = lt_lines ).

        append_line( EXPORTING i_line = `` CHANGING ct_lines = lt_lines ).
        append_line( EXPORTING i_line = `Source code with real line numbers:` CHANGING ct_lines = lt_lines ).

        LOOP AT mo_debugger->mo_window->mt_source INTO DATA(ls_source_for_prompt).
          append_line(
            EXPORTING
              i_line = |--- include={ ls_source_for_prompt-include } ---|
            CHANGING
              ct_lines = lt_lines ).

          IF ls_source_for_prompt-source IS INITIAL.
            append_line( EXPORTING i_line = `source unavailable` CHANGING ct_lines = lt_lines ).
            CONTINUE.
          ENDIF.

          LOOP AT ls_source_for_prompt-source->lines INTO DATA(lv_source_line).
            append_line(
              EXPORTING
                i_line = |{ sy-tabix }: { lv_source_line }|
              CHANGING
                ct_lines = lt_lines ).
          ENDLOOP.
        ENDLOOP.
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

      WHEN 'report_findings'.
        IF is_action-status = 'confirmed'.
          DATA lv_detail TYPE string.
          DATA(lv_valid) = validate_evidence(
            EXPORTING is_action = is_action
            IMPORTING ev_detail = lv_detail ).

          IF lv_valid = abap_true.
            rv_text =
              |FINDINGS CONFIRMED - { lv_detail }. Diagnosis: { is_action-diagnosis }| &&
              COND #( WHEN is_action-fix_suggestion IS NOT INITIAL
                      THEN | Fix: { is_action-fix_suggestion }|
                      ELSE `` ).
          ELSE.
            rv_text =
              |FINDINGS REJECTED - { lv_detail } | &&
              |This diagnosis stays a hypothesis; it is NOT confirmed. | &&
              |Propose the next debugger action (set_breakpoint / step_debugger / | &&
              |read_variable) that would produce verifiable evidence for | &&
              |evidence_variable={ is_action-evidence_variable }, then call | &&
              |report_findings again once that evidence is visible in Variable | &&
              |history or Current variables/state.|.
          ENDIF.
        ELSE.
          rv_text =
            |Hypothesis recorded: { is_action-diagnosis }. Not yet confirmed - | &&
            |continue investigating and propose the next debugger action.|.
        ENDIF.

      WHEN OTHERS.
        rv_text = execute_plugin_tool( is_action ).
        IF is_action-tool = 'set_breakpoint'
        AND mo_debugger IS BOUND
        AND mo_debugger->mo_window IS BOUND.
          IF rv_text CS 'set=X'.
            remember_ai_breakpoint( is_action ).
          ENDIF.
          IF rv_text CS 'deleted=X'.
            forget_ai_breakpoint( is_action ).
          ENDIF.
          mo_debugger->mo_window->set_program_line( mo_debugger->mo_window->m_prg-line ).
        ENDIF.
    ENDCASE.

    IF mv_last_tool_result IS INITIAL.
      mv_last_tool_result = rv_text.
    ELSE.
      mv_last_tool_result = mv_last_tool_result && cl_abap_char_utilities=>newline && rv_text.
    ENDIF.

    APPEND |{ sy-datum } { sy-uzeit } tool={ is_action-tool } command={ is_action-command } variable={ is_action-variable } target={ is_action-include }:{ is_action-line } reason={ is_action-reason } result={ rv_text }|
      TO mt_action_log.

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


METHOD get_action_log_text.

    CHECK mt_action_log IS NOT INITIAL.

    rv_text = `Confirmed AI actions:`.
    LOOP AT mt_action_log INTO DATA(lv_log_line).
      rv_text = rv_text &&
        cl_abap_char_utilities=>newline &&
        |{ sy-tabix }. { lv_log_line }|.
    ENDLOOP.

  ENDMETHOD.


METHOD get_default_api_key.

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

    " Ask for the password on every AI request.
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
      'Find likely bugs, suspicious state transitions, wrong variable values, ' &&
      'and useful next debug actions. ' &&
      'In a single turn you may call several tools together when they form ' &&
      'one logical chain, for example set_breakpoint together with ' &&
      'step_debugger F8 to run to it, or a single step_debugger together ' &&
      'with every read_variable call needed to inspect the resulting state. ' &&
      'The chain you propose in one turn is confirmed once by the user and ' &&
      'then executed automatically in this fixed order: all set_breakpoint ' &&
      'calls first, then at most one step_debugger call, then all ' &&
      'read_variable calls; you are not asked again between these steps, ' &&
      'so request every variable you will need at that point together with ' &&
      'the step, in the same turn. ' &&
      'Call at most one step_debugger per turn; never propose two step or ' &&
      'continue commands in the same turn, since you have not observed the ' &&
      'state in between them. ' &&
      'CONCLUSIONS GO THROUGH A TOOL, NOT PROSE - this overrides any older ' &&
      'habit of writing a free-text "Findings: confirmed" sentence. The ' &&
      'only way to state a conclusion, hypothesis or confirmed, is to call ' &&
      'the report_findings tool. Do not write your own "Findings" or ' &&
      '"confirmed by runtime evidence" wording in prose; call the tool ' &&
      'instead and let its result stand as the record. ' &&
      'If source code is available, you may form a diagnosis from code ' &&
      'reading, but call report_findings with status=hypothesis until ' &&
      'runtime state confirms it - never status=confirmed at that point. ' &&
      'report_findings with status=confirmed is only valid when ' &&
      'evidence_variable and evidence_value literally appear together, ' &&
      'this turn, in the user prompt sections "Current variables/state:", ' &&
      '"Variable history:", or "Last confirmed tool result:". A value you ' &&
      'compute yourself by mentally re-executing the ABAP logic, ' &&
      'hand-tracing a VALUE #( ... ) construction, or reasoning about what ' &&
      'a loop or calculation "must" produce is source-reading, not runtime ' &&
      'evidence, even when you are confident it is correct and even when ' &&
      'it later turns out to be correct - use status=hypothesis for that. ' &&
      'A status=confirmed call that does not match those sections will be ' &&
      'rejected automatically and the investigation continues; if that ' &&
      'happens, propose the next debugger action that would produce the ' &&
      'missing evidence instead of repeating the same confirmed claim. ' &&
      'For a suspected logic bug, first set a breakpoint on the most ' &&
      'suspicious executable line, preferably the condition or assignment ' &&
      'that can prove the bug, not merely the loop header; combine it in ' &&
      'the same turn with step_debugger F8 to run to it, unless another ' &&
      'breakpoint should logically fire first. ' &&
      'After the breakpoint is confirmed and reached, combine step_debugger ' &&
      'with the read_variable calls needed to verify the relevant variables ' &&
      'in the same turn, before calling report_findings with status=confirmed. ' &&
      'If the bug is not yet runtime-confirmed, call report_findings with ' &&
      'status=hypothesis stating which diagnosis you suspect, and in the ' &&
      'same turn propose the next chain of debugger actions needed to ' &&
      'confirm it. ' &&
      'STOP CONDITION - this overrides every other instruction about ' &&
      'proposing actions: once report_findings has been called with ' &&
      'status=confirmed and accepted (its result says FINDINGS CONFIRMED, ' &&
      'not FINDINGS REJECTED), the investigation is over. Do not call ' &&
      'set_breakpoint, step_debugger, read_variable, or report_findings ' &&
      'again in that turn or any later turn for the same bug. Do not ' &&
      'invent further steps to double-check an already-confirmed fact, and ' &&
      'do not keep stepping through more loop iterations or customers ' &&
      '"just to be sure" once a FINDINGS CONFIRMED result exists. In that ' &&
      'case, summarize the fix (what code change would correct it) and say ' &&
      'plainly that no further debugging is needed - propose no new tool ' &&
      'chain. Only continue proposing debugger actions while a genuine ' &&
      'open question remains that runtime state has not yet answered, or ' &&
      'while the last report_findings call was rejected or was a hypothesis. ' &&
      'Use read_variable when the exact runtime value of any ABAP variable, ' &&
      'field, component, reference, or table expression is needed; call it ' &&
      'once per variable, and include every variable you currently need in ' &&
      'the same turn instead of asking one at a time. ' &&
      'Use set_breakpoint with real TPDA include and 1-based source line ' &&
      'numbers when stopping at a specific source line is useful; the ' &&
      'chain still requires user confirmation before it executes. ' &&
      'Do not set a breakpoint again if it is already listed under Known ' &&
      'AI-set breakpoints; continue to it or verify state there instead. ' &&
      'After a relevant breakpoint has been set and the next goal is to ' &&
      'reach it - including reaching the SAME breakpoint again on a later ' &&
      'loop iteration or a later call - propose step_debugger F8/continue; ' &&
      'do not use F5, F6, or F7 to walk toward a known breakpoint, since ' &&
      'single-stepping only advances one statement at a time and will not ' &&
      'reliably land on the next hit of that breakpoint. ' &&
      'Use F5/F6/F7 only once you are already at or near the suspicious ' &&
      'area and need a single-step observation, not to travel toward a ' &&
      'breakpoint that has not fired yet. ' &&
      'Never call F8/continue unless you explain why it is safe and what ' &&
      'breakpoint or guard stop will catch execution. ' &&
      'Before continuing, assume a guard breakpoint exists at the end of ' &&
      'the current include. ' &&
      'Do not reveal hidden chain-of-thought; write a concise analysis ' &&
      'summary instead. ' &&
      'Answer in English. Structure the answer as: Intent, Analysis, then ' &&
      'a report_findings tool call carrying your conclusion, plus any ' &&
      'further debugger tool calls needed this turn.'.
  ENDMETHOD.


  METHOD get_tools_json.

    DATA(lv_findings_desc) =
      `The ONLY way to conclude the investigation. ` &&
      `status=hypothesis: you suspect a cause from source reading or ` &&
      `partial evidence but have not yet observed a matching entry in ` &&
      `Variable history or Current variables/state; you must still ` &&
      `propose the next debugger action in this same turn. ` &&
      `status=confirmed: you can cite an exact evidence_variable and ` &&
      `evidence_value that literally appear together in the Variable ` &&
      `history or Current variables/state sections of the current prompt ` &&
      `(optionally at evidence_step); a value you computed yourself by ` &&
      `re-executing or hand-tracing the ABAP code does NOT qualify, even ` &&
      `if correct. A confirmed call that cannot be matched against those ` &&
      `sections will be rejected and the investigation will continue.`.

    DATA(lv_json) =
      `[` &&
      `{ "type":"function", "function": {` &&
      `"name":"step_debugger",` &&
      `"description":"Request one debugger movement. Use F8 to continue to the next breakpoint or guard stop.",` &&
      `"parameters": { "type":"object", "properties": {` &&
      `"command": { "type":"string", "enum":["F5","F6","F7","F8"],` &&
      `"description":"F5 step into, F6 step over, F7 step out, F8 continue to the next breakpoint or guard stop" },` &&
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
      `"additionalProperties":false } } },` &&
      `{ "type":"function", "function": {` &&
      `"name":"report_findings",` &&
      `"description":"` && lv_findings_desc && `",` &&
      `"parameters": { "type":"object", "properties": {` &&
      `"status": { "type":"string", "enum":["hypothesis","confirmed"],` &&
      `"description":"hypothesis = still investigating; confirmed = runtime-verified, ends the investigation" },` &&
      `"diagnosis": { "type":"string",` &&
      `"description":"One or two sentences describing the suspected or confirmed root cause" },` &&
      `"evidence_variable": { "type":"string",` &&
      `"description":"Required when status=confirmed. Exact variable name or path as printed in Variable history or Current variables/state" },` &&
      `"evidence_step": { "type":"string",` &&
      `"description":"Required when status=confirmed. Step number as shown in Variable history / Recent debugger steps" },` &&
      `"evidence_value": { "type":"string",` &&
      `"description":"Required when status=confirmed. Exact value shown for that variable at that step/state" },` &&
      `"fix_suggestion": { "type":"string",` &&
      `"description":"Suggested ABAP code change that would correct the confirmed bug" } },` &&
      `"required":["status","diagnosis"],` &&
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
        mode    TYPE string,
        reason  TYPE string,
      END OF ty_break_args,
      BEGIN OF ty_read_args,
        variable TYPE string,
        reason   TYPE string,
      END OF ty_read_args,
      BEGIN OF ty_findings_args,
        status            TYPE string,
        diagnosis         TYPE string,
        evidence_variable TYPE string,
        evidence_step     TYPE string,
        evidence_value    TYPE string,
        fix_suggestion    TYPE string,
      END OF ty_findings_args.

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
          TRANSLATE rs_action-mode TO LOWER CASE.
          CONDENSE rs_action-mode.
          IF rs_action-mode IS INITIAL.
            rs_action-mode = 'set'.
          ENDIF.

        ELSEIF i_name = 'read_variable'.
          DATA ls_read TYPE ty_read_args.
          /ui2/cl_json=>deserialize( EXPORTING json = i_arguments CHANGING data = ls_read ).
          MOVE-CORRESPONDING ls_read TO rs_action.

        ELSEIF i_name = 'report_findings'.
          DATA ls_findings TYPE ty_findings_args.
          /ui2/cl_json=>deserialize( EXPORTING json = i_arguments CHANGING data = ls_findings ).
          MOVE-CORRESPONDING ls_findings TO rs_action.
          TRANSLATE rs_action-status TO LOWER CASE.
          CONDENSE rs_action-status.
          IF rs_action-status <> 'confirmed'.
            rs_action-status = 'hypothesis'.
          ENDIF.
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

    CLEAR et_actions.
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

        LOOP AT lt_tool_calls INTO DATA(ls_call).
          APPEND parse_tool_call(
            i_name      = ls_call-name
            i_arguments = ls_call-arguments ) TO et_actions.
        ENDLOOP.

        ev_text =
          |Provider: { c_provider } / { c_model }| &&
          cl_abap_char_utilities=>newline &&
          |Time: { lo_llm->get_last_seconds( ) } sec, tokens in/out: { lo_llm->mv_last_tok_in }/{ lo_llm->mv_last_tok_out }| &&
          cl_abap_char_utilities=>newline &&
          lv_guard &&
          cl_abap_char_utilities=>newline &&
          cl_abap_char_utilities=>newline &&
          lv_answer.

        IF et_actions IS NOT INITIAL.
          ev_text = ev_text &&
            cl_abap_char_utilities=>newline &&
            cl_abap_char_utilities=>newline &&
            |Pending AI actions ({ lines( et_actions ) }):|.

          LOOP AT et_actions INTO DATA(ls_pending_action).
            ev_text = ev_text &&
              cl_abap_char_utilities=>newline &&
              |{ sy-tabix }. { ls_pending_action-tool } { ls_pending_action-command }{ ls_pending_action-variable } { ls_pending_action-include }:{ ls_pending_action-line } - { ls_pending_action-reason }|.
          ENDLOOP.

          ev_text = ev_text &&
            cl_abap_char_utilities=>newline &&
            cl_abap_char_utilities=>newline &&
            |Press AI again to confirm and run this chain.|.
        ELSE.
          DATA(lv_action_log) = get_action_log_text( ).
          IF lv_action_log IS NOT INITIAL.
            ev_text = ev_text &&
              cl_abap_char_utilities=>newline &&
              cl_abap_char_utilities=>newline &&
              lv_action_log.
          ENDIF.
        ENDIF.

      CATCH cx_root INTO DATA(lx_root).
        ev_text = |AI agent failed: { lx_root->get_text( ) }|.
    ENDTRY.

  ENDMETHOD.


  METHOD create.
    ro_agent = NEW zcl_smd_ai_agent( io_debugger = io_debugger ).
  ENDMETHOD.


  METHOD ask_password.

    DATA lt_fields TYPE STANDARD TABLE OF sval WITH EMPTY KEY.

    APPEND VALUE #( tabname    = 'ZAICODE_APIKEY'
                    fieldname  = 'SECRET'
                    fieldtext  = 'Password'
                    field_obl   = 'X' ) TO lt_fields.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title          = 'Enter password for the stored AI API key'
      TABLES
        fields                = lt_fields
      EXCEPTIONS
        OTHERS                = 2.

    IF sy-subrc <> 0.
      CLEAR rv_password.
      RETURN.
    ENDIF.

    READ TABLE lt_fields INDEX 1 INTO DATA(ls_field).
    IF sy-subrc = 0.
      rv_password = ls_field-value.
    ENDIF.

  ENDMETHOD.


METHOD forget_ai_breakpoint.

    DATA lv_include TYPE string.
    DATA lv_key TYPE string.

    CHECK is_action-tool = 'set_breakpoint'.
    CHECK is_action-include IS NOT INITIAL.
    CHECK is_action-line > 0.

    lv_include = is_action-include.
    TRANSLATE lv_include TO UPPER CASE.
    CONDENSE lv_include.
    lv_key = |{ lv_include }:{ is_action-line }|.

    DELETE mt_ai_breakpoints WHERE table_line = lv_key.

  ENDMETHOD.


METHOD remember_ai_breakpoint.

    DATA lv_include TYPE string.
    DATA lv_key TYPE string.

    CHECK is_action-tool = 'set_breakpoint'.
    CHECK is_action-include IS NOT INITIAL.
    CHECK is_action-line > 0.

    lv_include = is_action-include.
    TRANSLATE lv_include TO UPPER CASE.
    CONDENSE lv_include.
    lv_key = |{ lv_include }:{ is_action-line }|.

    READ TABLE mt_ai_breakpoints TRANSPORTING NO FIELDS
      WITH KEY table_line = lv_key.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    APPEND lv_key TO mt_ai_breakpoints.

  ENDMETHOD.


METHOD is_known_ai_breakpoint.

    DATA lv_include TYPE string.
    DATA lv_key TYPE string.

    CHECK is_action-tool = 'set_breakpoint'.
    CHECK is_action-include IS NOT INITIAL.
    CHECK is_action-line > 0.

    lv_include = is_action-include.
    TRANSLATE lv_include TO UPPER CASE.
    CONDENSE lv_include.
    lv_key = |{ lv_include }:{ is_action-line }|.

    READ TABLE mt_ai_breakpoints TRANSPORTING NO FIELDS
      WITH KEY table_line = lv_key.
    rv_known = xsdbool( sy-subrc = 0 ).

  ENDMETHOD.
METHOD reset_last_tool_result.
    CLEAR mv_last_tool_result.
  ENDMETHOD.
METHOD get_last_tool_result.
    rv_text = mv_last_tool_result.
  ENDMETHOD.
  METHOD validate_evidence.

    CLEAR ev_detail.
    rv_valid = abap_false.

    IF mo_debugger IS NOT BOUND.
      ev_detail = 'No debugger session is bound - nothing to verify against.'.
      RETURN.
    ENDIF.

    IF is_action-evidence_variable IS INITIAL.
      ev_detail = 'evidence_variable is empty - cannot verify.'.
      RETURN.
    ENDIF.

    DATA lv_step_str TYPE string.
    DATA lv_step     TYPE i.
    DATA lv_have_step TYPE abap_bool VALUE abap_false.

    lv_step_str = is_action-evidence_step.
    REPLACE ALL OCCURRENCES OF '#' IN lv_step_str WITH ``.
    CONDENSE lv_step_str.
    IF lv_step_str CO '0123456789' AND lv_step_str IS NOT INITIAL.
      lv_step = lv_step_str.
      lv_have_step = abap_true.
    ENDIF.

    " 1) Variable history - the authoritative, step-tagged evidence source
    LOOP AT mo_debugger->mt_vars_hist INTO DATA(ls_hist).
      IF lv_have_step = abap_true AND ls_hist-step <> lv_step.
        CONTINUE.
      ENDIF.

      IF ls_hist-name CS is_action-evidence_variable
      OR is_action-evidence_variable CS ls_hist-name
      OR ls_hist-path CS is_action-evidence_variable.

        IF is_action-evidence_value IS INITIAL
        OR ls_hist-short CS is_action-evidence_value
        OR is_action-evidence_value CS ls_hist-short.
          rv_valid = abap_true.
          ev_detail = |Matched in Variable history: step={ ls_hist-step } { ls_hist-name } = { ls_hist-short }|.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " 2) Current variables/state - fallback for "right now" evidence
    LOOP AT mo_debugger->mt_state INTO DATA(ls_state).
      IF ls_state-name CS is_action-evidence_variable
      OR is_action-evidence_variable CS ls_state-name
      OR ls_state-path CS is_action-evidence_variable.

        IF is_action-evidence_value IS INITIAL
        OR ls_state-short CS is_action-evidence_value
        OR is_action-evidence_value CS ls_state-short.
          rv_valid = abap_true.
          ev_detail = |Matched in Current variables/state: { ls_state-name } = { ls_state-short }|.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.

    ev_detail =
      |No matching entry found in Variable history| &&
      COND #( WHEN lv_have_step = abap_true THEN | (step { lv_step })| ELSE `` ) &&
      | or Current variables/state for variable '{ is_action-evidence_variable }'| &&
      | with value '{ is_action-evidence_value }'. This is source-reading, not runtime evidence.|.

  ENDMETHOD.
ENDCLASS.
