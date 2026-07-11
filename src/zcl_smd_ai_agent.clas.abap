CLASS zcl_smd_ai_agent DEFINITION
  PUBLIC
  CREATE PRIVATE .


  PUBLIC SECTION.
    CONSTANTS c_provider TYPE string VALUE 'MISTRAL'.
    CONSTANTS c_keyname  TYPE string VALUE 'DEFAULT'.
    CONSTANTS c_model    TYPE text255 VALUE 'codestral-latest'.

    METHODS constructor
      IMPORTING
        !io_debugger TYPE REF TO zcl_smd_debugger_base
        !is_config   TYPE zcl_abapai_llm_client=>ty_ai_config OPTIONAL.

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
        !is_config    TYPE zcl_abapai_llm_client=>ty_ai_config OPTIONAL
      RETURNING
        VALUE(ro_agent) TYPE REF TO zcl_smd_ai_agent.
    METHODS reset_last_tool_result.
    METHODS get_last_tool_result
      RETURNING VALUE(rv_text) TYPE string.
    METHODS get_action_log_text
      RETURNING
        VALUE(rv_text) TYPE string.
    METHODS is_at_guard_breakpoint
      RETURNING
        VALUE(rv_at_guard) TYPE abap_bool.
    METHODS has_confirmed_findings
      RETURNING
        VALUE(rv_confirmed) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.

TYPES tt_string TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    DATA mo_debugger TYPE REF TO zcl_smd_debugger_base.
    DATA ms_config TYPE zcl_abapai_llm_client=>ty_ai_config.
    DATA mv_last_error TYPE string.
    DATA mv_last_tool_result TYPE string.
    DATA mv_findings_confirmed TYPE abap_bool.
    DATA mv_api_password TYPE string.
    DATA mv_password_popup_open TYPE xfeld.
    DATA mv_waiting_for_password TYPE xfeld.
    DATA mv_guard_breakpoint TYPE string.
    DATA mo_password_popup TYPE REF TO zcl_smd_password_popup.
    DATA mt_action_log TYPE tt_string.
    DATA mv_log_llm TYPE i.
    DATA mv_log_tool TYPE i.
    DATA mv_total_tok_in TYPE i.
    DATA mv_total_tok_out TYPE i.
    DATA mv_source_window_include TYPE string.
    DATA mv_source_window_from TYPE i.
    DATA mv_source_window_to TYPE i.
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

    METHODS cleanup_breakpoints.

    METHODS execute_plugin_tool
      IMPORTING
        !is_action     TYPE zif_smd_ai_agent_types=>ty_action
      RETURNING
        VALUE(rv_text) TYPE string.

    METHODS set_source_window
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
    METHODS evidence_value_matches
      IMPORTING
        !i_actual       TYPE string
        !i_expected     TYPE string
      RETURNING
        VALUE(rv_match) TYPE abap_bool.
METHODS validate_evidence
      IMPORTING is_action TYPE zif_smd_ai_agent_types=>ty_action
      EXPORTING ev_detail TYPE string
      RETURNING VALUE(rv_valid) TYPE abap_bool.
ENDCLASS.



CLASS ZCL_SMD_AI_AGENT IMPLEMENTATION.

  METHOD constructor.
    mo_debugger = io_debugger.
    ms_config = is_config.
  ENDMETHOD.


  method APPEND_LINE.
    APPEND i_line TO ct_lines.
  endmethod.


METHOD build_prompt.

    DATA lt_lines TYPE tt_string.
    DATA lv_line TYPE string.
    DATA lv_newline TYPE string.
    DATA lv_current_include TYPE string.
    DATA lv_window_active TYPE abap_bool.
    DATA lv_history_instruction TYPE string.

    lv_newline = cl_abap_char_utilities=>newline.
    lv_history_instruction = `MANDATORY FIRST ACTION: analyze Variable history against the source before proposing any breakpoint or step. If the history proves the defect, report it as confirmed immediately; do not set a breakpoint.`.

    append_line( EXPORTING i_line = |Task: { i_task }| CHANGING ct_lines = lt_lines ).
    append_line(
      EXPORTING
        i_line = lv_history_instruction
      CHANGING
        ct_lines = lt_lines ).
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
        lv_current_include = mo_debugger->mo_window->m_prg-include.
        TRANSLATE lv_current_include TO UPPER CASE.
        CONDENSE lv_current_include.

        IF mv_source_window_include IS NOT INITIAL
        AND lv_current_include = mv_source_window_include.
          lv_window_active = abap_true.
        ENDIF.

        lv_line = |Screen program: program={ mo_debugger->mo_window->m_prg-program } |.
        lv_line = lv_line && |include={ mo_debugger->mo_window->m_prg-include } |.
        lv_line = lv_line && |line={ mo_debugger->mo_window->m_prg-line }|.
        append_line( EXPORTING i_line = lv_line CHANGING ct_lines = lt_lines ).

        append_line( EXPORTING i_line = `` CHANGING ct_lines = lt_lines ).
        IF lv_window_active = abap_true.
          append_line(
            EXPORTING
              i_line = |Source code window with real line numbers: include={ mv_source_window_include } lines={ mv_source_window_from }-{ mv_source_window_to }|
            CHANGING
              ct_lines = lt_lines ).
          IF mo_debugger->mo_window->m_prg-line < mv_source_window_from
          OR mo_debugger->mo_window->m_prg-line > mv_source_window_to.
            append_line(
              EXPORTING
                i_line = |Current line { mo_debugger->mo_window->m_prg-line } is outside the chosen source window; call set_source_window together with the next real debugger action if another code range is needed.|
              CHANGING
                ct_lines = lt_lines ).
          ENDIF.
        ELSE.
          append_line( EXPORTING i_line = `Full source code with real line numbers:` CHANGING ct_lines = lt_lines ).
          append_line( EXPORTING i_line = `Call set_source_window with the first and last source line needed, together with the real debugger actions for this turn.` CHANGING ct_lines = lt_lines ).
        ENDIF.

        LOOP AT mo_debugger->mo_window->mt_source INTO DATA(ls_source_for_prompt).
          DATA(lv_source_include) = ls_source_for_prompt-include.
          TRANSLATE lv_source_include TO UPPER CASE.
          CONDENSE lv_source_include.

          IF lv_window_active = abap_true AND lv_source_include <> mv_source_window_include.
            CONTINUE.
          ENDIF.

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
            IF lv_window_active = abap_true
            AND ( sy-tabix < mv_source_window_from OR sy-tabix > mv_source_window_to ).
              CONTINUE.
            ENDIF.

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

        DATA(lv_hist_line) = ``.
        DATA(lv_hist_program) = ``.
        DATA(lv_hist_include) = ``.
        READ TABLE mo_debugger->mt_steps INTO DATA(ls_hist_step)
          WITH KEY step = ls_hist-step.
        IF sy-subrc = 0.
          lv_hist_line = | line={ ls_hist_step-line }|.
          lv_hist_program = | program={ ls_hist_step-program }|.
          lv_hist_include = | include={ ls_hist_step-include }|.
        ENDIF.

        append_line(
          EXPORTING
            i_line = |step={ ls_hist-step } stack={ ls_hist-stack }{ lv_hist_line }{ lv_hist_program }{ lv_hist_include } { ls_hist-leaf } { ls_hist-name } = { ls_hist-short } type={ ls_hist-type } path={ ls_hist-path }|
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

    IF sy-subrc = 0.
      TRANSLATE lv_include TO UPPER CASE.
      CONDENSE lv_include.
      mv_guard_breakpoint = |{ lv_include }:{ lv_line }|.
    ENDIF.

  ENDMETHOD.


  METHOD execute_action.

    CASE is_action-tool.
      WHEN 'read_variable'.
        rv_text = read_variable( is_action-variable ).

      WHEN 'set_source_window'.
        rv_text = set_source_window( is_action ).

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

    IF rv_text CS 'FINDINGS CONFIRMED'.
      mv_findings_confirmed = abap_true.
    ENDIF.

    IF mv_last_tool_result IS INITIAL.
      mv_last_tool_result = rv_text.
    ELSE.
      mv_last_tool_result = mv_last_tool_result && cl_abap_char_utilities=>newline && rv_text.
    ENDIF.

    IF mv_log_llm IS INITIAL.
      mv_log_llm = 1.
    ENDIF.
    ADD 1 TO mv_log_tool.

    DATA(lv_log_line) =
      |{ mv_log_llm }.{ mv_log_tool } **tool** `{ is_action-tool }`| &&
      | command=`{ is_action-command }` variable=`{ is_action-variable }`| &&
      | target=`{ is_action-include }:{ is_action-line }`| &&
      | range=`{ is_action-from_line }-{ is_action-to_line }`| &&
      | reason={ is_action-reason } result={ rv_text }|.

    IF is_action-tool = 'set_breakpoint' AND rv_text CS 'deleted=X'.
      lv_log_line = |!DELETE_BP! { lv_log_line }|.
    ENDIF.

    APPEND lv_log_line TO mt_action_log.

    IF is_action-tool = 'set_breakpoint' AND rv_text CS 'set=X'.
      cleanup_breakpoints( ).
    ENDIF.

  ENDMETHOD.


METHOD cleanup_breakpoints.

    DATA lv_include TYPE string.
    DATA lv_program TYPE string.
    DATA lv_key TYPE string.
    DATA lv_deleted TYPE string.
    DATA lv_type TYPE char1.
    DATA lt_seen TYPE tt_string.

    CHECK mo_debugger IS BOUND.
    CHECK mo_debugger->mo_window IS BOUND.

    LOOP AT mo_debugger->mo_window->mt_bpoints INTO DATA(ls_breakpoint).
      lv_include = ls_breakpoint-include.
      IF lv_include IS INITIAL.
        lv_include = mo_debugger->mo_window->m_prg-include.
      ENDIF.

      CHECK lv_include IS NOT INITIAL.
      CHECK ls_breakpoint-line > 0.

      TRANSLATE lv_include TO UPPER CASE.
      CONDENSE lv_include.
      lv_key = |{ lv_include }:{ ls_breakpoint-line }|.

      READ TABLE lt_seen TRANSPORTING NO FIELDS WITH KEY table_line = lv_key.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.
      APPEND lv_key TO lt_seen.

      IF lv_key = mv_guard_breakpoint.
        CONTINUE.
      ENDIF.

      READ TABLE mt_ai_breakpoints TRANSPORTING NO FIELDS WITH KEY table_line = lv_key.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      lv_program = ls_breakpoint-program.
      IF lv_program IS INITIAL.
        lv_program = lv_include.
      ENDIF.

      lv_type = ls_breakpoint-type.
      IF lv_type IS INITIAL.
        lv_type = 'S'.
      ENDIF.

      CALL FUNCTION 'RS_DELETE_BREAKPOINT'
        EXPORTING
          index        = ls_breakpoint-line
          mainprog     = CONV progname( lv_program )
          program      = CONV progname( lv_include )
          bp_type      = lv_type
        EXCEPTIONS
          not_executed = 1
          OTHERS       = 2.

      IF sy-subrc = 0.
        ADD 1 TO mv_log_tool.
        lv_deleted = |!DELETE_BP! { mv_log_llm }.{ mv_log_tool } **auto delete breakpoint** target=`{ lv_key }` type=`{ lv_type }` reason=cleanup: keep only final guard and AI-set breakpoints|.
        APPEND lv_deleted TO mt_action_log.
      ENDIF.
    ENDLOOP.

    mo_debugger->mo_window->set_program_line( mo_debugger->mo_window->m_prg-line ).

  ENDMETHOD.


METHOD set_source_window.

    DATA lv_include TYPE string.
    DATA lv_from TYPE i.
    DATA lv_to TYPE i.

    lv_include = is_action-include.
    TRANSLATE lv_include TO UPPER CASE.
    CONDENSE lv_include.
    lv_from = is_action-from_line.
    lv_to = is_action-to_line.

    IF lv_include IS INITIAL.
      rv_text = 'set_source_window: include is empty'.
      RETURN.
    ENDIF.

    IF lv_from <= 0 OR lv_to <= 0 OR lv_from > lv_to.
      rv_text = |set_source_window: invalid range { lv_from }-{ lv_to }|.
      RETURN.
    ENDIF.

    mv_source_window_include = lv_include.
    mv_source_window_from = lv_from.
    mv_source_window_to = lv_to.

    rv_text = |Source window set: { mv_source_window_include }:{ mv_source_window_from }-{ mv_source_window_to }. Reason: { is_action-reason }|.

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

    rv_text = `# AI Log`.
    LOOP AT mt_action_log INTO DATA(lv_log_line).
      rv_text = rv_text &&
        cl_abap_char_utilities=>newline &&
        cl_abap_char_utilities=>newline &&
        lv_log_line.
    ENDLOOP.

  ENDMETHOD.


METHOD is_at_guard_breakpoint.

    DATA lv_include TYPE string.
    DATA lv_key TYPE string.

    CHECK mv_guard_breakpoint IS NOT INITIAL.
    CHECK mo_debugger IS BOUND.
    CHECK mo_debugger->mo_window IS BOUND.

    lv_include = mo_debugger->mo_window->m_prg-include.
    TRANSLATE lv_include TO UPPER CASE.
    CONDENSE lv_include.
    lv_key = |{ lv_include }:{ mo_debugger->mo_window->m_prg-line }|.

    rv_at_guard = xsdbool( lv_key = mv_guard_breakpoint ).

  ENDMETHOD.


METHOD get_default_api_key.

    CLEAR: rv_api_key, mv_last_error, mv_waiting_for_password.

    IF ms_config-apikey IS NOT INITIAL.
      rv_api_key = ms_config-apikey.
      RETURN.
    ENDIF.

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

    IF mv_api_password IS INITIAL.
      mv_api_password = ask_password( ).
    ENDIF.

    IF mv_api_password IS INITIAL.
      mv_waiting_for_password = abap_true.
      mv_last_error = |Enter AI key password in the popup, close it, then press AI Run again.|.
      RETURN.
    ENDIF.

    TRY.
        rv_api_key = zcl_aicode_crypto=>decrypt(
          i_username = ls_key-username
          i_provider = c_provider
          i_name     = c_keyname
          i_password = mv_api_password
          i_secret   = ls_key-secret ).
      CATCH cx_sec_sxml_encrypt_error.
        CLEAR mv_api_password.
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
      'Never repeat the same sentence or the same proposed breakpoint ' &&
      'plan in prose. If you notice that your answer is repeating itself, ' &&
      'stop writing prose immediately and use the available tools instead. ' &&
      'Keep Intent and Analysis short: at most one paragraph each. ' &&
      'When the prompt contains full source code, call set_source_window ' &&
      'in the same turn with the include, first line, and last line you ' &&
      'need for further debugging. Keep this window as small as practical ' &&
      'but large enough to understand the logic. Later prompts may include ' &&
      'only that window; if execution leaves it, full source will be sent ' &&
      'again and you must choose a new source window. Never call only ' &&
      'set_source_window by itself; pair it with real debugger actions ' &&
      'such as set_breakpoint, step_debugger, or read_variable unless the ' &&
      'investigation is already complete. ' &&
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
      'Treat Variable history as a primary runtime evidence source. First ' &&
      'compare the code with the ordered variable transitions in that table ' &&
      'and look for the first incorrect value or invariant violation. If ' &&
      'the history already proves the defect, call report_findings with ' &&
      'status=confirmed immediately; do not request another step merely to ' &&
      'reconfirm evidence already present in the history. Source-only ' &&
      'reasoning without supporting history remains a hypothesis. ' &&
      'This history-first analysis is mandatory before every breakpoint ' &&
      'or step proposal. Never set a breakpoint above the current position ' &&
      'just to rediscover a transition that is already recorded in Variable ' &&
      'history. If history is sufficient, the only tool call should be ' &&
      'report_findings with status=confirmed. ' &&
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
      'Only when Variable history lacks the decisive transition, set a ' &&
      'breakpoint on the most ' &&
      'suspicious executable line, preferably the condition or assignment ' &&
      'that can prove the bug, not merely the loop header; combine it in ' &&
      'the same turn with step_debugger F8 to run to it, unless another ' &&
      'breakpoint should logically fire first. ' &&
      'Prefer setting breakpoints after the operation that changes the ' &&
      'value you need to inspect, so F8 lands where the post-state is ' &&
      'already visible. Set a breakpoint before an operation only when ' &&
      'the pre-state itself is needed as evidence. ' &&
      'When debugging loops, prefer checking the last relevant operation ' &&
      'inside the loop body, or the last operation that can prove the ' &&
      'iteration result, to minimize repeated loop iterations. ' &&
      'A breakpoint on the last executable line of a block is sufficient ' &&
      'to inspect the block result: never add earlier breakpoints in that ' &&
      'same block, because execution reaching the final breakpoint already ' &&
      'passes through them. Add an earlier breakpoint only when a separate ' &&
      'pre-state or intermediate transition is specifically required and ' &&
      'cannot be inferred from the variable history. ' &&
      'After the breakpoint is confirmed and reached, combine step_debugger ' &&
      'with the read_variable calls needed to verify the relevant variables ' &&
      'in the same turn, before calling report_findings with status=confirmed. ' &&
      'If the same turn has just read the evidence_variable with the ' &&
      'evidence_value you need, call report_findings with status=confirmed ' &&
      'in that same tool chain; do not call status=hypothesis again for ' &&
      'the same diagnosis. ' &&
      'Only if the code and history do not distinguish the possible causes, ' &&
      'call report_findings with status=hypothesis and request the minimum ' &&
      'next debugger action needed to resolve that specific uncertainty. ' &&
      'Prefer one targeted F5, F6, F7, or F8 action over exploratory stepping. ' &&
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
      'Use set_breakpoint with mode=delete to remove AI-set temporary ' &&
      'breakpoints as soon as they have served their purpose, especially ' &&
      'loop breakpoints that would otherwise catch every F8/continue. ' &&
      'When deleting a breakpoint, use the same include, line, and ' &&
      'breakpoint_type that were used to set it. ' &&
      'After a relevant breakpoint has been set and the next goal is to ' &&
      'reach it - including reaching the SAME breakpoint again on a later ' &&
      'loop iteration or a later call - propose step_debugger F8/continue; ' &&
      'do not use F5, F6, or F7 to walk toward a known breakpoint, since ' &&
      'single-stepping only advances one statement at a time and will not ' &&
      'reliably land on the next hit of that breakpoint. ' &&
      'Prefer F8/continue over F5/F6/F7 whenever a breakpoint or guard ' &&
      'stop will catch execution; single-step only for a local observation ' &&
      'that cannot be reached safely with F8. ' &&
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
      'tool calls. Do not output long numbered plans when tools are available; ' &&
      'convert the plan into actual tool calls. End the text immediately ' &&
      'after the tool calls are chosen. ' &&
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
      `status=confirmed: base the conclusion on the completed Variable ` &&
      `history in the prompt. evidence_variable, evidence_step, and ` &&
      `evidence_value are useful references, but do not request another ` &&
      `live debugger read merely to repeat a historical observation. A ` &&
      `finding based on the collected history is already runtime evidence.`.

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
      `"name":"set_source_window",` &&
      `"description":"Choose the source include and first/last line needed for the next debugging steps. Use together with real debugger actions; do not call this as the only action.",` &&
      `"parameters": { "type":"object", "properties": {` &&
      `"include": { "type":"string",` &&
      `"description":"ABAP include/program containing the relevant code window" },` &&
      `"from_line": { "type":"integer",` &&
      `"description":"First 1-based source line needed for analysis" },` &&
      `"to_line": { "type":"integer",` &&
      `"description":"Last 1-based source line needed for analysis" },` &&
      `"reason": { "type":"string",` &&
      `"description":"Why this source window is sufficient" } },` &&
      `"required":["include","from_line","to_line","reason"],` &&
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
      BEGIN OF ty_source_window_args,
        include   TYPE string,
        from_line TYPE i,
        to_line   TYPE i,
        reason    TYPE string,
      END OF ty_source_window_args,
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

        ELSEIF i_name = 'set_source_window'.
          DATA ls_source_window TYPE ty_source_window_args.
          /ui2/cl_json=>deserialize( EXPORTING json = i_arguments CHANGING data = ls_source_window ).
          MOVE-CORRESPONDING ls_source_window TO rs_action.

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

    CONSTANTS c_max_llm_attempts TYPE i VALUE 3.
    CONSTANTS c_max_answer_chars TYPE i VALUE 1800.

    CLEAR et_actions.
    DATA(lv_api_key) = get_default_api_key( ).
    IF lv_api_key IS INITIAL.
      IF mv_waiting_for_password = abap_true.
        ev_text = mv_last_error.
      ELSE.
        ev_text = |AI agent cannot start: { mv_last_error }|.
      ENDIF.
      RETURN.
    ENDIF.

    TRY.
        DATA(lv_guard) = ensure_guard_breakpoint( ).

        DATA(lo_llm) = NEW zcl_abapai_llm_client(
          i_model    = COND #( WHEN ms_config-model IS INITIAL THEN c_model ELSE ms_config-model )
          i_apikey   = lv_api_key
          i_provider = COND #( WHEN ms_config-provider IS INITIAL THEN c_provider ELSE ms_config-provider ) ).

        lo_llm->set_temperature( '0.1' ).
        lo_llm->set_max_tokens( 1200 ).

        DATA(lo_context) = NEW zcl_ai_tool_context(
          io_llm        = lo_llm
          i_agents_path = `` ).

        zcl_ai_tool_factory=>initialize( lo_context ).

        DATA lt_tool_calls TYPE zcl_code_ai_api=>tt_tool_calls.
        DATA lv_answer TYPE string.
        DATA lv_attempt TYPE i.
        DATA lv_error_text TYPE string.
        DATA lv_error_lc TYPE string.

        WHILE lv_attempt < c_max_llm_attempts.
          ADD 1 TO lv_attempt.
          TRY.
              CLEAR lt_tool_calls.
              lv_answer = lo_llm->ask_with_tools(
                EXPORTING
                  i_prompt        = build_prompt( i_task )
                  i_system_prompt = get_system_prompt( )
                  i_tools_json    = get_tools_json( )
                IMPORTING
                  et_tool_calls   = lt_tool_calls ).
              EXIT.
            CATCH cx_root INTO DATA(lx_llm).
              lv_error_text = lx_llm->get_text( ).
              lv_error_lc = lv_error_text.
              TRANSLATE lv_error_lc TO LOWER CASE.

              IF lv_attempt < c_max_llm_attempts
              AND ( lv_error_lc CS 'limit'
                 OR lv_error_lc CS '429'
                 OR lv_error_lc CS 'quota'
                 OR lv_error_lc CS 'rate'
                 OR lv_error_lc CS 'too many requests' ).
                APPEND |LLM call hit limit on attempt { lv_attempt }: { lv_error_text }. Waiting 5 sec and retrying.|
                  TO mt_action_log.
                WAIT UP TO 5 SECONDS.
                CONTINUE.
              ENDIF.

              ev_text = |AI agent failed: { lv_error_text }|.
              RETURN.
          ENDTRY.
        ENDWHILE.

        LOOP AT lt_tool_calls INTO DATA(ls_call).
          APPEND parse_tool_call(
            i_name      = ls_call-name
            i_arguments = ls_call-arguments ) TO et_actions.
        ENDLOOP.

        DATA(lv_answer_log) = lv_answer.
        IF strlen( lv_answer_log ) > c_max_answer_chars.
          lv_answer_log = lv_answer_log(c_max_answer_chars) &&
            cl_abap_char_utilities=>newline &&
            |[AI answer truncated in log/UI: model produced too much prose instead of tool calls.]|.
        ENDIF.

        ev_text =
          |Provider: { c_provider } / { c_model }| &&
          cl_abap_char_utilities=>newline &&
          |Time: { lo_llm->get_last_seconds( ) } sec, tokens in/out: { lo_llm->mv_last_tok_in }/{ lo_llm->mv_last_tok_out }| &&
          cl_abap_char_utilities=>newline &&
          lv_guard &&
          cl_abap_char_utilities=>newline &&
          cl_abap_char_utilities=>newline &&
          lv_answer_log.

        IF et_actions IS NOT INITIAL.
          ev_text = ev_text &&
            cl_abap_char_utilities=>newline &&
            cl_abap_char_utilities=>newline &&
            |Pending AI actions ({ lines( et_actions ) }):|.

          LOOP AT et_actions INTO DATA(ls_pending_action).
            IF ls_pending_action-tool = 'set_source_window'.
              ev_text = ev_text &&
                cl_abap_char_utilities=>newline &&
                |{ sy-tabix }. { ls_pending_action-tool }| &&
                | { ls_pending_action-include }:{ ls_pending_action-from_line }-{ ls_pending_action-to_line }| &&
                | - { ls_pending_action-reason }|.
            ELSE.
              ev_text = ev_text &&
                cl_abap_char_utilities=>newline &&
                |{ sy-tabix }. { ls_pending_action-tool } { ls_pending_action-command }| &&
                |{ ls_pending_action-variable } { ls_pending_action-include }:{ ls_pending_action-line }| &&
                | - { ls_pending_action-reason }|.
            ENDIF.
          ENDLOOP.

        ENDIF.

        ADD 1 TO mv_log_llm.
        CLEAR mv_log_tool.
        mv_total_tok_in = mv_total_tok_in + lo_llm->mv_last_tok_in.
        mv_total_tok_out = mv_total_tok_out + lo_llm->mv_last_tok_out.

        DATA(lv_llm_log_line) =
          |{ mv_log_llm }. **LLM call** task={ i_task }| &&
          | seconds={ lo_llm->get_last_seconds( ) }| &&
          | tokens in/out=`{ lo_llm->mv_last_tok_in }/{ lo_llm->mv_last_tok_out }`| &&
          | total in/out=`{ mv_total_tok_in }/{ mv_total_tok_out }`| &&
          | actions={ lines( et_actions ) } answer={ lv_answer_log }|.

        APPEND lv_llm_log_line TO mt_action_log.

      CATCH cx_root INTO DATA(lx_root).
        ev_text = |AI agent failed: { lx_root->get_text( ) }|.
    ENDTRY.

  ENDMETHOD.


  METHOD create.
    ro_agent = NEW zcl_smd_ai_agent(
      io_debugger = io_debugger
      is_config   = is_config ).
  ENDMETHOD.


  METHOD ask_password.

    IF mv_password_popup_open IS INITIAL.
      mv_password_popup_open = abap_true.
      mo_password_popup = NEW zcl_smd_password_popup(
        ir_password = REF #( mv_api_password )
        ir_open     = REF #( mv_password_popup_open ) ).
    ENDIF.

    rv_password = mv_api_password.

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
METHOD has_confirmed_findings.
    rv_confirmed = mv_findings_confirmed.
  ENDMETHOD.
  METHOD validate_evidence.

    CLEAR ev_detail.
    rv_valid = abap_false.

    IF mo_debugger IS NOT BOUND.
      ev_detail = 'No debugger session is bound - nothing to verify against.'.
      RETURN.
    ENDIF.

    "The debugger run is already complete when the AI analyzes this prompt.
    "Variable history is the collected runtime evidence; do not require a
    "second live read or an exact current-value match for a historical finding.
    IF mo_debugger->mt_vars_hist IS NOT INITIAL.
      rv_valid = abap_true.
      ev_detail = |Variable history available ({ lines( mo_debugger->mt_vars_hist ) } entries); finding validated against the completed debug run|.
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

    " 1) Last tool result - evidence read earlier in the same action chain
    IF mv_last_tool_result IS NOT INITIAL.
      SPLIT mv_last_tool_result AT cl_abap_char_utilities=>newline INTO TABLE DATA(lt_tool_lines).

      LOOP AT lt_tool_lines INTO DATA(lv_tool_line).
        IF lv_tool_line CS is_action-evidence_variable
        AND ( is_action-evidence_value IS INITIAL
           OR lv_tool_line CS is_action-evidence_value ).
          rv_valid = abap_true.
          ev_detail = |Matched in Last tool result: { lv_tool_line }|.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " 2) Variable history - the authoritative, step-tagged evidence source
    LOOP AT mo_debugger->mt_vars_hist INTO DATA(ls_hist).
      IF lv_have_step = abap_true AND ls_hist-step <> lv_step.
        CONTINUE.
      ENDIF.

      IF ls_hist-name CS is_action-evidence_variable
      OR is_action-evidence_variable CS ls_hist-name
      OR ls_hist-path CS is_action-evidence_variable.

        IF is_action-evidence_value IS INITIAL
        OR evidence_value_matches(
             i_actual   = ls_hist-short
             i_expected = is_action-evidence_value ) = abap_true.
          rv_valid = abap_true.
          ev_detail = |Matched in Variable history: step={ ls_hist-step } { ls_hist-name } = { ls_hist-short }|.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " 3) Current variables/state - fallback for "right now" evidence
    LOOP AT mo_debugger->mt_state INTO DATA(ls_state).
      IF ls_state-name CS is_action-evidence_variable
      OR is_action-evidence_variable CS ls_state-name
      OR ls_state-path CS is_action-evidence_variable.

        IF is_action-evidence_value IS INITIAL
        OR evidence_value_matches(
             i_actual   = ls_state-short
             i_expected = is_action-evidence_value ) = abap_true.
          rv_valid = abap_true.
          ev_detail = |Matched in Current variables/state: { ls_state-name } = { ls_state-short }|.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.

    ev_detail =
      |No matching entry found in Last tool result, Variable history| &&
      COND #( WHEN lv_have_step = abap_true THEN | (step { lv_step })| ELSE `` ) &&
      | or Current variables/state for variable '{ is_action-evidence_variable }'| &&
      | with value '{ is_action-evidence_value }'. This is source-reading, not runtime evidence.|.

  ENDMETHOD.


METHOD evidence_value_matches.

    DATA lv_actual TYPE string.
    DATA lv_expected TYPE string.
    DATA lv_actual_num TYPE string.
    DATA lv_expected_num TYPE string.
    DATA lv_actual_dec TYPE decfloat34.
    DATA lv_expected_dec TYPE decfloat34.

    lv_actual = i_actual.
    lv_expected = i_expected.
    CONDENSE: lv_actual, lv_expected.

    IF lv_actual CS lv_expected OR lv_expected CS lv_actual.
      rv_match = abap_true.
      RETURN.
    ENDIF.

    lv_actual_num = lv_actual.
    lv_expected_num = lv_expected.
    REPLACE ALL OCCURRENCES OF ',' IN lv_actual_num WITH ``.
    REPLACE ALL OCCURRENCES OF ',' IN lv_expected_num WITH ``.
    CONDENSE: lv_actual_num, lv_expected_num.

    TRY.
        lv_actual_dec = lv_actual_num.
        lv_expected_dec = lv_expected_num.
        IF lv_actual_dec = lv_expected_dec.
          rv_match = abap_true.
        ENDIF.
      CATCH cx_sy_conversion_no_number cx_sy_conversion_overflow.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
