CLASS zcl_smd_aitool_breakpoint DEFINITION
  PUBLIC
  INHERITING FROM zcl_aitool_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS c_tool_name TYPE string VALUE 'set_breakpoint'.

    METHODS zif_ai_tool~get_tool_name REDEFINITION.
    METHODS zif_ai_tool~get_schema REDEFINITION.
    METHODS zif_ai_tool~get_prompt_fragment REDEFINITION.
    METHODS zif_ai_tool~execute       REDEFINITION.

  PRIVATE SECTION.
    METHODS get_int_attribute
      IMPORTING
        !i_json         TYPE string
        !i_name         TYPE string
      RETURNING VALUE(rv_value) TYPE i.
ENDCLASS.

CLASS zcl_smd_aitool_breakpoint IMPLEMENTATION.

  METHOD zif_ai_tool~get_tool_name.
    rv_name = c_tool_name.
  ENDMETHOD.

  METHOD zif_ai_tool~get_schema.

    rv_schema =
      `{ "type":"function", "function": {` &&
      `"name":"set_breakpoint",` &&
      `"description":"Set, delete, or toggle an ABAP debugger breakpoint.",` &&
      `"parameters": { "type":"object", "properties": {` &&
      `"program": { "type":"string",` &&
      `"description":"Main program. If omitted, include is used." },` &&
      `"include": { "type":"string",` &&
      `"description":"ABAP include/program for the breakpoint line." },` &&
      `"line": { "type":"integer",` &&
      `"description":"1-based source line number." },` &&
      `"breakpoint_type": { "type":"string", "enum":["S","E"],` &&
      `"description":"S = session breakpoint, E = external breakpoint." },` &&
      `"mode": { "type":"string", "enum":["set","delete","toggle"],` &&
      `"description":"Breakpoint operation. Default set." },` &&
      `"reason": { "type":"string",` &&
      `"description":"Short visible intent." } },` &&
      `"required":["include","line","reason"],` &&
      `"additionalProperties":false } } }`.

  ENDMETHOD.

  METHOD zif_ai_tool~get_prompt_fragment.

    rv_fragment =
      `### set_breakpoint` && cl_abap_char_utilities=>newline &&
      `Sets, deletes, or toggles an ABAP debugger breakpoint.` && cl_abap_char_utilities=>newline &&
      `Use this tool only when a breakpoint directly supports the user's debugging goal. ` &&
      `Always state the intent in the reason argument. ` &&
      `Use mode=delete when a loop breakpoint has already served its purpose and would stop repeated continue/F8 actions.`.

  ENDMETHOD.

  METHOD zif_ai_tool~execute.

    DATA(lv_program) = get_json_attribute( i_json = i_arguments i_name = 'program' ).
    DATA(lv_include) = get_json_attribute( i_json = i_arguments i_name = 'include' ).
    DATA(lv_mode)    = get_json_attribute( i_json = i_arguments i_name = 'mode' ).
    DATA(lv_type)    = get_json_attribute( i_json = i_arguments i_name = 'breakpoint_type' ).
    DATA(lv_reason)  = get_json_attribute( i_json = i_arguments i_name = 'reason' ).
    DATA(lv_line)    = get_int_attribute( i_json = i_arguments i_name = 'line' ).

    TRANSLATE lv_program TO UPPER CASE.
    TRANSLATE lv_include TO UPPER CASE.
    TRANSLATE lv_mode TO LOWER CASE.
    TRANSLATE lv_type TO UPPER CASE.
    CONDENSE: lv_program, lv_include, lv_mode, lv_type.

    IF lv_include IS INITIAL.
      rs_result-error_text = 'set_breakpoint: include is empty'.
      RETURN.
    ENDIF.

    IF lv_program IS INITIAL.
      lv_program = lv_include.
    ENDIF.

    IF lv_line <= 0.
      rs_result-error_text = |set_breakpoint: invalid line { lv_line }|.
      RETURN.
    ENDIF.

    IF lv_mode IS INITIAL.
      lv_mode = 'set'.
    ENDIF.

    IF lv_type IS INITIAL.
      lv_type = 'S'.
    ENDIF.

    IF lv_type <> 'S' AND lv_type <> 'E'.
      rs_result-error_text = |set_breakpoint: unsupported breakpoint_type { lv_type }. Use S or E.|.
      RETURN.
    ENDIF.

    DATA(lv_deleted) = abap_false.
    DATA(lv_set) = abap_false.

    IF lv_mode = 'delete' OR lv_mode = 'toggle'.
      CALL FUNCTION 'RS_DELETE_BREAKPOINT'
        EXPORTING
          index    = lv_line
          mainprog = CONV progname( lv_program )
          program  = CONV progname( lv_include )
          bp_type  = CONV char1( lv_type )
        EXCEPTIONS
          not_executed = 1
          OTHERS       = 2.
      IF sy-subrc = 0.
        lv_deleted = abap_true.
      ENDIF.
    ENDIF.

    IF lv_mode = 'set' OR ( lv_mode = 'toggle' AND lv_deleted = abap_false ).
      CALL FUNCTION 'RS_SET_BREAKPOINT'
        EXPORTING
          index       = lv_line
          program     = CONV progname( lv_include )
          mainprogram = CONV progname( lv_program )
          bp_type     = CONV char1( lv_type )
        EXCEPTIONS
          not_executed = 1
          OTHERS       = 2.
      IF sy-subrc = 0.
        lv_set = abap_true.
      ENDIF.
    ENDIF.

    IF lv_mode <> 'set' AND lv_mode <> 'delete' AND lv_mode <> 'toggle'.
      rs_result-error_text = |set_breakpoint: unsupported mode { lv_mode }. Use set, delete or toggle.|.
      RETURN.
    ENDIF.

    rs_result-save_required = abap_false.
    rs_result-xml_payload =
      |set_breakpoint result: mode={ lv_mode }, program={ lv_program }, include={ lv_include }, line={ lv_line }, type={ lv_type }, set={ lv_set }, deleted={ lv_deleted }. Reason: { lv_reason }|.

  ENDMETHOD.

  METHOD get_int_attribute.

    DATA lv_value TYPE string.
    DATA(lv_pattern) = |"{ i_name }"\\s*:\\s*([0-9]+)|.
    FIND FIRST OCCURRENCE OF REGEX lv_pattern IN i_json
      SUBMATCHES lv_value.
    rv_value = CONV i( lv_value ).

  ENDMETHOD.

ENDCLASS.
