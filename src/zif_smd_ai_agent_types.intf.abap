INTERFACE zif_smd_ai_agent_types
  PUBLIC.

  TYPES:
    BEGIN OF ty_action,
      tool              TYPE string,
      command           TYPE string,
      program           TYPE string,
      include           TYPE string,
      line              TYPE i,
      mode              TYPE string,
      variable          TYPE string,
      reason            TYPE string,
      arguments         TYPE string,
      status            TYPE string,
      diagnosis         TYPE string,
      evidence_variable TYPE string,
      evidence_step     TYPE string,
      evidence_value    TYPE string,
      fix_suggestion    TYPE string,
    END OF ty_action.

  TYPES tt_action TYPE STANDARD TABLE OF ty_action WITH EMPTY KEY.

ENDINTERFACE.
