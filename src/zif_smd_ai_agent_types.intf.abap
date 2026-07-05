INTERFACE zif_smd_ai_agent_types
  PUBLIC.

  TYPES:
    BEGIN OF ty_action,
      tool     TYPE string,
      command  TYPE string,
      program  TYPE string,
      include  TYPE string,
      line     TYPE i,
      mode     TYPE string,
      variable TYPE string,
      reason   TYPE string,
      arguments TYPE string,
    END OF ty_action.

ENDINTERFACE.
