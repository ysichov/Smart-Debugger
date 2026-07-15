class ZCL_SMD_POPUP definition
  public
  create public .

public section.

  class-data M_COUNTER type I .
  data M_ADDITIONAL_NAME type STRING .
  data MO_BOX type ref to CL_GUI_DIALOGBOX_CONTAINER .
  data MO_SPLITTER type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_SPLITTER_IMP_EXP type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_VARIABLES_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_TABLES_CONTAINER type ref to CL_GUI_CONTAINER .

  methods CONSTRUCTOR
    importing
      !I_ADDITIONAL_NAME type STRING optional .
  methods CREATE
    importing
      !I_WIDTH type I
      !I_HIGHT type I
      !I_NAME type TEXT100 optional
    returning
      value(RO_BOX) type ref to CL_GUI_DIALOGBOX_CONTAINER .
  methods ON_BOX_CLOSE
    for event CLOSE of CL_GUI_DIALOGBOX_CONTAINER
    importing
      !SENDER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SMD_POPUP IMPLEMENTATION.


  method CONSTRUCTOR.

    m_additional_name = i_additional_name.


  endmethod.


  method CREATE.


    DATA: top  TYPE i,
          left TYPE i.

    ADD 1 TO m_counter.
    top  = left = 1 + 2 * ( m_counter DIV 5 ) +  ( m_counter MOD 5 ) * 10.

    CREATE OBJECT ro_box
      EXPORTING
        width                       = i_width
        height                      = i_hight
        top                         = top
        left                        = left
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


  endmethod.


  method ON_BOX_CLOSE.

    "close dependent child popups of this box and drop finished entries,
    "otherwise children stay open orphaned and mt_popups grows forever
    LOOP AT zcl_smd_appl=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>) WHERE parent = sender.
      IF <popup>-child IS BOUND.
        <popup>-child->free( EXCEPTIONS cntl_error        = 1
                                        cntl_system_error = 2
                                        OTHERS            = 3 ).
        CLEAR <popup>-child.
      ENDIF.
    ENDLOOP.
    DELETE zcl_smd_appl=>mt_popups WHERE child = sender OR child IS INITIAL.

    sender->free( EXCEPTIONS cntl_error        = 1
                             cntl_system_error = 2
                             OTHERS            = 3 ).

  endmethod.
ENDCLASS.
