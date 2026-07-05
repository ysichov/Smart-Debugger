*<SCRIPT:PERSISTENT>
REPORT  z_smart_debugger_script.

*<SCRIPT:HEADER>
*<SCRIPTNAME>Z_SMART_DEBUGGER_SCRIPT</SCRIPTNAME>
*<SCRIPT_CLASS>lcl_debugger_script</SCRIPT_CLASS>
*<SINGLE_RUN>X</SINGLE_RUN>

*</SCRIPT:HEADER>

*<SCRIPT:PRESETTINGS>

*</SCRIPT:PRESETTINGS>

*<SCRIPT:SCRIPT_CLASS>
*<SCRIPT:PERSISTENT>

*<SCRIPT:HEADER>
*<SCRIPTNAME>Z_SMART_DEBUGGER_TEST</SCRIPTNAME>
*<SCRIPT_CLASS>lcl_debugger_script</SCRIPT_CLASS>
*<SCRIPT_COMMENT>Debugger Skript: Default Template</SCRIPT_COMMENT>
*<SINGLE_STEP>X</SINGLE_STEP>

*</SCRIPT:HEADER>

*<SCRIPT:PRESETTINGS>

*</SCRIPT:PRESETTINGS>

*<SCRIPT:SCRIPT_CLASS>

"REPORT smart_debugger_Script.
*  & Smart  Debugger (Project ARIADNA - Advanced Reverse Ingeneering Abap Debugger with New Analytycs )
*  & Multi-windows program for viewing all objects and data structures in debug
*  &---------------------------------------------------------------------*
*  & version: beta 0.9.600
*  & Git https://github.com/ysichov/SDDE
*  & RU description - https://ysychov.wordpress.com/2020/07/27/abap-simple-debugger-data-explorer/
*  & EN description - https://github.com/ysichov/SDDE/wiki

*  & Written by Yurii Sychov
*  & e-mail:   ysichov@gmail.com
*  & blog:     https://ysychov.wordpress.com/blog/
*  & LinkedIn: https://www.linkedin.com/in/ysychov/
*  &---------------------------------------------------------------------*

*  & External resources
*  & https://github.com/WegnerDan/abapMermaid
*  & https://github.com/ysichov/abapMermaid - should be used this fork with scroll enabled
*  & https://gist.github.com/AtomKrieg/7f4ec2e2f49b82def162e85904b7e25b - data object visualizer

*  & Inspired by
*  & https://habr.com/ru/articles/504908/
*  & https://github.com/larshp/ABAP-Object-Visualizer - Abap Object Visualizer
*  & https://github.com/ysichov/SDE_abapgit - Simple Data Explorer

CLASS lcl_debugger_script DEFINITION DEFERRED.

CLASS lcl_debugger_script DEFINITION INHERITING FROM zcl_smd_debugger_base.

  PUBLIC SECTION.
    METHODS: prologue  REDEFINITION,
             init      REDEFINITION,
             script    REDEFINITION,
             end       REDEFINITION.

ENDCLASS.

CLASS lcl_debugger_script IMPLEMENTATION.

  METHOD prologue.
    super->prologue( ).
  ENDMETHOD.                    "prolog

  METHOD init.

    CONSTANTS: c_mask TYPE x VALUE '01'.

    is_step = abap_on.
    zcl_smd_appl=>check_mermaid( ).
    zcl_smd_appl=>init_lang( ).
    zcl_smd_appl=>init_icons_table( ).

    mo_window = NEW zcl_smd_window( me ).

    mo_tree_imp = NEW zcl_smd_rtti_tree( i_header   = 'Importing parameters'
                                     i_type     = 'I'
                                     i_cont     = mo_window->mo_importing_container
                                     i_debugger = me ).

    mo_tree_local = NEW zcl_smd_rtti_tree( i_header   = 'Variables'
                                       i_type     = 'L'
                                       i_cont     = mo_window->mo_locals_container
                                       i_debugger = me ).

    mo_tree_exp = NEW zcl_smd_rtti_tree( i_header   = 'Exporting & Returning parameters'
                                     i_type     = 'E'
                                     i_cont     = mo_window->mo_exporting_container
                                     i_debugger = me ).

    mo_tree_local->m_locals = mo_tree_local->m_locals BIT-XOR c_mask.

  ENDMETHOD.

  METHOD script.

    run_script( ).
    show_step( ).
    me->break( ).

  ENDMETHOD.

  METHOD end. "dummy method
  ENDMETHOD.

ENDCLASS.                    "lcl_debugger_script IMPLEMENTATION


*</SCRIPT:SCRIPT_CLASS>

*</SCRIPT:PERSISTENT>
