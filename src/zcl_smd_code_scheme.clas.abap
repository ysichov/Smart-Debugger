"! Mermaid scheme of the control structure of one include: IF/ELSEIF/ELSE,
"! CASE/WHEN, LOOP/DO/WHILE, TRY/CATCH and the enclosing METHOD/FORM.
"! Plain statements are collapsed into "N operations" nodes — the point is
"! the shape of the branch, not every line of it.
"!
"! Blocks are taken from CL_CI_SCAN->STRUCTURES, which already knows the
"! first and last statement of every block and their nesting. Pairing the
"! keywords by hand (the way the old MAGIC_SEARCH did) needs a case for
"! single-line IF ... ENDIF, for SELECT without ENDSELECT, for AT without
"! ENDAT — the scanner has all of that right already.
CLASS zcl_smd_code_scheme DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    "! @parameter it_source | source lines of the include
    "! @parameter it_kw     | parsed statement keywords (index / line / name)
    "! @parameter io_scan   | scan of the same include; without it there is
    "!                        no block information and only a title is drawn
    "! @parameter i_title   | caption of the root node
    CLASS-METHODS build
      IMPORTING it_source    TYPE STANDARD TABLE
                it_kw        TYPE zcl_smd_window=>tt_kword OPTIONAL
                io_scan      TYPE REF TO cl_ci_scan OPTIONAL
                i_title      TYPE string OPTIONAL
      RETURNING VALUE(rv_mm) TYPE string.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_line,
        line  TYPE i,
        text  TYPE string,
        word  TYPE string,
        depth TYPE i,
        kind  TYPE char1,   " 'O'=opener 'B'=branch 'C'=closer 'P'=plain
                            " 'S'=whole block on one line
        end   TYPE i,       " last line of this header's own branch segment
        all   TYPE i,       " openers: last line before the matching closer
      END OF ts_line,
      tt_line TYPE STANDARD TABLE OF ts_line WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_stmt,
        index TYPE i,
        line  TYPE i,
        word  TYPE string,
      END OF ts_stmt,
      tt_stmt TYPE STANDARD TABLE OF ts_stmt WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_block,
        open  TYPE i,
        close TYPE i,
        word  TYPE string,
      END OF ts_block,
      tt_block TYPE STANDARD TABLE OF ts_block WITH EMPTY KEY.

    CONSTANTS c_apos TYPE c LENGTH 1 VALUE ''''.
    CONSTANTS c_branches TYPE string VALUE ' ELSEIF ELSE WHEN CATCH CLEANUP '.

    CLASS-METHODS analyze
      IMPORTING it_source       TYPE STANDARD TABLE
                it_kw           TYPE zcl_smd_window=>tt_kword
                io_scan         TYPE REF TO cl_ci_scan
      RETURNING VALUE(rt_lines) TYPE tt_line.

    "! Closing keyword expected for an opening one, '' if not a block opener.
    CLASS-METHODS closer_of
      IMPORTING i_word          TYPE string
      RETURNING VALUE(r_closer) TYPE string.

    "! Statement text, trimmed and stripped of what would break a label.
    CLASS-METHODS scheme_label
      IMPORTING i_text        TYPE string
      RETURNING VALUE(r_text) TYPE string.

ENDCLASS.



CLASS zcl_smd_code_scheme IMPLEMENTATION.


  METHOD build.

    TYPES: BEGIN OF ts_prev,
             depth TYPE i,
             node  TYPE string,
             line  TYPE i,
           END OF ts_prev.
    DATA lt_prev TYPE STANDARD TABLE OF ts_prev WITH EMPTY KEY.
    DATA lv_edges TYPE string.

    TYPES: BEGIN OF ts_sub,
             endline TYPE i,
           END OF ts_sub.
    DATA lt_sub TYPE STANDARD TABLE OF ts_sub WITH EMPTY KEY.

    DATA(lt_lines) = analyze( it_source = it_source it_kw = it_kw io_scan = io_scan ).

    " Running count of plain statements, so the number of operations between
    " two lines is one subtraction rather than a scan.
    DATA lt_ops TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DATA(lv_run) = 0.
    LOOP AT lt_lines INTO DATA(ls_cnt).
      IF ls_cnt-kind = 'P' AND ls_cnt-word IS NOT INITIAL.
        lv_run = lv_run + 1.
      ENDIF.
      APPEND lv_run TO lt_ops.
    ENDLOOP.

    rv_mm = |flowchart LR\n|.

    " A METHOD/FORM line is the root and carries the qualified name
    DATA(lv_root) = 0.
    LOOP AT lt_lines INTO DATA(ls_root)
      WHERE kind = 'O' AND ( word = 'METHOD' OR word = 'FORM' OR word = 'MODULE' ).
      lv_root = ls_root-line.
      EXIT.
    ENDLOOP.

    DATA(lv_prev_node)  = ||.
    DATA(lv_prev_depth) = 0.
    DATA(lv_prev_line)  = 0.
    IF lv_root = 0.
      rv_mm = rv_mm && |  start(["{ scheme_label( i_title ) }"])\n|.
      lv_prev_node = |start|.
    ENDIF.

    LOOP AT lt_lines INTO DATA(ls_line).

      " Close every frame whose block ended before this line
      WHILE lines( lt_sub ) > 0.
        READ TABLE lt_sub INTO DATA(ls_sub) INDEX 1.
        IF ls_sub-endline >= ls_line-line. EXIT. ENDIF.
        rv_mm = rv_mm && |  end\n|.
        DELETE lt_sub INDEX 1.
      ENDWHILE.

      CHECK ls_line-kind = 'O' OR ls_line-kind = 'B' OR ls_line-kind = 'S'.

      DATA(lv_node)  = |n{ ls_line-line }|.
      DATA(lv_label) = COND string(
        WHEN ls_line-line = lv_root AND i_title IS NOT INITIAL
        THEN scheme_label( i_title )
        ELSE scheme_label( ls_line-text ) ).

      " A loop is the frame around its body, not a node of its own
      IF ls_line-kind = 'O' AND ls_line-all > ls_line-line
        AND ( ls_line-word = 'LOOP' OR ls_line-word = 'DO' OR ls_line-word = 'WHILE' ).
        rv_mm = rv_mm && |  subgraph g{ ls_line-line }["{ lv_label }"]\n|.
        rv_mm = rv_mm && |  direction LR\n|.
        INSERT VALUE #( endline = ls_line-all ) INTO lt_sub INDEX 1.
        CONTINUE.
      ENDIF.

      DATA(lv_shape) = SWITCH string( ls_line-word
        WHEN 'IF' OR 'CASE'                 THEN |{ lv_node }\{"{ lv_label }"\}|
        WHEN 'METHOD' OR 'FORM' OR 'MODULE' THEN |{ lv_node }[["{ lv_label }"]]|
        ELSE                                     |{ lv_node }("{ lv_label }")| ).
      rv_mm = rv_mm && |  { lv_shape }\n|.

      " Edge source: previous node of the same level, or the enclosing
      " header when this is the first structure line of a block.
      DATA(lv_from)      = lv_prev_node.
      DATA(lv_from_line) = lv_prev_line.
      LOOP AT lt_prev INTO DATA(ls_prev) WHERE depth = ls_line-depth.
        lv_from      = ls_prev-node.
        lv_from_line = ls_prev-line.
      ENDLOOP.
      IF ls_line-depth > lv_prev_depth.
        lv_from      = lv_prev_node.
        lv_from_line = lv_prev_line.
      ENDIF.

      IF lv_from IS NOT INITIAL.
        DATA(lv_ops) = 0.
        IF ls_line-line > lv_from_line + 1 AND lv_from_line > 0.
          READ TABLE lt_ops INTO DATA(lv_to_cnt) INDEX ls_line-line - 1.
          IF sy-subrc = 0.
            READ TABLE lt_ops INTO DATA(lv_fr_cnt) INDEX lv_from_line.
            IF sy-subrc = 0. lv_ops = lv_to_cnt - lv_fr_cnt. ENDIF.
          ENDIF.
        ENDIF.

        IF lv_ops > 0.
          DATA(lv_ops_node) = |o{ ls_line-line }|.
          rv_mm = rv_mm && |  { lv_ops_node }["{ lv_ops } { COND string(
            WHEN lv_ops = 1 THEN 'operation' ELSE 'operations' ) }"]\n|.
          lv_edges = lv_edges && |  { lv_from } --> { lv_ops_node }\n|.
          lv_edges = lv_edges && |  { lv_ops_node } --> { lv_node }\n|.
        ELSE.
          lv_edges = lv_edges && |  { lv_from } --> { lv_node }\n|.
        ENDIF.
      ENDIF.

      DELETE lt_prev WHERE depth >= ls_line-depth.
      APPEND VALUE #( depth = ls_line-depth node = lv_node line = ls_line-line ) TO lt_prev.
      lv_prev_node  = lv_node.
      lv_prev_depth = ls_line-depth.
      lv_prev_line  = ls_line-line.
    ENDLOOP.

    WHILE lines( lt_sub ) > 0.
      rv_mm = rv_mm && |  end\n|.
      DELETE lt_sub INDEX 1.
    ENDWHILE.

    " Edges last: an edge written inside a subgraph pulls its nodes into
    " that subgraph and the nesting falls apart.
    rv_mm = rv_mm && lv_edges.

  ENDMETHOD.


  METHOD analyze.

    FIELD-SYMBOLS <lv_src> TYPE any.
    DATA lt_stmt  TYPE tt_stmt.
    DATA lt_block TYPE tt_block.
    DATA lv_depth TYPE i.

    LOOP AT it_source ASSIGNING <lv_src>.
      APPEND INITIAL LINE TO rt_lines ASSIGNING FIELD-SYMBOL(<ls_line>).
      <ls_line>-line = sy-tabix.
      <ls_line>-text = <lv_src>.
      " 'P' rather than a blank: a string template drops trailing blanks
      <ls_line>-kind = 'P'.
    ENDLOOP.

    " One entry per statement, in program order — several statements can
    " share a line ("IF x. y. ENDIF." written on one line).
    LOOP AT it_kw INTO DATA(ls_kw).
      CHECK ls_kw-line > 0 AND ls_kw-line <= lines( rt_lines ).
      APPEND VALUE #( index = ls_kw-index
                      line  = ls_kw-line
                      word  = to_upper( ls_kw-name ) ) TO lt_stmt.
    ENDLOOP.
    SORT lt_stmt BY index.

    " The line's own word is the first statement starting on it
    LOOP AT lt_stmt INTO DATA(ls_first).
      READ TABLE rt_lines ASSIGNING <ls_line> INDEX ls_first-line.
      CHECK sy-subrc = 0.
      IF <ls_line>-word IS INITIAL. <ls_line>-word = ls_first-word. ENDIF.
    ENDLOOP.

    CHECK io_scan IS BOUND.

    " Blocks straight from the scanner
    LOOP AT io_scan->structures INTO DATA(ls_struc).
      READ TABLE lt_stmt INTO DATA(ls_sf) WITH KEY index = ls_struc-stmnt_from.
      CHECK sy-subrc = 0.
      READ TABLE lt_stmt INTO DATA(ls_st) WITH KEY index = ls_struc-stmnt_to.
      CHECK sy-subrc = 0.
      " KEY_START is a flag (domain BOOLEAN), not the keyword — the opening
      " word comes from the statement itself, which also filters out the
      " structures that are not blocks at all.
      DATA(lv_key) = ls_sf-word.
      CHECK closer_of( lv_key ) IS NOT INITIAL.
      " A block on one line holds nothing to fold but is still a branch
      IF ls_sf-line = ls_st-line.
        READ TABLE rt_lines ASSIGNING <ls_line> INDEX ls_sf-line.
        IF sy-subrc = 0 AND <ls_line>-kind = 'P'. <ls_line>-kind = 'S'. ENDIF.
        CONTINUE.
      ENDIF.
      APPEND VALUE #( open = ls_sf-line close = ls_st-line word = lv_key ) TO lt_block.
    ENDLOOP.

    " Depth as a running sum over the blocks, plus the opener/closer marks
    DATA lt_delta TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DO lines( rt_lines ) TIMES.
      APPEND 0 TO lt_delta.
    ENDDO.
    LOOP AT lt_block INTO DATA(ls_block).
      READ TABLE rt_lines ASSIGNING <ls_line> INDEX ls_block-open.
      IF sy-subrc = 0. <ls_line>-kind = 'O'. ENDIF.
      READ TABLE rt_lines ASSIGNING <ls_line> INDEX ls_block-close.
      IF sy-subrc = 0. <ls_line>-kind = 'C'. ENDIF.
      READ TABLE lt_delta ASSIGNING FIELD-SYMBOL(<lv_d>) INDEX ls_block-open + 1.
      IF sy-subrc = 0. <lv_d> = <lv_d> + 1. ENDIF.
      READ TABLE lt_delta ASSIGNING <lv_d> INDEX ls_block-close.
      IF sy-subrc = 0. <lv_d> = <lv_d> - 1. ENDIF.
    ENDLOOP.

    LOOP AT rt_lines ASSIGNING <ls_line>.
      READ TABLE lt_delta INTO DATA(lv_step) INDEX sy-tabix.
      IF sy-subrc = 0. lv_depth = lv_depth + lv_step. ENDIF.
      IF lv_depth < 0. lv_depth = 0. ENDIF.
      <ls_line>-depth = lv_depth.
    ENDLOOP.

    " Branches, attached to the block that encloses them
    DATA lt_owner TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    LOOP AT rt_lines ASSIGNING <ls_line>.
      DATA(lv_line_no) = sy-tabix.
      LOOP AT lt_block INTO ls_block WHERE open = lv_line_no.
        INSERT ls_block-word INTO lt_owner INDEX 1.
      ENDLOOP.
      LOOP AT lt_block INTO ls_block WHERE close = lv_line_no.
        DELETE lt_owner INDEX 1.
      ENDLOOP.

      CHECK <ls_line>-kind = 'P'.
      CHECK <ls_line>-word IS NOT INITIAL.
      CHECK c_branches CS | { <ls_line>-word } |.
      READ TABLE lt_owner INDEX 1 INTO DATA(lv_owner).
      CHECK sy-subrc = 0.
      " ELSEIF/ELSE belong to IF, WHEN to CASE, CATCH/CLEANUP to TRY
      DATA(lv_ok) = xsdbool(
        (    ( <ls_line>-word = 'ELSEIF' OR <ls_line>-word = 'ELSE' ) AND lv_owner = 'IF' )
        OR ( <ls_line>-word = 'WHEN' AND lv_owner = 'CASE' )
        OR ( ( <ls_line>-word = 'CATCH' OR <ls_line>-word = 'CLEANUP' ) AND lv_owner = 'TRY' ) ).
      CHECK lv_ok = abap_true.
      <ls_line>-kind  = 'B'.
      <ls_line>-depth = <ls_line>-depth - 1.
      IF <ls_line>-depth < 0. <ls_line>-depth = 0. ENDIF.
    ENDLOOP.

    " For every header, the segment ends before the next line at the same
    " depth that is a branch or a closer.
    LOOP AT rt_lines ASSIGNING <ls_line> WHERE kind = 'O' OR kind = 'B'.
      DATA(lv_start) = sy-tabix.
      DATA(lv_end)   = <ls_line>-line.
      DATA(lv_from)  = lv_start + 1.
      LOOP AT rt_lines ASSIGNING FIELD-SYMBOL(<ls_next>) FROM lv_from.
        IF <ls_next>-depth <= <ls_line>-depth
          AND ( <ls_next>-kind = 'B' OR <ls_next>-kind = 'C' ).
          EXIT.
        ENDIF.
        lv_end = <ls_next>-line.
      ENDLOOP.
      <ls_line>-end = lv_end.
    ENDLOOP.

    " Openers also get the whole-block extent
    LOOP AT rt_lines ASSIGNING <ls_line> WHERE kind = 'O'.
      DATA(lv_ostart) = sy-tabix.
      DATA(lv_ofrom)  = lv_ostart + 1.
      LOOP AT rt_lines ASSIGNING <ls_next> FROM lv_ofrom.
        IF <ls_next>-kind = 'C' AND <ls_next>-depth = <ls_line>-depth.
          <ls_line>-all = <ls_next>-line - 1.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF <ls_line>-all < <ls_line>-line. <ls_line>-all = <ls_line>-line. ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD closer_of.
    CASE i_word.
      WHEN 'IF'.      r_closer = 'ENDIF'.
      WHEN 'CASE'.    r_closer = 'ENDCASE'.
      WHEN 'LOOP'.    r_closer = 'ENDLOOP'.
      WHEN 'DO'.      r_closer = 'ENDDO'.
      WHEN 'WHILE'.   r_closer = 'ENDWHILE'.
      WHEN 'TRY'.     r_closer = 'ENDTRY'.
      WHEN 'METHOD'.  r_closer = 'ENDMETHOD'.
      WHEN 'FORM'.    r_closer = 'ENDFORM'.
      WHEN 'MODULE'.  r_closer = 'ENDMODULE'.
      WHEN 'SELECT'.  r_closer = 'ENDSELECT'.
      WHEN 'AT'.      r_closer = 'ENDAT'.
      WHEN 'PROVIDE'. r_closer = 'ENDPROVIDE'.
    ENDCASE.
  ENDMETHOD.


  METHOD scheme_label.
    r_text = condense( i_text ).
    " The diagram source passes through HTML twice, so angle brackets are
    " read as tags: FIELD-SYMBOL(<WATCH>) loses everything from the '<' on.
    " Entities do not survive either — they decode back on the second pass —
    " so the brackets are replaced outright.
    REPLACE ALL OCCURRENCES OF '<>' IN r_text WITH ' NE '.
    REPLACE ALL OCCURRENCES OF '<=' IN r_text WITH ' LE '.
    REPLACE ALL OCCURRENCES OF '>=' IN r_text WITH ' GE '.
    REPLACE ALL OCCURRENCES OF '->' IN r_text WITH '.'.
    REPLACE ALL OCCURRENCES OF '=>' IN r_text WITH '.'.
    REPLACE ALL OCCURRENCES OF '<' IN r_text WITH '('.
    REPLACE ALL OCCURRENCES OF '>' IN r_text WITH ')'.
    " Characters that would end a mermaid node or its label
    REPLACE ALL OCCURRENCES OF '"' IN r_text WITH c_apos.
    REPLACE ALL OCCURRENCES OF '[' IN r_text WITH '('.
    REPLACE ALL OCCURRENCES OF ']' IN r_text WITH ')'.
    REPLACE ALL OCCURRENCES OF '{' IN r_text WITH '('.
    REPLACE ALL OCCURRENCES OF '}' IN r_text WITH ')'.
    REPLACE ALL OCCURRENCES OF '|' IN r_text WITH '/'.
    REPLACE ALL OCCURRENCES OF ';' IN r_text WITH ' '.
    IF strlen( r_text ) > 80.
      r_text = |{ r_text(77) }...|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
