CLASS zcl_smd_markdown_html DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS to_html
      IMPORTING
        !i_markdown    TYPE string
      RETURNING
        VALUE(rv_html) TYPE string.

  PRIVATE SECTION.
    CLASS-METHODS escape_html
      IMPORTING
        !i_text        TYPE string
      RETURNING
        VALUE(rv_html) TYPE string.

    CLASS-METHODS inline_markdown
      IMPORTING
        !i_text        TYPE string
      RETURNING
        VALUE(rv_html) TYPE string.
ENDCLASS.

CLASS zcl_smd_markdown_html IMPLEMENTATION.

  METHOD to_html.

    DATA lt_lines TYPE STANDARD TABLE OF string.
    DATA lv_line TYPE string.
    DATA lv_trim TYPE string.
    DATA lv_text TYPE string.
    DATA lv_cr TYPE c LENGTH 1.
    DATA lv_prefix2 TYPE c LENGTH 2.
    DATA lv_prefix3 TYPE c LENGTH 3.
    DATA lv_prefix4 TYPE c LENGTH 4.
    DATA lv_prefix5 TYPE c LENGTH 5.
    DATA lv_in_code TYPE abap_bool.

    SPLIT i_markdown AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.
    lv_cr = cl_abap_char_utilities=>cr_lf.

    rv_html =
      `<html><head><meta http-equiv="content-type" content="text/html; charset=utf-8">` &&
      `<style>` &&
      `body{font-family:Segoe UI,Arial,sans-serif;font-size:13px;line-height:1.35;margin:8px;color:#111827;background:#ffffff;}` &&
      `h1,h2,h3,h4{margin:10px 0 6px 0;font-weight:650;color:#111827;}` &&
      `h1{font-size:18px;}h2{font-size:16px;}h3{font-size:15px;}h4{font-size:14px;}` &&
      `p{margin:5px 0 8px 0;}` &&
      `.li{margin:3px 0 3px 14px;text-indent:-12px;}` &&
      `.num{margin:3px 0 3px 18px;text-indent:-16px;}` &&
      `pre{font-family:Consolas,monospace;font-size:12px;line-height:1.3;margin:6px 0;padding:6px;border:1px solid #d1d5db;background:#f9fafb;white-space:pre-wrap;}` &&
      `code{font-family:Consolas,monospace;background:#f3f4f6;border:1px solid #e5e7eb;padding:0 2px;}` &&
      `strong{font-weight:700;}` &&
      `.rule{border-top:1px solid #d1d5db;margin:8px 0;}` &&
      `</style></head><body>`.

    LOOP AT lt_lines INTO lv_line.
      REPLACE ALL OCCURRENCES OF lv_cr IN lv_line WITH ``.

      lv_trim = lv_line.
      SHIFT lv_trim LEFT DELETING LEADING space.
      lv_prefix2 = lv_trim.
      lv_prefix3 = lv_trim.
      lv_prefix4 = lv_trim.
      lv_prefix5 = lv_trim.

      IF lv_trim CP '```*'.
        IF lv_in_code = abap_true.
          rv_html = rv_html && `</pre>`.
          lv_in_code = abap_false.
        ELSE.
          rv_html = rv_html && `<pre>`.
          lv_in_code = abap_true.
        ENDIF.
        CONTINUE.
      ENDIF.

      IF lv_in_code = abap_true.
        rv_html = rv_html && escape_html( lv_line ) && cl_abap_char_utilities=>newline.
        CONTINUE.
      ENDIF.

      IF lv_trim IS INITIAL.
        CONTINUE.
      ENDIF.

      IF lv_trim = '---' OR lv_trim = '***'.
        rv_html = rv_html && `<div class="rule"></div>`.
      ELSEIF lv_prefix5 = '#### '.
        lv_text = lv_trim+5.
        rv_html = rv_html && `<h4>` && inline_markdown( lv_text ) && `</h4>`.
      ELSEIF lv_prefix4 = '### '.
        lv_text = lv_trim+4.
        rv_html = rv_html && `<h3>` && inline_markdown( lv_text ) && `</h3>`.
      ELSEIF lv_prefix3 = '## '.
        lv_text = lv_trim+3.
        rv_html = rv_html && `<h2>` && inline_markdown( lv_text ) && `</h2>`.
      ELSEIF lv_prefix2 = '# '.
        lv_text = lv_trim+2.
        rv_html = rv_html && `<h1>` && inline_markdown( lv_text ) && `</h1>`.
      ELSEIF lv_prefix2 = '- ' OR lv_prefix2 = '* '.
        lv_text = lv_trim+2.
        rv_html = rv_html && `<div class="li">&bull; ` && inline_markdown( lv_text ) && `</div>`.
      ELSEIF strlen( lv_trim ) >= 3
          AND lv_prefix3+0(1) CO '0123456789'
          AND lv_prefix3+1(2) = '. '.
        lv_text = lv_trim+3.
        rv_html = rv_html && `<div class="num">` && lv_prefix3 && inline_markdown( lv_text ) && `</div>`.
      ELSEIF strlen( lv_trim ) >= 4
          AND lv_prefix4+0(1) CO '0123456789'
          AND lv_prefix4+1(1) CO '0123456789'
          AND lv_prefix4+2(2) = '. '.
        lv_text = lv_trim+4.
        rv_html = rv_html && `<div class="num">` && lv_prefix4 && inline_markdown( lv_text ) && `</div>`.
      ELSE.
        rv_html = rv_html && `<p>` && inline_markdown( lv_trim ) && `</p>`.
      ENDIF.
    ENDLOOP.

    IF lv_in_code = abap_true.
      rv_html = rv_html && `</pre>`.
    ENDIF.

    rv_html = rv_html && `</body></html>`.

  ENDMETHOD.

  METHOD escape_html.

    rv_html = i_text.
    REPLACE ALL OCCURRENCES OF `&` IN rv_html WITH `&amp;`.
    REPLACE ALL OCCURRENCES OF `<` IN rv_html WITH `&lt;`.
    REPLACE ALL OCCURRENCES OF `>` IN rv_html WITH `&gt;`.
    REPLACE ALL OCCURRENCES OF `"` IN rv_html WITH `&quot;`.

  ENDMETHOD.

  METHOD inline_markdown.

    DATA lv_tag TYPE string.
    DATA lv_open TYPE abap_bool.

    rv_html = escape_html( i_text ).

    lv_open = abap_true.
    WHILE rv_html CS '**'.
      IF lv_open = abap_true.
        lv_tag = `<strong>`.
        lv_open = abap_false.
      ELSE.
        lv_tag = `</strong>`.
        lv_open = abap_true.
      ENDIF.
      REPLACE FIRST OCCURRENCE OF '**' IN rv_html WITH lv_tag.
    ENDWHILE.

    IF lv_open = abap_false.
      rv_html = rv_html && `</strong>`.
    ENDIF.

    lv_open = abap_true.
    WHILE rv_html CS '`'.
      IF lv_open = abap_true.
        lv_tag = `<code>`.
        lv_open = abap_false.
      ELSE.
        lv_tag = `</code>`.
        lv_open = abap_true.
      ENDIF.
      REPLACE FIRST OCCURRENCE OF '`' IN rv_html WITH lv_tag.
    ENDWHILE.

    IF lv_open = abap_false.
      rv_html = rv_html && `</code>`.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
