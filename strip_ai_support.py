#!/usr/bin/env python3
"""
The standalone build has no use for the AI agent (it depends on the whole
ABAP-AI-Code repo - ~40 extra classes - purely so a debugger script window
can call an LLM, which isn't meaningful outside the modular online install).

Rather than merging ABAP-AI-Code at all, this strips the AI wiring out of
the *working copy* of zcl_smd_window.clas.abap used for the standalone
build only - the real repo file (with full AI support) is untouched.

Exactly two methods and one attribute in zcl_smd_window reference
zcl_smd_ai_agent:
  - attribute mo_ai_agent           TYPE REF TO zcl_smd_ai_agent
  - METHOD run_ai_agent             (drives the AI agent loop)
  - METHOD show_ai_log              (reads mo_ai_agent->get_action_log_text)
create_ai_panel only builds UI controls and never touches mo_ai_agent, so
it is left alone - the AI panel still opens, it just can't do anything.
"""
import re
import sys


def strip_ai(path):
    with open(path, encoding="utf-8") as f:
        text = f.read()

    before = text

    text = text.replace(
        "mo_ai_agent            TYPE REF TO zcl_smd_ai_agent,",
        "mo_ai_agent            TYPE REF TO object, \"AI agent stripped from standalone build",
    )

    text = re.sub(
        r"  METHOD run_ai_agent\.\n.*?\n  ENDMETHOD\.",
        "  METHOD run_ai_agent.\n"
        "    \"AI agent stripped from standalone build - see strip_ai_support.py\n"
        "  ENDMETHOD.",
        text,
        flags=re.DOTALL,
    )

    text = re.sub(
        r"  METHOD show_ai_log\.\n.*?\n  ENDMETHOD\.",
        "  METHOD show_ai_log.\n"
        "    DATA lv_log TYPE string.\n"
        "    lv_log = `# AI Log` && cl_abap_char_utilities=>newline && cl_abap_char_utilities=>newline &&\n"
        "             `AI agent is not available in the standalone build.`.\n"
        "    mo_ai_log_viewer = NEW zcl_smd_html_viewer(\n"
        "      i_markdown = lv_log\n"
        "      i_title    = 'AI Log' ).\n"
        "  ENDMETHOD.",
        text,
        flags=re.DOTALL,
    )

    if text == before:
        print("strip_ai_support: nothing matched - check method/attribute text still lines up")
        sys.exit(1)

    with open(path, "w", encoding="utf-8", newline="\n") as f:
        f.write(text)
    print("strip_ai_support: AI agent wiring stripped from working copy")


if __name__ == "__main__":
    strip_ai(sys.argv[1])
