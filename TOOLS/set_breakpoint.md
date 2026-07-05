### set_breakpoint

Sets, deletes, or toggles an ABAP debugger breakpoint.

Use this tool only when a breakpoint directly supports the user's debugging
goal. Always state the intent in the `reason` argument: what condition, branch,
statement, or exit point you want to catch and why.

Arguments:
- include: ABAP include/program containing the source line.
- line: 1-based source line number.
- program: main program/class pool/report. If omitted, include is used.
- breakpoint_type: `S` for session breakpoint, `E` for external breakpoint.
  Use `S` unless the user explicitly needs an external breakpoint.
- mode: `set`, `delete`, or `toggle`. Use `set` by default.
- reason: short explanation visible to the user before/after execution.

Safety:
- Do not scatter speculative breakpoints. Prefer one precise breakpoint.
- For AI-driven debugger control, set a guard breakpoint near the end of the
  current program/include before using continue/F8, so the debug session does
  not leave the inspected program accidentally.
