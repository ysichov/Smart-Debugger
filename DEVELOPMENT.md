# Development Guide — Smart Debugger

## Code Style & Documentation

### Language Convention

**All code comments, docstrings, and documentation must be in English.**

This applies to:
- Inline code comments
- Method/class documentation comments
- Git commit messages (recommended)
- Markdown files (.md) in `docs/`
- All analysis and review documents

This convention ensures consistency across multi-person teams and facilitates international collaboration.

## Architecture

### Key Invariants

**Tree node synchronization:** Use the `purge_node()` method (in `zcl_smd_rtti_tree`) when deleting nodes.
This method cleans stale keys from both `mt_vars` and `mt_classes_leaf` tables. Never call `node->delete()`
directly without cleanup.

**Stack level:** Always read debugger stack level from `mo_debugger->ms_stack`, not from
`mo_window->mt_stack[ 1 ]`. The window's `mt_stack` table is substituted by a pseudo-stack when
COVERAGE view is active.

**Variable identity in history:** Implemented in `hist_same_var()` (`zcl_smd_debugger_base`).
Variables are identical only if:
1. For objects `{O:...}`: globally by name
2. For globals/class data: program + name
3. For locals/parameters: program + name + stack + eventtype + eventname
4. **Type must also match** — different types = different variables (checked via `absolute_name`)

**Table history compression:** Tables stored as deltas in `mt_vars_hist`. Full snapshot every 20 deltas
(`c_max_chain` constant). Methods: `build_tab_delta` (construct), `restore_tab_hist` (unroll on replay).
See `zcl_smd_appl=>var_table` for delta fields: `is_delta`, `delta_from`, `delta_del`.

### Legacy Code

**`z_smart_debugger_standalone.prog.abap`** is a legacy pre-refactor version kept for reference only.
Do not modify it. All fixes and features are implemented in the `zcl_smd_*` classes.

## Testing & Verification

See `docs/CODE_REVIEW_2026-07-15.md` sections 7 for comprehensive verification scenarios covering:
- Tree node synchronization with type changes
- Delta compression and memory efficiency
- Same-named variables across scopes
- Popup lifecycle and memory leaks
- Text viewer string handling
- Editor markers and breakpoints
- COVERAGE pseudo-stack behavior
- Mermaid diagram generation
- Table popup auto-refresh

## Tools & Resources

- **Syntax checking:** Use ADT's SyntaxCheck feature (available via MCP abap-adt)
- **Code review:** Full audit in `docs/CODE_REVIEW_2026-07-15.md` (2026-07-15)
- **Memory audit:** `docs/CODE_REVIEW_2026-07-15.md` section 2 (delta compression) and section 4.2 (popup leak fixes)

## Common Patterns

### Event-Driven Updates

Table popups auto-refresh via event. Pattern:
1. Define event in container class: `EVENTS navigated.`
2. Raise after state change: `raise_navigated( ).`
3. Subscribe in constructor: `SET HANDLER on_event FOR container.`
4. Unsubscribe on cleanup: `SET HANDLER on_event FOR container ACTIVATION space.`

See `zcl_smd_table_viewer=>on_navigated` for example.

### Safe Type Comparison

Always compare via `describe_by_data_ref( )->absolute_name`, not value comparison. Pattern:
```abap
DATA o_old_descr TYPE REF TO cl_abap_typedescr.
DATA o_new_descr TYPE REF TO cl_abap_typedescr.
o_old_descr = cl_abap_typedescr=>describe_by_data_ref( mr_old_ref ).
o_new_descr = cl_abap_typedescr=>describe_by_data_ref( mr_new_ref ).
IF o_old_descr->absolute_name <> o_new_descr->absolute_name.
  "types differ - treat as changed
ELSE.
  "safe to compare values
ENDIF.
```

## Recent Fixes (2026-07-15)

Two waves of fixes addressing 13 issues:
1. **First wave:** Node key cleanup, delta compression, type-safe comparisons
2. **Second wave:** Dead code removal, memory leaks, unguarded access, SQL syntax

All fixed issues documented with root causes and verification scenarios in
`docs/CODE_REVIEW_2026-07-15.md`.
