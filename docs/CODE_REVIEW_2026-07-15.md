# Smart Debugger — Code Review (2026-07-15)

Complete pass through `src/*.clas.abap` (excluding `z_smart_debugger_standalone.prog.abap` —
legacy pre-refactor version, left untouched). This document records: what was fixed in this session,
what was discovered and fixed during review, and architectural decisions made.

---

## 1. Fixed: Tree node key synchronization (`zcl_smd_rtti_tree`)

**Symptom:** after deleting/replacing nodes, stale keys remained in `mt_vars` / `mt_classes_leaf`
tables → `get_node( )` / `add_node( related_node = ... )` calls crashed with dumps.

**Root causes and fixes:**

- `traverse_elem`: variable `o_node` was overwritten by `RECEIVING node = o_node` in `add_node`,
  so when replacing a node, the **new** node was deleted instead of the old one, leaving its key
  stale in `mt_vars`. Now the old node is saved to `o_old_node` and that is deleted.
- Introduced private method **`purge_node`**: before deleting a node, collects all subtree node keys
  via `get_subtree( )` and cleans them from `mt_vars` and `mt_classes_leaf` (deletion itself under
  `TRY`). Applied at all deletion points: `traverse_struct`, `traverse_elem`, `traverse_table`,
  `traverse_obj`, `del_variable`, `delete_node`.
- `traverse_obj`: `get_node( var-key )` wrapped in `TRY` (stale key is cleaned from tables instead
  of crashing); old `mt_vars` entry deleted before `APPEND` (was accumulating duplicates, with
  `READ TABLE` finding the dead one).
- `delete_node`: now cleans `mt_vars`/`mt_classes_leaf` by key and subtree (previously
  `mt_classes_leaf` was never cleaned except by full `clear`).

## 2. Fixed: Delta compression for table history (memory)

**Was:** every internal table change stored a full copy in `mt_vars_hist` and `mt_vars_hist_view`
→ memory grew linearly with step count.

**Now** (`zcl_smd_debugger_base`):

- `mt_vars_hist_view` holds only the **latest** full copy per variable (used only for change
  detection) — old records deleted before insert.
- In `mt_vars_hist` for standard tables, a **delta** is stored: common prefix/suffix are identified,
  only the replaced block is saved (`is_delta`, `delta_from`, `delta_del` — new fields in
  `zcl_smd_appl=>var_table`). Methods: `build_tab_delta` (construct), `restore_tab_hist` (unroll on
  replay), `hist_same_var` (variable identity in chain).
- Checkpoint: full snapshot every 20 deltas (constant `c_max_chain`). Full copy also preserved if:
  delta ≥ table size, type is not standard table, old/new types differ, or any exception occurs.
- Replay (`run_script_hist`): delta unrolled once per displayed variable (after filtering), not per
  history record; for `m_hide` branch (hide empty) — in-place because real values needed.
- Broken chain (missing base snapshot) → variable hidden, not dumped.

**Variable identity in delta chain** (`hist_same_var`) mirrors `save_hist` lookup: objects `{O:...}`
globally by name; otherwise program must match; `GLOBAL`/`CLASS` by name within program; others —
plus `stack + eventtype + eventname`. Type (`type` field) must also match.

## 3. Fixed: Same-named variables with different types

**Symptom:** same-named variables with different types in different blocks (methods, programs)
compared directly → dump on incompatible types.

- `save_hist`: lookup in `mt_vars_hist_view` includes `program` (global variables compared only
  within same program; locals only within their frame+program). Value comparison via new method
  **`hist_value_changed`**: checks actual types first (`describe_by_data_ref( )->absolute_name`),
  different types = "changed" without comparing values; same types → comparison under `TRY`.
- `mt_state`: `type` field now always updated (was only if empty; `mt_state` row reused by name,
  and same-named variable in another block kept wrong type).
- `zcl_smd_rtti_tree=>traverse_table`: `absolute_name` checked before value comparison (type not
  saved in `mt_vars` for tables before — added `<vars>-type`).
- `build_tab_delta`: different table types → full snapshot, no row-by-row comparison.

## 4. Fixed during full review (this session)

| # | File | Problem | Fix |
|---|------|---------|-----|
| 4.1 | `zcl_smd_text_viewer` | Viewing a string **corrupted the variable value**: `SHIFT <str> LEFT` executed on stored string reference — after viewing, tree/history showed truncated value ≤255 chars. Plus no `RETURN` on `cl_gui_textedit` creation error → crash on unbound `mo_text`. | Work with local copy; added `RETURN`. |
| 4.2 | `zcl_smd_table_viewer` | **Memory leak:** `on_table_close` (frees table copy and `mt_obj` entry) never registered — `on_box_close` registered twice instead. Every opened table lived in memory until session end. | `SET HANDLER on_table_close FOR mo_box` (one registration, duplicate removed). |
| 4.3 | `zcl_smd_debugger_base=>read_class_globals` | `describe_by_name` used without checking `sy-subrc`/initial ref → `refc->attributes` on unbound ref = crash if class not describable. Plus `READ TABLE mt_obj WITH KEY name = class` compared against never-filled variable — should be `prog-name`. | Guard + `TRY` on cast + `prog-name`. |
| 4.4 | `zcl_smd_source_parser=>parse_tokens` | Risk of **infinite loop**: `cx_scan_iterator_reached_end` caught but loop continued; if end-of-iterator reached before last statement (`statement_index` < `max` forever), `DO` appended same token endlessly. | `EXIT` in `CATCH`. |
| 4.5 | `zcl_smd_sel_opt=>update_sel_row` | Typo: `CLEAR: c_sel_row-low, c_sel_row-low.` — `high` not cleared. | `low, high`. |

## 5. Second wave of fixes (same date, by request)

All former "noted but not fixed" items eliminated:

| # | File | Problem | Fix |
|---|------|---------|-----|
| 5.1 | `zcl_smd_debugger_base=>traverse` | Dead branch for table "copying": `DESCRIBE FIELD` on `REF TO data` always returns `'l'`; if reachable, `GET REFERENCE OF` unassigned FS would crash. Actual copies come from `elem_clone`/`create_simple_var`. | Block replaced with `m_variable = ir_up.` and clarifying comment. |
| 5.2 | `zcl_smd_debugger_base` | `mt_new_string` grew all session (one string per string variable display), even for values not in history. | Strings created via `CREATE DATA ... TYPE string` (heap, refcounted — freed when history no longer holds ref). Attribute `mt_new_string` removed. |
| 5.3 | `show_coverage` + `mt_stack[ 1 ]` readers | COVERAGE replaced `mo_window->mt_stack` with pseudo-stack; tree and `save_hist` read `mt_stack[ 1 ]` as real stack (+ crash on empty table). | All reads switched to `ms_stack` (4 places in `zcl_smd_rtti_tree`, 3 in `save_hist`). `mt_stack` now display-only. |
| 5.4 | `zcl_smd_window=>set_program_line` | Marker 7 (current-line arrow) got all session breakpoint lines; marker 2 (breakpoints) overwritten by second `set_marker` with watch/coverage list. | Arrow uses separate table with only current line; marker 2 set once with merged list (breakpoints + watch + coverage, sorted + deduplicated). |
| 5.5 | `zcl_smd_mermaid=>magic_search` | `<if>` used without assignment check (`ENDIF`/`WHEN`/`ELSE` without recorded `IF/CASE` in filtered steps → crash); `mt_if`/`ms_if` accumulated between SMART button clicks. | `IS ASSIGNED` guards at all 3 points + `UNASSIGN` after block closes + fallback `pre_stack = line`; `CLEAR mt_if, ms_if` at method start. |
| 5.6 | `zcl_smd_sel_opt` | `on_grid_button_click` / `on_f4`: `READ ... INDEX es_row_no-row_id ASSIGNING` without `sy-subrc` check → crash on click outside data rows. | Early `RETURN` on `sy-subrc <> 0` (F4 delegates to standard help). |
| 5.7 | `zcl_smd_appl=>init_lang` | `ORDER BY` on unselected fields — fails strict syntax check on newer releases. | `ladatum`/`lauzeit` added to SELECT list (`CORRESPONDING FIELDS` ignores them). |
| 5.8 | `zcl_smd_popup=>on_box_close` | Child popup cleanup commented out: children hung around, `mt_popups` grew unbounded. | Child windows closed (`free` with `EXCEPTIONS`, standard GUI exceptions don't dump), entries deleted (`child = sender OR child IS INITIAL`). `on_table_close` viewer additionally cleans its `mt_popups` entry. |

Out of scope: **`z_smart_debugger_standalone.prog.abap`** — legacy pre-refactor version,
all listed bugs present in it too (not modified by agreement).

## 6. Feature: Auto-refresh for open table windows

Open table windows (`zcl_smd_table_viewer`) previously showed a snapshot from open time — navigating
required closing and reopening.

Implemented via event:

- `zcl_smd_window`: event **`navigated`** + method `raise_navigated`. Raised after every state
  display: `show_step` (live step), history branch in `hnd_toolbar`, tree REFRESH button.
- `zcl_smd_table_viewer`: subscribes in constructor (`on_navigated`), unsubscribes in `on_table_close`.
  On event, finds current reference of its variable (by `m_additional_name` = fullname) in `mt_vars`
  of three trees, then `mt_state`; if found and is table, calls `update_table` with change detection:
  (1) same snapshot object? skip; (2) different type? treat as changed; (3) same type, different
  content? full rebind ALV (survives variable type change between blocks, re-applies select-options
  filter, refreshes panel); (4) unchanged content? skip.
- Not refreshed (intentional): drill-down windows (`NAME[ n ]-COMP`), service windows HISTORY/Steps —
  no `mt_vars` entries for these; they keep snapshots.
- Entire `update_table` under `TRY` — navigation never crashes due to open window.

## 7. Verification scenarios

1. **Tree:** variable (struct/table) changes type between blocks (same name) — step through both,
   then navigate history back/forth. Before: crash on `get_node`/compare. Now: node replaced.
2. **Deltas:** table growing in loop (APPEND each step), >20 steps. Back/forward history — values match
   live run; memory doesn't grow linearly with table size.
3. **Same-named variables:** `FORM A` / `FORM B` with local `lt_data` of different types + global with
   same name in called program.
4. **Popup leak:** open/close many tables via double-click — `mt_obj` and `mt_popups` don't grow
   (check in debugger-of-debugger, DEBUG button).
5. **Text viewer:** double-click string variable >255 chars, close, reopen — value intact.
6. **Editor markers:** set session breakpoint, hit SMART/COVERAGE — breakpoint mark stays; only one
   arrow for current line.
7. **COVERAGE:** after COVERAGE button, step — `mt_vars`/`mt_state` records get real stacklevel
   (from `ms_stack`), tree builds correctly.
8. **Mermaid SMART:** run twice in row, on steps starting inside IF/CASE block — no crashes,
   diagram builds.
9. **Table popup auto-refresh:** open table via double-click, step forward/backward through history —
   popup content updates with each step (table growing in loop case included); select-options filter
   persists; close popup — navigation continues without error.
