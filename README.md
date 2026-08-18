# Smart Debugger

**Project ARIADNA** — *Advanced Reverse Engineering ABAP Debugger with New Analytics*
Version: beta 0.9.600 · SAP GUI debugger script · no modification of the inspected code

<img width="621" height="616" alt="Smart Debugger" src="https://github.com/user-attachments/assets/dd3108d6-a3ee-4a2e-9a0b-66b91cb03e9b" />

Smart Debugger is a **debugger script** for the SAP GUI New Debugger. It opens its own multi-window
workspace that shows *all* variables, structures, internal tables and object instances of the current
program at once — and, while you run, it records every step and every variable change, so you can also
step **backwards** through the execution you already passed.

---

## Why you need it

The standard ABAP debugger shows you one variable at a time, in the present moment only. Smart Debugger
gives you the whole picture and the whole timeline.

| Everyday pain in the standard debugger | What Smart Debugger does |
|---|---|
| You add variables to the watch list one by one | All locals, globals, parameters, `SY`, `CLASS-DATA` and LDB variables are already there, in hierarchical trees |
| Deep structures and object instances take endless clicking | Full tree drill-down, objects split into public / private / protected attributes |
| Only one internal table on screen at a time | Double-click any table → its own ALV window. **No limit** — open ten tables side by side |
| You stepped one line too far → restart the whole transaction | **Step back.** History of every step and every value change is recorded |
| "Where did this value come from?" | Double-click a variable = watchpoint; jump to the previous/next stop condition, forward or backward |
| Screen full of empty and irrelevant variables | `Initials` hides empty values; `Locals` / `Globals` / `SYST` / `CLASS-DATA` / `LDB` toggle whole groups |
| No idea of the call flow you just executed | Mermaid **call-flow diagram**, **live calls graph**, **control-structure scheme** of the include |
| "Which branches actually ran?" | **Coverage** highlights every executed line, on every stack level |
| Installing tooling into a customer system | Nothing to install: paste one script into the debugger. abapGit install is optional |

**Bottom line:** fewer debug restarts, fewer "let me run it again with another breakpoint" cycles.
Time is money.

---

## Table of contents

- [Why you need it](#why-you-need-it)
- [Requirements](#requirements)
- [Installation](#installation)
  - [Option A — copy & paste (nothing to install)](#option-a--copy--paste-nothing-to-install)
  - [Option B — abapGit (modular classes)](#option-b--abapgit-modular-classes)
- [Quick start](#quick-start)
- [Window layout](#window-layout)
- [Toolbar reference](#toolbar-reference)
- [Variable trees](#variable-trees)
- [Tables, strings and popups](#tables-strings-and-popups)
- [Watchpoints](#watchpoints)
- [Time machine: backward debugging](#time-machine-backward-debugging)
- [Diagrams (Mermaid)](#diagrams-mermaid)
- [Coverage](#coverage)
- [AI assistant (optional)](#ai-assistant-optional)
- [Performance tips](#performance-tips)
- [Repository layout](#repository-layout)
- [For developers](#for-developers)
- [Links and credits](#links-and-credits)
- [License](#license)

---

## Requirements

| | |
|---|---|
| **SAP GUI** | Required. Smart Debugger is built on `CL_GUI_*` controls and runs inside the New Debugger, not in ADT/Eclipse |
| **Release** | ABAP 7.40+ (inline declarations, `VALUE`/`COND`, string templates). The generated standalone file is additionally patched for 7.50 |
| **Authorization** | Standard debugging authorization (`S_DEVELOP` with activity `DEBUG`) |
| **abapMermaid** | *Optional.* Needed only for the diagram buttons. Use the scroll-enabled fork: [ysichov/abapMermaid](https://github.com/ysichov/abapMermaid) (original: [WegnerDan/abapMermaid](https://github.com/WegnerDan/abapMermaid)). Without it the `Diagram` / `Live Debug` buttons are simply not shown |

---

## Installation

### Option A — copy & paste (nothing to install)

Take **[`src/z_smart_debugger_standalone.prog.abap`](src/z_smart_debugger_standalone.prog.abap)** —
one self-contained file (~9 000 lines) with every class merged in — and paste it into the debugger
`Script` tab. Nothing is created in the system, nothing is transported.

> The AI panel is **not** part of the standalone build (it depends on a separate repository).
> Everything else is identical to the modular version.

### Option B — abapGit (modular classes)

Clone this repository with [abapGit](https://abapgit.org) into any package. You get the
`ZCL_SMD_*` classes plus two programs:

| Object | Purpose |
|---|---|
| `Z_SMART_DEBUGGER` | The thin debugger script that instantiates the classes — paste **this** one in the `Script` tab |
| `Z_SMART_DEBUGGER_STANDALONE` | The merged single-file variant (generated, do not edit) |

Use this option if you want the AI panel, or if several developers should share one installed version.

---

## Quick start

1. Set break-points anywhere in the ABAP code you want to inspect and start the transaction/report.
2. When a break-point triggers, switch to the last tab of the New Debugger — **`Script`**.
3. Paste the program source (see [Installation](#installation)), or press **Load script** if you saved
   it before. *Tip: paste once, press **Save script**, and afterwards just load it by name.*
4. Select the radio button **Execute directly** and press **Start Script**.

<img width="1526" height="823" alt="Script tab" src="https://github.com/user-attachments/assets/54e99fad-be8c-4c6b-aad9-989ab141fbe9" />

---

## Window layout

<img width="1749" height="900" alt="Main window" src="https://github.com/user-attachments/assets/2d6c79e4-9473-4d2c-a290-85f7bc5cfeb3" />

| Area | Content |
|---|---|
| **Top** | Toolbar (see below) |
| **Center** | Source code viewer of the current include. Click the left border of a line to set/delete a **session** break-point, `Ctrl` + click for an **external** one |
| **Right, upper** | `Variables` — locals, globals, `SY`, class data, LDB |
| **Right, lower left** | `Importing parameters` |
| **Right, lower right** | `Exporting & Returning parameters` |
| **Bottom** | Call stack / execution history ALV |
| **Left** | AI panel — hidden until the `AI` button is pressed |

Importing and exporting/returning parameters live in their own panels on purpose: when you debug a
method or function module, that is exactly the split you care about.

<img width="1748" height="827" alt="Import and export parameters" src="https://github.com/user-attachments/assets/1263a948-46a6-4017-a898-e61f94d79cc0" />

---

## Toolbar reference

Buttons that carry a state show the **current** state in their label (pressing `History On` turns
recording off and the label becomes `History OFF`).

### Run control

| Button | Meaning |
|---|---|
| `Step into` (F5) | One step ahead |
| `Step over` (F6) | One operation ahead |
| `Step out` (F7) | Leave the current stack level |
| `Continue` (F8) | To the next break-point or watchpoint |
| `Forward` / `Backward` | Direction switch. In `Backward` mode F6/F7 disappear, F5 becomes **Step back** and F8 becomes **to the previous stop condition** |

<img width="1025" height="40" alt="Forward buttons" src="https://github.com/user-attachments/assets/f6e2cce9-125d-43ed-8690-fd2cb2faf3b5" />
<img width="1214" height="43" alt="Backward buttons" src="https://github.com/user-attachments/assets/ec84be4b-fdf6-4000-a199-37c8b3ce300f" />

### Recording

| Button | Meaning | Default |
|---|---|---|
| `History On/OFF` | Record the step/stack history | **On** |
| `Vars History On/OFF` | Record every variable change | **On** |
| `Depth N` | How many stack levels below the current one are recorded, 0…9. `Depth 0` = current level only; `Depth 1` = current level plus the forms/methods/functions it calls; and so on. Click cycles the value | **9** |
| `Only Z` / `Z & Standard` | Record Z code only, or standard SAP code as well | **Only Z** |
| `Alpha` / `Beta` | Analysis engine. **Alpha** parses the executed statement and refreshes variables only after value-changing operations — much faster, may skip exotic changes. **Beta** re-reads everything on every step — slower, more precise | **Alpha** |

### Analysis

| Button | Meaning |
|---|---|
| `Diagram` | Mermaid diagram of all calls performed so far (needs abapMermaid) |
| `Live Debug` | Separate window with its own F5/F6/F7/F8 and a calls graph that redraws as you run (needs abapMermaid) |
| `Calculations Flow` | Mermaid scheme of the control structure of the current include: `IF/ELSEIF/ELSE`, `CASE/WHEN`, `LOOP/DO/WHILE`, `TRY/CATCH`, with plain statements collapsed into "N operations" nodes |
| `Coverage` | Paint every executed line of the current include; double-click a stack row to switch include |
| `Steps` | Open the recorded step table as an ALV |
| `History` | Open the recorded variable-change history as an ALV |

### Display and utilities

| Button | Meaning |
|---|---|
| `Visualization OFF/ON` | When ON, the source view follows execution line by line during long runs (light refresh only — variables and trees are not rebuilt on every step) |
| `Clear vars` | Remove all selected variables / watchpoints |
| `AI`, `AI Log` | See [AI assistant](#ai-assistant-optional). Hidden when no AI configuration is present |
| `Debug` | Activate the internal `BREAK-POINT`s of the debugger script itself — for developing Smart Debugger, not for debugging your program |
| `Info` (globe icon) | Opens the documentation links in a browser |

---

## Variable trees

Every variable is a tree node: structures, deep structures and object instances can be expanded down
to the leaf. Object attributes are grouped and colour-coded — 🟢 public, 🔴 private, 🟡 protected.

Tables with header lines and tables declared with `OCCURS` appear as `variable_name[]` next to their
header structure variable.

Functions available in the `Variables` tree toolbar:

| Function | Meaning |
|---|---|
| `Initials` | Show/hide variables with initial (empty) values — on/off switch |
| `Locals` | Show/hide local variables |
| `Globals` | Show/hide global variables |
| `SYST` | Show/hide the `SY` structure (`sy-datum`, `sy-uname`, …) |
| `CLASS-DATA` | Show/hide static (global) attributes of the active classes |
| `LDB` | Show/hide Logical Database globals — hidden by default, because in the standard debugger this noise cannot be switched off |
| Refresh | Re-read the current step and rebuild the trees |

---

## Tables, strings and popups

**Double-click** on a variable:

- an **internal table** opens in its own ALV window — as many windows as you like, side by side;
- a **string** opens in a text viewer window.

<img width="1686" height="654" alt="Tables in separate windows" src="https://github.com/user-attachments/assets/0f3bebe8-14cc-42ac-9c21-751ce5cbfcfd" />

Each table window offers:

| Function | Meaning |
|---|---|
| Select-Options panel | Filter the table with real select-options, per column |
| `Tech names` | Toggle technical field names / descriptions |
| Language buttons | Re-translate column headers into another logon language |
| `Show empty columns` | Show or hide columns that contain no data |
| Standard ALV functions | Toggle the standard ALV toolbar (sort, filter, layout, export) |

Open table windows **follow navigation**: when you step forward or backward, they refresh themselves
with the table content of the step you are standing on.

---

## Watchpoints

Creating a watchpoint is a **double-click** on the variable — it is highlighted and added to the
selected-variables table. Double-click again removes it. `Clear vars` removes all of them.

Watchpoints are what `Continue` (F8) and `to the previous stop condition` stop at — in both directions.
A selected variable also drags in its dependants: values written into it by called forms, methods and
functions are tracked as well.

<img width="906" height="438" alt="Watchpoints" src="https://github.com/user-attachments/assets/7731bab7-49b2-4133-b3b5-63cca1561d4b" />

<img width="1676" height="904" alt="Calculations flow" src="https://github.com/user-attachments/assets/e19e609a-dfe2-402b-beaf-4b83e86d9eb7" />

---

## Time machine: backward debugging

With `History On` and `Vars History On` (both default) Smart Debugger stores every step and every
variable change while the program runs. Switch the direction button to **Backward** and walk into the
past: `Step back` for one step, `to the previous stop condition` for the previous break-point or
watchpoint. Switch back to **Forward** and continue as usual.

The recorded data is what feeds the `Steps` and `History` tables, the `Coverage` view and the diagrams.

Internally, table snapshots are stored as **deltas** with a full snapshot every 20 changes, so a long
run with large internal tables stays affordable in memory. What is recorded is controlled by
`Depth N` and `Only Z` / `Z & Standard`.

---

## Diagrams (Mermaid)

All three diagram views need [abapMermaid](https://github.com/ysichov/abapMermaid) (scroll-enabled fork).
If the class is not installed, the diagram buttons are hidden.

| View | What it draws |
|---|---|
| `Diagram` | *Calls flow* — every call performed during the recorded execution |
| `Live Debug` | *Live calls graph* — same graph, but in its own window with its own step buttons, redrawn whenever the current frame changes |
| `Calculations Flow` | *Control-structure scheme* of the include on display, built from the ABAP scanner: branches, loops and `TRY` blocks in their real nesting |

<img width="1855" height="851" alt="Mermaid calls diagram" src="https://github.com/user-attachments/assets/e2e3b4a7-988f-4e66-8068-65383f63aed4" />

---

## Coverage

`Coverage` paints every line of the current include that was actually executed. The bottom ALV switches
to the list of includes that were touched — double-click a row to see the coverage of that include.
So you get code coverage across all stack levels, from a normal debugging session.

<img width="1752" height="894" alt="Coverage" src="https://github.com/user-attachments/assets/2490c4f5-f24e-47e5-adcd-ea5558b62372" />

---

## AI assistant (optional)

The `AI` button opens a panel where an LLM analyses the current step, the stack, the variables and the
change history and proposes the most likely cause of the bug. It can set its own guard break-point and
drive the debugger, and every call and action is written to a log (`AI Log`).

- Requires the separate **ABAP-AI-Code** project, which supplies the LLM client and the tool factory,
  and writes the provider/model/API-key configuration that Smart Debugger reads at start-up.
- Without that configuration the `AI` button is **not displayed** at all.
- The tool contract used by the agent is documented in [`TOOLS/set_breakpoint.md`](TOOLS/set_breakpoint.md).
- The generated standalone file contains **no** AI support.

This part is the youngest and least finished piece of the project.

---

## Performance tips

Recording everything on every step costs time. When a run feels slow:

1. Lower **`Depth`** — `Depth 0`…`Depth 2` is usually enough, `Depth 9` records nine stack levels below you.
2. Keep **`Only Z`** — `Z & Standard` records standard SAP code as well and multiplies the volume.
3. Stay on **`Alpha`** — switch to `Beta` only when you suspect a change was missed.
4. Switch **`Vars History OFF`** when you only need the call flow, not the values.
5. Use **`Visualization`** deliberately: it is a light per-step source refresh, useful to *watch*
   execution, but it still costs a frontend round trip per step.

---

## Repository layout

```
src/
  z_smart_debugger.prog.abap             thin debugger script (entry point)
  z_smart_debugger_standalone.prog.abap  generated single-file build - do not edit
  zcl_smd_debugger_base.clas.abap        step engine, history, watchpoints
  zcl_smd_window.clas.abap               main window, toolbar, source viewer
  zcl_smd_rtti_tree.clas.abap            variable trees (RTTI based)
  zcl_smd_table_viewer.clas.abap         table popups + select-options
  zcl_smd_sel_opt.clas.abap              select-options control
  zcl_smd_source_parser.clas.abap        statement/keyword scanner
  zcl_smd_code_scheme.clas.abap          control-structure scheme (Calculations Flow)
  zcl_smd_mermaid.clas.abap              diagram windows
  zcl_smd_ai_agent.clas.abap             AI agent (modular install only)
  zcl_smd_*.clas.abap                    text viewer, popups, RTTI, HTML, helpers
TOOLS/                                   AI tool contracts
docs/                                    code review and analysis documents
generate_standalone.sh / .bat            standalone build
```

## For developers

- [`DEVELOPMENT.md`](DEVELOPMENT.md) — architecture invariants, coding conventions, common patterns
- [`STANDALONE.md`](STANDALONE.md) — how the single-file build is generated
- [`docs/CODE_REVIEW_2026-07-15.md`](docs/CODE_REVIEW_2026-07-15.md) — full audit with verification scenarios

Edit the `ZCL_SMD_*` classes only; `z_smart_debugger_standalone.prog.abap` is regenerated from them.

## Links and credits

Written by **Yurii Sychov** — [blog](https://ysychov.wordpress.com/blog/) ·
[LinkedIn](https://www.linkedin.com/in/ysychov/) · ysichov@gmail.com

- RU description: <https://ysychov.wordpress.com/2020/07/27/abap-simple-debugger-data-explorer/>
- Predecessor project SDDE: <https://github.com/ysichov/SDDE> ([wiki](https://github.com/ysichov/SDDE/wiki))

External resources and inspiration:

- [abapMermaid](https://github.com/WegnerDan/abapMermaid) / [scroll-enabled fork](https://github.com/ysichov/abapMermaid)
- [Data object visualizer](https://gist.github.com/AtomKrieg/7f4ec2e2f49b82def162e85904b7e25b) by AtomKrieg
- [ABAP-Object-Visualizer](https://github.com/larshp/ABAP-Object-Visualizer) by larshp
- [Simple Data Explorer](https://github.com/ysichov/SDE_abapgit)
- <https://habr.com/ru/articles/504908/>

Example used in the screenshots — iterator pattern:
<http://zevolving.com/2012/01/iterator-design-pattern-to-access-linked-list/>

<img width="1743" height="881" alt="Iterators example" src="https://github.com/user-attachments/assets/02a0666b-25bd-4904-ad2b-b648d0f08daf" />

## License

[MIT](LICENSE)
