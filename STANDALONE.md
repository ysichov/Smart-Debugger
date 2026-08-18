# Standalone Generation Guide

## Overview

Smart Debugger exists in two forms:

| Form | Files | Usage | Maintenance |
|------|-------|-------|-------------|
| **Modular** | `zcl_smd_*.clas.abap` | Development, testing, version control | Primary — edit these |
| **Standalone** | `z_smart_debugger_standalone.prog.abap` | Single-file deployment, transport | **Generated — do not edit** |

## When to Generate

Generate a new standalone version when you:
- Merge pull requests with class changes
- Release a new version for distribution
- Need a transportable single-file artifact

**Do NOT manually edit** `z_smart_debugger_standalone.prog.abap`. Changes will be lost on the next
generation.

## Generation Process

### Windows

Double-click `generate_standalone.bat` or run:
```batch
generate_standalone.bat
```

### Linux/macOS/WSL

```bash
./generate_standalone.sh
```

Both scripts perform the same steps in platform-native shells.

### Prerequisites

Install `abapmerge` globally:
```bash
npm install -g abapmerge
```

Verify installation:
```bash
abapmerge --version
```

## What Gets Merged

The standalone program combines:
- `z_smart_debugger.prog.abap` (main entry point, renamed to `z_smart_debugger_script.prog.abap`
  during the build so abapmerge matches its `REPORT z_smart_debugger_script.` statement)
- All remaining `zcl_smd_*.clas.abap` classes
- All interfaces and helper classes

**Excluded:**
- `zcl_smd_ai_agent` and `zcl_smd_aitool_breakpoint` — they depend on ~40 classes of the separate
  ABAP-AI-Code repo. The AI wiring inside `zcl_smd_window` is stripped from the working copy by
  `strip_ai_support.py`, so the merged program still compiles. **The standalone build has no AI support.**
- `z_smart_debugger_standalone.prog.abap` itself
- Documentation files
- Test/example programs

**Also patched during the build:**
- The `clear_line_markers` call is removed — `cl_gui_abapedit` does not have it on 7.50
- Classes are reordered by cross-references (`fix_class_ref_order.py`); abapmerge only orders by
  `INHERITING FROM`

Only the working copy under `/c/soft/GitHub/src` is touched — the repository files keep full AI support.

## Output

On success, the script generates:
- `src/z_smart_debugger_standalone.prog.abap` — the merged single file

If abapmerge is not installed or bash is not available, you will see an error. Check prerequisites above.

## Verification

After generation:
1. Line count should be significantly larger than any single class
2. Header comments from `z_smart_debugger.prog.abap` should be preserved
3. All class content should be concatenated in dependency order

To verify all classes were merged, count the implementation blocks (case-insensitive — some class
files are serialized in lower case):
```bash
grep -cEi "^class +zcl_smd_[a-z_]+ +implementation" src/z_smart_debugger_standalone.prog.abap
```

Should equal the number of `src/zcl_smd_*.clas.abap` files **minus the two AI classes** that the
build excludes (currently 17).

## Troubleshooting

**"abapmerge: command not found"**
- Install via `npm install -g abapmerge`
- Or use absolute path: `/path/to/node/abapmerge`

**"bash: command not found" (Windows)**
- Install Git for Windows (includes Git Bash)
- Or use WSL: `wsl ./generate_standalone.sh`
- Or run from Git Bash terminal directly

**Merge conflict or missing dependency**
- Verify all `zcl_smd_*.clas.abap` files are present in `src/`
- Ensure class definitions have no circular dependencies
- Check that `z_smart_debugger.prog.abap` includes all required classes in REPORT statement

**Output file is smaller than expected**
- Check that abapmerge completed without errors (scroll up in terminal)
- Verify no files were removed accidentally
- Regenerate and compare line count

## Git Workflow

When committing changes:
1. Edit modular classes in feature branch
2. Run `generate_standalone.sh` locally
3. Commit both class changes **and** updated standalone file
4. Push together (ensures consistency)

Example:
```bash
git add src/zcl_smd_debugger_base.clas.abap
./generate_standalone.sh
git add src/z_smart_debugger_standalone.prog.abap
git commit -m "Feature: auto-refresh table popups"
git push
```

This keeps the standalone version in sync with classes.
