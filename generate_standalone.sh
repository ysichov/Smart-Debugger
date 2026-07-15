#!/usr/bin/env bash
set -e

SD_SRC="/c/soft/GitHub/Smart-Debugger/src"
TARGET_SRC="/c/soft/GitHub/src"
TARGET_DIR="/c/soft/GitHub"

echo "Copying Smart Debugger class/interface files..."
rm -rf "$TARGET_SRC"
mkdir -p "$TARGET_SRC"
cp "$SD_SRC"/*.clas.abap "$TARGET_SRC/" 2>/dev/null || true
cp "$SD_SRC"/*.intf.abap "$TARGET_SRC/" 2>/dev/null || true

echo "Removing AI-agent classes (they pull in the whole separate ABAP-AI-Code"
echo "repo, which is not usable in a standalone build anyway)..."
# zcl_smd_ai_agent / zcl_smd_aitool_breakpoint depend on ~40 classes in
# ABAP-AI-Code (zcl_aitool_base, zcl_abapai_llm_client, ...). Merging that
# whole repo in just for a debugger-script AI panel isn't worth the extra
# cross-repo class-ordering risk. zcl_smd_window's AI wiring is stripped
# separately below (strip_ai_support.py) so it still compiles without them.
rm -f "$TARGET_SRC/zcl_smd_ai_agent.clas.abap" "$TARGET_SRC/zcl_smd_aitool_breakpoint.clas.abap"

echo "Stripping AI wiring from the working copy of zcl_smd_window..."
python "$SD_SRC/../strip_ai_support.py" "$TARGET_SRC/zcl_smd_window.clas.abap"

echo "Removing CLEAR_LINE_MARKERS call (cl_gui_abapedit on 7.50 doesn't have it,"
echo "only CREATE_MARKER) - only the standalone working copy is touched."
sed -i "/mo_code_viewer->clear_line_markers( 'S' )\./d" "$TARGET_SRC/zcl_smd_window.clas.abap"

echo "Preparing entrypoint..."
# abapmerge derives the "main report" name from the entrypoint filename
# (everything before the first dot) and only injects classes once it finds a
# matching "REPORT <name>." line in the file. z_smart_debugger.prog.abap is a
# SAP GUI debugger-script container whose actual REPORT statement is
# "z_smart_debugger_script", not "z_smart_debugger" - so the working copy must
# be renamed to match, otherwise abapmerge silently passes the file through
# unmerged (no error, just no classes injected).
cp "$SD_SRC/z_smart_debugger.prog.abap" "$TARGET_SRC/z_smart_debugger_script.prog.abap"

echo "Running abapmerge..."
cd "$TARGET_DIR"
abapmerge -f src/z_smart_debugger_script.prog.abap -o z_smart_debugger_standalone.prog.abap

echo "Copying result back to Smart-Debugger/src..."
cp "$TARGET_DIR/z_smart_debugger_standalone.prog.abap" "$SD_SRC/z_smart_debugger_standalone.prog.abap"

echo "Fixing cross-class reference ordering (abapmerge only orders by INHERITING FROM)..."
python "$SD_SRC/../fix_class_ref_order.py" "$SD_SRC/z_smart_debugger_standalone.prog.abap"

echo "Restoring header comments..."
# Extract comment/blank lines from z_smart_debugger.prog.abap after line 1, stop at first code line
header=$(awk 'NR==1{next} /^[[:space:]]*($|")/{print; next} {exit}' "$SD_SRC/z_smart_debugger.prog.abap")
# Insert header between line 1 and the rest of standalone
{ head -1 "$SD_SRC/z_smart_debugger_standalone.prog.abap"; echo "$header"; tail -n +2 "$SD_SRC/z_smart_debugger_standalone.prog.abap"; } \
  > /tmp/z_smart_debugger_standalone_fixed.abap
cp /tmp/z_smart_debugger_standalone_fixed.abap "$SD_SRC/z_smart_debugger_standalone.prog.abap"

echo "Done. Generated z_smart_debugger_standalone.prog.abap"
