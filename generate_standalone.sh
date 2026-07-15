#!/usr/bin/env bash
set -e

SD_SRC="/c/soft/GitHub/Smart-Debugger/src"
AI_SRC="/c/soft/GitHub/ABAP-AI-Code/src"
TARGET_SRC="/c/soft/GitHub/src"
TARGET_DIR="/c/soft/GitHub"

echo "Copying Smart Debugger class/interface files..."
rm -rf "$TARGET_SRC"
mkdir -p "$TARGET_SRC"
cp "$SD_SRC"/*.clas.abap "$TARGET_SRC/" 2>/dev/null || true
cp "$SD_SRC"/*.intf.abap "$TARGET_SRC/" 2>/dev/null || true

echo "Copying shared AI-tool class/interface files from ABAP-AI-Code..."
# zcl_smd_aitool_breakpoint inherits from zcl_aitool_base, which lives in the
# separate ABAP-AI-Code repo - only classes/interfaces are needed here, not
# that repo's own standalone .prog.abap entrypoints.
cp "$AI_SRC"/*.clas.abap "$TARGET_SRC/" 2>/dev/null || true
cp "$AI_SRC"/*.intf.abap "$TARGET_SRC/" 2>/dev/null || true

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

echo "Restoring header comments..."
# Extract comment/blank lines from z_smart_debugger.prog.abap after line 1, stop at first code line
header=$(awk 'NR==1{next} /^[[:space:]]*($|")/{print; next} {exit}' "$SD_SRC/z_smart_debugger.prog.abap")
# Insert header between line 1 and the rest of standalone
{ head -1 "$SD_SRC/z_smart_debugger_standalone.prog.abap"; echo "$header"; tail -n +2 "$SD_SRC/z_smart_debugger_standalone.prog.abap"; } \
  > /tmp/z_smart_debugger_standalone_fixed.abap
cp /tmp/z_smart_debugger_standalone_fixed.abap "$SD_SRC/z_smart_debugger_standalone.prog.abap"

echo "Done. Generated z_smart_debugger_standalone.prog.abap"
