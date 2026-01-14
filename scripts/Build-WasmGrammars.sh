#!/bin/bash
# Build all Tier 3 WASM grammars from source

# Don't exit on errors - we want to continue building other grammars
# set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
OUTPUT_DIR="$SCRIPT_DIR/../native/uast_core/grammars-wasm"
# Use Windows-compatible temp path
TEMP_DIR="$HOME/wasm-build-temp"

mkdir -p "$OUTPUT_DIR"
rm -rf "$TEMP_DIR" 2>/dev/null || true
mkdir -p "$TEMP_DIR"

echo "Temp dir: $TEMP_DIR"

# Tier 3 grammars - repo|subdir format (using | as delimiter to avoid URL colon issues)
declare -A GRAMMARS=(
    ["verilog"]="https://github.com/tree-sitter/tree-sitter-verilog"
    ["latex"]="https://github.com/latex-lsp/tree-sitter-latex"
    ["sql"]="https://github.com/DerekStride/tree-sitter-sql"
    ["fortran"]="https://github.com/stadelmanma/tree-sitter-fortran"
    ["fsharp"]="https://github.com/ionide/tree-sitter-fsharp|fsharp"
    ["kotlin"]="https://github.com/fwcd/tree-sitter-kotlin"
    ["cobol"]="https://github.com/yutaro-sakamoto/tree-sitter-cobol"
    ["scala"]="https://github.com/tree-sitter/tree-sitter-scala"
    ["objc"]="https://github.com/tree-sitter-grammars/tree-sitter-objc"
    ["julia"]="https://github.com/tree-sitter/tree-sitter-julia"
    ["d"]="https://github.com/gdamore/tree-sitter-d"
    ["crystal"]="https://github.com/keidax/tree-sitter-crystal"
    ["cuda"]="https://github.com/tree-sitter-grammars/tree-sitter-cuda"
    ["haskell"]="https://github.com/tree-sitter/tree-sitter-haskell"
    ["swift"]="https://github.com/alex-pinkus/tree-sitter-swift"
    ["perl"]="https://github.com/tree-sitter-perl/tree-sitter-perl"
    ["arduino"]="https://github.com/tree-sitter-grammars/tree-sitter-arduino"
    ["agda"]="https://github.com/tree-sitter/tree-sitter-agda"
    ["ocaml"]="https://github.com/tree-sitter/tree-sitter-ocaml|grammars/ocaml"
    ["apex"]="https://github.com/aheber/tree-sitter-sfapex|apex"
    ["dart"]="https://github.com/UserNobody14/tree-sitter-dart"
    ["groovy"]="https://github.com/murtaza64/tree-sitter-groovy"
    ["commonlisp"]="https://github.com/theHamsta/tree-sitter-commonlisp"
    ["zig"]="https://github.com/tree-sitter-grammars/tree-sitter-zig"
    ["awk"]="https://github.com/Beaglefoot/tree-sitter-awk"
    ["vim"]="https://github.com/tree-sitter-grammars/tree-sitter-vim"
    ["r"]="https://github.com/r-lib/tree-sitter-r"
    ["bitbake"]="https://github.com/tree-sitter-grammars/tree-sitter-bitbake"
    ["ada"]="https://github.com/briot/tree-sitter-ada"
    ["cairo"]="https://github.com/tree-sitter-grammars/tree-sitter-cairo"
    ["dhall"]="https://github.com/jbellerb/tree-sitter-dhall"
    ["cue"]="https://github.com/eonpatapon/tree-sitter-cue"
    ["doxygen"]="https://github.com/tree-sitter-grammars/tree-sitter-doxygen"
    ["comment"]="https://github.com/stsewd/tree-sitter-comment"
)

SUCCEEDED=0
FAILED=0
FAILED_LIST=""

echo "=============================================="
echo "Building ${#GRAMMARS[@]} Tier 3 WASM grammars"
echo "Output: $OUTPUT_DIR"
echo "=============================================="

for name in "${!GRAMMARS[@]}"; do
    SPEC="${GRAMMARS[$name]}"

    # Parse repo and subdir (using | as delimiter)
    if [[ "$SPEC" == *"|"* ]]; then
        REPO="${SPEC%%|*}"
        SUBDIR="${SPEC#*|}"
    else
        REPO="$SPEC"
        SUBDIR=""
    fi

    echo ""

    # Skip if already built
    if [ -f "$OUTPUT_DIR/${name}.wasm" ]; then
        echo ">>> Skipping $name (already built)"
        SUCCEEDED=$((SUCCEEDED + 1))
        continue
    fi

    echo ">>> Building $name..."

    CLONE_DIR="$TEMP_DIR/$name"

    # Clone
    if ! git clone --depth 1 "$REPO" "$CLONE_DIR"; then
        echo "    FAILED: clone error"
        FAILED=$((FAILED + 1))
        FAILED_LIST="$FAILED_LIST $name(clone)"
        continue
    fi

    # Determine grammar directory
    if [ -n "$SUBDIR" ]; then
        GRAMMAR_DIR="$CLONE_DIR/$SUBDIR"
    else
        GRAMMAR_DIR="$CLONE_DIR"
    fi

    cd "$GRAMMAR_DIR"

    # Check if parser.c exists, if not run generate
    if [ ! -f "src/parser.c" ]; then
        echo "    Running tree-sitter generate..."
        if ! tree-sitter generate 2>&1; then
            echo "    FAILED: generate error"
            FAILED=$((FAILED + 1))
            FAILED_LIST="$FAILED_LIST $name(generate)"
            cd "$TEMP_DIR"
            continue
        fi
    fi

    # Build WASM
    echo "    Running tree-sitter build --wasm..."
    if ! tree-sitter build --wasm 2>&1; then
        echo "    FAILED: wasm build error"
        FAILED=$((FAILED + 1))
        FAILED_LIST="$FAILED_LIST $name(wasm)"
        cd "$TEMP_DIR"
        continue
    fi

    # Find and copy WASM file
    WASM_FILE=$(find . -maxdepth 1 -name "*.wasm" -type f | head -1)
    if [ -z "$WASM_FILE" ]; then
        echo "    FAILED: no wasm file generated"
        FAILED=$((FAILED + 1))
        FAILED_LIST="$FAILED_LIST $name(missing)"
        cd "$TEMP_DIR"
        continue
    fi

    cp "$WASM_FILE" "$OUTPUT_DIR/${name}.wasm"
    SIZE=$(ls -lh "$OUTPUT_DIR/${name}.wasm" | awk '{print $5}')
    echo "    SUCCESS: ${name}.wasm ($SIZE)"
    SUCCEEDED=$((SUCCEEDED + 1))

    cd "$TEMP_DIR"
done

# Cleanup
rm -rf "$TEMP_DIR" 2>/dev/null || true

echo ""
echo "=============================================="
echo "BUILD COMPLETE"
echo "=============================================="
echo "Succeeded: $SUCCEEDED / ${#GRAMMARS[@]}"
echo "Failed: $FAILED / ${#GRAMMARS[@]}"

if [ -n "$FAILED_LIST" ]; then
    echo ""
    echo "Failed grammars:$FAILED_LIST"
fi

echo ""
echo "Output: $OUTPUT_DIR"
ls -lh "$OUTPUT_DIR"/*.wasm 2>/dev/null | head -10 || echo "No WASM files yet"
