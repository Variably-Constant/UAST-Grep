#!/bin/bash
# Build native shared libraries for grammars that failed WASM compilation
#
# WASM failures:
# - doxygen: Uses fprintf, isalnum, stderr (C library functions)
# - vim: Uses iswpunct (C library function)
# - cobol: Out of memory during LLVM optimization (grammar too large - 30MB parser.c)
#
# Native build issues:
# - cobol: Uses C99 VLA (Variable Length Arrays) in scanner.c which MSVC doesn't support
#          Needs GCC/Clang on Windows or use Linux/macOS builds

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$SCRIPT_DIR/.."
OUTPUT_DIR="$PROJECT_DIR/native/uast_core/grammars-native"
TEMP_DIR="$HOME/native-build-temp"

# Note: We must clone the full repos because tree-sitter build needs grammar.json
# The existing grammars/ directory only contains parser.c/scanner.c without grammar.json

# Determine file extension based on OS
case "$(uname -s)" in
    MINGW*|MSYS*|CYGWIN*|Windows_NT)
        EXT="dll"
        OS_NAME="Windows"
        ;;
    Darwin*)
        EXT="dylib"
        OS_NAME="macOS"
        ;;
    Linux*)
        EXT="so"
        OS_NAME="Linux"
        ;;
    *)
        echo "Unknown OS: $(uname -s)"
        exit 1
        ;;
esac

echo "=============================================="
echo "Building Native Grammar Libraries ($OS_NAME)"
echo "=============================================="
echo "Extension: .$EXT"
echo "Output: $OUTPUT_DIR"
echo ""

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Grammars that failed WASM compilation - repo|subdir format
declare -A GRAMMARS=(
    ["doxygen"]="https://github.com/tree-sitter-grammars/tree-sitter-doxygen"
    ["vim"]="https://github.com/tree-sitter-grammars/tree-sitter-vim"
    ["cobol"]="https://github.com/yutaro-sakamoto/tree-sitter-cobol"
)

SUCCEEDED=0
FAILED=0
SKIPPED=0
FAILED_LIST=""

for name in "${!GRAMMARS[@]}"; do
    REPO="${GRAMMARS[$name]}"
    OUTPUT_FILE="$OUTPUT_DIR/${name}.$EXT"

    echo ">>> Processing $name..."

    # Skip if already built
    if [ -f "$OUTPUT_FILE" ]; then
        echo "    SKIPPED: $OUTPUT_FILE already exists"
        SKIPPED=$((SKIPPED + 1))
        continue
    fi

    # Always clone from repo - we need grammar.json which isn't in local grammars/
    echo "    Cloning from $REPO..."
    rm -rf "$TEMP_DIR/$name" 2>/dev/null || true
    mkdir -p "$TEMP_DIR"

    if ! git clone --depth 1 "$REPO" "$TEMP_DIR/$name" 2>&1; then
        echo "    FAILED: clone error"
        FAILED=$((FAILED + 1))
        FAILED_LIST="$FAILED_LIST $name(clone)"
        continue
    fi

    GRAMMAR_DIR="$TEMP_DIR/$name"

    # Generate parser if needed
    if [ ! -f "$GRAMMAR_DIR/src/parser.c" ]; then
        echo "    Running tree-sitter generate..."
        cd "$GRAMMAR_DIR"
        if ! tree-sitter generate 2>&1; then
            echo "    FAILED: generate error"
            FAILED=$((FAILED + 1))
            FAILED_LIST="$FAILED_LIST $name(generate)"
            continue
        fi
    fi

    # Build native library
    echo "    Running tree-sitter build..."
    cd "$GRAMMAR_DIR"

    # Use -o to specify output path directly
    if ! tree-sitter build -o "$OUTPUT_FILE" 2>&1; then
        echo "    FAILED: native build error"
        FAILED=$((FAILED + 1))
        FAILED_LIST="$FAILED_LIST $name(build)"
        continue
    fi

    # Verify output exists
    if [ -f "$OUTPUT_FILE" ]; then
        SIZE=$(ls -lh "$OUTPUT_FILE" | awk '{print $5}')
        echo "    SUCCESS: ${name}.$EXT ($SIZE)"
        SUCCEEDED=$((SUCCEEDED + 1))
    else
        echo "    FAILED: output file not created"
        FAILED=$((FAILED + 1))
        FAILED_LIST="$FAILED_LIST $name(missing)"
    fi
done

# Cleanup temp directory
rm -rf "$TEMP_DIR" 2>/dev/null || true

# Summary
TOTAL=${#GRAMMARS[@]}
echo ""
echo "=============================================="
echo "BUILD COMPLETE"
echo "=============================================="
echo "Succeeded: $SUCCEEDED / $TOTAL"
echo "Skipped:   $SKIPPED / $TOTAL (already built)"
echo "Failed:    $FAILED / $TOTAL"

if [ -n "$FAILED_LIST" ]; then
    echo ""
    echo "Failed grammars:$FAILED_LIST"
fi

echo ""
echo "Output directory: $OUTPUT_DIR"
if [ -d "$OUTPUT_DIR" ]; then
    echo ""
    ls -lh "$OUTPUT_DIR"/*.$EXT 2>/dev/null || echo "No native libraries built yet"
fi

# Exit with error only if ALL grammars failed
if [ $SUCCEEDED -eq 0 ] && [ $SKIPPED -eq 0 ]; then
    echo ""
    echo "ERROR: All grammar builds failed!"
    exit 1
fi

# Note: Some failures are expected (e.g., COBOL on Windows with MSVC due to VLA)
