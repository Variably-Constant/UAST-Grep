#!/bin/bash
# Bash Test File for UAST-Grep
# Tests: functions, variables, control flow, error handling

# Strict mode
set -euo pipefail
IFS=$'\n\t'

# Constants
readonly MAX_ITEMS=100
readonly DEFAULT_NAME="UAST-Grep"

# Global variables
declare -g global_counter=0
declare -a items=()
declare -A cache=()

# String interpolation
name="UAST-Grep"
interpolated="Testing ${name} parser"
complex="Length: ${#name}"

# Here document
read -r -d '' help_text << 'EOF' || true
This is a here document.
It can span multiple lines.
And preserves formatting.
EOF

# Function definitions
log() {
    local level="${1:-INFO}"
    local message="${2:-}"
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] [${level}] ${message}"
}

# Function with return value
calculate_sum() {
    local a="${1:-0}"
    local b="${2:-0}"
    echo $((a + b))
}

# Function with local variables
transform() {
    local item="$1"
    local result

    # If-elif-else
    if [[ "$item" =~ ^[0-9]+$ ]]; then
        result=$((item * 2))
    elif [[ -n "$item" ]]; then
        result="${item^^}"  # Uppercase
    else
        result=""
    fi

    echo "$result"
}

# Process function
process_items() {
    local -n input_items=$1
    local -a results=()

    # For loop with index
    for ((i = 0; i < ${#input_items[@]}; i++)); do
        local transformed
        transformed=$(transform "${input_items[i]}")
        results+=("$transformed")
    done

    # For-in loop
    for item in "${input_items[@]}"; do
        cache["$item"]="$item"
    done

    # While loop
    local counter=0
    while ((counter < 10)); do
        ((counter++))
    done

    # Until loop
    until ((counter >= 20)); do
        ((counter++))
    done

    # Return array via nameref
    printf '%s\n' "${results[@]}"
}

# Error handling function
risky_operation() {
    local filename="${1:-test.txt}"

    # Trap for cleanup
    trap 'log "ERROR" "Operation failed"; return 1' ERR

    if [[ ! -f "$filename" ]]; then
        log "ERROR" "File not found: $filename"
        return 1
    fi

    local content
    content=$(cat "$filename")
    log "INFO" "Read ${#content} characters"

    trap - ERR
    return 0
}

# Function with default parameters
greet() {
    local name="${1:-World}"
    local greeting="${2:-Hello}"
    echo "${greeting}, ${name}!"
}

# Case statement
get_status_message() {
    local code="$1"
    case "$code" in
        200)
            echo "OK"
            ;;
        404)
            echo "Not Found"
            ;;
        500)
            echo "Server Error"
            ;;
        *)
            echo "Unknown"
            ;;
    esac
}

# Pattern matching with case
describe_file() {
    local file="$1"
    case "$file" in
        *.txt)
            echo "Text file"
            ;;
        *.sh)
            echo "Shell script"
            ;;
        *.py | *.pyc)
            echo "Python file"
            ;;
        *)
            echo "Unknown file type"
            ;;
    esac
}

# Select menu
show_menu() {
    local PS3="Choose an option: "
    local options=("Option 1" "Option 2" "Exit")

    select opt in "${options[@]}"; do
        case "$opt" in
            "Option 1")
                echo "Selected Option 1"
                ;;
            "Option 2")
                echo "Selected Option 2"
                ;;
            "Exit")
                break
                ;;
            *)
                echo "Invalid option"
                ;;
        esac
    done
}

# Process substitution
compare_outputs() {
    diff <(echo "first") <(echo "second") || true
}

# Command substitution
current_date=$(date '+%Y-%m-%d')
files_count=$(ls -1 | wc -l)

# Arithmetic operations
calculate() {
    local a=$1
    local b=$2
    local sum=$((a + b))
    local product=$((a * b))
    local quotient=$((a / b))
    local remainder=$((a % b))
    echo "Sum: $sum, Product: $product, Quotient: $quotient, Remainder: $remainder"
}

# Array operations
array_operations() {
    local -a arr=(1 2 3 4 5)

    # Length
    echo "Length: ${#arr[@]}"

    # Slice
    echo "Slice [1:3]: ${arr[@]:1:3}"

    # Append
    arr+=(6 7)

    # Iterate
    for elem in "${arr[@]}"; do
        echo "Element: $elem"
    done

    # Index access
    echo "First: ${arr[0]}, Last: ${arr[-1]}"
}

# Associative array operations
hash_operations() {
    local -A hash=(
        [name]="UAST-Grep"
        [version]="1.0"
        [author]="Test"
    )

    # Iterate keys
    for key in "${!hash[@]}"; do
        echo "$key: ${hash[$key]}"
    done

    # Check key exists
    if [[ -v hash[name] ]]; then
        echo "Name exists: ${hash[name]}"
    fi
}

# String operations
string_operations() {
    local str="Hello, World!"

    # Length
    echo "Length: ${#str}"

    # Substring
    echo "Substring: ${str:0:5}"

    # Replace
    echo "Replace: ${str/World/UAST-Grep}"

    # Remove prefix/suffix
    local path="/path/to/file.txt"
    echo "Basename: ${path##*/}"
    echo "Dirname: ${path%/*}"

    # Case conversion
    echo "Uppercase: ${str^^}"
    echo "Lowercase: ${str,,}"
}

# Conditional expressions
check_conditions() {
    local file="$1"
    local value="$2"

    # File tests
    [[ -f "$file" ]] && echo "Is regular file"
    [[ -d "$file" ]] && echo "Is directory"
    [[ -r "$file" ]] && echo "Is readable"
    [[ -w "$file" ]] && echo "Is writable"
    [[ -x "$file" ]] && echo "Is executable"

    # String tests
    [[ -n "$value" ]] && echo "Not empty"
    [[ -z "$value" ]] || echo "Empty"

    # Numeric comparisons
    [[ "$value" -eq 0 ]] && echo "Equals zero"
    [[ "$value" -gt 0 ]] && echo "Greater than zero"

    # Pattern matching
    [[ "$value" == *"pattern"* ]] && echo "Contains pattern"
    [[ "$value" =~ ^[0-9]+$ ]] && echo "Is numeric"
}

# Subshell
run_in_subshell() {
    (
        cd /tmp
        pwd
        # Changes don't affect parent
    )
}

# Background process
run_background() {
    sleep 10 &
    local pid=$!
    echo "Background PID: $pid"
    wait "$pid"
}

# Main function
main() {
    log "INFO" "Starting $DEFAULT_NAME"

    # Initialize data
    local -a data=(1 2 3 "hello" "world")

    # Process items
    local results
    results=$(process_items data)

    log "INFO" "Processing complete"
    echo "Results: $results"

    # Test functions
    echo "Sum: $(calculate_sum 5 3)"
    echo "Status: $(get_status_message 200)"
    echo "$(greet "UAST-Grep")"

    # String operations
    string_operations

    # Calculate
    calculate 17 5
}

# Run main if executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
