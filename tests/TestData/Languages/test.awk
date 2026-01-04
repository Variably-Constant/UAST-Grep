#!/usr/bin/awk -f
# AWK Test File for UAST-Grep
# Tests: patterns, actions, functions, arrays, control flow

# BEGIN block - runs before processing any input
BEGIN {
    # Constants
    MAX_ITEMS = 100
    DEFAULT_NAME = "UAST-Grep"

    # Field separator
    FS = ","
    OFS = "\t"

    # Record separator
    RS = "\n"
    ORS = "\n"

    # Initialize variables
    total = 0
    count = 0
    sum = 0

    # Initialize arrays
    delete items
    delete cache

    # Print header
    print "=== UAST-Grep AWK Test ==="
    print "Processing started at:", strftime("%Y-%m-%d %H:%M:%S")
    print ""
}

# Function definitions
function log_message(level, message) {
    printf "[%s] [%s] %s\n", strftime("%H:%M:%S"), level, message
}

function calculate_sum(a, b) {
    return a + b
}

function transform(value,    result) {
    # Local variable 'result' declared in parameter list
    if (value ~ /^[0-9]+$/) {
        result = value * 2
    } else if (value ~ /^[a-zA-Z]+$/) {
        result = toupper(value)
    } else {
        result = value
    }
    return result
}

function array_length(arr,    count) {
    count = 0
    for (key in arr) {
        count++
    }
    return count
}

function array_sum(arr,    total, key) {
    total = 0
    for (key in arr) {
        if (arr[key] ~ /^[0-9]+$/) {
            total += arr[key]
        }
    }
    return total
}

function print_array(arr, name,    key) {
    print name ":"
    for (key in arr) {
        printf "  [%s] = %s\n", key, arr[key]
    }
}

function trim(str) {
    gsub(/^[ \t]+|[ \t]+$/, "", str)
    return str
}

function split_and_process(str, delimiter,    parts, n, i) {
    n = split(str, parts, delimiter)
    for (i = 1; i <= n; i++) {
        parts[i] = transform(trim(parts[i]))
    }
    return n
}

# Pattern: Match all lines
{
    # Increment line count
    count++

    # Store original line
    original = $0

    # Process each field
    for (i = 1; i <= NF; i++) {
        # Trim whitespace
        $i = trim($i)

        # Store in items array
        items[count, i] = $i

        # Cache unique values
        if (!($i in cache)) {
            cache[$i] = 1
        } else {
            cache[$i]++
        }
    }
}

# Pattern: Lines starting with #
/^#/ {
    # Skip comments
    next
}

# Pattern: Empty lines
/^[[:space:]]*$/ {
    # Skip empty lines
    next
}

# Pattern: Lines with numbers
/[0-9]+/ {
    # Process numeric data
    for (i = 1; i <= NF; i++) {
        if ($i ~ /^[0-9]+$/) {
            sum += $i
            numeric_count++
        }
    }
}

# Pattern: Match specific field values
$1 == "header" {
    print "Found header row:", $0
    header_found = 1
}

# Pattern: Field contains pattern
$2 ~ /error|warning|critical/ {
    log_message("ALERT", "Found issue in line " NR ": " $0)
    errors[NR] = $0
}

# Pattern: Numeric range
NR >= 1 && NR <= 10 {
    # First 10 lines
    first_lines[NR] = $0
}

# Pattern: Regular expression
/^[A-Z][a-z]+,[0-9]+/ {
    # Matches lines like "Name,123"
    split($0, parts, ",")
    names[parts[1]] = parts[2]
}

# Pattern: Complex condition
NF >= 3 && $1 !~ /^#/ && length($0) > 10 {
    # Lines with 3+ fields, not comments, longer than 10 chars
    valid_lines++
}

# Pattern with range
/START/,/END/ {
    # Process lines between START and END markers
    between_markers++
}

# END block - runs after processing all input
END {
    print ""
    print "=== Processing Complete ==="
    print ""

    # Summary statistics
    print "Statistics:"
    printf "  Total lines processed: %d\n", count
    printf "  Numeric values found: %d\n", numeric_count
    printf "  Sum of numeric values: %d\n", sum
    printf "  Average: %.2f\n", (numeric_count > 0 ? sum / numeric_count : 0)
    printf "  Unique values cached: %d\n", array_length(cache)
    printf "  Valid lines: %d\n", valid_lines
    printf "  Lines between markers: %d\n", between_markers
    print ""

    # Error summary
    error_count = array_length(errors)
    if (error_count > 0) {
        printf "Errors found: %d\n", error_count
        for (line_num in errors) {
            printf "  Line %d: %s\n", line_num, errors[line_num]
        }
        print ""
    }

    # Most frequent values
    print "Top 5 most frequent values:"
    # Simple sorting (AWK doesn't have built-in sort for arrays)
    max_count = 0
    for (val in cache) {
        if (cache[val] > max_count) {
            max_count = cache[val]
        }
    }

    printed = 0
    for (c = max_count; c >= 1 && printed < 5; c--) {
        for (val in cache) {
            if (cache[val] == c && printed < 5) {
                printf "  %s: %d occurrences\n", val, cache[val]
                printed++
            }
        }
    }

    print ""

    # Conditional output
    if (count == 0) {
        print "WARNING: No data processed"
        exit_code = 1
    } else if (error_count > 0) {
        print "WARNING: Processing completed with errors"
        exit_code = 2
    } else {
        print "SUCCESS: Processing completed successfully"
        exit_code = 0
    }

    print ""
    print "Finished at:", strftime("%Y-%m-%d %H:%M:%S")

    # Exit with appropriate code
    exit exit_code
}

# Error handling function
function handle_error(message) {
    print "ERROR:", message > "/dev/stderr"
    error_occurred = 1
}

# String formatting function
function format_number(num, decimals,    fmt) {
    fmt = "%." decimals "f"
    return sprintf(fmt, num)
}

# Date/time formatting
function format_timestamp(epoch,    format) {
    format = "%Y-%m-%d %H:%M:%S"
    return strftime(format, epoch)
}

# Command execution (if supported)
function run_command(cmd,    result) {
    while ((cmd | getline result) > 0) {
        print "Command output:", result
    }
    close(cmd)
}

# JSON-like output
function to_json(key, value) {
    gsub(/"/, "\\\"", value)
    return sprintf("\"%s\": \"%s\"", key, value)
}

# CSV parsing with quoted fields
function parse_csv(line, fields,    i, n, in_quotes) {
    n = 0
    in_quotes = 0
    current = ""

    for (i = 1; i <= length(line); i++) {
        c = substr(line, i, 1)

        if (c == "\"") {
            in_quotes = !in_quotes
        } else if (c == "," && !in_quotes) {
            fields[++n] = current
            current = ""
        } else {
            current = current c
        }
    }

    if (current != "") {
        fields[++n] = current
    }

    return n
}
