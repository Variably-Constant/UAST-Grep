# R Test File for UAST-Grep
# Tests: functions, classes, variables, control flow, error handling

# Single line comment

# Load packages
library(methods)

# Constants
MAX_ITEMS <- 100L
DEFAULT_NAME <- "UAST-Grep"

# Variables
global_counter <- 0L
name <- "UAST-Grep"
interpolated <- paste0("Testing ", name, " parser")
complex <- sprintf("Length: %d", nchar(name))

# Vector types
int_vector <- c(1L, 2L, 3L, 4L, 5L)
num_vector <- c(1.0, 2.5, 3.7)
char_vector <- c("a", "b", "c")
logical_vector <- c(TRUE, FALSE, TRUE)

# List (mixed types)
mixed_list <- list(
  numbers = c(1, 2, 3),
  text = "hello",
  nested = list(a = 1, b = 2)
)

# Named vector
named_vec <- c(x = 1, y = 2, z = 3)

# Matrix
mat <- matrix(1:9, nrow = 3, ncol = 3)

# Data frame
df <- data.frame(
  name = c("Alice", "Bob", "Charlie"),
  age = c(30L, 25L, 35L),
  score = c(85.5, 92.0, 78.5),
  stringsAsFactors = FALSE
)

# S4 Class definition
setClass(
  "Person",
  slots = c(
    name = "character",
    age = "numeric",
    email = "character"
  ),
  prototype = list(
    name = "Unknown",
    age = 0,
    email = NA_character_
  )
)

# Constructor method
setMethod(
  "initialize",
  "Person",
  function(.Object, name, age, email = NA_character_) {
    .Object@name <- name
    .Object@age <- age
    .Object@email <- email
    .Object
  }
)

# Generic method
setGeneric("is_adult", function(object) standardGeneric("is_adult"))

setMethod(
  "is_adult",
  "Person",
  function(object) {
    object@age >= 18
  }
)

# S3 class (alternative approach)
DataProcessor <- function(name = DEFAULT_NAME) {
  structure(
    list(
      name = name,
      count = 0L,
      cache = new.env(hash = TRUE)
    ),
    class = "DataProcessor"
  )
}

# S3 method
process.DataProcessor <- function(processor, items) {
  results <- vector("list", length(items))

  # For loop with index
  for (i in seq_along(items)) {
    results[[i]] <- transform_item(items[[i]])
  }

  # For-in loop
  for (item in items) {
    key <- as.character(item)
    assign(key, item, envir = processor$cache)
  }

  # While loop
  counter <- 0L
  while (counter < 10L) {
    counter <- counter + 1L
  }

  # Repeat loop with break
  repeat {
    counter <- counter + 1L
    if (counter >= 20L) break
  }

  processor$count <- length(results)
  results
}

# Log method
log_message <- function(processor, message) {
  cat(sprintf("[%s] %s\n", processor$name, message))
}

# Transform function
transform_item <- function(item) {
  # Type-based transformation
  if (is.numeric(item) && length(item) > 1) {
    return(item * 2)
  } else if (is.character(item)) {
    return(toupper(item))
  } else if (is.numeric(item)) {
    return(item * 2)
  } else {
    return(item)
  }
}

# Error handling
risky_operation <- function(filename) {
  result <- tryCatch(
    {
      content <- readLines(filename, warn = FALSE)
      message(sprintf("Read %d lines", length(content)))
      content
    },
    error = function(e) {
      warning(sprintf("Error: %s", e$message))
      NULL
    },
    warning = function(w) {
      warning(sprintf("Warning: %s", w$message))
      NULL
    },
    finally = {
      message("Operation complete")
    }
  )
  result
}

# Calculate sum
calculate_sum <- function(a, b = 0) {
  a + b
}

# Variadic function
sum_all <- function(...) {
  args <- list(...)
  Reduce(`+`, args, accumulate = FALSE)
}

# Higher-order functions
map_items <- function(items, func) {
  lapply(items, func)
}

filter_items <- function(items, predicate) {
  Filter(predicate, items)
}

reduce_items <- function(items, func, init) {
  Reduce(func, items, init)
}

# Get status message
get_status_message <- function(code) {
  messages <- c(
    "200" = "OK",
    "404" = "Not Found",
    "500" = "Server Error"
  )
  msg <- messages[as.character(code)]
  if (is.na(msg)) "Unknown" else msg
}

# Switch expression
describe_type <- function(x) {
  switch(
    class(x)[1],
    "numeric" = "Numeric value",
    "character" = "Character string",
    "list" = "List structure",
    "data.frame" = "Data frame",
    "Unknown type"
  )
}

# Vectorized operations
vectorized_example <- function(x) {
  # Element-wise operations
  doubled <- x * 2
  squared <- x^2
  logged <- log(x + 1)

  # Conditional vectorization
  result <- ifelse(x > 0, x * 2, 0)

  # Apply family
  matrix_result <- apply(mat, 1, sum)  # Row sums

  list(
    doubled = doubled,
    squared = squared,
    logged = logged,
    conditional = result
  )
}

# Anonymous function (lambda)
double_fn <- function(x) x * 2

# Pipe operator (magrittr style)
process_pipeline <- function(data) {
  # Using base R pipe (R 4.1+)
  result <- data |>
    Filter(function(x) x > 0, x = _) |>
    sapply(function(x) x * 2)

  result
}

# Environment operations
env_example <- function() {
  e <- new.env()
  e$x <- 10
  e$y <- 20

  # List environment contents
  ls(e)

  # Check existence
  exists("x", envir = e)

  # Get value
  get("x", envir = e)

  e
}

# String operations
string_operations <- function() {
  s <- "Hello, World!"

  # Length
  len <- nchar(s)

  # Substring
  sub_str <- substr(s, 1, 5)

  # Split
  parts <- strsplit(s, ", ")[[1]]

  # Paste
  joined <- paste(parts, collapse = " - ")

  # Pattern matching
  has_world <- grepl("World", s)

  # Replacement
  replaced <- gsub("World", "UAST-Grep", s)

  # Case conversion
  upper <- toupper(s)
  lower <- tolower(s)

  list(
    length = len,
    substring = sub_str,
    upper = upper,
    replaced = replaced
  )
}

# Control flow examples
control_flow_examples <- function(x) {
  # If-else
  if (x > 0) {
    result <- "Positive"
  } else if (x < 0) {
    result <- "Negative"
  } else {
    result <- "Zero"
  }

  # Inline if
  status <- if (x > 0) "Positive" else "Non-positive"

  # For with next and break
  for (i in 1:10) {
    if (i == 3) next  # Skip iteration
    if (i == 8) break # Exit loop
    cat(i, " ")
  }

  cat("\n")
  list(result = result, status = status)
}

# Main execution
main <- function() {
  processor <- DataProcessor("Main")

  data <- list(1L, 2L, 3L, "hello", c(4L, 5L))
  results <- process.DataProcessor(processor, data)

  log_message(processor, "Processing complete")
  cat("Results:", paste(sapply(results, toString), collapse = ", "), "\n")

  # Test functions
  cat("Sum:", calculate_sum(5, 3), "\n")
  cat("Sum all:", sum_all(1, 2, 3, 4, 5), "\n")
  cat("Status:", get_status_message(200), "\n")

  # Higher-order functions
  doubled <- map_items(1:5, function(x) x * 2)
  cat("Doubled:", paste(unlist(doubled), collapse = ", "), "\n")

  filtered <- filter_items(1:5, function(x) x > 2)
  cat("Filtered:", paste(filtered, collapse = ", "), "\n")

  total <- reduce_items(1:5, `+`, 0)
  cat("Total:", total, "\n")

  # S4 Person
  person <- new("Person", name = "Alice", age = 30)
  cat(sprintf("%s is adult: %s\n", person@name, is_adult(person)))

  # String operations
  str_results <- string_operations()
  cat("Upper:", str_results$upper, "\n")

  # Vectorized
  vec_results <- vectorized_example(int_vector)
  cat("Doubled vector:", paste(vec_results$doubled, collapse = ", "), "\n")
}

# Run main
main()
