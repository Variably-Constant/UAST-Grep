//! Rust Test File for UAST-Grep
//! Tests: functions, structs, traits, enums, variables, control flow, error handling

// Single line comment
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::fs::File;
use std::io::{self, BufRead, BufReader};

// Constants
const MAX_ITEMS: usize = 100;
static DEFAULT_NAME: &str = "UAST-Grep";

// Type alias
type Result<T> = std::result::Result<T, Box<dyn Error>>;

// Custom error type
#[derive(Debug)]
struct ProcessingError {
    message: String,
}

impl fmt::Display for ProcessingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Processing error: {}", self.message)
    }
}

impl Error for ProcessingError {}

// Trait definition
trait Processor {
    fn process(&mut self, items: Vec<Box<dyn std::any::Any>>) -> Result<Vec<Box<dyn std::any::Any>>>;
    fn log(&self, message: &str);
}

// Enum with data
#[derive(Debug, Clone)]
enum Status {
    Ok,
    NotFound,
    Error(String),
}

impl Status {
    fn code(&self) -> i32 {
        match self {
            Status::Ok => 200,
            Status::NotFound => 404,
            Status::Error(_) => 500,
        }
    }
}

// Struct with derive macros
#[derive(Debug, Clone)]
struct DataProcessor {
    name: String,
    count: usize,
    cache: HashMap<String, String>,
}

impl DataProcessor {
    // Constructor (associated function)
    fn new(name: Option<&str>) -> Self {
        DataProcessor {
            name: name.unwrap_or(DEFAULT_NAME).to_string(),
            count: 0,
            cache: HashMap::new(),
        }
    }

    // Method with mutable self
    fn process_items(&mut self, items: &[i32]) -> Vec<i32> {
        let mut results = Vec::with_capacity(items.len());

        // For loop with iterator
        for item in items.iter() {
            results.push(self.transform(*item));
        }

        // For loop with range
        for i in 0..items.len() {
            self.cache.insert(i.to_string(), items[i].to_string());
        }

        // While loop
        let mut counter = 0;
        while counter < 10 {
            counter += 1;
        }

        // Loop with break and continue
        loop {
            if counter >= 20 {
                break;
            }
            if counter % 2 == 0 {
                counter += 1;
                continue;
            }
            counter += 1;
        }

        results
    }

    fn transform(&self, item: i32) -> i32 {
        item * 2
    }

    // Method with Result return type
    fn risky_operation(&self) -> Result<String> {
        let file = File::open("test.txt")?;
        let reader = BufReader::new(file);

        let mut content = String::new();
        for line in reader.lines() {
            content.push_str(&line?);
            content.push('\n');
        }

        Ok(content)
    }

    fn log(&self, message: &str) {
        println!("[{}] {}", self.name, message);
    }
}

// Implement trait for struct
impl Processor for DataProcessor {
    fn process(&mut self, _items: Vec<Box<dyn std::any::Any>>) -> Result<Vec<Box<dyn std::any::Any>>> {
        Ok(Vec::new())
    }

    fn log(&self, message: &str) {
        println!("[{}] {}", self.name, message);
    }
}

// Generic struct
struct Pair<T, U> {
    first: T,
    second: U,
}

impl<T, U> Pair<T, U> {
    fn new(first: T, second: U) -> Self {
        Pair { first, second }
    }
}

// Generic function with trait bounds
fn process_collection<T: Clone + std::fmt::Debug>(items: &[T]) -> Vec<T> {
    items.to_vec()
}

// Function with pattern matching
fn get_status_message(code: i32) -> &'static str {
    match code {
        200 => "OK",
        404 => "Not Found",
        500 => "Server Error",
        _ => "Unknown",
    }
}

// Function with if-let
fn extract_error(status: &Status) -> Option<&String> {
    if let Status::Error(msg) = status {
        Some(msg)
    } else {
        None
    }
}

// Closure examples
fn apply_operation<F>(x: i32, y: i32, op: F) -> i32
where
    F: Fn(i32, i32) -> i32,
{
    op(x, y)
}

// Async function
async fn async_operation(data: &str) -> Result<String> {
    Ok(format!("Processed: {}", data))
}

// Main function
fn main() -> Result<()> {
    let mut processor = DataProcessor::new(Some("Main"));

    let data = vec![1, 2, 3, 4, 5];
    let results = processor.process_items(&data);

    processor.log(&format!("Results: {:?}", results));

    // Pattern matching with if-let
    if let Err(e) = processor.risky_operation() {
        processor.log(&format!("Error: {}", e));
    }

    // Closure usage
    let multiply = |x, y| x * y;
    let product = apply_operation(5, 3, multiply);
    println!("Product: {}", product);

    // Match expression
    let status = Status::Ok;
    match &status {
        Status::Ok => println!("All good!"),
        Status::NotFound => println!("Resource missing"),
        Status::Error(msg) => println!("Error: {}", msg),
    }

    // String interpolation
    let name = "UAST-Grep";
    let message = format!("Testing {} parser", name);
    println!("{}", message);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transform() {
        let processor = DataProcessor::new(None);
        assert_eq!(processor.transform(5), 10);
    }
}
