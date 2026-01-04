/**
 * Groovy Test File for UAST-Grep
 * Tests: functions, classes, variables, control flow, error handling
 */

// Single line comment
package com.example.test

import groovy.transform.Immutable
import groovy.transform.ToString
import groovy.transform.CompileStatic

// Constants
final MAX_ITEMS = 100
final DEFAULT_NAME = 'UAST-Grep'

// Type alias (using annotations)
// Groovy doesn't have direct type aliases, using traits instead

// Trait (interface with implementation)
trait Loggable {
    abstract String getName()

    void log(String message) {
        println "[${getName()}] $message"
    }
}

// Enum
enum Status {
    OK(200, 'OK'),
    NOT_FOUND(404, 'Not Found'),
    SERVER_ERROR(500, 'Server Error')

    final int code
    final String message

    Status(int code, String message) {
        this.code = code
        this.message = message
    }
}

// Immutable data class (similar to record)
@Immutable
class Person {
    String name
    int age
    String email

    boolean isAdult() {
        age >= 18
    }
}

// Abstract class
abstract class BaseProcessor implements Loggable {
    protected String name
    protected int count = 0

    BaseProcessor(String name) {
        this.name = name
    }

    @Override
    String getName() {
        return name
    }

    abstract List<Object> process(List<Object> items)
}

// Concrete class with @ToString
@ToString(includeNames = true)
class DataProcessor extends BaseProcessor {
    private Map<String, Object> cache = [:]

    DataProcessor(String name = DEFAULT_NAME) {
        super(name)
    }

    @Override
    List<Object> process(List<Object> items) {
        def results = []

        // C-style for loop
        for (int i = 0; i < items.size(); i++) {
            results << transform(items[i])
        }

        // For-in loop
        for (item in items) {
            cache[item.toString()] = item
        }

        // Each closure (idiomatic)
        items.each { item ->
            // Process each item
        }

        // Each with index
        items.eachWithIndex { item, index ->
            log "Item $index: $item"
        }

        // While loop
        int counter = 0
        while (counter < 10) {
            counter++
        }

        // Do-while loop
        do {
            counter++
        } while (counter < 20)

        count = results.size()
        results
    }

    private Object transform(Object item) {
        // Switch expression with closures
        switch (item) {
            case List:
                return item.collect { it * 2 }
            case String:
                return item.toUpperCase()
            case Number:
                return item * 2
            default:
                return item
        }
    }

    void riskyOperation() {
        // Try-catch-finally
        try {
            def content = new File('test.txt').text
            log "Read ${content.size()} characters"
        } catch (FileNotFoundException e) {
            log "File not found: ${e.message}"
        } catch (IOException e) {
            log "IO error: ${e.message}"
        } finally {
            log 'Operation complete'
        }
    }
}

// Static compilation for better performance
@CompileStatic
class Calculator {
    static int add(int a, int b) {
        a + b
    }

    static int multiply(int a, int b) {
        a * b
    }
}

// Standalone functions (as Closure)
def calculateSum = { int a, int b = 0 -> a + b }

// Variadic function
def sumAll = { int... numbers -> numbers.sum() }

// Higher-order function
def mapItems = { List items, Closure transform ->
    items.collect(transform)
}

def filterItems = { List items, Closure predicate ->
    items.findAll(predicate)
}

def reduceItems = { List items, initial, Closure combine ->
    items.inject(initial, combine)
}

// Closure with delegate
def withProcessor = { String name, Closure block ->
    def processor = new DataProcessor(name)
    block.delegate = processor
    block.resolveStrategy = Closure.DELEGATE_FIRST
    block()
    processor
}

// Get status message
String getStatusMessage(int code) {
    def messages = [
        200: 'OK',
        404: 'Not Found',
        500: 'Server Error'
    ]
    messages[code] ?: 'Unknown'
}

// Elvis operator and safe navigation
String processNullable(String value) {
    // Elvis operator
    def nonNull = value ?: 'default'

    // Safe navigation
    def length = value?.length() ?: 0

    // Safe method call chain
    def upper = value?.toUpperCase()?.trim()

    nonNull
}

// Spread operator
def spreadExample(List<Person> people) {
    // Spread operator to get all names
    def names = people*.name

    // Spread with method call
    def ages = people*.age.sum()

    [names, ages]
}

// GString (interpolated strings)
def name = 'UAST-Grep'
def interpolated = "Testing $name parser"
def complex = "Length: ${name.length()}"
def multiline = """
This is a multiline string.
It can span multiple lines.
And allows $name interpolation.
"""

// List operations
def listOperations() {
    def list = [1, 2, 3, 4, 5]

    // Collect (map)
    def doubled = list.collect { it * 2 }

    // Find all (filter)
    def evens = list.findAll { it % 2 == 0 }

    // Find first
    def first = list.find { it > 3 }

    // Inject (reduce)
    def sum = list.inject(0) { acc, val -> acc + val }

    // Group by
    def grouped = list.groupBy { it % 2 == 0 ? 'even' : 'odd' }

    // Take and drop
    def taken = list.take(3)
    def dropped = list.drop(2)

    [doubled, evens, sum, grouped]
}

// Map operations
def mapOperations() {
    def map = [a: 1, b: 2, c: 3]

    // Each on map
    map.each { key, value ->
        println "$key: $value"
    }

    // Collect entries
    def transformed = map.collectEntries { k, v -> [(k.toUpperCase()): v * 2] }

    // Find all entries
    def filtered = map.findAll { k, v -> v > 1 }

    // Submap
    def subset = map.subMap(['a', 'b'])

    [transformed, filtered, subset]
}

// Range
def rangeExample() {
    def range = 1..10
    def exclusive = 1..<10
    def reverse = 10..1

    def stepped = range.step(2)

    [range.size(), exclusive.toList(), stepped]
}

// Regular expressions
def regexExample(String text) {
    // Match operator
    if (text ==~ /\d+/) {
        println 'Matches digits'
    }

    // Find operator
    def matcher = text =~ /\w+/
    def words = matcher.collect { it }

    // Replace
    def replaced = text.replaceAll(/\d+/, 'NUMBER')

    words
}

// Metaprogramming - add method at runtime
String.metaClass.toTitleCase = {
    delegate.split(' ').collect { word ->
        word.capitalize()
    }.join(' ')
}

// Main execution
def main() {
    def processor = new DataProcessor('Main')

    def data = [1, 2, 3, 'hello', [4, 5]]
    def results = processor.process(data)

    processor.log "Processing complete"
    processor.log "Results: ${results.join(', ')}"

    // Test functions
    println "Sum: ${calculateSum(5, 3)}"
    println "Sum all: ${sumAll(1, 2, 3, 4, 5)}"
    println "Status: ${getStatusMessage(200)}"

    // Higher-order functions
    def doubled = mapItems([1, 2, 3, 4, 5]) { it * 2 }
    println "Doubled: ${doubled.join(', ')}"

    def filtered = filterItems([1, 2, 3, 4, 5]) { it > 2 }
    println "Filtered: ${filtered.join(', ')}"

    def total = reduceItems([1, 2, 3, 4, 5], 0) { acc, n -> acc + n }
    println "Total: $total"

    // Person (immutable)
    def person = new Person(name: 'Alice', age: 30, email: 'alice@example.com')
    println "${person.name} is adult: ${person.isAdult()}"

    // With closure
    withProcessor('Builder') {
        log 'Using builder pattern'
    }

    // List operations
    def (d, e, s, g) = listOperations()
    println "Doubled: ${d.join(', ')}"

    // Metaprogramming
    println 'hello world'.toTitleCase()

    // Range
    println "Range: ${(1..5).toList()}"

    // Safe navigation
    println "Nullable: ${processNullable(null)}"
}

main()
