/**
 * D Language Test File for UAST-Grep
 * Tests: functions, classes, templates, ranges, contracts
 */
module test;

import std.stdio;
import std.string;
import std.algorithm;
import std.range;
import std.array;
import std.conv;
import std.exception;
import std.traits;
import core.exception;

// Constants
enum MAX_ITEMS = 100;
enum DEFAULT_NAME = "UAST-Grep";

// Enum
enum Status {
    ok = 200,
    notFound = 404,
    serverError = 500
}

// Struct
struct Person {
    string name;
    int age;
    string email;
    bool active = true;

    // Constructor
    this(string name, int age, string email = null) {
        this.name = name;
        this.age = age;
        this.email = email;
    }

    // Method
    bool isAdult() const {
        return age >= 18;
    }

    // Operator overload
    int opCmp(ref const Person other) const {
        return this.name < other.name ? -1 :
               this.name > other.name ? 1 : 0;
    }

    // String representation
    string toString() const {
        return format("Person(%s, %d)", name, age);
    }
}

// Class with interface
interface IProcessor {
    int[] process(int[] items);
    void log(string message);
}

// Abstract class
abstract class BaseProcessor : IProcessor {
    protected string name;
    protected int count = 0;

    this(string name = DEFAULT_NAME) {
        this.name = name;
    }

    @property string getName() const { return name; }
    @property int getCount() const { return count; }

    abstract int[] process(int[] items);

    void log(string message) {
        writefln("[%s] %s", name, message);
    }
}

// Concrete class
class DataProcessor : BaseProcessor {
    private int[string] cache;

    this(string name = "DataProcessor") {
        super(name);
    }

    override int[] process(int[] items) {
        int[] results;

        // For loop
        foreach (i, item; items) {
            results ~= transform(item);
            cache[to!string(i)] = item;
        }

        // For loop with index
        for (size_t i = 0; i < items.length; i++) {
            count++;
        }

        // While loop
        int counter = 0;
        while (counter < 10) {
            counter++;
        }

        // Do-while loop
        do {
            counter++;
        } while (counter < 20);

        return results;
    }

    private int transform(int item) {
        // Switch expression
        return item > 0 ? item * 2 :
               item < 0 ? item * -1 : 0;
    }

    void riskyOperation() {
        // Try-catch-finally
        try {
            auto file = File("test.txt", "r");
            scope(exit) file.close();
            auto content = file.byLine().join("\n");
            log(format("Read %d bytes", content.length));
        }
        catch (FileException e) {
            log(format("File error: %s", e.msg));
        }
        catch (Exception e) {
            log(format("Error: %s", e.msg));
            throw e;
        }
        finally {
            log("Operation complete");
        }
    }
}

// Template function
T add(T)(T a, T b) {
    return a + b;
}

// Template with constraints
T findMax(T)(T[] items) if (is(typeof(items[0] < items[0]) == bool)) {
    enforce(!items.empty, "Array cannot be empty");
    return items.fold!((a, b) => a > b ? a : b);
}

// Template class
class Pair(T, U) {
    T first;
    U second;

    this(T first, U second) {
        this.first = first;
        this.second = second;
    }

    Pair!(U, T) swap() {
        return new Pair!(U, T)(second, first);
    }
}

// Template mixin
mixin template Loggable() {
    void logInfo(string msg) {
        writefln("[INFO] %s", msg);
    }

    void logError(string msg) {
        stderr.writefln("[ERROR] %s", msg);
    }
}

class Logger {
    mixin Loggable;
}

// Compile-time function
int factorial(int n) pure nothrow @safe {
    return n <= 1 ? 1 : n * factorial(n - 1);
}

static assert(factorial(5) == 120);

// CTFE (Compile-Time Function Execution)
enum FACTORIAL_10 = factorial(10);

// Range-based operations
int[] processRange(int[] items) {
    return items
        .filter!(x => x > 0)
        .map!(x => x * 2)
        .array;
}

// Lazy range
auto lazyRange(int start, int end) {
    return iota(start, end)
        .filter!(x => x % 2 == 0)
        .map!(x => x ^^ 2);
}

// Contract programming
int divide(int a, int b)
in {
    assert(b != 0, "Divisor cannot be zero");
}
out (result) {
    assert(result * b <= a, "Result overflow");
}
do {
    return a / b;
}

// Invariant
class Counter {
    private int value;

    invariant {
        assert(value >= 0, "Counter cannot be negative");
    }

    void increment() {
        value++;
    }

    void decrement()
    in {
        assert(value > 0, "Cannot decrement below zero");
    }
    do {
        value--;
    }
}

// Alias
alias IntArray = int[];
alias StringMap = int[string];
alias ProcessFunc = int function(int);

// Lambda and delegates
int applyOperation(int x, int y, int delegate(int, int) op) {
    return op(x, y);
}

// Struct with operator overloads
struct Vector2D {
    float x, y;

    Vector2D opBinary(string op)(Vector2D other) if (op == "+") {
        return Vector2D(x + other.x, y + other.y);
    }

    Vector2D opBinary(string op)(float scalar) if (op == "*") {
        return Vector2D(x * scalar, y * scalar);
    }

    float opIndex(size_t i) const {
        return i == 0 ? x : y;
    }
}

// Scope guards
void scopeGuardExample() {
    auto resource = acquireResource();
    scope(exit) releaseResource(resource);
    scope(failure) handleFailure();
    scope(success) handleSuccess();

    // Use resource
    doSomething(resource);
}

// Placeholder functions
void* acquireResource() { return null; }
void releaseResource(void* r) {}
void handleFailure() {}
void handleSuccess() {}
void doSomething(void* r) {}

// Unittest
unittest {
    assert(add(2, 3) == 5);
    assert(add(2.5, 3.5) == 6.0);

    auto p = Person("Alice", 30);
    assert(p.isAdult());

    auto processor = new DataProcessor();
    auto results = processor.process([1, 2, 3]);
    assert(results == [2, 4, 6]);
}

// Main function
void main(string[] args) {
    writeln("UAST-Grep D Language Test");

    auto processor = new DataProcessor("Main");

    // Process data
    auto data = [1, 2, 3, 4, 5];
    auto results = processor.process(data);

    processor.log(format("Results: %s", results));

    // Template usage
    writefln("Sum: %d", add(5, 3));
    writefln("Max: %d", findMax([1, 5, 3, 9, 2]));

    // Pair
    auto pair = new Pair!(string, int)("hello", 42);
    writefln("Pair: (%s, %d)", pair.first, pair.second);

    // Lambda
    auto result = applyOperation(5, 3, (a, b) => a * b);
    writefln("Operation result: %d", result);

    // Range operations
    auto processed = processRange([-1, 2, -3, 4, 5]);
    writefln("Processed: %s", processed);

    // Lazy range
    writeln("Lazy even squares:");
    foreach (x; lazyRange(0, 10)) {
        writeln(x);
    }

    // Person
    auto person = Person("Alice", 30, "alice@example.com");
    writefln("Person: %s, Adult: %s", person.toString(), person.isAdult());

    // Vector operations
    auto v1 = Vector2D(1.0, 2.0);
    auto v2 = Vector2D(3.0, 4.0);
    auto v3 = v1 + v2;
    writefln("Vector sum: (%.1f, %.1f)", v3.x, v3.y);

    writeln("Test complete");
}
