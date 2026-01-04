/**
 * Java Test File for UAST-Grep
 * Tests: functions, classes, interfaces, variables, control flow, error handling
 */

package com.example.test;

import java.io.*;
import java.util.*;
import java.util.function.*;
import java.util.stream.*;

// Interface definition
interface Processor {
    List<Object> process(List<Object> items) throws ProcessingException;
    void log(String message);
}

// Custom exception
class ProcessingException extends Exception {
    public ProcessingException(String message) {
        super(message);
    }

    public ProcessingException(String message, Throwable cause) {
        super(message, cause);
    }
}

// Abstract base class
abstract class BaseProcessor implements Processor {
    protected final String name;
    protected int count;

    public BaseProcessor(String name) {
        this.name = name;
        this.count = 0;
    }

    public abstract List<Object> process(List<Object> items) throws ProcessingException;

    @Override
    public void log(String message) {
        System.out.printf("[%s] %s%n", name, message);
    }
}

// Enum definition
enum Status {
    OK(200, "OK"),
    NOT_FOUND(404, "Not Found"),
    SERVER_ERROR(500, "Server Error");

    private final int code;
    private final String message;

    Status(int code, String message) {
        this.code = code;
        this.message = message;
    }

    public int getCode() { return code; }
    public String getMessage() { return message; }
}

// Record (Java 16+)
record Person(String name, int age, String email) {
    public boolean isAdult() {
        return age >= 18;
    }
}

// Main class
public class test {
    // Constants
    private static final int MAX_ITEMS = 100;
    private static final String DEFAULT_NAME = "UAST-Grep";

    // Instance variables
    private final String name;
    private Map<String, Object> cache;

    // Constructor
    public test(String name) {
        this.name = name != null ? name : DEFAULT_NAME;
        this.cache = new HashMap<>();
    }

    // Method with generics
    public <T> List<T> processItems(List<T> items, Function<T, T> transformer) {
        List<T> results = new ArrayList<>();

        // Traditional for loop
        for (int i = 0; i < items.size(); i++) {
            results.add(transformer.apply(items.get(i)));
        }

        // Enhanced for loop (foreach)
        for (T item : items) {
            cache.put(item.toString(), item);
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

    // Method with streams
    public List<Integer> processWithStreams(List<Integer> numbers) {
        return numbers.stream()
            .filter(n -> n > 0)
            .map(n -> n * 2)
            .sorted()
            .collect(Collectors.toList());
    }

    // Method with error handling
    public String riskyOperation() {
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new FileReader("test.txt"));
            StringBuilder content = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                content.append(line).append("\n");
            }
            return content.toString();
        } catch (FileNotFoundException e) {
            System.err.println("File not found: " + e.getMessage());
            return null;
        } catch (IOException e) {
            System.err.println("IO error: " + e.getMessage());
            return null;
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    System.err.println("Error closing reader: " + e.getMessage());
                }
            }
        }
    }

    // Try-with-resources
    public String safeFileRead(String path) throws IOException {
        try (BufferedReader reader = new BufferedReader(new FileReader(path))) {
            return reader.lines().collect(Collectors.joining("\n"));
        }
    }

    // Switch expression (Java 14+)
    public String getStatusMessage(int code) {
        return switch (code) {
            case 200 -> "OK";
            case 404 -> "Not Found";
            case 500 -> "Server Error";
            default -> "Unknown";
        };
    }

    // Pattern matching (Java 16+)
    public String describe(Object obj) {
        if (obj instanceof String s) {
            return "String of length " + s.length();
        } else if (obj instanceof Integer i) {
            return "Integer: " + i;
        } else if (obj instanceof List<?> list) {
            return "List with " + list.size() + " elements";
        }
        return "Unknown type";
    }

    // Static method
    public static int calculateSum(int a, int b) {
        return a + b;
    }

    // Varargs method
    public static int sumAll(int... numbers) {
        int total = 0;
        for (int n : numbers) {
            total += n;
        }
        return total;
    }

    // Lambda examples
    private final BiFunction<Integer, Integer, Integer> multiply = (x, y) -> x * y;

    // Main method
    public static void main(String[] args) {
        test processor = new test("Main");

        List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);
        List<Integer> doubled = processor.processItems(numbers, n -> n * 2);

        System.out.println("Doubled: " + doubled);
        System.out.println("Sum: " + calculateSum(5, 3));
        System.out.println("Sum all: " + sumAll(1, 2, 3, 4, 5));

        // Record usage
        Person person = new Person("Alice", 30, "alice@example.com");
        System.out.printf("%s is adult: %b%n", person.name(), person.isAdult());

        // Stream operations
        List<Integer> processed = processor.processWithStreams(numbers);
        System.out.println("Processed: " + processed);
    }
}
