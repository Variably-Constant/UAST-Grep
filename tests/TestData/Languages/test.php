<?php
/**
 * PHP Test File for UAST-Grep
 * Tests: functions, classes, variables, control flow, error handling
 */

// Single line comment
declare(strict_types=1);

namespace TestNamespace;

// Constants and variables
const MAX_ITEMS = 100;
$globalCounter = 0;
$name = "UAST-Grep";
$interpolated = "Testing {$name} parser";

/**
 * Interface definition
 */
interface Loggable {
    public function log(string $message): void;
}

/**
 * Abstract base class
 */
abstract class BaseProcessor implements Loggable {
    protected string $name;
    private int $count = 0;

    public function __construct(string $name) {
        $this->name = $name;
    }

    abstract public function process(array $items): array;

    public function log(string $message): void {
        echo "[{$this->name}] $message\n";
    }
}

/**
 * Concrete class with full implementation
 */
class DataProcessor extends BaseProcessor {
    private array $cache = [];

    public function __construct(string $name = "Default") {
        parent::__construct($name);
    }

    public function process(array $items): array {
        $results = [];

        // For loop
        for ($i = 0; $i < count($items); $i++) {
            $results[] = $this->transform($items[$i]);
        }

        // Foreach loop
        foreach ($items as $key => $value) {
            $this->cache[$key] = $value;
        }

        // While loop
        $counter = 0;
        while ($counter < 10) {
            $counter++;
        }

        return $results;
    }

    private function transform(mixed $item): mixed {
        // If-else control flow
        if (is_array($item)) {
            return array_map(fn($x) => $x * 2, $item);
        } elseif (is_string($item)) {
            return strtoupper($item);
        } else {
            return $item;
        }
    }

    public function riskyOperation(): void {
        // Try-catch-finally error handling
        try {
            $file = fopen("test.txt", "r");
            if ($file === false) {
                throw new \RuntimeException("Failed to open file");
            }
            $content = fread($file, 1024);
        } catch (\RuntimeException $e) {
            $this->log("Error: " . $e->getMessage());
        } catch (\Exception $e) {
            $this->log("General error: " . $e->getMessage());
        } finally {
            if (isset($file) && $file !== false) {
                fclose($file);
            }
        }
    }
}

// Function with typed parameters and return
function calculateSum(int $a, int $b): int {
    return $a + $b;
}

// Arrow function (PHP 7.4+)
$multiply = fn(int $x, int $y): int => $x * $y;

// Switch statement
function getStatusMessage(int $code): string {
    switch ($code) {
        case 200:
            return "OK";
        case 404:
            return "Not Found";
        case 500:
            return "Server Error";
        default:
            return "Unknown";
    }
}

// Match expression (PHP 8.0+)
function getHttpStatus(int $code): string {
    return match($code) {
        200 => "OK",
        404 => "Not Found",
        500 => "Internal Server Error",
        default => "Unknown Status"
    };
}

// Main execution
$processor = new DataProcessor("Main");
$data = [1, 2, 3, "hello", [4, 5]];
$result = $processor->process($data);
$processor->log("Processing complete");
