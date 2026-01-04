/**
 * TypeScript Test File for UAST-Grep
 * Tests: functions, classes, interfaces, variables, control flow, error handling
 */

// Single line comment
import { EventEmitter } from 'events';

// Constants and variables
const MAX_ITEMS: number = 100;
let globalCounter: number = 0;
const name: string = "UAST-Grep";
const interpolated: string = `Testing ${name} parser`;

// Type definitions
type ItemList = Array<unknown>;
type Callback<T> = (item: T) => void;

// Interface definition
interface Processor {
    process(items: ItemList): ItemList;
    log(message: string): void;
}

// Interface with optional and readonly properties
interface Config {
    readonly name: string;
    maxItems?: number;
    enabled: boolean;
}

// Enum
enum Status {
    Ok = 200,
    NotFound = 404,
    ServerError = 500,
}

// Abstract class
abstract class BaseProcessor implements Processor {
    protected name: string;
    protected count: number = 0;

    constructor(name: string) {
        this.name = name;
    }

    abstract process(items: ItemList): ItemList;

    log(message: string): void {
        console.log(`[${this.name}] ${message}`);
    }
}

// Concrete class with full implementation
class DataProcessor extends BaseProcessor {
    private cache: Map<string, unknown> = new Map();

    constructor(name: string = "Default") {
        super(name);
    }

    process(items: ItemList): ItemList {
        const results: unknown[] = [];

        // For loop
        for (let i = 0; i < items.length; i++) {
            results.push(this.transform(items[i]));
        }

        // For-of loop
        for (const item of items) {
            this.cache.set(String(item), item);
        }

        // For-in loop
        const obj = { a: 1, b: 2, c: 3 };
        for (const key in obj) {
            if (Object.prototype.hasOwnProperty.call(obj, key)) {
                console.log(key);
            }
        }

        // While loop
        let counter = 0;
        while (counter < 10) {
            counter++;
        }

        // Do-while loop
        do {
            counter++;
        } while (counter < 20);

        return results;
    }

    private transform(item: unknown): unknown {
        // If-else control flow
        if (Array.isArray(item)) {
            return item.map((x: number) => x * 2);
        } else if (typeof item === 'string') {
            return item.toUpperCase();
        } else if (typeof item === 'number') {
            return item * 2;
        } else {
            return item;
        }
    }

    async riskyOperation(): Promise<void> {
        // Try-catch-finally error handling
        try {
            const response = await fetch('https://api.example.com/data');
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
            const data = await response.json();
            this.log(`Received: ${JSON.stringify(data)}`);
        } catch (error) {
            if (error instanceof Error) {
                this.log(`Error: ${error.message}`);
            }
        } finally {
            this.log('Operation complete');
        }
    }
}

// Generic class
class Pair<T, U> {
    constructor(public first: T, public second: U) {}

    swap(): Pair<U, T> {
        return new Pair(this.second, this.first);
    }
}

// Generic function
function mapArray<T, U>(items: T[], fn: (item: T) => U): U[] {
    return items.map(fn);
}

// Function with default parameters
function calculateSum(a: number, b: number = 0): number {
    return a + b;
}

// Arrow function
const multiply = (x: number, y: number): number => x * y;

// Function with rest parameters
function sumAll(...numbers: number[]): number {
    return numbers.reduce((acc, n) => acc + n, 0);
}

// Function overloads
function format(value: string): string;
function format(value: number): string;
function format(value: string | number): string {
    if (typeof value === 'string') {
        return value.trim();
    }
    return value.toFixed(2);
}

// Switch statement
function getStatusMessage(code: Status): string {
    switch (code) {
        case Status.Ok:
            return "OK";
        case Status.NotFound:
            return "Not Found";
        case Status.ServerError:
            return "Server Error";
        default:
            return "Unknown";
    }
}

// Type guards
function isString(value: unknown): value is string {
    return typeof value === 'string';
}

// Decorator (experimental)
function logged(target: any, propertyKey: string, descriptor: PropertyDescriptor) {
    const originalMethod = descriptor.value;
    descriptor.value = function(...args: any[]) {
        console.log(`Calling ${propertyKey}`);
        const result = originalMethod.apply(this, args);
        console.log(`Finished ${propertyKey}`);
        return result;
    };
    return descriptor;
}

// Record type
interface Person {
    name: string;
    age: number;
    email?: string;
}

// Utility types usage
type ReadonlyPerson = Readonly<Person>;
type PartialPerson = Partial<Person>;
type PersonKeys = keyof Person;

// Conditional type
type NonNullable<T> = T extends null | undefined ? never : T;

// Main execution
const processor = new DataProcessor("Main");
const data: ItemList = [1, 2, 3, "hello", [4, 5]];
const result = processor.process(data);
processor.log(`Processing complete: ${JSON.stringify(result)}`);

// Generic usage
const pair = new Pair<string, number>("hello", 42);
console.log(`Pair: ${pair.first}, ${pair.second}`);

const doubled = mapArray([1, 2, 3], x => x * 2);
console.log(`Doubled: ${doubled}`);

// Export
export { DataProcessor, Processor, Status };
