/**
 * JavaScript Test File for UAST-Grep
 * Tests: functions, classes, variables, control flow, error handling
 */

// Single line comment
'use strict';

// Constants and variables
const MAX_ITEMS = 100;
let globalCounter = 0;
var legacyVar = "deprecated";
const name = "UAST-Grep";
const interpolated = `Testing ${name} parser`;

// Object destructuring
const { log, error } = console;

// Array destructuring
const [first, second, ...rest] = [1, 2, 3, 4, 5];

// Class definition with ES6+ features
class BaseProcessor {
    #privateField = 0; // Private field
    static staticCounter = 0;

    constructor(name) {
        this.name = name;
        this.count = 0;
        BaseProcessor.staticCounter++;
    }

    process(items) {
        throw new Error("Must implement process method");
    }

    log(message) {
        console.log(`[${this.name}] ${message}`);
    }

    get privateValue() {
        return this.#privateField;
    }

    set privateValue(value) {
        this.#privateField = value;
    }
}

// Derived class
class DataProcessor extends BaseProcessor {
    #cache = new Map();

    constructor(name = "Default") {
        super(name);
    }

    process(items) {
        const results = [];

        // For loop
        for (let i = 0; i < items.length; i++) {
            results.push(this.#transform(items[i]));
        }

        // For-of loop
        for (const item of items) {
            this.#cache.set(String(item), item);
        }

        // For-in loop
        const obj = { a: 1, b: 2, c: 3 };
        for (const key in obj) {
            if (Object.hasOwnProperty.call(obj, key)) {
                this.log(`Key: ${key}`);
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

    #transform(item) {
        // If-else control flow
        if (Array.isArray(item)) {
            return item.map(x => x * 2);
        } else if (typeof item === 'string') {
            return item.toUpperCase();
        } else if (typeof item === 'number') {
            return item * 2;
        } else {
            return item;
        }
    }

    async riskyOperation() {
        // Try-catch-finally error handling
        try {
            const response = await fetch('https://api.example.com/data');
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
            const data = await response.json();
            this.log(`Received: ${JSON.stringify(data)}`);
        } catch (error) {
            this.log(`Error: ${error.message}`);
        } finally {
            this.log('Operation complete');
        }
    }
}

// Regular function
function calculateSum(a, b) {
    return a + b;
}

// Arrow function
const multiply = (x, y) => x * y;

// Arrow function with implicit return
const double = x => x * 2;

// Function with default parameters
function greet(name = "World") {
    return `Hello, ${name}!`;
}

// Function with rest parameters
function sumAll(...numbers) {
    return numbers.reduce((acc, n) => acc + n, 0);
}

// Generator function
function* numberGenerator(n) {
    for (let i = 0; i < n; i++) {
        yield i;
    }
}

// Async function
async function asyncOperation(data) {
    return new Promise((resolve, reject) => {
        setTimeout(() => {
            resolve(`Processed: ${data}`);
        }, 100);
    });
}

// IIFE (Immediately Invoked Function Expression)
const result = (function() {
    return "IIFE result";
})();

// Switch statement
function getStatusMessage(code) {
    switch (code) {
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

// Ternary operator
const isAdult = (age) => age >= 18 ? "Adult" : "Minor";

// Nullish coalescing and optional chaining
const getValue = (obj) => obj?.nested?.value ?? "default";

// Spread operator
const mergedArray = [...[1, 2], ...[3, 4]];
const mergedObject = { ...{ a: 1 }, ...{ b: 2 } };

// Object methods
const person = {
    name: "Alice",
    age: 30,
    greet() {
        return `Hello, I'm ${this.name}`;
    },
    get info() {
        return `${this.name}, ${this.age}`;
    }
};

// Symbol
const uniqueKey = Symbol('unique');

// Map and Set
const map = new Map([['key1', 'value1'], ['key2', 'value2']]);
const set = new Set([1, 2, 3, 3, 4]);

// Proxy
const handler = {
    get(target, prop) {
        return prop in target ? target[prop] : `Property ${prop} not found`;
    }
};
const proxy = new Proxy({}, handler);

// Main execution
const processor = new DataProcessor("Main");
const data = [1, 2, 3, "hello", [4, 5]];
const processed = processor.process(data);
processor.log(`Processing complete: ${JSON.stringify(processed)}`);

// Test functions
console.log("Sum:", calculateSum(5, 3));
console.log("Product:", multiply(5, 3));
console.log("Sum all:", sumAll(1, 2, 3, 4, 5));

// Generator usage
for (const num of numberGenerator(5)) {
    console.log("Generated:", num);
}

// Export (ES modules)
export { DataProcessor, calculateSum, multiply };
export default DataProcessor;
