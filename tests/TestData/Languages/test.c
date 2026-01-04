/**
 * C Test File for UAST-Grep
 * Tests: functions, structs, variables, control flow, error handling
 */

/* Multi-line comment block */

// Single line comment
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <errno.h>

// Preprocessor definitions
#define MAX_ITEMS 100
#define DEFAULT_NAME "UAST-Grep"
#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))
#define MAX(a, b) ((a) > (b) ? (a) : (b))

// Type definitions
typedef int (*TransformFunc)(int);
typedef struct DataProcessor DataProcessor;

// Enum definition
typedef enum {
    STATUS_OK = 200,
    STATUS_NOT_FOUND = 404,
    STATUS_SERVER_ERROR = 500
} Status;

// Struct definition
typedef struct {
    char name[64];
    int age;
    char* email;  // Nullable
} Person;

// Forward declaration
struct DataProcessor;

// Struct with function pointers (OOP-like)
typedef struct {
    char name[64];
    int count;
    int* cache;
    size_t cache_size;

    // Method pointers
    int* (*process)(struct DataProcessor* self, int* items, size_t len, size_t* out_len);
    void (*log)(struct DataProcessor* self, const char* message);
    void (*destroy)(struct DataProcessor* self);
} DataProcessor;

// Function prototypes
DataProcessor* DataProcessor_new(const char* name);
int* DataProcessor_process(DataProcessor* self, int* items, size_t len, size_t* out_len);
void DataProcessor_log(DataProcessor* self, const char* message);
void DataProcessor_destroy(DataProcessor* self);
int transform(int value);
const char* get_status_message(Status status);

// Global variables
static int global_counter = 0;
const char* const app_name = DEFAULT_NAME;

// Constructor function
DataProcessor* DataProcessor_new(const char* name) {
    DataProcessor* processor = (DataProcessor*)malloc(sizeof(DataProcessor));
    if (processor == NULL) {
        return NULL;
    }

    strncpy(processor->name, name ? name : DEFAULT_NAME, sizeof(processor->name) - 1);
    processor->name[sizeof(processor->name) - 1] = '\0';
    processor->count = 0;
    processor->cache = NULL;
    processor->cache_size = 0;

    // Assign methods
    processor->process = DataProcessor_process;
    processor->log = DataProcessor_log;
    processor->destroy = DataProcessor_destroy;

    return processor;
}

// Process method
int* DataProcessor_process(DataProcessor* self, int* items, size_t len, size_t* out_len) {
    if (self == NULL || items == NULL || len == 0) {
        *out_len = 0;
        return NULL;
    }

    int* results = (int*)malloc(len * sizeof(int));
    if (results == NULL) {
        *out_len = 0;
        return NULL;
    }

    // For loop
    for (size_t i = 0; i < len; i++) {
        results[i] = transform(items[i]);
    }

    // Cache the results
    if (self->cache != NULL) {
        free(self->cache);
    }
    self->cache = (int*)malloc(len * sizeof(int));
    if (self->cache != NULL) {
        memcpy(self->cache, results, len * sizeof(int));
        self->cache_size = len;
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

    self->count = (int)len;
    *out_len = len;
    return results;
}

// Log method
void DataProcessor_log(DataProcessor* self, const char* message) {
    if (self != NULL && message != NULL) {
        printf("[%s] %s\n", self->name, message);
    }
}

// Destructor
void DataProcessor_destroy(DataProcessor* self) {
    if (self != NULL) {
        if (self->cache != NULL) {
            free(self->cache);
        }
        free(self);
    }
}

// Transform function
int transform(int value) {
    return value * 2;
}

// Function with switch statement
const char* get_status_message(Status status) {
    switch (status) {
        case STATUS_OK:
            return "OK";
        case STATUS_NOT_FOUND:
            return "Not Found";
        case STATUS_SERVER_ERROR:
            return "Server Error";
        default:
            return "Unknown";
    }
}

// Function with error handling
int risky_operation(const char* filename, char* buffer, size_t buffer_size) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        fprintf(stderr, "Error opening file: %s\n", strerror(errno));
        return -1;
    }

    size_t bytes_read = fread(buffer, 1, buffer_size - 1, file);
    if (ferror(file)) {
        fprintf(stderr, "Error reading file: %s\n", strerror(errno));
        fclose(file);
        return -1;
    }

    buffer[bytes_read] = '\0';
    fclose(file);

    return (int)bytes_read;
}

// Variadic function
int sum_all(int count, ...) {
    va_list args;
    va_start(args, count);

    int total = 0;
    for (int i = 0; i < count; i++) {
        total += va_arg(args, int);
    }

    va_end(args);
    return total;
}

// Function pointer usage
int apply_operation(int x, int y, int (*operation)(int, int)) {
    return operation(x, y);
}

int add(int a, int b) { return a + b; }
int multiply(int a, int b) { return a * b; }

// Conditional compilation
#ifdef DEBUG
void debug_print(const char* message) {
    printf("[DEBUG] %s\n", message);
}
#else
#define debug_print(msg) ((void)0)
#endif

// Static function (file scope)
static int calculate_sum(int a, int b) {
    return a + b;
}

// Inline function (C99+)
static inline int square(int x) {
    return x * x;
}

// Recursive function
int factorial(int n) {
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}

// Array operations
void process_array(int* arr, size_t len) {
    // If-else
    if (len == 0) {
        return;
    } else if (len == 1) {
        arr[0] *= 2;
    } else {
        for (size_t i = 0; i < len; i++) {
            arr[i] *= 2;
        }
    }
}

// Pointer arithmetic
void swap(int* a, int* b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

// Main function
int main(int argc, char* argv[]) {
    // Variable declarations
    int data[] = {1, 2, 3, 4, 5};
    size_t data_len = ARRAY_SIZE(data);
    size_t result_len;

    // Create processor
    DataProcessor* processor = DataProcessor_new("Main");
    if (processor == NULL) {
        fprintf(stderr, "Failed to create processor\n");
        return 1;
    }

    // Process data
    int* result = processor->process(processor, data, data_len, &result_len);
    if (result != NULL) {
        processor->log(processor, "Processing complete");

        // Print results
        printf("Results: ");
        for (size_t i = 0; i < result_len; i++) {
            printf("%d ", result[i]);
        }
        printf("\n");

        free(result);
    }

    // Test other functions
    printf("Sum: %d\n", calculate_sum(5, 3));
    printf("Factorial(5): %d\n", factorial(5));
    printf("Status: %s\n", get_status_message(STATUS_OK));

    // Function pointer
    int product = apply_operation(5, 3, multiply);
    printf("Product: %d\n", product);

    // Ternary operator
    int max_val = (5 > 3) ? 5 : 3;
    printf("Max: %d\n", max_val);

    // Cleanup
    processor->destroy(processor);

    return 0;
}
