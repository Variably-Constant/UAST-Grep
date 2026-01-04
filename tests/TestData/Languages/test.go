// Go Test File for UAST-Grep
// Tests: functions, structs, interfaces, variables, control flow, error handling

package main

import (
	"errors"
	"fmt"
	"io"
	"os"
	"strings"
)

// Constants
const (
	MaxItems    = 100
	DefaultName = "UAST-Grep"
)

// Variable declarations
var (
	globalCounter int    = 0
	name          string = "UAST-Grep"
)

// Interface definition
type Processor interface {
	Process(items []interface{}) ([]interface{}, error)
	Log(message string)
}

// Interface for logging
type Loggable interface {
	Log(message string)
}

// Struct definition
type DataProcessor struct {
	name  string
	count int
	cache map[string]interface{}
}

// Constructor function
func NewDataProcessor(name string) *DataProcessor {
	if name == "" {
		name = DefaultName
	}
	return &DataProcessor{
		name:  name,
		count: 0,
		cache: make(map[string]interface{}),
	}
}

// Method implementation
func (p *DataProcessor) Process(items []interface{}) ([]interface{}, error) {
	if items == nil {
		return nil, errors.New("items cannot be nil")
	}

	results := make([]interface{}, 0, len(items))

	// For loop with index
	for i := 0; i < len(items); i++ {
		transformed := p.transform(items[i])
		results = append(results, transformed)
	}

	// Range loop (foreach equivalent)
	for key, value := range items {
		p.cache[fmt.Sprintf("%d", key)] = value
	}

	// While equivalent
	counter := 0
	for counter < 10 {
		counter++
	}

	// Infinite loop with break
	for {
		if counter >= 20 {
			break
		}
		counter++
	}

	return results, nil
}

// Private method (lowercase)
func (p *DataProcessor) transform(item interface{}) interface{} {
	// Type switch
	switch v := item.(type) {
	case []int:
		result := make([]int, len(v))
		for i, n := range v {
			result[i] = n * 2
		}
		return result
	case string:
		return strings.ToUpper(v)
	case int:
		return v * 2
	case float64:
		return v * 2.0
	default:
		return item
	}
}

// Log method
func (p *DataProcessor) Log(message string) {
	fmt.Printf("[%s] %s\n", p.name, message)
}

// Function with error handling
func (p *DataProcessor) RiskyOperation() error {
	file, err := os.Open("test.txt")
	if err != nil {
		return fmt.Errorf("failed to open file: %w", err)
	}
	defer file.Close()

	content, err := io.ReadAll(file)
	if err != nil {
		return fmt.Errorf("failed to read file: %w", err)
	}

	p.Log(fmt.Sprintf("Read %d bytes", len(content)))
	return nil
}

// Function with multiple return values
func calculateSumAndProduct(a, b int) (sum int, product int) {
	sum = a + b
	product = a * b
	return
}

// Variadic function
func sumAll(numbers ...int) int {
	total := 0
	for _, n := range numbers {
		total += n
	}
	return total
}

// Higher-order function
func applyOperation(x, y int, op func(int, int) int) int {
	return op(x, y)
}

// Closure
func counter() func() int {
	count := 0
	return func() int {
		count++
		return count
	}
}

// Generic function (Go 1.18+)
func Map[T, U any](items []T, fn func(T) U) []U {
	result := make([]U, len(items))
	for i, item := range items {
		result[i] = fn(item)
	}
	return result
}

// Generic struct
type Pair[T, U any] struct {
	First  T
	Second U
}

// Select with channels
func channelExample(ch chan int, done chan bool) {
	for {
		select {
		case val := <-ch:
			fmt.Println("Received:", val)
		case <-done:
			return
		default:
			// Non-blocking
		}
	}
}

// Main function
func main() {
	processor := NewDataProcessor("Main")

	data := []interface{}{1, 2, 3, "hello", []int{4, 5}}
	result, err := processor.Process(data)

	if err != nil {
		processor.Log(fmt.Sprintf("Error: %v", err))
		return
	}

	processor.Log(fmt.Sprintf("Processing complete: %v", result))

	// Test other functions
	sum, product := calculateSumAndProduct(5, 3)
	fmt.Printf("Sum: %d, Product: %d\n", sum, product)

	total := sumAll(1, 2, 3, 4, 5)
	fmt.Println("Total:", total)

	// Closure usage
	next := counter()
	fmt.Println(next(), next(), next())

	// Generic usage
	doubled := Map([]int{1, 2, 3}, func(x int) int { return x * 2 })
	fmt.Println("Doubled:", doubled)
}
