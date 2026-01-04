#=
Julia Test File for UAST-Grep
Tests: functions, types, variables, control flow, error handling
=#

# Single line comment

# Module definition
module TestModule

# Exports
export DataProcessor, process, calculate_sum
export Person, is_adult

# Constants
const MAX_ITEMS = 100
const DEFAULT_NAME = "UAST-Grep"

# Type alias
const ItemList = Vector{Any}

# Global variables
global_counter = 0
name = "UAST-Grep"
interpolated = "Testing $name parser"
complex = "Length: $(length(name))"

# Abstract type
abstract type AbstractProcessor end

# Struct definition (immutable by default)
struct Person
    name::String
    age::Int
    email::Union{String, Nothing}

    # Inner constructor with validation
    function Person(name::String, age::Int, email::Union{String, Nothing}=nothing)
        age < 0 && throw(ArgumentError("Age cannot be negative"))
        new(name, age, email)
    end
end

# Method for struct
function is_adult(person::Person)::Bool
    person.age >= 18
end

# Mutable struct
mutable struct DataProcessor <: AbstractProcessor
    name::String
    count::Int
    cache::Dict{String, Any}

    # Constructor with default
    function DataProcessor(name::String=DEFAULT_NAME)
        new(name, 0, Dict{String, Any}())
    end
end

# Enum
@enum Status begin
    OK = 200
    NotFound = 404
    ServerError = 500
end

# Process function
function process(processor::DataProcessor, items::ItemList)::ItemList
    results = Any[]

    # For loop with index
    for i in eachindex(items)
        push!(results, transform(items[i]))
    end

    # For-in loop
    for item in items
        processor.cache[string(item)] = item
    end

    # While loop
    counter = 0
    while counter < 10
        counter += 1
    end

    # For loop with range
    for i in 1:10
        # Loop body
    end

    processor.count = length(results)
    return results
end

# Transform function with multiple dispatch
function transform(item::Vector{T}) where T <: Number
    return item .* 2
end

function transform(item::String)
    return uppercase(item)
end

function transform(item::Number)
    return item * 2
end

function transform(item)
    return item
end

# Log function
function log(processor::DataProcessor, message::String)
    println("[$(processor.name)] $message")
end

# Error handling
function risky_operation(processor::DataProcessor, filename::String)
    try
        content = read(filename, String)
        log(processor, "Read $(length(content)) characters")
        return content
    catch e
        if e isa SystemError
            log(processor, "File error: $(e.prefix)")
        else
            log(processor, "Error: $e")
            rethrow(e)
        end
        return nothing
    finally
        log(processor, "Operation complete")
    end
end

# Calculate sum with default parameter
function calculate_sum(a::Number, b::Number=0)::Number
    return a + b
end

# Variadic function
function sum_all(numbers::Number...)::Number
    return sum(numbers)
end

# Higher-order function
function map_items(items, func::Function)
    return map(func, items)
end

# Anonymous functions (lambdas)
const double = x -> x * 2
const add = (x, y) -> x + y

# Closure
function make_counter()
    count = 0
    return () -> begin
        count += 1
        return count
    end
end

# Get status message
function get_status_message(code::Int)::String
    if code == 200
        return "OK"
    elseif code == 404
        return "Not Found"
    elseif code == 500
        return "Server Error"
    else
        return "Unknown"
    end
end

# Ternary operator
function check_positive(n::Number)::String
    return n > 0 ? "Positive" : "Non-positive"
end

# Short-circuit evaluation
function validate(value)
    value === nothing && return "No value"
    isempty(value) && return "Empty"
    return "Valid: $value"
end

# Multiple dispatch demonstration
greet(name::String) = "Hello, $name!"
greet(name::String, greeting::String) = "$greeting, $name!"
greet(names::Vector{String}) = "Hello, $(join(names, ", "))!"

# Parametric type
struct Pair{T, U}
    first::T
    second::U
end

function swap(p::Pair{T, U})::Pair{U, T} where {T, U}
    return Pair(p.second, p.first)
end

# Broadcasting
function broadcast_example(arr::Vector{<:Number})
    doubled = arr .* 2
    squared = arr .^ 2
    condition = arr .> 0
    return (doubled, squared, condition)
end

# Comprehensions
function comprehension_examples()
    # Array comprehension
    squares = [x^2 for x in 1:5]

    # Filtered comprehension
    even_squares = [x^2 for x in 1:10 if iseven(x)]

    # Dictionary comprehension
    word_lengths = Dict(word => length(word) for word in ["hello", "world"])

    # Generator expression
    sum_gen = sum(x^2 for x in 1:100)

    return (squares, even_squares, word_lengths, sum_gen)
end

# String operations
function string_operations()
    s = "Hello, World!"

    # Length
    len = length(s)

    # Substring
    sub = s[1:5]

    # Split and join
    parts = split(s, ", ")
    joined = join(parts, " - ")

    # Contains
    has_world = contains(s, "World")

    # Replace
    replaced = replace(s, "World" => "UAST-Grep")

    # Case
    upper = uppercase(s)
    lower = lowercase(s)

    return (length=len, substring=sub, upper=upper, replaced=replaced)
end

# Tuple operations
function tuple_examples()
    # Named tuple
    point = (x=1, y=2, z=3)

    # Access
    x_val = point.x

    # Destructuring
    (a, b, c) = (1, 2, 3)

    # Multiple return values
    return (point, x_val, a + b + c)
end

# Pattern matching (using macros/dispatch)
function describe(x)
    if x isa Number
        return x > 0 ? "Positive number" : "Non-positive number"
    elseif x isa String
        return "String of length $(length(x))"
    elseif x isa Vector
        return "Vector with $(length(x)) elements"
    else
        return "Unknown type"
    end
end

# Async/concurrent
function async_example()
    # Task
    t = @async begin
        sleep(0.1)
        return 42
    end

    # Fetch result
    result = fetch(t)

    return result
end

# Main function
function main()
    processor = DataProcessor("Main")

    data = Any[1, 2, 3, "hello", [4, 5]]
    results = process(processor, data)

    log(processor, "Processing complete")
    log(processor, "Results: $(join(string.(results), ", "))")

    # Test functions
    println("Sum: $(calculate_sum(5, 3))")
    println("Sum all: $(sum_all(1, 2, 3, 4, 5))")
    println("Status: $(get_status_message(200))")
    println(greet("World"))

    # Higher-order functions
    doubled = map_items([1, 2, 3, 4, 5], x -> x * 2)
    println("Doubled: $(join(doubled, ", "))")

    # Closure
    counter = make_counter()
    println("Counter: $(counter()), $(counter()), $(counter())")

    # Person
    person = Person("Alice", 30, "alice@example.com")
    println("$(person.name) is adult: $(is_adult(person))")

    # Pair
    pair = Pair("hello", 42)
    swapped = swap(pair)
    println("Pair: ($(pair.first), $(pair.second)) -> ($(swapped.first), $(swapped.second))")

    # Comprehensions
    squares, even_squares, _, _ = comprehension_examples()
    println("Squares: $(join(squares, ", "))")
    println("Even squares: $(join(even_squares, ", "))")

    # String operations
    str_ops = string_operations()
    println("Upper: $(str_ops.upper)")
end

end # module

# Run main
using .TestModule
TestModule.main()
