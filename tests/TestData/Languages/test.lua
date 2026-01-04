--[[
    Lua Test File for UAST-Grep
    Tests: functions, tables, variables, control flow, error handling
--]]

-- Single line comment

-- Module definition (pattern)
local Test = {}

-- Constants
local MAX_ITEMS = 100
local DEFAULT_NAME = "UAST-Grep"

-- Local variables
local globalCounter = 0
local name = "UAST-Grep"
local interpolated = string.format("Testing %s parser", name)

-- Long string literal
local longString = [[
This is a long string literal.
It can span multiple lines.
No escaping needed for "quotes" or 'apostrophes'.
]]

-- Table as class (OOP pattern)
local DataProcessor = {}
DataProcessor.__index = DataProcessor

-- Constructor
function DataProcessor.new(processorName)
    local self = setmetatable({}, DataProcessor)
    self.name = processorName or DEFAULT_NAME
    self.count = 0
    self.cache = {}
    return self
end

-- Method
function DataProcessor:process(items)
    local results = {}

    -- Numeric for loop
    for i = 1, #items do
        local transformed = self:transform(items[i])
        table.insert(results, transformed)
    end

    -- Generic for loop (ipairs)
    for index, item in ipairs(items) do
        self.cache[tostring(item)] = item
    end

    -- While loop
    local counter = 0
    while counter < 10 do
        counter = counter + 1
    end

    -- Repeat-until loop
    repeat
        counter = counter + 1
    until counter >= 20

    self.count = #results
    return results
end

-- Private method (by convention, underscore prefix)
function DataProcessor:transform(item)
    -- Type checking with if-elseif-else
    local itemType = type(item)

    if itemType == "table" then
        local result = {}
        for i, v in ipairs(item) do
            result[i] = v * 2
        end
        return result
    elseif itemType == "string" then
        return string.upper(item)
    elseif itemType == "number" then
        return item * 2
    else
        return item
    end
end

-- Log method
function DataProcessor:log(message)
    print(string.format("[%s] %s", self.name, message))
end

-- Error handling method
function DataProcessor:riskyOperation(filename)
    -- pcall for error handling
    local success, result = pcall(function()
        local file, err = io.open(filename, "r")
        if not file then
            error("Failed to open file: " .. (err or "unknown error"))
        end

        local content = file:read("*all")
        file:close()

        return content
    end)

    if success then
        self:log(string.format("Read %d characters", #result))
        return result
    else
        self:log(string.format("Error: %s", result))
        return nil, result
    end
end

-- Standalone function
local function calculateSum(a, b)
    return a + b
end

-- Variadic function
local function sumAll(...)
    local args = {...}
    local total = 0
    for _, v in ipairs(args) do
        total = total + v
    end
    return total
end

-- Higher-order function
local function map(tbl, func)
    local result = {}
    for i, v in ipairs(tbl) do
        result[i] = func(v)
    end
    return result
end

-- Closure
local function counter()
    local count = 0
    return function()
        count = count + 1
        return count
    end
end

-- Anonymous function (lambda)
local double = function(x) return x * 2 end

-- Multiple return values
local function divmod(a, b)
    return math.floor(a / b), a % b
end

-- Function with table argument (named parameters)
local function greet(options)
    local name = options.name or "World"
    local greeting = options.greeting or "Hello"
    return string.format("%s, %s!", greeting, name)
end

-- Status message using lookup table
local statusMessages = {
    [200] = "OK",
    [404] = "Not Found",
    [500] = "Server Error"
}

local function getStatusMessage(code)
    return statusMessages[code] or "Unknown"
end

-- Metatable example
local Vector = {}
Vector.__index = Vector

function Vector.new(x, y)
    return setmetatable({x = x, y = y}, Vector)
end

function Vector.__add(a, b)
    return Vector.new(a.x + b.x, a.y + b.y)
end

function Vector.__tostring(v)
    return string.format("Vector(%d, %d)", v.x, v.y)
end

-- Coroutine example
local function producer()
    return coroutine.create(function()
        for i = 1, 5 do
            coroutine.yield(i)
        end
    end)
end

-- Module pattern with private state
local function createModule()
    local private = {}
    local public = {}

    private.data = {}

    function public.add(item)
        table.insert(private.data, item)
    end

    function public.get(index)
        return private.data[index]
    end

    function public.size()
        return #private.data
    end

    return public
end

-- Table operations
local function tableOperations()
    local t = {1, 2, 3, 4, 5}

    -- Insert at end
    table.insert(t, 6)

    -- Insert at position
    table.insert(t, 1, 0)

    -- Remove
    table.remove(t)

    -- Sort
    table.sort(t)

    -- Concatenate
    local str = table.concat(t, ", ")

    return t, str
end

-- String operations
local function stringOperations()
    local s = "Hello, World!"

    -- Length
    local len = #s

    -- Substring
    local sub = string.sub(s, 1, 5)

    -- Find
    local start, finish = string.find(s, "World")

    -- Replace
    local replaced = string.gsub(s, "World", "UAST-Grep")

    -- Pattern matching
    for word in string.gmatch(s, "%w+") do
        print(word)
    end

    -- Format
    local formatted = string.format("Length: %d", len)

    return sub, replaced, formatted
end

-- Person table
local Person = {}
Person.__index = Person

function Person.new(personName, age, email)
    local self = setmetatable({}, Person)
    self.name = personName
    self.age = age
    self.email = email
    return self
end

function Person:isAdult()
    return self.age >= 18
end

-- Main execution
local function main()
    local processor = DataProcessor.new("Main")

    local data = {1, 2, 3, "hello", {4, 5}}
    local results = processor:process(data)

    processor:log("Processing complete")
    processor:log(string.format("Results: %s", table.concat(map(results, tostring), ", ")))

    -- Test functions
    print(string.format("Sum: %d", calculateSum(5, 3)))
    print(string.format("Sum all: %d", sumAll(1, 2, 3, 4, 5)))
    print(string.format("Status: %s", getStatusMessage(200)))
    print(greet{name = "UAST-Grep"})

    -- Multiple return values
    local quot, rem = divmod(17, 5)
    print(string.format("17 / 5 = %d remainder %d", quot, rem))

    -- Closure
    local next = counter()
    print(next(), next(), next())

    -- Person
    local person = Person.new("Alice", 30, "alice@example.com")
    print(string.format("%s is adult: %s", person.name, tostring(person:isAdult())))

    -- Vector with metatable
    local v1 = Vector.new(1, 2)
    local v2 = Vector.new(3, 4)
    local v3 = v1 + v2
    print(tostring(v3))

    -- Coroutine
    local co = producer()
    for i = 1, 5 do
        local _, value = coroutine.resume(co)
        print(string.format("Produced: %d", value))
    end
end

-- Run main
main()

-- Export module
Test.DataProcessor = DataProcessor
Test.calculateSum = calculateSum
Test.map = map

return Test
