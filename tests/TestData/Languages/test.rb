# Ruby Test File for UAST-Grep
# Tests: functions, classes, modules, variables, control flow, error handling

# Frozen string literal for optimization
# frozen_string_literal: true

# Constants and variables
MAX_ITEMS = 100
$global_counter = 0
local_name = "UAST-Grep"
interpolated = "Testing #{local_name} parser"

# Module definition
module Loggable
  def log(message)
    puts "[#{self.class.name}] #{message}"
  end
end

# Mixin module
module Countable
  attr_accessor :count

  def increment
    @count ||= 0
    @count += 1
  end
end

# Base class
class BaseProcessor
  include Loggable
  include Countable

  attr_reader :name

  def initialize(name)
    @name = name
    @count = 0
  end

  def process(items)
    raise NotImplementedError, "Subclass must implement process"
  end
end

# Derived class with full implementation
class DataProcessor < BaseProcessor
  def initialize(name = "Default")
    super(name)
    @cache = {}
  end

  def process(items)
    results = []

    # For loop (times iterator)
    3.times do |i|
      log("Iteration #{i}")
    end

    # Each iterator (foreach equivalent)
    items.each do |item|
      results << transform(item)
    end

    # Each with index
    items.each_with_index do |item, index|
      @cache[index] = item
    end

    # While loop
    counter = 0
    while counter < 10
      counter += 1
    end

    # Until loop
    until counter >= 20
      counter += 1
    end

    results
  end

  def risky_operation
    # Begin-rescue-ensure error handling
    begin
      file = File.open("test.txt", "r")
      content = file.read
    rescue Errno::ENOENT => e
      log("File not found: #{e.message}")
    rescue StandardError => e
      log("Error: #{e.message}")
    ensure
      file&.close
    end
  end

  private

  def transform(item)
    # Case-when control flow
    case item
    when Array
      item.map { |x| x * 2 }
    when String
      item.upcase
    when Numeric
      item * 2
    else
      item
    end
  end
end

# Standalone function
def calculate_sum(a, b)
  a + b
end

# Lambda and proc
multiply = ->(x, y) { x * y }
add_proc = proc { |x, y| x + y }

# Block with yield
def with_logging(name)
  puts "Starting #{name}"
  result = yield if block_given?
  puts "Finished #{name}"
  result
end

# If-elsif-else with modifiers
def get_status_message(code)
  if code == 200
    "OK"
  elsif code == 404
    "Not Found"
  elsif code == 500
    "Server Error"
  else
    "Unknown"
  end
end

# Ternary and unless
def validate(value)
  return "Invalid" if value.nil?
  return "Empty" unless value.respond_to?(:empty?) && !value.empty?
  value.is_a?(String) ? value.strip : value.to_s
end

# Struct for simple data
Person = Struct.new(:name, :age) do
  def adult?
    age >= 18
  end
end

# Main execution
processor = DataProcessor.new("Main")
data = [1, 2, 3, "hello", [4, 5]]
result = processor.process(data)
processor.log("Processing complete: #{result.inspect}")

with_logging("test") do
  puts "Inside block"
end
