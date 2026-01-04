# Crystal Test File for UAST-Grep
# Tests: classes, modules, types, control flow, macros

# Module with constants
module Constants
  MAX_ITEMS   = 100
  DEFAULT_NAME = "UAST-Grep"
  VERSION     = "1.0.0"
end

# Enum
enum Status
  OK         = 200
  NotFound   = 404
  ServerError = 500

  def message : String
    case self
    in OK         then "OK"
    in NotFound   then "Not Found"
    in ServerError then "Server Error"
    end
  end
end

# Struct (value type)
struct Point
  getter x : Int32
  getter y : Int32

  def initialize(@x : Int32, @y : Int32)
  end

  def +(other : Point) : Point
    Point.new(@x + other.x, @y + other.y)
  end

  def distance_from_origin : Float64
    Math.sqrt((@x ** 2 + @y ** 2).to_f)
  end
end

# Record (immutable data class)
record Person,
  name : String,
  age : Int32,
  email : String? = nil do
  def adult? : Bool
    @age >= 18
  end
end

# Abstract class
abstract class Processor
  getter name : String
  property count : Int32 = 0

  def initialize(@name : String = Constants::DEFAULT_NAME)
  end

  abstract def process(items : Array(Int32 | String)) : Array(Int32 | String)

  def log(message : String) : Nil
    puts "[#{@name}] #{message}"
  end
end

# Concrete class
class DataProcessor < Processor
  @cache : Hash(String, Int32 | String)

  def initialize(name : String = "DataProcessor")
    super(name)
    @cache = {} of String => Int32 | String
  end

  def process(items : Array(Int32 | String)) : Array(Int32 | String)
    results = [] of Int32 | String

    # Each with index
    items.each_with_index do |item, index|
      result = transform(item)
      results << result
      @cache[index.to_s] = result
    end

    @count = results.size
    results
  end

  private def transform(item : Int32 | String) : Int32 | String
    # Case with type matching
    case item
    when Int32
      item * 2
    when String
      item.upcase
    end
  end

  def get_cached(key : String) : (Int32 | String)?
    @cache[key]?
  end
end

# Generic class
class Pair(T, U)
  getter first : T
  getter second : U

  def initialize(@first : T, @second : U)
  end

  def swap : Pair(U, T)
    Pair.new(@second, @first)
  end

  def map_first(&block : T -> V) : Pair(V, U) forall V
    Pair.new(yield @first, @second)
  end
end

# Module for mixins
module Loggable
  abstract def name : String

  def log_info(message : String) : Nil
    puts "[INFO] [#{name}] #{message}"
  end

  def log_error(message : String) : Nil
    STDERR.puts "[ERROR] [#{name}] #{message}"
  end
end

# Class including module
class Service
  include Loggable

  getter name : String

  def initialize(@name : String)
  end
end

# Exception classes
class ProcessingError < Exception
end

class ValidationError < Exception
  getter field : String

  def initialize(@field : String, message : String)
    super(message)
  end
end

# Function with blocks
def with_timing(label : String? = nil, &block)
  start = Time.monotonic
  result = yield
  elapsed = Time.monotonic - start
  puts "#{label}: #{elapsed.total_seconds.round(3)}s" if label
  result
end

# Function with generics
def map_items(items : Array(T), &block : T -> U) : Array(U) forall T, U
  items.map { |item| yield item }
end

def filter_items(items : Array(T), &block : T -> Bool) : Array(T) forall T
  items.select { |item| yield item }
end

# Proc and closure
def make_counter(initial : Int32 = 0) : -> Int32
  count = initial
  -> {
    count += 1
    count
  }
end

def make_adder(n : Int32) : Int32 -> Int32
  ->(x : Int32) { x + n }
end

# Macros
macro define_getter(name, type)
  @{{name.id}} : {{type}}

  def {{name.id}} : {{type}}
    @{{name.id}}
  end
end

macro property_with_default(name, type, default)
  @{{name.id}} : {{type}} = {{default}}

  def {{name.id}} : {{type}}
    @{{name.id}}
  end

  def {{name.id}}=(value : {{type}})
    @{{name.id}} = value
  end
end

# Class using macros
class Config
  define_getter version, String
  property_with_default max_items, Int32, 100
  property_with_default enabled, Bool, true

  def initialize(@version : String = Constants::VERSION)
  end
end

# Fiber and channels
def async_example
  channel = Channel(Int32).new

  spawn do
    (1..5).each do |i|
      channel.send(i)
      sleep 0.1.seconds
    end
    channel.close
  end

  while value = channel.receive?
    puts "Received: #{value}"
  end
end

# File I/O with error handling
def risky_operation(filename : String) : String?
  begin
    content = File.read(filename)
    puts "Read #{content.size} bytes"
    content
  rescue ex : File::NotFoundError
    puts "File not found: #{ex.message}"
    nil
  rescue ex : IO::Error
    puts "IO error: #{ex.message}"
    nil
  ensure
    puts "Operation complete"
  end
end

# HTTP client example (type annotations)
def fetch_data(url : String) : String?
  # Would use HTTP::Client in real code
  # response = HTTP::Client.get(url)
  # response.body if response.status_code == 200
  nil
end

# Main entry point
def main
  processor = DataProcessor.new("Main")

  with_timing("Processing") do
    data = [1, 2, 3, "hello", 5] of Int32 | String
    results = processor.process(data)
    processor.log("Results: #{results}")
  end

  # Control flow examples
  value = 42

  # If expression
  result = if value > 0
             "positive"
           elsif value < 0
             "negative"
           else
             "zero"
           end
  puts result

  # Unless
  puts "Not zero" unless value == 0

  # Case expression
  message = case value
            when 0..9   then "single digit"
            when 10..99 then "double digit"
            else             "many digits"
            end
  puts message

  # While loop
  counter = 0
  while counter < 5
    counter += 1
  end

  # Until loop
  counter = 0
  until counter >= 5
    counter += 1
  end

  # Loop with break
  sum = 0
  (1..100).each do |i|
    break if i > 10
    sum += i
  end

  # Times
  5.times { |i| puts "Iteration #{i}" }

  # Person record
  person = Person.new("Alice", 30, "alice@example.com")
  puts "#{person.name} is adult: #{person.adult?}"

  # Generic pair
  pair = Pair.new("hello", 42)
  swapped = pair.swap
  puts "Swapped: (#{swapped.first}, #{swapped.second})"

  # Closures
  counter_fn = make_counter
  puts "Counter: #{counter_fn.call}, #{counter_fn.call}, #{counter_fn.call}"

  adder = make_adder(10)
  puts "Add 10 to 5: #{adder.call(5)}"

  # String interpolation
  name = Constants::DEFAULT_NAME
  puts "Testing #{name} parser"
  puts "Version: #{Constants::VERSION}"
end

# Run main
main
