# Elixir Test File for UAST-Grep
# Tests: functions, modules, variables, control flow, error handling

defmodule Test do
  @moduledoc """
  Test module for UAST-Grep parser testing.
  Demonstrates Elixir language features.
  """

  # Module attributes (constants)
  @max_items 100
  @default_name "UAST-Grep"

  # Type specifications
  @type item_list :: [any()]
  @type status :: :ok | :not_found | :server_error
  @type result :: {:ok, any()} | {:error, any()}

  # Struct definition
  defmodule Person do
    @enforce_keys [:name, :age]
    defstruct [:name, :age, email: nil]

    @type t :: %__MODULE__{
      name: String.t(),
      age: non_neg_integer(),
      email: String.t() | nil
    }

    @spec is_adult?(t()) :: boolean()
    def is_adult?(%__MODULE__{age: age}), do: age >= 18
  end

  # GenServer implementation
  defmodule Processor do
    use GenServer

    # Client API
    def start_link(name \\ @default_name) do
      GenServer.start_link(__MODULE__, name, name: __MODULE__)
    end

    def process(items) do
      GenServer.call(__MODULE__, {:process, items})
    end

    def stop do
      GenServer.stop(__MODULE__)
    end

    # Server callbacks
    @impl true
    def init(name) do
      log(name, "Initializing processor")
      {:ok, %{name: name, count: 0, cache: %{}}}
    end

    @impl true
    def handle_call({:process, items}, _from, state) do
      results = process_items(items)
      new_state = %{state | count: length(results)}
      {:reply, {:ok, results}, new_state}
    end

    defp log(name, message) do
      IO.puts("[#{name}] #{message}")
    end
  end

  # Public functions
  @doc """
  Process a list of items and return transformed results.
  """
  @spec process_items(item_list()) :: item_list()
  def process_items(items) do
    # List comprehension
    results = for item <- items, do: transform(item)

    # Enum operations
    Enum.each(items, fn item ->
      IO.puts("Processing: #{inspect(item)}")
    end)

    results
  end

  @doc """
  Transform a single item based on its type.
  """
  @spec transform(any()) :: any()
  def transform(item) when is_list(item) and is_integer(hd(item)) do
    Enum.map(item, &(&1 * 2))
  end

  def transform(item) when is_binary(item) do
    String.upcase(item)
  end

  def transform(item) when is_integer(item) do
    item * 2
  end

  def transform(item) when is_float(item) do
    item * 2.0
  end

  def transform(item), do: item

  @doc """
  Get status message using case expression.
  """
  @spec get_status_message(status()) :: String.t()
  def get_status_message(status) do
    case status do
      :ok -> "OK"
      :not_found -> "Not Found"
      :server_error -> "Server Error"
      _ -> "Unknown"
    end
  end

  @doc """
  Calculate sum of two numbers.
  """
  @spec calculate_sum(number(), number()) :: number()
  def calculate_sum(a, b), do: a + b

  @doc """
  Risky operation with error handling.
  """
  @spec risky_operation(String.t()) :: result()
  def risky_operation(filename) do
    try do
      content = File.read!(filename)
      length = byte_size(content)
      IO.puts("[#{@default_name}] Read #{length} bytes")
      {:ok, content}
    rescue
      e in File.Error ->
        IO.puts("[#{@default_name}] File error: #{inspect(e)}")
        {:error, :file_error}
      e in RuntimeError ->
        IO.puts("[#{@default_name}] Error: #{e.message}")
        {:error, e.message}
    catch
      :exit, reason ->
        IO.puts("[#{@default_name}] Exit: #{inspect(reason)}")
        {:error, :exit}
    after
      IO.puts("[#{@default_name}] Operation complete")
    end
  end

  @doc """
  Pattern matching with with expression.
  """
  @spec safe_divide(number(), number()) :: result()
  def safe_divide(a, b) do
    with true <- is_number(a),
         true <- is_number(b),
         false <- b == 0 do
      {:ok, a / b}
    else
      false -> {:error, :not_a_number}
      true -> {:error, :division_by_zero}
    end
  end

  @doc """
  Cond expression example.
  """
  @spec describe(any()) :: String.t()
  def describe(value) do
    cond do
      is_integer(value) and value > 0 -> "Positive integer"
      is_integer(value) and value < 0 -> "Negative integer"
      is_integer(value) -> "Zero"
      is_list(value) -> "List with #{length(value)} elements"
      is_map(value) -> "Map with #{map_size(value)} keys"
      true -> "Unknown type"
    end
  end

  @doc """
  If expression example.
  """
  @spec check_positive(number()) :: String.t()
  def check_positive(n) do
    if n > 0 do
      "Positive"
    else
      "Not positive"
    end
  end

  @doc """
  Unless expression example.
  """
  @spec check_empty(list()) :: String.t()
  def check_empty(list) do
    unless Enum.empty?(list) do
      "Has elements"
    else
      "Empty"
    end
  end

  # Private functions
  defp double(x), do: x * 2

  # Higher-order functions
  @spec map_items(item_list(), (any() -> any())) :: item_list()
  def map_items(items, func) do
    Enum.map(items, func)
  end

  @spec filter_items(item_list(), (any() -> boolean())) :: item_list()
  def filter_items(items, func) do
    Enum.filter(items, func)
  end

  @spec reduce_items(item_list(), any(), (any(), any() -> any())) :: any()
  def reduce_items(items, initial, func) do
    Enum.reduce(items, initial, func)
  end

  # Pipe operator examples
  @spec process_numbers(list(number())) :: list(number())
  def process_numbers(numbers) do
    numbers
    |> Enum.filter(&(&1 > 0))
    |> Enum.map(&(&1 * 2))
    |> Enum.sort()
    |> Enum.take(5)
  end

  # Anonymous function examples
  def anonymous_function_examples do
    # Short syntax
    double = &(&1 * 2)

    # Full syntax
    add = fn a, b -> a + b end

    # Capture operator
    upcase = &String.upcase/1

    {double.(5), add.(2, 3), upcase.("hello")}
  end

  # String interpolation
  @spec greeting(String.t()) :: String.t()
  def greeting(name) do
    "Hello, #{name}! Welcome to #{@default_name}."
  end

  # Multi-line string
  @spec help_text() :: String.t()
  def help_text do
    """
    This is a multi-line string.
    It preserves formatting and allows #{@default_name} interpolation.
    """
  end

  # Sigils
  def sigil_examples do
    # String sigil
    string = ~s(String with "quotes")

    # Charlist sigil
    charlist = ~c(charlist)

    # Word list sigil
    words = ~w(one two three)

    # Regex sigil
    regex = ~r/pattern/i

    {string, charlist, words, regex}
  end

  # Recursion with pattern matching
  @spec factorial(non_neg_integer()) :: non_neg_integer()
  def factorial(0), do: 1
  def factorial(n) when n > 0, do: n * factorial(n - 1)

  # Tail-recursive version
  @spec factorial_tail(non_neg_integer()) :: non_neg_integer()
  def factorial_tail(n), do: do_factorial(n, 1)

  defp do_factorial(0, acc), do: acc
  defp do_factorial(n, acc), do: do_factorial(n - 1, n * acc)
end

# Main execution
defmodule Main do
  def run do
    items = [1, 2, 3, "hello", [4, 5]]
    results = Test.process_items(items)
    IO.puts("Results: #{inspect(results)}")

    # Person struct
    person = %Test.Person{name: "Alice", age: 30, email: "alice@example.com"}
    IO.puts("#{person.name} is adult: #{Test.Person.is_adult?(person)}")

    # Pattern matching
    case Test.safe_divide(10, 2) do
      {:ok, result} -> IO.puts("Result: #{result}")
      {:error, reason} -> IO.puts("Error: #{reason}")
    end

    # Pipe operator
    numbers = [5, -1, 3, -2, 8, 1]
    processed = Test.process_numbers(numbers)
    IO.puts("Processed: #{inspect(processed)}")

    IO.puts(Test.greeting("World"))
  end
end
