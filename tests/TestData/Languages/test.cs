/**
 * C# Test File for UAST-Grep
 * Tests: functions, classes, interfaces, variables, control flow, error handling
 */

// Single line comment
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace TestNamespace;

// Interface definition
public interface IProcessor
{
    List<object> Process(List<object> items);
    void Log(string message);
}

// Interface with default implementation
public interface ILoggable
{
    void Log(string message) => Console.WriteLine($"[Default] {message}");
}

// Abstract class
public abstract class BaseProcessor : IProcessor, ILoggable
{
    protected readonly string _name;
    protected int _count = 0;

    protected BaseProcessor(string name)
    {
        _name = name;
    }

    public abstract List<object> Process(List<object> items);

    public void Log(string message)
    {
        Console.WriteLine($"[{_name}] {message}");
    }
}

// Record type (C# 9+)
public record Person(string Name, int Age, string? Email = null)
{
    public bool IsAdult => Age >= 18;
}

// Record struct (C# 10+)
public readonly record struct Point(int X, int Y);

// Enum with attributes
public enum Status
{
    Ok = 200,
    NotFound = 404,
    ServerError = 500
}

// Primary constructor (C# 12)
public class DataProcessor(string name = "Default") : BaseProcessor(name)
{
    // Constants
    private const int MaxItems = 100;

    // Static readonly
    private static readonly string DefaultName = "UAST-Grep";

    // Properties
    public string Name => _name;
    public int Count { get; private set; } = 0;

    // Field with target-typed new
    private readonly Dictionary<string, object> _cache = new();

    public override List<object> Process(List<object> items)
    {
        List<object> results = [];  // Collection expression

        // For loop
        for (int i = 0; i < items.Count; i++)
        {
            results.Add(Transform(items[i]));
        }

        // Foreach loop
        foreach (var item in items)
        {
            _cache[item?.ToString() ?? "null"] = item!;
        }

        // While loop
        int counter = 0;
        while (counter < 10)
        {
            counter++;
        }

        // Do-while loop
        do
        {
            counter++;
        } while (counter < 20);

        Count = results.Count;
        return results;
    }

    private object Transform(object item)
    {
        // Switch expression with pattern matching
        return item switch
        {
            int[] arr => arr.Select(x => x * 2).ToArray(),
            string s => s.ToUpper(),
            int n => n * 2,
            double d => d * 2.0,
            null => throw new ArgumentNullException(nameof(item)),
            _ => item
        };
    }

    public async Task RiskyOperationAsync()
    {
        // Try-catch-finally with specific exceptions
        try
        {
            await using var stream = new FileStream("test.txt", FileMode.Open);
            using var reader = new StreamReader(stream);
            string content = await reader.ReadToEndAsync();
            Log($"Read {content.Length} characters");
        }
        catch (FileNotFoundException ex)
        {
            Log($"File not found: {ex.Message}");
        }
        catch (IOException ex) when (ex.HResult == -2147024864)
        {
            Log($"File in use: {ex.Message}");
        }
        catch (Exception ex)
        {
            Log($"Error: {ex.Message}");
            throw;
        }
        finally
        {
            Log("Operation complete");
        }
    }

    // Method with nullable reference types
    public string? GetCachedValue(string key)
    {
        return _cache.TryGetValue(key, out var value) ? value?.ToString() : null;
    }
}

// Generic class with constraints
public class Pair<T, U> where T : notnull where U : notnull
{
    public required T First { get; init; }
    public required U Second { get; init; }

    public Pair<U, T> Swap() => new() { First = Second, Second = First };
}

// Extension methods
public static class ProcessorExtensions
{
    public static void LogMany(this IProcessor processor, params string[] messages)
    {
        foreach (var message in messages)
        {
            processor.Log(message);
        }
    }
}

// Generic method with multiple type parameters
public static class Utilities
{
    public static List<TResult> Map<TSource, TResult>(
        IEnumerable<TSource> items,
        Func<TSource, TResult> selector)
    {
        return items.Select(selector).ToList();
    }

    // Tuple return
    public static (int Sum, int Product) Calculate(int a, int b)
    {
        return (a + b, a * b);
    }

    // Ref returns
    public static ref int FindMax(int[] numbers)
    {
        int maxIndex = 0;
        for (int i = 1; i < numbers.Length; i++)
        {
            if (numbers[i] > numbers[maxIndex])
                maxIndex = i;
        }
        return ref numbers[maxIndex];
    }

    // Local function
    public static int SumWithValidation(int[] numbers)
    {
        ValidateInput();
        return numbers.Sum();

        void ValidateInput()
        {
            if (numbers == null || numbers.Length == 0)
                throw new ArgumentException("Numbers cannot be null or empty");
        }
    }
}

// Interpolated string
public class Program
{
    public static async Task Main(string[] args)
    {
        var processor = new DataProcessor("Main");

        List<object> data = [1, 2, 3, "hello", new[] { 4, 5 }];
        var result = processor.Process(data);

        processor.Log($"Processing complete: {string.Join(", ", result)}");

        // Pattern matching in if
        if (result is [var first, .., var last])
        {
            Console.WriteLine($"First: {first}, Last: {last}");
        }

        // Person record
        var person = new Person("Alice", 30, "alice@example.com");
        Console.WriteLine($"{person.Name} is adult: {person.IsAdult}");

        // Tuple destructuring
        var (sum, product) = Utilities.Calculate(5, 3);
        Console.WriteLine($"Sum: {sum}, Product: {product}");

        // LINQ query
        var evenNumbers = from n in Enumerable.Range(1, 10)
                          where n % 2 == 0
                          select n * 2;

        // LINQ method syntax
        var doubled = Enumerable.Range(1, 5)
            .Where(n => n > 2)
            .Select(n => n * 2)
            .ToList();

        await processor.RiskyOperationAsync();
    }
}
