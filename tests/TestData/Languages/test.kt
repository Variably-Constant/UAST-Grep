/**
 * Kotlin Test File for UAST-Grep
 * Tests: functions, classes, interfaces, variables, control flow, error handling
 */

// Single line comment
package com.example.test

import java.io.File
import java.io.IOException
import kotlinx.coroutines.*

// Constants
const val MAX_ITEMS = 100
val DEFAULT_NAME = "UAST-Grep"

// Type alias
typealias ItemList = List<Any?>

// Interface definition
interface Processor {
    fun process(items: ItemList): ItemList
    fun log(message: String)
}

// Sealed class for state
sealed class Status {
    object Ok : Status()
    object NotFound : Status()
    data class Error(val message: String) : Status()
}

// Data class
data class Person(
    val name: String,
    val age: Int,
    val email: String? = null
) {
    fun isAdult(): Boolean = age >= 18
}

// Abstract class
abstract class BaseProcessor(protected val name: String) : Processor {
    protected var count: Int = 0

    override fun log(message: String) {
        println("[$name] $message")
    }
}

// Concrete class with primary constructor
class DataProcessor(
    name: String = "Default"
) : BaseProcessor(name) {

    private val cache = mutableMapOf<String, Any?>()

    override fun process(items: ItemList): ItemList {
        val results = mutableListOf<Any?>()

        // For loop with index
        for (i in items.indices) {
            results.add(transform(items[i]))
        }

        // For-each loop
        for (item in items) {
            cache[item.toString()] = item
        }

        // While loop
        var counter = 0
        while (counter < 10) {
            counter++
        }

        // Do-while loop
        do {
            counter++
        } while (counter < 20)

        // Range loop
        for (i in 1..5) {
            log("Iteration $i")
        }

        // Step and downTo
        for (i in 10 downTo 0 step 2) {
            log("Countdown: $i")
        }

        count = results.size
        return results
    }

    private fun transform(item: Any?): Any? {
        // When expression (pattern matching)
        return when (item) {
            is List<*> -> item.map { (it as? Int)?.times(2) }
            is String -> item.uppercase()
            is Int -> item * 2
            is Double -> item * 2.0
            null -> null
            else -> item
        }
    }

    fun riskyOperation() {
        // Try-catch-finally
        try {
            val content = File("test.txt").readText()
            log("Read ${content.length} characters")
        } catch (e: IOException) {
            log("IO error: ${e.message}")
        } catch (e: Exception) {
            log("Error: ${e.message}")
            throw e
        } finally {
            log("Operation complete")
        }
    }

    // Suspend function (coroutines)
    suspend fun asyncOperation(data: String): String {
        delay(100)
        return "Processed: $data"
    }
}

// Object declaration (singleton)
object Logger {
    fun log(message: String) {
        println("[Logger] $message")
    }
}

// Companion object
class Factory {
    companion object {
        fun create(name: String): DataProcessor = DataProcessor(name)
    }
}

// Extension function
fun String.toTitleCase(): String {
    return split(" ").joinToString(" ") { word ->
        word.lowercase().replaceFirstChar { it.uppercase() }
    }
}

// Extension property
val String.wordCount: Int
    get() = split(Regex("\\s+")).size

// Inline function with reified type
inline fun <reified T> List<*>.filterByType(): List<T> {
    return filterIsInstance<T>()
}

// Higher-order function
fun <T, R> ItemList.mapItems(transform: (Any?) -> R): List<R> {
    return map { transform(it) }
}

// Function with default parameters
fun calculateSum(a: Int, b: Int = 0): Int = a + b

// Lambda expression
val multiply: (Int, Int) -> Int = { x, y -> x * y }

// Function returning function
fun adder(x: Int): (Int) -> Int = { y -> x + y }

// Null safety examples
fun processNullable(value: String?): String {
    // Safe call
    val length = value?.length ?: 0

    // Elvis operator
    val nonNull = value ?: "default"

    // Not-null assertion (use with caution)
    // val forced = value!!

    return nonNull
}

// When with conditions
fun getStatusMessage(code: Int): String = when {
    code == 200 -> "OK"
    code in 400..499 -> "Client Error"
    code in 500..599 -> "Server Error"
    else -> "Unknown"
}

// Destructuring declarations
fun destructuringExample() {
    val (name, age) = Person("Alice", 30)
    println("$name is $age years old")

    val map = mapOf("a" to 1, "b" to 2)
    for ((key, value) in map) {
        println("$key = $value")
    }
}

// String interpolation
val name = "UAST-Grep"
val interpolated = "Testing $name parser"
val complex = "Length: ${name.length}"

// Main function
fun main() {
    val processor = DataProcessor("Main")

    val data: ItemList = listOf(1, 2, 3, "hello", listOf(4, 5))
    val result = processor.process(data)

    processor.log("Processing complete: $result")

    // Scope functions
    val person = Person("Alice", 30).also {
        println("Created person: ${it.name}")
    }

    val info = person.let { "${it.name} is ${it.age}" }

    with(processor) {
        log("Using with scope function")
    }

    // Collection operations
    val numbers = listOf(1, 2, 3, 4, 5)
    val doubled = numbers.map { it * 2 }
    val filtered = numbers.filter { it > 2 }
    val sum = numbers.reduce { acc, n -> acc + n }

    // Coroutine example
    runBlocking {
        val result = processor.asyncOperation("test")
        println(result)
    }
}
