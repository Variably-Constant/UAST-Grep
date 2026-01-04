/**
 * Scala Test File for UAST-Grep
 * Tests: functions, classes, traits, variables, control flow, error handling
 */

// Single line comment
package com.example.test

import scala.util.{Try, Success, Failure}
import scala.concurrent.{Future, ExecutionContext}
import scala.io.Source
import java.io.{File, IOException}

// Object for constants (companion-like)
object Constants {
  val MaxItems: Int = 100
  val DefaultName: String = "UAST-Grep"
}

// Type alias
type ItemList = List[Any]

// Trait (interface)
trait Processor {
  def process(items: ItemList): ItemList
  def log(message: String): Unit
}

// Trait with implementation
trait Loggable {
  def name: String

  def log(message: String): Unit = {
    println(s"[$name] $message")
  }
}

// Sealed trait for ADT
sealed trait Status
case object Ok extends Status
case object NotFound extends Status
case class Error(message: String) extends Status

// Case class (immutable data)
case class Person(
  name: String,
  age: Int,
  email: Option[String] = None
) {
  def isAdult: Boolean = age >= 18
}

// Enum (Scala 3)
enum Color(val rgb: Int):
  case Red extends Color(0xFF0000)
  case Green extends Color(0x00FF00)
  case Blue extends Color(0x0000FF)

// Abstract class
abstract class BaseProcessor(val name: String) extends Processor with Loggable {
  protected var count: Int = 0

  def getCount: Int = count
}

// Concrete class
class DataProcessor(override val name: String = "Default") extends BaseProcessor(name) {
  private val cache = scala.collection.mutable.Map[String, Any]()

  override def process(items: ItemList): ItemList = {
    var results = List.empty[Any]

    // For loop with index
    for (i <- items.indices) {
      results = results :+ transform(items(i))
    }

    // For-each style
    for (item <- items) {
      cache(item.toString) = item
    }

    // While loop
    var counter = 0
    while (counter < 10) {
      counter += 1
    }

    // Do-while loop
    do {
      counter += 1
    } while (counter < 20)

    // For comprehension
    val doubled = for {
      item <- items
      if item.isInstanceOf[Int]
    } yield item.asInstanceOf[Int] * 2

    count = results.length
    results
  }

  private def transform(item: Any): Any = item match {
    // Pattern matching
    case list: List[Int @unchecked] => list.map(_ * 2)
    case s: String => s.toUpperCase
    case n: Int => n * 2
    case d: Double => d * 2.0
    case null => null
    case other => other
  }

  def riskyOperation(): Unit = {
    // Try-catch-finally
    try {
      val source = Source.fromFile("test.txt")
      val content = try source.mkString finally source.close()
      log(s"Read ${content.length} characters")
    } catch {
      case e: IOException =>
        log(s"IO error: ${e.getMessage}")
      case e: Exception =>
        log(s"Error: ${e.getMessage}")
        throw e
    } finally {
      log("Operation complete")
    }
  }

  // Using Try monad
  def safeRead(path: String): Try[String] = Try {
    val source = Source.fromFile(path)
    try source.mkString finally source.close()
  }
}

// Companion object
object DataProcessor {
  def apply(name: String): DataProcessor = new DataProcessor(name)

  def default: DataProcessor = new DataProcessor()
}

// Implicit class (extension method)
implicit class StringOps(val s: String) extends AnyVal {
  def toTitleCase: String = s.split(" ").map(_.capitalize).mkString(" ")
  def wordCount: Int = s.split("\\s+").length
}

// Generic class with variance
class Pair[+A, +B](val first: A, val second: B) {
  def swap: Pair[B, A] = new Pair(second, first)

  override def toString: String = s"($first, $second)"
}

// Generic function with type bounds
def processCollection[T <: AnyRef](items: List[T]): List[T] = items

// Higher-order function
def mapItems[T, R](items: List[T])(f: T => R): List[R] = items.map(f)

// Curried function
def add(x: Int)(y: Int): Int = x + y

// Function with default parameters
def calculateSum(a: Int, b: Int = 0): Int = a + b

// Lambda expressions
val multiply: (Int, Int) => Int = (x, y) => x * y
val double: Int => Int = _ * 2

// Partial function
val divide: PartialFunction[Int, Int] = {
  case n if n != 0 => 100 / n
}

// Pattern matching function
def getStatusMessage(code: Int): String = code match {
  case 200 => "OK"
  case 404 => "Not Found"
  case 500 => "Server Error"
  case n if n >= 400 && n < 500 => "Client Error"
  case _ => "Unknown"
}

// Option handling
def processOptional(value: Option[String]): String = {
  // Pattern matching
  value match {
    case Some(v) => v.toUpperCase
    case None => "default"
  }
}

// For comprehension with Option
def combineOptions(a: Option[Int], b: Option[Int]): Option[Int] = {
  for {
    x <- a
    y <- b
  } yield x + y
}

// String interpolation
val name = "UAST-Grep"
val interpolated = s"Testing $name parser"
val complex = s"Length: ${name.length}"
val formatted = f"Pi is approximately ${math.Pi}%.2f"
val raw = raw"No escape: \n stays as backslash-n"

// Main object
object Main extends App {
  val processor = DataProcessor("Main")

  val data: ItemList = List(1, 2, 3, "hello", List(4, 5))
  val result = processor.process(data)

  processor.log(s"Processing complete: $result")

  // Collection operations
  val numbers = List(1, 2, 3, 4, 5)
  val doubled = numbers.map(_ * 2)
  val filtered = numbers.filter(_ > 2)
  val sum = numbers.reduce(_ + _)
  val folded = numbers.foldLeft(0)(_ + _)

  // Tuple
  val tuple = (1, "hello", 3.14)
  val (a, b, c) = tuple

  // Case class pattern matching
  val person = Person("Alice", 30, Some("alice@example.com"))
  person match {
    case Person(n, age, _) if age >= 18 => println(s"$n is an adult")
    case Person(n, _, _) => println(s"$n is a minor")
  }

  // Try/Success/Failure
  processor.safeRead("test.txt") match {
    case Success(content) => println(s"Content: $content")
    case Failure(e) => println(s"Error: ${e.getMessage}")
  }

  println(s"Sum: ${calculateSum(5, 3)}")
  println(s"Product: ${multiply(5, 3)}")
}
