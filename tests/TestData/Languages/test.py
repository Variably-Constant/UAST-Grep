"""
Python Test File for UAST-Grep
Tests: functions, classes, variables, control flow, error handling
"""

# Single line comment
from abc import ABC, abstractmethod
from typing import List, Dict, Any, Optional
from dataclasses import dataclass
import os

# Constants and variables
MAX_ITEMS: int = 100
global_counter: int = 0
name: str = "UAST-Grep"
interpolated: str = f"Testing {name} parser"

# Type alias
ItemList = List[Any]


# Abstract base class
class BaseProcessor(ABC):
    """Abstract base class for processors."""

    def __init__(self, name: str) -> None:
        self._name = name
        self._count = 0

    @property
    def name(self) -> str:
        return self._name

    @abstractmethod
    def process(self, items: ItemList) -> ItemList:
        """Process items - must be implemented by subclasses."""
        pass

    def log(self, message: str) -> None:
        print(f"[{self._name}] {message}")


# Dataclass for structured data
@dataclass
class Person:
    name: str
    age: int
    email: Optional[str] = None

    def is_adult(self) -> bool:
        return self.age >= 18


# Concrete class with full implementation
class DataProcessor(BaseProcessor):
    """Concrete processor implementation."""

    def __init__(self, name: str = "Default") -> None:
        super().__init__(name)
        self._cache: Dict[str, Any] = {}

    def process(self, items: ItemList) -> ItemList:
        results: List[Any] = []

        # For loop
        for i in range(len(items)):
            results.append(self._transform(items[i]))

        # For-each style loop
        for item in items:
            self._cache[str(item)] = item

        # While loop
        counter = 0
        while counter < 10:
            counter += 1

        # List comprehension
        doubled = [x * 2 for x in range(5) if x > 0]

        # Dict comprehension
        mapping = {k: v for k, v in enumerate(items)}

        return results

    def _transform(self, item: Any) -> Any:
        # If-elif-else control flow
        if isinstance(item, list):
            return [x * 2 for x in item]
        elif isinstance(item, str):
            return item.upper()
        elif isinstance(item, (int, float)):
            return item * 2
        else:
            return item

    def risky_operation(self) -> None:
        # Try-except-else-finally error handling
        try:
            with open("test.txt", "r") as file:
                content = file.read()
        except FileNotFoundError as e:
            self.log(f"File not found: {e}")
        except IOError as e:
            self.log(f"IO error: {e}")
        else:
            self.log("File read successfully")
        finally:
            self.log("Operation complete")

    def match_example(self, code: int) -> str:
        # Match statement (Python 3.10+)
        match code:
            case 200:
                return "OK"
            case 404:
                return "Not Found"
            case 500:
                return "Server Error"
            case _:
                return "Unknown"


# Standalone function with type hints
def calculate_sum(a: int, b: int) -> int:
    """Calculate sum of two numbers."""
    return a + b


# Lambda function
multiply = lambda x, y: x * y


# Generator function
def number_generator(n: int):
    """Generate numbers from 0 to n-1."""
    for i in range(n):
        yield i


# Async function
async def async_operation(data: str) -> str:
    """Async operation example."""
    return f"Processed: {data}"


# Decorator
def logged(func):
    """Logging decorator."""
    def wrapper(*args, **kwargs):
        print(f"Calling {func.__name__}")
        result = func(*args, **kwargs)
        print(f"Finished {func.__name__}")
        return result
    return wrapper


@logged
def decorated_function(x: int) -> int:
    return x * 2


# Context manager
class ResourceManager:
    def __enter__(self):
        print("Acquiring resource")
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        print("Releasing resource")
        return False


# Main execution
if __name__ == "__main__":
    processor = DataProcessor("Main")
    data = [1, 2, 3, "hello", [4, 5]]
    result = processor.process(data)
    processor.log(f"Processing complete: {result}")

    person = Person("Alice", 30, "alice@example.com")
    print(f"{person.name} is adult: {person.is_adult()}")
