/**
 * C++ Test File for UAST-Grep
 * Tests: functions, classes, templates, variables, control flow, error handling
 */

// Single line comment
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <memory>
#include <functional>
#include <optional>
#include <variant>
#include <fstream>
#include <stdexcept>
#include <algorithm>
#include <ranges>
#include <concepts>

// Namespace
namespace uast {

// Constants
constexpr int MAX_ITEMS = 100;
constexpr std::string_view DEFAULT_NAME = "UAST-Grep";

// Type alias
using ItemList = std::vector<std::variant<int, std::string, std::vector<int>>>;
using Callback = std::function<void(const std::string&)>;

// Enum class
enum class Status {
    Ok = 200,
    NotFound = 404,
    ServerError = 500
};

// Concept (C++20)
template<typename T>
concept Numeric = std::is_arithmetic_v<T>;

// Abstract class (interface)
class IProcessor {
public:
    virtual ~IProcessor() = default;
    virtual std::vector<int> process(const std::vector<int>& items) = 0;
    virtual void log(const std::string& message) const = 0;
};

// Struct with methods
struct Person {
    std::string name;
    int age;
    std::optional<std::string> email;

    [[nodiscard]] bool isAdult() const { return age >= 18; }

    // Comparison operator (C++20)
    auto operator<=>(const Person&) const = default;
};

// Base class
class BaseProcessor : public IProcessor {
protected:
    std::string name_;
    mutable int count_ = 0;

public:
    explicit BaseProcessor(std::string name) : name_(std::move(name)) {}
    virtual ~BaseProcessor() = default;

    void log(const std::string& message) const override {
        std::cout << "[" << name_ << "] " << message << "\n";
    }

    [[nodiscard]] const std::string& getName() const { return name_; }
    [[nodiscard]] int getCount() const { return count_; }
};

// Derived class with final
class DataProcessor final : public BaseProcessor {
private:
    std::map<std::string, int> cache_;

public:
    // Constructor with default argument
    explicit DataProcessor(const std::string& name = "Default")
        : BaseProcessor(name) {}

    // Move constructor
    DataProcessor(DataProcessor&&) noexcept = default;
    DataProcessor& operator=(DataProcessor&&) noexcept = default;

    // Copy disabled
    DataProcessor(const DataProcessor&) = delete;
    DataProcessor& operator=(const DataProcessor&) = delete;

    std::vector<int> process(const std::vector<int>& items) override {
        std::vector<int> results;
        results.reserve(items.size());

        // Range-based for loop
        for (const auto& item : items) {
            results.push_back(transform(item));
        }

        // Traditional for loop
        for (size_t i = 0; i < items.size(); ++i) {
            cache_[std::to_string(items[i])] = items[i];
        }

        // While loop
        int counter = 0;
        while (counter < 10) {
            ++counter;
        }

        // Do-while loop
        do {
            ++counter;
        } while (counter < 20);

        count_ = static_cast<int>(results.size());
        return results;
    }

    void riskyOperation() const {
        // Try-catch with multiple handlers
        try {
            std::ifstream file("test.txt");
            if (!file.is_open()) {
                throw std::runtime_error("Failed to open file");
            }

            std::string content((std::istreambuf_iterator<char>(file)),
                               std::istreambuf_iterator<char>());
            log("Read " + std::to_string(content.size()) + " characters");
        }
        catch (const std::runtime_error& e) {
            log(std::string("Runtime error: ") + e.what());
        }
        catch (const std::exception& e) {
            log(std::string("Error: ") + e.what());
            throw;
        }
        catch (...) {
            log("Unknown error occurred");
            throw;
        }
    }

private:
    [[nodiscard]] static int transform(int value) noexcept {
        return value * 2;
    }
};

// Template class
template<typename T, typename U>
class Pair {
private:
    T first_;
    U second_;

public:
    Pair(T first, U second) : first_(std::move(first)), second_(std::move(second)) {}

    [[nodiscard]] const T& first() const { return first_; }
    [[nodiscard]] const U& second() const { return second_; }

    [[nodiscard]] Pair<U, T> swap() const {
        return Pair<U, T>(second_, first_);
    }
};

// Template function with concept
template<Numeric T>
T add(T a, T b) {
    return a + b;
}

// Variadic template
template<typename... Args>
auto sumAll(Args... args) {
    return (args + ...);  // Fold expression
}

// Function with auto return type
auto multiply(int x, int y) -> int {
    return x * y;
}

// Lambda expressions
inline auto getMultiplier(int factor) {
    return [factor](int x) { return x * factor; };
}

// Constexpr function
constexpr int factorial(int n) {
    return n <= 1 ? 1 : n * factorial(n - 1);
}

// Switch with [[fallthrough]]
std::string_view getStatusMessage(Status status) {
    switch (status) {
        case Status::Ok:
            return "OK";
        case Status::NotFound:
            return "Not Found";
        case Status::ServerError:
            [[fallthrough]];
        default:
            return "Server Error";
    }
}

// Structured bindings
void structuredBindingsExample() {
    std::map<std::string, int> map{{"a", 1}, {"b", 2}};
    for (const auto& [key, value] : map) {
        std::cout << key << ": " << value << "\n";
    }

    auto [first, second] = std::make_pair(1, 2);
    std::cout << first << ", " << second << "\n";
}

// If with initializer (C++17)
void ifInitializerExample(const std::map<std::string, int>& map) {
    if (auto it = map.find("key"); it != map.end()) {
        std::cout << "Found: " << it->second << "\n";
    } else {
        std::cout << "Not found\n";
    }
}

// Ranges and views (C++20)
void rangesExample() {
    std::vector<int> numbers{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

    auto result = numbers
        | std::views::filter([](int n) { return n % 2 == 0; })
        | std::views::transform([](int n) { return n * 2; })
        | std::views::take(3);

    for (int n : result) {
        std::cout << n << " ";
    }
    std::cout << "\n";
}

} // namespace uast

// String literal
using namespace std::string_literals;
const auto name = "UAST-Grep"s;
const auto interpolated = "Testing "s + name + " parser"s;

// Main function
int main() {
    using namespace uast;

    auto processor = std::make_unique<DataProcessor>("Main");

    std::vector<int> data{1, 2, 3, 4, 5};
    auto result = processor->process(data);

    processor->log("Processing complete");

    // Print results using algorithm
    std::cout << "Results: ";
    std::ranges::for_each(result, [](int n) { std::cout << n << " "; });
    std::cout << "\n";

    // Template usage
    auto pair = Pair<std::string, int>("hello", 42);
    std::cout << "Pair: " << pair.first() << ", " << pair.second() << "\n";

    // Lambda usage
    auto doubler = getMultiplier(2);
    std::cout << "Doubled 5: " << doubler(5) << "\n";

    // Variadic template
    std::cout << "Sum: " << sumAll(1, 2, 3, 4, 5) << "\n";

    // Constexpr
    constexpr auto fact5 = factorial(5);
    std::cout << "5! = " << fact5 << "\n";

    // Person with optional
    Person person{"Alice", 30, "alice@example.com"};
    std::cout << person.name << " is adult: " << std::boolalpha << person.isAdult() << "\n";

    // Variant usage
    ItemList items;
    items.emplace_back(42);
    items.emplace_back("hello"s);
    items.emplace_back(std::vector<int>{1, 2, 3});

    for (const auto& item : items) {
        std::visit([](const auto& v) {
            using T = std::decay_t<decltype(v)>;
            if constexpr (std::is_same_v<T, int>) {
                std::cout << "Int: " << v << "\n";
            } else if constexpr (std::is_same_v<T, std::string>) {
                std::cout << "String: " << v << "\n";
            } else if constexpr (std::is_same_v<T, std::vector<int>>) {
                std::cout << "Vector with " << v.size() << " elements\n";
            }
        }, item);
    }

    return 0;
}
