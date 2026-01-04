/// Dart Test File for UAST-Grep
/// Tests: functions, classes, variables, control flow, error handling

// Single line comment
import 'dart:async';
import 'dart:io';
import 'dart:collection';

// Constants
const int maxItems = 100;
const String defaultName = 'UAST-Grep';
final DateTime startTime = DateTime.now();

// Type alias
typedef ItemList = List<dynamic>;
typedef Callback<T> = void Function(T item);

// Enum
enum Status {
  ok(200, 'OK'),
  notFound(404, 'Not Found'),
  serverError(500, 'Server Error');

  final int code;
  final String message;

  const Status(this.code, this.message);
}

// Mixin
mixin Loggable {
  String get logPrefix;

  void log(String message) {
    print('[$logPrefix] $message');
  }
}

// Abstract class
abstract class Processor {
  List<dynamic> process(ItemList items);
  void log(String message);
}

// Interface (implicit)
abstract interface class Cacheable {
  void cache(String key, dynamic value);
  dynamic? getCached(String key);
}

// Base class
class BaseProcessor with Loggable implements Processor {
  final String name;
  int _count = 0;

  BaseProcessor(this.name);

  @override
  String get logPrefix => name;

  int get count => _count;

  @override
  List<dynamic> process(ItemList items) {
    throw UnimplementedError('Subclass must implement process');
  }
}

// Concrete class
class DataProcessor extends BaseProcessor implements Cacheable {
  final Map<String, dynamic> _cache = {};

  DataProcessor([String name = 'Default']) : super(name);

  // Named constructor
  DataProcessor.withPrefix(String prefix) : super('$prefix-Processor');

  @override
  List<dynamic> process(ItemList items) {
    final results = <dynamic>[];

    // For loop
    for (var i = 0; i < items.length; i++) {
      results.add(_transform(items[i]));
    }

    // For-in loop
    for (final item in items) {
      cache(item.toString(), item);
    }

    // While loop
    var counter = 0;
    while (counter < 10) {
      counter++;
    }

    // Do-while loop
    do {
      counter++;
    } while (counter < 20);

    _count = results.length;
    return results;
  }

  dynamic _transform(dynamic item) {
    // If-else control flow
    if (item is List<int>) {
      return item.map((x) => x * 2).toList();
    } else if (item is String) {
      return item.toUpperCase();
    } else if (item is int) {
      return item * 2;
    } else if (item is double) {
      return item * 2.0;
    } else {
      return item;
    }
  }

  // Switch expression (Dart 3.0+)
  String getStatusMessage(int code) => switch (code) {
    200 => 'OK',
    404 => 'Not Found',
    500 => 'Server Error',
    _ => 'Unknown',
  };

  @override
  void cache(String key, dynamic value) {
    _cache[key] = value;
  }

  @override
  dynamic? getCached(String key) => _cache[key];

  // Async method with error handling
  Future<void> riskyOperation() async {
    try {
      final file = File('test.txt');
      final content = await file.readAsString();
      log('Read ${content.length} characters');
    } on FileSystemException catch (e) {
      log('File error: ${e.message}');
    } on Exception catch (e) {
      log('Error: $e');
      rethrow;
    } finally {
      log('Operation complete');
    }
  }

  // Stream example
  Stream<int> numberStream(int count) async* {
    for (var i = 0; i < count; i++) {
      await Future.delayed(Duration(milliseconds: 100));
      yield i;
    }
  }
}

// Record type (Dart 3.0+)
typedef PersonRecord = ({String name, int age, String? email});

// Pattern matching (Dart 3.0+)
String describeValue(Object value) {
  return switch (value) {
    int n when n < 0 => 'Negative integer',
    int n => 'Positive integer: $n',
    String s => 'String of length ${s.length}',
    List<int> list => 'Int list with ${list.length} items',
    {'name': String name, 'age': int age} => 'Person: $name, $age',
    _ => 'Unknown type',
  };
}

// Extension methods
extension StringExtension on String {
  String toTitleCase() {
    return split(' ').map((word) {
      if (word.isEmpty) return word;
      return word[0].toUpperCase() + word.substring(1).toLowerCase();
    }).join(' ');
  }

  int get wordCount => split(RegExp(r'\s+')).length;
}

// Generic class
class Pair<T, U> {
  final T first;
  final U second;

  const Pair(this.first, this.second);

  Pair<U, T> swap() => Pair(second, first);

  @override
  String toString() => '($first, $second)';
}

// Generic function
List<R> mapItems<T, R>(List<T> items, R Function(T) transform) {
  return items.map(transform).toList();
}

// Function with optional parameters
int calculateSum(int a, [int b = 0]) => a + b;

// Function with named parameters
String greet({required String name, String greeting = 'Hello'}) {
  return '$greeting, $name!';
}

// Arrow function
final multiply = (int x, int y) => x * y;

// Null safety
String processNullable(String? value) {
  // Null-aware operators
  final length = value?.length ?? 0;
  final nonNull = value ?? 'default';

  // Null assertion
  // final forced = value!;

  return nonNull;
}

// Late initialization
class Config {
  late final String name;
  late int _value;

  void initialize(String name, int value) {
    this.name = name;
    _value = value;
  }
}

// String interpolation
final name = 'UAST-Grep';
final interpolated = 'Testing $name parser';
final complex = 'Length: ${name.length}';

// Multiline string
final multiline = '''
This is a multiline string
with multiple lines
and $name interpolation
''';

// Main function
void main() async {
  final processor = DataProcessor('Main');

  final ItemList data = [1, 2, 3, 'hello', [4, 5]];
  final result = processor.process(data);

  processor.log('Processing complete: $result');

  // Collection literals
  final list = [1, 2, 3];
  final set = {1, 2, 3};
  final map = {'a': 1, 'b': 2};

  // Collection operators
  final doubled = list.map((n) => n * 2).toList();
  final filtered = list.where((n) => n > 1).toList();
  final sum = list.fold(0, (acc, n) => acc + n);

  // Spread operator
  final merged = [...list, 4, 5];
  final mapMerged = {...map, 'c': 3};

  // Collection if/for
  final conditional = [
    1,
    2,
    if (true) 3,
    for (var i in [4, 5]) i,
  ];

  // Cascade notation
  final items = <int>[]
    ..add(1)
    ..add(2)
    ..add(3);

  // Await stream
  await for (final number in processor.numberStream(3)) {
    print('Received: $number');
  }

  print('Sum: ${calculateSum(5, 3)}');
  print(greet(name: 'World'));
}
