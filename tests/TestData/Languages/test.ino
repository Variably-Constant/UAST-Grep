/**
 * Arduino Test File for UAST-Grep
 * Tests: setup/loop, pin control, serial, interrupts, classes
 */

// Include libraries
#include <Arduino.h>
#include <Wire.h>
#include <SPI.h>
#include <EEPROM.h>

// Constants
const int LED_PIN = 13;
const int BUTTON_PIN = 2;
const int ANALOG_PIN = A0;
const int PWM_PIN = 9;
const unsigned long DEBOUNCE_DELAY = 50;
const int MAX_ITEMS = 100;
const char* DEFAULT_NAME = "UAST-Grep";

// Macros
#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))
#define CLAMP(x, low, high) ((x) < (low) ? (low) : ((x) > (high) ? (high) : (x)))

// Enums
enum State {
  STATE_IDLE,
  STATE_RUNNING,
  STATE_PAUSED,
  STATE_ERROR
};

enum Command {
  CMD_START = 'S',
  CMD_STOP = 'X',
  CMD_PAUSE = 'P',
  CMD_RESUME = 'R',
  CMD_STATUS = '?'
};

// Struct
struct SensorData {
  int value;
  unsigned long timestamp;
  bool valid;
};

struct Config {
  int threshold;
  int interval;
  bool enabled;
};

// Class definition
class DataProcessor {
private:
  String name;
  int count;
  int* cache;
  int cacheSize;
  State state;

public:
  // Constructor
  DataProcessor(const String& n = DEFAULT_NAME) : name(n), count(0), cache(nullptr), cacheSize(0), state(STATE_IDLE) {
    // Initialize
  }

  // Destructor
  ~DataProcessor() {
    if (cache != nullptr) {
      delete[] cache;
    }
  }

  // Initialize with cache size
  bool init(int size) {
    if (size <= 0 || size > MAX_ITEMS) {
      return false;
    }

    cache = new int[size];
    if (cache == nullptr) {
      return false;
    }

    cacheSize = size;
    memset(cache, 0, size * sizeof(int));
    state = STATE_RUNNING;
    return true;
  }

  // Process data
  int process(int* items, int length) {
    if (state != STATE_RUNNING || items == nullptr) {
      return -1;
    }

    int sum = 0;

    // For loop
    for (int i = 0; i < length; i++) {
      int transformed = transform(items[i]);
      sum += transformed;

      // Cache if space available
      if (count < cacheSize) {
        cache[count++] = transformed;
      }
    }

    return sum;
  }

  // Transform function
  int transform(int value) {
    // Switch statement
    switch (value % 4) {
      case 0:
        return value * 2;
      case 1:
        return value + 10;
      case 2:
        return value - 5;
      default:
        return value;
    }
  }

  // Log message
  void log(const String& message) {
    Serial.print("[");
    Serial.print(name);
    Serial.print("] ");
    Serial.println(message);
  }

  // Getters
  String getName() const { return name; }
  int getCount() const { return count; }
  State getState() const { return state; }

  // Setters
  void setState(State s) { state = s; }
};

// Global variables
DataProcessor processor;
volatile bool buttonPressed = false;
volatile unsigned long lastInterruptTime = 0;
SensorData sensorBuffer[10];
int bufferIndex = 0;
Config config = {512, 1000, true};
State currentState = STATE_IDLE;

// Interrupt service routine
void buttonISR() {
  unsigned long currentTime = millis();

  // Debounce
  if (currentTime - lastInterruptTime > DEBOUNCE_DELAY) {
    buttonPressed = true;
    lastInterruptTime = currentTime;
  }
}

// Timer interrupt (for boards that support it)
// ISR(TIMER1_COMPA_vect) {
//   // Timer interrupt code
// }

// Setup function
void setup() {
  // Initialize serial
  Serial.begin(115200);
  while (!Serial) {
    ; // Wait for serial port
  }

  Serial.println(F("UAST-Grep Arduino Test Starting..."));

  // Pin modes
  pinMode(LED_PIN, OUTPUT);
  pinMode(BUTTON_PIN, INPUT_PULLUP);
  pinMode(PWM_PIN, OUTPUT);

  // Attach interrupt
  attachInterrupt(digitalPinToInterrupt(BUTTON_PIN), buttonISR, FALLING);

  // Initialize I2C
  Wire.begin();

  // Initialize SPI
  SPI.begin();

  // Initialize processor
  if (!processor.init(50)) {
    Serial.println(F("Failed to initialize processor"));
    while (1) {
      digitalWrite(LED_PIN, HIGH);
      delay(100);
      digitalWrite(LED_PIN, LOW);
      delay(100);
    }
  }

  processor.log(F("Initialized successfully"));

  // Read config from EEPROM
  EEPROM.get(0, config);

  // Validate config
  if (config.threshold < 0 || config.threshold > 1023) {
    config.threshold = 512;
    config.interval = 1000;
    config.enabled = true;
    EEPROM.put(0, config);
  }

  currentState = STATE_RUNNING;
}

// Main loop
void loop() {
  static unsigned long lastSampleTime = 0;
  static unsigned long lastBlinkTime = 0;
  static bool ledState = false;

  unsigned long currentTime = millis();

  // Handle button press
  if (buttonPressed) {
    buttonPressed = false;
    handleButton();
  }

  // Handle serial commands
  if (Serial.available() > 0) {
    handleSerial();
  }

  // State machine
  switch (currentState) {
    case STATE_IDLE:
      // Do nothing
      break;

    case STATE_RUNNING:
      // Sample sensor at interval
      if (currentTime - lastSampleTime >= (unsigned long)config.interval) {
        lastSampleTime = currentTime;
        sampleSensor();
      }

      // Blink LED
      if (currentTime - lastBlinkTime >= 500) {
        lastBlinkTime = currentTime;
        ledState = !ledState;
        digitalWrite(LED_PIN, ledState);
      }
      break;

    case STATE_PAUSED:
      // Turn off LED
      digitalWrite(LED_PIN, LOW);
      break;

    case STATE_ERROR:
      // Fast blink
      if (currentTime - lastBlinkTime >= 100) {
        lastBlinkTime = currentTime;
        ledState = !ledState;
        digitalWrite(LED_PIN, ledState);
      }
      break;
  }
}

// Sample sensor
void sampleSensor() {
  int rawValue = analogRead(ANALOG_PIN);

  // Store in buffer
  sensorBuffer[bufferIndex].value = rawValue;
  sensorBuffer[bufferIndex].timestamp = millis();
  sensorBuffer[bufferIndex].valid = true;

  bufferIndex = (bufferIndex + 1) % ARRAY_SIZE(sensorBuffer);

  // Process if above threshold
  if (rawValue > config.threshold) {
    int values[] = {rawValue, rawValue / 2, rawValue / 4};
    int result = processor.process(values, 3);

    // PWM output based on result
    int pwmValue = CLAMP(map(result, 0, 1000, 0, 255), 0, 255);
    analogWrite(PWM_PIN, pwmValue);

    // Log
    char buffer[64];
    snprintf(buffer, sizeof(buffer), "Sensor: %d, Result: %d, PWM: %d", rawValue, result, pwmValue);
    processor.log(buffer);
  }
}

// Handle button press
void handleButton() {
  // Toggle pause/resume
  if (currentState == STATE_RUNNING) {
    currentState = STATE_PAUSED;
    processor.setState(STATE_PAUSED);
    processor.log(F("Paused"));
  } else if (currentState == STATE_PAUSED) {
    currentState = STATE_RUNNING;
    processor.setState(STATE_RUNNING);
    processor.log(F("Resumed"));
  }
}

// Handle serial commands
void handleSerial() {
  char cmd = Serial.read();

  switch (cmd) {
    case CMD_START:
      currentState = STATE_RUNNING;
      processor.log(F("Started"));
      break;

    case CMD_STOP:
      currentState = STATE_IDLE;
      processor.log(F("Stopped"));
      break;

    case CMD_PAUSE:
      currentState = STATE_PAUSED;
      processor.log(F("Paused"));
      break;

    case CMD_RESUME:
      currentState = STATE_RUNNING;
      processor.log(F("Resumed"));
      break;

    case CMD_STATUS:
      printStatus();
      break;

    default:
      Serial.print(F("Unknown command: "));
      Serial.println(cmd);
      break;
  }
}

// Print status
void printStatus() {
  Serial.println(F("=== Status ==="));
  Serial.print(F("Name: "));
  Serial.println(processor.getName());
  Serial.print(F("State: "));
  Serial.println(currentState);
  Serial.print(F("Count: "));
  Serial.println(processor.getCount());
  Serial.print(F("Threshold: "));
  Serial.println(config.threshold);
  Serial.print(F("Interval: "));
  Serial.println(config.interval);
  Serial.print(F("Free RAM: "));
  Serial.println(freeRam());
  Serial.println(F("=============="));
}

// Get free RAM
int freeRam() {
  extern int __heap_start, *__brkval;
  int v;
  return (int)&v - (__brkval == 0 ? (int)&__heap_start : (int)__brkval);
}

// I2C scan helper
void i2cScan() {
  Serial.println(F("Scanning I2C..."));
  for (byte address = 1; address < 127; address++) {
    Wire.beginTransmission(address);
    if (Wire.endTransmission() == 0) {
      Serial.print(F("Found device at 0x"));
      Serial.println(address, HEX);
    }
  }
  Serial.println(F("Scan complete"));
}
