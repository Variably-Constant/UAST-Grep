# Makefile Test File for UAST-Grep
# Tests: variables, rules, patterns, functions, conditionals

# =============================================================================
# Variables
# =============================================================================

# Simple variables (recursively expanded)
CC = gcc
CXX = g++
LD = $(CC)

# Immediately expanded variables
PROJECT := UAST-Grep
VERSION := 1.0.0
BUILD_DIR := build
SRC_DIR := src
OBJ_DIR := $(BUILD_DIR)/obj
BIN_DIR := $(BUILD_DIR)/bin

# Append to variable
CFLAGS := -Wall -Wextra -Werror
CFLAGS += -std=c11
CFLAGS += -O2

CXXFLAGS := -Wall -Wextra -Werror
CXXFLAGS += -std=c++17

LDFLAGS := -L/usr/local/lib
LDLIBS := -lm -lpthread

# Conditional assignment (set if not already set)
PREFIX ?= /usr/local
DESTDIR ?=

# Multi-line variable
define HELP_TEXT
UAST-Grep Build System
======================
Available targets:
  all      - Build everything
  clean    - Remove build artifacts
  test     - Run tests
  install  - Install to $(PREFIX)
endef

# Shell command in variable
GIT_HASH := $(shell git rev-parse --short HEAD 2>/dev/null || echo "unknown")
DATE := $(shell date +%Y-%m-%d)

# Source and object files
SOURCES := $(wildcard $(SRC_DIR)/*.c)
OBJECTS := $(SOURCES:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)
HEADERS := $(wildcard $(SRC_DIR)/*.h)

# Automatic variables examples in comments
# $@ = target name
# $< = first prerequisite
# $^ = all prerequisites
# $* = stem (pattern match)
# $(@D) = directory of target
# $(@F) = file part of target

# =============================================================================
# Conditionals
# =============================================================================

# Operating system detection
UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Linux)
    PLATFORM := linux
    CFLAGS += -D_GNU_SOURCE
endif
ifeq ($(UNAME_S),Darwin)
    PLATFORM := macos
    CFLAGS += -D_DARWIN_C_SOURCE
endif

# Debug/Release mode
ifdef DEBUG
    CFLAGS += -g -DDEBUG
    CXXFLAGS += -g -DDEBUG
else
    CFLAGS += -DNDEBUG
    CXXFLAGS += -DNDEBUG
endif

# Optional feature
ifneq ($(FEATURE_X),)
    CFLAGS += -DENABLE_FEATURE_X
    LDLIBS += -lfeaturex
endif

# Nested conditional
ifeq ($(CC),clang)
    CFLAGS += -Weverything
else ifeq ($(CC),gcc)
    CFLAGS += -Wpedantic
endif

# =============================================================================
# Default target
# =============================================================================

.DEFAULT_GOAL := all

.PHONY: all clean test install uninstall help
.PHONY: debug release check lint format

# =============================================================================
# Build rules
# =============================================================================

all: $(BIN_DIR)/$(PROJECT)

# Create directories
$(BUILD_DIR) $(OBJ_DIR) $(BIN_DIR):
	@mkdir -p $@

# Compile C source files
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c $(HEADERS) | $(OBJ_DIR)
	@echo "CC  $<"
	@$(CC) $(CFLAGS) -c $< -o $@

# Compile C++ source files
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp $(HEADERS) | $(OBJ_DIR)
	@echo "CXX $<"
	@$(CXX) $(CXXFLAGS) -c $< -o $@

# Link executable
$(BIN_DIR)/$(PROJECT): $(OBJECTS) | $(BIN_DIR)
	@echo "LD  $@"
	@$(LD) $(LDFLAGS) $^ $(LDLIBS) -o $@

# =============================================================================
# Pattern rules
# =============================================================================

# Static library
lib%.a:
	$(AR) rcs $@ $^

# Shared library
lib%.so: CFLAGS += -fPIC
lib%.so:
	$(CC) -shared $(LDFLAGS) $^ -o $@

# Assembly from C
%.s: %.c
	$(CC) $(CFLAGS) -S $< -o $@

# Preprocessed output
%.i: %.c
	$(CC) $(CFLAGS) -E $< -o $@

# =============================================================================
# Special targets
# =============================================================================

# Clean build artifacts
clean:
	@echo "Cleaning..."
	@rm -rf $(BUILD_DIR)
	@rm -f *.o *.a *.so

# Deep clean
distclean: clean
	@rm -f config.h config.mk
	@rm -rf deps/

# =============================================================================
# Test targets
# =============================================================================

test: $(BIN_DIR)/$(PROJECT)
	@echo "Running tests..."
	@./scripts/run-tests.sh

check: test

# =============================================================================
# Installation
# =============================================================================

install: $(BIN_DIR)/$(PROJECT)
	@echo "Installing to $(DESTDIR)$(PREFIX)..."
	@install -d $(DESTDIR)$(PREFIX)/bin
	@install -m 755 $(BIN_DIR)/$(PROJECT) $(DESTDIR)$(PREFIX)/bin/

uninstall:
	@echo "Uninstalling from $(DESTDIR)$(PREFIX)..."
	@rm -f $(DESTDIR)$(PREFIX)/bin/$(PROJECT)

# =============================================================================
# Development targets
# =============================================================================

debug: CFLAGS += -g -O0 -DDEBUG
debug: clean all

release: CFLAGS += -O3 -DNDEBUG
release: clean all

# Code formatting
format:
	@clang-format -i $(SRC_DIR)/*.c $(SRC_DIR)/*.h

# Static analysis
lint:
	@cppcheck --enable=all $(SRC_DIR)/

# =============================================================================
# Functions
# =============================================================================

# Define a function
define compile_template
$(OBJ_DIR)/$(1).o: $(SRC_DIR)/$(1).c
	$$(CC) $$(CFLAGS) -c $$< -o $$@
endef

# Call function for each source
$(foreach src,$(basename $(notdir $(SOURCES))),$(eval $(call compile_template,$(src))))

# File manipulation functions
UPPERCASE = $(shell echo $(1) | tr '[:lower:]' '[:upper:]')
LOWERCASE = $(shell echo $(1) | tr '[:upper:]' '[:lower:]')

# String functions
FILES := file1.c file2.c file3.c
OBJS := $(patsubst %.c,%.o,$(FILES))
BASES := $(basename $(FILES))
EXTS := $(suffix $(FILES))
DIRS := $(dir $(FILES))
NOTDIRS := $(notdir $(FILES))
JOINED := $(addprefix $(OBJ_DIR)/,$(OBJS))
FIRST := $(firstword $(FILES))
LAST := $(lastword $(FILES))
FILTERED := $(filter %.c,$(FILES))
FILTERED_OUT := $(filter-out %.h,$(FILES) header.h)
SORTED := $(sort z.c a.c m.c)
WORDS := $(words $(FILES))

# =============================================================================
# Help
# =============================================================================

help:
	$(info $(HELP_TEXT))
	@echo ""
	@echo "Variables:"
	@echo "  CC=$(CC)"
	@echo "  PREFIX=$(PREFIX)"
	@echo "  DEBUG=$(DEBUG)"

# =============================================================================
# Dependencies
# =============================================================================

# Include dependency files if they exist
-include $(OBJECTS:.o=.d)

# Generate dependency files
$(OBJ_DIR)/%.d: $(SRC_DIR)/%.c | $(OBJ_DIR)
	@$(CC) $(CFLAGS) -MM -MT '$(@:.d=.o)' $< -MF $@

# Include external makefile
-include config.mk

# =============================================================================
# Recursive make (for subdirectories)
# =============================================================================

SUBDIRS := lib tests

.PHONY: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

# Order-only prerequisites
libs: | $(OBJ_DIR)
	$(MAKE) -C lib

# =============================================================================
# Verbosity control
# =============================================================================

V ?= 0
ifeq ($(V),0)
    Q := @
    MAKEFLAGS += --no-print-directory
else
    Q :=
endif
