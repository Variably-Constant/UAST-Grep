# CMake Test File for UAST-Grep
# Tests: variables, functions, macros, conditions, targets

# =============================================================================
# Project Setup
# =============================================================================

cmake_minimum_required(VERSION 3.20)

# Project definition with version
project(UASTGrep
    VERSION 1.0.0
    DESCRIPTION "UAST-Grep test project for CMake parsing"
    HOMEPAGE_URL "https://github.com/example/UAST-Grep"
    LANGUAGES C CXX
)

# CMake policies
cmake_policy(SET CMP0074 NEW)  # Use <Package>_ROOT variables
cmake_policy(SET CMP0077 NEW)  # Option honors normal variables

# =============================================================================
# Options and Cache Variables
# =============================================================================

# Boolean option
option(UAST_BUILD_TESTS "Build test suite" ON)
option(UAST_BUILD_DOCS "Build documentation" OFF)
option(UAST_ENABLE_COVERAGE "Enable code coverage" OFF)
option(UAST_USE_SANITIZERS "Enable sanitizers" OFF)

# String option with choices
set(UAST_LOG_LEVEL "INFO" CACHE STRING "Logging level")
set_property(CACHE UAST_LOG_LEVEL PROPERTY STRINGS "DEBUG" "INFO" "WARN" "ERROR")

# Path option
set(UAST_INSTALL_PREFIX "/usr/local" CACHE PATH "Installation prefix")

# List variable
set(SUPPORTED_LANGUAGES
    "C"
    "CXX"
    "Fortran"
    CACHE STRING "Supported languages"
)

# Internal variable (not exposed in cache)
set(INTERNAL_VAR "internal" CACHE INTERNAL "Internal variable")

# =============================================================================
# Variables
# =============================================================================

# Simple variables
set(PROJECT_ROOT ${CMAKE_CURRENT_SOURCE_DIR})
set(SRC_DIR ${PROJECT_ROOT}/src)
set(INCLUDE_DIR ${PROJECT_ROOT}/include)
set(LIB_DIR ${PROJECT_ROOT}/lib)
set(TEST_DIR ${PROJECT_ROOT}/tests)
set(BUILD_DIR ${CMAKE_BINARY_DIR})

# List operations
set(SOURCE_FILES
    main.c
    parser.c
    lexer.c
    ast.c
)

list(APPEND SOURCE_FILES utils.c)
list(PREPEND SOURCE_FILES preamble.c)
list(REMOVE_ITEM SOURCE_FILES preamble.c)
list(LENGTH SOURCE_FILES NUM_SOURCES)
list(GET SOURCE_FILES 0 FIRST_SOURCE)
list(SORT SOURCE_FILES)
list(REVERSE SOURCE_FILES)
list(JOIN SOURCE_FILES ";" SOURCES_STRING)

# String operations
set(VERSION_STRING "1.0.0-beta")
string(REPLACE "-" "." VERSION_CLEAN ${VERSION_STRING})
string(TOUPPER ${VERSION_STRING} VERSION_UPPER)
string(TOLOWER ${VERSION_STRING} VERSION_LOWER)
string(LENGTH ${VERSION_STRING} VERSION_LENGTH)
string(SUBSTRING ${VERSION_STRING} 0 5 VERSION_PREFIX)
string(FIND ${VERSION_STRING} "beta" BETA_POS)
string(REGEX MATCH "[0-9]+\\.[0-9]+\\.[0-9]+" VERSION_NUMBERS ${VERSION_STRING})
string(REGEX REPLACE "([0-9]+)\\.([0-9]+)\\.([0-9]+)" "\\1" MAJOR_VERSION ${VERSION_NUMBERS})

# File operations
file(GLOB HEADER_FILES "${INCLUDE_DIR}/*.h")
file(GLOB_RECURSE ALL_SOURCES "${SRC_DIR}/*.c" "${SRC_DIR}/*.cpp")
file(MAKE_DIRECTORY ${BUILD_DIR}/generated)
file(STRINGS ${PROJECT_ROOT}/VERSION VERSION_CONTENT)
file(READ ${PROJECT_ROOT}/README.md README_CONTENT)
file(WRITE ${BUILD_DIR}/generated/version.txt "${PROJECT_VERSION}")
file(APPEND ${BUILD_DIR}/generated/version.txt "\n${GIT_HASH}")

# =============================================================================
# Conditionals
# =============================================================================

# If-elseif-else
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(DEBUG_MODE ON)
    add_compile_definitions(DEBUG=1)
elseif(CMAKE_BUILD_TYPE STREQUAL "Release")
    set(DEBUG_MODE OFF)
    add_compile_definitions(NDEBUG=1)
else()
    message(WARNING "Unknown build type: ${CMAKE_BUILD_TYPE}")
endif()

# Platform detection
if(WIN32)
    set(PLATFORM "Windows")
    add_compile_definitions(PLATFORM_WINDOWS)
elseif(APPLE)
    set(PLATFORM "macOS")
    add_compile_definitions(PLATFORM_MACOS)
elseif(UNIX)
    set(PLATFORM "Linux")
    add_compile_definitions(PLATFORM_LINUX)
endif()

# Compiler detection
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    set(COMPILER_FLAGS "-Wall -Wextra -Werror")
elseif(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    set(COMPILER_FLAGS "-Wall -Wextra -Werror -Weverything")
elseif(MSVC)
    set(COMPILER_FLAGS "/W4 /WX")
endif()

# Boolean conditions
if(UAST_BUILD_TESTS AND UAST_ENABLE_COVERAGE)
    message(STATUS "Building tests with coverage")
endif()

if(NOT DEFINED ENV{CI})
    message(STATUS "Not running in CI")
endif()

if(DEFINED CACHE{UAST_LOG_LEVEL})
    message(STATUS "Log level is cached")
endif()

# =============================================================================
# Functions and Macros
# =============================================================================

# Function definition
function(add_uast_library TARGET_NAME)
    # Parse arguments
    cmake_parse_arguments(
        LIB
        "STATIC;SHARED"
        "NAMESPACE"
        "SOURCES;DEPENDENCIES"
        ${ARGN}
    )

    # Create library
    if(LIB_SHARED)
        add_library(${TARGET_NAME} SHARED ${LIB_SOURCES})
    else()
        add_library(${TARGET_NAME} STATIC ${LIB_SOURCES})
    endif()

    # Set properties
    set_target_properties(${TARGET_NAME} PROPERTIES
        CXX_STANDARD 17
        CXX_STANDARD_REQUIRED ON
        POSITION_INDEPENDENT_CODE ON
    )

    # Add dependencies
    if(LIB_DEPENDENCIES)
        target_link_libraries(${TARGET_NAME} PRIVATE ${LIB_DEPENDENCIES})
    endif()

    # Export target
    if(LIB_NAMESPACE)
        add_library(${LIB_NAMESPACE}::${TARGET_NAME} ALIAS ${TARGET_NAME})
    endif()
endfunction()

# Macro definition (variables affect caller's scope)
macro(configure_sanitizers TARGET_NAME)
    if(UAST_USE_SANITIZERS)
        target_compile_options(${TARGET_NAME} PRIVATE
            -fsanitize=address
            -fsanitize=undefined
            -fno-omit-frame-pointer
        )
        target_link_options(${TARGET_NAME} PRIVATE
            -fsanitize=address
            -fsanitize=undefined
        )
    endif()
endmacro()

# =============================================================================
# Find Packages
# =============================================================================

# Required package
find_package(Threads REQUIRED)

# Optional package
find_package(GTest QUIET)
if(GTest_FOUND)
    message(STATUS "Found GTest: ${GTest_VERSION}")
endif()

# Config mode package
find_package(LLVM CONFIG)

# Module mode with components
find_package(Boost 1.70 COMPONENTS filesystem system OPTIONAL_COMPONENTS regex)

# PkgConfig
find_package(PkgConfig)
if(PKG_CONFIG_FOUND)
    pkg_check_modules(LIBFFI libffi)
endif()

# =============================================================================
# Targets
# =============================================================================

# Executable
add_executable(UAST-Grep
    ${SRC_DIR}/main.cpp
    ${SRC_DIR}/cli.cpp
)

# Library
add_library(uast-core STATIC
    ${SRC_DIR}/parser.cpp
    ${SRC_DIR}/lexer.cpp
    ${SRC_DIR}/ast.cpp
)

# Interface library (header-only)
add_library(uast-common INTERFACE)

# Object library
add_library(uast-utils OBJECT
    ${SRC_DIR}/utils.cpp
)

# Target properties
set_target_properties(UAST-Grep PROPERTIES
    OUTPUT_NAME "UAST-Grep"
    RUNTIME_OUTPUT_DIRECTORY ${BUILD_DIR}/bin
    DEBUG_POSTFIX "d"
)

set_target_properties(uast-core PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${BUILD_DIR}/lib
    LIBRARY_OUTPUT_DIRECTORY ${BUILD_DIR}/lib
    VERSION ${PROJECT_VERSION}
    SOVERSION ${PROJECT_VERSION_MAJOR}
)

# Include directories
target_include_directories(uast-core
    PUBLIC
        $<BUILD_INTERFACE:${INCLUDE_DIR}>
        $<INSTALL_INTERFACE:include>
    PRIVATE
        ${SRC_DIR}
)

# Compile definitions
target_compile_definitions(uast-core
    PUBLIC
        UAST_VERSION="${PROJECT_VERSION}"
    PRIVATE
        $<$<CONFIG:Debug>:DEBUG_BUILD>
)

# Compile options
target_compile_options(uast-core
    PRIVATE
        $<$<CXX_COMPILER_ID:GNU>:-Wall -Wextra>
        $<$<CXX_COMPILER_ID:MSVC>:/W4>
)

# Link libraries
target_link_libraries(UAST-Grep
    PRIVATE
        uast-core
        Threads::Threads
        $<$<BOOL:${Boost_FOUND}>:Boost::filesystem>
)

# =============================================================================
# Generator Expressions
# =============================================================================

# Build type expressions
target_compile_definitions(uast-core PRIVATE
    $<$<CONFIG:Debug>:DEBUG_MODE=1>
    $<$<CONFIG:Release>:RELEASE_MODE=1>
    $<$<CONFIG:RelWithDebInfo>:DEBUG_INFO=1>
)

# Platform expressions
target_compile_definitions(uast-core PRIVATE
    $<$<PLATFORM_ID:Windows>:WIN32_LEAN_AND_MEAN>
    $<$<PLATFORM_ID:Linux>:_GNU_SOURCE>
)

# Compiler expressions
target_compile_options(uast-core PRIVATE
    $<$<CXX_COMPILER_ID:GNU>:-Wpedantic>
    $<$<CXX_COMPILER_ID:Clang>:-Wdocumentation>
    $<$<CXX_COMPILER_ID:MSVC>:/permissive->
)

# =============================================================================
# Tests
# =============================================================================

if(UAST_BUILD_TESTS)
    enable_testing()
    include(CTest)

    add_executable(uast-tests
        ${TEST_DIR}/test_main.cpp
        ${TEST_DIR}/test_parser.cpp
        ${TEST_DIR}/test_lexer.cpp
    )

    target_link_libraries(uast-tests PRIVATE uast-core)

    if(GTest_FOUND)
        target_link_libraries(uast-tests PRIVATE GTest::GTest GTest::Main)
    endif()

    add_test(NAME unit_tests COMMAND uast-tests)
    set_tests_properties(unit_tests PROPERTIES
        TIMEOUT 60
        ENVIRONMENT "TEST_DATA_DIR=${TEST_DIR}/data"
    )
endif()

# =============================================================================
# Installation
# =============================================================================

include(GNUInstallDirs)

install(TARGETS UAST-Grep uast-core
    EXPORT UASTGrepTargets
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

install(DIRECTORY ${INCLUDE_DIR}/
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
    FILES_MATCHING PATTERN "*.h" PATTERN "*.hpp"
)

install(EXPORT UASTGrepTargets
    FILE UASTGrepTargets.cmake
    NAMESPACE UASTGrep::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/UASTGrep
)

# =============================================================================
# CPack Configuration
# =============================================================================

set(CPACK_PACKAGE_NAME ${PROJECT_NAME})
set(CPACK_PACKAGE_VERSION ${PROJECT_VERSION})
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY ${PROJECT_DESCRIPTION})
set(CPACK_GENERATOR "TGZ;ZIP")
include(CPack)

# =============================================================================
# Messages
# =============================================================================

message(STATUS "")
message(STATUS "UAST-Grep Configuration Summary:")
message(STATUS "  Version:        ${PROJECT_VERSION}")
message(STATUS "  Platform:       ${PLATFORM}")
message(STATUS "  Build type:     ${CMAKE_BUILD_TYPE}")
message(STATUS "  C++ Compiler:   ${CMAKE_CXX_COMPILER_ID} ${CMAKE_CXX_COMPILER_VERSION}")
message(STATUS "  Install prefix: ${CMAKE_INSTALL_PREFIX}")
message(STATUS "  Build tests:    ${UAST_BUILD_TESTS}")
message(STATUS "")
