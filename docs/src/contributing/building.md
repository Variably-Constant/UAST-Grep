# Building from Source

This guide covers building UAST-Grep from source code.

## Prerequisites

### Required Tools

| Tool | Version | Purpose |
|------|---------|---------|
| **Rust** | stable (1.70+) | Core library and CLI |
| **C Compiler** | GCC/Clang/MSVC | Grammar compilation |
| **Python** | 3.8+ | Python bindings |
| **maturin** | 1.0+ | Python build tool |
| **.NET SDK** | 8.0+ | .NET bindings |

### Installing Prerequisites

#### Windows

```powershell
# Install Rust
winget install Rustlang.Rustup
rustup update stable

# Install Python (includes pip)
winget install Python.Python.3.11

# Install maturin
pip install maturin

# Install .NET SDK
winget install Microsoft.DotNet.SDK.8

# Install Visual Studio Build Tools (for MSVC)
winget install Microsoft.VisualStudio.2022.BuildTools
# Select "Desktop development with C++"
```

#### Linux (Ubuntu/Debian)

```bash
# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env

# Install build tools
sudo apt-get update
sudo apt-get install -y build-essential pkg-config

# Install Python
sudo apt-get install -y python3 python3-pip python3-venv
pip install maturin

# Install .NET SDK
wget https://packages.microsoft.com/config/ubuntu/22.04/packages-microsoft-prod.deb
sudo dpkg -i packages-microsoft-prod.deb
sudo apt-get update
sudo apt-get install -y dotnet-sdk-8.0
```

#### macOS

```bash
# Install Xcode Command Line Tools
xcode-select --install

# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env

# Install Python (via Homebrew)
brew install python@3.11
pip3 install maturin

# Install .NET SDK
brew install --cask dotnet-sdk
```

## Building the Rust Core

The Rust core is the foundation of UAST-Grep.

### Clone the Repository

```bash
git clone https://github.com/MarkusMcNugen/UAST-Grep.git
cd UAST-Grep
```

### Build the CLI

```bash
cd native/uast_core

# Debug build (faster compilation, slower execution)
cargo build

# Release build (slower compilation, optimized)
cargo build --release

# Run tests
cargo test

# Run the CLI
cargo run -- languages
# or
./target/release/uast-grep languages
```

### Build Time Expectations

| Build Type | Time | Size |
|------------|------|------|
| Debug (first) | ~3-5 min | ~200MB |
| Debug (incremental) | ~10-30 sec | ~200MB |
| Release (first) | ~5-8 min | ~29MB |
| Release (incremental) | ~1-2 min | ~29MB |

The initial build is slow because it compiles 37 tree-sitter grammars.

### Build Features

```bash
# Build with WASM support (default)
cargo build --features wasm-download

# Build without WASM (smaller binary)
cargo build --no-default-features

# Build with built-in grammars only
cargo build --no-default-features --features builtin-grammars
```

## Building Python Bindings

The Python bindings use PyO3 and maturin.

### Development Build

```bash
cd UAST-Grep/python

# Create virtual environment (recommended)
python -m venv .venv
source .venv/bin/activate  # Linux/macOS
# or
.venv\Scripts\activate  # Windows

# Install in development mode
maturin develop --features python

# Run tests
pytest tests/
```

### Release Build

```bash
# Build wheel
maturin build --release --features python

# Wheel is in target/wheels/
ls target/wheels/
```

### Multiple Python Versions

```bash
# Build for specific Python version
maturin build --release --features python --interpreter python3.10

# Build for multiple versions
maturin build --release --features python --interpreter python3.9 python3.10 python3.11
```

## Building .NET Bindings

The .NET bindings use P/Invoke to call the Rust library.

### Build Native Library First

```bash
cd UAST-Grep/native/uast_core
cargo build --release

# Copy the native library
# Windows:
copy target\release\uast_core.dll ..\..\..\src\UAST.Native\runtimes\win-x64\native\

# Linux:
cp target/release/libuast_core.so ../../src/UAST.Native/runtimes/linux-x64/native/

# macOS:
cp target/release/libuast_core.dylib ../../src/UAST.Native/runtimes/osx-x64/native/
```

### Build .NET Project

```bash
cd UAST-Grep/src/UAST.Native

# Build
dotnet build -c Release

# Run tests
dotnet test ../UAST.Native.Tests -c Release

# Create NuGet package
dotnet pack -c Release -o ../../artifacts
```

## Cross-Compilation

### Windows to Linux

```bash
# Install cross
cargo install cross

# Build for Linux
cd native/uast_core
cross build --release --target x86_64-unknown-linux-gnu
```

### Linux to Windows

```bash
# Install MinGW toolchain
sudo apt-get install mingw-w64

# Add target
rustup target add x86_64-pc-windows-gnu

# Build
cargo build --release --target x86_64-pc-windows-gnu
```

### macOS Universal Binary

```bash
# Add ARM64 target
rustup target add aarch64-apple-darwin

# Build for both architectures
cargo build --release --target x86_64-apple-darwin
cargo build --release --target aarch64-apple-darwin

# Create universal binary
lipo -create \
    target/x86_64-apple-darwin/release/uast-grep \
    target/aarch64-apple-darwin/release/uast-grep \
    -output target/release/uast-grep-universal
```

## Build Troubleshooting

### Missing C Compiler

```
error: linker `cc` not found
```

Install a C compiler:
- Windows: Install Visual Studio Build Tools
- Linux: `sudo apt-get install build-essential`
- macOS: `xcode-select --install`

### Tree-sitter Grammar Compilation Fails

```
error: failed to run custom build command for `tree-sitter-...`
```

Try:
```bash
# Clean and rebuild
cargo clean
cargo build --release
```

### Python Version Mismatch

```
error: Python 3.x is required but Python 3.y was found
```

Specify the Python interpreter:
```bash
maturin develop --features python --interpreter python3.11
```

### .NET Native Library Not Found

```
DllNotFoundException: Unable to load DLL 'uast_core'
```

Ensure the native library is in the correct location:
```
UAST-Grep/
└── src/UAST.Native/
    └── runtimes/
        ├── win-x64/native/uast_core.dll
        ├── linux-x64/native/libuast_core.so
        └── osx-x64/native/libuast_core.dylib
```

## Development Workflow

### Making Changes

```bash
# Make changes to Rust code
cd native/uast_core

# Format code
cargo fmt

# Check for issues
cargo clippy

# Run tests
cargo test

# Build and test
cargo build --release
./target/release/uast-grep languages
```

### Running Benchmarks

```bash
cd native/uast_core
cargo bench
```

### Generating Documentation

```bash
cd native/uast_core
cargo doc --open
```

## Continuous Integration

See `.github/workflows/` for CI configuration:

- `build.yml` - Build and test on all platforms
- `release.yml` - Create releases on tag push
- `docs.yml` - Build and deploy documentation

## Next Steps

- [Adding Languages](adding-languages.md) - Add new tree-sitter grammars
- [Architecture](architecture.md) - Understand the codebase structure
