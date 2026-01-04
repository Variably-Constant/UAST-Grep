# Installation

UAST-Grep can be installed multiple ways depending on your needs.

## Pre-built Binaries (Recommended)

Download the latest release for your platform from [GitHub Releases](https://github.com/Variably-Constant/UAST-Grep/releases):

### Windows

```powershell
# PowerShell
Invoke-WebRequest -Uri "https://github.com/Variably-Constant/UAST-Grep/releases/latest/download/uast-grep-windows-x64.zip" -OutFile "uast-grep.zip"
Expand-Archive -Path "uast-grep.zip" -DestinationPath "."
.\uast-grep.exe --version
```

Or with curl:

```bash
curl -LO https://github.com/Variably-Constant/UAST-Grep/releases/latest/download/uast-grep-windows-x64.zip
unzip uast-grep-windows-x64.zip
./uast-grep.exe --version
```

### Linux

```bash
# x64
curl -LO https://github.com/Variably-Constant/UAST-Grep/releases/latest/download/uast-grep-linux-x64.tar.gz
tar xzf uast-grep-linux-x64.tar.gz
chmod +x uast-grep
./uast-grep --version

# ARM64
curl -LO https://github.com/Variably-Constant/UAST-Grep/releases/latest/download/uast-grep-linux-arm64.tar.gz
tar xzf uast-grep-linux-arm64.tar.gz
chmod +x uast-grep
./uast-grep --version
```

### macOS

```bash
# Apple Silicon (M1/M2/M3)
curl -LO https://github.com/Variably-Constant/UAST-Grep/releases/latest/download/uast-grep-macos-arm64.tar.gz
tar xzf uast-grep-macos-arm64.tar.gz
chmod +x uast-grep
./uast-grep --version

# Intel
curl -LO https://github.com/Variably-Constant/UAST-Grep/releases/latest/download/uast-grep-macos-x64.tar.gz
tar xzf uast-grep-macos-x64.tar.gz
chmod +x uast-grep
./uast-grep --version
```

## From Source (Rust)

Building from source requires the Rust toolchain:

```bash
# Install Rust if you don't have it
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Clone and build
git clone https://github.com/Variably-Constant/UAST-Grep.git
cd UAST-Grep/native/uast_core
cargo build --release

# Binary is at target/release/uast-grep
./target/release/uast-grep --version
```

> **Note:** Building from source compiles 37 tree-sitter grammars and takes approximately 3 minutes on a modern machine.

## Python Package

### From PyPI

```bash
pip install uast-grep
```

### From Source

```bash
git clone https://github.com/Variably-Constant/UAST-Grep.git
cd UAST-Grep/python

# Install maturin (build tool for PyO3)
pip install maturin

# Build and install
maturin develop --features python
```

### Verify Installation

```python
from uast_grep import supported_languages, version

print(f"UAST-Grep version: {version()}")
print(f"Supported languages: {len(supported_languages())}")
```

## .NET Library

### From NuGet

```bash
dotnet add package UAST.Native
```

### From Source

```bash
git clone https://github.com/Variably-Constant/UAST-Grep.git
cd UAST-Grep

# Build the native library first (requires Rust)
cd native/uast_core
cargo build --release
cd ../..

# Build .NET wrapper
dotnet build src/UAST.Native -c Release
```

### Verify Installation

```csharp
using UAST.Native;

UastNative.Init();
Console.WriteLine($"Version: {UastNativeExtensions.GetVersion()}");
Console.WriteLine($"Languages: {UastNative.LanguageCount()}");
```

## WASM Grammars

For languages not built into the binary (like Kotlin, Scala, or Swift), UAST-Grep downloads WASM grammars on first use. These are cached in:

- **Linux/macOS:** `~/.uast/grammars/`
- **Windows:** `%USERPROFILE%\.uast\grammars\`

### Offline Installation

For air-gapped environments, download the WASM grammar bundle:

```bash
curl -LO https://github.com/Variably-Constant/UAST-Grep/releases/latest/download/grammars-wasm.zip
unzip grammars-wasm.zip -d ~/.uast/grammars/
```

Or set a custom path:

```bash
export UAST_WASM_PATH=/path/to/grammars
```

### Disable Auto-Download

To prevent automatic downloads:

```bash
export UAST_OFFLINE=1
```

## Verifying Installation

After installation, verify everything works:

```bash
# Check version
uast-grep --version

# List supported languages
uast-grep languages

# Test a simple search
echo 'def hello(): pass' | uast-grep run -p FunctionDeclaration -l python --stdin
```

## Next Steps

Now that you have UAST-Grep installed, head to the [Quick Start](quick-start.md) guide to learn the basics!
