# UAST-Grep Deployment Guide

> **INTERNAL DOCUMENT** - This file is excluded from git.
>
> Complete guide for releasing UAST-Grep to all package registries.

## Overview

UAST-Grep is distributed through four channels:

| Channel | Package | Audience |
|---------|---------|----------|
| **GitHub Releases** | Binaries + WASM | CLI users |
| **crates.io** | `uast-grep` | Rust developers |
| **NuGet** | `UAST.Native` | .NET developers |
| **PyPI** | `uast-grep` | Python developers |

---

## Quick Release Workflow

For a standard release, run these scripts in order:

```powershell
# 1. Verify/update version across all files
.\Check-Version.ps1 -Version "1.0.0" -Update

# 2. Build all artifacts
.\Build-RustBinaries.ps1 -AllPlatforms    # Requires cross for non-Windows
.\Build-Packages.ps1 -DotNet -Python

# 3. Create release archives
.\Create-ReleaseArchives.ps1 -Version "1.0.0"

# 4. Publish to all registries
.\Publish-GitHub.ps1 -Version "1.0.0"
.\Publish-Crates.ps1
.\Publish-NuGet.ps1
.\Publish-PyPI.ps1
```

---

## Prerequisites

### Required Tools

```bash
# Rust toolchain
rustup update stable
cargo install cargo-release cross

# .NET SDK
dotnet --version  # 8.0+

# Python packaging
pip install maturin twine build

# GitHub CLI
gh --version

# Cross-compilation targets (for multi-platform releases)
rustup target add x86_64-unknown-linux-gnu
rustup target add x86_64-unknown-linux-musl
rustup target add aarch64-unknown-linux-gnu
rustup target add x86_64-apple-darwin
rustup target add aarch64-apple-darwin
rustup target add x86_64-pc-windows-msvc
```

### Required Credentials

| Registry | Credential | Location |
|----------|------------|----------|
| GitHub | `GITHUB_TOKEN` | `gh auth login` or env var |
| crates.io | `CARGO_REGISTRY_TOKEN` | `~/.cargo/credentials.toml` |
| NuGet | `NUGET_API_KEY` | env var or `dotnet nuget` config |
| PyPI | `TWINE_USERNAME` + `TWINE_PASSWORD` | env vars or `~/.pypirc` |

---

## Deployment Scripts

All deployment scripts are in this `deploy/` folder:

| Script | Purpose |
|--------|---------|
| `Check-Version.ps1` | Verify/update version across all files |
| `Build-RustBinaries.ps1` | Build Rust binaries for all platforms |
| `Build-Packages.ps1` | Build .NET and Python packages |
| `Create-ReleaseArchives.ps1` | Create zip/tar.gz archives for GitHub |
| `Publish-GitHub.ps1` | Create GitHub release with assets |
| `Publish-Crates.ps1` | Publish to crates.io |
| `Publish-NuGet.ps1` | Publish to NuGet.org |
| `Publish-PyPI.ps1` | Publish to PyPI |

---

## Step 1: Version Management

### Semantic Versioning

```
MAJOR.MINOR.PATCH[-PRERELEASE]

Examples:
  1.0.0        - First stable release
  1.1.0        - New features, backward compatible
  1.1.1        - Bug fixes only
  2.0.0-alpha  - Breaking changes, pre-release
  2.0.0-rc.1   - Release candidate
```

### Files Updated by Check-Version.ps1

The script updates version in ALL of these files:

- `native/uast_core/Cargo.toml` - `version = "X.Y.Z"`
- `native/uast_core/src/wasm_loader.rs` - `GRAMMAR_VERSION = "vX.Y.Z"`
- `src/UAST.Native/UAST.Native.csproj` - `<Version>X.Y.Z</Version>`
- `python/pyproject.toml` - `version = "X.Y.Z"`
- `python/uast_grep/__init__.py` - `__version__ = "X.Y.Z"`

### Usage

```powershell
# Check if all versions match (read-only)
.\Check-Version.ps1 -Version "1.0.0"

# Update all files to new version
.\Check-Version.ps1 -Version "1.0.0" -Update
```

---

## Step 2: Build Artifacts

### Rust CLI Binaries

```powershell
# Windows only (fast, no cross-compilation needed)
.\Build-RustBinaries.ps1

# All platforms (requires cross for Linux/macOS)
.\Build-RustBinaries.ps1 -AllPlatforms

# Specific targets
.\Build-RustBinaries.ps1 -Targets "x86_64-pc-windows-msvc","x86_64-unknown-linux-gnu"
```

### WASM Grammars

```powershell
# From scripts/ folder
..\scripts\Build-WasmGrammars.ps1

# Output: native/uast_core/grammars-wasm/*.wasm (34 files)
```

### .NET and Python Packages

```powershell
# Build both
.\Build-Packages.ps1 -DotNet -Python

# Or individually
.\Build-Packages.ps1 -DotNet
.\Build-Packages.ps1 -Python

# Output:
#   artifacts/UAST.Native.X.Y.Z.nupkg
#   artifacts/uast_grep-X.Y.Z-*.whl
```

---

## Step 3: GitHub Release

### Create Release Archives

```powershell
# Create all archives for a version
.\Create-ReleaseArchives.ps1 -Version "1.0.0"

# Include WASM grammars
.\Create-ReleaseArchives.ps1 -Version "1.0.0" -IncludeWasm

# Output: release/v1.0.0/*.zip, *.tar.gz
```

### Publish to GitHub

```powershell
# Create release (interactive confirmation)
.\Publish-GitHub.ps1 -Version "1.0.0"

# Create as draft
.\Publish-GitHub.ps1 -Version "1.0.0" -Draft

# Create pre-release
.\Publish-GitHub.ps1 -Version "2.0.0-beta.1" -Prerelease
```

### Release Checklist

- [ ] All binaries tested on target platforms
- [ ] WASM grammars load correctly
- [ ] Version numbers updated everywhere
- [ ] CHANGELOG.md updated
- [ ] README.md examples work

---

## Step 4: Publish to crates.io

```powershell
# Dry run first (recommended)
.\Publish-Crates.ps1 -DryRun

# Publish for real
.\Publish-Crates.ps1

# Verify at https://crates.io/crates/uast-grep
```

### crates.io Checklist

- [ ] `Cargo.toml` has correct metadata (description, license, repository)
- [ ] All dependencies are published to crates.io
- [ ] No path dependencies (use version numbers)
- [ ] `README.md` is included and renders correctly

---

## Step 5: Publish to NuGet

```powershell
# Publish (requires NUGET_API_KEY env var or -ApiKey parameter)
.\Publish-NuGet.ps1

# Or with explicit API key
.\Publish-NuGet.ps1 -ApiKey "your-api-key"

# Verify at https://www.nuget.org/packages/UAST.Native
```

### NuGet Checklist

- [ ] `.csproj` has correct metadata (PackageId, Authors, Description)
- [ ] Native DLL is included in the package
- [ ] Package supports both net472 and net8.0
- [ ] README.md is included in package

---

## Step 6: Publish to PyPI

```powershell
# Publish with twine (default)
.\Publish-PyPI.ps1

# Publish with maturin
.\Publish-PyPI.ps1 -UseMaturin

# Test on TestPyPI first
.\Publish-PyPI.ps1 -TestPyPI

# Verify at https://pypi.org/project/uast-grep/
```

### PyPI Checklist

- [ ] `pyproject.toml` has correct metadata
- [ ] Wheel includes native library for target platform
- [ ] Type stubs are included (if applicable)
- [ ] README renders correctly on PyPI

---

## Step 7: Post-Release Verification

### GitHub Release

```bash
# Download and test binary
curl -LO https://github.com/Variably-Constant/UAST-Grep/releases/latest/download/uast-grep-linux-x64.tar.gz
tar xzf uast-grep-linux-x64.tar.gz
./uast-grep --version
./uast-grep languages | head -5
```

### crates.io

```bash
# Install from crates.io
cargo install uast-grep
uast-grep --version
```

### NuGet

```bash
# Create test project
dotnet new console -n UastTest
cd UastTest
dotnet add package UAST.Native
# Add test code and run
dotnet run
```

### PyPI

```bash
# Install from PyPI
pip install uast-grep
python -c "from uast_grep import supported_languages; print(len(supported_languages()))"
```

---

## Automated CI/CD (GitHub Actions)

For fully automated releases, create `.github/workflows/release.yml`:

```yaml
name: Release

on:
  push:
    tags:
      - 'v*'

jobs:
  build:
    strategy:
      matrix:
        include:
          - os: ubuntu-latest
            target: x86_64-unknown-linux-gnu
            asset: uast-grep-linux-x64.tar.gz
          - os: ubuntu-latest
            target: aarch64-unknown-linux-gnu
            asset: uast-grep-linux-arm64.tar.gz
          - os: macos-latest
            target: x86_64-apple-darwin
            asset: uast-grep-macos-x64.tar.gz
          - os: macos-latest
            target: aarch64-apple-darwin
            asset: uast-grep-macos-arm64.tar.gz
          - os: windows-latest
            target: x86_64-pc-windows-msvc
            asset: uast-grep-windows-x64.zip

    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
        with:
          targets: ${{ matrix.target }}
      - run: cargo build --release --target ${{ matrix.target }}
      - uses: actions/upload-artifact@v4
        with:
          name: ${{ matrix.asset }}
          path: target/${{ matrix.target }}/release/uast-grep*

  publish-crates:
    needs: build
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - run: cargo publish --token ${{ secrets.CARGO_REGISTRY_TOKEN }}
        working-directory: native/uast_core

  publish-nuget:
    needs: build
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-dotnet@v4
        with:
          dotnet-version: '8.0.x'
      - run: dotnet pack -c Release
        working-directory: src/UAST.Native
      - run: dotnet nuget push **/*.nupkg --api-key ${{ secrets.NUGET_API_KEY }} --source https://api.nuget.org/v3/index.json

  publish-pypi:
    needs: build
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: PyO3/maturin-action@v1
        with:
          command: publish
          args: --features python
        env:
          MATURIN_PYPI_TOKEN: ${{ secrets.PYPI_API_TOKEN }}
```

---

## Troubleshooting

### crates.io: "crate version already exists"

You cannot overwrite a published version. Bump the version number.

### NuGet: "package already exists"

Same as above - bump version. NuGet doesn't allow overwrites.

### PyPI: "file already exists"

Same - bump version or use `--skip-existing` for re-uploads of same version.

### GitHub Release: "tag already exists"

Delete the tag and recreate, or use a different version:
```bash
git tag -d v1.0.0
git push origin :refs/tags/v1.0.0
```

### Cross-compilation fails

Install cross-compilation toolchains:
```bash
# For Linux from macOS/Windows
cargo install cross
cross build --target x86_64-unknown-linux-gnu

# For macOS (requires actual macOS or osxcross)
# Best done via GitHub Actions on macos-latest runner
```

---

## Release Cadence

| Type | Frequency | Trigger |
|------|-----------|---------|
| **Major** | As needed | Breaking changes |
| **Minor** | Monthly | New features |
| **Patch** | As needed | Bug fixes |
| **Pre-release** | As needed | Testing before major |

---

## Contacts

- **Maintainer**: Mark Newton
- **Repository**: https://github.com/Variably-Constant/UAST-Grep
- **Issues**: https://github.com/Variably-Constant/UAST-Grep/issues

---

*Last updated: 2026-01-03*
