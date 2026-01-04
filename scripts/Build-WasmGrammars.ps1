<#
.SYNOPSIS
    Compiles Tier 3 grammars to WebAssembly (.wasm) files.

.DESCRIPTION
    Uses tree-sitter CLI to compile grammar C sources to WASM format.
    These grammars are loaded at runtime for niche/large languages.

    PREREQUISITES:
    - tree-sitter CLI 0.26.3+: cargo install tree-sitter-cli
    - Emscripten SDK (for WASM compilation): https://emscripten.org/docs/getting_started/downloads.html
    - Grammar sources already downloaded to grammars/

.PARAMETER GrammarsDir
    Source directory with grammar C files. Default: native/uast_core/grammars

.PARAMETER OutputDir
    Output directory for .wasm files. Default: native/uast_core/grammars-wasm

.PARAMETER Languages
    Specific languages to compile. Default: all Tier 3 languages

.EXAMPLE
    .\Build-WasmGrammars.ps1
    Compiles all Tier 3 grammars to WASM.

.EXAMPLE
    .\Build-WasmGrammars.ps1 -Languages sql,cobol
    Compiles only SQL and COBOL to WASM.
#>

param(
    [string]$GrammarsDir = "$PSScriptRoot\..\native\uast_core\grammars",
    [string]$OutputDir = "$PSScriptRoot\..\native\uast_core\grammars-wasm",
    [string[]]$Languages
)

$ErrorActionPreference = 'Stop'

# Tier 3: Languages to compile to WASM (34 languages)
$Tier3Languages = @(
    "verilog", "latex", "sql", "fortran", "fsharp", "kotlin", "cobol",
    "scala", "objc", "julia", "d", "crystal", "cuda", "haskell", "swift",
    "perl", "arduino", "agda", "ocaml", "apex", "dart", "groovy",
    "commonlisp", "zig", "awk", "vim", "r", "bitbake", "ada", "cairo",
    "dhall", "cue", "doxygen", "comment"
)

function Write-Status {
    param([string]$Message, [string]$Color = "White")
    Write-Host $Message -ForegroundColor $Color
}

# Check prerequisites
$treeSitterCli = Get-Command tree-sitter -ErrorAction SilentlyContinue
if (-not $treeSitterCli) {
    Write-Status "ERROR: tree-sitter CLI not found. Install with: cargo install tree-sitter-cli" "Red"
    exit 1
}

# Check for Emscripten
$emcc = Get-Command emcc -ErrorAction SilentlyContinue
if (-not $emcc) {
    Write-Status "WARNING: Emscripten (emcc) not found. WASM compilation may fail." "Yellow"
    Write-Status "Install from: https://emscripten.org/docs/getting_started/downloads.html" "Yellow"
}

# Create output directory
$outputPath = $OutputDir
if (-not (Test-Path $outputPath)) {
    New-Item -ItemType Directory -Path $outputPath -Force | Out-Null
}

# Determine which languages to compile
$targetLanguages = if ($Languages) { $Languages } else { $Tier3Languages }

Write-Status ""
Write-Status "UAST-Grep WASM Grammar Compiler" "Cyan"
Write-Status "=================================" "Cyan"
Write-Status "Source: $GrammarsDir" "Gray"
Write-Status "Output: $OutputDir" "Gray"
Write-Status "Languages: $($targetLanguages.Count)" "Gray"
Write-Status ""

$grammarsPath = $GrammarsDir
$results = @()

foreach ($lang in $targetLanguages) {
    $grammarPath = Join-Path $grammarsPath $lang

    if (-not (Test-Path $grammarPath)) {
        Write-Status "[$lang] SKIP - grammar not found" "Yellow"
        $results += @{ Name = $lang; Success = $false; Error = "Not found" }
        continue
    }

    $srcDir = Join-Path $grammarPath "src"
    $parserC = Join-Path $srcDir "parser.c"

    if (-not (Test-Path $parserC)) {
        Write-Status "[$lang] SKIP - parser.c not found" "Yellow"
        $results += @{ Name = $lang; Success = $false; Error = "No parser.c" }
        continue
    }

    Write-Status "[$lang] Compiling to WASM..." "Gray"

    try {
        # Use tree-sitter build-wasm command
        # This requires the grammar to have a proper package.json or we compile manually

        $outputWasm = Join-Path $outputPath "$lang.wasm"

        # Compile using emcc directly if tree-sitter build-wasm fails
        Push-Location $grammarPath

        # Try tree-sitter build --wasm first
        $result = & tree-sitter build --wasm 2>&1

        if ($LASTEXITCODE -eq 0) {
            # tree-sitter outputs to tree-sitter-{lang}.wasm in current dir
            $generatedWasm = Get-ChildItem -Filter "*.wasm" -ErrorAction SilentlyContinue | Select-Object -First 1
            if ($generatedWasm) {
                Move-Item $generatedWasm.FullName $outputWasm -Force
                Write-Status "[$lang] SUCCESS - $([math]::Round((Get-Item $outputWasm).Length/1KB, 1)) KB" "Green"
                $results += @{ Name = $lang; Success = $true; Size = (Get-Item $outputWasm).Length }
            } else {
                throw "WASM file not generated"
            }
        } else {
            throw "tree-sitter build --wasm failed: $result"
        }

        Pop-Location
    }
    catch {
        Pop-Location -ErrorAction SilentlyContinue
        Write-Status "[$lang] FAILED - $_" "Red"
        $results += @{ Name = $lang; Success = $false; Error = $_.ToString() }
    }
}

# Summary
Write-Status ""
Write-Status "=================================" "Cyan"
$succeeded = ($results | Where-Object { $_.Success }).Count
$failed = ($results | Where-Object { -not $_.Success }).Count
$totalSize = ($results | Where-Object { $_.Success } | Measure-Object -Property Size -Sum).Sum

Write-Status "Results: $succeeded succeeded, $failed failed" $(if ($failed -eq 0) { "Green" } else { "Yellow" })
if ($totalSize -gt 0) {
    Write-Status "Total size: $([math]::Round($totalSize/1MB, 2)) MB" "Gray"
}

if ($failed -gt 0) {
    Write-Status ""
    Write-Status "Failed grammars:" "Red"
    $results | Where-Object { -not $_.Success } | ForEach-Object {
        Write-Status "  - $($_.Name): $($_.Error)" "Red"
    }
}

Write-Status ""
Write-Status "WASM files written to: $outputPath" "Cyan"
