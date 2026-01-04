<#
.SYNOPSIS
    Regenerates tree-sitter grammar files for tree-sitter 0.26.3 ABI compatibility.

.DESCRIPTION
    This script clones each grammar repository, runs `tree-sitter generate` to create
    parser.c files compatible with the current tree-sitter version, then copies them
    to the grammars/ directory.

    KNOWN EDGE CASES HANDLED:
    - Monorepo grammars (fsharp, ocaml, php, tsx, typescript, xml) have shared
      common/scanner.h files. These are flattened to scanner_common.h and include
      paths are fixed automatically.
    - Extra header files (vim has keywords.h, vue has tag.h) are copied automatically.
    - Extra source files (yaml has schema.*.c) are copied automatically.
    - COBOL exports tree_sitter_COBOL (uppercase) - handled in builtin_languages.rs.

    PREREQUISITES:
    - tree-sitter CLI 0.26.3+: cargo install tree-sitter-cli
    - Node.js/npm (for grammars with package.json)
    - Git

.PARAMETER GrammarsDir
    The output directory for grammar sources. Default: native/uast_core/grammars

.PARAMETER TempDir
    Temporary directory for cloning repos. Default: $env:TEMP/ts-grammar-build

.PARAMETER Languages
    Specific languages to regenerate. Default: all

.PARAMETER Parallel
    Number of parallel jobs. Default: 4

.EXAMPLE
    .\Build-NativeGrammars.ps1
    Regenerates all 70 grammars.

.EXAMPLE
    .\Build-NativeGrammars.ps1 -Languages python,javascript,rust
    Regenerates only the specified languages.
#>

[CmdletBinding()]
param(
    [string]$GrammarsDir = "$PSScriptRoot\..\native\uast_core\grammars",
    [string]$TempDir = "$env:TEMP\ts-grammar-build",
    [string[]]$Languages,
    [int]$Parallel = 4
)

# ============================================================================
# Grammar Registry - All 71 languages with their GitHub repositories
# ============================================================================
#
# TIER SYSTEM:
# - BUILTIN (37 languages): Compiled into the binary for fast access
# - WASM (34 languages): Loaded from .wasm files at runtime (niche/large)
#
# This allows the binary to stay small (~30 MB) while still supporting
# all 71 languages when WASM files are available.
# ============================================================================

$GrammarRegistry = @{
    # =========================================================================
    # BUILTIN GRAMMARS (37 languages) - Compiled into binary
    # =========================================================================

    # Core (essential for PowerShell ecosystem)
    "powershell" = @{ Repo = "https://github.com/airbus-cert/tree-sitter-powershell"; Tier = "BUILTIN" }
    "c-sharp"    = @{ Repo = "https://github.com/tree-sitter/tree-sitter-c-sharp"; Tier = "BUILTIN" }

    # Data formats (PS parses these constantly)
    "json"       = @{ Repo = "https://github.com/tree-sitter/tree-sitter-json"; Tier = "BUILTIN" }
    "yaml"       = @{ Repo = "https://github.com/tree-sitter-grammars/tree-sitter-yaml"; Tier = "BUILTIN" }
    "xml"        = @{ Repo = "https://github.com/tree-sitter-grammars/tree-sitter-xml"; Tier = "BUILTIN"; SubDir = "xml" }
    "toml"       = @{ Repo = "https://github.com/tree-sitter-grammars/tree-sitter-toml"; Tier = "BUILTIN" }
    "csv"        = @{ Repo = "https://github.com/tree-sitter-grammars/tree-sitter-csv"; Tier = "BUILTIN" }

    # DevOps (PS is THE DevOps language on Windows)
    "dockerfile" = @{ Repo = "https://github.com/camdencheek/tree-sitter-dockerfile"; Tier = "BUILTIN" }
    "hcl"        = @{ Repo = "https://github.com/tree-sitter-grammars/tree-sitter-hcl"; Tier = "BUILTIN" }
    "bicep"      = @{ Repo = "https://github.com/tree-sitter-grammars/tree-sitter-bicep"; Tier = "BUILTIN" }
    "nix"        = @{ Repo = "https://github.com/nix-community/tree-sitter-nix"; Tier = "BUILTIN" }
    "bash"       = @{ Repo = "https://github.com/tree-sitter/tree-sitter-bash"; Tier = "BUILTIN" }

    # Web (PS automation targets)
    "html"       = @{ Repo = "https://github.com/tree-sitter/tree-sitter-html"; Tier = "BUILTIN" }
    "css"        = @{ Repo = "https://github.com/tree-sitter/tree-sitter-css"; Tier = "BUILTIN" }
    "javascript" = @{ Repo = "https://github.com/tree-sitter/tree-sitter-javascript"; Tier = "BUILTIN" }
    "typescript" = @{ Repo = "https://github.com/tree-sitter/tree-sitter-typescript"; Tier = "BUILTIN"; SubDir = "typescript" }
    "tsx"        = @{ Repo = "https://github.com/tree-sitter/tree-sitter-typescript"; Tier = "BUILTIN"; SubDir = "tsx" }
    "vue"        = @{ Repo = "https://github.com/tree-sitter-grammars/tree-sitter-vue"; Tier = "BUILTIN" }
    "angular"    = @{ Repo = "https://github.com/dlvandenberg/tree-sitter-angular"; Tier = "BUILTIN" }

    # Documentation
    "markdown"   = @{ Repo = "https://github.com/tree-sitter-grammars/tree-sitter-markdown"; Tier = "BUILTIN"; SubDir = "tree-sitter-markdown" }

    # Backend (common languages PS integrates with)
    "python"     = @{ Repo = "https://github.com/tree-sitter/tree-sitter-python"; Tier = "BUILTIN" }
    "go"         = @{ Repo = "https://github.com/tree-sitter/tree-sitter-go"; Tier = "BUILTIN" }
    "java"       = @{ Repo = "https://github.com/tree-sitter/tree-sitter-java"; Tier = "BUILTIN" }
    "c"          = @{ Repo = "https://github.com/tree-sitter/tree-sitter-c"; Tier = "BUILTIN" }
    "cpp"        = @{ Repo = "https://github.com/tree-sitter/tree-sitter-cpp"; Tier = "BUILTIN" }
    "rust"       = @{ Repo = "https://github.com/tree-sitter/tree-sitter-rust"; Tier = "BUILTIN" }

    # Scripting
    "ruby"       = @{ Repo = "https://github.com/tree-sitter/tree-sitter-ruby"; Tier = "BUILTIN" }
    "php"        = @{ Repo = "https://github.com/tree-sitter/tree-sitter-php"; Tier = "BUILTIN"; SubDir = "php" }
    "lua"        = @{ Repo = "https://github.com/tree-sitter-grammars/tree-sitter-lua"; Tier = "BUILTIN" }

    # Build/Config
    "cmake"      = @{ Repo = "https://github.com/uyha/tree-sitter-cmake"; Tier = "BUILTIN" }
    "make"       = @{ Repo = "https://github.com/tree-sitter-grammars/tree-sitter-make"; Tier = "BUILTIN" }
    "proto"      = @{ Repo = "https://github.com/mitchellh/tree-sitter-proto"; Tier = "BUILTIN" }
    "graphql"    = @{ Repo = "https://github.com/bkegley/tree-sitter-graphql"; Tier = "BUILTIN" }

    # Functional
    "elixir"     = @{ Repo = "https://github.com/elixir-lang/tree-sitter-elixir"; Tier = "BUILTIN" }
    "erlang"     = @{ Repo = "https://github.com/WhatsApp/tree-sitter-erlang"; Tier = "BUILTIN" }
    "clojure"    = @{ Repo = "https://github.com/sogaiu/tree-sitter-clojure"; Tier = "BUILTIN" }
    "elm"        = @{ Repo = "https://github.com/elm-tooling/tree-sitter-elm"; Tier = "BUILTIN" }

    # =========================================================================
    # WASM GRAMMARS (34 languages) - Loaded from .wasm files at runtime
    # =========================================================================
    # These are either very large (>15MB parser.c) or rarely used.
    # Use Build-WasmGrammars.ps1 to compile these to .wasm files.

    # Very large grammars (>15MB parser.c)
    "verilog"    = @{ Repo = "https://github.com/tree-sitter/tree-sitter-verilog"; Tier = "WASM" }
    "latex"      = @{ Repo = "https://github.com/latex-lsp/tree-sitter-latex"; Tier = "WASM" }
    "sql"        = @{ Repo = "https://github.com/DerekStride/tree-sitter-sql"; Tier = "WASM" }
    "fortran"    = @{ Repo = "https://github.com/stadelmanma/tree-sitter-fortran"; Tier = "WASM" }
    "fsharp"     = @{ Repo = "https://github.com/ionide/tree-sitter-fsharp"; Tier = "WASM"; SubDir = "fsharp" }
    "kotlin"     = @{ Repo = "https://github.com/fwcd/tree-sitter-kotlin"; Tier = "WASM" }
    "cobol"      = @{ Repo = "https://github.com/yutaro-sakamoto/tree-sitter-cobol"; Tier = "WASM" }
    "scala"      = @{ Repo = "https://github.com/tree-sitter/tree-sitter-scala"; Tier = "WASM" }
    "objc"       = @{ Repo = "https://github.com/tree-sitter-grammars/tree-sitter-objc"; Tier = "WASM" }
    "julia"      = @{ Repo = "https://github.com/tree-sitter/tree-sitter-julia"; Tier = "WASM" }
    "d"          = @{ Repo = "https://github.com/gdamore/tree-sitter-d"; Tier = "WASM" }
    "crystal"    = @{ Repo = "https://github.com/keidax/tree-sitter-crystal"; Tier = "WASM" }
    "cuda"       = @{ Repo = "https://github.com/tree-sitter-grammars/tree-sitter-cuda"; Tier = "WASM" }
    "haskell"    = @{ Repo = "https://github.com/tree-sitter/tree-sitter-haskell"; Tier = "WASM" }
    "swift"      = @{ Repo = "https://github.com/alex-pinkus/tree-sitter-swift"; Tier = "WASM" }
    "perl"       = @{ Repo = "https://github.com/tree-sitter-perl/tree-sitter-perl"; Tier = "WASM" }
    "arduino"    = @{ Repo = "https://github.com/tree-sitter-grammars/tree-sitter-arduino"; Tier = "WASM" }
    "agda"       = @{ Repo = "https://github.com/tree-sitter/tree-sitter-agda"; Tier = "WASM" }
    "ocaml"      = @{ Repo = "https://github.com/tree-sitter/tree-sitter-ocaml"; Tier = "WASM"; SubDir = "grammars/ocaml" }

    # Niche/specialized grammars
    "apex"       = @{ Repo = "https://github.com/aheber/tree-sitter-sfapex"; Tier = "WASM"; SubDir = "sfapex" }
    "dart"       = @{ Repo = "https://github.com/UserNobody14/tree-sitter-dart"; Tier = "WASM" }
    "groovy"     = @{ Repo = "https://github.com/murtaza64/tree-sitter-groovy"; Tier = "WASM" }
    "commonlisp" = @{ Repo = "https://github.com/theHamsta/tree-sitter-commonlisp"; Tier = "WASM" }
    "zig"        = @{ Repo = "https://github.com/tree-sitter-grammars/tree-sitter-zig"; Tier = "WASM" }
    "awk"        = @{ Repo = "https://github.com/Beaglefoot/tree-sitter-awk"; Tier = "WASM" }
    "vim"        = @{ Repo = "https://github.com/tree-sitter-grammars/tree-sitter-vim"; Tier = "WASM" }
    "r"          = @{ Repo = "https://github.com/r-lib/tree-sitter-r"; Tier = "WASM" }
    "bitbake"    = @{ Repo = "https://github.com/tree-sitter-grammars/tree-sitter-bitbake"; Tier = "WASM" }
    "ada"        = @{ Repo = "https://github.com/briot/tree-sitter-ada"; Tier = "WASM" }
    "cairo"      = @{ Repo = "https://github.com/tree-sitter-grammars/tree-sitter-cairo"; Tier = "WASM" }
    "dhall"      = @{ Repo = "https://github.com/jbellerb/tree-sitter-dhall"; Tier = "WASM" }
    "cue"        = @{ Repo = "https://github.com/eonpatapon/tree-sitter-cue"; Tier = "WASM" }
    "doxygen"    = @{ Repo = "https://github.com/tree-sitter-grammars/tree-sitter-doxygen"; Tier = "WASM" }
    "comment"    = @{ Repo = "https://github.com/stsewd/tree-sitter-comment"; Tier = "WASM" }
}

# ============================================================================
# Helper Functions
# ============================================================================

function Write-Status {
    param([string]$Message, [string]$Color = "White")
    Write-Host $Message -ForegroundColor $Color
}

function Build-Grammar {
    param(
        [string]$Name,
        [hashtable]$Config,
        [string]$TempDir,
        [string]$GrammarsDir
    )

    $repo = $Config.Repo
    $subDir = $Config.SubDir
    $cloneDir = Join-Path $TempDir $Name
    $destDir = Join-Path $GrammarsDir "$Name\src"

    try {
        Write-Status "[$Name] Cloning from $repo..." "Cyan"

        # Clone shallow
        $cloneResult = git clone --depth 1 $repo $cloneDir 2>&1
        if ($LASTEXITCODE -ne 0) {
            throw "Clone failed: $cloneResult"
        }

        # Navigate to subdirectory if specified
        $workDir = if ($subDir) { Join-Path $cloneDir $subDir } else { $cloneDir }
        if (-not (Test-Path $workDir)) {
            # Try common patterns
            $possibleDirs = @(
                (Join-Path $cloneDir $subDir),
                (Join-Path $cloneDir "grammars\$Name"),
                (Join-Path $cloneDir $Name)
            )
            foreach ($dir in $possibleDirs) {
                if (Test-Path $dir) {
                    $workDir = $dir
                    break
                }
            }
        }

        Push-Location $workDir

        # Install npm dependencies if needed
        if (Test-Path "package.json") {
            Write-Status "[$Name] Installing npm dependencies..." "Gray"
            $npmResult = npm install --ignore-scripts 2>&1
            # Ignore npm errors, they're often not critical
        }

        # Generate grammar
        Write-Status "[$Name] Running tree-sitter generate..." "Yellow"
        $genResult = tree-sitter generate 2>&1
        if ($LASTEXITCODE -ne 0) {
            # Check if it's just a warning
            if ($genResult -notmatch "error") {
                Write-Status "[$Name] Warning during generate: $genResult" "Yellow"
            } else {
                throw "Generate failed: $genResult"
            }
        }

        Pop-Location

        # Find and copy parser files
        $srcDir = Join-Path $workDir "src"
        if (-not (Test-Path $srcDir)) {
            throw "No src directory found after generate"
        }

        # Ensure destination exists
        New-Item -ItemType Directory -Path $destDir -Force | Out-Null

        # Copy parser.c (required)
        $parserC = Join-Path $srcDir "parser.c"
        if (-not (Test-Path $parserC)) {
            throw "parser.c not found in $srcDir"
        }
        Copy-Item $parserC -Destination $destDir -Force

        # Copy scanner files (optional)
        $scannerC = Join-Path $srcDir "scanner.c"
        $scannerCC = Join-Path $srcDir "scanner.cc"
        if (Test-Path $scannerC) {
            Copy-Item $scannerC -Destination $destDir -Force
        }
        if (Test-Path $scannerCC) {
            Copy-Item $scannerCC -Destination $destDir -Force
        }

        # Copy ALL header files (some grammars need extra .h files like tag.h, keywords.h)
        $headerFiles = Get-ChildItem -Path $srcDir -Filter "*.h" -ErrorAction SilentlyContinue
        foreach ($header in $headerFiles) {
            Copy-Item $header.FullName -Destination $destDir -Force
        }

        # Copy extra .c files for grammars that need them (e.g., yaml has schema.*.c)
        $extraCFiles = Get-ChildItem -Path $srcDir -Filter "*.c" -ErrorAction SilentlyContinue |
            Where-Object { $_.Name -ne "parser.c" -and $_.Name -ne "scanner.c" }
        foreach ($cFile in $extraCFiles) {
            Copy-Item $cFile.FullName -Destination $destDir -Force
            Write-Status "[$Name] Copied extra: $($cFile.Name)" "Gray"
        }

        # ====================================================================
        # POST-PROCESSING: Handle monorepo grammars with shared scanner files
        # Some grammars use #include "../../common/scanner.h" which won't work
        # after copying to our flat directory structure. We flatten these.
        # ====================================================================

        # Check if this is a monorepo grammar with a common/ directory
        $commonDir = Join-Path (Split-Path $workDir -Parent) "common"
        if (Test-Path $commonDir) {
            $commonScannerH = Join-Path $commonDir "scanner.h"
            if (Test-Path $commonScannerH) {
                # Copy common/scanner.h as scanner_common.h
                $destCommonH = Join-Path $destDir "scanner_common.h"
                Copy-Item $commonScannerH -Destination $destCommonH -Force
                Write-Status "[$Name] Copied common scanner header" "Gray"

                # Fix the #include in scanner.c to use local file
                $scannerCPath = Join-Path $destDir "scanner.c"
                if (Test-Path $scannerCPath) {
                    $content = Get-Content $scannerCPath -Raw
                    # Replace various relative paths to common/scanner.h
                    $content = $content -replace '#include\s+"\.\.\/\.\.\/common\/scanner\.h"', '#include "scanner_common.h"'
                    $content = $content -replace '#include\s+"\.\.\/\.\.\/\.\.\/common\/scanner\.h"', '#include "scanner_common.h"'
                    Set-Content $scannerCPath -Value $content -NoNewline
                    Write-Status "[$Name] Fixed scanner.c include paths" "Gray"
                }
            }
        }

        # Cleanup
        Remove-Item -Recurse -Force $cloneDir -ErrorAction SilentlyContinue

        Write-Status "[$Name] SUCCESS" "Green"
        return @{ Name = $Name; Success = $true; Error = $null }
    }
    catch {
        Pop-Location -ErrorAction SilentlyContinue
        Remove-Item -Recurse -Force $cloneDir -ErrorAction SilentlyContinue
        Write-Status "[$Name] FAILED: $_" "Red"
        return @{ Name = $Name; Success = $false; Error = $_.ToString() }
    }
}

# ============================================================================
# Main Script
# ============================================================================

Write-Status "=" * 60 "Cyan"
Write-Status "  UAST-Grep Grammar Regeneration Script" "Cyan"
Write-Status "  tree-sitter version: $(tree-sitter --version)" "Cyan"
Write-Status "=" * 60 "Cyan"

# Verify prerequisites
$treeSitterVersion = tree-sitter --version 2>$null
if (-not $treeSitterVersion) {
    Write-Status "ERROR: tree-sitter CLI not found. Install with: cargo install tree-sitter-cli" "Red"
    exit 1
}
Write-Status "Using: $treeSitterVersion" "Gray"

# Filter languages if specified
$targetGrammars = if ($Languages -and $Languages.Count -gt 0) {
    $GrammarRegistry.GetEnumerator() | Where-Object { $Languages -contains $_.Key }
} else {
    $GrammarRegistry.GetEnumerator()
}

$total = ($targetGrammars | Measure-Object).Count
Write-Status "Regenerating $total grammars..." "White"
Write-Status "Output: $GrammarsDir" "Gray"
Write-Status ""

# Create temp directory
New-Item -ItemType Directory -Path $TempDir -Force | Out-Null

# Process grammars
$results = @()
$count = 0

foreach ($grammar in $targetGrammars) {
    $count++
    Write-Status "[$count/$total] Processing $($grammar.Key)..." "White"
    $result = Build-Grammar -Name $grammar.Key -Config $grammar.Value -TempDir $TempDir -GrammarsDir $GrammarsDir
    $results += $result
}

# Summary
Write-Status ""
Write-Status "=" * 60 "Cyan"
Write-Status "  BUILD SUMMARY" "Cyan"
Write-Status "=" * 60 "Cyan"

$succeeded = $results | Where-Object { $_.Success }
$failed = $results | Where-Object { -not $_.Success }

Write-Status "Succeeded: $($succeeded.Count)/$total" "Green"
if ($failed.Count -gt 0) {
    Write-Status "Failed: $($failed.Count)" "Red"
    Write-Status ""
    Write-Status "Failed grammars:" "Yellow"
    foreach ($f in $failed) {
        Write-Status "  - $($f.Name): $($f.Error)" "Red"
    }
}

# Cleanup temp
Remove-Item -Recurse -Force $TempDir -ErrorAction SilentlyContinue

Write-Status ""
Write-Status "Done! Run 'cargo build' in native/uast_core to compile." "Green"
