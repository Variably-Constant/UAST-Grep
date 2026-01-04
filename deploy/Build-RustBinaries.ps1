<#
.SYNOPSIS
    Builds Rust CLI binaries for all target platforms.

.DESCRIPTION
    Compiles the uast-grep CLI for Windows, Linux, and macOS using cargo/cross.
    Requires Rust toolchain and optionally 'cross' for cross-compilation.

.PARAMETER Targets
    Specific targets to build. Default: all supported targets.

.PARAMETER Release
    Build in release mode (default: true).

.EXAMPLE
    .\Build-RustBinaries.ps1
    Builds all platform binaries.

.EXAMPLE
    .\Build-RustBinaries.ps1 -Targets "x86_64-pc-windows-msvc"
    Builds only Windows x64.
#>

[CmdletBinding()]
param(
    [string[]]$Targets,

    [switch]$Debug
)

$ErrorActionPreference = 'Stop'
$ProjectRoot = Split-Path $PSScriptRoot -Parent
$CargoDir = Join-Path $ProjectRoot 'native/uast_core'

# All supported targets
$AllTargets = @{
    'x86_64-pc-windows-msvc' = @{
        Name = 'Windows x64'
        UseCross = $false
        Binary = 'uast-grep.exe'
        Archive = 'uast-grep-windows-x64.zip'
    }
    'x86_64-unknown-linux-gnu' = @{
        Name = 'Linux x64'
        UseCross = $true
        Binary = 'uast-grep'
        Archive = 'uast-grep-linux-x64.tar.gz'
    }
    'aarch64-unknown-linux-gnu' = @{
        Name = 'Linux ARM64'
        UseCross = $true
        Binary = 'uast-grep'
        Archive = 'uast-grep-linux-arm64.tar.gz'
    }
    'x86_64-apple-darwin' = @{
        Name = 'macOS x64'
        UseCross = $true
        Binary = 'uast-grep'
        Archive = 'uast-grep-macos-x64.tar.gz'
    }
    'aarch64-apple-darwin' = @{
        Name = 'macOS ARM64'
        UseCross = $true
        Binary = 'uast-grep'
        Archive = 'uast-grep-macos-arm64.tar.gz'
    }
}

# Filter targets if specified
if ($Targets) {
    $BuildTargets = @{}
    foreach ($t in $Targets) {
        if ($AllTargets.ContainsKey($t)) {
            $BuildTargets[$t] = $AllTargets[$t]
        } else {
            Write-Warning "Unknown target: $t"
        }
    }
} else {
    $BuildTargets = $AllTargets
}

$releaseFlag = if ($Debug) { '' } else { '--release' }
$buildType = if ($Debug) { 'debug' } else { 'release' }

Write-Host ""
Write-Host "UAST-Grep Rust Binary Builder" -ForegroundColor Cyan
Write-Host "==============================" -ForegroundColor Cyan
Write-Host "Build type: $buildType"
Write-Host "Targets: $($BuildTargets.Count)"
Write-Host ""

Push-Location $CargoDir

try {
    $results = @()

    foreach ($target in $BuildTargets.Keys) {
        $config = $BuildTargets[$target]
        Write-Host "Building $($config.Name) ($target)..." -ForegroundColor Yellow

        $cmd = if ($config.UseCross) { 'cross' } else { 'cargo' }

        # Check if cross is available for cross-compilation
        if ($config.UseCross) {
            $crossAvailable = Get-Command cross -ErrorAction SilentlyContinue
            if (-not $crossAvailable) {
                Write-Warning "  'cross' not installed, skipping $target"
                Write-Warning "  Install with: cargo install cross"
                $results += [PSCustomObject]@{
                    Target = $target
                    Name = $config.Name
                    Status = 'SKIPPED'
                    Binary = ''
                    Size = ''
                }
                continue
            }
        }

        # Ensure target is installed
        $rustupOutput = rustup target list --installed 2>&1
        if ($rustupOutput -notmatch $target) {
            Write-Host "  Installing target $target..." -ForegroundColor Gray
            rustup target add $target
        }

        # Build
        $buildArgs = @('build', '--target', $target)
        if ($releaseFlag) { $buildArgs += '--release' }

        $buildResult = & $cmd @buildArgs 2>&1
        $buildExitCode = $LASTEXITCODE

        if ($buildExitCode -eq 0) {
            $binaryPath = "target/$target/$buildType/$($config.Binary)"
            $size = if (Test-Path $binaryPath) {
                $bytes = (Get-Item $binaryPath).Length
                "{0:N2} MB" -f ($bytes / 1MB)
            } else { 'N/A' }

            Write-Host "  Success: $binaryPath ($size)" -ForegroundColor Green
            $results += [PSCustomObject]@{
                Target = $target
                Name = $config.Name
                Status = 'SUCCESS'
                Binary = $binaryPath
                Size = $size
            }
        } else {
            Write-Host "  Failed!" -ForegroundColor Red
            $results += [PSCustomObject]@{
                Target = $target
                Name = $config.Name
                Status = 'FAILED'
                Binary = ''
                Size = ''
            }
        }
    }

    Write-Host ""
    Write-Host "Build Results" -ForegroundColor Cyan
    Write-Host "=============" -ForegroundColor Cyan
    $results | Format-Table -AutoSize

    $succeeded = ($results | Where-Object { $_.Status -eq 'SUCCESS' }).Count
    $failed = ($results | Where-Object { $_.Status -eq 'FAILED' }).Count

    if ($failed -gt 0) {
        Write-Host "Completed with errors: $succeeded succeeded, $failed failed" -ForegroundColor Yellow
        exit 1
    } else {
        Write-Host "All builds completed successfully!" -ForegroundColor Green
        exit 0
    }
}
finally {
    Pop-Location
}
