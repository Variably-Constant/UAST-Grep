<#
.SYNOPSIS
    Creates release archives from built binaries.

.DESCRIPTION
    Packages Rust binaries into zip/tar.gz archives for GitHub release.
    Also packages WASM grammars into a single archive.

.PARAMETER Version
    The version tag (e.g., "1.0.0" or "v1.0.0").

.PARAMETER OutputDir
    Directory for release archives. Default: ./release/vX.Y.Z

.EXAMPLE
    .\Create-ReleaseArchives.ps1 -Version "1.0.0"
    Creates release archives in ./release/v1.0.0/
#>

[CmdletBinding()]
param(
    [Parameter(Mandatory)]
    [string]$Version,

    [string]$OutputDir
)

$ErrorActionPreference = 'Stop'
$ProjectRoot = Split-Path $PSScriptRoot -Parent
$CargoDir = Join-Path $ProjectRoot 'native/uast_core'

# Normalize version
if (-not $Version.StartsWith('v')) {
    $versionTag = "v$Version"
} else {
    $versionTag = $Version
    $Version = $Version.Substring(1)
}

# Set output directory
if (-not $OutputDir) {
    $OutputDir = Join-Path $ProjectRoot "release/$versionTag"
}

if (-not (Test-Path $OutputDir)) {
    New-Item -ItemType Directory -Path $OutputDir -Force | Out-Null
}

Write-Host ""
Write-Host "UAST-Grep Release Archive Creator" -ForegroundColor Cyan
Write-Host "==================================" -ForegroundColor Cyan
Write-Host "Version: $versionTag"
Write-Host "Output:  $OutputDir"
Write-Host ""

# Binary targets and their archive configs
$Binaries = @{
    'x86_64-pc-windows-msvc' = @{
        Binary = 'uast-grep.exe'
        Archive = "uast-grep-windows-x64.zip"
        Format = 'zip'
    }
    'x86_64-unknown-linux-gnu' = @{
        Binary = 'uast-grep'
        Archive = "uast-grep-linux-x64.tar.gz"
        Format = 'tar.gz'
    }
    'aarch64-unknown-linux-gnu' = @{
        Binary = 'uast-grep'
        Archive = "uast-grep-linux-arm64.tar.gz"
        Format = 'tar.gz'
    }
    'x86_64-apple-darwin' = @{
        Binary = 'uast-grep'
        Archive = "uast-grep-macos-x64.tar.gz"
        Format = 'tar.gz'
    }
    'aarch64-apple-darwin' = @{
        Binary = 'uast-grep'
        Archive = "uast-grep-macos-arm64.tar.gz"
        Format = 'tar.gz'
    }
}

$results = @()

# Create binary archives
foreach ($target in $Binaries.Keys) {
    $config = $Binaries[$target]
    $binaryPath = Join-Path $CargoDir "target/$target/release/$($config.Binary)"
    $archivePath = Join-Path $OutputDir $config.Archive

    if (-not (Test-Path $binaryPath)) {
        Write-Host "  Skipping $target (binary not found)" -ForegroundColor Yellow
        $results += [PSCustomObject]@{
            Target = $target
            Archive = $config.Archive
            Status = 'SKIPPED'
            Size = ''
        }
        continue
    }

    Write-Host "  Creating $($config.Archive)..." -ForegroundColor Gray

    try {
        if ($config.Format -eq 'zip') {
            # Windows zip
            Compress-Archive -Path $binaryPath -DestinationPath $archivePath -Force
        } else {
            # tar.gz (requires tar command)
            $binaryDir = Split-Path $binaryPath -Parent
            $binaryName = Split-Path $binaryPath -Leaf
            Push-Location $binaryDir
            tar -czvf $archivePath $binaryName 2>&1 | Out-Null
            Pop-Location
        }

        $size = "{0:N2} MB" -f ((Get-Item $archivePath).Length / 1MB)
        Write-Host "    Created: $($config.Archive) ($size)" -ForegroundColor Green
        $results += [PSCustomObject]@{
            Target = $target
            Archive = $config.Archive
            Status = 'SUCCESS'
            Size = $size
        }
    }
    catch {
        Write-Host "    Failed: $_" -ForegroundColor Red
        $results += [PSCustomObject]@{
            Target = $target
            Archive = $config.Archive
            Status = 'FAILED'
            Size = ''
        }
    }
}

# Create WASM grammar archive
$wasmDir = Join-Path $CargoDir 'grammars-wasm'
$wasmArchive = Join-Path $OutputDir 'grammars-wasm.zip'

if (Test-Path $wasmDir) {
    $wasmFiles = Get-ChildItem $wasmDir -Filter "*.wasm"
    if ($wasmFiles.Count -gt 0) {
        Write-Host "  Creating grammars-wasm.zip ($($wasmFiles.Count) files)..." -ForegroundColor Gray
        try {
            Compress-Archive -Path "$wasmDir/*.wasm" -DestinationPath $wasmArchive -Force
            $size = "{0:N2} MB" -f ((Get-Item $wasmArchive).Length / 1MB)
            Write-Host "    Created: grammars-wasm.zip ($size)" -ForegroundColor Green
            $results += [PSCustomObject]@{
                Target = 'WASM Grammars'
                Archive = 'grammars-wasm.zip'
                Status = 'SUCCESS'
                Size = $size
            }
        }
        catch {
            Write-Host "    Failed: $_" -ForegroundColor Red
            $results += [PSCustomObject]@{
                Target = 'WASM Grammars'
                Archive = 'grammars-wasm.zip'
                Status = 'FAILED'
                Size = ''
            }
        }
    }
} else {
    Write-Host "  WASM grammars not found (run Build-WasmGrammars.ps1 first)" -ForegroundColor Yellow
    $results += [PSCustomObject]@{
        Target = 'WASM Grammars'
        Archive = 'grammars-wasm.zip'
        Status = 'SKIPPED'
        Size = ''
    }
}

Write-Host ""
Write-Host "Archive Results" -ForegroundColor Cyan
Write-Host "===============" -ForegroundColor Cyan
$results | Format-Table -AutoSize

$archives = Get-ChildItem $OutputDir -File
Write-Host ""
Write-Host "Release archives created in: $OutputDir" -ForegroundColor Green
Write-Host "Total files: $($archives.Count)"
Write-Host ""
Write-Host "Next: Create GitHub release with:" -ForegroundColor Yellow
Write-Host "  gh release create $versionTag $OutputDir/*" -ForegroundColor Gray
