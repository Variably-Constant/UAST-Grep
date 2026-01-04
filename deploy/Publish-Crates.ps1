<#
.SYNOPSIS
    Publishes the Rust crate to crates.io.

.DESCRIPTION
    Runs cargo publish to upload the uast-grep crate.
    Requires CARGO_REGISTRY_TOKEN or cargo login.

.PARAMETER DryRun
    Perform a dry run without actually publishing.

.EXAMPLE
    .\Publish-Crates.ps1 -DryRun
    Tests the publish without uploading.

.EXAMPLE
    .\Publish-Crates.ps1
    Publishes to crates.io
#>

[CmdletBinding()]
param(
    [switch]$DryRun
)

$ErrorActionPreference = 'Stop'
$ProjectRoot = Split-Path $PSScriptRoot -Parent
$CargoDir = Join-Path $ProjectRoot 'native/uast_core'

Write-Host ""
Write-Host "crates.io Publisher" -ForegroundColor Cyan
Write-Host "===================" -ForegroundColor Cyan

Push-Location $CargoDir

try {
    # Get current version from Cargo.toml
    $cargoToml = Get-Content 'Cargo.toml' -Raw
    if ($cargoToml -match 'version = "([^"]+)"') {
        $version = $Matches[1]
        Write-Host "Version: $version"
    }

    # Check if already published
    Write-Host ""
    Write-Host "Checking crates.io..." -ForegroundColor Gray
    $crateInfo = cargo search uast-grep 2>&1
    if ($crateInfo -match "uast-grep = `"([^`"]+)`"") {
        $publishedVersion = $Matches[1]
        Write-Host "Currently published: $publishedVersion"
        if ($publishedVersion -eq $version) {
            Write-Warning "Version $version is already published!"
            if (-not $DryRun) {
                Write-Host "Bump the version before publishing."
                exit 1
            }
        }
    } else {
        Write-Host "Not yet published to crates.io"
    }

    # Run pre-publish checks
    Write-Host ""
    Write-Host "Running pre-publish checks..." -ForegroundColor Yellow

    Write-Host "  cargo check..." -ForegroundColor Gray
    cargo check --release
    if ($LASTEXITCODE -ne 0) { throw "cargo check failed" }

    Write-Host "  cargo test..." -ForegroundColor Gray
    cargo test --release
    if ($LASTEXITCODE -ne 0) { throw "cargo test failed" }

    Write-Host "  cargo clippy..." -ForegroundColor Gray
    cargo clippy --release -- -D warnings 2>&1 | Out-Null
    # Don't fail on clippy warnings, just notify

    Write-Host "  cargo package --list..." -ForegroundColor Gray
    cargo package --list

    # Publish
    Write-Host ""
    if ($DryRun) {
        Write-Host "Dry run: cargo publish --dry-run" -ForegroundColor Yellow
        cargo publish --dry-run
    } else {
        Write-Host "Publishing to crates.io..." -ForegroundColor Yellow
        cargo publish
    }

    if ($LASTEXITCODE -eq 0) {
        Write-Host ""
        if ($DryRun) {
            Write-Host "Dry run completed successfully!" -ForegroundColor Green
            Write-Host "Run without -DryRun to publish." -ForegroundColor Yellow
        } else {
            Write-Host "Published to crates.io!" -ForegroundColor Green
            Write-Host "View at: https://crates.io/crates/uast-grep" -ForegroundColor Cyan
        }
    } else {
        throw "cargo publish failed"
    }
}
catch {
    Write-Host ""
    Write-Host "Failed: $_" -ForegroundColor Red
    exit 1
}
finally {
    Pop-Location
}
