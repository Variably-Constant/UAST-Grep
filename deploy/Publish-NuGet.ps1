<#
.SYNOPSIS
    Publishes the .NET package to NuGet.org.

.DESCRIPTION
    Pushes the UAST.Native NuGet package.
    Requires NUGET_API_KEY environment variable.

.PARAMETER ApiKey
    NuGet API key. Default: uses NUGET_API_KEY env var.

.PARAMETER Source
    NuGet source URL. Default: nuget.org

.EXAMPLE
    .\Publish-NuGet.ps1
    Publishes using NUGET_API_KEY env var.

.EXAMPLE
    .\Publish-NuGet.ps1 -ApiKey "your-api-key"
    Publishes with explicit API key.
#>

[CmdletBinding()]
param(
    [string]$ApiKey,
    [string]$Source = 'https://api.nuget.org/v3/index.json'
)

$ErrorActionPreference = 'Stop'
$ProjectRoot = Split-Path $PSScriptRoot -Parent
$ArtifactsDir = Join-Path $ProjectRoot 'artifacts'

Write-Host ""
Write-Host "NuGet Publisher" -ForegroundColor Cyan
Write-Host "===============" -ForegroundColor Cyan

# Get API key
if (-not $ApiKey) {
    $ApiKey = $env:NUGET_API_KEY
}
if (-not $ApiKey) {
    Write-Error "No API key provided. Set NUGET_API_KEY environment variable or use -ApiKey parameter."
    exit 1
}

# Find the package
$packages = Get-ChildItem $ArtifactsDir -Filter "UAST.Native.*.nupkg" -ErrorAction SilentlyContinue
if (-not $packages -or $packages.Count -eq 0) {
    Write-Error "No NuGet package found in $ArtifactsDir"
    Write-Error "Run Build-Packages.ps1 -DotNet first."
    exit 1
}

$package = $packages | Sort-Object LastWriteTime -Descending | Select-Object -First 1
Write-Host "Package: $($package.Name)"
Write-Host "Size:    $("{0:N2} KB" -f ($package.Length / 1KB))"
Write-Host "Source:  $Source"
Write-Host ""

# Extract version from package name
if ($package.Name -match 'UAST\.Native\.(\d+\.\d+\.\d+)\.nupkg') {
    $version = $Matches[1]
    Write-Host "Version: $version"
}

# Check if already published
Write-Host ""
Write-Host "Checking NuGet.org..." -ForegroundColor Gray
try {
    $nugetInfo = Invoke-RestMethod "https://api.nuget.org/v3-flatcontainer/uast.native/index.json" -ErrorAction SilentlyContinue
    if ($nugetInfo.versions -contains $version) {
        Write-Warning "Version $version is already published on NuGet.org!"
        Write-Host "Bump the version before publishing."
        exit 1
    }
    Write-Host "Currently published versions: $($nugetInfo.versions[-3..-1] -join ', ')..."
}
catch {
    Write-Host "Package not yet on NuGet.org (first publish)"
}

# Confirm
Write-Host ""
$confirm = Read-Host "Publish $($package.Name) to NuGet.org? (y/N)"
if ($confirm -ne 'y') {
    Write-Host "Cancelled." -ForegroundColor Yellow
    exit 0
}

# Push
Write-Host ""
Write-Host "Publishing to NuGet.org..." -ForegroundColor Yellow
dotnet nuget push $package.FullName --api-key $ApiKey --source $Source

if ($LASTEXITCODE -eq 0) {
    Write-Host ""
    Write-Host "Published to NuGet.org!" -ForegroundColor Green
    Write-Host "View at: https://www.nuget.org/packages/UAST.Native/$version" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "Note: It may take a few minutes to appear in search results." -ForegroundColor Gray
} else {
    Write-Host ""
    Write-Host "Failed to publish!" -ForegroundColor Red
    exit 1
}
