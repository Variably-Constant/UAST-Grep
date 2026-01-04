<#
.SYNOPSIS
    Publishes the Python package to PyPI.

.DESCRIPTION
    Uploads the uast-grep wheel to PyPI using twine or maturin.
    Requires TWINE_USERNAME/TWINE_PASSWORD or MATURIN_PYPI_TOKEN.

.PARAMETER UseMaturin
    Use maturin publish instead of twine (default: false).

.PARAMETER TestPyPI
    Publish to TestPyPI instead of PyPI.

.EXAMPLE
    .\Publish-PyPI.ps1
    Publishes using twine.

.EXAMPLE
    .\Publish-PyPI.ps1 -UseMaturin
    Publishes using maturin publish.

.EXAMPLE
    .\Publish-PyPI.ps1 -TestPyPI
    Publishes to TestPyPI for testing.
#>

[CmdletBinding()]
param(
    [switch]$UseMaturin,
    [switch]$TestPyPI
)

$ErrorActionPreference = 'Stop'
$ProjectRoot = Split-Path $PSScriptRoot -Parent
$PythonDir = Join-Path $ProjectRoot 'python'
$ArtifactsDir = Join-Path $ProjectRoot 'artifacts'

Write-Host ""
Write-Host "PyPI Publisher" -ForegroundColor Cyan
Write-Host "==============" -ForegroundColor Cyan

if ($UseMaturin) {
    # Use maturin publish
    $maturin = Get-Command maturin -ErrorAction SilentlyContinue
    if (-not $maturin) {
        Write-Error "maturin not found. Install with: pip install maturin"
        exit 1
    }

    # Check for token
    if (-not $env:MATURIN_PYPI_TOKEN) {
        Write-Warning "MATURIN_PYPI_TOKEN not set. You'll be prompted for credentials."
    }

    Push-Location $PythonDir
    try {
        $repoArg = if ($TestPyPI) { '--repository', 'testpypi' } else { @() }

        Write-Host "Publishing with maturin..." -ForegroundColor Yellow
        maturin publish --features python @repoArg

        if ($LASTEXITCODE -eq 0) {
            Write-Host ""
            Write-Host "Published to PyPI!" -ForegroundColor Green
        } else {
            throw "maturin publish failed"
        }
    }
    finally {
        Pop-Location
    }
} else {
    # Use twine
    $twine = Get-Command twine -ErrorAction SilentlyContinue
    if (-not $twine) {
        Write-Error "twine not found. Install with: pip install twine"
        exit 1
    }

    # Find wheel
    $wheels = @()
    $wheels += Get-ChildItem $ArtifactsDir -Filter "uast_grep*.whl" -ErrorAction SilentlyContinue
    $wheels += Get-ChildItem "$PythonDir/target/wheels" -Filter "*.whl" -ErrorAction SilentlyContinue

    if ($wheels.Count -eq 0) {
        Write-Error "No wheel found. Run Build-Packages.ps1 -Python first."
        exit 1
    }

    $wheel = $wheels | Sort-Object LastWriteTime -Descending | Select-Object -First 1
    Write-Host "Wheel: $($wheel.Name)"
    Write-Host "Size:  $("{0:N2} MB" -f ($wheel.Length / 1MB))"
    Write-Host ""

    # Extract version
    if ($wheel.Name -match 'uast_grep-([^-]+)-') {
        $version = $Matches[1]
        Write-Host "Version: $version"
    }

    # Check credentials
    if (-not $env:TWINE_USERNAME -or -not $env:TWINE_PASSWORD) {
        Write-Warning "TWINE_USERNAME/TWINE_PASSWORD not set."
        Write-Host "You can also use a ~/.pypirc file or be prompted."
    }

    # Repository URL
    $repoUrl = if ($TestPyPI) {
        'https://test.pypi.org/legacy/'
    } else {
        'https://upload.pypi.org/legacy/'
    }

    Write-Host "Repository: $(if ($TestPyPI) { 'TestPyPI' } else { 'PyPI' })"
    Write-Host ""

    # Confirm
    $confirm = Read-Host "Publish $($wheel.Name)? (y/N)"
    if ($confirm -ne 'y') {
        Write-Host "Cancelled." -ForegroundColor Yellow
        exit 0
    }

    # Check package before upload
    Write-Host ""
    Write-Host "Checking package..." -ForegroundColor Gray
    twine check $wheel.FullName

    # Upload
    Write-Host ""
    Write-Host "Uploading to $(if ($TestPyPI) { 'TestPyPI' } else { 'PyPI' })..." -ForegroundColor Yellow

    $twineArgs = @('upload')
    if ($TestPyPI) {
        $twineArgs += '--repository-url'
        $twineArgs += $repoUrl
    }
    $twineArgs += $wheel.FullName

    twine @twineArgs

    if ($LASTEXITCODE -eq 0) {
        Write-Host ""
        Write-Host "Published to $(if ($TestPyPI) { 'TestPyPI' } else { 'PyPI' })!" -ForegroundColor Green
        if ($TestPyPI) {
            Write-Host "View at: https://test.pypi.org/project/uast-grep/" -ForegroundColor Cyan
            Write-Host ""
            Write-Host "Test install with:" -ForegroundColor Gray
            Write-Host "  pip install -i https://test.pypi.org/simple/ uast-grep" -ForegroundColor Gray
        } else {
            Write-Host "View at: https://pypi.org/project/uast-grep/" -ForegroundColor Cyan
            Write-Host ""
            Write-Host "Install with:" -ForegroundColor Gray
            Write-Host "  pip install uast-grep" -ForegroundColor Gray
        }
    } else {
        Write-Host ""
        Write-Host "Failed to publish!" -ForegroundColor Red
        exit 1
    }
}
