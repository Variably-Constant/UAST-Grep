<#
.SYNOPSIS
    Builds .NET and Python packages for distribution.

.DESCRIPTION
    Creates NuGet package (.nupkg) and Python wheel (.whl) for publishing.

.PARAMETER DotNet
    Build .NET package only.

.PARAMETER Python
    Build Python package only.

.EXAMPLE
    .\Build-Packages.ps1
    Builds both .NET and Python packages.

.EXAMPLE
    .\Build-Packages.ps1 -DotNet
    Builds .NET package only.
#>

[CmdletBinding()]
param(
    [switch]$DotNet,
    [switch]$Python
)

$ErrorActionPreference = 'Stop'
$ProjectRoot = Split-Path $PSScriptRoot -Parent

# If neither specified, build both
if (-not $DotNet -and -not $Python) {
    $DotNet = $true
    $Python = $true
}

$artifactsDir = Join-Path $ProjectRoot 'artifacts'
if (-not (Test-Path $artifactsDir)) {
    New-Item -ItemType Directory -Path $artifactsDir -Force | Out-Null
}

Write-Host ""
Write-Host "UAST-Grep Package Builder" -ForegroundColor Cyan
Write-Host "=========================" -ForegroundColor Cyan

$results = @()

# Build .NET package
if ($DotNet) {
    Write-Host ""
    Write-Host "Building .NET package..." -ForegroundColor Yellow

    $csprojPath = Join-Path $ProjectRoot 'src/UAST.Native/UAST.Native.csproj'

    if (-not (Test-Path $csprojPath)) {
        Write-Warning ".NET project not found: $csprojPath"
        $results += [PSCustomObject]@{
            Package = '.NET (UAST.Native)'
            Status = 'NOT FOUND'
            Output = ''
        }
    } else {
        Push-Location (Split-Path $csprojPath -Parent)
        try {
            # Build
            dotnet build -c Release
            if ($LASTEXITCODE -ne 0) { throw "dotnet build failed" }

            # Pack
            dotnet pack -c Release -o $artifactsDir --no-build
            if ($LASTEXITCODE -ne 0) { throw "dotnet pack failed" }

            $nupkg = Get-ChildItem $artifactsDir -Filter "UAST.Native.*.nupkg" | Sort-Object LastWriteTime -Descending | Select-Object -First 1
            if ($nupkg) {
                Write-Host "  Created: $($nupkg.Name)" -ForegroundColor Green
                $results += [PSCustomObject]@{
                    Package = '.NET (UAST.Native)'
                    Status = 'SUCCESS'
                    Output = $nupkg.FullName
                }
            }
        }
        catch {
            Write-Host "  Failed: $_" -ForegroundColor Red
            $results += [PSCustomObject]@{
                Package = '.NET (UAST.Native)'
                Status = 'FAILED'
                Output = ''
            }
        }
        finally {
            Pop-Location
        }
    }
}

# Build Python package
if ($Python) {
    Write-Host ""
    Write-Host "Building Python package..." -ForegroundColor Yellow

    $pythonDir = Join-Path $ProjectRoot 'python'

    if (-not (Test-Path $pythonDir)) {
        Write-Warning "Python project not found: $pythonDir"
        $results += [PSCustomObject]@{
            Package = 'Python (uast-grep)'
            Status = 'NOT FOUND'
            Output = ''
        }
    } else {
        Push-Location $pythonDir
        try {
            # Check for maturin
            $maturin = Get-Command maturin -ErrorAction SilentlyContinue
            if (-not $maturin) {
                throw "maturin not installed. Run: pip install maturin"
            }

            # Build wheel
            maturin build --release --features python
            if ($LASTEXITCODE -ne 0) { throw "maturin build failed" }

            $wheel = Get-ChildItem "target/wheels" -Filter "*.whl" | Sort-Object LastWriteTime -Descending | Select-Object -First 1
            if ($wheel) {
                # Copy to artifacts
                Copy-Item $wheel.FullName $artifactsDir -Force
                Write-Host "  Created: $($wheel.Name)" -ForegroundColor Green
                $results += [PSCustomObject]@{
                    Package = 'Python (uast-grep)'
                    Status = 'SUCCESS'
                    Output = (Join-Path $artifactsDir $wheel.Name)
                }
            }
        }
        catch {
            Write-Host "  Failed: $_" -ForegroundColor Red
            $results += [PSCustomObject]@{
                Package = 'Python (uast-grep)'
                Status = 'FAILED'
                Output = ''
            }
        }
        finally {
            Pop-Location
        }
    }
}

Write-Host ""
Write-Host "Build Results" -ForegroundColor Cyan
Write-Host "=============" -ForegroundColor Cyan
$results | Format-Table -AutoSize

Write-Host ""
Write-Host "Artifacts directory: $artifactsDir" -ForegroundColor Gray
