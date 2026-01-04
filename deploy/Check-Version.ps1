<#
.SYNOPSIS
    Verifies all version numbers are synchronized across the project.

.DESCRIPTION
    Checks that the version in Cargo.toml, wasm_loader.rs, .csproj, and pyproject.toml
    all match. Optionally updates them to a new version.

.PARAMETER Version
    The expected version to check against, or the new version to set.

.PARAMETER Update
    If specified, updates all version files to the specified version.

.EXAMPLE
    .\Check-Version.ps1 -Version "1.0.0"
    Checks that all files have version 1.0.0

.EXAMPLE
    .\Check-Version.ps1 -Version "1.1.0" -Update
    Updates all files to version 1.1.0
#>

[CmdletBinding()]
param(
    [Parameter(Mandatory)]
    [string]$Version,

    [switch]$Update
)

$ErrorActionPreference = 'Stop'
$ProjectRoot = Split-Path $PSScriptRoot -Parent

# Version file locations
$VersionFiles = @{
    'Cargo.toml' = @{
        Path = Join-Path $ProjectRoot 'native/uast_core/Cargo.toml'
        Pattern = 'version = "{0}"'
        Regex = 'version = "([^"]+)"'
    }
    'wasm_loader.rs' = @{
        Path = Join-Path $ProjectRoot 'native/uast_core/src/wasm_loader.rs'
        Pattern = 'GRAMMAR_VERSION: &str = "v{0}"'
        Regex = 'GRAMMAR_VERSION.*"v([^"]+)"'
    }
    'UAST.Native.csproj' = @{
        Path = Join-Path $ProjectRoot 'src/UAST.Native/UAST.Native.csproj'
        Pattern = '<Version>{0}</Version>'
        Regex = '<Version>([^<]+)</Version>'
    }
    'pyproject.toml' = @{
        Path = Join-Path $ProjectRoot 'python/pyproject.toml'
        Pattern = 'version = "{0}"'
        Regex = 'version = "([^"]+)"'
    }
}

$results = @()
$allMatch = $true

foreach ($name in $VersionFiles.Keys) {
    $config = $VersionFiles[$name]
    $path = $config.Path

    if (-not (Test-Path $path)) {
        Write-Warning "File not found: $path"
        $results += [PSCustomObject]@{
            File = $name
            Path = $path
            CurrentVersion = 'NOT FOUND'
            ExpectedVersion = $Version
            Match = $false
        }
        $allMatch = $false
        continue
    }

    $content = Get-Content $path -Raw
    $match = [regex]::Match($content, $config.Regex)

    if ($match.Success) {
        $currentVersion = $match.Groups[1].Value
        $isMatch = $currentVersion -eq $Version

        $results += [PSCustomObject]@{
            File = $name
            Path = $path
            CurrentVersion = $currentVersion
            ExpectedVersion = $Version
            Match = $isMatch
        }

        if (-not $isMatch) {
            $allMatch = $false

            if ($Update) {
                Write-Host "Updating $name : $currentVersion -> $Version" -ForegroundColor Yellow
                $newContent = $content -replace $config.Regex, ($config.Pattern -f $Version)
                Set-Content $path -Value $newContent -NoNewline
            }
        }
    } else {
        Write-Warning "Could not find version pattern in $name"
        $results += [PSCustomObject]@{
            File = $name
            Path = $path
            CurrentVersion = 'PATTERN NOT FOUND'
            ExpectedVersion = $Version
            Match = $false
        }
        $allMatch = $false
    }
}

# Display results
Write-Host ""
Write-Host "Version Check Results" -ForegroundColor Cyan
Write-Host "=====================" -ForegroundColor Cyan
$results | Format-Table -AutoSize

if ($allMatch) {
    Write-Host "All versions match: $Version" -ForegroundColor Green
    exit 0
} elseif ($Update) {
    Write-Host "Versions updated to: $Version" -ForegroundColor Green
    Write-Host "Run 'git diff' to review changes." -ForegroundColor Yellow
    exit 0
} else {
    Write-Host "Version mismatch detected!" -ForegroundColor Red
    Write-Host "Run with -Update to synchronize versions." -ForegroundColor Yellow
    exit 1
}
