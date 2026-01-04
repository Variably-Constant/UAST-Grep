<#
.SYNOPSIS
    Creates a GitHub release with all artifacts.

.DESCRIPTION
    Tags the release, creates a GitHub release, and uploads all binary archives.
    Requires GitHub CLI (gh) to be installed and authenticated.

.PARAMETER Version
    The version tag (e.g., "1.0.0").

.PARAMETER Draft
    Create as draft release (not published).

.PARAMETER Prerelease
    Mark as pre-release.

.PARAMETER NotesFile
    Path to release notes file. Default: CHANGELOG.md

.EXAMPLE
    .\Publish-GitHub.ps1 -Version "1.0.0"
    Creates release v1.0.0

.EXAMPLE
    .\Publish-GitHub.ps1 -Version "2.0.0-beta.1" -Prerelease -Draft
    Creates a draft pre-release
#>

[CmdletBinding()]
param(
    [Parameter(Mandatory)]
    [string]$Version,

    [switch]$Draft,
    [switch]$Prerelease,
    [string]$NotesFile
)

$ErrorActionPreference = 'Stop'
$ProjectRoot = Split-Path $PSScriptRoot -Parent

# Normalize version tag
if (-not $Version.StartsWith('v')) {
    $versionTag = "v$Version"
} else {
    $versionTag = $Version
}

# Check for gh CLI
$gh = Get-Command gh -ErrorAction SilentlyContinue
if (-not $gh) {
    Write-Error "GitHub CLI (gh) not found. Install from: https://cli.github.com/"
    exit 1
}

# Check authentication
$authStatus = gh auth status 2>&1
if ($LASTEXITCODE -ne 0) {
    Write-Error "Not authenticated with GitHub. Run: gh auth login"
    exit 1
}

# Find release artifacts
$releaseDir = Join-Path $ProjectRoot "release/$versionTag"
if (-not (Test-Path $releaseDir)) {
    Write-Error "Release directory not found: $releaseDir"
    Write-Error "Run Create-ReleaseArchives.ps1 first."
    exit 1
}

$artifacts = Get-ChildItem $releaseDir -File
if ($artifacts.Count -eq 0) {
    Write-Error "No artifacts found in $releaseDir"
    exit 1
}

# Find release notes
if (-not $NotesFile) {
    $NotesFile = Join-Path $ProjectRoot 'CHANGELOG.md'
}
$hasNotes = Test-Path $NotesFile

Write-Host ""
Write-Host "GitHub Release Publisher" -ForegroundColor Cyan
Write-Host "========================" -ForegroundColor Cyan
Write-Host "Version:    $versionTag"
Write-Host "Artifacts:  $($artifacts.Count) files"
Write-Host "Draft:      $Draft"
Write-Host "Prerelease: $Prerelease"
Write-Host "Notes:      $(if ($hasNotes) { $NotesFile } else { '(none)' })"
Write-Host ""

# List artifacts
Write-Host "Artifacts to upload:" -ForegroundColor Yellow
foreach ($a in $artifacts) {
    $size = "{0:N2} MB" -f ($a.Length / 1MB)
    Write-Host "  - $($a.Name) ($size)"
}
Write-Host ""

# Confirm
$confirm = Read-Host "Create release $versionTag? (y/N)"
if ($confirm -ne 'y') {
    Write-Host "Cancelled." -ForegroundColor Yellow
    exit 0
}

# Create git tag if it doesn't exist
$existingTag = git tag -l $versionTag
if (-not $existingTag) {
    Write-Host "Creating git tag $versionTag..." -ForegroundColor Yellow
    git tag -a $versionTag -m "Release $versionTag"
    git push origin $versionTag
}

# Build gh release create command
$ghArgs = @('release', 'create', $versionTag)
$ghArgs += '--title'
$ghArgs += "UAST-Grep $versionTag"

if ($Draft) { $ghArgs += '--draft' }
if ($Prerelease) { $ghArgs += '--prerelease' }
if ($hasNotes) {
    $ghArgs += '--notes-file'
    $ghArgs += $NotesFile
}

# Add all artifacts
foreach ($a in $artifacts) {
    $ghArgs += $a.FullName
}

Write-Host "Creating GitHub release..." -ForegroundColor Yellow
& gh @ghArgs

if ($LASTEXITCODE -eq 0) {
    Write-Host ""
    Write-Host "Release created successfully!" -ForegroundColor Green
    Write-Host "View at: https://github.com/MarkusMcNugen/UAST-Grep/releases/tag/$versionTag" -ForegroundColor Cyan
} else {
    Write-Host ""
    Write-Host "Failed to create release!" -ForegroundColor Red
    exit 1
}
