<#
.SYNOPSIS
    PowerShell Test File for UAST-Grep
.DESCRIPTION
    Tests: functions, classes, variables, control flow, error handling
.NOTES
    This is a multi-line comment block
#>

# Single line comment
#Requires -Version 5.1

# Constants and variables
Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

[int]$MAX_ITEMS = 100
$script:GlobalCounter = 0
$Name = "UAST-Grep"
$Interpolated = "Testing $Name parser"
$HereString = @"
This is a here-string
with multiple lines
and $Name interpolation
"@

# Enum definition
enum Status {
    Ok = 200
    NotFound = 404
    ServerError = 500
}

# Class definition with inheritance
class BaseProcessor {
    [string]$Name
    hidden [int]$Count = 0

    BaseProcessor([string]$name) {
        $this.Name = $name
    }

    [void] Log([string]$message) {
        Write-Host "[$($this.Name)] $message"
    }

    [object[]] Process([object[]]$items) {
        throw [System.NotImplementedException]::new("Must override Process method")
    }
}

# Derived class
class DataProcessor : BaseProcessor {
    hidden [hashtable]$Cache = @{}

    DataProcessor() : base("Default") { }

    DataProcessor([string]$name) : base($name) { }

    [object[]] Process([object[]]$items) {
        $results = [System.Collections.Generic.List[object]]::new()

        # For loop
        for ($i = 0; $i -lt $items.Count; $i++) {
            $transformed = $this.Transform($items[$i])
            $results.Add($transformed)
        }

        # Foreach loop
        foreach ($item in $items) {
            $this.Cache["$item"] = $item
        }

        # While loop
        $counter = 0
        while ($counter -lt 10) {
            $counter++
        }

        # Do-while loop
        do {
            $counter++
        } while ($counter -lt 20)

        # Do-until loop
        do {
            $counter++
        } until ($counter -ge 30)

        $this.Count = $results.Count
        return $results.ToArray()
    }

    hidden [object] Transform([object]$item) {
        # Switch statement
        switch ($item) {
            { $_ -is [array] } { return $_.ForEach({ $_ * 2 }) }
            { $_ -is [string] } { return $_.ToUpper() }
            { $_ -is [int] } { return $_ * 2 }
            default { return $item }
        }
    }

    [void] RiskyOperation() {
        # Try-catch-finally error handling
        try {
            $content = Get-Content -Path "test.txt" -ErrorAction Stop
            $this.Log("Read $($content.Count) lines")
        }
        catch [System.IO.FileNotFoundException] {
            $this.Log("File not found: $_")
        }
        catch [System.UnauthorizedAccessException] {
            $this.Log("Access denied: $_")
        }
        catch {
            $this.Log("Error: $($_.Exception.Message)")
            throw
        }
        finally {
            $this.Log("Operation complete")
        }
    }
}

# Function with CmdletBinding
function Invoke-Processing {
    [CmdletBinding()]
    [OutputType([object[]])]
    param(
        [Parameter(Mandatory, ValueFromPipeline)]
        [object[]]$InputObject,

        [Parameter()]
        [string]$Name = "Default",

        [Parameter()]
        [switch]$Verbose
    )

    begin {
        $processor = [DataProcessor]::new($Name)
        $allItems = [System.Collections.Generic.List[object]]::new()
    }

    process {
        foreach ($item in $InputObject) {
            $allItems.Add($item)
        }
    }

    end {
        $results = $processor.Process($allItems.ToArray())
        return $results
    }
}

# Function with validation attributes
function Get-StatusMessage {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [ValidateSet(200, 404, 500)]
        [int]$Code
    )

    # If-elseif-else
    if ($Code -eq 200) {
        return "OK"
    }
    elseif ($Code -eq 404) {
        return "Not Found"
    }
    elseif ($Code -eq 500) {
        return "Server Error"
    }
    else {
        return "Unknown"
    }
}

# Function with splatting
function New-Processor {
    param(
        [string]$Name = "Default",
        [hashtable]$Options = @{}
    )

    $params = @{
        Name = $Name
    }

    # Merge options
    foreach ($key in $Options.Keys) {
        $params[$key] = $Options[$key]
    }

    return [DataProcessor]::new($params.Name)
}

# Pipeline function
function ConvertTo-DoubledValue {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory, ValueFromPipeline)]
        [int]$Value
    )

    process {
        $Value * 2
    }
}

# Filter
filter Where-GreaterThan {
    param([int]$Threshold = 0)
    if ($_ -gt $Threshold) { $_ }
}

# Workflow-like function with parallel
function Invoke-ParallelProcessing {
    param(
        [object[]]$Items,
        [int]$ThrottleLimit = 5
    )

    $Items | ForEach-Object -Parallel {
        # Simulate processing
        Start-Sleep -Milliseconds 100
        $_ * 2
    } -ThrottleLimit $ThrottleLimit
}

# Script block
$TransformBlock = {
    param([object]$Item)
    if ($Item -is [string]) { $Item.ToUpper() }
    else { $Item }
}

# Main execution
$processor = [DataProcessor]::new("Main")
$data = @(1, 2, 3, "hello", @(4, 5))
$result = $processor.Process($data)
$processor.Log("Processing complete: $($result -join ', ')")

# Pipeline usage
$doubled = 1..5 | ConvertTo-DoubledValue
Write-Host "Doubled: $($doubled -join ', ')"

# Invoke script block
$transformed = & $TransformBlock -Item "test"
Write-Host "Transformed: $transformed"

# Ternary operator (PS 7+)
$isAdult = $true
$status = $isAdult ? "Adult" : "Minor"

# Null-coalescing (PS 7+)
$value = $null
$result = $value ?? "default"

# Array slicing
$subset = $data[0..2]

# Hash table operations
$config = @{
    Name = "Config"
    MaxItems = 100
    Enabled = $true
}
$config.Add("NewKey", "NewValue")
