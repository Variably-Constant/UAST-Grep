"""Pytest configuration for uast_grep tests."""

import pytest


@pytest.fixture
def python_source():
    """Sample Python source code."""
    return """
def hello():
    print("Hello, World!")

def add(a, b):
    return a + b

class Calculator:
    def __init__(self):
        self.value = 0

    def add(self, x):
        self.value += x
        return self

    def subtract(self, x):
        self.value -= x
        return self
"""


@pytest.fixture
def rust_source():
    """Sample Rust source code."""
    return """
fn hello() {
    println!("Hello, World!");
}

fn add(a: i32, b: i32) -> i32 {
    a + b
}

struct Calculator {
    value: i32,
}

impl Calculator {
    fn new() -> Self {
        Calculator { value: 0 }
    }

    fn add(&mut self, x: i32) -> &mut Self {
        self.value += x;
        self
    }
}
"""


@pytest.fixture
def powershell_source():
    """Sample PowerShell source code."""
    return """
function Get-Hello {
    Write-Host "Hello, World!"
}

function Add-Numbers {
    param(
        [int]$a,
        [int]$b
    )
    return $a + $b
}

$result = Add-Numbers -a 1 -b 2
Write-Output $result
"""
