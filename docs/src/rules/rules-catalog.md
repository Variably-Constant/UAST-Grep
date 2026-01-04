# Rules Catalog

Complete index of all **3,873 rules** across security, performance, and quality categories.

## Summary Statistics

| Category | Rule Count | Coverage |
|----------|------------|----------|
| **Security** | 1,587 | 179 CWEs, 31 languages |
| **Performance** | 1,334 | 31 languages |
| **Quality** | 952 | 31 languages |
| **Total** | **3,873** | |

## Rules by Language

### Language Coverage Matrix

| Language | Security | Performance | Quality | Total |
|----------|----------|-------------|---------|-------|
| **C#** | 115 | 122 | 143 | 380 |
| **Python** | 114 | 82 | 89 | 285 |
| **Rust** | 114 | 131 | 46 | 291 |
| **Java** | 119 | 75 | 45 | 239 |
| **JavaScript** | 100 | 82 | 89 | 271 |
| **TypeScript** | 41 | 41 | 49 | 131 |
| **C** | 104 | 7 | 0 | 111 |
| **Ruby** | 99 | 72 | 89 | 260 |
| **PHP** | 63 | 56 | 58 | 177 |
| **Kotlin** | 85 | 50 | 0 | 135 |
| **Scala** | 71 | 70 | 80 | 221 |
| **Go** | 61 | 68 | 36 | 165 |
| **Lua** | 87 | 62 | 0 | 149 |
| **Clojure** | 78 | 65 | 0 | 143 |
| **PowerShell** | 78 | 59 | 0 | 137 |
| **Swift** | 30 | 30 | 27 | 87 |
| **Bash/Shell** | 70 | 63 | 60 | 193 |
| **Dockerfile** | 12 | 21 | 18 | 51 |
| **YAML/Config** | 41 | 18 | 30 | 89 |
| **Terraform/HCL** | 20 | 18 | 17 | 55 |
| **C++** | 2 | 68 | 0 | 70 |
| **SQL** | 0 | 5 | 0 | 5 |
| **Universal (`*`)** | 24 | 16 | 31 | 71 |
| **Other** | 49 | 53 | 45 | 147 |

---

## Security Rules (1,587)

### By Vulnerability Category

#### Injection Vulnerabilities

| CWE | Description | Rule Count | Languages |
|-----|-------------|------------|-----------|
| CWE-78 | OS Command Injection | 85 | Python, Java, Go, Ruby, PHP, JavaScript, C#, PowerShell, Bash, Rust, Kotlin, Scala, Clojure, Lua, C, YAML |
| CWE-89 | SQL Injection | 58 | Python, Java, Go, Ruby, PHP, JavaScript, C#, Rust, Kotlin, Scala, Clojure, Lua, Bash |
| CWE-94 | Code Injection | 101 | Python, Java, JavaScript, C#, PowerShell, Ruby, PHP, Bash, Go, Rust, Scala, Clojure, Lua, TypeScript, YAML |
| CWE-79 | Cross-Site Scripting (XSS) | 41 | JavaScript, Java, Python, Go, Ruby, PHP, C#, Scala, Clojure |
| CWE-90 | LDAP Injection | 4 | Java, C#, Ruby, Python |
| CWE-91 | XML Injection | 3 | Java, Python, C# |
| CWE-117 | Log Injection | 8 | Java, Python, Go, C#, Bash, Lua, Scala |
| CWE-1336 | Template Injection (SSTI) | 5 | Python, Java, Ruby |

#### Cryptographic Issues

| CWE | Description | Rule Count | Languages |
|-----|-------------|------------|-----------|
| CWE-327 | Broken Cryptography | 53 | Java, Python, Go, C#, JavaScript, Ruby, Rust, Kotlin, Scala, Clojure, C, YAML |
| CWE-328 | Weak Hash (MD5/SHA1) | 13 | Python, Java, Go, C, Lua, Clojure, Scala |
| CWE-330 | Insufficient Randomness | 16 | Python, Java, Go, JavaScript, C#, Ruby, Rust, C, Lua, Clojure, Scala, Bash |
| CWE-338 | Cryptographic PRNG | 10 | Python, JavaScript, Go, Kotlin, Lua |
| CWE-321 | Hardcoded Crypto Key | 12 | Python, Java, JavaScript, C#, Ruby, Rust, C, Clojure, Scala |
| CWE-326 | Inadequate Encryption Strength | 20 | Java, Python, Go, C#, Clojure, Scala, HCL, YAML |
| CWE-329 | No Random IV | 7 | Java, Python, Go, Ruby, C, Clojure, Scala |

#### Authentication & Authorization

| CWE | Description | Rule Count | Languages |
|-----|-------------|------------|-----------|
| CWE-798 | Hardcoded Credentials | 54 | Python, Java, JavaScript, Go, C#, Ruby, PowerShell, Bash, Rust, Kotlin, C, Lua, Clojure, Scala, Dockerfile, HCL, YAML |
| CWE-521 | Weak Password Requirements | 3 | JavaScript, C# |
| CWE-352 | Cross-Site Request Forgery | 8 | Java, Python, JavaScript, Go, Ruby, C#, Clojure |
| CWE-347 | Improper JWT Verification | 13 | Java, Python, JavaScript, Go, C#, Ruby, Clojure, Scala |
| CWE-384 | Session Fixation | 3 | Java, Python, Ruby |
| CWE-614 | Insecure Cookie (no Secure flag) | 10 | Python, JavaScript, Go, Ruby, C#, Clojure, Scala |
| CWE-1004 | Insecure Cookie (no HttpOnly) | 5 | Python, Go, C#, Clojure |

#### Deserialization

| CWE | Description | Rule Count | Languages |
|-----|-------------|------------|-----------|
| CWE-502 | Insecure Deserialization | 73 | Python, Java, C#, Ruby, PHP, JavaScript, Go, Rust, Kotlin, Scala, Clojure, Lua, Elixir |

#### Path & File Operations

| CWE | Description | Rule Count | Languages |
|-----|-------------|------------|-----------|
| CWE-22 | Path Traversal | 39 | Python, Java, JavaScript, Go, Ruby, PHP, C#, Rust, C, Lua, Clojure, Scala |
| CWE-73 | External Control of File Name | 4 | Python, Java, Go, PHP |
| CWE-434 | Unrestricted File Upload | 3 | PHP, Java, Python |
| CWE-377 | Insecure Temporary File | 11 | Python, Java, Go, C, Bash, Lua |

#### Network Security

| CWE | Description | Rule Count | Languages |
|-----|-------------|------------|-----------|
| CWE-295 | Improper Certificate Validation | 32 | Python, Java, Go, C#, JavaScript, Ruby, Rust, Kotlin, C, Swift, Clojure, Scala, Bash, INI, YAML |
| CWE-319 | Cleartext Transmission | 10 | Python, Go, Ruby, C#, Rust, Clojure |
| CWE-918 | Server-Side Request Forgery (SSRF) | 16 | Python, Java, JavaScript, Go, Ruby, Rust, Clojure, Scala |
| CWE-601 | Open Redirect | 12 | Python, Java, JavaScript, Go, Ruby, C#, Lua, Clojure, Scala |

#### XML Processing

| CWE | Description | Rule Count | Languages |
|-----|-------------|------------|-----------|
| CWE-611 | XML External Entity (XXE) | 23 | Java, Python, Go, Ruby, C#, Clojure, Scala |
| CWE-776 | XML Entity Expansion (Billion Laughs) | 2 | Java, Python |

#### Information Disclosure

| CWE | Description | Rule Count | Languages |
|-----|-------------|------------|-----------|
| CWE-200 | Information Exposure | 20 | Java, Go, C#, Bash, Dockerfile, Lua, Scala, HCL, YAML |
| CWE-209 | Error Message Information Leak | 16 | Java, Python, JavaScript, Go, Ruby, C#, Rust, Lua, Clojure, Scala |
| CWE-532 | Sensitive Info in Log Files | 20 | Java, Python, JavaScript, C#, Ruby, Rust, Kotlin, Bash, Clojure, Scala, YAML |
| CWE-215 | Debug Information Exposure | 3 | Python |

#### Memory Safety (C/C++/Rust)

| CWE | Description | Rule Count | Languages |
|-----|-------------|------------|-----------|
| CWE-119 | Buffer Overflow | 11 | C, C++, Go, C#, JavaScript, Rust, Swift |
| CWE-120 | Classic Buffer Overflow | 19 | C, Go |
| CWE-134 | Format String Vulnerability | 11 | C, Bash, C# |
| CWE-416 | Use After Free | 1 | C |
| CWE-476 | NULL Pointer Dereference | 7 | C, Go, Rust, Lua, Swift |
| CWE-787 | Out-of-bounds Write | 7 | C++, Go, C#, Rust, Lua |

#### Race Conditions

| CWE | Description | Rule Count | Languages |
|-----|-------------|------------|-----------|
| CWE-367 | Time-of-Check Time-of-Use (TOCTOU) | 11 | C, Go, Java, Bash, C#, Rust, Lua |
| CWE-362 | Concurrent Execution Race | 3 | Java, Go, Ruby |

#### Denial of Service

| CWE | Description | Rule Count | Languages |
|-----|-------------|------------|-----------|
| CWE-400 | Resource Exhaustion | 22 | Java, JavaScript, Go, Ruby, C#, Clojure, Bash, Lua, Elixir |
| CWE-835 | Infinite Loop | 3 | C, Lua |
| CWE-1333 | ReDoS (Regex DoS) | 15 | Python, Java, JavaScript, Go, Ruby, C#, Lua, Clojure, Scala |

---

## Performance Rules (1,334)

### By Category

#### Memory & Allocation

| Pattern | Languages | Description |
|---------|-----------|-------------|
| String concatenation in loops | Python, Java, JavaScript, C#, Go, Ruby | Use StringBuilder/join instead |
| Inefficient collection resizing | Java, C#, Go, Rust | Pre-allocate capacity |
| Boxing/unboxing overhead | Java, C#, Kotlin | Use primitive types |
| Memory leaks | C, C++, Rust, Go | Resource cleanup patterns |

#### Algorithm Efficiency

| Pattern | Languages | Description |
|---------|-----------|-------------|
| O(n²) patterns | Python, Java, JavaScript, Ruby, C# | Nested loops, repeated lookups |
| Inefficient sorting | All | Use built-in sorting |
| Repeated calculations | All | Cache/memoize results |
| Linear search on large data | All | Use hash-based lookups |

#### I/O & Network

| Pattern | Languages | Description |
|---------|-----------|-------------|
| Unbuffered I/O | Python, Java, Go, C, Rust | Use buffered readers/writers |
| N+1 query patterns | Python, Java, Ruby, C# | Batch database queries |
| Sync I/O in async context | Python, JavaScript, C#, Rust | Use async I/O |

#### Language-Specific Optimizations

##### Python (82 rules)
- Generator expressions vs list comprehensions
- `__slots__` for memory efficiency
- `lru_cache` for memoization
- `array` module for numeric data
- Avoid global variable lookups in loops

##### JavaScript/TypeScript (123 rules)
- Avoid `with` statement
- Use `Set` for membership tests
- Avoid `arguments` object
- DOM batch updates
- Web Worker usage patterns

##### Java (75 rules)
- StringBuilder vs string concatenation
- Stream API misuse
- Autoboxing overhead
- Lock contention patterns
- JIT-unfriendly patterns

##### C# (122 rules)
- `Span<T>` and `Memory<T>` usage
- `ArrayPool<T>` for buffers
- Async/await best practices
- LINQ deferred execution
- Value types vs reference types

##### Rust (131 rules)
- Clone vs borrow
- Iterator chains efficiency
- Async runtime selection
- Vec capacity pre-allocation
- Arc vs Rc usage

##### Go (68 rules)
- Goroutine leaks
- Channel buffer sizing
- Slice capacity hints
- String building efficiency
- Defer in loops

---

## Quality Rules (952)

### By Category

#### Code Maintainability

| Pattern | Description | Languages |
|---------|-------------|-----------|
| Dead code | Unreachable code paths | All |
| Duplicate code | Copy-paste patterns | All |
| Magic numbers | Unexplained constants | All |
| Long functions | Excessive complexity | All |
| Deep nesting | Deeply nested logic | All |

#### Error Handling

| Pattern | Description | Languages |
|---------|-------------|-----------|
| Empty catch blocks | CWE-390 | All |
| Swallowed exceptions | Ignored errors | All |
| Generic exception handling | Too broad catch | Java, Python, C# |
| Missing error checks | Unchecked returns | Go, C, Rust |

#### Best Practices

| Pattern | Description | Languages |
|---------|-------------|-----------|
| Deprecated API usage | Obsolete methods | All |
| Naming conventions | Poor variable names | All |
| Code comments | Missing/excessive | All |
| TODO/FIXME patterns | Unresolved items | All |

#### Type Safety

| Pattern | Description | Languages |
|---------|-------------|-----------|
| Type coercion issues | Implicit conversions | JavaScript, TypeScript |
| Null safety | Potential null refs | Java, C#, Kotlin |
| Type assertions | Unsafe casts | TypeScript, Go |

---

## Universal Rules (`language: "*"`)

These **71 rules** work across all supported languages:

### Security (24 universal rules)
- `universal-dangerous-code-execution` - CWE-94
- `universal-command-execution` - CWE-78
- `universal-sql-string-concat` - CWE-89
- `universal-hardcoded-password` - CWE-798
- `universal-weak-hash` - CWE-328
- `universal-ssl-verify-disabled` - CWE-295
- `universal-insecure-deserialization` - CWE-502
- `universal-path-traversal` - CWE-22
- `universal-dangerous-html` - CWE-79
- ... and 15 more

### Performance (16 universal rules)
- `universal-inefficient-string-concat`
- `universal-redundant-computation`
- `universal-unbuffered-io`
- ... and 13 more

### Quality (31 universal rules)
- `universal-empty-catch`
- `universal-dead-code`
- `universal-magic-numbers`
- `universal-complex-conditional`
- ... and 27 more

---

## Rule Severity Distribution

### Security Rules

| Severity | Count | Percentage |
|----------|-------|------------|
| error | 719 | 45.3% |
| warning | 666 | 42.0% |
| info | 76 | 4.8% |
| critical | 10 | 0.6% |
| high | 41 | 2.6% |
| medium | 70 | 4.4% |
| low | 5 | 0.3% |

### Performance Rules

| Severity | Count | Percentage |
|----------|-------|------------|
| warning | 589 | 44.2% |
| info | 545 | 40.8% |
| error | 154 | 11.5% |
| other | 46 | 3.5% |

### Quality Rules

| Severity | Count | Percentage |
|----------|-------|------------|
| warning | 426 | 44.7% |
| info | 249 | 26.2% |
| error | 232 | 24.4% |
| other | 45 | 4.7% |

---

## Using the Rules

### Scan with All Rules

```bash
# All rules
uast-grep scan -r rules/ ./src

# Security only
uast-grep scan -r rules/universal-security.yaml ./src

# Performance only
uast-grep scan -r rules/universal-performance.yaml ./src

# Quality only
uast-grep scan -r rules/universal-quality.yaml ./src
```

### Filter by Language

```bash
# Python security rules only
uast-grep scan -r rules/universal-security.yaml --lang python ./src
```

### SARIF Output for CI/CD

```bash
uast-grep scan -r rules/ -f sarif ./src > results.sarif
```

---

## Adding Custom Rules

See [Rule Syntax](syntax.md) for creating your own rules that follow the same patterns.

```yaml
id: my-custom-rule
language: python
severity: error
message: "Description of the issue"
tags: [security, custom, CWE-XXX]

rule:
  pattern: "dangerous_function(§INPUT)"

note: |
  Explanation and fix suggestion.
```
