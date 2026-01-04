# UAST-Grep Markdown Test File

## Introduction

This is a **Markdown test file** for *UAST-Grep* parser testing. It demonstrates various Markdown features including:

- Headers
- Paragraphs
- Formatting
- Lists
- Code blocks
- And much more!

---

## Text Formatting

### Emphasis

This text is **bold** and this is *italic*. You can also use __underscores__ for _emphasis_.

Combined ***bold and italic*** text is also possible, or ___like this___.

~~Strikethrough~~ text uses tildes.

### Inline Elements

Here's some `inline code` within a paragraph.

This is a [link to example.com](https://example.com "Example Site").

This is a [reference-style link][ref1].

[ref1]: https://example.com "Reference Link"

Here's an image: ![Alt text](image.png "Image Title")

### Special Characters

Special characters: &amp; &lt; &gt; &copy; &trade;

HTML entities: &nbsp; &mdash; &ndash; &hellip;

Unicode: 🚀 ✅ ❌ ⚠️ ℹ️

---

## Lists

### Unordered List

- First item
- Second item
  - Nested item 2a
  - Nested item 2b
    - Deep nested item
- Third item

Alternative markers:

* Asterisk item
* Another asterisk item

+ Plus item
+ Another plus item

### Ordered List

1. First step
2. Second step
   1. Sub-step 2.1
   2. Sub-step 2.2
3. Third step

### Task List

- [x] Completed task
- [ ] Incomplete task
- [x] Another completed task
- [ ] Another incomplete task

### Definition List

Term 1
: Definition for term 1

Term 2
: Definition for term 2
: Second definition for term 2

---

## Code

### Inline Code

Use the `console.log()` function for debugging.

### Fenced Code Block

```javascript
// JavaScript code block
function greet(name) {
    const message = `Hello, ${name}!`;
    console.log(message);
    return message;
}

class Calculator {
    add(a, b) {
        return a + b;
    }
}
```

```python
# Python code block
def greet(name: str) -> str:
    message = f"Hello, {name}!"
    print(message)
    return message

class Calculator:
    def add(self, a: int, b: int) -> int:
        return a + b
```

```powershell
# PowerShell code block
function Get-Greeting {
    param([string]$Name = "World")
    $message = "Hello, $Name!"
    Write-Host $message
    return $message
}
```

### Indented Code Block

    This is an indented code block.
    It uses 4 spaces or 1 tab.
    No syntax highlighting.

---

## Blockquotes

> This is a blockquote.
> It can span multiple lines.

> Nested blockquote
>
> > This is nested
> > Multiple levels deep

> Blockquote with other elements
>
> - List item in quote
> - Another item
>
> ```code
> Code in blockquote
> ```

---

## Tables

### Simple Table

| Header 1 | Header 2 | Header 3 |
|----------|----------|----------|
| Cell 1   | Cell 2   | Cell 3   |
| Cell 4   | Cell 5   | Cell 6   |

### Aligned Table

| Left | Center | Right |
|:-----|:------:|------:|
| L1   | C1     | R1    |
| L2   | C2     | R2    |
| L3   | C3     | R3    |

### Table with Formatting

| Feature | Status | Notes |
|---------|--------|-------|
| **Parsing** | ✅ | Fully supported |
| *Validation* | ⚠️ | Partial support |
| `Formatting` | ❌ | Not implemented |

---

## Horizontal Rules

Three or more...

---

Hyphens

***

Asterisks

___

Underscores

---

## Links

### Inline Links

[Inline link](https://example.com)

[Link with title](https://example.com "Title text")

[Relative link](../path/to/file.md)

### Reference Links

[Reference link][example]

[Numbered reference][1]

[Case-insensitive][Example]

[example]: https://example.com
[1]: https://example.com
[Example]: https://example.com

### Autolinks

<https://example.com>

<test@example.com>

---

## Images

### Inline Image

![Alt text](https://via.placeholder.com/150 "Image title")

### Reference Image

![Alt text][img1]

[img1]: https://via.placeholder.com/150 "Reference image"

### Image with Link

[![Alt text](image.png)](https://example.com)

---

## HTML in Markdown

<div class="custom-block">
  <p>This is <strong>HTML</strong> within Markdown.</p>
  <ul>
    <li>Item 1</li>
    <li>Item 2</li>
  </ul>
</div>

<details>
<summary>Click to expand</summary>

Hidden content here.

```code
Even code blocks work!
```

</details>

---

## Extended Syntax

### Footnotes

Here's a sentence with a footnote[^1].

[^1]: This is the footnote content.

### Abbreviations

*[HTML]: Hyper Text Markup Language
*[MD]: Markdown

The HTML specification is maintained by the W3C. MD is widely used.

### Highlight

==Highlighted text== using double equals.

### Subscript and Superscript

H~2~O is water.

X^2^ is X squared.

### Math (LaTeX)

Inline math: $E = mc^2$

Block math:
$$
\sum_{i=1}^{n} x_i = x_1 + x_2 + \cdots + x_n
$$

---

## Escaping

\*Not italic\*

\`Not code\`

\[Not a link\]

\# Not a header

---

## Conclusion

This document demonstrates the major features of Markdown syntax for testing the UAST-Grep parser. Each section exercises different aspects of the Markdown language.

*Last updated: 2024-01-15*
