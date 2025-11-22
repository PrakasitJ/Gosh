# Gosh (Go#)

> An experimental, OOP-inspired programming language built on top of Go

Gosh combines Go's legendary performance with intuitive, high-level syntax. Write expressive code without sacrificing speed.

## ✨ Why Gosh?

- **Intuitive Syntax** – OOP-inspired expressions that feel natural
- **Zero Overhead** – Transpiles directly to Go for native performance
- **Instant Execution** – Write and run in one command
- **Go Interop** – Drop into native Go code anytime with `::`
- **Simple Imports** – Clean, straightforward package management

## 🚀 Quick Start

```bash
gosh hello.gosh
```

```gosh
import fmt;

string greeting = "Hello, Gosh!";
:: fmt.Println(greeting)
```

## 📖 Language Guide

### Imports

All imports must be declared at the top of your file:

```gosh
import fmt; math; strings;
```

### Variables

Declare variables with explicit types:

```gosh
int count = 42;
bool active = true;
string name = "Gosh";
float pi = 3.14159;
```

### Expressions

Full support for arithmetic with proper operator precedence:

```gosh
int result = (10 + 20) * 3;
int complex = ((a + b) * c) / (d - e);
```

### Native Go Code

Use `::` to execute raw Go code inline:

```gosh
int x = 10;
int y = 20;

:: fmt.Printf("Sum: %d\n", x + y)
:: if x > 5 {
::     fmt.Println("x is greater than 5")
:: }
```

## 🔧 CLI Usage

```bash
# Run your code
gosh script.gosh

# Debug mode (shows generated Go code)
gosh script.gosh --debug=true
```

## 🏗️ How It Works

```
.gosh file → Lexer → Tokens → Parser → AST → Transpiler → Go code → Execution
```

1. **Lexer** – Breaks source into tokens character by character
2. **Parser** – Builds an Abstract Syntax Tree from tokens
3. **Transpiler** – Converts AST into valid Go code
4. **Execution** – Compiles and runs the generated Go program

## 📝 Example

```gosh
import fmt; math;

int radius = 5;
float area = math.Pi * float(radius * radius);

:: fmt.Printf("Circle area: %.2f\n", area)
```

Generated Go code:
```go
package main

import (
    "fmt"
    "math"
)

func main() {
    radius := 5
    area := math.Pi * float64(radius * radius)
    fmt.Printf("Circle area: %.2f\n", area)
}
```

## 🎯 Current Status

Gosh is experimental and under active development. Contributions, feedback, and ideas are welcome!

## 📄 License

[Your License Here]

---

**Built with ❤️ and Go's speed**