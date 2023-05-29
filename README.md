# Kilt

Kilt is a statically typed programming language

```go
name := getln("Input name: ");
age := getln("Input age: ");

println("Hey, " + name + " you are " + age + " years old!!!");
```

To run the compiler you need to use the command line to run it, and pass in args.

## Command Line Arguments
| Command | Description |
| --- | --- |
| `--Test` | Uses the built-in test file. |
| `--PrettyPrinter` | Prints out the AST in a pretty format. |
| `--ASTPrinter` | Prints out the direct AST tree. |
| `--PrintBytecode` | Prints the compiled bytecode. |

## Basic Syntax

### Variable decleration
```go
integer: int = 25;
floating_point: float = 25.1252;
boolean: bool = true;
name: string = "This is a string";
```

### Variables types can also be inferred
```go
name := 25; // this is an integer
```

### Control-flow
If statements
```go
alive := false;
if alive {
  println("You are alive");
}
else {
  println("You are not alive");
}
```
For statement
```go
for i := 0, i < 10, i += 1 {
  println(to_string(i));
}
```
Loop statement
```go
i := 0;
loop {
  if i > 10 {
    break;
  }
  
  println(to_string(i));
  
  if i < 0 {
    continue;
  }
}
```
### Functions
Functions are declared using this syntax
```go
func double(number: int): int {
  return number * 2;
}

// functions are called using this syntax
double(25);
```

## Building
### Build using CMake

```
cd Kilt
mkdir build
cmake --build build/
```
