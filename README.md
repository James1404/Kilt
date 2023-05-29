# Kilt

Kilt is a statically typed programming language

```go
name := getln("Input name: ");
age := getln("Input age: ");

println("Hey, " + name + " you are " + age + " years old!!!");
```

To run the compiler you need to use the command line to run it, and pass in args.

Args:

| Command | Description |
| --- | --- |
| `--Test` | Uses the built-in test file. |
| `--PrettyPrinter` | Prints out the AST in a pretty format. |
| `--ASTPrinter` | Prints out the direct AST tree. |
| `--PrintBytecode` | Prints the compiled bytecode. |
