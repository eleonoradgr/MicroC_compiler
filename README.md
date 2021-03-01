# MicroC compiler
Compiler for MicroC, a small subset of C language. Course page : https://github.com/lillo/compiler-course-unipi/

## Requirement to build the code
The code requires:
* OCaml >= 4.10.1
* Menhir >= 20200624
* ppx_deriving >= 4.5 
* llvm >= 10.0.0

You can install the required dependencies via `opam`
```sh
$ opam install menhir ppx_deriving llvm
```
To use the compile you also need to install [Clang](https://clang.llvm.org/).

## Building the code and using the compiler
Typing `make` will generate a `microcc.native` executable:
```
$ make
```

To clean-up the folder, run:
```
$ make clean
```

To Parse and print the AST of a source file input.mc, run:
```
$ ./microcc -p input.mc
```
To perform semantic checks and print the TAST of a source file input.mc, run:
```
$ ./microcc -s input.mc
```

To print the generated code of a source file input.mc, run:
```
$ ./microcc -d input.mc
```

To place the output into file output.txt, run:
```
$ ./microcc -d input.mc -o output.txt
```

To optimize the generated code, rum:
```
$ ./microcc -O input.mc
```

To compile, run:
```
$ ./microcc input.mc
```
Once you have obtained your bitcode file a.bc, you have to link it with the object code of the runtime support.
```
$ make rtsupport
$ llvm-link a.bc rt-support.bc -o test.bc
$ llc -filetype=obj test.bc
$ clang test.o
```

Otherwise, to obtain directly an execuable, run:
```
$ make rtsupport
$ make compile file=input.mc
```
## Testing of implementation
The directory `testsemant` contains some tests used to test semantic analysis.
```
$ make testsemant
```
The directory `testcodegen` contains some tests used to test semantic analysis.
Each test case consists of a small MicroC program (the file with extension `*.mc`) together 
with the result that the program is supposed to produce (the file with extension `*.out`). The following command execute all of them and check differences with expected output.
```
$ make testcodegen
```

## Directory structure #

Here is a description of content of the repository

    src/                               <-- The source code lives here
    Makefile                           <-- Driver for `make` (uses OCB)
    _tags                              <-- OCamlBuild configuration
    test/                              <-- Some programs used to test compiler
    testsemnat/                        <-- Some programs to test semantic anlaysis
    testcodegen/                       <-- Some programs to test code generation

## The source code

The `src/` directory provides:

    ast.ml                       <-- Definition of the abstract syntax tree of MicroC
    tast.ml                      <-- Definition of the typed abstract syntax tree of MicroC 
    microcc.ml                   <-- The file from which build the executable 
    parser_engine.ml             <-- Module to interact with the parser
    codegen.ml                   <-- Module that implements the code generation
    util.ml                      <-- Utility module  
    opt_pass.ml                  <-- Module that implements some simple optimizations
    rt-support.c                 <-- The runtime support to be linked to bitcode files produced by your compiler
    parser.mly                   <-- Menhir specification of the grammar 
    scanner.mll                  <-- ocamllex specification of the scanner
    semant.ml                    <-- Module that implements the semantic checker
    symbol_table.mli             <-- Interface of a polymorphic symbol table data type
    symbol_table.ml              <-- Implementation of a polymorphic symbol table data type 