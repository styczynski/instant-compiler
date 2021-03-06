# Instant Compiler
![badge](https://img.shields.io/docker/image-size/styczynski/insc-docker)
![GitHub last commit](https://img.shields.io/github/last-commit/styczynski/instant-compiler)
![workflow status](https://github.com/styczynski/instant-compiler/workflows/Build%20and%20release%20Docker%20image/badge.svg)

This project provides Instant language compiler.

## What is Instant lang?

A program in the Instant language consists of a sequence of statements separated by semicolons.
There are two kinds of statements:
 * expression - prints its value on stdout,
 * assignment of the form variable = expression - assigns value of the expression to he variable in the LHS; does not print anything.
Expressions are built from integer literals, variables and arithmetic operators. Evaluation order within an expression is not predefined (you can choose whatever order suits you best)

BNFC syntax for Instant lang is stated as follows:

```
Prog. Program ::= [Stmt] ;
SAss. Stmt ::= Ident "=" Exp;
SExp. Stmt ::= Exp ;
separator Stmt ";" ;

ExpAdd.            Exp1   ::= Exp2 "+"  Exp1 ;
ExpSub.            Exp2   ::= Exp2 "-"  Exp3 ;
ExpMul.            Exp3   ::= Exp3 "*"  Exp4 ;
ExpDiv.            Exp3   ::= Exp3 "/"  Exp4 ;
ExpLit.            Exp4   ::= Integer ;
ExpVar.            Exp4   ::= Ident ;
coercions Exp 4;
```

## What does this project do?

This project provides very simple compiler with JVM and LLVM backends for Instant language.

## Building

To build this project simply run:
```
	$ make all
```

Please note that build requires Stack to build. 
If you want to you can alternatively build using Docker. In that case no external dependencies except Docker are required.

## Building via Docker

To build the project Docker container please run:
```
    $ make build-docker
    # Created wrapper will invoke the already built Docker container underneath
    $ ./insc_jvm ./foo/bar.ins
```

## Project structure

 * bin - 3rd parties used by compiler (Jasmine distribution)
 * examples - input programs written in Instant with special format for comments
 * lib - runtime boilerplate code provided with the compiler
 * src/packages - source packages
 
 The `src/packages` directory contains sources of various packages:
 * cli-jvm - JVM backend command line interface
 * cli-llvm - LLVM backend command line interface
 * core - Core compiler logic module
 * parser - Parser/pretty printer for Instant language
 * test-preprocessor - Utility to convert input files to Haskell HSpec suites
 
 ## How it's build?
 
 After you do `make all` the following things happen:
 
 Firstly parser module is built. I used BNFC to generate praser code inside
 `src/packages/parser/parser` directory.
 
 Then the core module is build, then cli interfaces and test-preprocessor.
 After the test-preprocessor is built. It's used to generate `src/packages/cli-jvm/test/Generated/` and 
 `src/packages/cli-llvm/test/Generated/` directories containing generated HSpec tests for the compiler.
 
 After that we launch those suites.

## How to use it?

After you build the project you can run:
```
    $ ./insc_jvm ./foo/bar.ins
    $ java -cp "$(pwd)/foo/:$(pwd)/lib" bar
```

This will compile the input file to JVM bytecode and run it. The command will result in creation of:
 * runnable Jar file `./foo/bar.jar` (you can run in calling `java -jar ./foo/bar.jar`)
 * `./foo/bar.class` file with compiled main class
 * `./foo/bar.j` file with Jasmine assembler code

You can also use the LLVM backend:
```
    $ ./insc_llvm ./foo/bar.ins
    $ lli --extra-module $(pwd)/lib/runtime.ll $(pwd)/foo/bar.bc
```

This will compile the input file to LLVM bytecode and run it with `lli`. The command will result in creation of:
 * executable binary in `insc_build/llvm/` (you can run it calling `./insc_build/llvm/bar`)
 * LLVM human-readable bytecode inside `./foo/bar.ll`
 * Runnable LLVM code in `./foo/bar.bc` file
 
## Tests
 
As mentioned early the tests are simply inputs from `examples/` directory with special kinds of comments.
Those files are translated to HSpec suites and executed.
 
You can manually compile and run those files as ordinary files with Instant code.
 
```bash
    $ ./insc_jvm ./examples/good/Basic01.ins -r
    $ ./insc_llvm ./examples/good/Basic01.ins -r
```

`-r` flag runs the compiled file after compilation ends.

## Used tools and libraries

This project extensively use [Stack](https://docs.haskellstack.org/en/stable/README/) and [Shake](https://shakebuild.com/) with customized flows to build files.
To build the code LLVM and Jasmine assembler are used.
The [Shelly](http://hackage.haskell.org/package/shelly) is used to run command line tools.

Part of the codebase - that is test-preprocessor sources, configuration files and `Buildtools.hs` come from
my old Haskell projects. I am only author of them and have got exclusive ownership of the code.

The main parsing and code generation logic was made exclusively for the sole purpose of this project.