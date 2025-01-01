This repository contains the solution for the **Custom Language Parser** project from CMPSC 461: Programming Language Concepts at Penn State. The project implements a parser for a custom programming language, featuring variable scoping, type checking, and grammar parsing.

## **Project Overview**

The parser supports the following features:
- **Lexical Analysis**: Tokenizes input code into meaningful tokens such as keywords, operators, and identifiers.
- **Parsing**: Converts token streams into Abstract Syntax Trees (AST) using recursive descent parsing techniques.
- **Scoping Rules**: Implements block scoping for variables, ensuring variables are only accessible within their declared scope and nested blocks.
- **Type Checking**: Enforces type compatibility (e.g., `int` and `float` operations) and raises descriptive error messages for type mismatches.
- **Error Handling**: Detects and handles common programming errors such as:
  - Variable redeclaration in the same scope.
  - Usage of undeclared variables.
  - Type mismatches in expressions or assignments.

## **Language Grammar**

The grammar supports:
- Variable declarations
- Control flow (`if-else`, `while`)
- Arithmetic and boolean expressions
- Function calls

  
