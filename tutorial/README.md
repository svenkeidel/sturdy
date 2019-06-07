# Sturdy Tutorial

The aim of this tutorial is to explain different styles interpreters for the While language and how concrete and abstract interpreters can be described by the same generic implementation.
Context of this tutorial is the While language, an imperative programming language with assignments, if-statements and while loops.
The abstract syntax of this language can be found in `Syntax.hs` in the `src/` folder.

## Styles

- Direct Style: All effects of the language are explicit. Stores are passed explicitely and all errors are handled explictly.

- Monadic Style: Monads abstract over the effects of a programming language. The propagation of failure and the store are implicit. The current store is retrieved if necessary from the monad with the `get` operation and stored with the `put` operations. Exceptions can be thrown with the `throw` operation.

- Arrow Style: Arrows also abstract over the effects of a programming language. However, in comparision to monads, arrows allow to describe effects on the input of the computation, not only the output.

## Semantics

- Concrete Semantics: The file `ConcreteInterpreter.hs` describes the _concrete_ language semantics. This semantics describes how programs runs.
- Abstract Semantics: The file `AbstractInterpreter.hs` describes a static analysis of the While language that calculates the range of numbers variables can hold after execution.
- Generic Semantics: The concrete and abstract interpreter of the while language bear a striking resemblence. This resemblence can be made explicit by sharing the implementation of both interpreters. We factored the concrete and abstract interpreter of our language in a generic interpreter in `Final/GenericInterpreter.hs`.
