# Sturdy Tutorial

The aim of this tutorial is explain different styles interpreters for the while-language and how concrete and abstract interpreters can be described by the same generic implementation.
Context of this tutorial is the while-language, an imperative programming language with assignments, if-statements and while loops.
The abstract syntax of this language can be found in `Syntax.hs` in the `src/` folder.

## Styles

- Direct Style: All effects of the language are explicit. Failure and the store are propagated explicitly.
- Monadic Style: Monads abstract over the effects of a programming language. The propagation of failure and the store are implicit. Only if necessary, the current store can be retrieved from the monad, or an exception can be thrown.
- Arrow Style: Arrows also abstract over the effects of a programming language. However, in comparision to monads, arrows allow to describe effects on the input of the computation, not only the output.

## Semantics

- Concrete Semantics: All `DirectStyle.hs`, `MonadicStyle.hs`, and `ArrowStyle.hs` describe the same _concrete_ language semantics. This semantics describes how a program runs.
- Abstract Semantics: The file `IntervalSemantics.hs` describes an abstract semantics of the while-language that calculates the range of numbers variables can hold after execution.
- Shared Semantics: The concrete and abstract semantics of the while language bear a striking resemblence. This resemblence can be made explicit by sharing the implementation of both semantics.