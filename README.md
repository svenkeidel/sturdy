# Sturdy ![build-status](https://travis-ci.org/svenkeidel/sturdy.svg?branch=master)

<img align="left" alt="Sturdy Logo" src="/logo.svg" width="30%" height="15%">

Sturdy is a library to create sound static analyses in Haskell.
Static analyses are tools that produce information about computer programs without actually running the program.
Examples of static analyses are type checkers, bug finders (e.g. Java FindBugs), analyses for security (e.g. taint analyses), and analyses that are used for compiler optimizations.

<img align="right" alt="Sturdy Overview" src="/overview.png">

This project focuses on _sound_ static analyses.
A static analysis is sound if the results of the analysis reflect the actual runtime behavior of the program and users can rely on the results.
For example, if a static analysis used for compiler optimizations were unsound, the optimization might change the semantics of the program, which leads to unexpected behavior at runtime.
To this end, Sturdy follows the theory of _Compositional Soundness Proofs of Abstract Interpreters_ to simplify soundness proofs of static analyses.

In short, Sturdy factorizes the concrete interpreter and abstract interpreter (the static analysis) into a _generic interpreter_.
This generic interpreter is parameterized by interfaces that contain primitive operations that describe the semantics of the language, such as `try`, `catch` and `finally` for exceptions.
The concrete and abstract interpreter then instantiate the generic interpreter by implementing these interfaces.
This reorganization not only decouples different concerns in the implementation of the static analysis, but also in the its soundness proof.
More details can be found in the [paper](https://dl.acm.org/citation.cfm?id=3236767).

## Getting Started

To build, install the [Stack](https://www.haskellstack.org/) build tool and run `stack build` from the root directory of the project.

The sturdy project currently contains concrete and abstract and generic interpreters for the following languages:
* _PCF_, a higher-order functional language with numbers
* _While_, an imperative language with conditionals and while loops
* _Stratego_, a language for program transformations
* _Jimple_, a Java Bytecode suitable for static analysis
* _LambdaJS_, an intermediate representation for JavaScript

To run the tests of a particular language use `stack test sturdy-$(lang)`, e.g.,
```
stack test sturdy-stratego
```

## Publications

**Sound and Reusable Components for Abstract Interpretation**  
Sven Keidel and Sebastian Erdweg.  
_Object-Oriented Programming, Systems, Languages, and Applications (OOPSLA)_.
ACM, 2019
[[pdf]](https://svenkeidel.de/papers/analysis-components.pdf)


**Compositional Soundness Proofs of Abstract Interpreters**  
Sven Keidel, Casper Bach Poulsen and Sebastian Erdweg.  
_International Conference on Functional Programming (ICFP)_.
ACM, 2018
[[pdf]](https://dl.acm.org/citation.cfm?id=3236767)
[[Talk]](https://www.youtube.com/watch?v=zOqSlHAMGt4)

## Acknowledgments

I want to thank everyone that has contributed to this project (in alphabetical order):

Matthijs Bijman, Sebastian Erdweg, Jente Hidskes, Wouter Raateland
