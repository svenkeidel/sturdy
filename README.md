<span style="color: red; font-weight: bold">
The Haskell version of Sturdy is not maintained anymore. The Sturdy project has been reimplemented in Scala and is available at https://gitlab.rlp.net/plmz/sturdy.scala.
</span>

# Sturdy ![build-status](https://travis-ci.org/svenkeidel/sturdy.svg?branch=master)

<img align="left" alt="Sturdy Logo" src="/logo.svg" width="25%">

Sturdy is a library to create sound static analyses in Haskell.
Static analyses are tools that produce information about computer programs without actually running the program.
Examples of static analyses are type checkers, bug finders (e.g. Java FindBugs), analyses for security (e.g. taint analyses), and analyses that are used for compiler optimizations.

This project focuses on _sound_ static analyses.
A static analysis is sound if the results of the analysis reflect the actual runtime behavior of the program and users can rely on the results.
For example, if a static analysis used for compiler optimizations were unsound, the optimization might change the semantics of the program, which leads to unexpected behavior at runtime.
To this end, Sturdy follows the theory of _Compositional Soundness Proofs of Abstract Interpreters_ and _Sound and Reusable Components for Abstract Interpretation_ to simplify soundness proofs of static analyses.

<img align="right" alt="Sturdy Overview" src="/overview.png">

Sturdy factorizes the concrete interpreter and abstract interpreter (the static analysis) into a _generic interpreter_.
This generic interpreter is parameterized by interfaces that contain primitive operations that describe the semantics of the language, such as `try`, `catch` and `finally` for exceptions.
The concrete and abstract interpreter then instantiate the generic interpreter by implementing these interfaces.
This reorganization not only decouples different concerns in the implementation of the static analysis, but also simplifies its soundness proof.
More details can be found in our [ICFP paper](https://doi.org/10.1145/3236767).

Sturdy allows to construct static analyses modularly from reusable analysis components.
Each analysis component encapsulates a single analysis concern and can be proven sound independently from the analysis where it is used.
Furthermore, the theory of analysis components guarantees that a static analysis is sound, if all its analysis components are sound.
This means that analysis developers do not have to worry about soundness as long as they reuse sound analysis components.
More details can be found in our [OOPSLA paper](https://doi.org/10.1145/3360602).

## Getting Started

To build, install the [Stack](https://www.haskellstack.org/) build tool and run `stack build` from the root directory of the project.

The sturdy project currently contains concrete and abstract and generic interpreters for the following languages:
* _PCF_, a higher-order functional language
* _While_, an imperative language with conditionals and while loops
* _Scheme_, a functional language language in the LISP family.
* [_Stratego_](https://svenkeidel.de/papers/program-trans-analysis.pdf), a language for program transformations
* _LambdaJS_, an intermediate representation for JavaScript
* _Jimple_, a Java Bytecode suitable for static analysis (work in progress)

To run the tests of a particular language use `stack test sturdy-$(lang)`, e.g.,
```
stack test sturdy-pcf
```

## Publications

**Combinator-Based Fixpoint Algorithms for Big-Step Abstract Interpreters**  
Sven Keidel, Sebastian Erdweg, Tobias Hombücher  
International Conference on Functional Programming (ICFP). ACM, 2023.
[[pdf](https://svenkeidel.de/assets/papers/fixpoint_combinators.pdf)]

**Modular Abstract Definitional Interpreters for WebAssembly**  
Katharina Brandl, Sebastian Erdweg, Sven Keidel, Nils Hansen  
European Conference on Object-Oriented Programming (ECOOP). ACM, 2023.
[[pdf](https://svenkeidel.de/assets/papers/sturdy_wasm.pdf)]


**A Systematic Approach to Abstract Interpretation of Program Transformations**  
Sven Keidel and Sebastian Erdweg.  
Verification, Model Checking, and Abstract Interpretation (VMCAI). Springer, 2020.
[[pdf](https://svenkeidel.de/papers/program-trans-analysis.pdf)]

**Sound and Reusable Components for Abstract Interpretation**  
Sven Keidel and Sebastian Erdweg.  
Object-Oriented Programming, Systems, Languages, and Applications (OOPSLA).
ACM, 2019
[[pdf](https://doi.org/10.1145/3360602)]
[[Talk](https://youtu.be/uCM54R3ab-Q)]

**Compositional Soundness Proofs of Abstract Interpreters**  
Sven Keidel, Casper Bach Poulsen and Sebastian Erdweg.  
International Conference on Functional Programming (ICFP).
ACM, 2018
[[pdf](https://doi.org/10.1145/3236767)]
[[Talk](https://www.youtube.com/watch?v=zOqSlHAMGt4)]

## Acknowledgments

The Sturdy project is a joint effort of the following people (in alphabetical order):

Casper Bach Poulsen,
Jente Hidskes,
Matthijs Bijman,
Sarah Müller,
Sebastian Erdweg,
Sven Keidel,
Tobias Hombücher,
Wouter Raateland
