# ICFP Artifact, paper #23 _Compositional Soundness Proofs of Abstract Interpreters_

## Getting Started

The artifact is provided as a docker container. To obtain the artifact
run the following docker command:
```
docker pull svenkeidel/sturdy-icfp-18-artifact
```

Afterwards, run the following command to start a shell to get access
to the container:
```
docker run --interactive --tty --workdir="/opt/build/" svenkeidel/sturdy-icfp-18-artifact /bin/bash
```

To build the code base, use the Haskell build tool
[Stack](https://www.haskellstack.org/) and run `stack build`
from the top-level directory `/opt/build/`. To run the tests, run
`stack test` from the top-level directory. After running the tests,
the test reports can be found in
`/opt/build/.stack-work/logs/sturdy-stratego-*-test.log` and
`/opt/build/.stack-work/logs/sturdy-pcf-*-test.log`.

## Step-by-Step Instructions

This artifact consists of the code shown in figures throughout the paper.

- Figure 1:
  * [Concrete Interpreter](figure1/Concrete.hs)
  * [Abstract Interpreter](figure1/Abstract.hs)

- Figure 2:
  * [Concrete Interpreter](figure2/Concrete.hs)
  * [Abstract Interpreter](figure2/Abstract.hs)

- Figure 3:
  * [Shared Interpreter](figure3/Shared.hs)

- Figure 4:
  * [Concrete Interpreter](figure3/Concrete.hs)
  * [Abstract Interpreter](figure3/Abstract.hs)

- Section 6: Evaluation

  The evaluation in the paper consists two case studies, a tree-shape
  analysis for Stratego and a _k_-CFA analysis for PCF. Each case study
  consists of the Haskell code of the concrete and abstract interpreter,
  as well as the corresponding soundness proof (submitted in
  supplementary material). Since paper proofs are not part of artifact
  evaluation, the artifact for this section only consists of the code for
  the analyses.
  
  The code of each case study consists of a shared interpreter that is
  parameterized by an arrow-based interface and the concrete and
  abstract instantiation of the shared interpreter.  To implement
  the concrete and abstract interpreter arrows, we used _arrow
  transformers_ (analogous to _monad transformers_). These arrow
  transformers define, for example, how fixpoints are computed,
  exceptions are propagated, or how a store is threaded.  Each pair of
  corresponding concrete and abstract arrow transformer implements an
  arrow-based interface that is used by the shared interpreter.

  + Section 6.1 - Stratego:
    * [Syntax definition](stratego/src/Syntax.hs)
    * [Shared interpreter](stratego/src/SharedSemantics.hs) (includes code of figure 6)
    * [Concrete interpreter](stratego/src/ConcreteSemantics.hs)
    * [Abstract interpreter](stratego/src/WildcardSemantics.hs)
    * Fixpoint computations:
        [interface](lib/src/Control/Arrow/Fix.hs),
        [concrete](lib/src/Control/Arrow/Transformer/Concrete/FixPoint.hs),
        [abstract](lib/src/Control/Arrow/Transformer/Abstract/GreatestFixPoint.hs)
    * Exceptional computations:
        [interface](lib/src/Control/Arrow/Except.hs),
        [concrete](lib/src/Control/Arrow/Transformer/Concrete/Except.hs),
        [abstract](lib/src/Control/Arrow/Transformer/Abstract/HandleExcept.hs)
    * Powerset computations:
        [abstract](lib/src/Control/Arrow/Transformer/Abstract/Powerset.hs)
    * Tests:
        [concrete](stratego/test/ConcreteSemanticsSpec.hs),
        [abstract](stratego/test/WildcardSemanticsSpec.hs)
  
  + Section 6.2 - PCF:
    * [Syntax definition](pcf/src/Syntax.hs)
    * [Shared interpreter](pcf/src/SharedSemantics.hs) (includes code of figure 8)
    * [Concrete interpreter](pcf/src/ConcreteSemantics.hs) (includes code of figure 7)
    * [Abstract interpreter](pcf/src/IntervalAnalysis.hs) (includes code of figure 7)
    * [Intervals](lib/src/Data/Abstract/Interval.hs)
    * Fixpoint computations:
        [interface](lib/src/Control/Arrow/Fix.hs),
        [concrete](lib/src/Control/Arrow/Transformer/Concrete/FixPoint.hs),
        [abstract](lib/src/Control/Arrow/Transformer/Abstract/LeastFixPoint.hs)
    * Environments for _k_-CFA:
        [interface](lib/src/Control/Arrow/Environment.hs),
        [concrete](lib/src/Control/Arrow/Transformer/Concrete/Environment.hs),
        [abstract](lib/src/Control/Arrow/Transformer/Abstract/BoundedEnvironment.hs)
    * Recording of call-string for _k_-CFA:
        [abstract](lib/src/Control/Arrow/Transformer/Abstract/Contour.hs)
    * Exceptional computations:
        [interface](lib/src/Control/Arrow/Except.hs),
        [concrete](lib/src/Control/Arrow/Transformer/Concrete/Except.hs),
        [abstract](lib/src/Control/Arrow/Transformer/Abstract/PropagateExcept.hs)
    * Tests:
        [concrete](pcf/test/ConcreteSpec.hs),
        [abstract](pcf/test/IntervalAnalysisSpec.hs)
        [shared](pcf/test/SharedSpecs.hs)
