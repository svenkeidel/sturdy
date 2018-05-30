# ICFP Artifact, paper #23 _Compositional Soundness Proofs of Abstract Interpreters_

The evaluation in the paper consists two case studies, a tree-shape
analysis for Stratego and a _k_-CFA analysis for PCF. Each case study
consists of the Haskell code of the concrete and abstract interpreter,
as well as the corresponding soundness proof (submitted in
supplementary material). Since paper proofs are not part of artifact
evaluation, the artifact for this paper only consists of the code for
the analyses. This document serves as a starting point to compile and
test the case studies and to browse through the code base.

## Getting Started

The artifact is provided as a docker container with the code, the
compiler and libraries.  To obtain the artifact run the following
docker command:
```
docker pull svenkeidel/sturdy-icfp-18-artifact
```

Afterwards, run the following command to start a shell into the
container:
```
docker run --interactive --tty --workdir="/opt/build/" sturdy/artifact /bin/bash
```

To build the code base, use the build tool
[Haskell Stack](https://www.haskellstack.org/) and run `stack build`
from the top-level directory `/opt/build/`. To run the tests, run
`stack test` from the top-level directory. After running the tests,
the test reports can be found in
`/opt/build/.stack-work/logs/sturdy-stratego-*-test.log` and
`/opt/build/.stack-work/logs/sturdy-pcf-*-test.log`.

## Overview of the Code Base

The code of each case study consists of a shared interpreter that is
parameterized by an arrow-based interface and the concrete and
abstract instantiation of the shared interpreter.  To implement
the concrete and abstract interpreter arrows, we defined reusable
_arrow transformers_ (similar to _monad transformer_). These arrow
transformer define how fixpoints are computed, exceptions are
propagated, or how a store is threaded.  Each pair of corresponding
concrete and abstract arrow transformer implements an arrow-based
interface that is used by the shared interpreter.

We recommend to browse through code base in the order of the lists below.

- PCF:
  * [Syntax definition](pcf/src/Syntax.hs)
  * [Shared interpreter](pcf/src/SharedSemantics.hs)
  * [Concrete interpreter](pcf/src/ConcreteSemantics.hs)
  * [Abstract interpreter](pcf/src/IntervalAnalysis.hs)
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

- Stratego:
  * [Syntax definition](stratego/src/Syntax.hs)
  * [Shared interpreter](stratego/src/SharedSemantics.hs)
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
