stack clean; stack bench --ghc-options='' --benchmark-arguments='--csv=noinline-noprofunctor.csv' sturdy-lib
stack clean; stack bench --ghc-options='-DPROFUNCTOR' --benchmark-arguments='--csv=noinline-profunctor.csv' sturdy-lib
stack clean; stack bench --ghc-options='-D_INLINE' --benchmark-arguments='--csv=inline-noprofunctor.csv' sturdy-lib
stack clean; stack bench --ghc-options='-D_INLINE -DPROFUNCTOR' --benchmark-arguments='--csv=inline-profunctor.csv' sturdy-lib
