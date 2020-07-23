#!/usr/bin/env bash

RUNTIMES='runtimes.csv'
METRICS='metrics.csv'

# Clean up first, so there is no accidental interaction between consecutive benchmark runs.
rm -f bench.csv
stack clean

# Run the benchmarks and combine the resulting csv files.
stack bench --benchmark-arguments="--csv $RUNTIMES" sturdy-scheme 
# stack test --fast --test-arguments='-m "/TypedAnalysis/Benchmarks"' sturdy-scheme

# Post process the benchmark data
body() { # Apply a command to the body of a file
    IFS= read -r header
    printf '%s\n' "$header"
    "$@"
}
cat $METRICS  | body sort -k1 -t, > $METRICS.sorted
cat $RUNTIMES | body sort -k1 -t, > $RUNTIMES.sorted
paste -d, $METRICS.sorted $RUNTIMES.sorted > bench.csv

# cleanup
rm -f $RUNTIMES $RUNTIMES.sorted $METRICS $METRICS.sorted
