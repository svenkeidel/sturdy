RUNTIMES='runtimes.csv'
METRICS='metrics.csv'
rm -f $RUNTIMES $METRICS
stack bench --benchmark-arguments="--csv $RUNTIMES" sturdy-scheme
stack test --fast --test-arguments='-m "/TypedAnalysis/Benchmarks"' sturdy-scheme
paste -d, $METRICS $RUNTIMES > bench.csv
