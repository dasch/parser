allBenchmarks := $(wildcard benchmarks/*.elm)
benchmarkTargets := $(allBenchmarks:.elm=.html)

benchmarks/%.html: benchmarks/%.elm
	elm make $< --optimize --output $@


benchmarks: $(benchmarkTargets)
