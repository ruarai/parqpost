# Selective-access benchmark

Run `Rscript benchmarks/selective-access.R` from the source root. The script
creates a temporary Parquet file, checks spread-result equality, warms each
operation, and reports median elapsed time over three runs. Data generation,
writing, and opening the reusable dataset are outside the timings.

Workload: 4,000 draws, four chains, 1,021 parameter columns, and 21 selected
columns (`mu` plus 20 `theta` elements). Parquet size: 37.36 MiB. The spreading
result has 80,000 rows.

Measured on 16 October 2026 with R 4.2.0 on aarch64 macOS, arrow 23.0.1.1,
dplyr 1.2.1, posterior 1.6.1, and tidybayes 3.0.7:

| Task | Median seconds | R allocations (MiB) | Result size (MiB) |
| --- | ---: | ---: | ---: |
| Selective read | 0.059 | 2.555 | 0.690 |
| Full read | 0.031 | 0.031 | 31.330 |
| Selective spread | 0.055 | 11.090 | 2.751 |
| Projected read + tidybayes | 0.067 | 14.671 | 2.751 |
| Full read + tidybayes | 0.058 | 14.353 | 2.751 |

R allocations are measured in a separate run with `Rprofmem()`. They exclude
Arrow's native allocations and are **not total or peak process memory**. In
particular, full reads can return Arrow-backed vectors with little R allocation
while retaining substantial native buffers. Result sizes are `object.size()`
estimates, not peak working sets.

These are warm-cache, local-file measurements, not promises of a speedup. Plain
full reads were faster here: for this small file, their low setup overhead
outweighed extra I/O. Selective spreading used fewer R allocations than either
tidybayes baseline, with similar elapsed time to full-read tidybayes. Its wide
input also excludes the 1,000 unrelated parameter columns. Arrow's initial
expression map is constructed once when opening the dataset and reused across
tasks; opening time is excluded for every task.

Disk bandwidth, cache state, selected-column fraction, compression, number of
draws, schema width, and Arrow version can change these results. The tests verify
projection at the scan boundary separately from timing.
