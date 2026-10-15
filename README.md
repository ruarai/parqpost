# parqpost

Read only the posterior parameters you need from Parquet, then spread draws,
compute diagnostics, or summarize credible intervals in R.

```r
# From the package source directory:
install.packages(".", repos = NULL, type = "source")
```

```r
library(parqpost)

# Save once, using posterior's wide draws_df format.
samples <- posterior::as_draws_df(posterior::example_draws())
write_draws(samples, "posterior.parquet")

# Opening inspects schema, without collecting draw values.
draws <- open_draws("posterior.parquet")
variables(draws)

selected <- draws |>
  select_draws(c("mu", "theta")) |>
  subset_draws(chain = 1:4, iteration = 51:100)

wide <- collect_draws(selected)  # posterior::draws_df
tidy <- spread_draws(selected, mu, theta[i])
diagnostics <- diagnose_draws(selected, variables = "theta")
intervals <- summarise_intervals(
  selected, theta[i], .width = c(0.5, 0.8, 0.95)
)
```

`intervals` has one row per parameter element and interval width. Its `i`,
`.value`, `.lower`, and `.upper` columns can be mapped directly in ggplot2.
Use ordinary dplyr on `wide` or `tidy` to calculate derived quantities, and
tidybayes for additional summaries.

## Storage and execution

Input has one numeric column per parameter element (`mu`, `theta[1]`,
`beta[1,2]`, etc.), and explicit positive integer `.chain`, `.iteration`, and
`.draw` identifiers. Use post-warmup draws. Parameter indices must be positive
integers; recovered model labels and factor types are outside the initial API.

Opening, printing, listing variables, selecting parameters, and subsetting
chains/iterations are lazy. Each analysis selects its required parameter columns
plus identifiers before Arrow collects values. Parameter columns used by no part
of the task are excluded from the scan. Selection of a base name includes all
its elements; an exact element name selects just that element.

Collection, spreading, diagnostics, and intervals return in-memory results.
Selected draws must fit in RAM; spreading can multiply the number of rows.
Exact intervals and diagnostics are computed in R. Original identifiers are
preserved in collected and spread draws, which are ordered by chain/iteration.
Diagnostics require matching retained iteration sequences across chains.

`open_draws()` accepts Parquet files, directories, vectors of files, and existing
Arrow Datasets. Use `arrow::open_dataset(path, format = "ipc")`, for example, to
open another supported format first. Only formats such as Parquet with physical
column projection can avoid reading unrelated column data from storage; text
formats may still need to scan their bytes. Multi-file input must have compatible
schemas and globally unique draw identifiers. Row-level checks occur when a task
collects, after any subsetting. Integer64 parameter values are converted to R
doubles; values outside their exact range may lose precision with a warning.

`write_draws()` writes a single Parquet file and refuses existing destinations
unless `overwrite = TRUE`. Supply a data frame or draws_df with explicit identifiers.

## Supported spreading

```r
spread_draws(draws, mu, theta[i])       # broadcast scalar mu
spread_draws(draws, a[i], beta[i, j])   # join shared i
spread_draws(draws, a[i], b[j])         # independent indices: Cartesian product
```

All array dimensions must be named. Shared index values use an inner join;
disjoint shared indices yield zero rows. Results are grouped by dimensions,
matching tidybayes for this syntax. Index maps are joined once before expanding
draws. Regex matching, omitted dimensions, `|` widening, named expressions, and
full tidybayes syntax compatibility are not supported. The public names
`variables`, `subset_draws`, and `spread_draws` overlap other packages; use
`parqpost::` when needed.

`summarise_intervals()` uses the same expressions but summarizes each parameter
independently, without broadcasting draws. `.variable` contains the base name;
dimension columns identify elements, with integer `NA` for absent dimensions.
Intervals use exact type-7 equal-tailed quantiles and median (default) or mean.
Missing values are errors rather than silently discarded. Diagnostics use
posterior's R-hat, bulk/tail ESS, and MCSE implementations, preserving its
undefined results and warnings.

## Development

```r
roxygen2::roxygenize()
testthat::test_local()
```

Run `Rscript tools/check-style.R` for basic automated tidyverse style checks.
Build and check with `R CMD build .` and `R CMD check parqpost_0.1.0.tar.gz`.
The benchmark in `benchmarks/selective-access.R` reports elapsed time and R
allocations for selective reads/spreading and full-read baselines. See
`benchmarks/README.md` for methodology and measured results.

The maintainer name and email in `DESCRIPTION` are placeholders; replace them
before publishing the package.
