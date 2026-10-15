test_that("Parquet round trips preserve draws and prevent overwrites", {
  path <- tempfile(fileext = ".parquet")
  data <- example_wide()
  expect_identical(write_draws(data, path), path)
  x <- open_draws(path)
  expect_s3_class(x, "draws_dataset")
  expect_setequal(variables(x), setdiff(names(data), parqpost:::draw_ids))
  expect_s3_class(collect_draws(x), "draws_df")
  expect_equal(canonical(collect_draws(x)), canonical(data))
  expect_error(write_draws(data, path), "Destination exists")
  expect_identical(write_draws(data, path, overwrite = TRUE), path)
  expect_error(
    write_draws(data, dirname(path), overwrite = TRUE), "Destination exists"
  )
})

test_that("selection and subsetting preserve original identifiers", {
  data <- example_wide()
  data$.draw <- data$.draw + 1000L
  set.seed(42)
  x <- on_disk(data[sample(nrow(data)), ])
  out <- x |>
    select_draws(c("mu", "theta[2]")) |>
    subset_draws(chain = c(2L, 4L), iteration = 11:20) |>
    collect_draws()
  expected <- data[
    data$.chain %in% c(2L, 4L) & data$.iteration %in% 11:20,
    c("mu", "theta[2]", ".chain", ".iteration", ".draw")
  ]
  expect_equal(tibble::as_tibble(out), expected)
  expect_length(variables(select_draws(x, "theta")), 8L)
  expect_error(select_draws(x, "absent"), "Unknown")
  expect_error(select_draws(select_draws(x, "mu"), "tau"), "unavailable")
  expect_error(select_draws(x, character()), "nonempty")
  expect_error(subset_draws(x, iteration = 1.5), "positive integer")
  expect_error(collect_draws(subset_draws(x, iteration = 10000)), "No draws")
})

test_that("multi-file Parquet and existing IPC datasets work", {
  data <- example_wide()
  dir <- tempfile()
  dir.create(dir)
  paths <- file.path(dir, c("a.parquet", "b.parquet"))
  arrow::write_parquet(data[1:200, ], paths[[1L]])
  arrow::write_parquet(data[201:400, ], paths[[2L]])
  expect_equal(canonical(collect_draws(open_draws(dir))), canonical(data))
  expect_equal(
    canonical(collect_draws(open_draws(rev(paths)))), canonical(data)
  )
  ipc <- tempfile(fileext = ".arrow")
  arrow::write_feather(data, ipc)
  dataset <- arrow::open_dataset(ipc, format = "ipc")
  expect_equal(canonical(collect_draws(open_draws(dataset))), canonical(data))
})

test_that("schema and identifiers are validated at the appropriate boundary", {
  data <- example_wide()
  expect_error(on_disk(data[setdiff(names(data), ".draw")]), "require")
  bad <- data
  bad$label <- "a"
  expect_error(on_disk(bad), "numeric")
  bad <- data
  bad[["broken[1,a]"]] <- 0
  expect_error(on_disk(bad), "Malformed")
  bad <- data
  bad[["theta[1,2]"]] <- 0
  expect_error(on_disk(bad), "Inconsistent")
  bad <- data
  bad$.draw[[2L]] <- bad$.draw[[1L]]
  x <- on_disk(bad)
  expect_error(collect_draws(x), "must be unique")
  expect_error(write_draws(bad, tempfile()), "must be unique")
  bad <- data
  bad$.iteration[[2L]] <- bad$.iteration[[1L]]
  expect_error(collect_draws(on_disk(bad)), "pairs must be unique")
  for (value in c(NA_real_, Inf, 1.5, 0, .Machine$integer.max + 1)) {
    bad <- data
    bad$.chain[[1L]] <- value
    expect_error(collect_draws(on_disk(bad)), "positive integer")
  }
  only_ids <- arrow::InMemoryDataset$create(data[parqpost:::draw_ids])
  expect_error(open_draws(only_ids), "no parameters")
})

test_that("Arrow int64 values collect as posterior-compatible R numerics", {
  int64 <- function(x) arrow::Array$create(x, type = arrow::int64())
  values <- c(2^32, 2^32 + 2)
  table <- arrow::Table$create(
    mu = int64(values),
    .chain = int64(c(2, 2)),
    .iteration = int64(c(11, 12)),
    .draw = int64(c(101, 102))
  )
  x <- on_disk(table)
  wide <- collect_draws(x)
  expect_identical(wide$mu, values)
  expect_identical(wide$.chain, c(2L, 2L))
  expect_identical(wide$.iteration, c(11L, 12L))
  expect_identical(wide$.draw, c(101L, 102L))
  expect_identical(spread_draws(x, mu)$mu, values)
  expect_identical(summarise_intervals(x, mu)$.value, 2^32 + 1)
})

test_that("opening is lazy and every task projects its scan", {
  # Trace the real Arrow collector: no replacement of the query implementation.
  spy <- new.env(parent = emptyenv())
  spy$columns <- list()
  options(parqpost_test_spy = spy)
  on.exit(options(parqpost_test_spy = NULL), add = TRUE)
  ns <- asNamespace("arrow")
  suppressMessages(trace(
    "compute.arrow_dplyr_query", where = ns, print = FALSE,
    tracer = quote({
      spy <- getOption("parqpost_test_spy")
      spy$columns <- c(spy$columns, list(names(x)))
    })))
  on.exit(
    suppressMessages(untrace("compute.arrow_dplyr_query", where = ns)),
    add = TRUE
  )
  x <- on_disk()
  invisible(capture.output(print(x)))
  variables(x)
  expect_length(spy$columns, 0L)
  collect_draws(select_draws(x, "mu"))
  spread_draws(x, theta[i])
  diagnose_draws(x, "tau")
  summarise_intervals(x, mu)
  expect_equal(spy$columns, list(
    c(parqpost:::draw_ids, "mu"),
    c(parqpost:::draw_ids, paste0("theta[", 1:8, "]")),
    c(parqpost:::draw_ids, "tau"),
    c(parqpost:::draw_ids, "mu")
  ))
})
