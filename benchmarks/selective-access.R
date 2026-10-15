# Run from the repository root: Rscript benchmarks/selective-access.R
# Uses source files so installation of this development package is unnecessary.
for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(file)
}

run_benchmark <- function(n_iterations = 3L) {
  set.seed(20261016)
  n <- 4000L
  data <- tibble::tibble(
    .chain = rep(1:4, each = n / 4),
    .iteration = rep(seq_len(n / 4), 4),
    .draw = seq_len(n),
    mu = stats::rnorm(n)
  )
  for (i in 1:20) {
    data[[paste0("theta[", i, "]")]] <- stats::rnorm(n)
  }
  for (i in 1:1000) {
    data[[paste0("unused[", i, "]")]] <- stats::rnorm(n)
  }
  path <- tempfile(fileext = ".parquet")
  on.exit(unlink(path), add = TRUE)
  write_draws(data, path)
  x <- open_draws(path)
  selected <- c(draw_ids, "mu", paste0("theta[", 1:20, "]"))
  tasks <- list(
    selective_read = function() {
      collect_draws(select_draws(x, c("mu", "theta")))
    },
    full_read = function() arrow::read_parquet(path),
    selective_spread = function() spread_draws(x, mu, theta[i]),
    projected_tidybayes = function() {
      values <- dplyr::collect(dplyr::select(
        x$query, tidyselect::all_of(selected)
      ))
      tidybayes::spread_draws(values, mu, theta[i])
    },
    full_read_tidybayes = function() {
      tidybayes::spread_draws(arrow::read_parquet(path), mu, theta[i])
    }
  )
  # Warm all code paths and verify equivalent spread results before timing.
  warmed <- lapply(tasks, function(f) f())
  normalize <- function(x) {
    x <- dplyr::ungroup(x)
    dplyr::arrange(x[sort(names(x))], .draw, i)
  }
  stopifnot(isTRUE(all.equal(
    normalize(warmed$selective_spread), normalize(warmed$full_read_tidybayes)
  )))
  result_sizes <- vapply(warmed, function(x) {
    as.double(utils::object.size(x)) / 1024^2
  }, numeric(1))
  rm(warmed)
  results <- lapply(names(tasks), function(name) {
    f <- tasks[[name]]
    elapsed <- numeric(n_iterations)
    for (i in seq_len(n_iterations)) {
      gc()
      elapsed[[i]] <- system.time(invisible(f()))[["elapsed"]]
    }
    profile <- tempfile()
    utils::Rprofmem(profile)
    invisible(f())
    utils::Rprofmem(NULL)
    allocations <- readLines(profile)
    unlink(profile)
    bytes <- suppressWarnings(as.double(sub(" .*", "", allocations)))
    data.frame(
      task = name, median_seconds = stats::median(elapsed),
      r_allocated_mib = sum(bytes, na.rm = TRUE) / 1024^2,
      result_mib = result_sizes[[name]]
    )
  })
  cat(n, "draws;", ncol(data) - 3L, "parameter columns; 21 selected\n")
  cat("Parquet size:", round(file.info(path)$size / 1024^2, 2), "MiB\n")
  print(dplyr::bind_rows(results), row.names = FALSE)
  print(sessionInfo())
  invisible(results)
}

run_benchmark()
