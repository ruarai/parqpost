example_wide <- function() {
  tibble::as_tibble(posterior::as_draws_df(posterior::example_draws()))
}

on_disk <- function(data = example_wide()) {
  path <- tempfile(fileext = ".parquet")
  arrow::write_parquet(data, path)
  open_draws(path)
}

indexed_wide <- function() {
  x <- example_wide()
  for (i in 1:2) {
    for (j in 1:3) {
      x[[sprintf("beta[%s,%s]", i, j)]] <- x$mu * i + j
    }
    x[[sprintf("a[%s]", i)]] <- x$tau * i
  }
  for (j in 1:3) {
    x[[sprintf("b[%s]", j)]] <- x$mu + j
  }
  x[["cube[1,2,3]"]] <- x$mu
  x
}

canonical <- function(x) {
  x <- dplyr::ungroup(x)
  x <- x[sort(names(x))]
  x <- dplyr::arrange(x, dplyr::across(tidyselect::everything()))
  tibble::as_tibble(x)
}
