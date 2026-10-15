#' Compute standard posterior diagnostics
#' @param x A `draws_dataset`.
#' @param variables Character vector of base parameter names or exact element
#'   names, or `NULL` for all selected parameters.
#' @return A tibble with `variable`, `rhat`, `ess_bulk`, `ess_tail`,
#'   `mcse_mean`, and `mcse_sd`. Numerical behavior follows posterior,
#'   including warnings.
#' @details Each chain must have the same retained iteration identifiers.
#'   Draws are sorted before constructing iteration-by-chain matrices.
#' @export
diagnose_draws <- function(x, variables = NULL) {
  check_dataset(x)
  columns <- resolve_variables(x, variables)
  data <- collect_selected(x, columns)
  iterations <- split(data$.iteration, data$.chain)
  if (!all(vapply(iterations, identical, logical(1), iterations[[1L]]))) {
    rlang::abort(
      "Diagnostics require matching iteration sequences across chains."
    )
  }
  draws <- posterior::as_draws_df(data)
  tibble::as_tibble(posterior::summarise_draws(
    draws,
    rhat = posterior::rhat,
    ess_bulk = posterior::ess_bulk,
    ess_tail = posterior::ess_tail,
    mcse_mean = posterior::mcse_mean,
    mcse_sd = posterior::mcse_sd
  ))
}

#' Summarize draws with exact equal-tailed credible intervals
#' @param x A `draws_dataset`.
#' @param ... Unquoted parameter expressions, as in [spread_draws()].
#' @param .width Numeric vector of interval probabilities strictly between
#'   0 and 1.
#' @param .point Point estimate: `"median"` (default) or `"mean"`.
#' @return An ungrouped tibble with `.variable` (base parameter name), named
#'   dimensions, `.value`, `.lower`, `.upper`, `.width`, `.point`, and
#'   `.interval = "qi"`. Dimensions absent from a parameter are `NA_integer_`.
#' @details Parameters are summarized independently without broadcasting draws.
#'   Intervals use exact type-7 quantiles. Missing parameter values cause an
#'   error; they are not silently removed.
#' @export
summarise_intervals <- function(x, ..., .width = 0.95, .point = "median") {
  specs <- parse_specs(x, rlang::enquos(...))
  .point <- match.arg(.point, c("median", "mean"))
  if (!is.numeric(.width) || !length(.width) || anyNA(.width) ||
    any(!is.finite(.width) | .width <= 0 | .width >= 1)) {
    rlang::abort(
      "`.width` must contain probabilities strictly between 0 and 1."
    )
  }
  data <- collect_selected(x, spec_columns(specs))
  results <- lapply(specs, function(spec) {
    indices <- index_table(spec)
    dplyr::bind_rows(lapply(seq_len(nrow(spec$catalog)), function(i) {
      values <- data[[spec$catalog$column[[i]]]]
      if (anyNA(values)) {
        rlang::abort(paste0(
          "Missing values in parameter: ", spec$catalog$column[[i]]
        ))
      }
      probs <- c((1 - .width) / 2, (1 + .width) / 2)
      bounds <- stats::quantile(values, probs = probs, names = FALSE, type = 7)
      out <- indices[rep(i, length(.width)), , drop = FALSE]
      out$.variable <- spec$base
      out$.value <- if (.point == "median") {
        stats::median(values)
      } else {
        mean(values)
      }
      out$.lower <- bounds[seq_along(.width)]
      out$.upper <- bounds[length(.width) + seq_along(.width)]
      out$.width <- .width
      out$.point <- .point
      out$.interval <- "qi"
      out
    }))
  })
  out <- dplyr::bind_rows(results)
  dims <- unique(unlist(lapply(specs, `[[`, "dims"), use.names = FALSE))
  out[c(
    ".variable", dims, ".value", ".lower", ".upper",
    ".width", ".point", ".interval"
  )]
}
