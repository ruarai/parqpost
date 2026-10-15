#' Open posterior draws without reading draw values
#'
#' @param path A Parquet file, directory, vector of files, or Arrow Dataset.
#' @return A lazy `draws_dataset` object.
#' @details Input uses the wide `posterior::draws_df` layout, including numeric
#'   parameter columns and explicit `.chain`, `.iteration`, and `.draw` columns.
#'   Indexed parameters use positive integer indices, for example `theta[1]`.
#'   Schema inspection does not read draw values. Other formats can be supplied
#'   as an existing Arrow Dataset. Column pruning depends on its format.
#' @export
open_draws <- function(path) {
  dataset <- if (inherits(path, "Dataset")) {
    path
  } else {
    arrow::open_dataset(path, format = "parquet", unify_schemas = TRUE)
  }
  schema <- dataset$schema
  nms <- schema$names
  if (anyDuplicated(nms) || !all(draw_ids %in% nms)) {
    rlang::abort(
      "Draws require unique column names and .chain, .iteration, .draw."
    )
  }
  types <- vapply(schema$fields, function(x) x$type$ToString(), character(1))
  numeric_type <- grepl("^(u?int(8|16|32|64)|double|float|halffloat)$", types)
  if (!all(numeric_type)) {
    rlang::abort(paste0(
      "Draw columns must be numeric: ",
      paste(nms[!numeric_type], collapse = ", "), "."
    ))
  }
  params <- setdiff(nms, draw_ids)
  if (!length(params)) {
    rlang::abort("The dataset contains no parameters.")
  }
  catalog <- parse_columns(params)
  # Construct the reusable expression map once. Converting a very wide Dataset
  # to a dplyr query on every task otherwise dominates small selective scans.
  query <- dplyr::select(dataset, tidyselect::everything())
  structure(
    list(query = query, catalog = catalog, selected = params),
    class = "draws_dataset"
  )
}

draw_ids <- c(".chain", ".iteration", ".draw")

check_dataset <- function(x) {
  if (!inherits(x, "draws_dataset")) {
    rlang::abort("`x` must be a draws_dataset created by open_draws().")
  }
}

#' List available parameter columns
#' @param x A `draws_dataset`.
#' @return A character vector of selected parameter column names.
#' @export
variables <- function(x) {
  check_dataset(x)
  x$selected
}

#' Select parameters lazily
#' @param x A `draws_dataset`.
#' @param variables Character vector of base parameter names or exact element
#'   names. A base name selects all its indexed elements. `NULL` selects all
#'   currently available parameters.
#' @return A lazy `draws_dataset`.
#' @export
select_draws <- function(x, variables = NULL) {
  check_dataset(x)
  selected <- resolve_variables(x, variables)
  x$query <- dplyr::select(x$query, tidyselect::all_of(c(draw_ids, selected)))
  x$selected <- selected
  x
}

resolve_variables <- function(x, variables) {
  if (is.null(variables)) {
    return(x$selected)
  }
  if (!is.character(variables) || !length(variables) || anyNA(variables)) {
    rlang::abort("`variables` must be a nonempty character vector.")
  }
  selected <- lapply(variables, function(name) {
    matches <- x$catalog$column[
      x$catalog$column %in% x$selected &
        (x$catalog$column == name | x$catalog$base == name)
    ]
    if (!length(matches)) {
      rlang::abort(paste0("Unknown or unavailable parameter: ", name, "."))
    }
    matches
  })
  unique(unlist(selected, use.names = FALSE))
}

#' Subset chains and iterations lazily
#' @param x A `draws_dataset`.
#' @param chain,iteration Positive integer identifiers to retain, or `NULL` to
#'   retain all. Iteration identifiers are relative to each chain.
#' @return A lazy `draws_dataset`.
#' @export
subset_draws <- function(x, chain = NULL, iteration = NULL) {
  check_dataset(x)
  for (name in c("chain", "iteration")) {
    values <- get(name)
    if (!is.null(values)) {
      validate_integer(values, name)
      column <- paste0(".", name)
      x$query <- dplyr::filter(x$query, !!rlang::sym(column) %in% !!values)
    }
  }
  x
}

#' Collect selected posterior draws
#' @param x A `draws_dataset`.
#' @return A `posterior::draws_df`, sorted by chain and iteration, with original
#'   identifiers preserved. Empty selections and invalid identifiers are errors.
#' @export
collect_draws <- function(x) {
  check_dataset(x)
  out <- collect_selected(x, x$selected)
  draws <- posterior::as_draws_df(out)
  # posterior's constructor renumbers identifiers; disk identities are stable.
  draws[draw_ids] <- out[draw_ids]
  draws
}

# The single collection boundary used by all tasks. Projection happens here,
# even when the caller has not previously selected any parameters.
collect_selected <- function(x, columns) {
  query <- dplyr::select(x$query, tidyselect::all_of(c(draw_ids, columns)))
  out <- dplyr::collect(query)
  for (column in columns) {
    if (inherits(out[[column]], "integer64")) {
      out[[column]] <- as.double(out[[column]])
    }
  }
  validate_draws(out)
}

validate_integer <- function(x, name) {
  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x)) ||
    any(x < 1 | x > .Machine$integer.max | x != trunc(x))) {
    rlang::abort(paste0(
      "`", name, "` must contain positive integer identifiers."
    ))
  }
}

validate_draws <- function(x) {
  if (!nrow(x)) {
    rlang::abort("No draws remain after subsetting.")
  }
  for (name in draw_ids) {
    # Arrow int64 columns may collect as bit64 vectors. Check before coercion.
    values <- if (inherits(x[[name]], "integer64")) {
      as.double(x[[name]])
    } else {
      x[[name]]
    }
    validate_integer(values, name)
    x[[name]] <- as.integer(values)
  }
  if (anyDuplicated(x$.draw)) {
    rlang::abort("`.draw` identifiers must be unique.")
  }
  if (anyDuplicated(x[c(".chain", ".iteration")])) {
    rlang::abort("Chain/iteration pairs must be unique.")
  }
  x[order(x$.chain, x$.iteration), , drop = FALSE]
}

#' Write posterior draws to Parquet
#' @param x A `posterior::draws_df` or data frame in the same wide format.
#' @param path Destination Parquet file.
#' @param overwrite Whether to replace an existing file. Defaults to `FALSE`.
#' @return `path`, invisibly.
#' @export
write_draws <- function(x, path, overwrite = FALSE) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    rlang::abort("`path` must be a single file path.")
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    rlang::abort("`overwrite` must be TRUE or FALSE.")
  }
  if (dir.exists(path) || (file.exists(path) && !overwrite)) {
    rlang::abort(
      "Destination exists; use `overwrite = TRUE` to replace a file."
    )
  }
  if (!is.data.frame(x) || anyDuplicated(names(x)) ||
    !all(draw_ids %in% names(x))) {
    rlang::abort(
      "`x` must be a wide draws data frame with explicit identifiers."
    )
  }
  # Drop draws_df dispatch before validating ordinary data-frame operations.
  x <- tibble::as_tibble(x)
  params <- setdiff(names(x), draw_ids)
  if (!length(params) || !all(vapply(x[params], is.numeric, logical(1)))) {
    rlang::abort("Draws must contain numeric parameter columns.")
  }
  parse_columns(params)
  x <- validate_draws(x)
  arrow::write_parquet(x, path)
  invisible(path)
}

#' @export
print.draws_dataset <- function(x, ...) {
  cat("<parqpost draws_dataset>\n")
  cat(
    length(x$selected), "selected parameter columns; draw values remain lazy\n"
  )
  cat(paste(utils::head(x$selected, 8L), collapse = ", "), "\n")
  invisible(x)
}
