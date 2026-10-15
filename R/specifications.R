parse_columns <- function(columns) {
  bases <- sub("\\[.*$", "", columns)
  indices <- lapply(seq_along(columns), function(i) {
    column <- columns[[i]]
    if (!grepl("[\\[\\]]", column, perl = TRUE)) {
      return(integer())
    }
    pattern <- "^[^\\[\\]]+\\[[1-9][0-9]*( *, *[1-9][0-9]*)*\\]$"
    if (!grepl(pattern, column, perl = TRUE)) {
      rlang::abort(paste0("Malformed indexed parameter name: ", column, "."))
    }
    tokens <- strsplit(sub("\\]$", "", sub("^.*\\[", "", column)), ",")[[1]]
    values <- suppressWarnings(as.integer(trimws(tokens)))
    if (anyNA(values)) {
      rlang::abort(paste0(
        "Parameter indices exceed the integer range: ", column
      ))
    }
    values
  })
  out <- tibble::tibble(column = columns, base = bases, indices = indices)
  for (base in unique(bases)) {
    rows <- which(bases == base)
    if (length(unique(lengths(indices[rows]))) != 1L ||
      anyDuplicated(vapply(
        indices[rows], paste, character(1), collapse = ","
      ))) {
      rlang::abort(paste0(
        "Inconsistent or duplicate parameter dimensions: ", base
      ))
    }
  }
  out
}

parse_specs <- function(x, quosures) {
  check_dataset(x)
  if (!length(quosures)) {
    rlang::abort("Supply at least one parameter expression, such as theta[i].")
  }
  if (any(nzchar(names(quosures)))) {
    rlang::abort(
      "Named expressions are not supported; use parameter names directly."
    )
  }
  specs <- lapply(quosures, function(q) {
    expr <- rlang::quo_get_expr(q)
    if (rlang::is_symbol(expr)) {
      base <- rlang::as_string(expr)
      dims <- character()
    } else if (rlang::is_call(expr, "[")) {
      args <- as.list(expr)[-1L]
      if (length(args) < 2L || !rlang::is_symbol(args[[1L]]) ||
        !all(vapply(args[-1L], function(a) {
          rlang::is_symbol(a) && !rlang::is_missing(a)
        }, logical(1)))) {
        rlang::abort(paste0(
          "Use named dimensions, such as theta[i, j]; ",
          "omitted dimensions are unsupported."
        ))
      }
      base <- rlang::as_string(args[[1L]])
      dims <- vapply(args[-1L], rlang::as_string, character(1))
    } else {
      rlang::abort(paste0(
        "Unsupported expression; use a scalar name or theta[i, j]. ",
        "Regex and | widening are unsupported."
      ))
    }
    rows <- which(x$catalog$base == base & x$catalog$column %in% x$selected)
    if (!length(rows)) {
      rlang::abort(paste0("Unknown or unavailable parameter: ", base, "."))
    }
    catalog <- x$catalog[rows, ]
    if (any(lengths(catalog$indices) != length(dims))) {
      rlang::abort(paste0("Incompatible dimensions for parameter: ", base, "."))
    }
    if (anyDuplicated(dims)) {
      rlang::abort("Dimension names within a parameter must be distinct.")
    }
    list(base = base, dims = dims, catalog = catalog)
  })
  bases <- vapply(specs, `[[`, character(1), "base")
  dims <- unique(unlist(lapply(specs, `[[`, "dims"), use.names = FALSE))
  reserved <- c(
    draw_ids, ".variable", ".value", ".lower", ".upper",
    ".width", ".point", ".interval"
  )
  if (anyDuplicated(bases) || any(dims %in% c(bases, reserved)) ||
    any(bases %in% reserved)) {
    rlang::abort("Parameter and dimension names collide with output columns.")
  }
  specs
}

spec_columns <- function(specs) {
  unique(unlist(lapply(specs, function(s) s$catalog$column), use.names = FALSE))
}

index_table <- function(spec) {
  if (!length(spec$dims)) {
    return(tibble::tibble(.rows = nrow(spec$catalog)))
  }
  columns <- lapply(seq_along(spec$dims), function(i) {
    vapply(spec$catalog$indices, `[[`, integer(1), i)
  })
  names(columns) <- spec$dims
  tibble::as_tibble(columns)
}
