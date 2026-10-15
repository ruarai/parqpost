#' Spread indexed parameter draws into a tidy data frame
#'
#' @param x A `draws_dataset`.
#' @param ... Unquoted scalar or indexed parameter expressions, such as
#'   `mu, theta[i], beta[i, j]`. All dimensions must be named. Regex matching,
#'   omitted dimensions, and `|` widening are not supported.
#' @return A tibble grouped by dimension columns, with draw identifiers and one
#'   value column per parameter. Only requested parameter columns are collected.
#' @details Shared dimensions are joined on their index values; independent
#'   dimensions form Cartesian products. Scalars are broadcast across indices.
#'   Output size can therefore be much larger than the selected wide draws.
#' @export
spread_draws <- function(x, ...) {
  specs <- parse_specs(x, rlang::enquos(...))
  dims <- unique(unlist(lapply(specs, `[[`, "dims"), use.names = FALSE))
  keys <- utils::tail(
    make.unique(c(dims, rep(".parqpost_column", length(specs)))), length(specs)
  )
  # Join only small schema-derived maps, never the full draw tables.
  maps <- lapply(seq_along(specs), function(i) {
    map <- index_table(specs[[i]])
    map[[keys[[i]]]] <- seq_len(nrow(map))
    map
  })
  map <- maps[[1L]]
  if (length(maps) > 1L) {
    for (i in 2:length(maps)) {
      shared <- intersect(names(map), specs[[i]]$dims)
      if (length(shared)) {
        map <- dplyr::inner_join(
          map, maps[[i]], by = shared, relationship = "many-to-many"
        )
      } else {
        map <- dplyr::cross_join(map, maps[[i]])
      }
    }
  }
  data <- collect_selected(x, spec_columns(specs))
  n_indices <- nrow(map)
  row <- rep(seq_len(nrow(data)), each = n_indices)
  out <- data[row, draw_ids, drop = FALSE]
  for (i in seq_along(specs)) {
    spec <- specs[[i]]
    for (dim in spec$dims) {
      if (!dim %in% names(out)) {
        out[[dim]] <- rep(map[[dim]], times = nrow(data))
      }
    }
    column <- map[[keys[[i]]]]
    values <- as.matrix(data[spec$catalog$column])
    # Matrix linear indices address draw rows and selected parameter columns.
    offsets <- (as.double(rep(column, times = nrow(data))) - 1) * nrow(data)
    out[[spec$base]] <- values[row + offsets]
  }
  if (length(dims)) {
    out <- dplyr::group_by(out, !!!rlang::syms(dims))
  }
  out
}
