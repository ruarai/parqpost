test_that("supported spread expressions match tidybayes", {
  skip_if_not_installed("tidybayes")
  data <- indexed_wide()
  x <- on_disk(data)
  compare <- function(actual, expected) {
    expect_equal(canonical(actual), canonical(expected))
    expect_equal(dplyr::group_vars(actual), dplyr::group_vars(expected))
  }
  compare(spread_draws(x, mu), tidybayes::spread_draws(data, mu))
  compare(
    spread_draws(x, mu, theta[i]), tidybayes::spread_draws(data, mu, theta[i])
  )
  compare(
    spread_draws(x, beta[i, j], a[i]),
    tidybayes::spread_draws(data, beta[i, j], a[i])
  )
  compare(
    spread_draws(x, a[i], b[j]), tidybayes::spread_draws(data, a[i], b[j])
  )
  compare(
    spread_draws(x, a[i], b[i]), tidybayes::spread_draws(data, a[i], b[i])
  )
  compare(
    spread_draws(x, cube[i, j, k]), tidybayes::spread_draws(data, cube[i, j, k])
  )
  # Internal map names cannot collide with user-chosen dimension names.
  expect_equal(nrow(spread_draws(x, a[.parqpost_column], b[j])), 2400L)
})

test_that("unsupported syntax and collisions fail before collection", {
  x <- on_disk(indexed_wide())
  expect_error(spread_draws(x), "at least one")
  expect_error(spread_draws(x, theta), "Incompatible dimensions")
  expect_error(spread_draws(x, beta[i]), "Incompatible dimensions")
  expect_error(spread_draws(x, beta[, j]), "omitted dimensions")
  expect_error(spread_draws(x, beta[i, i]), "distinct")
  expect_error(spread_draws(x, beta[i, j] | j), "Unsupported expression")
  expect_error(spread_draws(x, theta[i], regex = TRUE), "Named expressions")
  expect_error(spread_draws(x, theta[mu], mu), "collide")
  expect_error(spread_draws(x, theta[.draw]), "collide")
  expect_error(spread_draws(x, mu, mu), "collide")
})

test_that("disjoint shared indices produce an empty grouped result", {
  data <- indexed_wide()
  data <- data[setdiff(names(data), "b[1]")]
  x <- select_draws(on_disk(data), c("a[1]", "b"))
  out <- spread_draws(x, a[i], b[i])
  expect_equal(nrow(out), 0L)
  expect_equal(dplyr::group_vars(out), "i")
})
