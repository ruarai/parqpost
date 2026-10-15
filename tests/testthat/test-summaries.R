test_that("diagnostics equal posterior after sorting shuffled draws", {
  data <- example_wide()
  set.seed(12)
  x <- on_disk(data[sample(nrow(data)), ])
  expected <- posterior::summarise_draws(
    posterior::as_draws_df(data[c("mu", "tau", parqpost:::draw_ids)]),
    rhat = posterior::rhat,
    ess_bulk = posterior::ess_bulk,
    ess_tail = posterior::ess_tail,
    mcse_mean = posterior::mcse_mean,
    mcse_sd = posterior::mcse_sd
  )
  expect_equal(diagnose_draws(x, c("mu", "tau")), tibble::as_tibble(expected))
  out <- diagnose_draws(subset_draws(x, chain = 2:4, iteration = 11:50), "mu")
  expect_equal(nrow(out), 1L)
  data <- data[-1L, ]
  expect_error(diagnose_draws(on_disk(data), "mu"), "matching iteration")
  # Equal counts are insufficient when retained iteration sequences differ.
  data <- example_wide()
  data$.iteration[[1L]] <- 101L
  expect_error(diagnose_draws(on_disk(data), "mu"), "matching iteration")
})

test_that("single-chain and constant diagnostics follow posterior", {
  data <- example_wide()
  data$mu <- 1
  x <- subset_draws(on_disk(data), chain = 1)
  expected <- posterior::summarise_draws(
    posterior::as_draws_df(data[
      data$.chain == 1, c("mu", parqpost:::draw_ids)
    ]),
    rhat = posterior::rhat, ess_bulk = posterior::ess_bulk,
    ess_tail = posterior::ess_tail, mcse_mean = posterior::mcse_mean,
    mcse_sd = posterior::mcse_sd
  )
  expect_equal(diagnose_draws(x, "mu"), tibble::as_tibble(expected))
})

test_that("intervals match exact quantiles without broadcasting", {
  data <- indexed_wide()
  x <- on_disk(data)
  widths <- c(0.5, 0.8, 0.95)
  out <- summarise_intervals(x, mu, a[i], b[j], .width = widths)
  expect_equal(nrow(out), 18L)
  expect_type(out$i, "integer")
  expect_type(out$j, "integer")
  expect_true(all(is.na(out$i[out$.variable == "mu"])))
  expect_equal(
    out$.value[out$.variable == "mu"], rep(stats::median(data$mu), 3)
  )
  for (k in seq_along(widths)) {
    bounds <- stats::quantile(
      data$mu, c((1 - widths[[k]]) / 2, (1 + widths[[k]]) / 2), names = FALSE
    )
    expect_equal(c(out$.lower[[k]], out$.upper[[k]]), bounds)
  }
  means <- summarise_intervals(x, a[i], .point = "mean")
  expect_equal(means$.value, c(mean(data[["a[1]"]]), mean(data[["a[2]"]])))
  expect_equal(means$.interval, rep("qi", 2))
  expect_error(summarise_intervals(x, mu, .width = c(0, 1)), "probabilities")
  expect_error(summarise_intervals(x, mu, .width = NA_real_), "probabilities")
  expect_error(summarise_intervals(x, mu, .point = "mode"), "arg")
  data$mu[[1L]] <- NA_real_
  expect_error(summarise_intervals(on_disk(data), mu), "Missing values")
})
