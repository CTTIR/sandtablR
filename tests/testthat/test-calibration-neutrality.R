# Bit-identity guard for the calibrated fixture.
#
# test-simulate.R checks that the calibrated scenario lands *within tolerance*
# of the published end state. That is the claim, but it is a weak guard against
# drift: max|difference| is 0.0396 against a 0.06 gate, so a change could add
# 1.5 percentage points to the ward share and still pass while having moved the
# model materially.
#
# This file asserts the stronger property every calibration-neutral change
# actually has: the seeded run is *unchanged*, not merely still acceptable.
# A change that trips these numbers is not necessarily wrong, but it is
# certainly not neutral, and it has to be justified rather than discovered.

test_that("the calibrated fixture is bit-identical", {
  skip_on_cran()

  sc <- sdt_reference_scenario()
  run <- sdt_simulate(
    sc,
    replications = 24,
    horizon = sdt_reference_horizon(),
    seed = 11,
    verbose = FALSE
  )

  # Shape of the three monitored tables.
  expect_identical(nrow(run$events), 3356L)
  expect_identical(nrow(run$arrivals), 407L)
  expect_identical(nrow(run$resources), 5757L)

  # The eight published reporting locations, exactly as counted today.
  s <- summary(run, reference = TRUE)
  expect_identical(s$n, c(67L, 149L, 31L, 123L, 3L, 213L, 374L, 0L))
  expect_equal(max(abs(s$difference)), 0.03958333, tolerance = 1e-7)

  # Whole-log identity, when a hash is available. The count vector above
  # already pins the end state; this pins every intermediate step too.
  skip_if_not_installed("digest")
  expect_identical(
    digest::digest(run$events), "eab66533c904ff9b17f1498f77be5d14"
  )
})

test_that("the generators are seed-stable", {
  # .cohort_base() draws inside a tibble() call, so column order is draw
  # order. Any inserted, removed or reordered draw moves both the golden
  # cohort and the calibration fixture at once. n = 40 as well as n = 10: a
  # small n can hide a changed probability by luck.
  co10 <- sdt_synthetic_cohort(n = 10, seed = 1)
  co40 <- sdt_synthetic_cohort(n = 40, seed = 1)

  expect_identical(co10$life_points, c(1L, 1L, 5L, 3L, 1L, 4L, 4L, 6L, 6L, 1L))
  expect_identical(
    co40$triage[seq_len(10L)],
    c("red", "red", "green", "yellow", "red", "yellow", "yellow", "green",
      "green", "red")
  )
  expect_identical(sum(co40$ventilated), 10L)
  expect_identical(
    unname(table(co40$therapy)[["none"]]), 10L
  )
})

test_that("the shipped routing never reaches the off-board states", {
  # morgue and external are part of the board vocabulary so that observed
  # data can be represented. The engine has no branch to either, so a
  # simulated run must never place a casualty there. If this ever fails, the
  # end_state_map entries for those two states have become load-bearing and
  # the calibration has to be re-derived.
  skip_on_cran()
  run <- sdt_simulate(
    sdt_reference_scenario(),
    replications = 4,
    horizon = sdt_reference_horizon(),
    seed = 11,
    verbose = FALSE
  )
  s <- summary(run)
  off <- s$n[s$area %in% c("morgue", "external")]
  expect_identical(off, c(0L, 0L))
  expect_false(any(run$events$area %in% c("morgue", "external")))
})
