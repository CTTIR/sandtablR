skip_if_not_installed("simmer")

small_run <- function(n = 12, ...) {
  co <- sdt_synthetic_cohort(n = n, seed = 4)
  sc <- sdt_scenario(co, resources = list(or = 2, icu_imc = 4, ward = 15))
  sdt_simulate(sc, seed = 1, verbose = FALSE, ...)
}

test_that("sdt_simulate() returns a game_run with the documented tables", {
  run <- small_run(replications = 2)
  expect_s3_class(run, "game_run")
  expect_true(all(c("arrivals", "events", "resources", "scenario", "meta") %in%
                    names(run)))
  expect_true("replication" %in% names(run$arrivals))
  expect_named(
    run$events,
    c("replication", "id", "step", "area", "start", "end", "duration",
      "service_time")
  )
})

test_that("sdt_simulate() is reproducible for a given seed", {
  expect_identical(small_run(replications = 2), small_run(replications = 2))
})

test_that("every casualty appears in the event log", {
  run <- small_run(n = 20, replications = 3)
  expect_setequal(unique(run$events$id), run$scenario$cohort$id)
  expect_length(table(run$events$replication), 3L)
})

test_that("event steps are contiguous and time-ordered per casualty", {
  run <- small_run(n = 20, replications = 2)
  by_case <- split(run$events, list(run$events$replication, run$events$id),
                   drop = TRUE)
  for (ev in by_case) {
    expect_identical(ev$step, seq_len(nrow(ev)))
    expect_false(is.unsorted(ev$start))
    expect_true(all(ev$end >= ev$start))
    # A state ends exactly where the next begins: no gaps, no overlaps.
    if (nrow(ev) > 1L) {
      expect_equal(ev$end[-nrow(ev)], ev$start[-1L])
    }
  }
})

test_that("nobody is recorded outside the horizon", {
  run <- small_run(n = 20, replications = 2, horizon = 200)
  expect_true(all(run$events$start <= 200))
  expect_true(all(run$events$end <= 200))
})

test_that("routing follows the sighting category", {
  run <- small_run(n = 60, replications = 3)
  ev <- dplyr::left_join(
    run$events,
    dplyr::select(tibble::as_tibble(run$scenario$cohort), "id", "triage"),
    by = "id"
  )
  # Everyone starts in triage.
  expect_true(all(ev$area[ev$step == 1L] == "triage"))
  # Reds never enter the yellow bay and vice versa.
  expect_false(any(ev$area == "ed_yellow" & ev$triage == "red"))
  expect_false(any(ev$area == "ed_red" & ev$triage == "yellow"))
  # Greens are cared for in the green treatment area and never reach theatre.
  expect_setequal(unique(ev$area[ev$triage == "green"]),
                  c("triage", "green_area"))
})

test_that("critical care and ward beds are terminal", {
  run <- small_run(n = 60, replications = 3)
  ev <- dplyr::arrange(run$events, .data$replication, .data$id, .data$step)
  by_case <- split(ev, list(ev$replication, ev$id), drop = TRUE)
  for (case in by_case) {
    hit <- which(case$area %in% c("icu_imc", "ward", "green_area"))
    if (length(hit)) {
      expect_identical(max(hit), nrow(case))
    }
  }
})

test_that("theatre time responds to the therapy concept", {
  # TASC is quickest and ETC longest, so mean theatre service must be ordered.
  co <- sdt_synthetic_cohort(n = 400, seed = 5)
  co$triage <- "red"
  co$life_points <- 3L
  sc <- sdt_scenario(co, resources = list(or = 50, ed_red = 50,
                                          diagnostics = 50, icu_imc = 50,
                                          ward = 200, triage = 20))
  run <- sdt_simulate(sc, replications = 1, horizon = Inf, seed = 2,
                      verbose = FALSE)
  ev <- dplyr::left_join(
    run$events[run$events$area == "or", ],
    dplyr::select(tibble::as_tibble(co), "id", "therapy"),
    by = "id"
  )
  means <- tapply(ev$service_time, ev$therapy, mean, na.rm = TRUE)
  skip_if(anyNA(means[c("DCS", "TASC")]), "not enough surgical casualties")
  expect_lt(means[["TASC"]], means[["DCS"]])
})

test_that("sdt_simulate() validates its arguments", {
  co <- sdt_synthetic_cohort(n = 5, seed = 1)
  sc <- sdt_scenario(co)
  expect_error(sdt_simulate(co), "scenario")
  expect_error(sdt_simulate(sc, replications = 0), "positive whole number")
  expect_error(sdt_simulate(sc, horizon = -1), "single positive number")
  expect_error(sdt_simulate(sc, horizon = c(1, 2)), "single positive number")
  expect_error(sdt_simulate(sc, service_times = "fast"), "named list")
  expect_error(
    sdt_simulate(sc, service_times = list(canteen = 5)),
    "must be board areas"
  )
})

test_that("service_times overrides reach the engine", {
  co <- sdt_synthetic_cohort(n = 30, seed = 6)
  co$triage <- "green"
  sc <- sdt_scenario(co, resources = list(green_area = 30, triage = 10))
  mean_green <- function(...) {
    run <- sdt_simulate(sc, horizon = Inf, seed = 3, verbose = FALSE, ...)
    mean(run$events$service_time[run$events$area == "green_area"],
         na.rm = TRUE)
  }
  expect_gt(mean_green(service_times = list(green_area = 200)),
            mean_green(service_times = list(green_area = 2)))
})

test_that("an empty cohort produces empty tables of the right shape", {
  co <- sdt_cohort(sdt_synthetic_cohort(n = 3, seed = 1)[0, ])
  sc <- sdt_scenario(co)
  run <- sdt_simulate(sc, replications = 2, verbose = FALSE)
  expect_identical(nrow(run$events), 0L)
  expect_identical(nrow(run$arrivals), 0L)
  expect_named(
    run$events,
    c("replication", "id", "step", "area", "start", "end", "duration",
      "service_time")
  )
  s <- summary(run)
  expect_identical(nrow(s), length(sdt_board_areas()))
  expect_true(all(s$n == 0L))
})

test_that("verbose reporting can be switched on", {
  co <- sdt_synthetic_cohort(n = 3, seed = 1)
  sc <- sdt_scenario(co)
  expect_message(sdt_simulate(sc, replications = 1, verbose = TRUE),
                 "Replication 1/1")
})

test_that("parallel replications produce the same structure as sequential", {
  skip_if_not_installed("future.apply")
  co <- sdt_synthetic_cohort(n = 15, seed = 8)
  sc <- sdt_scenario(co, resources = list(or = 2))
  par <- sdt_simulate(sc, replications = 3, parallel = TRUE, seed = 9,
                      verbose = FALSE)
  seq_run <- sdt_simulate(sc, replications = 3, parallel = FALSE, seed = 9,
                          verbose = FALSE)
  # Different RNG streams, but the same shape and the same casualties.
  expect_gt(nrow(par$events), 0L)
  expect_named(par$events, names(seq_run$events))
  expect_setequal(unique(par$events$id), unique(seq_run$events$id))
  expect_identical(par$meta$replications, seq_run$meta$replications)
})


# summary() -------------------------------------------------------------------

test_that("summary.game_run() covers every board area", {
  s <- summary(small_run(n = 20, replications = 2))
  expect_s3_class(s, "tbl_df")
  expect_named(s, c("area", "label", "n", "proportion"))
  expect_identical(s$area, sdt_board_areas())
  expect_equal(sum(s$proportion), 1, tolerance = 1e-8)
})

test_that("summary.game_run(reference = TRUE) adds the published columns", {
  s <- summary(small_run(n = 20, replications = 2), reference = TRUE)
  expect_named(s, c("area", "label", "n", "proportion", "reference",
                    "difference"))
  ref <- sdt_reference("published")$end_state
  expect_equal(s$reference[s$area == "ward"], unname(ref[["ward"]]))
})

test_that("summary.game_run() rejects a non-flag reference argument", {
  expect_error(summary(small_run(replications = 1), reference = "yes"), "TRUE")
})

# The calibration claim, made checkable.
#
# This checks all eight locations the source reports, not the three it is
# easiest to hit. An earlier version of this test compared ward, ICU/IMC and
# theatre and folded everything else into a single "other" cell, which passed
# while the engine was holding twice the published share of casualties in the
# emergency department: the error was inside the cell that absorbed it.
#
# The tolerance is the tolerance of the fit recorded by data-raw/calibrate.R,
# with room for replication noise. If a change to the routing or the service
# times breaks it, re-run that script rather than widening this number.
test_that("the calibrated scenario reproduces the published end state", {
  skip_on_cran()
  sc <- sdt_reference_scenario()
  run <- sdt_simulate(sc, replications = 24,
                      horizon = sdt_reference_horizon(),
                      seed = 11, verbose = FALSE)
  s <- summary(run, reference = TRUE)
  expect_setequal(s$area, names(sdt_reference("published")$end_state))
  expect_false(anyNA(s$reference))
  expect_lt(max(abs(s$difference)), 0.06)
})

test_that("every casualty is accounted for at the end of the game", {
  # The published shares are over all patient cards, so a casualty the game
  # never reached has to count as "not yet seen" rather than vanish from the
  # denominator.
  run <- small_run(n = 20, replications = 3)
  s <- summary(run)
  expect_identical(sum(s$n), 20L * 3L)
  expect_equal(sum(s$proportion), 1, tolerance = 1e-8)
})
