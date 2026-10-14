# Printing, coercion and the argument-checking helpers.
#
# These are the paths a user hits first and a test suite reaches last: a print
# method that errors is as broken as a broken model, and an error message that
# cannot be formatted turns a clear failure into an obscure one.

test_that("a patient card formats and prints as one line", {
  card <- sdt_patient_card("P01", 34, "m", 4L, TRUE, "thorax", "red", "DCS",
                           1L)
  txt <- format(card)
  expect_type(txt, "character")
  expect_length(txt, 1L)
  expect_match(txt, "P01")
  expect_match(txt, "34y m")
  expect_match(txt, "thorax")
  expect_match(txt, "triage red")
  expect_match(txt, "ventilated")
  # print() must show exactly what format() produced.
  expect_output(print(card), txt, fixed = TRUE)
  expect_invisible(print(card))
})

test_that("a card that is not ventilated says nothing about ventilation", {
  card <- sdt_patient_card("P02", 20, "f", 5L, FALSE, "head", "green",
                           "TASC", 2L)
  expect_no_match(format(card), "ventilated")
})

test_that("format() and print() reject extra arguments", {
  card <- sdt_patient_card("P01", 34, "m", 4L, TRUE, "thorax", "red", "DCS",
                           1L)
  expect_error(format(card, extra = 1))
  expect_error(print(card, extra = 1))
})

test_that("validate_patient_card() catches life points out of range", {
  card <- sdt_patient_card("P01", 34, "m", 4L, TRUE, "thorax", "red", "DCS",
                           1L)
  card$life_points <- 99L
  expect_error(validate_patient_card(card), class = "sandtablR_error_card")
  card$life_points <- -1L
  expect_error(validate_patient_card(card), "life_points")
  # 0 is the card's death marker, not an out-of-range value.
  card$life_points <- 0L
  expect_no_error(validate_patient_card(card))
})

test_that("validate_patient_card() rejects a non-card", {
  expect_error(validate_patient_card(list(triage = "red")), "patient_card")
})


# Scenario ---------------------------------------------------------------------

test_that("a scenario formats with its size, mix and capacities", {
  sc <- sdt_scenario(sdt_synthetic_cohort(n = 12, seed = 1),
                     resources = list(or = 3), name = "concert")
  txt <- format(sc)
  expect_length(txt, 3L)
  expect_match(txt[[1L]], "concert")
  expect_match(txt[[1L]], "12 casualties")
  expect_match(txt[[2L]], "triage mix")
  expect_match(txt[[3L]], "or: 3")
  expect_output(print(sc), "concert")
  expect_invisible(print(sc))
})

test_that("a one-casualty scenario is singular", {
  sc <- sdt_scenario(sdt_synthetic_cohort(n = 1, seed = 1))
  expect_match(format(sc)[[1L]], "1 casualty")
})

test_that("as_tibble() on a scenario returns its cohort", {
  sc <- sdt_scenario(sdt_synthetic_cohort(n = 9, seed = 1))
  out <- tibble::as_tibble(sc)
  expect_s3_class(out, "tbl_df")
  expect_identical(nrow(out), 9L)
  expect_identical(out$id, sc$cohort$id)
  expect_error(tibble::as_tibble(sc, extra = 1))
})

test_that("autoplot() on a scenario draws the triage mix", {
  sc <- sdt_scenario(sdt_synthetic_cohort(n = 9, seed = 1))
  expect_s3_class(ggplot2::autoplot(sc), "ggplot")
  expect_error(ggplot2::autoplot(sc, extra = 1))
})

test_that("sdt_scenario() rejects malformed resources", {
  co <- sdt_synthetic_cohort(n = 5, seed = 1)
  expect_error(sdt_scenario(co, resources = "lots"), "named list")
  expect_error(sdt_scenario(co, resources = list(canteen = 2)),
               "must be board areas")
})

test_that("summary() on a scenario counts triage by therapy", {
  sc <- sdt_scenario(sdt_synthetic_cohort(n = 30, seed = 1))
  out <- summary(sc)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("triage", "therapy", "n"))
  expect_identical(sum(out$n), 30L)
})


# game_run ---------------------------------------------------------------------

test_that("a game run formats with replications, casualties and events", {
  skip_if_not_installed("simmer")
  sc <- sdt_scenario(sdt_synthetic_cohort(n = 10, seed = 1))
  run <- sdt_simulate(sc, replications = 2, seed = 1, verbose = FALSE)
  txt <- format(run)
  expect_length(txt, 2L)
  expect_match(txt[[1L]], "2 replications")
  expect_match(txt[[2L]], "area events")
  expect_output(print(run), "game_run")
  expect_invisible(print(run))
})

test_that("a single-replication run is singular", {
  skip_if_not_installed("simmer")
  sc <- sdt_scenario(sdt_synthetic_cohort(n = 5, seed = 1))
  run <- sdt_simulate(sc, replications = 1, seed = 1, verbose = FALSE)
  expect_match(format(run)[[1L]], "1 replication over")
})


# Argument checks --------------------------------------------------------------

test_that(".check_string() names the offending argument", {
  f <- function(label) .check_string(label)
  expect_error(f(1), "label")
  expect_error(f(c("a", "b")), "single non-NA string")
  expect_error(f(NA_character_), "single non-NA string")
  expect_invisible(f("ok"))
})

test_that(".check_count() accepts whole numbers and rejects the rest", {
  f <- function(n) .check_count(n)
  expect_identical(f(3), 3L)
  expect_error(f(0), "positive whole number")
  expect_error(f(-1), "positive whole number")
  expect_error(f(1.5), "positive whole number")
  expect_error(f(NA), "positive whole number")
  expect_error(f(c(1, 2)), "positive whole number")
})

test_that(".check_flag() insists on a single TRUE or FALSE", {
  f <- function(flag) .check_flag(flag)
  expect_invisible(f(TRUE))
  expect_error(f(NA), "TRUE")
  expect_error(f("yes"), "TRUE")
  expect_error(f(c(TRUE, FALSE)), "TRUE")
})

test_that(".check_class() reports what it got", {
  f <- function(x) .check_class(x, "scenario")
  err <- expect_error(f(1), "scenario")
  expect_match(conditionMessage(err), "numeric")
})

test_that(".require_pkgs() names every missing package and how to install it", {
  # Package names that cannot exist, so the message is deterministic.
  one <- expect_error(.require_pkgs("notarealpackage000"))
  expect_match(conditionMessage(one), "notarealpackage000")
  expect_match(conditionMessage(one), "install.packages\\(\"notarealpackage000\"\\)")

  many <- expect_error(
    .require_pkgs(c("notarealpackage000", "notarealpackage001"))
  )
  expect_match(conditionMessage(many), "install.packages\\(c\\(")
  expect_match(conditionMessage(many), "notarealpackage001")
})

test_that(".require_pkgs() passes when everything is present", {
  expect_true(.require_pkgs(c("stats", "utils")))
})


# Launcher ---------------------------------------------------------------------

test_that("the optional app package list is honest about what is missing", {
  missing <- .sdt_optional_app_pkgs()
  expect_type(missing, "character")
  expect_false(any(vapply(missing, requireNamespace, logical(1),
                          quietly = TRUE)))
})
