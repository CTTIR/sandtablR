test_that("sdt_cohort() returns a validated tibble subclass", {
  co <- sdt_cohort(sdt_synthetic_cohort(n = 12, seed = 1))
  expect_s3_class(co, "cohort")
  expect_s3_class(co, "tbl_df")
  expect_identical(nrow(co), 12L)
  expect_invisible(validate_cohort(co))
})

test_that("sdt_cohort() puts the contract columns first and keeps extras", {
  raw <- sdt_synthetic_cohort(n = 5, seed = 1)
  raw$note <- letters[1:5]
  raw <- raw[, rev(names(raw))]
  co <- sdt_cohort(raw)
  expect_identical(names(co)[1:9], .cohort_columns())
  expect_true("note" %in% names(co))
})

test_that("sdt_cohort() orders by arrival_order", {
  raw <- sdt_synthetic_cohort(n = 6, seed = 2)
  shuffled <- raw[c(4, 1, 6, 2, 5, 3), ]
  co <- sdt_cohort(shuffled)
  expect_identical(co$arrival_order, sort(co$arrival_order))
})

test_that("sdt_cohort() errors on missing contract columns", {
  expect_error(sdt_cohort(tibble::tibble(x = 1)), "missing 9 required columns")
})

test_that("sdt_cohort() points a scenario at its own cohort", {
  sc <- sdt_scenario(sdt_synthetic_cohort(n = 4, seed = 1))
  expect_error(sdt_cohort(sc), "sdt_cohort\\(x\\$cohort\\)")
})

test_that("validate_cohort() fires on every controlled vocabulary", {
  fields <- c("sex", "injury_region", "triage", "therapy")
  for (field in fields) {
    raw <- sdt_synthetic_cohort(n = 6, seed = 3)
    raw[[field]][[2L]] <- "not-a-level"
    expect_error(
      sdt_cohort(raw),
      class = "sandtablR_error_cohort",
      info = field
    )
  }
})

test_that("validate_cohort() reports every bad row at once, not just the first", {
  raw <- sdt_synthetic_cohort(n = 8, seed = 4)
  raw$triage[c(2L, 5L, 7L)] <- "purple"
  err <- expect_error(sdt_cohort(raw), class = "sandtablR_error_cohort")
  msg <- conditionMessage(err)
  expect_match(msg, "2")
  expect_match(msg, "5")
  expect_match(msg, "7")
})

test_that("validate_cohort() collects problems across several columns", {
  raw <- sdt_synthetic_cohort(n = 6, seed = 5)
  raw$triage[[1L]] <- "purple"
  raw$therapy[[2L]] <- "XYZ"
  raw$life_points[[3L]] <- 99L
  err <- expect_error(sdt_cohort(raw), class = "sandtablR_error_cohort")
  msg <- conditionMessage(err)
  expect_match(msg, "triage")
  expect_match(msg, "therapy")
  expect_match(msg, "life_points")
})

test_that("validate_cohort() catches out-of-range and duplicate values", {
  base <- sdt_synthetic_cohort(n = 5, seed = 6)

  # 0 is the death marker on the version 2.0 card and is valid; -1 is not.
  low <- base
  low$life_points[[1L]] <- -1L
  expect_error(sdt_cohort(low), class = "sandtablR_error_cohort")

  dead <- base
  dead$life_points[[1L]] <- 0L
  expect_no_error(sdt_cohort(dead))

  high <- base
  high$life_points[[1L]] <- 13L
  expect_error(sdt_cohort(high), class = "sandtablR_error_cohort")

  dup <- base
  dup$id[[2L]] <- dup$id[[1L]]
  expect_error(sdt_cohort(dup), "unique")

  old <- base
  old$age[[1L]] <- 200L
  expect_error(sdt_cohort(old), class = "sandtablR_error_cohort")
})

test_that("a zero-row cohort is valid and keeps its column types", {
  empty <- sdt_synthetic_cohort(n = 3, seed = 1)[0, ]
  co <- sdt_cohort(empty)
  expect_s3_class(co, "cohort")
  expect_identical(nrow(co), 0L)
  expect_identical(names(co), .cohort_columns())
  expect_type(co$id, "character")
  expect_type(co$life_points, "integer")
  expect_type(co$ventilated, "logical")
  expect_invisible(validate_cohort(co))
})

test_that("cohorts round-trip through patient cards", {
  co <- sdt_synthetic_cohort(n = 7, seed = 8)
  cards <- sdt_as_patient_cards(co)
  expect_length(cards, 7L)
  expect_s3_class(cards[[1L]], "patient_card")
  expect_named(cards, co$id)

  rebuilt <- sdt_cohort(dplyr::bind_rows(lapply(
    cards, function(card) tibble::as_tibble(unclass(card))
  )))
  expect_identical(rebuilt$id, co$id)
  expect_identical(rebuilt$triage, co$triage)
  expect_identical(rebuilt$therapy, co$therapy)
  expect_identical(rebuilt$life_points, co$life_points)
  expect_identical(rebuilt$age, co$age)
})

test_that("sdt_as_patient_cards() handles a zero-row cohort", {
  co <- sdt_cohort(sdt_synthetic_cohort(n = 3, seed = 1)[0, ])
  expect_length(sdt_as_patient_cards(co), 0L)
})

test_that("the cohort header reports size and triage mix", {
  co <- sdt_synthetic_cohort(n = 10, seed = 1)
  header <- tbl_sum.cohort(co)
  expect_named(header, c("A cohort", "Triage"))
  expect_match(header[["A cohort"]], "10 casualties")
  expect_match(header[["Triage"]], "red")
})
