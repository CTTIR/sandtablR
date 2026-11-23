test_that("sdt_scenario() builds and summarises", {
  co <- sdt_synthetic_cohort(n = 15, seed = 2)
  sc <- sdt_scenario(co, resources = list(or = 2), name = "t")
  expect_s3_class(sc, "scenario")
  expect_equal(sc$meta$n, 15)
  expect_equal(sc$resources$or, 2)
  expect_s3_class(summary(sc), "tbl_df")
})

test_that("sdt_scenario() errors on missing columns", {
  expect_error(sdt_scenario(tibble::tibble(x = 1)), "missing 4 required columns")
})

test_that("sdt_scenario() refuses a capacity it would otherwise coerce", {
  co <- sdt_synthetic_cohort(n = 5, seed = 1)
  # Each of these used to be coerced without a word: 0 and -1 to 1, 2.7 and
  # "2" to 2, Inf and 3e9 to NA, so the simulated board was not the requested
  # one.
  for (cap in list(0, -1, 2.7, NA_real_, Inf, 3e9, "2", c(1, 2))) {
    expect_error(
      sdt_scenario(co, resources = list(or = cap)),
      "whole-number capacities"
    )
  }
  # Every offending area is named in the one error.
  err <- tryCatch(
    sdt_scenario(co, resources = list(or = 0, ward = 1.5, icu_imc = 3)),
    error = conditionMessage
  )
  expect_match(err, "or", fixed = TRUE)
  expect_match(err, "ward", fixed = TRUE)
  expect_no_match(err, "icu_imc", fixed = TRUE)
  # Whole doubles are fine and arrive as integers.
  sc <- sdt_scenario(co, resources = list(or = 3, icu_imc = 6L))
  expect_identical(sc$resources$or, 3L)
  expect_identical(sc$resources$icu_imc, 6L)
  expect_identical(sc$resources$ward, 1L)
})
