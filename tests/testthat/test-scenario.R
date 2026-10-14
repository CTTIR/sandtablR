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
