test_that("plot functions return ggplot objects", {
  sc <- sdt_scenario(sdt_synthetic_cohort(n = 10, seed = 1))
  expect_s3_class(sdt_plot_triage_mix(sc), "ggplot")
})
