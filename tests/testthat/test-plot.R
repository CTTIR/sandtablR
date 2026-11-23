test_that("plot functions return ggplot objects", {
  sc <- sdt_scenario(sdt_synthetic_cohort(n = 10, seed = 1))
  expect_s3_class(sdt_plot_triage_mix(sc), "ggplot")
})

test_that("the board subtitle is plain ASCII and accounts for every casualty", {
  skip_if_not_installed("simmer")
  run <- sdt_simulate(sdt_reference_scenario(), replications = 1, seed = 1,
                      verbose = FALSE)
  # An em dash here made every non-UTF-8 graphics device, including the pdf()
  # device R CMD check runs examples on, warn and substitute a hyphen.
  late <- sdt_plot_board(run)$labels$subtitle
  early <- sdt_plot_board(run, at = sdt_reference_horizon() / 2)$labels$subtitle
  for (s in c(late, early)) {
    expect_identical(iconv(s, "UTF-8", "ASCII"), s)
  }
  expect_match(late, "^Replication 1 at 270 min: 40 of 40 casualties")
  expect_match(early, "not yet arrived$")
})
