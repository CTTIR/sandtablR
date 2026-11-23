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

test_that("area legends give every board state a key and a display label", {
  skip_if_not_installed("simmer")
  co <- sdt_synthetic_cohort(15, seed = 1)
  run <- sdt_simulate(sdt_scenario(co), replications = 2, seed = 1,
                      verbose = FALSE)
  # No casualty reaches the mortuary, a transfer or "not yet seen" here, so
  # those three levels exist only because of drop = FALSE. They used to be
  # listed with an empty swatch, and the flow legend showed column names.
  flow <- sdt_plot_flow(run)
  expect_true(isTRUE(flow$layers[[1]]$show.legend))
  keys <- ggplot2::get_guide_data(flow, "colour")
  expect_identical(nrow(keys), length(sdt_board_areas()))
  expect_identical(keys$.label, sdt_area_labels())

  skip_if_not_installed("TraMineR")
  for (type in c("index", "distribution")) {
    p <- sdt_plot_sequences(run, type = type)
    expect_true(isTRUE(p$layers[[1]]$show.legend), info = type)
    fills <- ggplot2::get_guide_data(p, "fill")
    expect_identical(nrow(fills), length(sdt_board_areas()), info = type)
  }
})

test_that("the utilisation plot names its areas, replications and counts", {
  skip_if_not_installed("simmer")
  co <- sdt_synthetic_cohort(15, seed = 1)
  run <- sdt_simulate(sdt_scenario(co), replications = 2, seed = 1,
                      verbose = FALSE)
  p <- sdt_plot_utilisation(run)
  # The legend used to show column names ("icu_imc"), the strips a bare
  # replication number, and the axis fractional places such as 0.5.
  keys <- ggplot2::get_guide_data(p, "colour")
  expect_true(all(keys$.label %in% sdt_area_labels()))
  expect_false(any(keys$.label %in% c("icu_imc", "ed_red", "green_area")))
  built <- ggplot2::ggplot_build(p)
  strips <- built$layout$layout$replication
  expect_identical(
    p$facet$params$labeller(data.frame(replication = strips))[[1]],
    paste("Replication", strips)
  )
  y_breaks <- built$layout$panel_params[[1]]$y$breaks
  y_breaks <- y_breaks[!is.na(y_breaks)]
  expect_true(all(y_breaks == round(y_breaks)))
  expect_identical(.sdt_whole_breaks(c(0, 2)), c(0, 1, 2))
})
