test_that("held-out metric selection is warning-free and preserves class scores", {
  skip_if_not_installed("tune")
  skip_if_not_installed("yardstick")
  withr::local_options(lifecycle_verbosity = "warning")
  levels <- c("red", "yellow", "green")
  truth <- factor(rep(levels, each = 2), levels = levels)
  predictions <- tibble::tibble(
    called_triage = truth,
    .pred_red = c(.8, .7, .1, .2, .1, .6),
    .pred_yellow = c(.1, .2, .8, .7, .1, .1),
    .pred_green = c(.1, .1, .1, .1, .8, .3),
    .pred_class = factor(rep("green", 6), levels = levels)
  )
  local_mocked_bindings(
    collect_predictions = function(x, ...) predictions,
    .package = "tune"
  )
  warnings <- character()
  scores <- withCallingHandlers(
    .score_last_fit(NULL, "called_triage", sdt_metric_set()),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(warnings, character())
  expect_equal(scores$.estimate[scores$.metric == "accuracy"], 5 / 6)
  expect_equal(scores$.estimate[scores$.metric == "kap"], .75)
  expected_class <- factor(c("red", "red", "yellow", "yellow", "green", "red"),
                           levels = levels)
  expect_equal(scores$.estimate[scores$.metric == "f_meas"],
               yardstick::f_meas_vec(truth, expected_class))
  expect_equal(scores$.estimate[scores$.metric == "roc_auc"],
               yardstick::roc_auc_vec(truth, as.matrix(predictions[, 2:4])))
})
