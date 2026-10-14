explain_ready <- function() {
  need <- c("parsnip", "recipes", "workflows", "rsample", "tune",
            "yardstick", "dials", "hardhat", "rpart", "DALEX", "DALEXtra")
  all(vapply(need, requireNamespace, logical(1), quietly = TRUE))
}

skip_unless_explain <- function() {
  skip_if_not(explain_ready(), "DALEX / tidymodels not installed")
}

explain_fit <- function(n = 200) {
  co <- sdt_add_decisions(sdt_synthetic_cohort(n = n, seed = 3), seed = 4)
  sdt_fit(co, target = "called_triage", engine = "tree", tune = FALSE,
          folds = 3L, seed = 1, verbose = FALSE)
}

test_that("sdt_explain() builds an explainer over the held-out split", {
  skip_unless_explain()
  fit <- explain_fit()
  ex <- sdt_explain(fit)
  expect_s3_class(ex, "explainer")
  expect_setequal(names(ex$data), fit$predictors)
  # Explaining on the rows the model was fitted to would flatter it, so the
  # default is the test half.
  expect_identical(nrow(ex$data), nrow(rsample::testing(fit$split)))
})

test_that("the explainer predicts class probabilities, not a single column", {
  skip_unless_explain()
  fit <- explain_fit()
  ex <- sdt_explain(fit)
  probs <- ex$predict_function(fit$workflow, ex$data)
  expect_true(is.matrix(probs))
  expect_identical(ncol(probs), 3L)
  # loss_cross_entropy indexes these columns by the observed class name, so
  # the tidymodels ".pred_" prefix has to be gone.
  expect_setequal(colnames(probs), sdt_triage_levels())
  expect_equal(unname(rowSums(probs)), rep(1, nrow(probs)), tolerance = 1e-8)
})

test_that("sdt_importance() returns a ggplot", {
  skip_on_cran()
  skip_unless_explain()
  expect_s3_class(sdt_importance(explain_fit(), n_permutations = 3L), "ggplot")
})

test_that("sdt_profile() returns a ggplot for PDP and ALE", {
  skip_on_cran()
  skip_unless_explain()
  ex <- sdt_explain(explain_fit())
  expect_s3_class(sdt_profile(ex, variables = "life_points"), "ggplot")
  expect_s3_class(
    sdt_profile(ex, variables = "life_points", type = "accumulated"),
    "ggplot"
  )
})

test_that("sdt_profile() rejects an unknown type", {
  skip_unless_explain()
  expect_error(sdt_profile(explain_fit(), type = "sideways"))
})

test_that("sdt_shap() accepts a one-row data frame or an id", {
  skip_on_cran()
  skip_unless_explain()
  fit <- explain_fit()
  ex <- sdt_explain(fit)
  test <- rsample::testing(fit$split)
  expect_s3_class(sdt_shap(ex, test[1, ], n_permutations = 3L), "ggplot")
  expect_s3_class(
    sdt_shap(ex, test$id[[1L]], data = test, n_permutations = 3L),
    "ggplot"
  )
})

test_that("sdt_shap() reports a bad casualty argument clearly", {
  skip_unless_explain()
  fit <- explain_fit()
  ex <- sdt_explain(fit)
  test <- rsample::testing(fit$split)
  expect_error(sdt_shap(ex, test[1:3, ]), "single casualty")
  expect_error(sdt_shap(ex, "P001"), "needs .*data")
  expect_error(sdt_shap(ex, "nobody", data = test), "No casualty with id")
})

test_that("sdt_explain() refuses an empty explanation set", {
  skip_unless_explain()
  fit <- explain_fit()
  empty <- rsample::testing(fit$split)[0, ]
  expect_error(sdt_explain(fit, data = empty), "at least one row")
})

test_that("explainers can be reused across explanations", {
  skip_on_cran()
  skip_unless_explain()
  ex <- sdt_explain(explain_fit())
  expect_s3_class(sdt_importance(ex, n_permutations = 2L), "ggplot")
  expect_s3_class(sdt_profile(ex, variables = "age"), "ggplot")
})

test_that("the explain entry points reject a non-model", {
  expect_error(sdt_explain(sdt_synthetic_cohort(n = 5, seed = 1)))
})
