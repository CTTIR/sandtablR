tidymodels_ready <- function() {
  need <- c("parsnip", "recipes", "workflows", "rsample", "tune",
            "yardstick", "dials", "hardhat")
  all(vapply(need, requireNamespace, logical(1), quietly = TRUE))
}

skip_unless_tidymodels <- function(engine = "rpart") {
  skip_if_not(tidymodels_ready(), "tidymodels not installed")
  skip_if_not_installed(engine)
}

bench_data <- function(n = 300, seed = 5) {
  sdt_add_decisions(sdt_synthetic_cohort(n = n, seed = seed), seed = seed + 1L)
}

quick_fit <- function(...) {
  sdt_fit(bench_data(), target = "called_triage", engine = "tree",
          tune = FALSE, folds = 3L, seed = 1, verbose = FALSE, ...)
}


# Engine registry -------------------------------------------------------------

test_that("sdt_engines() reports every engine and its box", {
  eng <- sdt_engines()
  expect_named(eng, c("engine", "package", "class_box", "installed"))
  expect_setequal(eng$engine,
                  c("multinom", "glmnet", "tree", "mars",
                    "xgboost", "ranger", "svm_rbf", "mlp", "transformer"))
  # Readability is graded, not binary: boosting sits between a model you can
  # read and one you can only perturb, and "closed" is a fourth grade rather
  # than a darker black -- the transformer decides in a representation it
  # learned, so a per-column attribution describes something it no longer
  # reasons in.
  expect_setequal(eng$class_box[eng$engine %in% c("glmnet", "tree")], "glass")
  expect_identical(eng$class_box[eng$engine == "xgboost"], "grey")
  expect_identical(eng$class_box[eng$engine == "ranger"], "black")
  expect_identical(eng$class_box[eng$engine == "transformer"], "closed")
  expect_true(all(eng$class_box %in% c("glass", "grey", "black", "closed")))
  expect_type(eng$installed, "logical")
  # The registry is ordered by readability. Both sdt_fit()'s validation and
  # the app's engine picker read that order rather than restating it, so a
  # scrambled registry would silently reorder the workbench.
  expect_identical(eng$class_box,
                   sort(factor(eng$class_box,
                               levels = c("glass", "grey", "black", "closed"))) |>
                     as.character())
})

test_that("every registered engine has a fixed and a tuned spec", {
  # The registry and the two spec switches are three lists that must agree.
  # switch() returns NULL for an unlisted engine rather than erroring, so an
  # engine added to the registry alone fails only at fit time.
  for (e in sdt_engines()$engine) {
    expect_s3_class(sandtablR:::.spec_fixed(e), "model_spec")
    expect_s3_class(sandtablR:::.spec_tuned(e), "model_spec")
  }
})

test_that("sdt_fit() accepts every installed engine and rejects unknown ones", {
  skip_unless_tidymodels()
  expect_error(
    sdt_fit(bench_data(), target = "called_triage", engine = "not_an_engine",
            tune = FALSE, seed = 1L, verbose = FALSE)
  )
  eng <- sdt_engines()
  # rpart is the one engine the rest of this file already requires, so it is
  # the only fit exercised by default; the others are checked when present.
  for (e in eng$engine[eng$installed]) {
    skip_if_not_installed(eng$package[eng$engine == e])
    fit <- sdt_fit(bench_data(), target = "called_triage", engine = e,
                   predictors = c("age", "sex", "life_points"),
                   tune = FALSE, seed = 1L, verbose = FALSE)
    expect_s3_class(fit, "sdt_model")
    expect_identical(fit$engine, e)
    expect_true(all(c("accuracy", "roc_auc") %in% sdt_metrics(fit)$.metric))
  }
})

test_that("sdt_metric_set() covers kappa as well as accuracy", {
  skip_if_not_installed("yardstick")
  ms <- sdt_metric_set()
  expect_true(inherits(ms, "metric_set"))
  expect_true(all(c("accuracy", "kap", "roc_auc", "f_meas") %in%
                    names(attr(ms, "metrics"))))
})


# Fitting ---------------------------------------------------------------------

test_that("sdt_fit() returns an sdt_model carrying its provenance", {
  skip_unless_tidymodels()
  fit <- quick_fit()
  expect_s3_class(fit, "sdt_model")
  expect_identical(fit$engine, "tree")
  expect_identical(fit$target, "called_triage")
  expect_identical(fit$class_box, "glass")
  # The split and resamples are kept, so the numbers can be traced back.
  expect_s3_class(fit$split, "rsplit")
  expect_s3_class(fit$resamples, "rset")
})

test_that("predictors exclude identifiers and every outcome column", {
  skip_unless_tidymodels()
  fit <- quick_fit()
  expect_false(any(c("id", "arrival_order") %in% fit$predictors))
  # A model for the participant's call must not be handed the true category.
  expect_false(any(.sdt_outcomes() %in% fit$predictors))
  expect_true("life_points" %in% fit$predictors)
})

test_that("logical predictors survive preprocessing", {
  # `ventilated` is logical, and recipes counts logicals as neither nominal
  # nor numeric, so without an explicit step it reaches glmnet and xgboost as
  # a column they cannot put in a numeric matrix.
  skip_unless_tidymodels("glmnet")
  fit <- sdt_fit(bench_data(), target = "called_triage", engine = "glmnet",
                 tune = FALSE, folds = 3L, seed = 1, verbose = FALSE)
  expect_true("ventilated" %in% fit$predictors)
  expect_identical(nrow(sdt_metrics(fit)), 4L)
})

test_that("sdt_metrics() always has the same three columns", {
  skip_unless_tidymodels()
  m <- sdt_metrics(quick_fit())
  expect_s3_class(m, "tbl_df")
  expect_named(m, c(".metric", ".estimator", ".estimate"))
  expect_setequal(m$.metric, c("accuracy", "f_meas", "kap", "roc_auc"))
  expect_true(all(m$.estimate >= 0 & m$.estimate <= 1))
})

test_that("sdt_predictions() returns held-out predictions with the truth", {
  skip_unless_tidymodels()
  preds <- sdt_predictions(quick_fit())
  expect_s3_class(preds, "tbl_df")
  expect_true("truth" %in% names(preds))
  expect_true(".pred_class" %in% names(preds))
  expect_gt(length(grep("^\\.pred_", names(preds))), 1L)
})

test_that("predict() on an sdt_model returns a tibble", {
  skip_unless_tidymodels()
  fit <- quick_fit()
  out <- predict(fit, bench_data(n = 20, seed = 9))
  expect_s3_class(out, "tbl_df")
  expect_identical(nrow(out), 20L)
  expect_true(".pred_class" %in% names(out))
})

test_that("printing an sdt_model names the engine, box and score", {
  skip_unless_tidymodels()
  txt <- format(quick_fit())
  expect_match(txt[[1L]], "tree")
  expect_match(txt[[1L]], "glass-box")
  expect_match(txt[[1L]], "called_triage")
  expect_match(txt[[2L]], "predictors")
})

test_that("tuning runs and records what it chose", {
  skip_on_cran()
  skip_unless_tidymodels()
  fit <- sdt_fit(bench_data(n = 200), target = "called_triage",
                 engine = "tree", tune = TRUE, grid = 3L, folds = 3L,
                 seed = 1, verbose = FALSE)
  expect_false(is.null(fit$tuning))
  expect_false(is.null(fit$best))
  expect_identical(nrow(fit$best), 1L)
})

test_that("sdt_fit() validates its arguments", {
  skip_unless_tidymodels()
  co <- bench_data(n = 60)
  expect_error(sdt_fit(co, target = "eye_colour"))
  expect_error(sdt_fit(co, engine = "neural"))
  expect_error(sdt_fit(co, prop = 0), "strictly in")
  expect_error(sdt_fit(co, prop = 1), "strictly in")
  expect_error(sdt_fit(co, folds = 0), "positive whole number")
  expect_error(
    sdt_fit(co, target = "called_triage", predictors = "nonesuch",
            verbose = FALSE),
    "missing 1 predictor"
  )
})

test_that("sdt_fit() refuses a target with only one class", {
  skip_unless_tidymodels()
  co <- bench_data(n = 60)
  co$called_triage <- "red"
  expect_error(
    sdt_fit(co, target = "called_triage", tune = FALSE, verbose = FALSE),
    "at least two classes"
  )
})

test_that("the true category is near-deterministic, the call is not", {
  # This is why the bench targets the participant's call: life points fix the
  # true category by construction, so every engine scores 1 on it.
  skip_on_cran()
  skip_unless_tidymodels()
  co <- bench_data(n = 300)
  truth_fit <- sdt_fit(co, target = "triage", engine = "tree", tune = FALSE,
                       folds = 3L, seed = 1, verbose = FALSE)
  call_fit <- quick_fit()
  auc <- function(f) {
    m <- sdt_metrics(f)
    m$.estimate[m$.metric == "roc_auc"]
  }
  expect_gt(auc(truth_fit), 0.98)
  expect_lt(auc(call_fit), 0.95)
})


# The bench -------------------------------------------------------------------

test_that("sdt_workflowset() ranks engines on one leaderboard", {
  skip_on_cran()
  skip_unless_tidymodels()
  ws <- sdt_workflowset(bench_data(), target = "called_triage",
                        engines = c("tree", "glmnet"), tune = FALSE,
                        folds = 3L, seed = 1, verbose = FALSE)
  expect_named(ws, c("engine", "class_box", ".metric", ".estimator",
                     ".estimate"))
  expect_setequal(unique(ws$engine), c("tree", "glmnet"))
  expect_identical(nrow(ws), 8L)
  # Best roc_auc first.
  auc <- ws[ws$.metric == "roc_auc", ]
  expect_identical(auc$engine[[1L]], auc$engine[which.max(auc$.estimate)])
})

test_that("sdt_workflowset() keeps the fitted models and names what it skipped", {
  skip_on_cran()
  skip_unless_tidymodels()
  ws <- sdt_workflowset(bench_data(), target = "called_triage",
                        engines = "tree", tune = FALSE, folds = 3L,
                        seed = 1, verbose = FALSE)
  models <- attr(ws, "models")
  expect_named(models, "tree")
  expect_s3_class(models$tree, "sdt_model")
  expect_identical(attr(ws, "skipped"), character())
})

test_that("sdt_workflowset() rejects an unknown engine", {
  expect_error(
    sdt_workflowset(bench_data(n = 40), engines = "telepathy",
                    verbose = FALSE),
    "unknown engine"
  )
})


# Curves and agreement --------------------------------------------------------

test_that("sdt_roc() returns a ggplot", {
  skip_unless_tidymodels()
  expect_s3_class(sdt_roc(quick_fit()), "ggplot")
})

test_that("sdt_consistency_score() is one row with a kappa in range", {
  skip_if_not_installed("yardstick")
  out <- sdt_consistency_score(
    c("red", "red", "yellow", "green"),
    c("red", "yellow", "yellow", "green")
  )
  expect_named(out, c(".metric", ".estimator", ".estimate", "n"))
  expect_identical(nrow(out), 1L)
  expect_identical(out$.metric, "kap")
  expect_identical(out$n, 4L)
})

test_that("perfect agreement scores 1 and chance agreement scores near 0", {
  skip_if_not_installed("yardstick")
  labels <- rep(c("red", "yellow", "green"), each = 40)
  expect_equal(sdt_consistency_score(labels, labels)$.estimate, 1)

  # Two raters who both always say "red" agree completely but share no
  # judgement; kappa is the measure that notices.
  all_red <- rep("red", 120)
  expect_true(is.na(sdt_consistency_score(all_red, all_red)$.estimate) ||
                sdt_consistency_score(all_red, all_red)$.estimate == 0)
})

test_that("sdt_consistency_score() rejects mismatched lengths", {
  skip_if_not_installed("yardstick")
  expect_error(sdt_consistency_score(c("red", "green"), "red"),
               "same length")
})

test_that("sdt_consistency_score() drops incomplete pairs", {
  skip_if_not_installed("yardstick")
  out <- sdt_consistency_score(
    c("red", NA, "green", "yellow"),
    c("red", "red", NA, "yellow")
  )
  expect_identical(out$n, 2L)
})

test_that("sdt_consistency_score() handles no usable pairs", {
  skip_if_not_installed("yardstick")
  out <- sdt_consistency_score(c(NA, NA), c(NA, NA))
  expect_identical(out$n, 0L)
  expect_true(is.na(out$.estimate))
})

test_that("parallel is off by default and restores the caller's plan", {
  skip_if_not_installed("future")
  # Default off. A package that claims cores without being asked is a bad
  # guest, and every entry point that can parallelise agrees on the default.
  expect_false(eval(formals(sdt_fit)$parallel))
  expect_false(eval(formals(sdt_workflowset)$parallel))
  expect_false(eval(formals(sdt_simulate)$parallel))

  # parallel = FALSE must not touch the plan at all.
  before <- future::plan()
  expect_false(sandtablR:::.sdt_parallel_plan(FALSE))
  expect_identical(class(future::plan()), class(before))

  # parallel = TRUE sets one for the duration of the CALLING frame and puts
  # the caller's back afterwards, so the setting cannot leak into a session.
  # class(plan())[[1]] is "FutureStrategy" for every strategy, so the
  # discriminating element is further down the vector.
  inner <- function() {
    sandtablR:::.sdt_parallel_plan(TRUE, workers = 2L)
    class(future::plan())
  }
  during <- inner()
  expect_true(any(c("multicore", "multisession", "cluster") %in% during))
  expect_false("sequential" %in% during)
  expect_identical(class(future::plan()), class(before))
})

test_that("the worker count leaves a core free and is capped", {
  n <- sandtablR:::.sdt_workers()
  expect_true(n >= 1L && n <= 8L)
  expect_identical(sandtablR:::.sdt_workers(3L), 3L)
})
