# The model bench and the reference distributions, as package code.
#
# test-model.R covers the happy path of a fixed fit. These cover what it does
# not reach: the tuning branch, the engine registry when a package is absent,
# the shape guarantees callers rely on, and every error the public API is
# supposed to raise instead of returning something misleading.

bench_pkgs <- function() {
  c("parsnip", "recipes", "workflows", "rsample", "tune", "yardstick",
    "dials", "hardhat")
}

skip_unless_bench <- function(...) {
  for (pkg in c(bench_pkgs(), ...)) {
    skip_if_not_installed(pkg)
  }
}

bench_cohort <- function(n = 60, seed = 3) {
  sdt_add_decisions(sdt_synthetic_cohort(n = n, seed = seed), seed = seed + 1L)
}

# Deliberately small: two folds on sixty casualties is enough to exercise every
# branch, and the suite has to stay under two minutes.
fixed_fit <- function(...) {
  sdt_fit(bench_cohort(), target = "called_triage", engine = "tree",
          tune = FALSE, folds = 2L, seed = 1, verbose = FALSE, ...)
}

# An engine table in which the black-box engines point at packages nobody has,
# so the "skipped" paths can be reached on a machine where everything is in
# fact installed.
absent_engines <- function() {
  tibble::tibble(
    engine = c("glmnet", "tree", "ranger", "xgboost"),
    package = c("glmnet", "rpart", "sandtabl.absent", "sandtabl.absent2"),
    class_box = c("glass", "glass", "black", "black")
  )
}


# The engine registry ---------------------------------------------------------

test_that("sdt_engines() computes the installed flag rather than storing it", {
  # A stored flag would be baked in at build time and wrong on every other
  # machine, which is exactly the sort of thing that only shows up in CI.
  eng <- sdt_engines()
  expect_s3_class(eng, "tbl_df")
  for (i in seq_along(eng$package)) {
    expect_identical(
      eng$installed[[i]],
      requireNamespace(eng$package[[i]], quietly = TRUE),
      info = eng$package[[i]]
    )
  }
})

test_that("every tunable specification really exposes tunable parameters", {
  skip_unless_bench()
  # parsnip decides which arguments are tunable by inspecting the *expression*
  # each was given, so a tune() marker routed through a variable or a helper
  # produces a spec with an empty parameter set: no grid is built and the
  # placeholder reaches the engine, which fails with "comparison (>) is not
  # possible for language types". This catches that at the specification.
  eng <- sdt_engines()
  for (engine in eng$engine[eng$installed]) {
    spec <- .model_spec(engine, tune = TRUE)
    params <- hardhat::extract_parameter_set_dials(spec)
    if (engine == "transformer") {
      # This engine intentionally uses fixed architecture and training settings.
      expect_identical(nrow(params), 0L)
    } else {
      expect_gt(nrow(params), 0L)
    }
    expect_identical(spec$mode, "classification")
  }
})

test_that("the fixed specifications leave nothing to tune", {
  skip_unless_bench()
  # Anything still marked tunable here would reach the engine as a placeholder
  # when tune = FALSE, because that path never builds a grid.
  eng <- sdt_engines()
  for (engine in eng$engine[eng$installed]) {
    params <- hardhat::extract_parameter_set_dials(
      .model_spec(engine, tune = FALSE)
    )
    expect_identical(nrow(params), 0L)
  }
})

test_that("sdt_metric_set() scores a three-class prediction table", {
  skip_if_not_installed("yardstick")
  lv <- c("green", "red", "yellow")
  preds <- tibble::tibble(
    truth = factor(rep(lv, each = 4L), levels = lv),
    .pred_class = factor(rep(lv, times = 4L), levels = lv),
    .pred_green = rep(0.5, 12L),
    .pred_red = rep(0.3, 12L),
    .pred_yellow = rep(0.2, 12L)
  )
  out <- sdt_metric_set()(
    preds, truth = "truth", estimate = ".pred_class",
    ".pred_green", ".pred_red", ".pred_yellow"
  )
  est <- stats::setNames(out$.estimate, out$.metric)
  kind <- stats::setNames(out$.estimator, out$.metric)
  expect_setequal(names(est), c("accuracy", "f_meas", "kap", "roc_auc"))

  # The fixture is built so every score is arithmetic rather than a snapshot:
  # six of the twelve calls land on the diagonal, and both margins are
  # uniform, so accuracy is 1/2, expected agreement is 1/3, and kappa is
  # (1/2 - 1/3) / (2/3). Per class precision and recall are both 1/2, so the
  # macro f_meas is 1/2 too.
  expect_equal(est[["accuracy"]], 0.5)
  expect_equal(est[["kap"]], 0.25)
  expect_equal(est[["f_meas"]], 0.5)
  # Every casualty carries the same probabilities, so no threshold separates
  # anything and the curve sits on the diagonal.
  expect_equal(est[["roc_auc"]], 0.5)

  # The estimator matters as much as the number. roc_auc has to be the
  # Hand-Till one-versus-all summary, because that is what sdt_roc() draws
  # one curve per class of; a binary or macro estimator here would mean the
  # plot and the number are describing different things.
  expect_identical(kind[["roc_auc"]], "hand_till")
  expect_identical(kind[["accuracy"]], "multiclass")
  expect_identical(kind[["kap"]], "multiclass")
  expect_identical(kind[["f_meas"]], "macro")
})


# Tuning ----------------------------------------------------------------------

test_that("tuning stores what it searched and reports what it is doing", {
  skip_unless_bench("rpart")
  # A cohort this small leaves levels out of a fold, and tune prints that
  # catalogue straight to stderr rather than raising a condition, so it is
  # diverted to keep the check log readable. The package's own progress is a
  # message, and is captured separately.
  msgs <- character()
  utils::capture.output(
    msgs <- testthat::capture_messages(
      fit <- sdt_fit(bench_cohort(n = 30), target = "called_triage",
                     engine = "tree", tune = TRUE, grid = 2L, folds = 2L,
                     seed = 1, verbose = TRUE)
    ),
    type = "message"
  )
  joined <- paste(msgs, collapse = " ")
  expect_match(joined, "Tuning")
  expect_match(joined, "Fitting")

  # The grid and the winner are kept so a reported score can be traced back to
  # the candidate that produced it.
  expect_s3_class(fit$tuning, "tune_results")
  expect_identical(nrow(fit$best), 1L)
  expect_contains(names(fit$best), c("cost_complexity", "min_n"))
  # The workflow that was finally fitted must be the finalised one; a leftover
  # tune() marker here means finalize_workflow() was skipped.
  expect_identical(
    nrow(hardhat::extract_parameter_set_dials(fit$workflow)), 0L
  )
  expect_identical(nrow(sdt_metrics(fit)), 4L)
})


# Shape guarantees ------------------------------------------------------------

test_that("predict() keeps its shape down to a single row", {
  skip_unless_bench("rpart")
  fit <- fixed_fit()
  newdata <- bench_cohort(n = 12, seed = 11)
  many <- predict(fit, newdata)
  one <- predict(fit, newdata[1L, ])
  # Downstream code binds these together, so a single row must not come back
  # as a bare factor or drop to a vector.
  expect_s3_class(one, "tbl_df")
  expect_identical(names(one), names(many))
  expect_identical(nrow(one), 1L)
  expect_identical(nrow(many), 12L)
})

test_that("predict() returns one probability column per class", {
  skip_unless_bench("rpart")
  fit <- fixed_fit()
  probs <- predict(fit, bench_cohort(n = 12, seed = 11), type = "prob")
  expect_s3_class(probs, "tbl_df")
  expect_identical(nrow(probs), 12L)
  # One column per class of the target, named and ordered by the target's own
  # levels. Downstream code reads these by name and binds them by position,
  # so a dropped class or a reordering is not a cosmetic difference.
  expect_identical(
    names(probs), paste0(".pred_", levels(sdt_predictions(fit)$truth))
  )
  expect_equal(rowSums(as.matrix(probs)), rep(1, 12L), tolerance = 1e-6)
})

test_that("sdt_predictions() and sdt_metrics() describe the same rows", {
  skip_unless_bench("rpart")
  fit <- fixed_fit()
  preds <- sdt_predictions(fit)
  # The metrics are the held-out ones, so the predictions have to be the test
  # half of the split rather than the training data they were fitted on.
  expect_identical(nrow(preds), nrow(rsample::testing(fit$split)))
  expect_false(fit$target %in% names(preds))
  expect_s3_class(preds$truth, "factor")
  expect_named(sdt_metrics(fit), c(".metric", ".estimator", ".estimate"))
})

test_that("sdt_roc() draws one curve per class and names the model", {
  skip_unless_bench("rpart")
  # The Hand-Till roc_auc the bench reports is a one-versus-all summary, so a
  # single curve here would mean the plot is not showing what the number says.
  fit <- fixed_fit()
  g <- sdt_roc(fit)
  expect_s3_class(g, "ggplot")
  expect_true(".level" %in% names(g$data))
  expect_setequal(
    as.character(unique(g$data$.level)), levels(sdt_predictions(fit)$truth)
  )
  # Both axes are rates, so a point outside the unit square would mean the
  # curve is not what the axis labels claim.
  coords <- c(g$data$sensitivity, g$data$specificity)
  expect_gte(min(coords), 0)
  expect_lte(max(coords), 1)
  expect_identical(
    g$labels$subtitle, "tree (glass-box) predicting called_triage"
  )
})

test_that("sdt_roc() labels the empty case instead of erroring", {
  skip_unless_bench("rpart")
  # A ROC curve needs class probabilities, and a model whose predictions carry
  # only a hard call has none. The workbench draws this into a panel, so the
  # honest answer is a titled blank rather than an error the user cannot act
  # on.
  local_mocked_bindings(
    sdt_predictions = function(model) {
      tibble::tibble(truth = factor("red"), .pred_class = factor("red"))
    },
    .package = "sandtablR"
  )
  model <- structure(
    list(engine = "tree", class_box = "glass", target = "called_triage"),
    class = "sdt_model"
  )
  g <- sdt_roc(model)
  expect_s3_class(g, "ggplot")
  expect_match(g$labels$title, "No predicted probabilities")
})

test_that("print()ing a model writes its format() lines and returns it", {
  skip_unless_bench("rpart")
  fit <- fixed_fit()
  # print() has to return its argument invisibly, or piping a model through it
  # to inspect a fit mid-chain drops the model.
  lines <- utils::capture.output(res <- withVisible(print(fit)))
  expect_match(lines[[1L]], "sdt_model", fixed = TRUE)
  expect_false(res$visible)
  expect_identical(res$value, fit)
})


# Agreement -------------------------------------------------------------------

test_that("sdt_consistency_score() honours an explicit level set", {
  skip_if_not_installed("yardstick")
  truth <- c("red", "red", "yellow", "yellow")
  called <- c("red", "yellow", "yellow", "yellow")
  # Naming a level nobody used is allowed: the level set is what the raters
  # could have said, not what they did say.
  wide <- sdt_consistency_score(
    truth, called, levels = c("green", "red", "yellow")
  )
  expect_identical(nrow(wide), 1L)
  expect_identical(wide$n, 4L)
  expect_false(is.na(wide$.estimate))

  # One level is the degenerate case: two raters restricted to a single label
  # agree completely and share no judgement, so kappa is undefined and
  # reporting 1 would be exactly the wrong answer.
  forced <- sdt_consistency_score(truth, called, levels = "red")
  expect_true(is.na(forced$.estimate))
  expect_identical(forced$n, 4L)
})

test_that("sdt_consistency_score() scores a model against a repeated call", {
  skip_unless_bench("rpart")
  fit <- fixed_fit()
  newdata <- bench_cohort(n = 40, seed = 21)
  called <- predict(fit, newdata)$.pred_class
  out <- sdt_consistency_score(newdata$called_triage, called)
  expect_identical(nrow(out), 1L)
  expect_identical(out$n, 40L)
  expect_identical(out$.metric, "kap")
  # Both labellings use all three levels here, so kappa is defined and scored
  # across all of them; an NA or a binary estimator would mean the level set
  # collapsed on the way in.
  expect_identical(out$.estimator, "multiclass")
  expect_false(is.na(out$.estimate))
  expect_gte(out$.estimate, -1)
  expect_lte(out$.estimate, 1)

  # predict() hands back a factor and the cohort column is character. Kappa is
  # over labels, so which of the two you pass must not change the answer: a
  # comparison that fell through to the factor's integer codes would quietly
  # be scoring something else.
  expect_identical(
    out, sdt_consistency_score(newdata$called_triage, as.character(called))
  )
  # The default level set is documented as the observed union, so naming that
  # union explicitly has to leave the score where it was.
  observed <- sort(union(unique(newdata$called_triage),
                         unique(as.character(called))))
  expect_identical(
    out,
    sdt_consistency_score(newdata$called_triage, called, levels = observed)
  )
})


# The bench -------------------------------------------------------------------

test_that("sdt_workflowset() screens exactly the engines it is given", {
  skip_unless_bench("rpart", "glmnet")
  ws <- sdt_workflowset(bench_cohort(), target = "called_triage",
                        engines = c("tree", "glmnet"), tune = FALSE,
                        folds = 2L, prop = 0.7, seed = 1, verbose = FALSE)
  expect_setequal(unique(ws$engine), c("tree", "glmnet"))
  expect_identical(nrow(ws), 8L)
  expect_type(ws$engine, "character")
  expect_setequal(ws$class_box, "glass")
  models <- attr(ws, "models")
  expect_named(models, c("tree", "glmnet"))
  expect_s3_class(models$glmnet, "sdt_model")
  expect_identical(attr(ws, "skipped"), character())
})

test_that("sdt_workflowset() names the engines whose package is absent", {
  skip_unless_bench("rpart")
  # Engines with no package must be reported, not silently dropped: a
  # leaderboard missing a row is otherwise indistinguishable from one where
  # that engine simply lost.
  local_mocked_bindings(.sdt_engines = absent_engines, .package = "sandtablR")
  msgs <- character()
  expect_warning(
    msgs <- testthat::capture_messages(
      ws <- sdt_workflowset(bench_cohort(), target = "called_triage",
                            engines = c("tree", "ranger"), tune = FALSE,
                            folds = 2L, seed = 1, verbose = TRUE)
    ),
    "no package installed"
  )
  # The engines it did screen are named as it goes, so a long screen is not a
  # silent wait.
  expect_match(paste(msgs, collapse = " "), "Screening")
  expect_identical(attr(ws, "skipped"), "ranger")
  expect_named(attr(ws, "models"), "tree")
  expect_setequal(unique(ws$engine), "tree")
})

test_that("sdt_workflowset() skips a transformer with no working backend", {
  skip_unless_bench("rpart")
  # keras3 can be installed with no Python backend behind it, and the
  # transformer fit then aborts. That must cost the board one row, not every
  # row. The transformer is pointed at a package every machine has, so it
  # counts as installed whether or not keras3 is.
  keras_engines <- function() {
    tibble::tibble(
      engine = c("tree", "transformer"),
      package = c("rpart", "stats"),
      class_box = c("glass", "closed")
    )
  }
  local_mocked_bindings(
    .sdt_engines = keras_engines,
    .sdt_keras_ready = function() FALSE,
    .package = "sandtablR"
  )
  expect_warning(
    ws <- sdt_workflowset(bench_cohort(), target = "called_triage",
                          engines = c("tree", "transformer"), tune = FALSE,
                          folds = 2L, seed = 1, verbose = TRUE),
    "no working backend"
  )
  expect_identical(attr(ws, "skipped"), "transformer")
  expect_named(attr(ws, "models"), "tree")
  expect_setequal(unique(ws$engine), "tree")
})

test_that("sdt_workflowset() returns an empty leaderboard, not an error", {
  # No skip: with nothing usable it returns before it fits anything, so this
  # is exactly the path a machine with no engines installed takes.
  local_mocked_bindings(.sdt_engines = absent_engines, .package = "sandtablR")
  expect_warning(
    ws <- sdt_workflowset(bench_cohort(n = 20), target = "called_triage",
                          engines = c("ranger", "xgboost"), tune = FALSE,
                          folds = 2L, seed = 1, verbose = TRUE),
    "no package installed"
  )
  # The columns are promised by the documentation, so callers can bind and
  # filter the result whether or not anything was fitted.
  expect_named(ws, c("engine", "class_box", ".metric", ".estimator",
                     ".estimate"))
  expect_identical(nrow(ws), 0L)
  expect_setequal(attr(ws, "skipped"), c("ranger", "xgboost"))
  expect_identical(attr(ws, "models"), list())
})

test_that("sdt_workflowset() counts the unknown engines it was handed", {
  expect_error(
    sdt_workflowset(bench_cohort(n = 20), engines = c("telepathy", "runes"),
                    verbose = FALSE),
    "2 unknown engines"
  )
})


# Validation ------------------------------------------------------------------

test_that("sdt_fit() rejects arguments that are not what they claim", {
  skip_unless_bench("rpart")
  co <- bench_cohort(n = 30)
  expect_error(sdt_fit(co, tune = "yes"), "must be .TRUE. or .FALSE.")
  expect_error(sdt_fit(co, verbose = NA), "must be .TRUE. or .FALSE.")
  expect_error(sdt_fit(co, grid = 0), "positive whole number")
  expect_error(sdt_fit(co, grid = 2.5), "positive whole number")
  expect_error(sdt_fit(co, folds = c(2, 3)), "positive whole number")
  expect_error(sdt_fit(co, prop = NA), "strictly in")
  expect_error(sdt_fit(co, prop = c(0.5, 0.6)), "strictly in")
  expect_error(
    sdt_fit(co, target = "called_triage", tune = FALSE, seed = -1,
            verbose = FALSE),
    "positive whole number"
  )
})

test_that("sdt_fit() refuses data that cannot answer the question", {
  skip_unless_bench("rpart")
  # A cohort with no decisions recorded has no participant call to predict;
  # saying so beats fitting a model on a column that is not there.
  plain <- sdt_synthetic_cohort(n = 30, seed = 3)
  expect_error(
    sdt_fit(plain, target = "called_triage", tune = FALSE, verbose = FALSE),
    "has no called_triage column"
  )
  expect_error(
    sdt_fit(bench_cohort(n = 30), target = "called_triage",
            predictors = character(), tune = FALSE, verbose = FALSE),
    "No predictors left"
  )
  expect_error(
    sdt_fit(bench_cohort(n = 30), target = "called_triage",
            predictors = c("age", "nonesuch", "alsonot"), tune = FALSE,
            verbose = FALSE),
    "missing 2 predictors"
  )
})

test_that("the reader functions refuse anything but an sdt_model", {
  # No skip: all three check the class before they ask for a package, so the
  # message a user gets for the wrong argument is testable on a bare install.
  for (f in list(sdt_metrics, sdt_predictions, sdt_roc)) {
    expect_error(f(list(engine = "tree")), "sdt_model")
  }
})

test_that("a metrics table with no scores says what actually failed", {
  # When every fit in a resample errors, tune returns a table with no .metric
  # column at all. Surfacing that as "column not found" would hide the real
  # cause, which tune recorded in its notes.
  expect_error(.tidy_metrics(tibble::tibble(x = 1)), "every fit failed")
  expect_error(.tidy_metrics(tibble::tibble(x = 1)), "show_notes")
  noted <- tibble::tibble(
    .notes = list(tibble::tibble(note = "the engine went bang"))
  )
  expect_error(.tidy_metrics(tibble::tibble(x = 1), fit = noted),
               "the engine went bang")
  # An empty note table has to fall back rather than index out of bounds.
  empty <- tibble::tibble(.notes = list(tibble::tibble()))
  expect_error(.tidy_metrics(tibble::tibble(x = 1), fit = empty),
               "show_notes")
})

test_that("resampled metrics are folded onto the same three columns", {
  # collect_metrics() on resamples names the column `mean`, not `.estimate`;
  # every metric table the package hands out has to look the same whatever
  # produced it.
  out <- .tidy_metrics(tibble::tibble(
    .metric = c("roc_auc", "accuracy"),
    .estimator = c("hand_till", "multiclass"),
    mean = c(0.8, 0.6)
  ))
  expect_named(out, c(".metric", ".estimator", ".estimate"))
  expect_identical(out$.metric, c("accuracy", "roc_auc"))
  expect_identical(out$.estimate, c(0.6, 0.8))
})


# Reference distributions -----------------------------------------------------

test_that(".sdt_report_area() folds every board area onto a location", {
  # summary.game_run() reports against the published end-of-game table, so an
  # area with no mapping would appear as a location the paper never names.
  areas <- sdt_board_areas()
  folded <- .sdt_report_area(areas)
  expect_length(folded, length(areas))
  expect_null(names(folded))
  expect_contains(.sdt_end_state_areas(), folded)
})

test_that(".sdt_report_area() passes an unmapped label through unchanged", {
  # The mapping is a fold, not a filter: anything it does not know has to
  # survive so an unexpected area is visible rather than turned into NA.
  expect_identical(.sdt_report_area("nowhere"), "nowhere")
  # "ed_red" is one of the two board areas the paper reports as a single "ed",
  # so this asserts the fold and the pass-through in the same call.
  expect_identical(.sdt_report_area(c("ed_red", "nowhere")),
                   c("ed", "nowhere"))
})

test_that("sdt_reference_scenario() honours the seed it is given", {
  a <- sdt_reference_scenario(seed = 11)
  b <- sdt_reference_scenario(seed = 11)
  c_other <- sdt_reference_scenario(seed = 12)
  expect_identical(a$cohort, b$cohort)
  expect_false(identical(a$cohort, c_other$cohort))
  # The seed is recorded, so a board someone shows you can be rebuilt.
  expect_identical(a$meta$seed, 11L)
  expect_identical(a$meta$name, "reference")
})

test_that("sdt_reference_scenario() defaults to the calibration settings", {
  cal <- sdt_reference("defaults")$calibration
  sc <- sdt_reference_scenario()
  expect_identical(sc$meta$seed, as.integer(cal$seed))
  expect_identical(nrow(sc$cohort), as.integer(cal$n))
  for (area in names(cal$resources)) {
    expect_identical(sc$resources[[area]], as.integer(cal$resources[[area]]),
                     info = area)
  }
})

test_that("sdt_reference_horizon() reports the calibrated game length", {
  horizon <- sdt_reference_horizon()
  # The accessor exists so callers do not reach into the reference list by
  # hand, which only helps if it serves the calibration horizon and not some
  # neighbouring number from the same block.
  expect_identical(horizon, sdt_reference("defaults")$calibration$horizon)
  expect_length(horizon, 1L)
  expect_type(horizon, "double")
  expect_gt(horizon, 0)
})

test_that("the reference is read once and served from the cache after", {
  saved <- .sdt_cache$reference
  withr::defer(assign("reference", saved, envir = .sdt_cache))
  rm(list = "reference", envir = .sdt_cache)

  first <- .tdsc_reference()
  expect_true(exists("reference", envir = .sdt_cache, inherits = FALSE))
  # Poking a sentinel in proves the second call reads the cache rather than
  # the file again, which a plain identical() check cannot distinguish.
  .sdt_cache$reference <- list(sentinel = TRUE)
  expect_identical(.tdsc_reference(), list(sentinel = TRUE))
  expect_true(is.list(first))
})
