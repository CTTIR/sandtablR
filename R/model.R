# Machine learning workbench --------------------------------------------------
# Orchestration over tidymodels. Every engine lives in Suggests and is checked
# at the point of use, so the package installs and checks with none of them
# present.
#
# The bench spans "glass-box" models you can read (penalised multinomial
# regression, a single decision tree) and "black-box" models you cannot
# (random forest, gradient boosting). Both are trained through one interface
# and scored on the same held-out split, because the only honest way to say
# what interpretability costs is to measure it.

# Which engines exist, what package each needs, and how readable it is.
#
# Readability is graded rather than binary, because the usual glass/black split
# puts gradient boosting in the same bucket as a model nothing can be recovered
# from, and that is not what it is. The grade is defined by where an
# explanation comes from, which is checkable, rather than by how interpretable
# the model feels:
#
#   glass  The fitted model can be printed and read. A glmnet fit is its
#          coefficients; an rpart fit is its rules. There is nothing to
#          explain it with, because it is the explanation.
#   grey   The model cannot be read whole, but its own structure yields
#          per-feature attributions without perturbing anything: boosted trees
#          are shallow and additive, so a prediction decomposes exactly.
#   black  Attributions come only from perturbing the inputs and watching the
#          output. A deep bagged forest is here not because trees are
#          unknowable but because this package reaches it only through the
#          model-agnostic path.
#
# The grade is a judgement about this package's explanation route, not a fact
# about the algorithm. Adding an exact tree explainer for ranger would move it
# to grey, and that would be a change to make deliberately.
.sdt_engines <- function() {
  tibble::tibble(
    # Ordered by readability -- glass, then grey, then black -- because the
    # workbench presents engines in that order and reads it from here rather
    # than restating it. `tree` leads the glass tier deliberately: it is the
    # cheapest to fit and so the right default for an interactive picker.
    engine = c("tree", "multinom", "glmnet", "mars",
               "xgboost", "ranger", "svm_rbf", "mlp", "transformer"),
    package = c("rpart", "nnet", "glmnet", "earth",
                "xgboost", "ranger", "kernlab", "nnet", "keras3"),
    class_box = c("glass", "glass", "glass", "grey",
                  "grey", "black", "black", "black", "closed")
  )
}

# The grades, from most readable to least. Ordered, so a leaderboard or a
# legend can sort by readability rather than alphabetically.
#
# "closed" is not a synonym for "black". A black-box model can still be
# explained from the outside: permute an input, watch the prediction move, and
# the attribution means something because the input is a column a clinician
# recognises. A closed-box model learns its own internal representation first,
# so a per-column attribution is an attribution to something the model no
# longer reasons in. The distinction is about whether an explanation is
# FAITHFUL, not about whether one can be produced - DALEX will happily produce
# one either way, and that is exactly the trap.
.sdt_box_levels <- function() c("glass", "grey", "black", "closed")

#' Model engines available to the workbench
#'
#' Lists the supported engines, the package each needs, whether that package
#' is installed, and how readable the fitted model is.
#'
#' `class_box` grades readability by where an explanation comes from:
#' `"glass"` when the fitted model can simply be read, `"grey"` when its own
#' structure yields exact per-feature attributions, `"black"` when attributions
#' come only from perturbing the inputs, and `"closed"` when the model learns
#' its own representation first, so a per-input attribution no longer describes
#' what the model reasons in. Gradient boosting sits in the middle rather than
#' with the black boxes, which is where a two-way split would put it.
#'
#' @return A [tibble::tibble()] with columns `engine`, `package`, `class_box`
#'   and `installed`.
#' @export
#' @examples
#' sdt_engines()
sdt_engines <- function() {
  out <- .sdt_engines()
  out$installed <- vapply(
    out$package, requireNamespace, logical(1), quietly = TRUE
  )
  out
}

.box_of <- function(engine) {
  spec <- .sdt_engines()
  spec$class_box[match(engine, spec$engine)]
}

.engine_pkg <- function(engine) {
  spec <- .sdt_engines()
  spec$package[match(engine, spec$engine)]
}

# Model specifications, in a tunable and a fixed variant.
#
# The `tune::tune()` calls have to be written out literally. parsnip decides
# which arguments are tunable by inspecting the *expression* each was given,
# so a marker passed through a helper or a variable evaluates to the same
# object but is not recognised: the parameter set comes back empty, no grid is
# built, and the placeholder reaches the engine, which fails with "comparison
# (>) is not possible for language types". Hence the duplication below.
.model_spec <- function(engine, tune = TRUE) {
  spec <- if (tune) .spec_tuned(engine) else .spec_fixed(engine)
  parsnip::set_mode(spec, "classification")
}

.spec_tuned <- function(engine) {
  switch(engine,
    ranger = parsnip::set_engine(
      parsnip::rand_forest(
        trees = 500L, mtry = tune::tune(), min_n = tune::tune()
      ),
      "ranger", importance = "permutation"
    ),
    xgboost = parsnip::set_engine(
      parsnip::boost_tree(
        trees = 300L, tree_depth = tune::tune(), learn_rate = tune::tune()
      ),
      "xgboost"
    ),
    glmnet = parsnip::set_engine(
      parsnip::multinom_reg(penalty = tune::tune(), mixture = tune::tune()),
      "glmnet"
    ),
    tree = parsnip::set_engine(
      parsnip::decision_tree(
        cost_complexity = tune::tune(), min_n = tune::tune()
      ),
      "rpart"
    ),
    # Unpenalised multinomial logistic. nnet maps `penalty` onto its weight
    # decay and has no elastic-net mixture, so only penalty is tuned.
    multinom = parsnip::set_engine(
      parsnip::multinom_reg(penalty = tune::tune()), "nnet", trace = FALSE
    ),
    mars = parsnip::set_engine(
      parsnip::mars(num_terms = tune::tune(), prod_degree = tune::tune()),
      "earth"
    ),
    svm_rbf = parsnip::set_engine(
      parsnip::svm_rbf(cost = tune::tune(), rbf_sigma = tune::tune()),
      "kernlab"
    ),
    mlp = parsnip::set_engine(
      parsnip::mlp(hidden_units = tune::tune(), penalty = tune::tune(),
                   epochs = 200L),
      "nnet", trace = FALSE
    ),
    transformer = sdt_transformer(d_model = 16L, num_heads = 2L, epochs = 30L)
  )
}

.spec_fixed <- function(engine) {
  switch(engine,
    ranger = parsnip::set_engine(
      parsnip::rand_forest(trees = 500L, min_n = 5L),
      "ranger", importance = "permutation"
    ),
    xgboost = parsnip::set_engine(
      parsnip::boost_tree(trees = 300L, tree_depth = 6L, learn_rate = 0.1),
      "xgboost"
    ),
    glmnet = parsnip::set_engine(
      parsnip::multinom_reg(penalty = 0.01, mixture = 1),
      "glmnet"
    ),
    tree = parsnip::set_engine(
      parsnip::decision_tree(cost_complexity = 0.01, min_n = 5L),
      "rpart"
    ),
    multinom = parsnip::set_engine(
      parsnip::multinom_reg(penalty = 0), "nnet", trace = FALSE
    ),
    mars = parsnip::set_engine(
      parsnip::mars(num_terms = 10L, prod_degree = 1L), "earth"
    ),
    svm_rbf = parsnip::set_engine(
      parsnip::svm_rbf(cost = 1, rbf_sigma = 0.1), "kernlab"
    ),
    mlp = parsnip::set_engine(
      parsnip::mlp(hidden_units = 8L, penalty = 0.01, epochs = 200L),
      "nnet", trace = FALSE
    ),
    transformer = sdt_transformer(d_model = 16L, num_heads = 2L, epochs = 30L)
  )
}

# The shared preprocessing pipeline. Median imputation before dummies so a
# missing numeric does not silently drop a row; normalisation last so the
# penalised models see comparable scales.
#
# The logical step is not optional. `ventilated` is a logical column, and
# recipes counts logicals as neither nominal nor numeric, so it slips past
# both all_nominal_predictors() and all_numeric_predictors() and arrives at
# glmnet and xgboost as a column they cannot put in a numeric matrix.
.model_recipe <- function(data, target, predictors) {
  form <- stats::as.formula(
    paste(target, "~", paste(predictors, collapse = " + "))
  )
  rec <- recipes::recipe(form, data = data)
  rec <- recipes::step_mutate_at(
    rec, recipes::all_logical_predictors(), fn = as.integer
  )
  rec <- recipes::step_impute_median(rec, recipes::all_numeric_predictors())
  rec <- recipes::step_unknown(rec, recipes::all_nominal_predictors())
  rec <- recipes::step_dummy(rec, recipes::all_nominal_predictors())
  rec <- recipes::step_zv(rec, recipes::all_predictors())
  recipes::step_normalize(rec, recipes::all_numeric_predictors())
}

#' The metric set the workbench scores on
#'
#' Area under the ROC curve, accuracy, Cohen's kappa and macro F. Kappa is
#' included deliberately: on a cohort that is 44% red, accuracy alone rewards a
#' model for guessing the majority class.
#'
#' @return A \pkg{yardstick} metric set.
#' @export
#' @examples
#' if (requireNamespace("yardstick", quietly = TRUE)) {
#'   sdt_metric_set()
#' }
sdt_metric_set <- function() {
  .require_pkgs("yardstick")
  yardstick::metric_set(
    yardstick::roc_auc, yardstick::accuracy,
    yardstick::kap, yardstick::f_meas
  )
}

# Everything that is an outcome, or identifies a row, is kept out of the
# predictors by default. That includes the *other* outcome columns: a model
# for the participant's call must not be handed the true category, and a model
# for triage must not be handed therapy.
.sdt_outcomes <- function() {
  c("triage", "therapy", "called_triage", "called_therapy")
}

.default_predictors <- function(data, target) {
  setdiff(names(data), c("id", "arrival_order", .sdt_outcomes(), target))
}

#' Fit a triage or therapy classification model
#'
#' Trains a supervised classifier predicting either the sighting category or
#' the therapy concept of a casualty from its card, through a full
#' \pkg{tidymodels} path: a preprocessing recipe, stratified cross-validation,
#' a tuning grid, and a final fit evaluated on a held-out split.
#'
#' The returned object keeps the split, the resamples and the tuning results,
#' not only the fitted workflow, so the numbers reported later can be traced
#' back to what produced them.
#'
#' @param data A cohort or data frame of casualties.
#' @param target Outcome to predict. `"triage"` and `"therapy"` are the true
#'   values on the card; `"called_triage"` and `"called_therapy"` are what a
#'   participant assigned, as produced by [sdt_add_decisions()]. The true
#'   values are close to a deterministic function of the card, so every engine
#'   scores near 1 on them and the comparison is uninformative. The
#'   participant's call is the target worth benchmarking.
#' @param engine One of [sdt_engines()]. `"multinom"`, `"glmnet"` and `"tree"`
#'   are glass-box; `"mars"` and `"xgboost"` are grey-box; `"ranger"`,
#'   `"svm_rbf"` and `"mlp"` are black-box.
#' @param predictors Character vector of predictor columns. Defaults to the
#'   card fields, excluding identifiers and every outcome column, so a model
#'   for the participant's call cannot simply read the true category.
#' @param tune If `TRUE`, tune hyperparameters over a grid; if `FALSE`, fit the
#'   specification's defaults once.
#' @param grid Number of candidate hyperparameter settings.
#' @param folds Number of cross-validation folds, stratified on the target.
#' @param prop Proportion of the data used for training in the initial split.
#' @param parallel If `TRUE`, fit resamples and grid points in parallel.
#'   Defaults to `FALSE`: a package that claims every core without being asked
#'   is a bad guest on a shared machine, and a sequential run needs no thought
#'   about RNG streams. It pays off on a tuned fit or an engine screen and
#'   costs a little on a single untuned one. The backend is set up for the
#'   duration of the call and the caller's plan is restored on exit.
#' @param workers Number of parallel workers, or `NULL` to choose one: a core
#'   short of the machine, capped at 8.
#' @param seed Optional integer seed, applied with [withr::local_seed()].
#' @param verbose If `TRUE`, report progress via \pkg{cli}.
#'
#' @return An S3 object of class `sdt_model` with elements `workflow`,
#'   `engine`, `target`, `predictors`, `class_box`, `split`, `resamples`,
#'   `tuning`, `best` and `metrics`.
#' @seealso [sdt_workflowset()] to screen every engine at once,
#'   [sdt_metrics()], [sdt_roc()], [sdt_explain()].
#' @export
#' @examples
#' co <- sdt_synthetic_cohort(n = 200, seed = 3)
#' needed <- c("parsnip", "recipes", "workflows", "rsample", "tune",
#'             "yardstick", "rpart")
#' if (all(vapply(needed, requireNamespace, logical(1), quietly = TRUE))) {
#'   fit <- sdt_fit(co, target = "triage", engine = "tree",
#'                  tune = FALSE, seed = 1, verbose = FALSE)
#'   fit
#'   sdt_metrics(fit)
#' }
sdt_fit <- function(data,
                    target = c("triage", "therapy",
                               "called_triage", "called_therapy"),
                    engine = NULL,
                    predictors = NULL,
                    tune = TRUE,
                    grid = 5L,
                    folds = 5L,
                    prop = 0.75,
                    parallel = FALSE,
                    workers = NULL,
                    seed = NULL,
                    verbose = TRUE) {
  target <- rlang::arg_match(target)
  .check_flag(parallel)
  # tune dispatches resampling and grid search through the future framework,
  # so establishing a plan is all that is needed to parallelise both. Scoped
  # to this call: the caller's plan is restored on exit.
  .sdt_parallel_plan(parallel, workers)
  # Validated against the registry rather than a vector written out here: an
  # engine added to .sdt_engines() becomes fittable without a second edit in
  # this signature, which is how the original four fell out of step with it.
  # The DEFAULT stays "ranger" -- it is what every existing caller that omits
  # `engine` already gets, and the registry is ordered by readability, not by
  # what should be the default.
  engine <- if (is.null(engine)) "ranger" else engine
  engine <- rlang::arg_match(engine, .sdt_engines()$engine)
  .check_flag(tune)
  .check_flag(verbose)
  grid <- .check_count(grid)
  folds <- .check_count(folds)
  if (!is.numeric(prop) || length(prop) != 1L || is.na(prop) ||
        prop <= 0 || prop >= 1) {
    cli::cli_abort("{.arg prop} must be a single number strictly in (0, 1).")
  }
  .require_pkgs(c("parsnip", "recipes", "workflows", "rsample", "tune",
                  "yardstick", "dials", "hardhat"))
  .require_pkgs(.engine_pkg(engine))
  if (!is.null(seed)) {
    withr::local_seed(.check_count(seed))
  }

  data <- tibble::as_tibble(data)
  predictors <- predictors %||% .default_predictors(data, target)
  .check_model_data(data, target, predictors)
  data[[target]] <- factor(data[[target]])

  split <- rsample::initial_split(data, prop = prop, strata = !!target)
  train <- rsample::training(split)
  resamples <- rsample::vfold_cv(train, v = folds, strata = !!target)

  wf <- workflows::workflow()
  wf <- workflows::add_recipe(wf, .model_recipe(train, target, predictors))
  wf <- workflows::add_model(wf, .model_spec(engine, tune = tune))
  metrics <- sdt_metric_set()

  tuning <- NULL
  best <- NULL
  if (tune) {
    if (verbose) {
      cli::cli_inform("Tuning {.val {engine}} over {grid} candidate{?s}.")
    }
    tuning <- .tune_workflow(wf, resamples, train, grid, metrics)
    best <- tune::select_best(tuning, metric = "roc_auc")
    wf <- tune::finalize_workflow(wf, best)
  }

  if (verbose) cli::cli_inform("Fitting {.val {engine}} for {.field {target}}.")
  final <- tune::last_fit(wf, split, metrics = metrics)
  scored <- .score_last_fit(final, target, metrics)

  structure(
    list(
      workflow = tune::extract_workflow(final),
      engine = engine,
      target = target,
      predictors = predictors,
      class_box = .box_of(engine),
      split = split,
      resamples = resamples,
      tuning = tuning,
      best = best,
      last_fit = final,
      metrics = .tidy_metrics(scored, fit = final)
    ),
    class = "sdt_model"
  )
}

# Hard classes from the probabilities, not from the engine ---------------------
#
# WHY THIS EXISTS. \pkg{parsnip}'s `mars` classification prediction is written
# for two classes and only two: its post-processor takes the first response
# column, thresholds it at 0.5, and chooses between `lvl[1]` and `lvl[2]`
# (`parsnip::get_model_env()$mars_predict`). Handed a three-class target it can
# never return the third level and it permutes the other two. The symptom is
# distinctive and was sitting in this package's own leaderboard: accuracy
# 0.1499 and kappa **-0.3948** beside an unremarkable AUC of 0.8289. The
# probabilities were right the whole time; only the hard class was wrong.
#
# The repair is to take the class from the probabilities, because for a
# probabilistic classifier that is what the hard class IS. It is applied to
# every engine rather than to `mars` alone: for the other eight it reproduces
# what parsnip already returns, so it costs nothing, and there is no
# engine-specific branch to go stale when parsnip fixes this upstream.
.class_from_prob <- function(preds, lev = NULL) {
  lev <- lev %||% sub("^\\.pred_", "",
                      setdiff(grep("^\\.pred_", names(preds), value = TRUE),
                              ".pred_class"))
  cols <- paste0(".pred_", lev)
  if (length(lev) < 2L || !all(cols %in% names(preds))) {
    return(preds)
  }
  m <- as.matrix(preds[, cols, drop = FALSE])
  preds$.pred_class <- factor(lev[max.col(m, ties.method = "first")],
                              levels = lev)
  preds
}

# The held-out metrics, recomputed from the repaired predictions rather than
# collected from `last_fit()`. Same metric set, same rows; the only difference
# is which `.pred_class` the class metrics see.
.score_last_fit <- function(final, target, metrics) {
  preds <- tibble::as_tibble(tune::collect_predictions(final))
  if (!target %in% names(preds)) {
    return(tune::collect_metrics(final))
  }
  lev <- levels(preds[[target]])
  preds <- .class_from_prob(preds, lev)
  cols <- rlang::syms(paste0(".pred_", lev))
  metrics(preds, truth = !!rlang::sym(target), !!!cols,
          estimate = .data$.pred_class)
}

# Building the grid separately is what makes mtry work: its upper bound is the
# number of predictors, which is not known until the recipe has been applied.
.tune_workflow <- function(wf, resamples, train, grid, metrics,
                           call = rlang::caller_env()) {
  params <- hardhat::extract_parameter_set_dials(wf)
  if (!nrow(params)) {
    cli::cli_abort(
      c(
        "The model specification exposes no tunable parameters.",
        "i" = "Fit it with {.code tune = FALSE} instead."
      ),
      call = call
    )
  }
  params <- dials::finalize(params, x = train)
  suppressWarnings(tune::tune_grid(
    wf,
    resamples = resamples,
    grid = grid,
    param_info = params,
    metrics = metrics,
    control = tune::control_grid(save_pred = FALSE, verbose = FALSE)
  ))
}

.check_model_data <- function(data, target, predictors,
                              call = rlang::caller_env()) {
  if (!target %in% names(data)) {
    cli::cli_abort("{.arg data} has no {.field {target}} column.", call = call)
  }
  missing <- setdiff(predictors, names(data))
  if (length(missing)) {
    n_missing <- length(missing)
    cli::cli_abort(
      c(
        "{.arg data} is missing {n_missing} predictor{?s}.",
        "x" = "Missing: {.field {missing}}."
      ),
      call = call
    )
  }
  if (!length(predictors)) {
    cli::cli_abort("No predictors left after excluding identifiers.",
                   call = call)
  }
  n_classes <- length(unique(stats::na.omit(data[[target]])))
  if (n_classes < 2L) {
    cli::cli_abort(
      c(
        "{.field {target}} must have at least two classes to classify.",
        "x" = "Found {n_classes}."
      ),
      call = call
    )
  }
  invisible(data)
}

# One shape for every metric table the package returns, whatever produced it.
# When every model in a resample fails, tune returns a table with no .metric
# column at all; surfacing that as "column not found" would hide the actual
# problem, which tune recorded in its notes.
.tidy_metrics <- function(x, fit = NULL, call = rlang::caller_env()) {
  out <- tibble::as_tibble(x)
  if (!".metric" %in% names(out)) {
    cli::cli_abort(
      c(
        "The model produced no metrics: every fit failed.",
        "i" = .fit_notes(fit)
      ),
      call = call
    )
  }
  keep <- intersect(c(".metric", ".estimator", ".estimate"), names(out))
  out <- out[, keep, drop = FALSE]
  if (!".estimate" %in% names(out) && "mean" %in% names(x)) {
    out$.estimate <- x$mean
  }
  dplyr::arrange(out, .data$.metric)
}

# Pull the first note tune recorded, so the error says what actually broke.
.fit_notes <- function(fit) {
  fallback <- "Run {.code tune::show_notes(.Last.tune.result)} for details."
  if (is.null(fit) || !".notes" %in% names(fit)) {
    return(fallback)
  }
  notes <- tryCatch(dplyr::bind_rows(fit$.notes), error = function(e) NULL)
  if (is.null(notes) || !nrow(notes) || !"note" %in% names(notes)) {
    return(fallback)
  }
  unique(notes$note)[[1L]]
}

#' Metrics for a fitted model
#'
#' Returns the held-out performance of an [sdt_fit()] model, always with the
#' same three columns whatever engine produced it.
#'
#' @param model An `sdt_model` from [sdt_fit()].
#' @return A [tibble::tibble()] with columns `.metric`, `.estimator` and
#'   `.estimate`.
#' @export
#' @examples
#' # See ?sdt_fit for a runnable example.
#' sdt_metric_set
sdt_metrics <- function(model) {
  .check_class(model, "sdt_model")
  model$metrics
}

#' Held-out predictions from a fitted model
#'
#' The predictions [sdt_fit()] made on the test half of its own split, with
#' the truth alongside. This is what the metrics were computed from.
#'
#' @param model An `sdt_model` from [sdt_fit()].
#' @return A [tibble::tibble()] with `.pred_class`, the per-class probability
#'   columns, and `truth`.
#' @export
#' @examples
#' # See ?sdt_fit for a runnable example.
#' sdt_metric_set
sdt_predictions <- function(model) {
  .check_class(model, "sdt_model")
  .require_pkgs("tune")
  preds <- tibble::as_tibble(tune::collect_predictions(model$last_fit))
  names(preds)[names(preds) == model$target] <- "truth"
  preds
}

#' Predict from a fitted mass-casualty model
#'
#' Scores new casualties with an [sdt_fit()] model. The return is always a
#' tibble, whatever the engine, so downstream code does not have to branch on
#' which model produced it.
#'
#' @param object An `sdt_model` from [sdt_fit()].
#' @param new_data A data frame of casualties to score.
#' @param ... Passed to the underlying [stats::predict()] method; `type =
#'   "prob"` returns class probabilities rather than a hard call.
#'
#' @return A [tibble::tibble()] with one row per row of `new_data`.
#' @seealso [sdt_predictions()] for the held-out predictions the model's own
#'   metrics were computed from.
#' @export
#' @examples
#' # See ?sdt_fit for a runnable example.
#' sdt_engines()
predict.sdt_model <- function(object, new_data, ...) {
  .require_pkgs("workflows")
  new_data <- tibble::as_tibble(new_data)
  out <- tibble::as_tibble(
    stats::predict(object$workflow, new_data = new_data, ...)
  )
  if (!".pred_class" %in% names(out)) {
    return(out)
  }
  # See .class_from_prob(): the class parsnip returns is not trustworthy for
  # every engine on a multiclass target, and the probabilities are.
  prob <- try(
    tibble::as_tibble(
      stats::predict(object$workflow, new_data = new_data, type = "prob")
    ),
    silent = TRUE
  )
  if (inherits(prob, "try-error")) {
    return(out)
  }
  out$.pred_class <- .class_from_prob(prob)$.pred_class
  out
}

#' @export
format.sdt_model <- function(x, ...) {
  rlang::check_dots_empty()
  auc <- x$metrics$.estimate[x$metrics$.metric == "roc_auc"]
  score <- if (length(auc)) sprintf(", roc_auc %.3f", auc[[1L]]) else ""
  c(
    sprintf(
      "<sdt_model> %s (%s-box) -> %s%s",
      x$engine, x$class_box, x$target, score
    ),
    sprintf("%d predictor%s: %s",
            length(x$predictors),
            if (length(x$predictors) == 1L) "" else "s",
            paste(x$predictors, collapse = ", "))
  )
}

#' @export
print.sdt_model <- function(x, ...) {
  rlang::check_dots_empty()
  cat(format(x), sep = "\n")
  invisible(x)
}


# Screening every engine ------------------------------------------------------

#' Screen every engine on one cohort
#'
#' Fits each available engine to the same data with the same preprocessing and
#' the same resamples, and returns a leaderboard. This is the whole bench:
#' glass-box and black-box models scored side by side, so the price of
#' interpretability is a number rather than an opinion.
#'
#' Engines whose package is not installed are skipped, and named in the
#' `skipped` attribute rather than silently dropped.
#'
#' @inheritParams sdt_fit
#' @param engines Which engines to screen. Defaults to all of them.
#'
#' @return A [tibble::tibble()] with one row per engine and metric:
#'   `engine`, `class_box`, `.metric`, `.estimator`, `.estimate`, ranked with
#'   the best `roc_auc` first. Carries a `skipped` attribute listing engines
#'   whose package was missing, and a `models` attribute holding the fitted
#'   `sdt_model` objects.
#' @seealso [sdt_fit()], [sdt_metrics()].
#' @export
#' @examples
#' co <- sdt_synthetic_cohort(n = 150, seed = 4)
#' needed <- c("parsnip", "recipes", "workflows", "rsample", "tune",
#'             "yardstick", "rpart")
#' if (all(vapply(needed, requireNamespace, logical(1), quietly = TRUE))) {
#'   sdt_workflowset(co, target = "triage", engines = "tree",
#'                   tune = FALSE, seed = 1, verbose = FALSE)
#' }
sdt_workflowset <- function(data,
                            target = c("triage", "therapy",
                                       "called_triage", "called_therapy"),
                            engines = NULL,
                            predictors = NULL,
                            tune = TRUE,
                            grid = 5L,
                            folds = 5L,
                            prop = 0.75,
                            parallel = FALSE,
                            workers = NULL,
                            seed = NULL,
                            verbose = TRUE) {
  target <- rlang::arg_match(target)
  .check_flag(verbose)
  .check_flag(parallel)
  .sdt_parallel_plan(parallel, workers)
  available <- sdt_engines()
  engines <- engines %||% available$engine
  unknown <- setdiff(engines, available$engine)
  if (length(unknown)) {
    cli::cli_abort(c(
      "{length(unknown)} unknown engine{?s}: {.val {unknown}}.",
      "i" = "Available: {.val {available$engine}}."
    ))
  }

  usable <- engines[engines %in% available$engine[available$installed]]
  skipped <- setdiff(engines, usable)
  if (length(skipped) && verbose) {
    cli::cli_warn(c(
      "!" = "Skipping {length(skipped)} engine{?s} with no package installed.",
      "i" = "Skipped: {.val {skipped}}."
    ))
  }
  if (!length(usable)) {
    out <- tibble::tibble(
      engine = character(), class_box = character(), .metric = character(),
      .estimator = character(), .estimate = numeric()
    )
    attr(out, "skipped") <- skipped
    attr(out, "models") <- list()
    return(out)
  }

  models <- lapply(usable, function(e) {
    if (verbose) cli::cli_inform("Screening {.val {e}}.")
    sdt_fit(
      data, target = target, engine = e, predictors = predictors,
      tune = tune, grid = grid, folds = folds, prop = prop,
      seed = seed, verbose = FALSE
    )
  })
  names(models) <- usable

  rows <- lapply(usable, function(e) {
    m <- sdt_metrics(models[[e]])
    m$engine <- e
    m$class_box <- .box_of(e)
    m[, c("engine", "class_box", ".metric", ".estimator", ".estimate")]
  })
  out <- dplyr::bind_rows(rows)
  out <- .rank_by_auc(out)
  attr(out, "skipped") <- skipped
  attr(out, "models") <- models
  out
}

# Order engines by held-out roc_auc, keeping each engine's metrics together.
.rank_by_auc <- function(x) {
  if (!nrow(x)) {
    return(x)
  }
  auc <- x[x$.metric == "roc_auc", c("engine", ".estimate")]
  order_of <- auc$engine[order(-auc$.estimate)]
  order_of <- c(order_of, setdiff(unique(x$engine), order_of))
  x$engine <- factor(x$engine, levels = order_of)
  out <- dplyr::arrange(x, .data$engine, .data$.metric)
  out$engine <- as.character(out$engine)
  out
}


# Curves and agreement --------------------------------------------------------

#' ROC curve for a fitted model
#'
#' Draws the receiver operating characteristic of an [sdt_fit()] model on its
#' held-out split. With more than two classes, one curve per class is drawn
#' one-versus-all, which is what `roc_auc`'s Hand-Till estimator summarises.
#'
#' @param model An `sdt_model` from [sdt_fit()].
#' @return A \pkg{ggplot2} object.
#' @export
#' @examples
#' # See ?sdt_fit for a runnable example.
#' sdt_metric_set
sdt_roc <- function(model) {
  .check_class(model, "sdt_model")
  .require_pkgs(c("yardstick", "tune"))
  preds <- sdt_predictions(model)
  prob_cols <- grep("^\\.pred_", names(preds), value = TRUE)
  prob_cols <- setdiff(prob_cols, ".pred_class")
  if (!length(prob_cols)) {
    return(.sdt_blank_plot("No predicted probabilities"))
  }

  curve <- yardstick::roc_curve(
    preds, truth = "truth", dplyr::all_of(prob_cols)
  )
  colour <- if (".level" %in% names(curve)) ".level" else NULL

  mapping <- if (is.null(colour)) {
    ggplot2::aes(x = 1 - .data$specificity, y = .data$sensitivity)
  } else {
    ggplot2::aes(
      x = 1 - .data$specificity, y = .data$sensitivity,
      colour = .data$.level
    )
  }
  ggplot2::ggplot(curve, mapping) +
    ggplot2::geom_abline(
      slope = 1, intercept = 0, linetype = "dashed", colour = "grey60"
    ) +
    ggplot2::geom_path(linewidth = 0.8) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = "ROC curve",
      subtitle = sprintf("%s (%s-box) predicting %s", model$engine,
                         model$class_box, model$target),
      x = "1 - specificity", y = "Sensitivity", colour = "Class"
    ) +
    .sdt_theme()
}

#' Agreement between two labellings of the same casualties
#'
#' Cohen's kappa between two sets of labels for the same units: a participant's
#' calls against the card's true category, one participant against another, or
#' a model's predictions across a repeated scenario. Kappa rather than raw
#' agreement, because on a cohort that is 44% red, two people who both guess
#' "red" every time agree 44% of the time while sharing no judgement at all.
#'
#' @param truth Reference labels.
#' @param estimate Labels to compare against `truth`.
#' @param levels Optional level set. Defaults to the union of both inputs, so
#'   a level nobody used does not distort the expected agreement.
#'
#' @return A one-row [tibble::tibble()] with `.metric`, `.estimator`,
#'   `.estimate` and `n`.
#' @export
#' @examples
#' if (requireNamespace("yardstick", quietly = TRUE)) {
#'   truth <- c("red", "red", "yellow", "green", "green")
#'   called <- c("red", "yellow", "yellow", "green", "green")
#'   sdt_consistency_score(truth, called)
#' }
sdt_consistency_score <- function(truth, estimate, levels = NULL) {
  .require_pkgs("yardstick")
  if (length(truth) != length(estimate)) {
    cli::cli_abort(c(
      "{.arg truth} and {.arg estimate} must be the same length.",
      "x" = "Got {length(truth)} and {length(estimate)}."
    ))
  }
  keep <- !is.na(truth) & !is.na(estimate)
  truth <- as.character(truth)[keep]
  estimate <- as.character(estimate)[keep]
  if (!length(truth)) {
    return(tibble::tibble(
      .metric = "kap", .estimator = NA_character_,
      .estimate = NA_real_, n = 0L
    ))
  }

  levels <- levels %||% sort(union(unique(truth), unique(estimate)))
  if (length(levels) < 2L) {
    # Two raters who both used exactly one label agree completely and share no
    # judgement at all. Kappa is undefined there, and reporting 1 would be
    # exactly the wrong answer.
    return(tibble::tibble(
      .metric = "kap", .estimator = NA_character_,
      .estimate = NA_real_, n = length(truth)
    ))
  }
  df <- tibble::tibble(
    truth = factor(truth, levels = levels),
    estimate = factor(estimate, levels = levels)
  )
  out <- tibble::as_tibble(
    yardstick::kap(df, truth = "truth", estimate = "estimate")
  )
  out$n <- length(truth)
  out
}
