# Explainability --------------------------------------------------------------
# Model-agnostic explanations via DALEX, so a decision tree and a random forest
# can be interrogated the same way and compared on one footing. That is the
# point of carrying both kinds of model: a glass-box model can be read
# directly, a black-box one cannot, and the only way to put them side by side
# is to explain both from the outside.
#
# Everything here is optional. DALEX and DALEXtra live in Suggests, and each
# entry point says plainly what to install rather than failing obscurely.

# Multiclass needs an explicit probability predictor. DALEX otherwise defaults
# to a regression-shaped predict, which produces a single meaningless column
# for a three-class outcome instead of failing.
#
# The column names matter as much as the values: DALEX::loss_cross_entropy
# looks the observed class up by name, as `predicted[i, observed[i]]`, and
# tidymodels returns `.pred_red` where the observed value is `red`. Left
# alone, every lookup is out of bounds.
.explain_predict <- function(model, newdata) {
  probs <- stats::predict(model, new_data = newdata, type = "prob")
  names(probs) <- sub("^\\.pred_", "", names(probs))
  as.matrix(probs)
}

#' Explain a fitted mass-casualty model
#'
#' Wraps a fitted [sdt_fit()] model in a \pkg{DALEX} explainer, so permutation
#' importance, dependence profiles and per-casualty attributions all work off
#' one object.
#'
#' @param model An `sdt_model` from [sdt_fit()].
#' @param data Data to explain on. Defaults to the held-out half of the
#'   model's own split, which is the right choice: explaining a model on the
#'   rows it was fitted to flatters it.
#' @param y Optional true outcome vector; taken from `data` when omitted.
#'
#' @return A \pkg{DALEX} explainer.
#' @seealso [sdt_importance()], [sdt_profile()], [sdt_shap()].
#' @export
#' @examples
#' # See ?sdt_importance for a runnable example.
#' sdt_engines()
sdt_explain <- function(model, data = NULL, y = NULL) {
  .check_class(model, "sdt_model")
  .require_pkgs(c("DALEX", "DALEXtra", "rsample"))

  data <- data %||% rsample::testing(model$split)
  data <- tibble::as_tibble(data)
  if (!nrow(data)) {
    cli::cli_abort("{.arg data} must have at least one row to explain.")
  }
  target <- model$target
  y <- y %||% data[[target]]
  if (is.null(y)) {
    cli::cli_abort(
      "{.arg data} has no {.field {target}} column, so {.arg y} is needed."
    )
  }

  suppressWarnings(DALEXtra::explain_tidymodels(
    model$workflow,
    data = as.data.frame(data[, model$predictors, drop = FALSE]),
    y = as.character(y),
    predict_function = .explain_predict,
    label = paste0(model$engine, " (", model$class_box, "-box)"),
    verbose = FALSE
  ))
}

# Every entry point takes either a model or an explainer, so a caller can
# build the explainer once and reuse it across several explanations.
.as_explainer <- function(x, data = NULL) {
  if (inherits(x, "explainer")) {
    return(x)
  }
  sdt_explain(x, data = data)
}

#' Permutation variable importance
#'
#' How much worse the model gets when each predictor is shuffled. The measure
#' is model-agnostic, so a random forest and a decision tree are ranked on the
#' same scale.
#'
#' @param x An `sdt_model` from [sdt_fit()], or an explainer from
#'   [sdt_explain()].
#' @param data Optional data to explain on, passed to [sdt_explain()].
#' @param n_permutations Number of permutation rounds. More is steadier and
#'   slower.
#'
#' @return A \pkg{ggplot2} object.
#' @seealso [sdt_profile()], [sdt_shap()].
#' @export
#' @examples
#' co <- sdt_add_decisions(sdt_synthetic_cohort(n = 200, seed = 3), seed = 4)
#' needed <- c("parsnip", "recipes", "workflows", "rsample", "tune",
#'             "yardstick", "rpart", "DALEX", "DALEXtra")
#' if (all(vapply(needed, requireNamespace, logical(1), quietly = TRUE))) {
#'   fit <- sdt_fit(co, target = "called_triage", engine = "tree",
#'                  tune = FALSE, seed = 1, verbose = FALSE)
#'   sdt_importance(fit)
#' }
sdt_importance <- function(x, data = NULL, n_permutations = 10L) {
  .require_pkgs(c("DALEX", "DALEXtra"))
  n_permutations <- .check_count(n_permutations)
  explainer <- .as_explainer(x, data)

  parts <- suppressWarnings(DALEX::model_parts(
    explainer,
    loss_function = DALEX::loss_cross_entropy,
    B = n_permutations,
    type = "difference"
  ))
  # DALEX's plotting goes through ingredients, which still calls the
  # deprecated aes_string(). The warning is not actionable here.
  suppressWarnings(plot(parts)) +
    ggplot2::labs(
      title = "Permutation variable importance",
      subtitle = "Loss increase when each predictor is shuffled"
    )
}

#' Dependence profiles for a fitted model
#'
#' How the model's predicted probability moves as one predictor changes, with
#' the others held at their observed values. Partial dependence averages over
#' the data; accumulated local effects are more trustworthy when predictors
#' are correlated, which on a casualty card they are.
#'
#' @param x An `sdt_model` from [sdt_fit()], or an explainer from
#'   [sdt_explain()].
#' @param variables Predictors to profile. Defaults to all of them.
#' @param type `"partial"` for a PDP, `"accumulated"` for ALE.
#' @param data Optional data to explain on, passed to [sdt_explain()].
#'
#' @return A \pkg{ggplot2} object.
#' @seealso [sdt_importance()], [sdt_shap()].
#' @export
#' @examples
#' # See ?sdt_importance for a runnable example.
#' sdt_engines()
sdt_profile <- function(x,
                        variables = NULL,
                        type = c("partial", "accumulated"),
                        data = NULL) {
  .require_pkgs(c("DALEX", "DALEXtra"))
  type <- rlang::arg_match(type)
  explainer <- .as_explainer(x, data)

  profile <- suppressWarnings(DALEX::model_profile(
    explainer,
    variables = variables,
    type = type
  ))
  label <- if (identical(type, "partial")) {
    "Partial dependence"
  } else {
    "Accumulated local effects"
  }
  suppressWarnings(plot(profile)) +
    ggplot2::labs(
      title = label,
      subtitle = "Predicted probability as each predictor varies"
    )
}

#' Attribution for one casualty
#'
#' Breaks the model's prediction for a single casualty into per-predictor
#' contributions, using SHAP values. This answers "why this casualty?", as
#' opposed to importance, which answers "what does this model use in general?".
#'
#' @param x An `sdt_model` from [sdt_fit()], or an explainer from
#'   [sdt_explain()].
#' @param casualty A one-row data frame, or the `id` of a casualty in `data`.
#' @param data Optional data to explain on, passed to [sdt_explain()]. Also
#'   where `casualty` is looked up when given as an id.
#' @param n_permutations Number of sampling rounds for the SHAP estimate.
#'
#' @return A \pkg{ggplot2} object.
#' @seealso [sdt_importance()], [sdt_profile()].
#' @export
#' @examples
#' # See ?sdt_importance for a runnable example.
#' sdt_engines()
sdt_shap <- function(x, casualty, data = NULL, n_permutations = 10L) {
  .require_pkgs(c("DALEX", "DALEXtra"))
  n_permutations <- .check_count(n_permutations)
  explainer <- .as_explainer(x, data)

  observation <- .resolve_casualty(casualty, explainer, data)
  parts <- suppressWarnings(DALEX::predict_parts(
    explainer,
    new_observation = observation,
    type = "shap",
    B = n_permutations
  ))
  suppressWarnings(plot(parts)) +
    ggplot2::labs(subtitle = "Per-predictor contribution to this prediction")
}

# Accept either a one-row data frame or an id to look up.
.resolve_casualty <- function(casualty, explainer, data,
                              call = rlang::caller_env()) {
  if (is.data.frame(casualty)) {
    if (nrow(casualty) != 1L) {
      cli::cli_abort(
        c(
          "{.arg casualty} must be a single casualty.",
          "x" = "Got {nrow(casualty)} row{?s}."
        ),
        call = call
      )
    }
    keep <- intersect(names(explainer$data), names(casualty))
    return(as.data.frame(casualty[, keep, drop = FALSE]))
  }

  .check_string(casualty, call = call)
  if (is.null(data) || !"id" %in% names(data)) {
    cli::cli_abort(
      c(
        "Looking a casualty up by id needs an {.field id} column.",
        "i" = "Or pass the casualty itself as a one-row data frame."
      ),
      call = call
    )
  }
  hit <- which(data$id == casualty)
  if (!length(hit)) {
    cli::cli_abort("No casualty with id {.val {casualty}} in {.arg data}.",
                   call = call)
  }
  keep <- intersect(names(explainer$data), names(data))
  as.data.frame(data[hit[[1L]], keep, drop = FALSE])
}
