# A tabular transformer, as a closed-box engine -----------------------------
# Every other engine on the bench reasons in the columns it was given: a tree
# splits on `life_points`, a penalised regression puts a coefficient on it, a
# forest permutes it. This one does not. It projects each feature into a
# learned embedding, attends across those embeddings, and makes its decision in
# a representation of its own devising. Permuting an input column still moves
# the prediction, so DALEX will return a number - but that number attributes
# importance to something the model no longer reasons in. That is why the
# registry grades it "closed" rather than "black": the distinction is whether
# an explanation is FAITHFUL, not whether one can be produced.
#
# WHY IT IS HERE AT ALL. On this corpus it is the wrong tool and is expected to
# say so: the effective sample size is 46 cards, not 5,173 rows, and a model
# with thousands of parameters cannot learn more from 46 facts than a logistic
# regression does. It earns its place by being the honest upper bound on that
# claim -- and by being the model class that becomes reasonable once the
# simulator is used to expand the training set, which is the whole point of a
# behaviourally benchmarked synthetic cohort.
#
# The architecture is FT-Transformer shaped, kept deliberately small: one
# attention block, a feature embedding per column, mean-pool, softmax. Feature
# indices are supplied as a second input so every column gets its own learned
# embedding; with a single shared projection each feature becomes the same
# token and self-attention has nothing to attend to.

# Is a working keras3 with a reachable backend actually present? Import failure
# is the common case (no Python, no TensorFlow), and it must be a clean FALSE
# rather than an error at package load.
.sdt_keras_ready <- function() {
  if (!requireNamespace("keras3", quietly = TRUE)) {
    return(FALSE)
  }
  ok <- try(utils::getFromNamespace("keras", "keras3")$`__version__`,
            silent = TRUE)
  !inherits(ok, "try-error") && length(ok) > 0L
}

#' Fit a tabular transformer (engine plumbing)
#'
#' The fit function behind the `"transformer"` engine. Exported because parsnip
#' resolves a registered model's fit and predict callbacks with
#' `getExportedValue()`, so an internal dot-prefixed function is unreachable to
#' it. Call [sdt_fit()] rather than this.
#'
#' @param x Numeric predictor matrix from the recipe.
#' @param y Outcome factor.
#' @param d_model Embedding width per feature.
#' @param num_heads Attention heads.
#' @param epochs Training epochs.
#' @param batch_size Minibatch size.
#' @param dropout Dropout before the softmax.
#' @param ... Unused.
#' @return An `sdt_transformer` object.
#' @keywords internal
#' @export
sdt_transformer_fit <- function(x, y, d_model = 16L, num_heads = 2L,
                                 epochs = 30L, batch_size = 64L,
                                 dropout = 0.1, ...) {
  if (!.sdt_keras_ready()) {
    cli::cli_abort(c(
      "The {.val transformer} engine needs a working {.pkg keras3} backend.",
      "i" = "Install it with {.code keras3::install_keras()}."
    ))
  }
  # keras3:: throughout, not asNamespace()$: `compile` and `fit` are
  # re-exported generics that do not exist as objects in the namespace, and
  # `predict` is an S3 method that is not exported at all, so `ns$compile`
  # resolves to NULL and fails with "attempt to apply non-function".
  x <- as.matrix(x)
  lev <- levels(y)
  yi <- as.integer(y) - 1L
  p <- ncol(x)
  d_model <- max(2L, as.integer(d_model))
  num_heads <- max(1L, as.integer(num_heads))

  inp <- keras3::layer_input(shape = p)
  idx <- keras3::layer_input(shape = p, dtype = "int32")
  tok <- keras3::layer_dense(keras3::layer_reshape(inp, c(p, 1L)), d_model)
  pos <- keras3::layer_embedding(idx, input_dim = p, output_dim = d_model)
  h <- keras3::layer_add(list(tok, pos))
  att <- keras3::layer_multi_head_attention(
    num_heads = num_heads, key_dim = d_model
  )(h, h)
  h <- keras3::layer_layer_normalization()(keras3::layer_add(list(h, att)))
  ff <- keras3::layer_dense(
    keras3::layer_dense(h, d_model * 2L, activation = "relu"), d_model
  )
  h <- keras3::layer_layer_normalization()(keras3::layer_add(list(h, ff)))
  out <- keras3::layer_dense(
    keras3::layer_dropout(keras3::layer_global_average_pooling_1d(h), dropout),
    length(lev), activation = "softmax"
  )
  model <- keras3::keras_model(list(inp, idx), out)
  keras3::compile(
    model, optimizer = keras3::optimizer_adam(1e-3),
    loss = "sparse_categorical_crossentropy", metrics = "accuracy"
  )
  keras3::fit(
    model, list(x, .sdt_transformer_idx(nrow(x), p)), yi,
    epochs = as.integer(epochs), batch_size = as.integer(batch_size),
    verbose = 0L
  )
  structure(list(model = model, levels = lev, p = p), class = "sdt_transformer")
}

# The constant feature-index matrix the embedding input consumes.
.sdt_transformer_idx <- function(n, p) {
  matrix(rep(seq_len(p) - 1L, each = n), nrow = n)
}

#' Predict from a fitted tabular transformer (engine plumbing)
#'
#' A plain function rather than an S3 method: parsnip calls it from its own
#' namespace, where a method defined here would not be found unless separately
#' registered, and the failure mode is a silent dispatch to the default.
#'
#' @param object An `sdt_transformer` from [sdt_transformer_fit()].
#' @param new_data Numeric predictor matrix.
#' @param type `"prob"` or `"class"`.
#' @param ... Unused.
#' @return A data frame of class probabilities, or a factor.
#' @keywords internal
#' @export
sdt_transformer_predict <- function(object, new_data, type = "prob", ...) {
  x <- as.matrix(new_data)
  pr <- stats::predict(object$model,
                       list(x, .sdt_transformer_idx(nrow(x), object$p)),
                       verbose = 0L)
  colnames(pr) <- object$levels
  if (identical(type, "class")) {
    return(factor(object$levels[max.col(pr)], levels = object$levels))
  }
  as.data.frame(pr)
}

# Registration. Idempotent, because pkgload::load_all() re-runs .onLoad and
# parsnip errors on a duplicate model name.
.sdt_register_transformer <- function() {
  if (!requireNamespace("parsnip", quietly = TRUE)) {
    return(invisible(FALSE))
  }
  known <- try(parsnip::get_model_env()$models, silent = TRUE)
  if (inherits(known, "try-error")) {
    return(invisible(FALSE))
  }
  if ("sdt_transformer" %in% known) {
    return(invisible(TRUE))
  }
  parsnip::set_new_model("sdt_transformer")
  parsnip::set_model_mode("sdt_transformer", "classification")
  parsnip::set_model_engine("sdt_transformer", "classification", "keras3")
  parsnip::set_dependency("sdt_transformer", "keras3", "keras3",
                          mode = "classification")
  for (a in c("d_model", "num_heads", "epochs")) {
    parsnip::set_model_arg(
      model = "sdt_transformer", eng = "keras3", parsnip = a, original = a,
      func = list(pkg = "dials", fun = "hidden_units"), has_submodel = FALSE
    )
  }
  parsnip::set_encoding(
    model = "sdt_transformer", eng = "keras3", mode = "classification",
    options = list(predictor_indicators = "traditional",
                   compute_intercept = FALSE,
                   remove_intercept = TRUE, allow_sparse_x = FALSE)
  )
  parsnip::set_fit(
    model = "sdt_transformer", eng = "keras3", mode = "classification",
    value = list(interface = "matrix", protect = c("x", "y"),
                 func = c(pkg = "sandtablR", fun = "sdt_transformer_fit"),
                 defaults = list())
  )
  parsnip::set_pred(
    model = "sdt_transformer", eng = "keras3", mode = "classification",
    type = "class",
    value = list(pre = NULL, post = NULL,
                 func = c(pkg = "sandtablR",
                          fun = "sdt_transformer_predict"),
                 args = list(quote(object$fit), quote(new_data), "class"))
  )
  parsnip::set_pred(
    model = "sdt_transformer", eng = "keras3", mode = "classification",
    type = "prob",
    value = list(pre = NULL, post = NULL,
                 func = c(pkg = "sandtablR",
                          fun = "sdt_transformer_predict"),
                 args = list(quote(object$fit), quote(new_data), "prob"))
  )
  invisible(TRUE)
}

#' A tabular transformer specification
#'
#' A small FT-Transformer-shaped model for the workbench: one self-attention
#' block over per-feature embeddings, mean-pooled to a softmax. Registered as
#' the `"transformer"` engine in [sdt_engines()] and graded `"closed"`, because
#' it decides in a representation it learned rather than in the columns it was
#' given, so a per-column attribution describes something it no longer reasons
#' in.
#'
#' On a corpus whose effective sample size is the number of distinct cards, this
#' is expected to match rather than beat a logistic regression, and to say so.
#' It becomes the right tool when the training set is expanded with simulated
#' cohorts.
#'
#' @param d_model Embedding width per feature.
#' @param num_heads Attention heads.
#' @param epochs Training epochs.
#' @return A \pkg{parsnip} model specification.
#' @export
#' @examples
#' \dontrun{
#' sdt_transformer(d_model = 16, num_heads = 2)
#' }
sdt_transformer <- function(d_model = 16L, num_heads = 2L, epochs = 30L) {
  .sdt_register_transformer()
  parsnip::set_engine(
    parsnip::new_model_spec(
      "sdt_transformer",
      args = list(d_model = rlang::enquo(d_model),
                  num_heads = rlang::enquo(num_heads),
                  epochs = rlang::enquo(epochs)),
      eng_args = NULL, mode = "classification",
      user_specified_mode = TRUE, method = NULL, engine = NULL,
      user_specified_engine = TRUE
    ),
    "keras3"
  )
}
