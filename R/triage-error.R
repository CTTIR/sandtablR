# Triage error model ----------------------------------------------------------
# Fitting the MISTAKES teams make, and how far teams differ in making them.
#
# `sdt_fit_behaviour()` models what a team does: did it perform an eFAST, did
# it call this casualty red. That is an unconditional outcome, and it is the
# right object as long as there is nothing to be right or wrong about. Once a
# gold-standard sighting category exists the interesting outcome changes: not
# "did the team call red" but "given that this casualty IS red, what did the
# team call?". The response is the called category CONDITIONAL ON THE TRUE
# ONE, and its distribution is a row of the confusion matrix.
#
# That conditioning is not a detail. Measured on the 19 course exports against
# the gold standard, the overall hit rate is 0.821 -- but it is 0.937 on red,
# 0.685 on yellow and 0.634 on green, and the errors are overwhelmingly in one
# direction (over-triage 0.136, under-triage 0.043). A model of the marginal
# "called red" rate reproduces none of that structure, because the marginal
# rate is a mixture over truths and the mixture weights are the deck's.
#
# The parameterisation
# ~~~~~~~~~~~~~~~~~~~~
# For each true category k the response has three levels, so it has two free
# logits. This file uses the sequential pair
#
#     hit    P(called = k | true = k)
#     split  P(called = w1 | called != k, true = k)
#
# where w1 is the more severe of the two remaining categories. That is an
# EXACT reparameterisation of the 3-category multinomial, not an
# approximation: the multinomial likelihood factorises into the two binomial
# likelihoods, so fitting them separately is fitting the multinomial with the
# team random effects of the two logits constrained INDEPENDENT. The only
# parameter the decomposition gives up is the correlation between a team's
# accuracy and its error direction.
#
# `joint = TRUE` estimates that correlation, by stacking the two parts and
# fitting `(0 + part | group)` with an unstructured 2x2 covariance. It was
# tried on the real corpus, and it is why the decomposition is the default
# rather than a shortcut:
#
#   green   converged in 4s, correlation -0.050, and the joint AIC (1651.0)
#           equals the sum of the two separate AICs (1394.3 + 256.7). The
#           correlation is worth nothing here.
#   red     rank deficient -- 15 of the 30 red cards were never miscalled, so
#           they exist in the hit part and not in the split part -- and the
#           team covariance collapses onto a correlation of 0.99 with a split
#           SD at the boundary (4.7 to 5.5).
#   yellow  the split SD runs to 33-40 on the logit scale. That is separation
#           in a cell with 124 error rows, not a team effect.
#
# So the full multinomial mixed model does NOT fit reliably at this n, in two
# of the three cells, and the one cell where it does says the constraint the
# fallback imposes costs nothing. `joint = TRUE` is kept so that statement is
# reproducible rather than a footnote, and it records its own failure per
# category instead of quietly degrading.
#
# Why the card term is load-bearing
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cards within one true category are wildly unequal: red card 14 was called
# correctly by 26% of teams and 15 of the 30 red cards by 100% of them. Drop
# the card effect and the team variance does not merely shrink, it VANISHES --
# the fitted team SD is exactly 0 for red and for yellow, against 0.544 and
# 0.639 with it, and the AIC is 473 and 137 worse. The reason is that every
# group sees nearly the same deck, so the spread of raw hit rates across teams
# is SMALLER than binomial noise around the pooled rate would imply; a model
# with one probability per category can only explain that by setting the team
# variance to zero. The card term is what makes the team term visible.
#
# The split part is fitted WITHOUT a card term by default, and that is
# measured too. Its cells hold 176, 124 and 529 error rows with minority
# counts of 10, 24 and 38; a card-fixed split blows the yellow team SD up to
# 41, is worse by AIC than no card term at all for red, and leaves any card
# that was never miscalled with no coefficient at all. It costs little: for
# red both wrong answers are under-triage and for green both are over-triage,
# so the split moves cells inside the confusion matrix without moving the
# over- and under-triage rates. Only yellow's split IS the direction of
# error, and yellow is exactly the cell where a card term cannot be
# estimated.

# The two parts of a category's row in the confusion matrix.
.te_parts <- function() c("hit", "split")

# The card-effect ladder, most structure first. A cell demotes down it when
# the rung it is on cannot be estimated, and records every step.
.te_card_ladder <- function() c("fixed", "random", "none")

# The wrong answers for a true category, more severe first. The levels of
# `sdt_triage_levels()` are already in severity order, so this keeps
# over-triage in the first slot wherever over-triage is possible at all.
.te_wrong <- function(k, levels = sdt_triage_levels()) setdiff(levels, k)

.te_cell_key <- function(category, part) paste(category, part, sep = ".")

# Minimum data before a cell is worth handing to an optimiser. Same spirit and
# same numbers as `.beh_fit_one()`, so the two model families refuse in the
# same places.
.te_min_events <- function() 5L
.te_min_groups <- function() 5L

# A random-effect SD this wide on the logit scale is separation rather than a
# tendency, and is the trigger for demoting the card term. Shared with the
# behaviour model so "wide" means one thing in this package.
.te_wide_sd <- function() .beh_wide_sd()

# A fixed card coefficient this large means the card was called the same way
# by every team: plogis(10) is 0.99995. Kept rather than demoted -- it is a
# true statement about the card -- but flagged, because a simulated team will
# never err on it.
.te_separated_at <- function() 10

.te_s <- function(n) if (n == 1L) "" else "s"

.te_skip_row <- function(category, part, stage, reason) {
  tibble::tibble(
    category = category, part = part, stage = stage, reason = reason
  )
}

.te_empty_skips <- function() {
  tibble::tibble(
    category = character(), part = character(), stage = character(),
    reason = character()
  )
}

# Engines ---------------------------------------------------------------------

# A formula with no bar has no random effect, and `glmer()` refuses it. That
# happens on the "no team term" counterfactual when the data holds one block
# and the card term has been demoted away, which is exactly the model the
# necessity check has to be able to fit.
.te_has_re <- function(form) {
  any(grepl("|", as.character(form)[[3L]], fixed = TRUE))
}

.te_formula <- function(card_effect, use_block, use_group) {
  rhs <- character()
  if (identical(card_effect, "fixed")) rhs <- c(rhs, ".card")
  if (identical(card_effect, "random")) rhs <- c(rhs, "(1 | .card)")
  if (use_block) rhs <- c(rhs, "(1 | .block)")
  if (use_group) rhs <- c(rhs, "(1 | .group)")
  if (!length(rhs)) rhs <- "1"
  stats::as.formula(paste(".y ~", paste(rhs, collapse = " + ")))
}

.te_glmm <- function(form, d, engine) {
  if (!.te_has_re(form)) {
    return(stats::glm(form, data = d, family = stats::binomial()))
  }
  if (identical(engine, "lme4")) {
    ctrl <- lme4::glmerControl(
      optimizer = "bobyqa", calc.derivs = FALSE,
      optCtrl = list(maxfun = 2e5)
    )
    return(lme4::glmer(form, data = d, family = stats::binomial(),
                       control = ctrl))
  }
  glmmTMB::glmmTMB(form, data = d, family = stats::binomial())
}

# Fit and collect everything the optimiser said, so a demotion decision can be
# made on the content rather than on whether it happened to throw. Messages
# are collected as well as warnings: lme4 reports rank deficiency through
# `message()`, which is the one diagnosis that must not be missed, and
# letting it print would put optimiser chatter in front of a caller who has
# asked for none.
.te_try_fit <- function(form, d, engine) {
  msgs <- character()
  keep <- function(cond, restart) {
    msgs <<- c(msgs, conditionMessage(cond))
    invokeRestart(restart)
  }
  fit <- withCallingHandlers(
    tryCatch(.te_glmm(form, d, engine), error = function(e) {
      msgs <<- c(msgs, conditionMessage(e))
      NULL
    }),
    warning = function(w) keep(w, "muffleWarning"),
    message = function(m) keep(m, "muffleMessage")
  )
  list(fit = fit, messages = msgs)
}

.te_first_line <- function(x) {
  if (!length(x)) {
    return("unknown reason")
  }
  substr(gsub("\\s+", " ", x[[1L]]), 1L, 110L)
}

# Random-effect SDs, keyed by grouping factor, for either engine and for the
# `glm()` degenerate case.
.te_sds <- function(fit, engine) {
  blank <- c(.group = NA_real_, .block = NA_real_, .card = NA_real_)
  if (inherits(fit, "glm")) {
    return(blank)
  }
  if (identical(engine, "lme4")) {
    vc <- as.data.frame(lme4::VarCorr(fit))
    for (g in names(blank)) {
      v <- vc$sdcor[vc$grp == g]
      if (length(v)) blank[[g]] <- v[[1L]]
    }
    return(blank)
  }
  vc <- glmmTMB::VarCorr(fit)$cond
  for (g in names(blank)) {
    if (!is.null(vc[[g]])) blank[[g]] <- as.numeric(attr(vc[[g]], "stddev"))
  }
  blank
}

.te_fixef <- function(fit, engine) {
  if (inherits(fit, "glm")) {
    return(stats::coef(fit))
  }
  if (identical(engine, "lme4")) lme4::fixef(fit) else glmmTMB::fixef(fit)$cond
}

.te_ranef <- function(fit, engine) {
  if (inherits(fit, "glm")) {
    return(list())
  }
  re <- if (identical(engine, "lme4")) {
    lme4::ranef(fit)
  } else {
    glmmTMB::ranef(fit)$cond
  }
  lapply(re, function(tab) stats::setNames(tab[[1L]], rownames(tab)))
}

.te_ic <- function(fit) {
  ll <- tryCatch(stats::logLik(fit), error = function(e) NULL)
  if (is.null(ll)) {
    return(list(loglik = NA_real_, deviance = NA_real_, df = NA_real_,
                aic = NA_real_))
  }
  df <- as.numeric(attr(ll, "df"))
  ll <- as.numeric(ll)
  list(loglik = ll, deviance = -2 * ll, df = df, aic = -2 * ll + 2 * df)
}

# The card levels a fit actually carries. Not always the frame's: a split cell
# only ever sees the cards a team got wrong, so a card nobody miscalled is
# absent from it and has no coefficient to look up.
.te_fit_card_levels <- function(fit, engine) {
  mf <- if (inherits(fit, "glm")) {
    fit$model
  } else if (identical(engine, "glmmTMB")) {
    fit$frame
  } else {
    methods::slot(fit, "frame")
  }
  if (is.null(mf$.card)) NULL else levels(mf$.card)
}

# The linear predictor for each requested card at zero random effects. NA for
# a card this cell was never fitted on, which the simulator carries through
# rather than filling in.
.te_card_eta <- function(fit, engine, card_effect, cards) {
  beta <- .te_fixef(fit, engine)
  if (identical(card_effect, "fixed")) {
    lv <- .te_fit_card_levels(fit, engine)
    nd <- data.frame(.card = factor(lv, levels = lv))
    x <- stats::model.matrix(stats::as.formula("~ .card"), data = nd)
    keep <- intersect(colnames(x), names(beta))
    if (!identical(sort(keep), sort(names(beta)))) {
      cli::cli_abort(
        c("Fitted fixed effects do not match the card design matrix.",
          "i" = "The model formula changed without the simulator
                 following."),
        class = "sandtablR_error_triage"
      )
    }
    eta <- as.vector(x[, names(beta), drop = FALSE] %*% beta)
    return(eta[match(cards, lv)])
  }
  b0 <- unname(beta[["(Intercept)"]])
  if (identical(card_effect, "random")) {
    u <- .te_ranef(fit, engine)$.card
    return(b0 + unname(u[match(cards, names(u))]))
  }
  rep(b0, length(cards))
}

# Fitting one cell ------------------------------------------------------------

# Is a card term worth attempting on these rows at all? A fixed effect per
# card needs at least as many minority events as there are cards before it is
# anything but an interpolation of the cell -- the red split has 10 green
# calls spread over 15 cards, and fitting a coefficient per card there is
# fitting noise. The check is made before the optimiser is asked, because a
# fit that overfits does not fail, it succeeds and lies.
.te_card_feasible <- function(d, card_effect) {
  if (identical(card_effect, "none")) {
    return(TRUE)
  }
  n_lv <- nlevels(d$.card)
  if (n_lv < 2L) {
    return(FALSE)
  }
  minority <- min(sum(d$.y == 1L), sum(d$.y == 0L))
  minority >= n_lv
}

# What the optimiser said, read as a decision rather than as prose.
.te_diagnose <- function(msgs) {
  list(
    rankdef = any(grepl("rank deficient", msgs, ignore.case = TRUE)),
    nonconv = any(grepl("converge|degenerate|PIRLS|NaN", msgs,
                        ignore.case = TRUE))
  )
}

# Walk the card-effect ladder from the requested rung down, and stop on the
# first rung that produces a fit whose random effects are inside the
# boundary. Every step down is recorded with its reason, so the model can say
# what it tried and why it gave up on it.
.te_fit_ladder <- function(d, requested, engine, use_block, use_group) {
  ladder <- .te_card_ladder()
  start <- match(requested, ladder)
  steps <- list()
  for (ce in ladder[seq(start, length(ladder))]) {
    if (!.te_card_feasible(d, ce)) {
      steps[[length(steps) + 1L]] <- sprintf(
        "%s: too few minority events for %d card level%s", ce,
        nlevels(d$.card), .te_s(nlevels(d$.card))
      )
      next
    }
    form <- .te_formula(ce, use_block, use_group)
    environment(form) <- list2env(as.list(d), parent = environment(form))
    res <- .te_try_fit(form, d, engine)
    if (is.null(res$fit)) {
      steps[[length(steps) + 1L]] <- sprintf(
        "%s: fit failed (%s)", ce, .te_first_line(res$messages)
      )
      next
    }
    diag <- .te_diagnose(res$messages)
    if (diag$rankdef || diag$nonconv) {
      why <- if (diag$rankdef) "rank deficient" else "did not converge"
      steps[[length(steps) + 1L]] <- sprintf("%s: %s", ce, why)
      next
    }
    sds <- .te_sds(res$fit, engine)
    wide <- sds[is.finite(sds)] > .te_wide_sd()
    if (any(wide)) {
      worst <- max(sds[is.finite(sds)])
      steps[[length(steps) + 1L]] <- sprintf(
        "%s: random SD %.1f above the boundary of %g", ce, worst,
        .te_wide_sd()
      )
      next
    }
    return(list(fit = res$fit, card_effect = ce, form = form, sds = sds,
                messages = res$messages, steps = steps))
  }
  list(fit = NULL, card_effect = NA_character_, form = NULL, sds = NULL,
       messages = character(), steps = steps)
}

# One cell: the fitted model, the same model with the team term removed from
# exactly the same rows, and everything needed to say whether the team term
# earned its place.
.te_fit_cell <- function(y, md, requested, engine, use_block) {
  bad <- function(reason) list(fit = NULL, reason = reason)
  y <- as.logical(y)
  ok <- !is.na(y)
  if (!any(ok)) {
    return(bad("all values missing"))
  }
  d <- md[ok, , drop = FALSE]
  d$.y <- as.integer(y[ok])
  d$.group <- droplevels(d$.group)
  d$.block <- droplevels(d$.block)
  d$.card <- droplevels(d$.card)
  n_true <- sum(d$.y == 1L)
  n_false <- sum(d$.y == 0L)
  n_groups <- nlevels(d$.group)
  if (n_true == 0L || n_false == 0L) {
    lab <- if (n_true == 0L) "FALSE" else "TRUE"
    return(bad(sprintf("constant (all %s in %d rows)", lab, nrow(d))))
  }
  if (min(n_true, n_false) < .te_min_events()) {
    return(bad(sprintf("too few events (%d of %d rows)",
                       min(n_true, n_false), nrow(d))))
  }
  if (n_groups < .te_min_groups()) {
    return(bad(sprintf("too few groups with data (%d)", n_groups)))
  }
  use_block <- use_block && nlevels(d$.block) > 1L

  got <- .te_fit_ladder(d, requested, engine, use_block, TRUE)
  if (is.null(got$fit)) {
    return(bad(paste("no card effect could be fitted:",
                     paste(unlist(got$steps), collapse = "; "))))
  }

  # The counterfactual: identical rows, identical card effect, no team term.
  # It is the only fair "before", and it is what makes the necessity of the
  # team effect a measured claim rather than a stated one.
  null_form <- .te_formula(got$card_effect, use_block, FALSE)
  environment(null_form) <- list2env(as.list(d),
                                     parent = environment(null_form))
  null_res <- .te_try_fit(null_form, d, engine)
  null_ic <- .te_ic(null_res$fit)
  null_sds <- if (is.null(null_res$fit)) {
    c(.group = NA_real_, .block = NA_real_, .card = NA_real_)
  } else {
    .te_sds(null_res$fit, engine)
  }

  ic <- .te_ic(got$fit)
  chisq <- null_ic$deviance - ic$deviance
  df <- ic$df - null_ic$df
  p <- if (is.finite(chisq) && is.finite(df) && df > 0 && chisq >= 0) {
    stats::pchisq(chisq, df, lower.tail = FALSE)
  } else {
    NA_real_
  }
  beta <- .te_fixef(got$fit, engine)
  card_beta <- beta[names(beta) != "(Intercept)"]
  separated <- length(card_beta) > 0L &&
    any(abs(card_beta) > .te_separated_at())

  list(
    fit = got$fit, fit_null = null_res$fit, reason = NA_character_,
    card_effect = got$card_effect, form = got$form, form_null = null_form,
    n_obs = nrow(d), n_missing = sum(!ok), n_groups = n_groups,
    n_blocks = nlevels(d$.block), n_cards = nlevels(d$.card),
    prevalence = n_true / nrow(d),
    sd_group = unname(got$sds[[".group"]]),
    sd_block = unname(got$sds[[".block"]]),
    sd_card = unname(got$sds[[".card"]]),
    sd_block_null = unname(null_sds[[".block"]]),
    aic = ic$aic, aic_null = null_ic$aic,
    delta_aic = ic$aic - null_ic$aic, chisq = chisq, df = df, p_value = p,
    singular = isTRUE(got$sds[[".group"]] < 1e-6),
    boundary = isTRUE(got$sds[[".group"]] > .te_wide_sd()),
    separated = separated,
    demotions = unlist(got$steps) %||% character()
  )
}

# The joint (full multinomial) check ------------------------------------------

# Stack a category's two parts into one frame and fit them together with an
# unstructured 2x2 team covariance. Because the sequential decomposition is
# an exact reparameterisation of the multinomial, this IS the full multinomial
# mixed model for that category, and the separate fits are the same model with
# the off-diagonal held at zero. The single number it adds is therefore the
# whole cost of the fallback, and reporting it is the point: it is a
# diagnostic, not a second engine, and the simulator never reads it.
.te_joint_frame <- function(md, y_hit, y_split) {
  a <- md
  a$.part <- "hit"
  a$.y <- as.integer(y_hit)
  b <- md
  b$.part <- "split"
  b$.y <- as.integer(y_split)
  out <- rbind(a, b)
  out <- out[!is.na(out$.y), , drop = FALSE]
  out$.part <- factor(out$.part, levels = .te_parts())
  out
}

.te_joint_formula <- function(card_effect, use_block) {
  rhs <- "0 + .part"
  if (identical(card_effect, "fixed")) rhs <- paste(rhs, "+ .part:.card")
  if (identical(card_effect, "random")) rhs <- paste(rhs, "+ (1 | .card)")
  if (use_block) rhs <- paste(rhs, "+ (0 + .part | .block)")
  stats::as.formula(paste(".y ~", rhs, "+ (0 + .part | .group)"))
}

.te_joint_one <- function(md, y_hit, y_split, card_effect, engine,
                          use_block) {
  none <- function(reason) {
    list(fit = NULL, ok = FALSE, reason = reason, cor_group = NA_real_,
         sd_hit = NA_real_, sd_split = NA_real_, aic = NA_real_)
  }
  d <- .te_joint_frame(md, y_hit, y_split)
  if (!nrow(d) || length(unique(d$.part)) < 2L) {
    return(none("one part has no rows"))
  }
  d$.group <- droplevels(d$.group)
  d$.block <- droplevels(d$.block)
  d$.card <- droplevels(d$.card)
  form <- .te_joint_formula(card_effect, use_block && nlevels(d$.block) > 1L)
  environment(form) <- list2env(as.list(d), parent = environment(form))
  res <- .te_try_fit(form, d, engine)
  if (is.null(res$fit)) {
    return(none(paste("fit failed:", .te_first_line(res$messages))))
  }
  diag <- .te_diagnose(res$messages)
  vc <- if (identical(engine, "lme4")) {
    lme4::VarCorr(res$fit)$.group
  } else {
    glmmTMB::VarCorr(res$fit)$cond$.group
  }
  sd <- as.numeric(attr(vc, "stddev"))
  rho <- attr(vc, "correlation")[1L, 2L]
  ic <- .te_ic(res$fit)
  reason <- NA_character_
  if (diag$rankdef) {
    reason <- "rank deficient: a card exists in one part and not the other"
  } else if (diag$nonconv) {
    reason <- "did not converge"
  } else if (any(sd > .te_wide_sd())) {
    reason <- sprintf("team SD %.1f above the boundary of %g", max(sd),
                      .te_wide_sd())
  } else if (is.finite(rho) && abs(rho) > 0.99) {
    reason <- sprintf("team covariance singular (correlation %.3f)", rho)
  }
  list(fit = res$fit, ok = is.na(reason), reason = reason,
       cor_group = rho, sd_hit = sd[[1L]], sd_split = sd[[2L]], aic = ic$aic)
}

# Argument resolution ---------------------------------------------------------

.te_resolve_card_effect <- function(x, call) {
  ladder <- .te_card_ladder()
  default <- c(hit = "fixed", split = "none")
  if (!is.character(x) || !length(x) || length(x) > 2L || anyNA(x)) {
    cli::cli_abort(
      c("{.arg card_effect} must be one or two of {.val {ladder}}.",
        "i" = "One value applies to both parts; two set
               {.field hit} then {.field split}."),
      class = "sandtablR_error_triage", call = call
    )
  }
  bad <- setdiff(x, ladder)
  if (length(bad)) {
    n_bad <- length(bad)
    cli::cli_abort(
      c("{.arg card_effect} must name card effects.",
        "x" = "{n_bad} unrecognised value{?s}: {.val {bad}}.",
        "i" = "Allowed: {.val {ladder}}."),
      class = "sandtablR_error_triage", call = call
    )
  }
  nm <- names(x)
  if (is.null(nm) || !all(nzchar(nm))) {
    if (length(x) == 1L) {
      default[] <- x
      return(default)
    }
    default[["hit"]] <- x[[1L]]
    default[["split"]] <- x[[2L]]
    return(default)
  }
  unknown <- setdiff(nm, .te_parts())
  if (length(unknown)) {
    n_unknown <- length(unknown)
    parts <- .te_parts()
    cli::cli_abort(
      c("{.arg card_effect} names {n_unknown} unknown part{?s}:
         {.val {unknown}}.",
        "i" = "Parts are {.val {parts}}."),
      class = "sandtablR_error_triage", call = call
    )
  }
  default[nm] <- unname(x)
  default
}

# Every card must carry exactly one true category, or "the truth" is not a
# property of the card and nothing downstream means what it says.
.te_card_truth <- function(cid, t_val, call) {
  map <- unique(data.frame(c = cid, t = t_val, stringsAsFactors = FALSE))
  map <- map[!is.na(map$t), , drop = FALSE]
  dup <- unique(map$c[duplicated(map$c)])
  if (length(dup)) {
    n_dup <- length(dup)
    shown <- utils::head(dup, 5L)
    cli::cli_abort(
      c("{.arg truth} must be a property of the card.",
        "x" = "{n_dup} card{?s} carry more than one true category:
               {.val {shown}}.",
        "i" = "Apply the gold standard with
               {.fn sdt_apply_gold_standard} before fitting."),
      class = "sandtablR_error_triage", call = call
    )
  }
  stats::setNames(map$t, map$c)
}

.te_check_levels <- function(t_val, e_val, call) {
  levels <- sdt_triage_levels()
  seen <- unique(c(t_val[!is.na(t_val)], e_val[!is.na(e_val)]))
  unknown <- setdiff(seen, levels)
  if (length(unknown)) {
    n_unknown <- length(unknown)
    shown <- utils::head(unknown, 5L)
    cli::cli_abort(
      c("{.arg truth} and {.arg estimate} must hold triage levels.",
        "x" = "{n_unknown} unrecognised value{?s}: {.val {shown}}.",
        "i" = "Allowed: {.val {levels}}."),
      class = "sandtablR_error_triage", call = call
    )
  }
  invisible(TRUE)
}

# Fitting ---------------------------------------------------------------------

#' Fit a model of triage error, with its between-team variance
#'
#' Models the called sighting category **conditional on the true one**, so the
#' object being fitted is a row of the confusion matrix rather than a marginal
#' rate. Where [sdt_fit_behaviour()] answers "how often does a team call red",
#' this answers "given that this casualty is red, what does a team call, and
#' how far do teams differ in getting it wrong".
#'
#' # The parameterisation
#'
#' For each true category `k` the response has three levels and therefore two
#' free logits. They are fitted as a sequential pair:
#'
#' * **hit** -- `P(called = k | true = k)`, over every row with truth `k`.
#' * **split** -- `P(called = w1 | called != k, true = k)`, over the error
#'   rows only, where `w1` is the more severe of the two remaining categories.
#'
#' Each is a binomial mixed model
#'
#' ```
#' y ~ card + (1 | block) + (1 | group)
#' ```
#'
#' This is an **exact reparameterisation** of the three-category multinomial,
#' not an approximation to it: the multinomial likelihood factorises into the
#' two binomial likelihoods, so fitting them separately fits the multinomial
#' with the team random effects of the two logits held independent. The single
#' parameter the decomposition gives up is the correlation between a team's
#' accuracy and its error direction, and `joint = TRUE` estimates exactly that
#' (see below).
#'
#' # Why the card term is load-bearing
#'
#' Cards inside one true category are not alike. On the validation corpus red
#' card 14 was called correctly by 26% of teams and 15 of the 30 red cards by
#' every team. Removing the card term does not shrink the team effect, it
#' **erases** it: the fitted team SD goes to exactly 0 for red and for yellow,
#' against 0.544 and 0.639 with the card term, and the AIC is 473 and 137
#' worse. Every group saw nearly the same deck, so the spread of raw hit rates
#' across teams is *narrower* than binomial noise around a single pooled
#' probability, and a model with one probability per category can only account
#' for that by setting the team variance to zero.
#'
#' `card_effect` therefore defaults to `"fixed"` for the hit part. It takes
#' one value for both parts, or two -- `c("fixed", "none")` sets the hit part
#' then the split part -- or a named vector such as
#' `c(hit = "fixed", split = "random")`.
#'
#' The split part defaults to `"none"`, and that is measured rather than
#' assumed. Its cells hold few error rows with a small minority (176, 124 and
#' 529 rows with 10, 24 and 38 minority calls on this corpus); a card-fixed
#' split runs the yellow team SD up to 41 on the logit scale, is worse by AIC
#' than no card term for red, and leaves any card that no team ever miscalled
#' with no coefficient at all. It also costs little, because for red both
#' wrong answers are under-triage and for green both are over-triage: the
#' split moves cells *inside* the confusion matrix without moving the
#' over- and under-triage rates.
#'
#' # The ladder, and what gets skipped
#'
#' A card term is attempted only where there are at least as many minority
#' events as card levels, and a fit is rejected if it is rank deficient, fails
#' to converge, or lands a random-effect SD above 5 on the logit scale --
#' which is separation, not a tendency. Each rejection demotes the cell one
#' rung down `"fixed"`, `"random"`, `"none"` and is recorded in the cell's
#' `demotions`. A cell that runs out of rungs, or that is constant, or that
#' has too few events or groups, appears in `$skipped` with its reason and is
#' never dropped in silence. A fit that lands a zero team variance is **kept**
#' and flagged `singular`: teams not differing is a finding, not a failure.
#'
#' # The full multinomial
#'
#' `joint = TRUE` stacks a category's two parts and fits them together with an
#' unstructured team covariance, which is the full multinomial mixed model for
#' that category. It is a **diagnostic**: it measures the one parameter the
#' default gives up, and [sdt_simulate_triage()] never reads it. Run on the
#' validation corpus it converged for green only -- correlation `-0.050`, and
#' a joint AIC equal to the sum of the two separate ones -- while red came
#' back rank deficient with a team correlation of 0.99 and yellow ran its
#' split SD to 33-40. The full multinomial does not fit reliably at this n,
#' and where it does fit it says the constraint costs nothing.
#'
#' # The team term
#'
#' Every cell is fitted **twice**: once as above and once with `(1 | group)`
#' removed from exactly the same rows. `$cells` carries `sd_group`, the null
#' fit's AIC, `delta_aic` and a likelihood-ratio test, so the necessity of the
#' team term is a measured claim per cell rather than a stated one. The null
#' fits are kept, and [sdt_simulate_triage()] can simulate from them with
#' `team_effects = FALSE`.
#'
#' @param data A data frame of decision episodes, one row per (group, card),
#'   carrying both the true and the called category -- the shape
#'   [sdt_apply_gold_standard()] returns.
#' @param truth,estimate Columns holding the true and the called sighting
#'   category. Both must hold [sdt_triage_levels()].
#' @param group,block,card Columns holding the team, the session the team
#'   played in, and the patient card. `block` may be `NULL` to fit without a
#'   session term. Group labels that repeat across blocks are nested inside
#'   their block, with a warning.
#' @param engine `"lme4"` (default) or `"glmmTMB"`.
#' @param verbose If `TRUE`, report progress via \pkg{cli}.
#' @param card_effect How the card enters each part: one or two of `"fixed"`,
#'   `"random"` and `"none"`. See the sections above.
#' @param frame `"all"` (default) or `"matched"` for the
#'   composition-matched frame of [sdt_fit_behaviour()]. The default differs
#'   from that function on purpose: there dispersion is measured off the same
#'   frame that was fitted, while here the card term is in the model and the
#'   between-team dispersion is measured separately by [sdt_error_fidelity()],
#'   which does its own matching. Matching here would cost a category its
#'   cards -- on this corpus the matched frame keeps 2 of the 5 yellow cards.
#' @param min_coverage Passed to the matched frame; `NULL` chooses it.
#' @param joint If `TRUE`, also fit the full multinomial mixed model per
#'   category as a diagnostic. Slow, and not used by the simulator.
#'
#' @return An S3 object of class `sdt_triage_error_model`:
#'   \describe{
#'     \item{`cells`}{One row per (category, part) fitted, with `n_obs`,
#'       `prevalence`, the card effect actually used, `sd_group`, `sd_block`,
#'       `sd_card`, the null fit's `sd_block_null` and `aic_null`,
#'       `delta_aic`, `chisq`, `df`, `p_value`, and the `singular`,
#'       `boundary` and `separated` flags.}
#'     \item{`fits`,`fits_null`}{Named lists of fitted models, keyed
#'       `"category.part"`, with and without the team term.}
#'     \item{`joint`}{`NULL`, or one row per category with the joint fit's
#'       team correlation, per-part SDs, AIC and why it was rejected.}
#'     \item{`terms`}{Column names, card levels, the card-to-truth lookup and
#'       the wrong-answer ordering the split refers to.}
#'     \item{`data`}{The frame actually fitted on.}
#'     \item{`meta`}{Counts, frame, engine, versions, runtime and the observed
#'       confusion matrix.}
#'     \item{`skipped`}{One row per cell not modelled, with the reason.}
#'   }
#' @seealso [sdt_simulate_triage()] to draw new teams from the fit,
#'   [sdt_error_fidelity()] to score them, [sdt_triage_accuracy()] for the
#'   observed table.
#' @export
#' @examples
#' set.seed(1)
#' cards <- sprintf("c%02d", seq_len(12))
#' truth <- rep(c("red", "yellow", "green"), each = 4)
#' d <- expand.grid(card = cards, group_id = sprintf("g%02d", seq_len(20)),
#'                  stringsAsFactors = FALSE)
#' d$session_id <- ifelse(d$group_id < "g11", "s1", "s2")
#' d$triage <- truth[match(d$card, cards)]
#' b <- stats::rnorm(20, sd = 1)
#' p <- stats::plogis(1 + b[match(d$group_id, unique(d$group_id))])
#' hit <- stats::runif(nrow(d)) < p
#' wrong <- vapply(seq_len(nrow(d)), function(i) {
#'   sample(setdiff(sdt_triage_levels(), d$triage[[i]]), 1)
#' }, character(1))
#' d$called_triage <- ifelse(hit, d$triage, wrong)
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   sdt_fit_triage_error(d, verbose = FALSE)
#' }
sdt_fit_triage_error <- function(data,
                                 truth = "triage",
                                 estimate = "called_triage",
                                 group = "group_id",
                                 block = "session_id",
                                 card = "card",
                                 engine = "lme4",
                                 verbose = TRUE,
                                 card_effect = c(hit = "fixed",
                                                 split = "none"),
                                 frame = c("all", "matched"),
                                 min_coverage = NULL,
                                 joint = FALSE) {
  call <- rlang::current_env()
  frame <- rlang::arg_match(frame)
  engine <- rlang::arg_match(engine, c("lme4", "glmmTMB"))
  .check_flag(verbose)
  .check_flag(joint)
  .check_string(truth)
  .check_string(estimate)
  .check_string(group)
  .check_string(card)
  if (!is.null(block)) .check_string(block)
  card_effect <- .te_resolve_card_effect(card_effect, call)
  if (!is.data.frame(data) || !nrow(data)) {
    cli::cli_abort("{.arg data} must be a data frame with at least one row.",
                   class = "sandtablR_error_triage", call = call)
  }
  .require_pkgs(engine)

  keys <- c(truth = truth, estimate = estimate, group = group,
            block = block, card = card)
  absent <- keys[!keys %in% names(data)]
  if (length(absent)) {
    args <- names(absent)
    n_absent <- length(absent)
    cli::cli_abort(
      c("{.arg data} is missing {n_absent} named column{?s}.",
        "x" = "Not found: {.val {unname(absent)}}.",
        "i" = "Set {.arg {args}} to the column name{?s} your frame uses."),
      class = "sandtablR_error_triage", call = call
    )
  }

  t0 <- Sys.time()
  t_val <- as.character(data[[truth]])
  e_val <- as.character(data[[estimate]])
  .te_check_levels(t_val, e_val, call)
  gid <- as.character(data[[group]])
  cid <- as.character(data[[card]])
  bid <- if (is.null(block)) {
    rep("all", nrow(data))
  } else {
    as.character(data[[block]])
  }
  if (anyNA(gid) || anyNA(cid) || anyNA(bid)) {
    n_bad <- sum(is.na(gid) | is.na(cid) | is.na(bid))
    cli::cli_abort(
      c("Key columns must not be missing.",
        "x" = "{n_bad} row{?s} have a missing group, block or card."),
      class = "sandtablR_error_triage", call = call
    )
  }
  gid <- .beh_nest_groups(gid, bid, block)

  usable <- !is.na(t_val) & !is.na(e_val)
  n_dropped <- sum(!usable)
  if (sum(usable) < 2L) {
    cli::cli_abort(
      c("Too few rows carry both a truth and a call.",
        "x" = "{sum(usable)} of {nrow(data)} row{?s} are usable."),
      class = "sandtablR_error_triage", call = call
    )
  }
  if (verbose && n_dropped > 0L) {
    cli::cli_inform(
      "Dropping {n_dropped} row{?s} with a missing true or called category."
    )
  }
  card_truth <- .te_card_truth(cid[usable], t_val[usable], call)

  fr <- if (identical(frame, "matched")) {
    .beh_matched(gid[usable], cid[usable], min_coverage, call)
  } else {
    list(min_coverage = NA_integer_, cards = sort(unique(cid[usable])),
         groups = sort(unique(gid[usable])), grid = NULL)
  }
  keep <- usable & gid %in% fr$groups & cid %in% fr$cards
  if (sum(keep) < 2L || length(fr$groups) < 2L || length(fr$cards) < 2L) {
    n_g <- length(fr$groups)
    n_c <- length(fr$cards)
    cli::cli_abort(
      c("The fitting frame is too small to model.",
        "x" = "{n_c} card{?s} and {n_g} group{?s} survived the frame."),
      class = "sandtablR_error_triage", call = call
    )
  }

  fitted <- tibble::as_tibble(data[keep, , drop = FALSE])
  fitted[[group]] <- gid[keep]
  n_blocks_all <- length(unique(bid[keep]))
  use_block <- !is.null(block) && n_blocks_all > 1L
  if (!is.null(block) && !use_block) {
    cli::cli_warn(c(
      "!" = "Only {n_blocks_all} block in {.arg data}; dropping the
             {.code (1 | block)} term.",
      "i" = "Session-level and team-level spread cannot be told apart here."
    ), class = "sandtablR_warning_triage")
  }
  md <- data.frame(
    .card = factor(cid[keep], levels = fr$cards),
    .group = factor(gid[keep], levels = sort(unique(gid[keep]))),
    .block = factor(bid[keep], levels = sort(unique(bid[keep]))),
    stringsAsFactors = FALSE
  )
  t_keep <- t_val[keep]
  e_keep <- e_val[keep]

  out <- .te_fit_all(md, t_keep, e_keep, card_effect, engine, use_block,
                     joint, verbose)
  if (!length(out$fits)) {
    cli::cli_abort(
      c("No cell could be fitted.",
        "i" = "See the {.field skipped} table for why."),
      class = "sandtablR_error_triage", call = call
    )
  }
  .te_warn_flags(out$cells)

  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (verbose) {
    n_fit <- nrow(out$cells)
    secs <- round(elapsed, 1)
    cli::cli_inform(
      "Fitted {n_fit} cell{?s} in {secs}s; skipped {nrow(out$skipped)}."
    )
  }
  structure(
    list(
      cells = out$cells,
      fits = out$fits,
      fits_null = out$fits_null,
      joint = out$joint,
      terms = list(
        truth = truth, estimate = estimate, group = group, block = block,
        card = card, card_levels = fr$cards,
        card_truth = card_truth[fr$cards],
        levels = sdt_triage_levels(),
        wrong = stats::setNames(
          lapply(sdt_triage_levels(), .te_wrong), sdt_triage_levels()
        ),
        has_block = use_block,
        card_effect = card_effect
      ),
      data = fitted,
      meta = list(
        frame = list(type = frame, min_coverage = fr$min_coverage,
                     cards = fr$cards, groups = fr$groups, grid = fr$grid,
                     block_of = .beh_block_of(gid[keep], bid[keep])),
        n_rows = sum(keep), n_rows_input = nrow(data),
        n_dropped_label = n_dropped,
        n_groups = length(unique(gid[keep])),
        n_blocks = n_blocks_all, n_cards = length(fr$cards),
        engine = engine, versions = .beh_versions(engine),
        elapsed = elapsed,
        confusion = .triage_confusion(t_keep, e_keep)
      ),
      skipped = out$skipped
    ),
    class = "sdt_triage_error_model"
  )
}

# The per-category loop, kept out of the entry point so that the entry point
# reads as validation followed by one call.
.te_fit_all <- function(md, t_val, e_val, card_effect, engine, use_block,
                        joint, verbose) {
  levels <- sdt_triage_levels()
  cells <- list()
  fits <- list()
  fits_null <- list()
  skipped <- list()
  joint_rows <- list()
  for (k in levels) {
    idx <- which(t_val == k)
    wrong <- .te_wrong(k, levels)
    if (!length(idx)) {
      for (part in .te_parts()) {
        skipped[[.te_cell_key(k, part)]] <- .te_skip_row(
          k, part, "frame", "no row carries this true category"
        )
      }
      next
    }
    md_k <- md[idx, , drop = FALSE]
    called <- e_val[idx]
    y <- list(
      hit = called == k,
      split = ifelse(called == k, NA, called == wrong[[1L]])
    )
    for (part in .te_parts()) {
      key <- .te_cell_key(k, part)
      if (verbose) {
        cli::cli_inform("Fitting {.field {key}}.")
      }
      one <- .te_fit_cell(y[[part]], md_k, card_effect[[part]], engine,
                          use_block)
      if (is.null(one$fit)) {
        skipped[[key]] <- .te_skip_row(k, part, "fit", one$reason)
        next
      }
      fits[[key]] <- one$fit
      if (!is.null(one$fit_null)) fits_null[[key]] <- one$fit_null
      cells[[key]] <- tibble::tibble(
        category = k, part = part,
        response = if (identical(part, "hit")) {
          sprintf("called %s", k)
        } else {
          sprintf("called %s rather than %s", wrong[[1L]], wrong[[2L]])
        },
        n_obs = one$n_obs, n_missing = one$n_missing,
        n_groups = one$n_groups, n_blocks = one$n_blocks,
        n_cards = one$n_cards, prevalence = one$prevalence,
        card_effect = one$card_effect,
        sd_group = one$sd_group, sd_block = one$sd_block,
        sd_card = one$sd_card, sd_block_null = one$sd_block_null,
        aic = one$aic, aic_null = one$aic_null, delta_aic = one$delta_aic,
        chisq = one$chisq, df = one$df, p_value = one$p_value,
        singular = one$singular, boundary = one$boundary,
        separated = one$separated,
        demotions = paste(one$demotions, collapse = "; ")
      )
    }
    if (joint) {
      if (verbose) {
        cli::cli_inform("Fitting the joint multinomial for {.field {k}}.")
      }
      jt <- .te_joint_one(md_k, y$hit, y$split, card_effect[["hit"]],
                          engine, use_block)
      joint_rows[[k]] <- tibble::tibble(
        category = k, converged = jt$ok, cor_group = jt$cor_group,
        sd_group_hit = jt$sd_hit, sd_group_split = jt$sd_split,
        aic = jt$aic, reason = jt$reason %||% NA_character_
      )
    }
  }
  list(
    cells = if (length(cells)) dplyr::bind_rows(cells) else tibble::tibble(),
    fits = fits, fits_null = fits_null,
    joint = if (length(joint_rows)) dplyr::bind_rows(joint_rows) else NULL,
    skipped = if (length(skipped)) {
      dplyr::bind_rows(skipped)
    } else {
      .te_empty_skips()
    }
  )
}

# Say the awkward things out loud, once, in the same voice the behaviour model
# uses for the same conditions.
.te_warn_flags <- function(cells) {
  if (!nrow(cells)) {
    return(invisible(NULL))
  }
  sing <- cells$category[cells$singular]
  if (length(sing)) {
    keys <- .te_cell_key(cells$category[cells$singular],
                         cells$part[cells$singular])
    n_sing <- length(keys)
    cli::cli_warn(c(
      "!" = "{n_sing} cell{?s} fitted a zero team variance: {.val {keys}}.",
      "x" = "Simulated teams will be identical there.",
      "i" = "That is a finding about the cell, not a failure of the fit."
    ), class = "sandtablR_warning_triage")
  }
  sep <- cells[cells$separated, , drop = FALSE]
  if (nrow(sep)) {
    keys <- .te_cell_key(sep$category, sep$part)
    n_sep <- length(keys)
    lim <- .te_separated_at()
    cli::cli_warn(c(
      "!" = "{n_sep} cell{?s} carry a card coefficient beyond {lim} on the
             logit scale: {.val {keys}}.",
      "x" = "Those cards were called the same way by every team, so a
             simulated team will never err on them.",
      "i" = "It is a true statement about the deck; read it as one."
    ), class = "sandtablR_warning_triage")
  }
  demoted <- cells[nzchar(cells$demotions), , drop = FALSE]
  if (nrow(demoted)) {
    keys <- .te_cell_key(demoted$category, demoted$part)
    n_dem <- length(keys)
    cli::cli_warn(c(
      "!" = "{n_dem} cell{?s} demoted their card term: {.val {keys}}.",
      "i" = "Read {.field cells$demotions} for what was tried and why it
             was rejected."
    ), class = "sandtablR_warning_triage")
  }
  invisible(NULL)
}

#' @export
format.sdt_triage_error_model <- function(x, ...) {
  rlang::check_dots_empty()
  m <- x$meta
  cov <- if (is.na(m$frame$min_coverage)) {
    "all rows"
  } else {
    sprintf("coverage >= %d", m$frame$min_coverage)
  }
  out <- c(
    sprintf("<sdt_triage_error_model> %s | %s frame (%s)", m$engine,
            m$frame$type, cov),
    sprintf("%d card%s x %d group%s in %d block%s = %d rows (of %d)",
            m$n_cards, .te_s(m$n_cards), m$n_groups, .te_s(m$n_groups),
            m$n_blocks, .te_s(m$n_blocks), m$n_rows, m$n_rows_input),
    sprintf("%d cell%s fitted in %.1fs:", nrow(x$cells),
            .te_s(nrow(x$cells)), m$elapsed),
    sprintf("  %-14s %5s %7s %8s %8s %8s %7s", "cell", "n", "prev",
            "sd_group", "sd_block", "card", "dAIC")
  )
  cells <- x$cells
  flag <- ifelse(cells$singular, " singular",
                 ifelse(cells$boundary, " wide", ""))
  flag <- paste0(flag, ifelse(cells$separated, " separated", ""))
  out <- c(out, sprintf(
    "  %-14s %5d %7.3f %8.3f %8s %8s %7.1f%s",
    .te_cell_key(cells$category, cells$part), cells$n_obs,
    cells$prevalence, cells$sd_group,
    ifelse(is.na(cells$sd_block), "-", sprintf("%.3f", cells$sd_block)),
    cells$card_effect, cells$delta_aic, flag
  ))
  out <- c(out, .te_format_joint(x))
  if (nrow(x$skipped)) {
    out <- c(out, sprintf("%d skipped:", nrow(x$skipped)),
             sprintf("  %-14s %s",
                     .te_cell_key(x$skipped$category, x$skipped$part),
                     x$skipped$reason))
  }
  out
}

.te_format_joint <- function(x) {
  jt <- x$joint
  if (is.null(jt) || !nrow(jt)) {
    return(character())
  }
  c(
    "joint multinomial (diagnostic only):",
    sprintf("  %-14s %9s %9s %9s %7s  %s", "category", "converged",
            "cor(team)", "sd_hit", "aic", "reason"),
    sprintf("  %-14s %9s %9s %9s %7.1f  %s", jt$category,
            ifelse(jt$converged, "yes", "no"),
            ifelse(is.na(jt$cor_group), "-",
                   sprintf("%.3f", jt$cor_group)),
            ifelse(is.na(jt$sd_group_hit), "-",
                   sprintf("%.3f", jt$sd_group_hit)),
            jt$aic, ifelse(is.na(jt$reason), "", jt$reason))
  )
}

#' @export
print.sdt_triage_error_model <- function(x, ...) {
  rlang::check_dots_empty()
  cat(format(x), sep = "\n")
  invisible(x)
}

# Simulating triage error -----------------------------------------------------

# Which fitted object, card effect and random-effect SDs a cell contributes.
# `team_effects = FALSE` switches to the null fits -- a different model, not
# the same one with the draw suppressed, because removing the team term moves
# the card effects and the session variance too.
.te_cell_spec <- function(model, key, team_effects) {
  keys <- .te_cell_key(model$cells$category, model$cells$part)
  i <- match(key, keys)
  if (is.na(i)) {
    return(NULL)
  }
  row <- model$cells[i, , drop = FALSE]
  if (team_effects) {
    return(list(fit = model$fits[[key]], card_effect = row$card_effect,
                sd_group = row$sd_group, sd_block = row$sd_block))
  }
  fit <- model$fits_null[[key]]
  if (is.null(fit)) {
    return(NULL)
  }
  list(fit = fit, card_effect = row$card_effect, sd_group = 0,
       sd_block = row$sd_block_null)
}

# Draw, or look up, one cell's team and session effects.
.te_draw_re <- function(spec, model, n_sim, new_teams, pool,
                        team_effects) {
  sd_g <- if (is.na(spec$sd_group)) 0 else spec$sd_group
  sd_b <- if (is.na(spec$sd_block)) 0 else spec$sd_block
  if (new_teams) {
    return(list(group = stats::rnorm(n_sim, sd = sd_g),
                block = stats::rnorm(n_sim, sd = sd_b),
                source = NULL))
  }
  re <- .te_ranef(spec$fit, model$meta$engine)
  n <- length(pool)
  # A fit can be missing a term entirely -- the null fits have no team effect,
  # and a cell that fell back to `glm()` has neither -- so a mode that is not
  # there is zero rather than a silently recycled NA.
  look <- function(u, keys) {
    if (is.null(u) || !length(keys)) {
      return(rep(0, n))
    }
    v <- unname(u[match(keys, names(u))])
    v[is.na(v)] <- 0
    if (length(v) != n) rep(0, n) else v
  }
  g <- if (team_effects) look(re$.group, pool) else rep(0, n)
  b <- look(re$.block, unname(model$meta$frame$block_of[pool]))
  list(group = g, block = b, source = pool)
}

.te_group_pool <- function(model, new_teams, n_groups) {
  if (new_teams) {
    return(list(n_groups = n_groups, ids = NULL))
  }
  fitted_groups <- model$meta$frame$groups
  n_avail <- length(fitted_groups)
  if (n_groups > n_avail) {
    cli::cli_warn(c(
      "!" = "{.code new_teams = FALSE} reuses fitted groups, and this fit
             has {n_avail} of them.",
      "i" = "Simulating {n_avail} instead of {n_groups}."
    ), class = "sandtablR_warning_triage")
    n_groups <- n_avail
  }
  list(n_groups = n_groups, ids = fitted_groups[seq_len(n_groups)])
}

#' Simulate triage calls from a fitted error model
#'
#' Draws teams that were never in the data and, for each simulated team and
#' each card, a **called** sighting category given the card's **true** one. It
#' is the generative counterpart of [sdt_fit_triage_error()]: where that fits
#' the rows of the confusion matrix, this replays them.
#'
#' Each simulated team gets its own random effect per cell, drawn from
#' `N(0, sd_group)`, plus a session effect from `N(0, sd_block)`. For a card
#' of true category `k` the call is then a two-step draw: correct with
#' probability `plogis(card effect + team effect + session effect)` from the
#' `hit` cell, and otherwise one of the two wrong categories from the `split`
#' cell. That is the confusion-matrix row, redrawn for a new team.
#'
#' # What comes back as `NA`
#'
#' A cell that was never fitted is never guessed. If a category's `hit` cell
#' is missing, every card of that category returns `NA`; if only its `split`
#' cell is missing, the correct calls stand and the errors return `NA`. A card
#' with no coefficient in a cell -- a card the split part never saw, because
#' no team miscalled it -- returns `NA` for the same reason. Every one of
#' these is counted in the result's `gaps` attribute and warned about once.
#'
#' # What is not reproduced
#'
#' The two parts are drawn independently, so a simulated team's accuracy and
#' its error direction are uncorrelated. That is the one parameter the
#' decomposition gives up, and `joint = TRUE` in [sdt_fit_triage_error()]
#' measures it. Card-to-card dependence within a team -- a team that has just
#' over-triaged three greens in a row -- is absent as well: cards are drawn
#' independently given the team.
#'
#' @param model An `sdt_triage_error_model` from [sdt_fit_triage_error()].
#' @param n_groups Number of simulated teams per replicate.
#' @param cards Cards to deal to each team. `NULL` uses every card in the
#'   fitted frame; anything passed must be a subset of them.
#' @param seed Optional integer seed, applied with [withr::local_seed()].
#' @param new_teams If `TRUE` (default), redraw the team and session effects,
#'   so every simulated team is new. If `FALSE`, reuse the fitted conditional
#'   modes of the first `n_groups` fitted teams, which is a
#'   posterior-predictive check rather than a generator.
#' @param nsim Number of replicates. Replicates are stacked and carry a
#'   `.replicate` column; `sim_group` is unique across the whole result.
#' @param team_effects If `FALSE`, simulate from the null fits -- the same
#'   cells with `(1 | group)` removed. This is the counterfactual that shows
#'   what the team term is doing, and it is a different model rather than the
#'   same one with the draw switched off.
#'
#' @return An S3 object of class `sdt_simulated_triage`, a
#'   [tibble][tibble::tibble] with `.replicate`, `sim_group`, `card`,
#'   `triage`, `called_triage`, and the `hit`, `over` and `under` logicals.
#'   Carries the `model`, the `call`, a `draws` attribute holding the random
#'   effects used, and a `gaps` attribute counting the cells left `NA`.
#' @seealso [sdt_fit_triage_error()], [sdt_error_fidelity()].
#' @export
#' @examples
#' set.seed(1)
#' cards <- sprintf("c%02d", seq_len(12))
#' truth <- rep(c("red", "yellow", "green"), each = 4)
#' d <- expand.grid(card = cards, group_id = sprintf("g%02d", seq_len(20)),
#'                  stringsAsFactors = FALSE)
#' d$session_id <- ifelse(d$group_id < "g11", "s1", "s2")
#' d$triage <- truth[match(d$card, cards)]
#' b <- stats::rnorm(20, sd = 1)
#' p <- stats::plogis(1 + b[match(d$group_id, unique(d$group_id))])
#' hit <- stats::runif(nrow(d)) < p
#' wrong <- vapply(seq_len(nrow(d)), function(i) {
#'   sample(setdiff(sdt_triage_levels(), d$triage[[i]]), 1)
#' }, character(1))
#' d$called_triage <- ifelse(hit, d$triage, wrong)
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   fit <- sdt_fit_triage_error(d, verbose = FALSE)
#'   sdt_simulate_triage(fit, n_groups = 5, seed = 1)
#' }
sdt_simulate_triage <- function(model,
                                n_groups = 20L,
                                cards = NULL,
                                seed = NULL,
                                new_teams = TRUE,
                                nsim = 1L,
                                team_effects = TRUE) {
  call <- rlang::current_env()
  .check_class(model, "sdt_triage_error_model")
  n_groups <- .check_count(n_groups)
  nsim <- .check_count(nsim)
  .check_flag(new_teams)
  .check_flag(team_effects)
  if (!is.null(seed)) {
    withr::local_seed(.check_count(seed))
  }
  cards <- .te_resolve_cards(model, cards, call)
  pool <- .te_group_pool(model, new_teams, n_groups)
  n_groups <- pool$n_groups

  reps <- lapply(seq_len(nsim), function(r) {
    .te_simulate_one(model, cards, n_groups, new_teams, pool, r,
                     team_effects)
  })
  out <- dplyr::bind_rows(lapply(reps, `[[`, "sim"))
  draws <- dplyr::bind_rows(lapply(reps, `[[`, "draws"))
  gaps <- .te_gap_table(out, model)
  .te_warn_gaps(gaps)

  structure(
    tibble::as_tibble(out),
    class = c("sdt_simulated_triage", class(tibble::tibble())),
    model = model, call = sys.call(), draws = draws, gaps = gaps,
    seed = seed, new_teams = new_teams, team_effects = team_effects
  )
}

.te_resolve_cards <- function(model, cards, call) {
  levels_fit <- model$terms$card_levels
  if (is.null(cards)) {
    return(levels_fit)
  }
  cards <- as.character(cards)
  unknown <- setdiff(unique(cards), levels_fit)
  if (length(unknown)) {
    n_unknown <- length(unknown)
    shown <- utils::head(unknown, 5L)
    cli::cli_abort(
      c("{.arg cards} must be a subset of the fitted frame's cards.",
        "x" = "{n_unknown} unknown card{?s}: {.val {shown}}."),
      class = "sandtablR_error_triage", call = call
    )
  }
  unique(cards)
}

.te_simulate_one <- function(model, cards, n_groups, new_teams, pool, rep_id,
                             team_effects) {
  truth <- unname(model$terms$card_truth[cards])
  n_cards <- length(cards)
  ids <- sprintf("g%04d", (rep_id - 1L) * n_groups + seq_len(n_groups))
  sim <- tibble::tibble(
    .replicate = rep(rep_id, n_groups * n_cards),
    sim_group = rep(ids, each = n_cards),
    card = rep(cards, times = n_groups),
    triage = rep(truth, times = n_groups),
    called_triage = NA_character_
  )
  idx <- rep(seq_len(n_groups), each = n_cards)
  draws <- list()
  for (k in model$terms$levels) {
    rows <- which(sim$triage == k)
    if (!length(rows)) next
    wrong <- model$terms$wrong[[k]]
    hit <- .te_cell_draw(model, .te_cell_key(k, "hit"), sim$card[rows],
                         n_groups, new_teams, pool, team_effects, idx[rows])
    if (is.null(hit)) next
    draws[[.te_cell_key(k, "hit")]] <- .te_draw_rows(
      rep_id, ids, k, "hit", hit$re
    )
    called <- rep(NA_character_, length(rows))
    called[!is.na(hit$y) & hit$y] <- k
    miss <- which(!is.na(hit$y) & !hit$y)
    if (length(miss)) {
      split <- .te_cell_draw(model, .te_cell_key(k, "split"),
                             sim$card[rows][miss], n_groups, new_teams, pool,
                             team_effects, idx[rows][miss])
      if (!is.null(split)) {
        draws[[.te_cell_key(k, "split")]] <- .te_draw_rows(
          rep_id, ids, k, "split", split$re
        )
        called[miss] <- ifelse(is.na(split$y), NA_character_,
                               ifelse(split$y, wrong[[1L]], wrong[[2L]]))
      }
    }
    sim$called_triage[rows] <- called
  }
  rank <- .triage_rank()
  # `rank[...]` is a named lookup, and the names would ride into the tibble
  # as a per-row attribute nothing downstream wants.
  sim$hit <- sim$called_triage == sim$triage
  sim$over <- unname(rank[sim$called_triage] < rank[sim$triage])
  sim$under <- unname(rank[sim$called_triage] > rank[sim$triage])
  list(sim = sim, draws = dplyr::bind_rows(draws))
}

# One cell's Bernoulli draw for a set of rows, with the random effects it
# used, so the caller can record them.
.te_cell_draw <- function(model, key, row_cards, n_groups, new_teams, pool,
                          team_effects, row_group) {
  spec <- .te_cell_spec(model, key, team_effects)
  if (is.null(spec)) {
    return(NULL)
  }
  re <- .te_draw_re(spec, model, n_groups, new_teams, pool$ids,
                    team_effects)
  cards <- unique(row_cards)
  eta_card <- .te_card_eta(spec$fit, model$meta$engine, spec$card_effect,
                           cards)
  eta <- eta_card[match(row_cards, cards)] + re$group[row_group] +
    re$block[row_group]
  y <- rep(NA, length(eta))
  ok <- !is.na(eta)
  y[ok] <- stats::rbinom(sum(ok), 1L, stats::plogis(eta[ok])) == 1L
  list(y = as.logical(y), re = re)
}

.te_draw_rows <- function(rep_id, ids, category, part, re) {
  tibble::tibble(
    .replicate = rep_id, sim_group = ids, category = category, part = part,
    source_group = re$source %||% NA_character_,
    b_group = re$group, b_block = re$block
  )
}

# Where the simulation had nothing to say. Counted rather than filled in.
.te_gap_table <- function(sim, model) {
  na <- is.na(sim$called_triage)
  if (!any(na)) {
    return(tibble::tibble(category = character(), n_rows = integer(),
                          n_cards = integer()))
  }
  s <- sim[na, , drop = FALSE]
  tab <- lapply(split(s, s$triage), function(part) {
    tibble::tibble(category = part$triage[[1L]], n_rows = nrow(part),
                   n_cards = dplyr::n_distinct(part$card))
  })
  dplyr::bind_rows(tab)
}

.te_warn_gaps <- function(gaps) {
  if (!nrow(gaps)) {
    return(invisible(NULL))
  }
  n_rows <- sum(gaps$n_rows)
  cats <- gaps$category
  cli::cli_warn(c(
    "!" = "{n_rows} simulated call{?s} came back {.code NA}.",
    "x" = "True categories affected: {.val {cats}}.",
    "i" = "A cell that was never fitted is carried as {.code NA} rather
           than guessed; see the {.field gaps} attribute."
  ), class = "sandtablR_warning_triage")
}

#' @export
format.sdt_simulated_triage <- function(x, ...) {
  rlang::check_dots_empty()
  n_g <- dplyr::n_distinct(x$sim_group)
  n_c <- dplyr::n_distinct(x$card)
  n_r <- dplyr::n_distinct(x$.replicate)
  kind <- if (isTRUE(attr(x, "new_teams"))) "new teams" else "fitted teams"
  if (!isTRUE(attr(x, "team_effects"))) kind <- paste(kind, "(no team term)")
  gaps <- attr(x, "gaps")
  out <- c(
    sprintf("<sdt_simulated_triage> %d %s x %d card%s in %d replicate%s",
            n_g, kind, n_c, .te_s(n_c), n_r, .te_s(n_r)),
    sprintf("hit %.3f  over %.3f  under %.3f",
            mean(x$hit, na.rm = TRUE), mean(x$over, na.rm = TRUE),
            mean(x$under, na.rm = TRUE))
  )
  if (!is.null(gaps) && nrow(gaps)) {
    out <- c(out, sprintf("%d call%s not simulated (%s)", sum(gaps$n_rows),
                          .te_s(sum(gaps$n_rows)),
                          paste(gaps$category, collapse = ", ")))
  }
  out
}

#' @export
print.sdt_simulated_triage <- function(x, ...) {
  cat(format(x), sep = "\n")
  y <- x
  class(y) <- setdiff(class(y), "sdt_simulated_triage")
  print(y, ...)
  invisible(x)
}

# Scoring the error simulation ------------------------------------------------
#
# This is a separate function from `sdt_fidelity()` rather than an argument to
# it, and the reason is structural rather than stylistic.
#
# `sdt_fidelity()` scores UNCONDITIONAL per-group rates. Its statistic
# contract is one value per row of the frame, and `.fid_rates()` then enforces
# composition matching on the VALUE: a group is dropped for a statistic unless
# its non-missing cells cover every shared card. That rule is right for what
# it was built for, and it makes a conditional statistic impossible. "Hit rate
# among the red cards" is undefined -- necessarily NA -- on every card that is
# not red, so every group fails the coverage test on both sides. Measured:
# handed that statistic, `sdt_fidelity()` comes back with n_obs = 0,
# n_obs_rows = 0, an NA ratio and the reason "not evaluable". It is not that
# the answer would be wrong; there is no answer.
#
# The conditional statistics also need a different denominator per category
# (30 red cards against 5 yellow ones in this deck), a per-category
# composition check of their own, and a pair of ratios per statistic rather
# than one -- the level AND the between-team SD. So the frame matching, the
# statistic catalogue and the row shape all differ. What is shared is the
# verdict: the same 0.90-1.15 band and the same location tolerance, read from
# `sdt_fidelity()`'s own helpers so the two tables cannot drift apart.

.te_stats <- function() c("hit", "over", "under")

# Which (category, statistic) pairs are zero by construction. Over-triage is
# impossible on the most severe category and under-triage on the least, so
# both sides are flat and a ratio would be 0/0. They are reported as excluded
# rather than failed.
.te_structural <- function(category, statistic, levels = sdt_triage_levels()) {
  if (identical(category, "overall")) {
    return(FALSE)
  }
  (identical(statistic, "over") && identical(category, levels[[1L]])) ||
    (identical(statistic, "under") &&
       identical(category, levels[[length(levels)]]))
}

.te_stat_label <- function(category, statistic) {
  paste(category, statistic)
}

# The per-group rate of one statistic inside one true category, split by
# replicate. A group is dropped unless its non-missing calls cover every card
# of that category in the shared frame -- the same rule `.fid_rates()` applies
# to the unconditional case, applied to the conditional denominator.
.te_rates <- function(df, category, statistic, cards_of) {
  cards <- if (identical(category, "overall")) {
    unlist(cards_of, use.names = FALSE)
  } else {
    cards_of[[category]]
  }
  if (!length(cards)) {
    return(NULL)
  }
  keep <- df$.te_card %in% cards & !is.na(df[[statistic]])
  d <- df[keep, , drop = FALSE]
  if (!nrow(d)) {
    return(NULL)
  }
  unit <- paste(d$.te_group, d$.te_rep, sep = "\r")
  covered <- tapply(d$.te_card, unit, function(z) length(unique(z)))
  good <- names(covered)[!is.na(covered) & covered == length(cards)]
  n_incomplete <- length(covered) - length(good)
  d <- d[unit %in% good, , drop = FALSE]
  if (!nrow(d)) {
    return(NULL)
  }
  v <- as.numeric(d[[statistic]])
  by_rep <- split(seq_along(v), d$.te_rep)
  rates <- lapply(by_rep, function(i) {
    means <- tapply(v[i], d$.te_group[i], mean)
    stats::setNames(as.numeric(means), names(means))
  })
  list(rates = rates, n_rows = nrow(d), n_cards = length(cards),
       n_incomplete = n_incomplete)
}

# Both ratios for one statistic, from one pair of rate lists.
.te_two_ratios <- function(obs, sim) {
  o_sd <- .fid_sd(obs$rates)
  s_sd <- .fid_sd(sim$rates)
  o_mu <- .fid_mean(obs$rates)
  s_mu <- .fid_mean(sim$rates)
  list(
    level = list(observed = o_mu, simulated = s_mu,
                 ratio = .fid_ratio(o_mu, s_mu)),
    sd = list(observed = o_sd, simulated = s_sd,
              ratio = .fid_ratio(o_sd, s_sd)),
    location_z = if (is.na(o_sd) || o_sd == 0) {
      NA_real_
    } else {
      (s_mu - o_mu) / o_sd
    }
  )
}

.te_boot <- function(obs, sim, n_boot) {
  blank <- list(level = c(NA_real_, NA_real_), sd = c(NA_real_, NA_real_))
  if (n_boot < 1L) {
    return(blank)
  }
  draws <- vapply(seq_len(n_boot), function(i) {
    o <- .fid_resample(obs$rates)
    s <- .fid_resample(sim$rates)
    c(.fid_ratio(.fid_mean(o), .fid_mean(s)),
      .fid_ratio(.fid_sd(o), .fid_sd(s)))
  }, numeric(2))
  q <- function(v) {
    if (all(is.na(v))) {
      return(c(NA_real_, NA_real_))
    }
    unname(stats::quantile(v, c(0.025, 0.975), na.rm = TRUE, names = FALSE))
  }
  list(level = q(draws[1L, ]), sd = q(draws[2L, ]))
}

# Resolving the two frames ----------------------------------------------------

.te_fid_prepare <- function(df, group, card, truth, estimate, rep_col,
                            call) {
  out <- tibble::as_tibble(df)
  out$.te_card <- as.character(out[[card]])
  out$.te_group <- as.character(out[[group]])
  out$.te_rep <- if (is.null(rep_col)) {
    "1"
  } else {
    as.character(out[[rep_col]])
  }
  # A group label reused across replicates is not one team.
  if (!is.null(rep_col)) {
    out$.te_group <- paste(out$.te_rep, out$.te_group, sep = "\r")
  }
  t_val <- as.character(out[[truth]])
  e_val <- as.character(out[[estimate]])
  .te_check_levels(t_val, e_val, call)
  rank <- .triage_rank()
  out$.te_truth <- t_val
  out$hit <- e_val == t_val
  out$over <- unname(rank[e_val] < rank[t_val])
  out$under <- unname(rank[e_val] > rank[t_val])
  out[!is.na(t_val), , drop = FALSE]
}

# The composition-matched frame, and the per-category card sets it implies.
# Both sides must agree about which category each shared card belongs to, or
# the whole comparison is between two different decks wearing the same labels.
.te_fid_match <- function(obs, sim, cards, min_coverage, call) {
  shared <- intersect(unique(obs$.te_card), unique(sim$.te_card))
  if (length(shared) < 2L) {
    n_shared <- length(shared)
    cli::cli_abort(
      c("The two frames share {n_shared} card{?s}.",
        "i" = "Between-team dispersion needs at least 2 shared cards.",
        "x" = "Check that {.arg sim} and {.arg observed} use the same card
               labels."),
      class = "sandtablR_error_triage", call = call
    )
  }
  # Real play is truncated by the clock, so observed teams saw different
  # subsets of the deck while every simulated team is dealt all of it.
  # Requiring the full intersection would therefore keep only the handful of
  # observed teams that finished, so the card set is chosen the way
  # `sdt_fit_behaviour()` chooses it: maximise cards times complete teams.
  cards <- if (is.null(cards)) {
    inside <- obs$.te_card %in% shared
    .beh_matched(obs$.te_group[inside], obs$.te_card[inside], min_coverage,
                 call)$cards
  } else {
    unknown <- setdiff(as.character(cards), shared)
    if (length(unknown)) {
      n_unknown <- length(unknown)
      shown <- utils::head(unknown, 5L)
      cli::cli_abort(
        c("{.arg cards} must be shared by both frames.",
          "x" = "{n_unknown} not shared: {.val {shown}}."),
        class = "sandtablR_error_triage", call = call
      )
    }
    unique(as.character(cards))
  }
  if (length(cards) < 2L) {
    n_cards <- length(cards)
    cli::cli_abort(
      c("The matched frame kept {n_cards} card{?s}.",
        "i" = "Between-team dispersion needs at least 2."),
      class = "sandtablR_error_triage", call = call
    )
  }
  truth_of <- function(df) {
    map <- unique(data.frame(c = df$.te_card, t = df$.te_truth,
                             stringsAsFactors = FALSE))
    stats::setNames(map$t, map$c)[cards]
  }
  t_obs <- truth_of(obs)
  t_sim <- truth_of(sim)
  clash <- cards[!is.na(t_obs) & !is.na(t_sim) & t_obs != t_sim]
  if (length(clash)) {
    n_clash <- length(clash)
    shown <- utils::head(clash, 5L)
    cli::cli_abort(
      c("The two frames disagree about the true category of
         {n_clash} card{?s}.",
        "x" = "First: {.val {shown}}.",
        "i" = "Score a simulation against the frame its model was fitted
               on, or apply the same gold standard to both."),
      class = "sandtablR_error_triage", call = call
    )
  }
  complete <- function(df) {
    d <- df[df$.te_card %in% cards, , drop = FALSE]
    unit <- paste(d$.te_group, d$.te_rep, sep = "\r")
    seen <- tapply(d$.te_card, unit, function(z) length(unique(z)))
    keep <- names(seen)[!is.na(seen) & seen == length(cards)]
    d[unit %in% keep, , drop = FALSE]
  }
  obs_m <- complete(obs)
  sim_m <- complete(sim)
  n_obs <- dplyr::n_distinct(obs_m$.te_group)
  n_sim <- dplyr::n_distinct(sim_m$.te_group)
  if (n_obs < 2L || n_sim < 2L) {
    n_cards <- length(cards)
    cli::cli_abort(
      c("Too few complete teams to measure dispersion.",
        "x" = "{n_obs} observed and {n_sim} simulated team{?s} saw all
               {n_cards} shared cards.",
        "i" = "Both sides need at least 2."),
      class = "sandtablR_error_triage", call = call
    )
  }
  cards_of <- split(cards, unname(t_obs))
  frame <- list(
    cards = sort(cards), n_cards = length(cards),
    cards_per_category = vapply(
      stats::setNames(sdt_triage_levels(), sdt_triage_levels()),
      function(k) length(cards_of[[k]] %||% character()), integer(1)
    ),
    n_obs_groups = n_obs, n_sim_groups = n_sim,
    n_sim_reps = dplyr::n_distinct(sim_m$.te_rep),
    n_sim_groups_per_rep = as.integer(round(
      n_sim / max(1L, dplyr::n_distinct(sim_m$.te_rep))
    )),
    dropped_obs_groups = dplyr::n_distinct(obs$.te_group) - n_obs,
    dropped_sim_groups = dplyr::n_distinct(sim$.te_group) - n_sim,
    n_shared_cards = length(shared),
    dropped_cards = length(union(unique(obs$.te_card),
                                 unique(sim$.te_card))) - length(cards)
  )
  list(obs = obs_m, sim = sim_m, cards_of = cards_of, frame = frame)
}

# A category the matched frame has barely any cards of gives a per-team rate
# with two or three possible values, and a between-team SD computed from it is
# a statement about the deck as much as about the teams.
.te_warn_thin <- function(frame, min_cards = 3L) {
  thin <- frame$cards_per_category[frame$cards_per_category > 0L &
                                     frame$cards_per_category < min_cards]
  if (!length(thin)) {
    return(invisible(NULL))
  }
  n_thin <- length(thin)
  detail <- paste(sprintf("%s (%d)", names(thin), thin), collapse = ", ")
  cli::cli_warn(c(
    "!" = "{n_thin} true categor{?y/ies} with fewer than {min_cards} cards
           in the matched frame: {detail}.",
    "i" = "A team's rate there can only take a handful of values, so its
           between-team SD is coarse; read those rows with that in mind."
  ), class = "sandtablR_warning_triage")
}

#' Score how faithfully a simulation reproduces triage error
#'
#' Compares simulated triage calls against observed ones **per true category**,
#' and reports two ratios for every statistic: the level and the between-team
#' standard deviation, each as observed over simulated, each judged inside the
#' same 0.90-1.15 band the rest of the package uses. Getting the average error
#' rate right while making every team err identically is the failure this
#' function exists to catch, so the level alone is never enough.
#'
#' # Why this is not an argument to `sdt_fidelity()`
#'
#' [sdt_fidelity()] scores unconditional per-group rates, and enforces
#' composition matching on the value: a group is dropped for a statistic
#' unless its non-missing cells cover every shared card. A conditional
#' statistic -- the hit rate among the red cards -- is undefined on every card
#' that is not red, so under that rule every group is dropped on both sides.
#' Handed such a statistic, `sdt_fidelity()` does not fail loudly; it returns
#' the row with `n_obs = 0`, `n_obs_rows = 0`, an `NA` ratio and the reason
#' `"not evaluable"`, which is the correct answer to the question it was
#' asked and no answer at all to this one. The conditional case also needs a
#' different denominator per category, a per-category composition check, and
#' two ratios per statistic rather than one. What the two share is the
#' verdict: the band and the location tolerance are read from
#' [sdt_fidelity()]'s own helpers, so they cannot drift apart.
#'
#' # What is scored
#'
#' For each true category, and for the pooled `"overall"` row, the hit rate,
#' the over-triage rate and the under-triage rate. Direction is the clinically
#' meaningful part: over-triage spends a scarce resource, under-triage can
#' cost a life, and a single accuracy figure hides which one a team is doing.
#'
#' Two pairs are zero by construction and are excluded rather than failed:
#' over-triage on the most severe category and under-triage on the least,
#' since there is nothing above red or below green to move to. They are listed
#' in the `structural` attribute. Note also that for those same two categories
#' the remaining error rate is the exact complement of the hit rate, so
#' `red under` and `green over` carry the same information as `red hit` and
#' `green hit`; they are reported because the direction of error is what a
#' reader is looking for, not because they are independent evidence.
#'
#' # The matched frame
#'
#' Real play is truncated by the clock, so observed teams saw different
#' subsets of the deck while every simulated team is dealt all of it. Keeping
#' only the teams that saw the whole intersection would therefore keep the
#' handful that finished, so the card set is chosen the way
#' [sdt_fit_behaviour()] chooses it: the threshold that maximises cards times
#' complete teams, with ties going to the frame that keeps more teams. Pass
#' `cards` to fix the set by hand, or `min_coverage` to fix the threshold.
#'
#' The two sides must then agree about which true category each kept card
#' carries. A category left with fewer than three cards is warned about: a
#' team's rate there can take only a handful of values, so its between-team SD
#' is as much a property of the deck as of the teams.
#'
#' @param sim Simulated calls: an `sdt_simulated_triage` object from
#'   [sdt_simulate_triage()], or a data frame with one row per (team, card)
#'   carrying a true and a called category.
#' @param observed The observed calls, in the same shape. Optional when `sim`
#'   carries its fitted model, in which case the model's own fitting frame is
#'   used.
#' @param truth,estimate,group,card Column names. `NULL` takes them from the
#'   fitted model where there is one, and otherwise from the usual candidates.
#' @param cards Optional character vector fixing the matched frame's cards.
#'   `NULL` chooses them, as described above.
#' @param min_coverage Minimum number of observed teams a card must be seen
#'   by to enter the matched frame. `NULL` chooses it from the data.
#' @param n_boot Bootstrap replicates for the intervals. `0` skips them.
#' @param seed Optional integer seed, for a reproducible bootstrap.
#'
#' @return An S3 object of class `sdt_error_fidelity`, a
#'   [tibble][tibble::tibble] with one row per (statistic, measure):
#'   `statistic`, `category`, `quantity`, `measure` (`"level"` or `"sd"`),
#'   `observed`, `simulated`, `ratio`, `ci_lower`, `ci_upper`, `verdict`,
#'   `control`, `n_obs`, `n_sim`, `location_z`, `overlap`, the four percentile
#'   columns, `reason`, `n_cards`, `n_obs_rows`, `n_sim_rows` and
#'   `n_sim_reps`. Carries `band`, `location_tol`, `frame`, `structural`,
#'   `group_rates` and `n_boot` attributes. The column set is a superset of
#'   what [sdt_plot_fidelity()] needs, so the table can be drawn with it.
#' @seealso [sdt_simulate_triage()], [sdt_fidelity()],
#'   [sdt_triage_accuracy()].
#' @export
#' @examples
#' set.seed(1)
#' cards <- sprintf("c%02d", seq_len(12))
#' truth <- rep(c("red", "yellow", "green"), each = 4)
#' d <- expand.grid(card = cards, group_id = sprintf("g%02d", seq_len(20)),
#'                  stringsAsFactors = FALSE)
#' d$session_id <- ifelse(d$group_id < "g11", "s1", "s2")
#' d$triage <- truth[match(d$card, cards)]
#' b <- stats::rnorm(20, sd = 1)
#' p <- stats::plogis(1 + b[match(d$group_id, unique(d$group_id))])
#' hit <- stats::runif(nrow(d)) < p
#' wrong <- vapply(seq_len(nrow(d)), function(i) {
#'   sample(setdiff(sdt_triage_levels(), d$triage[[i]]), 1)
#' }, character(1))
#' d$called_triage <- ifelse(hit, d$triage, wrong)
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   fit <- sdt_fit_triage_error(d, verbose = FALSE)
#'   sim <- sdt_simulate_triage(fit, n_groups = 20, seed = 1)
#'   sdt_error_fidelity(sim, n_boot = 50, seed = 1)
#' }
sdt_error_fidelity <- function(sim, observed = NULL, truth = NULL,
                               estimate = NULL, group = NULL, card = NULL,
                               cards = NULL, min_coverage = NULL,
                               n_boot = 200L, seed = NULL) {
  call <- rlang::current_env()
  n_boot <- .fid_check_boot(n_boot)
  if (!is.null(seed)) withr::local_seed(.check_count(seed))
  if (!is.data.frame(sim)) {
    cli::cli_abort(
      c("{.arg sim} must be a data frame.",
        "x" = "Got {.cls {class(sim)[[1]]}} instead."),
      class = "sandtablR_error_triage", call = call
    )
  }
  model <- attr(sim, "model")
  obs <- observed %||% model$data
  if (is.null(obs)) {
    cli::cli_abort(
      c("Cannot find observed calls to score {.arg sim} against.",
        "i" = "Pass them as {.arg observed}.",
        "i" = "An {.cls sdt_simulated_triage} object carries its model, and
               so needs no {.arg observed}."),
      class = "sandtablR_error_triage", call = call
    )
  }
  if (!is.data.frame(obs)) {
    cli::cli_abort(
      c("{.arg observed} must be a data frame.",
        "x" = "Got {.cls {class(obs)[[1]]}} instead."),
      class = "sandtablR_error_triage", call = call
    )
  }
  obs <- tibble::as_tibble(obs)

  cols <- .te_fid_columns(sim, obs, model, truth, estimate, group, card,
                          call)
  rep_col <- intersect(.fid_rep_names(), names(sim))
  rep_col <- if (length(rep_col)) rep_col[[1L]] else NULL
  sim_p <- .te_fid_prepare(sim, cols$sim_group, cols$sim_card, cols$truth,
                           cols$estimate, rep_col, call)
  obs_p <- .te_fid_prepare(obs, cols$obs_group, cols$obs_card, cols$truth,
                           cols$estimate, NULL, call)
  matched <- .te_fid_match(obs_p, sim_p, cards, min_coverage, call)
  .te_warn_thin(matched$frame)

  rows <- list()
  rates <- list()
  structural <- character()
  for (k in c(sdt_triage_levels(), "overall")) {
    if (!identical(k, "overall") &&
          !length(matched$cards_of[[k]] %||% character())) {
      next
    }
    for (st in .te_stats()) {
      lab <- .te_stat_label(k, st)
      if (.te_structural(k, st)) {
        structural <- c(structural, lab)
        next
      }
      o <- .te_rates(matched$obs, k, st, matched$cards_of)
      s <- .te_rates(matched$sim, k, st, matched$cards_of)
      if (is.null(o) || is.null(s)) {
        next
      }
      rows[[lab]] <- .te_fid_rows(lab, k, st, o, s, n_boot,
                                  matched$frame$n_sim_reps)
      rates[[lab]] <- .te_fid_rate_rows(lab, o, s)
    }
  }
  if (!length(rows)) {
    cli::cli_abort(
      c("No statistic could be scored.",
        "i" = "The matched frame left no category with cards on both
               sides."),
      class = "sandtablR_error_triage", call = call
    )
  }
  out <- dplyr::bind_rows(rows)
  out <- out[order(out$verdict != "FAIL", out$category, out$quantity,
                   out$measure), , drop = FALSE]
  new_sdt_error_fidelity(
    tibble::as_tibble(out), frame = matched$frame,
    structural = structural, rates = dplyr::bind_rows(rates),
    n_boot = n_boot
  )
}

new_sdt_error_fidelity <- function(x, frame, structural, rates, n_boot) {
  structure(
    x, class = c("sdt_error_fidelity", class(tibble::tibble())),
    band = .fid_band(), location_tol = .fid_location_tol(),
    frame = frame, structural = structural, group_rates = rates,
    n_boot = n_boot
  )
}

# Two rows -- level and SD -- for one statistic.
.te_fid_rows <- function(lab, category, quantity, o, s, n_boot, n_reps) {
  parts <- .te_two_ratios(o, s)
  ci <- .te_boot(o, s, n_boot)
  band <- .fid_band()
  o_flat <- unlist(o$rates, use.names = FALSE)
  s_flat <- unlist(s$rates, use.names = FALSE)
  one <- function(measure) {
    p <- parts[[measure]]
    ok <- !is.na(p$ratio) && is.finite(p$ratio) &&
      p$ratio >= band[[1L]] && p$ratio <= band[[2L]]
    # The verdict is the band test on this row's own ratio and nothing else.
    # `location_z` is reported beside it because a ratio of two small rates
    # moves a long way on a difference a reader would call negligible, but it
    # is context rather than a second gate: gating on both would fail the
    # level row twice for one discrepancy.
    reason <- if (ok) character() else measure
    if (is.na(p$ratio)) reason <- "not evaluable"
    tibble::tibble(
      statistic = sprintf("%s (%s)", lab, measure),
      category = category, quantity = quantity, measure = measure,
      observed = p$observed, simulated = p$simulated, ratio = p$ratio,
      ci_lower = ci[[measure]][[1L]], ci_upper = ci[[measure]][[2L]],
      verdict = if (ok) "PASS" else "FAIL",
      control = FALSE,
      n_obs = length(o_flat),
      n_sim = as.integer(round(length(s_flat) / max(1L, length(s$rates)))),
      location_z = parts$location_z,
      overlap = .fid_overlap(o_flat, s_flat),
      observed_p10 = .fid_q(o_flat, 0.1), observed_p90 = .fid_q(o_flat, 0.9),
      simulated_p10 = .fid_q(s_flat, 0.1),
      simulated_p90 = .fid_q(s_flat, 0.9),
      reason = paste(reason, collapse = ", "),
      n_cards = o$n_cards, n_obs_rows = o$n_rows, n_sim_rows = s$n_rows,
      n_sim_reps = n_reps
    )
  }
  dplyr::bind_rows(one("level"), one("sd"))
}

.te_fid_rate_rows <- function(lab, o, s) {
  one <- function(part, src) {
    dplyr::bind_rows(lapply(names(part$rates), function(r) {
      v <- part$rates[[r]]
      tibble::tibble(statistic = lab, source = src, replicate = r,
                     group = sub("^[^\r]*\r", "", names(v)),
                     rate = unname(v))
    }))
  }
  dplyr::bind_rows(one(o, "observed"), one(s, "simulated"))
}

# Column resolution. The model knows what it was fitted on, so it is asked
# first; explicit arguments beat it, and the usual candidate lists catch a
# bare frame that came from somewhere else.
.te_fid_columns <- function(sim, obs, model, truth, estimate, group, card,
                            call) {
  terms <- model$terms
  truth <- truth %||% terms$truth %||% "triage"
  estimate <- estimate %||% terms$estimate %||% "called_triage"
  for (nm in c(truth, estimate)) {
    absent <- c(if (!nm %in% names(sim)) "sim",
                if (!nm %in% names(obs)) "observed")
    if (length(absent)) {
      n_absent <- length(absent)
      cli::cli_abort(
        c("Column {.field {nm}} is missing from {n_absent} frame{?s}.",
          "x" = "Not found in: {.arg {absent}}.",
          "i" = "Name the columns with {.arg truth} and {.arg estimate}."),
        class = "sandtablR_error_triage", call = call
      )
    }
  }
  list(
    truth = truth, estimate = estimate,
    sim_group = .fid_pick(sim, group, .fid_group_names(), "team", "sim",
                          call),
    obs_group = .fid_pick(obs, group %||% terms$group, .fid_group_names(),
                          "team", "observed", call),
    sim_card = .fid_pick(sim, card %||% terms$card, .fid_card_names(),
                         "card", "sim", call),
    obs_card = .fid_pick(obs, card %||% terms$card, .fid_card_names(),
                         "card", "observed", call)
  )
}

#' @exportS3Method tibble::tbl_sum
tbl_sum.sdt_error_fidelity <- function(x, ...) {
  frame <- attr(x, "frame")
  band <- attr(x, "band")
  n_fail <- sum(x$verdict == "FAIL", na.rm = TRUE)
  head <- c(
    "Triage error fidelity" = sprintf(
      "%d of %d ratio%s outside %.2f-%.2f", n_fail, nrow(x),
      .te_s(nrow(x)), band[[1L]], band[[2L]]
    )
  )
  if (!is.null(frame)) {
    per <- paste(sprintf("%s %d", names(frame$cards_per_category),
                         frame$cards_per_category), collapse = " / ")
    head <- c(head, "Matched frame" = sprintf(
      "%d cards (%s), %d observed vs %d simulated team%s x %d replicate%s",
      frame$n_cards, per, frame$n_obs_groups, frame$n_sim_groups_per_rep,
      .te_s(frame$n_sim_groups_per_rep), frame$n_sim_reps,
      .te_s(frame$n_sim_reps)
    ))
  }
  struct <- attr(x, "structural")
  if (length(struct)) {
    head <- c(head, "Excluded" = sprintf(
      "%s (zero by construction)", paste(struct, collapse = ", ")
    ))
  }
  head
}
