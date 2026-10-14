# Group behaviour model -------------------------------------------------------
# Fitting real teams, then simulating new ones.
#
# The discrete-event engine in `sdt_simulate()` answers "where do casualties go
# if everyone plays the reference policy?". It cannot answer "what would a
# further twenty teams have done?", because it has no team-level term at all:
# every replication draws from the same routing probabilities, so its spread is
# the Monte Carlo noise of one fixed policy, and it shrinks as the cohort grows
# rather than settling on the spread of real teams. Measured like for like on
# the validation corpus -- the same 31-card composition, 20 engine
# replications against 110 real groups -- the engine puts 8.4% of casualties
# through the operating room with an SD of 0.038, where the real groups put
# 24.1% through with an SD of 0.072 and a range of 0.10 to 0.45. It is the
# wrong level and half the spread, and no seed fixes either.
#
# What follows is the other half: a per-outcome hierarchical logistic model
#
#     y ~ card + (1 | block) + (1 | group)
#
# fitted to observed (group, card) decisions. The card fixed effect absorbs
# "this casualty is obviously operative"; the group random effect is the team's
# own tendency, which is the quantity a simulator needs and the engine lacks.
# Drawing a fresh group effect therefore produces a *new team*, not another
# replication of the same one.
#
# Two design facts are load-bearing, and both were measured rather than assumed:
#
#   * The frame must be composition matched. Play is truncated by the clock, so
#     every group saw a different subset of the deck, and between-group
#     dispersion computed on that unbalanced frame carries which cards a group
#     happened to reach as well as what it did with them. A simulator deals
#     every card to every simulated group, so it cannot produce that first
#     part, and the comparison is off for a reason that has nothing to do with
#     the model: measured on the full deck the ratio is 1.10 for TASC and 1.33
#     for eFAST, against 1.00 and 1.02 on the matched frame built from the same
#     rows. `frame = "matched"` restricts to cards seen by enough groups and to
#     the groups that saw all of them, and is the default for that reason.
#   * The team random effect is what makes the simulation faithful. With
#     `(1 | group)` the simulated between-group SD matches the observed one;
#     without it the simulator is too consistent, and the ratio moves away from
#     one by up to 60%.
#
# Each outcome is fitted separately. That is a deliberate simplification and a
# real limitation: a simulated team's tendencies are independent across
# outcomes, so the correlation between, say, doing an eFAST and going on to CT
# is not reproduced. It is recorded in the docs rather than hidden.
#
# Course composition enters this model as a BLOCK-level covariate and can
# enter it no other way. The participant register links people to courses,
# not to the groups inside them -- it carries no group, team or "Gruppe"
# column at all -- so every group in one course wears the same composition
# row. In
#
#     y ~ card + composition + (1 | block) + (1 | group)
#
# a composition term therefore competes with the SESSION random effect and
# can never explain the TEAM random effect, which is the one the simulator
# draws from. The random block intercept stays in the formula regardless: the
# covariates are there to explain part of it, not to stand in for it, and
# with fifteen informative courses a handful of between-course slopes cannot
# be trusted to do the second job. Whether they manage even the first is
# reported rather than assumed -- `$meta$block_variance` carries the block SD
# before and after, per outcome, and it is allowed to come out at zero.


# Outcome catalogue -----------------------------------------------------------
# Each entry knows the columns it can be built from and how to build it. `needs`
# is a list of alternatives; the outcome is available when any one of them is
# present in the data, which lets `therapy` and the `did_*` flags both work.

.beh_spec <- function(name, needs, fn, label) {
  list(name = name, needs = needs, fn = fn, label = label)
}

# Case- and whitespace-insensitive membership, so a frame that spells its
# levels in German ("Rot", "Sichtungsplatz") behaves like one that does not.
.beh_in <- function(x, values) {
  x <- tolower(trimws(as.character(x)))
  out <- x %in% tolower(values)
  out[is.na(x)] <- NA
  out
}

.beh_flag <- function(x) {
  if (is.logical(x)) {
    return(x)
  }
  if (is.numeric(x)) {
    vals <- unique(stats::na.omit(x))
    if (all(vals %in% c(0, 1))) {
      return(x == 1)
    }
  }
  NULL
}

# Locations that count as intensive care, and as never having left the
# screening area. Kept as vocabularies so a user's own labels can be matched
# without editing the catalogue.
.beh_icu_levels <- function() {
  c("ICU", "IMC", "icu_imc", "intensive care", "intensivstation")
}

.beh_screening_levels <- function() {
  c("Sichtungsplatz", "Sichtung", "screening", "triage")
}

.beh_catalogue <- function() {
  therapy_is <- function(level, flag) {
    function(d) {
      if (!is.null(d[["therapy"]])) {
        return(.beh_in(d[["therapy"]], level))
      }
      .beh_flag(d[[flag]])
    }
  }
  list(
    .beh_spec("tasc", list("therapy", "did_tasc"),
              therapy_is("TASC", "did_tasc"),
              "abbreviated surgical care"),
    .beh_spec("dcs", list("therapy", "did_dcs"),
              therapy_is("DCS", "did_dcs"), "damage control surgery"),
    .beh_spec("etc", list("therapy", "did_etc"),
              therapy_is("ETC", "did_etc"), "early total care"),
    .beh_spec("conservative", list("did_conservative"),
              function(d) .beh_flag(d[["did_conservative"]]),
              "conservative therapy"),
    .beh_spec("efast", list("did_efast"),
              function(d) .beh_flag(d[["did_efast"]]), "eFAST performed"),
    .beh_spec("xray", list("did_xray"),
              function(d) .beh_flag(d[["did_xray"]]), "X-ray performed"),
    .beh_spec("ct", list("did_ct"),
              function(d) .beh_flag(d[["did_ct"]]), "CT performed"),
    .beh_spec("red", list("called_triage"),
              function(d) .beh_in(d[["called_triage"]], c("red", "rot")),
              "triaged red"),
    .beh_spec("yellow", list("called_triage"),
              function(d) .beh_in(d[["called_triage"]], c("yellow", "gelb")),
              "triaged yellow"),
    .beh_spec("green", list("called_triage"),
              function(d) .beh_in(d[["called_triage"]], c("green", "gruen",
                                                          "grün")),
              "triaged green"),
    .beh_spec("icu", list("general_location"),
              function(d) .beh_in(d[["general_location"]], .beh_icu_levels()),
              "ended in intensive care"),
    .beh_spec("stranded", list("general_location"),
              function(d) {
                .beh_in(d[["general_location"]], .beh_screening_levels())
              },
              "ended at the screening area"),
    .beh_spec("ventilated", list("ventilated"),
              function(d) .beh_flag(d[["ventilated"]]), "ventilated"),
    .beh_spec("died", list("died"),
              function(d) .beh_flag(d[["died"]]), "died")
  )
}

.beh_available <- function(spec, nms) {
  any(vapply(spec$needs, function(cols) all(cols %in% nms), logical(1)))
}

# Build the outcome columns. Returns the derived logicals plus a table of the
# outcomes that could not be built at all, so nothing is dropped in silence.
.beh_build_outcomes <- function(data, outcomes, call) {
  cat_specs <- .beh_catalogue()
  names(cat_specs) <- vapply(cat_specs, `[[`, character(1), "name")
  nms <- names(data)

  if (is.null(outcomes)) {
    wanted <- names(cat_specs)
  } else {
    if (!is.character(outcomes) || anyNA(outcomes) || !length(outcomes)) {
      cli::cli_abort(
        "{.arg outcomes} must be a character vector of outcome names.",
        class = "sandtablR_error_behaviour", call = call
      )
    }
    wanted <- unique(outcomes)
    unknown <- setdiff(wanted, c(names(cat_specs), nms))
    if (length(unknown)) {
      known <- names(cat_specs)
      cli::cli_abort(
        c("{.arg outcomes} must name catalogue outcomes or columns of
           {.arg data}.",
          "x" = "Unknown: {.val {unknown}}.",
          "i" = "Catalogue: {.val {known}}."),
        class = "sandtablR_error_behaviour", call = call
      )
    }
  }

  built <- list()
  labels <- character()
  skipped <- list()
  for (nm in wanted) {
    spec <- cat_specs[[nm]]
    if (is.null(spec)) {
      y <- .beh_flag(data[[nm]])
      if (is.null(y)) {
        skipped[[nm]] <- "column is not logical or 0/1"
        next
      }
      built[[nm]] <- y
      labels[[nm]] <- nm
      next
    }
    if (!.beh_available(spec, nms)) {
      need <- vapply(spec$needs, paste, character(1), collapse = " + ")
      skipped[[nm]] <- paste0("needs ", paste(need, collapse = " or "))
      next
    }
    y <- spec$fn(data)
    if (is.null(y)) {
      skipped[[nm]] <- "source column is not logical or 0/1"
      next
    }
    built[[nm]] <- as.logical(y)
    labels[[nm]] <- spec$label
  }

  if (!length(built)) {
    cli::cli_abort(
      c("No modellable outcome could be derived from {.arg data}.",
        "i" = "Supply {.arg outcomes} naming logical columns, or add the
               source columns the catalogue reads."),
      class = "sandtablR_error_behaviour", call = call
    )
  }
  list(values = built, labels = labels,
       skipped = .beh_skip_table(skipped, "unavailable"))
}

.beh_skip_table <- function(x, stage) {
  if (!length(x)) {
    return(tibble::tibble(
      outcome = character(), stage = character(), reason = character()
    ))
  }
  tibble::tibble(
    outcome = names(x), stage = rep(stage, length(x)),
    reason = unname(unlist(x, use.names = FALSE))
  )
}


# The composition-matched frame -----------------------------------------------

# Coverage of each card, in groups that saw it.
.beh_coverage <- function(gid, card) {
  tab <- unique(data.frame(g = gid, c = card, stringsAsFactors = FALSE))
  sort(table(tab$c), decreasing = TRUE)
}

# Choose `min_coverage` by maximising the size of the balanced block, i.e. the
# number of cards kept times the number of groups that saw all of them. That is
# the total number of complete decision episodes available to the model, so it
# maximises the information both the card effects and the team effects are
# estimated from. Ties go to the frame with more groups, because groups are the
# sample size of the between-group statistic this model exists to reproduce.
.beh_choose_coverage <- function(gid, card) {
  cov <- .beh_coverage(gid, card)
  seen <- unique(data.frame(g = gid, c = card, stringsAsFactors = FALSE))
  cand <- sort(unique(as.integer(cov)))
  grid <- lapply(cand, function(t) {
    cards <- names(cov)[as.integer(cov) >= t]
    k <- table(seen$g[seen$c %in% cards])
    n_g <- sum(as.integer(k) == length(cards))
    data.frame(min_coverage = t, n_cards = length(cards), n_groups = n_g,
               n_rows = length(cards) * n_g)
  })
  grid <- do.call(rbind, grid)
  ord <- order(-grid$n_rows, -grid$n_groups, -grid$min_coverage)
  list(choice = grid[ord[[1]], , drop = FALSE], grid = grid[ord, ])
}

.beh_matched <- function(gid, card, min_coverage, call) {
  cov <- .beh_coverage(gid, card)
  chosen <- .beh_choose_coverage(gid, card)
  if (is.null(min_coverage)) {
    min_coverage <- chosen$choice$min_coverage
  } else {
    min_coverage <- .check_count(min_coverage)
    if (min_coverage > max(as.integer(cov))) {
      top <- max(as.integer(cov))
      cli::cli_abort(
        c("{.arg min_coverage} is larger than any card's coverage.",
          "x" = "Best covered card was seen by {top} group{?s}."),
        class = "sandtablR_error_behaviour", call = call
      )
    }
  }
  cards <- names(cov)[as.integer(cov) >= min_coverage]
  seen <- unique(data.frame(g = gid, c = card, stringsAsFactors = FALSE))
  k <- table(seen$g[seen$c %in% cards])
  groups <- names(k)[as.integer(k) == length(cards)]
  list(min_coverage = min_coverage, cards = cards, groups = groups,
       grid = chosen$grid)
}


# Block-level covariates ------------------------------------------------------
#
# One row per block, joined on the block key, centred and scaled over the
# distinct blocks so that each course counts once however many groups it
# happened to run. The centring is kept because the simulator has to invert
# it: a caller describes a course on the natural scale ("59% surgeons") and
# the fitted slope lives in standard-deviation units.
#
# Every check below collects rather than aborts on the first offender, so a
# caller repairing a covariate table sees all of it at once, in the spirit of
# `.field_problem()` in R/utils-check.R.

.beh_cov_term <- function(name) paste0(".z_", make.names(name))

# A standard deviation below this is a constant column, and a correlation
# above `.beh_alias_r()` is one covariate wearing two names. `.beh_close_r()`
# is not an error, only a warning: two covariates that correlate at 0.95 are
# still separately estimable in principle and hopeless in practice with as
# few blocks as a course corpus has.
.beh_cov_tol <- function() 1e-8
.beh_alias_r <- function() 0.999
.beh_close_r <- function() 0.9
# A block SD at or below this has nothing left in it to explain.
.beh_flat_block <- function() 1e-4

.beh_cov_bullet <- function(field, detail, values = NULL) {
  if (!length(values)) {
    return(cli::format_inline("{.field {field}}: {detail}."))
  }
  shown <- utils::head(as.character(values), 5L)
  hidden <- length(values) - length(shown)
  more <- if (hidden > 0L) cli::format_inline(" (+{hidden} more)") else ""
  cli::format_inline("{.field {field}}: {detail} {.val {shown}}{more}.")
}

.beh_cov_abort <- function(bullets, headline, hint, call) {
  bullets <- unlist(Filter(length, bullets), use.names = FALSE)
  if (!length(bullets)) {
    return(invisible(NULL))
  }
  msg <- c(headline, stats::setNames(bullets, rep("x", length(bullets))))
  if (!is.null(hint)) msg <- c(msg, "i" = hint)
  cli::cli_abort(msg, class = "sandtablR_error_behaviour", call = call)
}

.beh_num <- function(x) {
  paste(formatC(x, digits = 3, format = "g"), collapse = " to ")
}

# Two covariates a between-block regression cannot tell apart. This is not a
# remote possibility on a course corpus: a missingness indicator is often an
# exact copy of "this course had no participants at all", and in the real
# register the title non-response rate correlates with the doctorate share at
# -0.96 for the mechanical reason that both are computed from the same box.
.beh_cov_collinear <- function(z, names, call) {
  p <- ncol(z)
  if (p < 2L) {
    return(invisible(NULL))
  }
  r <- suppressWarnings(stats::cor(z))
  aliased <- list()
  close <- character()
  for (i in seq_len(p - 1L)) {
    for (j in seq(i + 1L, p)) {
      rho <- r[[i, j]]
      if (!is.finite(rho)) next
      pair <- paste(names[[i]], "and", names[[j]])
      if (abs(rho) >= .beh_alias_r()) {
        aliased[[pair]] <- cli::format_inline(
          "{.field {pair}}: correlate at {round(rho, 3)}."
        )
      } else if (abs(rho) >= .beh_close_r()) {
        close <- c(close, sprintf("%s (r = %.2f)", pair, rho))
      }
    }
  }
  .beh_cov_abort(
    list(aliased),
    "{.arg block_covariates} holds covariates that cannot be told apart.",
    "Each slope is estimated from as many points as there are blocks; keep
     one of each pair.",
    call
  )
  rank <- qr(cbind(1, z))$rank
  if (rank < p + 1L) {
    cli::cli_abort(
      c("{.arg block_covariates} is rank deficient.",
        "x" = "{p} covariate{?s} plus an intercept span only
               {rank} dimension{?s}.",
        "i" = "One of them is an exact linear combination of the others."),
      class = "sandtablR_error_behaviour", call = call
    )
  }
  if (length(close)) {
    n_close <- length(close)
    cli::cli_warn(
      c("!" = "{n_close} covariate pair{?s} nearly collinear: {close}.",
        "i" = "Their separate slopes are barely identified; read them
               together rather than apart."),
      class = "sandtablR_warning_behaviour"
    )
  }
  invisible(NULL)
}

# Validate `block_data` against the blocks actually being fitted, then centre
# and scale. Returns NULL when no covariates were asked for, and that NULL is
# the switch every downstream branch reads.
.beh_cov_prepare <- function(block_data, block_covariates, block, blocks,
                             use_block, call) {
  if (is.null(block_data) && is.null(block_covariates)) {
    return(NULL)
  }
  if (is.null(block_data) || is.null(block_covariates)) {
    cli::cli_abort(
      c("{.arg block_data} and {.arg block_covariates} go together.",
        "i" = "Pass both, or neither."),
      class = "sandtablR_error_behaviour", call = call
    )
  }
  if (!is.character(block_covariates) || !length(block_covariates) ||
        anyNA(block_covariates)) {
    cli::cli_abort(
      "{.arg block_covariates} must be a character vector of column names.",
      class = "sandtablR_error_behaviour", call = call
    )
  }
  if (!is.data.frame(block_data) || !nrow(block_data)) {
    cli::cli_abort(
      "{.arg block_data} must be a data frame with at least one row.",
      class = "sandtablR_error_behaviour", call = call
    )
  }
  block_covariates <- unique(block_covariates)
  if (is.null(block) || !use_block) {
    why <- if (is.null(block)) {
      "{.arg block} is NULL, so the model has no session term."
    } else {
      "{.arg data} holds one block, so its {.code (1 | block)} term went."
    }
    cli::cli_abort(
      c("Block covariates need more than one block.",
        "x" = why,
        "i" = "A course-level covariate is constant inside a block and can
               only be estimated across them."),
      class = "sandtablR_error_behaviour", call = call
    )
  }
  if (!block %in% names(block_data)) {
    have <- utils::head(names(block_data), 8L)
    cli::cli_abort(
      c("{.arg block_data} must carry the block key column {.val {block}}.",
        "x" = "Columns present: {.val {have}}.",
        "i" = "{.fn sdt_course_composition} keys on {.field file}, so fit
               with {.code block = \"file\"} to join on it."),
      class = "sandtablR_error_behaviour", call = call
    )
  }
  unknown <- setdiff(block_covariates, names(block_data))
  if (length(unknown)) {
    n_unknown <- length(unknown)
    cli::cli_abort(
      c("{.arg block_covariates} must name columns of {.arg block_data}.",
        "x" = "{n_unknown} not found: {.val {unknown}}."),
      class = "sandtablR_error_behaviour", call = call
    )
  }
  if (block %in% block_covariates) {
    cli::cli_abort(
      c("{.arg block_covariates} must not include the block key itself.",
        "x" = "{.val {block}} identifies the block; it cannot explain it."),
      class = "sandtablR_error_behaviour", call = call
    )
  }

  key <- as.character(block_data[[block]])
  dup <- unique(key[duplicated(key)])
  if (length(dup)) {
    n_dup <- length(dup)
    shown <- utils::head(dup, 5L)
    cli::cli_abort(
      c("{.arg block_data} must hold one row per block.",
        "x" = "{n_dup} key{?s} appear more than once: {.val {shown}}."),
      class = "sandtablR_error_behaviour", call = call
    )
  }
  gone <- setdiff(blocks, key)
  if (length(gone)) {
    n_gone <- length(gone)
    shown <- utils::head(gone, 5L)
    cli::cli_abort(
      c("{.arg block_data} has no row for every block being fitted.",
        "x" = "{n_gone} block{?s} missing: {.val {shown}}.",
        "i" = "Blocks are taken from the fitting frame, so a matched frame
               can leave fewer of them than {.arg data} started with."),
      class = "sandtablR_error_behaviour", call = call
    )
  }

  bd <- block_data[match(blocks, key), , drop = FALSE]
  n_b <- length(blocks)
  n_cov <- length(block_covariates)
  z <- matrix(NA_real_, nrow = n_b, ncol = n_cov)
  blank <- stats::setNames(rep(NA_real_, n_cov), block_covariates)
  centre <- scale <- lo <- hi <- blank
  kind <- stats::setNames(rep(NA_character_, n_cov), block_covariates)
  bad_type <- bad_na <- bad_flat <- list()
  for (j in seq_len(n_cov)) {
    nm <- block_covariates[[j]]
    v <- bd[[nm]]
    if (is.logical(v)) {
      kind[[nm]] <- "logical"
    } else if (is.numeric(v)) {
      kind[[nm]] <- "numeric"
    } else {
      cls <- class(v)[[1]]
      bad_type[[nm]] <- .beh_cov_bullet(
        nm, paste0("is ", cls, ", not numeric or logical")
      )
      next
    }
    v <- as.numeric(v)
    if (anyNA(v)) {
      bad_na[[nm]] <- .beh_cov_bullet(nm, "has no value for block",
                                      blocks[is.na(v)])
      next
    }
    s <- stats::sd(v)
    if (!is.finite(s) || s < .beh_cov_tol()) {
      bad_flat[[nm]] <- .beh_cov_bullet(
        nm, sprintf("is the same in all %d blocks", n_b)
      )
      next
    }
    centre[[nm]] <- mean(v)
    scale[[nm]] <- s
    lo[[nm]] <- min(v)
    hi[[nm]] <- max(v)
    z[, j] <- (v - centre[[nm]]) / s
  }
  .beh_cov_abort(
    list(bad_type, bad_na, bad_flat),
    "{.arg block_covariates} cannot all be used.",
    "A covariate must be numeric or logical, complete over the blocks being
     fitted, and not constant across them.",
    call
  )

  colnames(z) <- .beh_cov_term(block_covariates)
  rownames(z) <- blocks
  .beh_cov_collinear(z, block_covariates, call)
  list(
    key = block,
    blocks = blocks,
    z = z,
    table = tibble::tibble(
      name = block_covariates,
      term = unname(colnames(z)),
      kind = unname(kind[block_covariates]),
      centre = unname(centre[block_covariates]),
      scale = unname(scale[block_covariates]),
      min = unname(lo[block_covariates]),
      max = unname(hi[block_covariates])
    )
  )
}

# Expand the per-block covariate matrix to one row per decision episode.
.beh_cov_rows <- function(cov, bid) {
  out <- as.data.frame(cov$z[match(bid, cov$blocks), , drop = FALSE])
  rownames(out) <- NULL
  out
}

.beh_formula <- function(terms, use_block) {
  re <- if (use_block) {
    c("(1 | .block)", "(1 | .group)")
  } else {
    "(1 | .group)"
  }
  stats::as.formula(
    paste(".y ~", paste(c(".card", terms, re), collapse = " + "))
  )
}

.beh_design_formula <- function(terms) {
  stats::as.formula(paste("~", paste(c(".card", terms), collapse = " + ")))
}

# One log-likelihood pair for either engine. Both have a `logLik` method, and
# deriving the deviance and AIC from it keeps the two comparable where
# `deviance()` alone would not be.
.beh_ic <- function(fit) {
  ll <- tryCatch(stats::logLik(fit), error = function(e) NULL)
  if (is.null(ll)) {
    return(list(loglik = NA_real_, deviance = NA_real_, df = NA_real_,
                aic = NA_real_))
  }
  df <- as.numeric(attr(ll, "df"))
  ll <- as.numeric(ll)
  list(loglik = ll, deviance = -2 * ll, df = df, aic = -2 * ll + 2 * df)
}

# The same model without the covariates, on exactly the same rows. It is the
# only honest "before": a fit on a different row set would confound the
# covariates with whichever rows they happened to drop.
.beh_fit_null <- function(form_null, d, engine, use_block, nAGQ = 1L) {
  if (is.null(form_null)) {
    return(NULL)
  }
  environment(form_null) <- list2env(as.list(d),
                                     parent = environment(form_null))
  fit <- tryCatch(suppressWarnings(.beh_glmm(form_null, d, engine, nAGQ)),
                  error = function(e) NULL)
  if (is.null(fit)) {
    return(list(sd_group = NA_real_, sd_block = NA_real_,
                ic = .beh_ic(NULL)))
  }
  sds <- .beh_extract_sds(fit, engine, use_block)
  list(sd_group = sds$group, sd_block = sds$block, ic = .beh_ic(fit))
}

# The headline row: how much of the session spread the covariates took away.
# `var_explained` is deliberately left signed and unclamped -- a covariate
# that makes the block SD go up has told you something, and rounding it to
# zero would hide it.
.beh_block_row <- function(sds, null, ic, n_obs) {
  if (is.null(null)) {
    return(NULL)
  }
  before <- null$sd_block
  after <- sds$block
  explained <- if (!is.finite(before) || before <= .beh_flat_block()) {
    NA_real_
  } else {
    1 - (after^2) / (before^2)
  }
  chisq <- null$ic$deviance - ic$deviance
  df <- ic$df - null$ic$df
  p <- if (is.finite(chisq) && is.finite(df) && df > 0 && chisq >= 0) {
    stats::pchisq(chisq, df, lower.tail = FALSE)
  } else {
    NA_real_
  }
  tibble::tibble(
    n_obs = n_obs,
    sd_block_before = before, sd_block_after = after,
    var_explained = explained,
    sd_group_before = null$sd_group, sd_group_after = sds$group,
    deviance_before = null$ic$deviance, deviance_after = ic$deviance,
    aic_before = null$ic$aic, aic_after = ic$aic,
    delta_aic = ic$aic - null$ic$aic,
    chisq = chisq, df = df, p_value = p
  )
}


# Fitting ---------------------------------------------------------------------

#' Fit a hierarchical model of how teams behave
#'
#' Fits one binomial mixed model per outcome to observed (group, card) decision
#' episodes:
#'
#' ```
#' y ~ card + (1 | block) + (1 | group)
#' ```
#'
#' The card fixed effect carries what the casualty is; the group random effect
#' carries what the team is. That second term is the point. It is what lets
#' [sdt_simulate_groups()] draw a *new* team rather than another replication of
#' the ones already in the data, and it is what the discrete-event engine in
#' [sdt_simulate()] has no equivalent of.
#'
#' @section The composition-matched frame:
#' Real play is truncated by the clock, so groups see different subsets of the
#' deck. Any between-group statistic computed on that unbalanced frame mixes
#' team behaviour with *which cards the team reached*, and the second term is
#' much larger than the first. `frame = "matched"` therefore keeps only cards
#' seen by at least `min_coverage` groups and only the groups that saw all of
#' them, so every group in the fit answered the same questions. `frame = "all"`
#' fits on everything and warns: a simulator deals every card to every group,
#' so it cannot reproduce composition spread, and the dispersion ratio is then
#' wrong for a reason that has nothing to do with the model. Measured on the
#' validation corpus, the full deck reports 1.10 for TASC and 1.33 for eFAST
#' where the matched frame built from the same rows reports 1.00 and 1.02.
#'
#' With `min_coverage = NULL` the threshold is chosen to maximise cards times
#' groups -- the number of complete decision episodes -- with ties going to the
#' frame that keeps more groups.
#'
#' @section Course composition as a block covariate:
#' `block_data` is a table with one row per block, keyed by the `block`
#' column -- the shape [sdt_course_composition()] returns, which keys on
#' `file`, so fit with `block = "file"` to join on it. `block_covariates`
#' names the columns that enter the fixed part, giving
#'
#' ```
#' y ~ card + <covariates> + (1 | block) + (1 | group)
#' ```
#'
#' The random block intercept **stays**. The covariates are there to explain
#' part of the session spread, not to replace the term, and with fifteen
#' informative courses a handful of between-course slopes cannot be trusted
#' to replace it.
#'
#' They also cannot reach further than that. Participants are linked to
#' courses, never to the groups inside them, so composition is constant
#' within a block: it competes with the **session** random effect and can
#' never explain the **team** random effect, which is the one
#' [sdt_simulate_groups()] draws a new team from. A course-level covariate
#' cannot make a simulated team more or less like any other team in its own
#' course.
#'
#' Numeric and logical covariates are accepted; each is centred and scaled
#' over the distinct blocks, so a course counts once however many groups it
#' ran, and the centring is recorded in `$meta$composition$covariates` so the
#' simulator can invert it. A covariate that is constant across blocks, that
#' is missing for a block being fitted, or that is collinear with another is
#' refused, and every offender is named in one message.
#'
#' Each outcome is then fitted **twice**, with the covariates and without
#' them on exactly the same rows, and `$meta$block_variance` reports the
#' block SD before and after, the share of block variance removed, and the
#' deviance and AIC change. That doubles the fitting time and it is the point
#' of the exercise: the answer is allowed to be "nothing", and on this corpus
#' it usually is.
#'
#' @section Outcomes:
#' `outcomes = NULL` auto-detects everything the catalogue can build from the
#' columns present: `tasc`, `dcs`, `etc` and `conservative` from `therapy` or
#' the `did_*` flags; `efast`, `xray` and `ct` from `did_efast`, `did_xray` and
#' `did_ct`; `red`, `yellow` and `green` from `called_triage`; `icu` and
#' `stranded` from `general_location`; `ventilated` and `died` from the columns
#' of those names. You may instead pass a character vector naming catalogue
#' outcomes, logical columns of `data`, or both.
#'
#' A consensus or modal triage label is deliberately *not* a source. It is
#' derived from what the teams did, so modelling it would model the model.
#'
#' @section What gets skipped:
#' An outcome that cannot be built, is constant, has too few events or groups,
#' or whose fit fails or does not converge is recorded in `$skipped` with its
#' reason, never dropped in silence. A fit that converges to a zero team
#' variance is *kept* and flagged `singular`: that is a finding about the
#' outcome -- teams did not differ -- and hiding it would hide the negative
#' result the fidelity report exists to surface. A fit whose team SD lands
#' above 5 on the logit scale is flagged `boundary` for the mirror-image
#' reason: that is separation, and simulated teams will be over-dispersed.
#'
#' @param data A data frame of decision episodes, one row per (group, card).
#' @param outcomes Character vector of outcome names, or `NULL` to auto-detect.
#' @param group,block,card Column names holding the team identifier, the
#'   session (block) the team played in, and the patient card. `block` may be
#'   `NULL` to fit without a session term. Group labels that repeat across
#'   blocks are nested inside their block, with a warning.
#' @param engine `"lme4"` (default) or `"glmmTMB"`.
#' @param nAGQ Quadrature points for the \pkg{lme4} engine. `1` (default) is
#'   the Laplace approximation. A larger value uses adaptive Gauss-Hermite
#'   quadrature, which removes most of the upward bias the Laplace
#'   approximation carries in the variance component when clusters hold very
#'   few events -- the failure mode behind an over-dispersed simulated cohort
#'   for a rare behaviour. \pkg{lme4} supports it only for a model with one
#'   grouping factor, so it requires `block = NULL`; asking for it on a
#'   cross-classified model is an error rather than a silent downgrade.
#' @param frame `"matched"` (default) for the composition-matched frame, or
#'   `"all"` for every row.
#' @param min_coverage Minimum number of groups a card must be seen by to enter
#'   the matched frame. `NULL` chooses it from the data.
#' @param block_data Optional data frame with one row per block, keyed by the
#'   `block` column, holding block-level covariates -- typically the output of
#'   [sdt_course_composition()], which keys on `file`.
#' @param block_covariates Optional character vector naming the columns of
#'   `block_data` that enter the fixed part of the formula. Passed together
#'   with `block_data` or not at all.
#' @param verbose If `TRUE`, report progress via \pkg{cli}.
#'
#' @return An S3 object of class `sdt_behaviour_model`:
#'   \describe{
#'     \item{`fits`}{Named list of fitted models, one per modelled outcome.}
#'     \item{`outcomes`}{Names of the outcomes that were modelled.}
#'     \item{`frame`}{`"matched"` or `"all"`, as requested.}
#'     \item{`terms`}{The formula template and the column names it was built
#'       from, including `block_covariates`.}
#'     \item{`data`}{The frame actually fitted on, with the derived outcome
#'       columns attached.}
#'     \item{`meta`}{Counts, the frame definition, engine, package versions,
#'       runtime, and `sds`: per outcome, the rows used, the rows dropped as
#'       missing, the prevalence, and the group and block standard deviations
#'       on the logit scale. With block covariates it also carries
#'       `composition` (the centring and scaling used) and `block_variance`
#'       (the block SD before and after, per outcome).}
#'     \item{`skipped`}{One row per outcome not modelled, with the reason.}
#'   }
#' @seealso [sdt_simulate_groups()] to draw new teams from the fit.
#' @export
#' @examples
#' set.seed(1)
#' d <- expand.grid(
#'   card = sprintf("c%02d", seq_len(8)),
#'   group_id = sprintf("g%02d", seq_len(12)),
#'   stringsAsFactors = FALSE
#' )
#' d$session_id <- ifelse(d$group_id < "g07", "s1", "s2")
#' b <- stats::rnorm(12, sd = 1.2)
#' p <- stats::plogis(-0.5 + b[match(d$group_id, unique(d$group_id))])
#' d$did_efast <- stats::runif(nrow(d)) < p
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   fit <- sdt_fit_behaviour(d, outcomes = "efast", verbose = FALSE)
#'   fit
#' }
sdt_fit_behaviour <- function(data,
                              outcomes = NULL,
                              group = "group_id",
                              block = "session_id",
                              card = "card",
                              engine = "lme4",
                              nAGQ = 1L,
                              frame = c("matched", "all"),
                              min_coverage = NULL,
                              block_data = NULL,
                              block_covariates = NULL,
                              verbose = TRUE) {
  call <- rlang::current_env()
  frame <- rlang::arg_match(frame)
  engine <- rlang::arg_match(engine, c("lme4", "glmmTMB"))
  .check_flag(verbose)
  .check_string(group)
  .check_string(card)
  if (!is.null(block)) .check_string(block)
  if (!is.data.frame(data) || !nrow(data)) {
    cli::cli_abort("{.arg data} must be a data frame with at least one row.",
                   class = "sandtablR_error_behaviour", call = call)
  }
  .require_pkgs(engine)

  keys <- c(group = group, block = block, card = card)
  missing_cols <- keys[!keys %in% names(data)]
  if (length(missing_cols)) {
    args <- names(missing_cols)
    cli::cli_abort(
      c("{.arg data} is missing {length(missing_cols)} key column{?s}.",
        "x" = "Not found: {.val {unname(missing_cols)}}.",
        "i" = "Set {.arg {args}} to the column name{?s} your frame uses."),
      class = "sandtablR_error_behaviour", call = call
    )
  }

  t0 <- Sys.time()
  gid <- as.character(data[[group]])
  cid <- as.character(data[[card]])
  bid <- if (is.null(block)) rep("all", nrow(data)) else {
    as.character(data[[block]])
  }
  if (anyNA(gid) || anyNA(cid) || anyNA(bid)) {
    cli::cli_abort(
      c("Key columns must not be missing.",
        "x" = "{sum(is.na(gid) | is.na(cid) | is.na(bid))} row{?s} have a
               missing group, block or card."),
      class = "sandtablR_error_behaviour", call = call
    )
  }

  gid <- .beh_nest_groups(gid, bid, block)
  dup <- sum(duplicated(paste(gid, cid, sep = "\r")))
  if (dup) {
    cli::cli_warn(c(
      "!" = "{dup} duplicated (group, card) row{?s} in {.arg data}.",
      "i" = "They are kept as repeated trials; card coverage counts groups."
    ), class = "sandtablR_warning_behaviour")
  }

  built <- .beh_build_outcomes(data, outcomes, call)
  n_blocks_all <- length(unique(bid))
  use_block <- !is.null(block) && n_blocks_all > 1L
  if (!is.null(block) && !use_block) {
    cli::cli_warn(c(
      "!" = "Only {n_blocks_all} block in {.arg data}; dropping the
             {.code (1 | block)} term.",
      "i" = "Session-level and team-level spread cannot be told apart here."
    ), class = "sandtablR_warning_behaviour")
  }

  nAGQ <- .beh_check_nagq(nAGQ, use_block, engine, call)

  fr <- if (frame == "matched") {
    .beh_matched(gid, cid, min_coverage, call)
  } else {
    .beh_warn_all_frame()
    list(min_coverage = NA_integer_, cards = sort(unique(cid)),
         groups = sort(unique(gid)), grid = NULL)
  }
  keep <- gid %in% fr$groups & cid %in% fr$cards
  if (sum(keep) < 2L || length(fr$groups) < 2L || length(fr$cards) < 2L) {
    n_g <- length(fr$groups)
    n_c <- length(fr$cards)
    cli::cli_abort(
      c("The fitting frame is too small to model.",
        "x" = "{n_c} card{?s} and {n_g} group{?s} survived the frame."),
      class = "sandtablR_error_behaviour", call = call
    )
  }

  fitted <- data[keep, , drop = FALSE]
  for (nm in names(built$values)) fitted[[nm]] <- built$values[[nm]][keep]
  fitted[[group]] <- gid[keep]
  fitted <- tibble::as_tibble(fitted)

  md <- data.frame(
    .card = factor(cid[keep], levels = fr$cards),
    .group = factor(gid[keep], levels = sort(unique(gid[keep]))),
    .block = factor(bid[keep], levels = sort(unique(bid[keep]))),
    stringsAsFactors = FALSE
  )
  cov <- .beh_cov_prepare(block_data, block_covariates, block,
                          levels(md$.block), use_block, call)
  cov_terms <- character()
  if (!is.null(cov)) {
    cov_terms <- stats::setNames(cov$table$term, cov$table$name)
    md <- cbind(md, .beh_cov_rows(cov, as.character(md$.block)))
    if (verbose) {
      n_cov <- length(cov_terms)
      n_b <- length(cov$blocks)
      cli::cli_inform(
        "Carrying {n_cov} block covariate{?s} over {n_b} block{?s}; every
         outcome is fitted twice, with them and without."
      )
    }
  }
  form <- .beh_formula(unname(cov_terms), use_block)
  form_null <- if (length(cov_terms)) {
    .beh_formula(character(), use_block)
  } else {
    NULL
  }

  nms <- names(built$values)
  n_out <- length(nms)
  fits <- list()
  sds <- list()
  block_var <- list()
  skipped <- list()
  for (i in seq_along(nms)) {
    nm <- nms[[i]]
    if (verbose) {
      cli::cli_inform("Fitting {.field {nm}} ({i}/{n_out}).")
    }
    one <- .beh_fit_one(built$values[[nm]][keep], md, form, engine, use_block,
                        cov_terms, form_null, nAGQ = nAGQ)
    if (is.null(one$fit)) {
      skipped[[nm]] <- one$reason
      next
    }
    fits[[nm]] <- one$fit
    sds[[nm]] <- tibble::tibble(
      outcome = nm, label = built$labels[[nm]],
      n_obs = one$n_obs, n_missing = one$n_missing,
      n_groups = one$n_groups, prevalence = one$prevalence,
      sd_group = one$sd_group, sd_block = one$sd_block,
      sd_block_total = one$sd_block_total,
      singular = one$singular, boundary = one$sd_group > .beh_wide_sd()
    )
    if (!is.null(one$block_variance)) {
      block_var[[nm]] <- dplyr::bind_cols(
        tibble::tibble(outcome = nm, label = built$labels[[nm]]),
        one$block_variance
      )
    }
  }

  if (!length(fits)) {
    cli::cli_abort(
      c("No outcome could be fitted.",
        "i" = "See the {.field skipped} table for why."),
      class = "sandtablR_error_behaviour", call = call
    )
  }
  sds <- dplyr::bind_rows(sds)
  wide <- sds$outcome[sds$boundary]
  if (length(wide)) {
    n_wide <- length(wide)
    lim <- .beh_wide_sd()
    cli::cli_warn(c(
      "!" = "{n_wide} outcome{?s} fitted a team SD above {lim} on the logit
             scale: {.val {wide}}.",
      "x" = "That is separation rather than a smooth team effect: some teams
             never did it and some always did.",
      "i" = "Simulated teams will be over-dispersed in the tail for
             {n_wide} outcome{?s}; read the fidelity verdict as a warning
             about the outcome, not about the fit."
    ), class = "sandtablR_warning_behaviour")
  }
  sing <- sds$outcome[sds$singular]
  if (length(sing)) {
    n_sing <- length(sing)
    cli::cli_warn(c(
      "!" = "{n_sing} outcome{?s} fitted a zero team variance.",
      "x" = "Simulated teams will be identical for {.val {sing}}.",
      "i" = "That is a finding about the outcome, not a failure of the fit."
    ), class = "sandtablR_warning_behaviour")
  }

  block_variance <- if (length(block_var)) {
    dplyr::bind_rows(block_var)
  } else {
    NULL
  }
  .beh_warn_flat_blocks(block_variance)

  skip_tab <- dplyr::bind_rows(
    built$skipped, .beh_skip_table(skipped, "fit")
  )
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (verbose) {
    n_fit <- length(fits)
    secs <- round(elapsed, 1)
    cli::cli_inform(
      "Fitted {n_fit} outcome{?s} in {secs}s; skipped {nrow(skip_tab)}."
    )
  }

  structure(
    list(
      fits = fits,
      outcomes = names(fits),
      frame = frame,
      terms = list(
        formula = form, card = card, group = group, block = block,
        card_levels = fr$cards, has_block = use_block,
        block_covariates = if (is.null(cov)) character() else cov$table$name,
        cov_terms = unname(cov_terms)
      ),
      data = fitted,
      meta = list(
        n_groups = length(fr$groups), n_blocks = length(unique(bid[keep])),
        n_cards = length(fr$cards), n_rows = sum(keep),
        n_groups_input = length(unique(gid)),
        n_cards_input = length(unique(cid)),
        n_rows_input = nrow(data),
        frame = list(
          type = frame, min_coverage = fr$min_coverage, cards = fr$cards,
          groups = fr$groups, grid = fr$grid,
          block_of = .beh_block_of(gid[keep], bid[keep])
        ),
        engine = engine, nAGQ = nAGQ, versions = .beh_versions(engine),
        elapsed = elapsed, sds = sds,
        composition = if (is.null(cov)) {
          NULL
        } else {
          list(key = cov$key, covariates = cov$table, blocks = cov$blocks,
               z = cov$z)
        },
        block_variance = block_variance
      ),
      skipped = skip_tab
    ),
    class = "sdt_behaviour_model"
  )
}

# Which block each group played in, as a lookup keyed by group id.
.beh_block_of <- function(gid, bid) {
  map <- unique(data.frame(g = gid, b = bid, stringsAsFactors = FALSE))
  map <- map[!duplicated(map$g), , drop = FALSE]
  stats::setNames(map$b, map$g)
}

.beh_versions <- function(engine) {
  pkgs <- unique(c("sandtablR", engine, "lme4"))
  out <- vapply(pkgs, function(p) {
    v <- tryCatch(as.character(utils::packageVersion(p)),
                  error = function(e) NA_character_)
    v
  }, character(1))
  out[!is.na(out)]
}

.beh_warn_all_frame <- function() {
  cli::cli_warn(c(
    "!" = "{.code frame = \"all\"} fits on an unbalanced frame.",
    "x" = "Groups saw different cards, so between-group dispersion measured
           here is composition variance, not team variance.",
    "i" = "A simulator deals every card to every group, so it cannot
           reproduce that composition spread. Measured on the validation
           corpus, the full deck reports a dispersion ratio of 1.10 for TASC
           and 1.33 for eFAST where the matched frame reports 1.00 and 1.02
           from the same rows.",
    "i" = "Use {.code frame = \"matched\"} for anything you intend to report."
  ), class = "sandtablR_warning_behaviour")
}

# Say so out loud when the covariates were asked to explain a session spread
# that was not there to begin with. On this corpus that is the usual answer
# rather than an exception, and it is a fact about the design -- composition
# is a course-level covariate -- not a fault in the covariates.
.beh_warn_flat_blocks <- function(block_variance) {
  if (is.null(block_variance) || !nrow(block_variance)) {
    return(invisible(NULL))
  }
  live <- block_variance$sd_block_before > .beh_flat_block()
  live[is.na(live)] <- FALSE
  if (any(live)) {
    return(invisible(NULL))
  }
  n_out <- nrow(block_variance)
  cli::cli_warn(c(
    "!" = "No fitted outcome has any session spread left to explain.",
    "x" = "The block SD was already at zero for all {n_out} outcome{?s}
           before the covariates were added.",
    "i" = "A block-level covariate can only touch the block term, so this is
           the end of the road for it: it cannot reach the team term."
  ), class = "sandtablR_warning_behaviour")
}

# Nest group labels inside blocks when a label is reused across blocks. Real
# exports number groups A, B, C within each session, so a bare label is not a
# team; taking it at face value would pool unrelated teams into one random
# effect and shrink exactly the variance this model is for.
.beh_nest_groups <- function(gid, bid, block) {
  map <- unique(data.frame(g = gid, b = bid, stringsAsFactors = FALSE))
  tab <- table(map$g)
  dup <- names(tab)[as.integer(tab) > 1L]
  if (!length(dup) || is.null(block)) {
    return(gid)
  }
  n_dup <- length(dup)
  shown <- utils::head(dup, 5L)
  cli::cli_warn(c(
    "!" = "{n_dup} group label{?s} appear in more than one block:
           {.val {shown}}.",
    "i" = "Nesting them as {.code block/group}; pass a unique
           {.arg group} column to control this yourself."
  ), class = "sandtablR_warning_behaviour")
  paste(bid, gid, sep = "/")
}

# Fit one outcome, or say why not. Degeneracy is checked before the optimiser
# is asked to do anything, because a constant or near-constant response fails
# in ways that are hard to read afterwards.
.beh_fit_one <- function(y, md, form, engine, use_block,
                         cov_terms = character(), form_null = NULL,
                         min_groups = 5L, min_events = 5L, nAGQ = 1L) {
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
  if (min(n_true, n_false) < min_events) {
    return(bad(sprintf("too few events (%d of %d rows)",
                       min(n_true, n_false), nrow(d))))
  }
  if (n_groups < min_groups) {
    return(bad(sprintf("too few groups with data (%d)", n_groups)))
  }
  if (nlevels(d$.card) < 2L) {
    return(bad("fewer than two cards with data"))
  }
  # A block covariate that stops varying once this outcome's missing rows are
  # dropped is not estimable, and lme4 answers that by aliasing the column
  # away -- leaving the simulator with a coefficient vector that no longer
  # matches its design. Refused here instead, with the outcome recorded.
  if (length(cov_terms)) {
    flat <- vapply(unname(cov_terms), function(tm) {
      s <- stats::sd(d[[tm]])
      !is.finite(s) || s < .beh_cov_tol()
    }, logical(1))
    if (any(flat)) {
      gone <- names(cov_terms)[flat] %||% unname(cov_terms)[flat]
      return(bad(sprintf("block covariate constant in these rows (%s)",
                         paste(gone, collapse = ", "))))
    }
  }

  # Bind the model frame into the formula's environment. lme4 re-evaluates the
  # formula whenever a caller asks it for something the fit did not store --
  # `simulate(use.u = TRUE)` and `predict(newdata = )` both do -- and without
  # this the fitted object only works inside the call that made it.
  fenv <- list2env(as.list(d), parent = environment(form))
  environment(form) <- fenv

  msgs <- character()
  fit <- withCallingHandlers(
    tryCatch(.beh_glmm(form, d, engine, nAGQ), error = function(e) {
      msgs <<- c(msgs, conditionMessage(e))
      NULL
    }),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  if (is.null(fit)) {
    return(bad(paste("fit failed:", .beh_first_line(msgs))))
  }
  # Rank deficiency is a different failure from non-convergence and needs its
  # own exit: lme4 drops the aliased column and carries on, which the
  # simulator would only discover later, in a place that cannot explain it.
  rankdef <- grepl("rank deficient", msgs, ignore.case = TRUE)
  if (any(rankdef)) {
    return(bad(paste("rank deficient:", .beh_first_line(msgs[rankdef]))))
  }
  conv <- grepl("converge|degenerate|PIRLS|NaN", msgs, ignore.case = TRUE)
  if (any(conv)) {
    return(bad(paste("did not converge:", .beh_first_line(msgs[conv]))))
  }
  sds <- .beh_extract_sds(fit, engine, use_block)
  null <- .beh_fit_null(form_null, d, engine, use_block, nAGQ)
  list(
    fit = fit, reason = NA_character_, n_obs = nrow(d),
    n_missing = sum(!ok), n_groups = n_groups,
    prevalence = n_true / nrow(d), sd_group = sds$group,
    sd_block = sds$block, singular = sds$group < 1e-6,
    # Without covariates the total block spread is the fitted one. With them
    # the fitted value is the residual, and the total is what the null fit
    # measured -- the simulator has to know which is which.
    sd_block_total = if (is.null(null)) sds$block else null$sd_block,
    block_variance = .beh_block_row(sds, null, .beh_ic(fit), nrow(d))
  )
}

.beh_first_line <- function(x) {
  if (!length(x)) {
    return("unknown reason")
  }
  substr(gsub("\\s+", " ", x[[1]]), 1L, 120L)
}

.beh_glmm <- function(form, d, engine, nAGQ = 1L) {
  if (engine == "lme4") {
    ctrl <- lme4::glmerControl(optimizer = "bobyqa", calc.derivs = FALSE)
    return(lme4::glmer(form, data = d, family = stats::binomial(),
                       control = ctrl, nAGQ = nAGQ))
  }
  glmmTMB::glmmTMB(form, data = d, family = stats::binomial())
}

# lme4 integrates the random effects out by a Laplace approximation at
# `nAGQ = 1`, and that approximation is biased UPWARD in the variance component
# when a cluster carries very few events. On this corpus X-ray averages 0.65
# events per team and its fitted team SD is about 11% too large, which makes a
# simulated cohort more dispersed than the real one and reads as a fidelity
# failure. Adaptive Gauss-Hermite quadrature removes most of that bias, but
# lme4 supports it only for a model with ONE grouping factor. This is the gate:
# a caller that asks for quadrature on a cross-classified model is told why it
# cannot have it rather than handed a Laplace fit that silently ignored the
# request.
.beh_check_nagq <- function(nAGQ, use_block, engine, call) {
  nAGQ <- .check_count(nAGQ)
  if (nAGQ <= 1L) {
    return(1L)
  }
  if (!identical(engine, "lme4")) {
    cli::cli_abort(
      c("{.arg nAGQ} above 1 needs the {.val lme4} engine.",
        "i" = "{.pkg glmmTMB} has no quadrature option."),
      class = "sandtablR_error_behaviour", call = call
    )
  }
  if (use_block) {
    cli::cli_abort(
      c("{.arg nAGQ} above 1 needs a single grouping factor.",
        "x" = paste("This model has both {.code (1 | block)}",
                    "and {.code (1 | group)}."),
        "i" = "Fit with {.code block = NULL} to use quadrature."),
      class = "sandtablR_error_behaviour", call = call
    )
  }
  nAGQ
}

.beh_extract_sds <- function(fit, engine, use_block) {
  if (engine == "lme4") {
    vc <- as.data.frame(lme4::VarCorr(fit))
    pick <- function(g) {
      v <- vc$sdcor[vc$grp == g]
      if (length(v)) v[[1]] else NA_real_
    }
  } else {
    vc <- glmmTMB::VarCorr(fit)$cond
    pick <- function(g) {
      if (is.null(vc[[g]])) NA_real_ else as.numeric(attr(vc[[g]], "stddev"))
    }
  }
  list(group = pick(".group"),
       block = if (use_block) pick(".block") else NA_real_)
}

.beh_fixef <- function(fit, engine) {
  if (engine == "lme4") lme4::fixef(fit) else glmmTMB::fixef(fit)$cond
}

#' @export
format.sdt_behaviour_model <- function(x, ...) {
  rlang::check_dots_empty()
  m <- x$meta
  cov <- if (is.na(m$frame$min_coverage)) "all rows" else {
    sprintf("coverage >= %d", m$frame$min_coverage)
  }
  out <- c(
    sprintf("<sdt_behaviour_model> %s | %s frame (%s)",
            m$engine, x$frame, cov),
    sprintf("%d card%s x %d group%s in %d block%s = %d rows (of %d)",
            m$n_cards, .beh_s(m$n_cards), m$n_groups, .beh_s(m$n_groups),
            m$n_blocks, .beh_s(m$n_blocks), m$n_rows, m$n_rows_input),
    sprintf("%d outcome%s modelled in %.1fs:",
            length(x$outcomes), .beh_s(length(x$outcomes)), m$elapsed)
  )
  sds <- m$sds
  hdr <- sprintf("  %-13s %7s %8s %8s", "outcome", "prev", "sd_group",
                 "sd_block")
  rows <- sprintf(
    "  %-13s %7.3f %8.3f %8s%s",
    sds$outcome, sds$prevalence, sds$sd_group,
    ifelse(is.na(sds$sd_block), "-", sprintf("%.3f", sds$sd_block)),
    ifelse(sds$singular, "  singular",
           ifelse(sds$boundary, "  wide", ""))
  )
  out <- c(out, hdr, rows)
  out <- c(out, .beh_format_composition(x))
  if (nrow(x$skipped)) {
    out <- c(out, sprintf("%d skipped:", nrow(x$skipped)),
             sprintf("  %-13s %s", x$skipped$outcome, x$skipped$reason))
  }
  out
}

# The composition block of the printed model: the covariates, and what they
# did to the session spread. It prints whenever the model does, because a
# reader deciding whether the covariates earned their place should not have
# to know which corner of `$meta` to go looking in.
.beh_format_composition <- function(x) {
  bv <- x$meta$block_variance
  covs <- x$terms$block_covariates %||% character()
  if (is.null(bv) || !nrow(bv) || !length(covs)) {
    return(character())
  }
  pct <- ifelse(
    is.na(bv$var_explained), "-",
    sprintf("%.0f%%", 100 * bv$var_explained)
  )
  c(
    strwrap(
      sprintf("%d block covariate%s: %s", length(covs), .beh_s(length(covs)),
              paste(covs, collapse = ", ")),
      width = 76, exdent = 2
    ),
    sprintf("  %-13s %8s %8s %9s %8s", "outcome", "blk_sd0", "blk_sd1",
            "var_expl", "dAIC"),
    sprintf("  %-13s %8.3f %8.3f %9s %8.1f", bv$outcome,
            bv$sd_block_before, bv$sd_block_after, pct, bv$delta_aic)
  )
}

.beh_s <- function(n) if (n == 1L) "" else "s"

# A team standard deviation this wide on the logit scale is separation, not a
# smooth tendency: the middle 95% of teams would span a rate from about
# 1 in 20,000 to its mirror image. Measured on the validation corpus it is the
# signature of an outcome most teams never produced at all.
.beh_wide_sd <- function() 5

#' @export
print.sdt_behaviour_model <- function(x, ...) {
  rlang::check_dots_empty()
  cat(format(x), sep = "\n")
  invisible(x)
}


# Simulating new groups -------------------------------------------------------

#' Simulate new groups from a fitted behaviour model
#'
#' Draws teams that were never in the data. For each simulated group and each
#' modelled outcome the team's own random effect is redrawn from
#' `N(0, sd_group)`, a session effect is redrawn from `N(0, sd_block)`, and the
#' outcome is then drawn per card from the fitted card effects:
#'
#' ```
#' p = plogis(card effect + team effect + session effect)
#' ```
#'
#' This is deliberately not [stats::simulate()] on the fitted model with the
#' fitted grouping structure, which would put the training teams back on the
#' table. Every simulated group here is a new team playing in a new session.
#'
#' @section Which random effects are redrawn:
#' With `new_teams = TRUE` (the default) both the group and the block effect
#' are drawn fresh and independently for every simulated group, so a simulated
#' group is "a new team in a new session" and its total random effect has
#' variance `sd_group^2 + sd_block^2` -- the same marginal spread a real,
#' previously unseen group has.
#'
#' With `new_teams = FALSE` the fitted conditional modes are reused: simulated
#' groups are the observed teams, with their own session effects, playing the
#' same cards again. On a model fitted with block covariates their own
#' course's composition goes back into the linear predictor as well, because
#' the fitted session modes are residuals with that part already removed.
#' That is a posterior-predictive check of the fit, not a generator of new
#' teams. It takes the first `n_groups` fitted teams in frame
#' order rather than a random sample, so it is reproducible without a seed;
#' pass `n_groups = model$meta$n_groups` to use them all, which is what a
#' like-for-like check against the observed frame wants. The `draws` attribute
#' records which observed team each simulated one came from.
#'
#' @section Simulating a course of a given composition:
#' `composition` takes one row -- or several -- of block-level covariate
#' values on their natural scale, so a caller can ask for "a course whose
#' participants look like this". Each row is a course: `n_groups` teams are
#' drawn inside **each** of them, and they share one drawn session effect,
#' because groups in one course played in one session. The result carries a
#' `.composition` column naming the row each simulated team belongs to.
#'
#' Conditioning changes two things. The covariates' fixed contribution is
#' added to the linear predictor, and the session effect is drawn from the
#' **residual** block SD -- what is left after the covariates -- rather than
#' from the total. With `composition = NULL` neither happens: the covariates
#' sit at their fitted mean and the session effect is drawn from the total
#' block SD, which is the marginal spread of an unseen course and exactly
#' what a model fitted without covariates produces from the same seed.
#'
#' A composition value outside the range the fitted courses covered is
#' warned about rather than refused. It is extrapolation from a between-course
#' slope estimated on as many points as there were courses, and it does not
#' travel.
#'
#' `composition` cannot be combined with `new_teams = FALSE`: reusing the
#' fitted teams reuses the courses they actually played in, so asking for a
#' hypothetical composition at the same time asks for two contradictory
#' things.
#'
#' Each row is labelled by the block key when the caller passes one, so a
#' slice of [sdt_course_composition()] keeps its file name and the reader can
#' see which real course a hypothetical one was copied from. That label is a
#' course identifier, and [sdt_export_simulation()] will refuse to write it
#' into a pseudonymised bundle -- rename the key column to something neutral
#' before simulating if the result is going to be exported.
#'
#' @section What is not reproduced:
#' Each outcome was fitted separately, so a simulated team's tendencies are
#' drawn independently across outcomes. Real teams that image more also operate
#' more; that correlation is absent here. Card-to-card dependence within a
#' casualty (an eFAST leading to a CT) is likewise absent.
#'
#' Every simulated group is also dealt every card, while an observed group can
#' have missing outcome values -- a casualty its team never triaged. Those
#' groups differ in *which* cards contribute to their rate, and that is spread
#' the simulation has no way to produce. On the validation corpus `red` is
#' missing in 105 of 3410 cells and its dispersion ratio reads 1.20 over all
#' 110 groups but 0.97 over the 64 groups with a complete card set.
#' `model$meta$sds$n_missing` counts those cells, so a fidelity comparison can
#' restrict itself to the groups that have none.
#'
#' @param model An `sdt_behaviour_model` from [sdt_fit_behaviour()].
#' @param n_groups Number of simulated groups per replicate.
#' @param cards Cards to deal to each simulated group. `NULL` uses the fitted
#'   frame's cards; anything passed must be a subset of them.
#' @param seed Optional integer seed, applied with [withr::local_seed()].
#' @param new_teams If `TRUE`, redraw the team and session effects. If `FALSE`,
#'   reuse the fitted ones.
#' @param nsim Number of replicates. Replicates are stacked and carry a
#'   `.replicate` column; `sim_group` is unique across the whole result, so
#'   grouping by it never merges two different simulated teams.
#' @param composition Optional block-level covariate values to condition on:
#'   a data frame with one row per course (a slice of
#'   [sdt_course_composition()] will do), or a named list or vector for a
#'   single course. `NULL` simulates marginally, exactly as a model fitted
#'   without covariates does. Requires a model fitted with
#'   `block_covariates`.
#'
#' @return An S3 object of class `sdt_simulated_groups`, a
#'   [tibble::tibble()] with `.replicate`, `sim_group`, `card` and one logical
#'   column per modelled outcome. Carries the fitted `model`, the `call`, and a
#'   `draws` attribute holding the random effects used for each simulated
#'   group.
#' @seealso [sdt_fit_behaviour()].
#' @export
#' @examples
#' set.seed(1)
#' d <- expand.grid(
#'   card = sprintf("c%02d", seq_len(8)),
#'   group_id = sprintf("g%02d", seq_len(12)),
#'   stringsAsFactors = FALSE
#' )
#' d$session_id <- ifelse(d$group_id < "g07", "s1", "s2")
#' b <- stats::rnorm(12, sd = 1.2)
#' p <- stats::plogis(-0.5 + b[match(d$group_id, unique(d$group_id))])
#' d$did_efast <- stats::runif(nrow(d)) < p
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   fit <- sdt_fit_behaviour(d, outcomes = "efast", verbose = FALSE)
#'   sdt_simulate_groups(fit, n_groups = 5, seed = 1)
#' }
sdt_simulate_groups <- function(model,
                                n_groups = 20L,
                                cards = NULL,
                                seed = NULL,
                                new_teams = TRUE,
                                nsim = 1L,
                                composition = NULL) {
  call <- rlang::current_env()
  .check_class(model, "sdt_behaviour_model")
  n_groups <- .check_count(n_groups)
  nsim <- .check_count(nsim)
  .check_flag(new_teams)
  if (!is.null(seed)) {
    withr::local_seed(.check_count(seed))
  }

  levels_fit <- model$terms$card_levels
  if (is.null(cards)) {
    cards <- levels_fit
  } else {
    if (!is.character(cards) && !is.factor(cards)) {
      cards <- as.character(cards)
    }
    cards <- as.character(cards)
    unknown <- setdiff(unique(cards), levels_fit)
    if (length(unknown)) {
      n_unknown <- length(unknown)
      shown <- utils::head(unknown, 5L)
      cli::cli_abort(
        c("{.arg cards} must be a subset of the fitted frame's cards.",
          "x" = "{n_unknown} unknown card{?s}: {.val {shown}}."),
        class = "sandtablR_error_behaviour", call = call
      )
    }
    cards <- unique(cards)
  }

  engine <- model$meta$engine
  comp <- .beh_composition(model, composition, new_teams, call)
  .beh_warn_unconditioned(model, comp, new_teams)
  pool <- .beh_group_pool(model, new_teams, n_groups, call)
  n_groups <- pool$n_groups

  reps <- lapply(seq_len(nsim), function(r) {
    .beh_simulate_one(model, engine, cards, n_groups, new_teams, pool, r,
                      comp)
  })
  out <- dplyr::bind_rows(lapply(reps, `[[`, "sim"))
  draws <- dplyr::bind_rows(lapply(reps, `[[`, "draws"))

  structure(
    tibble::as_tibble(out),
    class = c("sdt_simulated_groups", class(tibble::tibble())),
    model = model, call = sys.call(), draws = draws,
    seed = seed, new_teams = new_teams, composition = comp
  )
}

# Conditioning on a composition -----------------------------------------------

# Read the requested courses, on the natural scale, and turn them into the z
# units the fit works in. Returns NULL when nothing was asked for, and that
# NULL keeps the unconditioned path byte-identical to what it was before
# composition existed.
.beh_composition <- function(model, composition, new_teams, call) {
  if (is.null(composition)) {
    return(NULL)
  }
  spec <- model$meta$composition
  if (is.null(spec)) {
    cli::cli_abort(
      c("{.arg composition} needs a model fitted with block covariates.",
        "i" = "Refit with {.arg block_data} and {.arg block_covariates}."),
      class = "sandtablR_error_behaviour", call = call
    )
  }
  if (!new_teams) {
    cli::cli_abort(
      c("{.arg composition} and {.code new_teams = FALSE} contradict.",
        "x" = "Reusing the fitted teams reuses the courses they played in.",
        "i" = "Ask for a hypothetical composition or for the observed teams,
               not for both at once."),
      class = "sandtablR_error_behaviour", call = call
    )
  }
  if (is.numeric(composition) || is.logical(composition)) {
    composition <- as.list(composition)
  }
  if (is.list(composition) && !is.data.frame(composition)) {
    composition <- tryCatch(
      as.data.frame(composition, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
  }
  if (!is.data.frame(composition) || !nrow(composition)) {
    cli::cli_abort(
      c("{.arg composition} must be a data frame with at least one row.",
        "i" = "One row per course, carrying the covariates the model was
               fitted with."),
      class = "sandtablR_error_behaviour", call = call
    )
  }

  cov <- spec$covariates
  absent <- setdiff(cov$name, names(composition))
  if (length(absent)) {
    n_absent <- length(absent)
    fitted_on <- cov$name
    cli::cli_abort(
      c("{.arg composition} is missing {n_absent} fitted covariate{?s}.",
        "x" = "Not found: {.val {absent}}.",
        "i" = "The model was fitted on {.val {fitted_on}}."),
      class = "sandtablR_error_behaviour", call = call
    )
  }

  n <- nrow(composition)
  z <- matrix(NA_real_, nrow = n, ncol = nrow(cov),
              dimnames = list(NULL, cov$term))
  bad_type <- bad_na <- list()
  outside <- character()
  for (j in seq_len(nrow(cov))) {
    nm <- cov$name[[j]]
    v <- composition[[nm]]
    if (is.logical(v)) v <- as.numeric(v)
    if (!is.numeric(v)) {
      cls <- class(composition[[nm]])[[1]]
      bad_type[[nm]] <- .beh_cov_bullet(
        nm, paste0("is ", cls, ", not numeric or logical")
      )
      next
    }
    if (anyNA(v)) {
      bad_na[[nm]] <- .beh_cov_bullet(nm, "is missing in row",
                                      which(is.na(v)))
      next
    }
    if (any(v < cov$min[[j]] | v > cov$max[[j]])) {
      outside <- c(outside, sprintf(
        "%s asked for %s where the courses ran %s", nm,
        .beh_num(range(v)), .beh_num(c(cov$min[[j]], cov$max[[j]]))
      ))
    }
    z[, j] <- (v - cov$centre[[j]]) / cov$scale[[j]]
  }
  .beh_cov_abort(
    list(bad_type, bad_na),
    "{.arg composition} cannot be read.",
    "Every fitted covariate must be present, numeric or logical, and not
     missing.",
    call
  )
  if (length(outside)) {
    n_out <- length(outside)
    cli::cli_warn(
      c("!" = "{n_out} covariate value{?s} outside the fitted range:
               {outside}.",
        "i" = "The model is extrapolating from a between-course slope
               estimated on as many points as there were courses."),
      class = "sandtablR_warning_behaviour"
    )
  }
  list(z = z, labels = .beh_comp_labels(composition, spec$key, n),
       values = tibble::as_tibble(composition[, cov$name, drop = FALSE]))
}

# What to call each requested course. The block key when the caller passed
# one, so a row lifted straight out of `sdt_course_composition()` keeps its
# file name; a position otherwise.
.beh_comp_labels <- function(composition, key, n) {
  if (!is.null(key) && key %in% names(composition)) {
    lab <- as.character(composition[[key]])
    if (!anyNA(lab) && !anyDuplicated(lab)) {
      return(lab)
    }
  }
  sprintf("composition%02d", seq_len(n))
}

# The fixed contribution of each requested course, on the logit scale.
.beh_comp_eta <- function(model, nm, engine, z) {
  beta <- .beh_fixef(model$fits[[nm]], engine)
  b <- beta[colnames(z)]
  if (anyNA(b)) {
    gone <- colnames(z)[is.na(b)]
    n_gone <- length(gone)
    cli::cli_abort(
      c("The fit has no coefficient for {n_gone} block covariate{?s}.",
        "x" = "Missing: {.val {gone}}.",
        "i" = "The model formula changed without the simulator following."),
      class = "sandtablR_error_behaviour"
    )
  }
  as.vector(z %*% b)
}

# Reusing a fitted team reuses the course it actually played in, so that
# course's composition contribution is known and belongs in the linear
# predictor: the fitted block modes are residuals with exactly that part
# already taken out of them, and leaving it out would shift every reused team
# back towards the average course.
.beh_observed_comp_eta <- function(model, nm, engine, source_ids) {
  spec <- model$meta$composition
  blocks <- model$meta$frame$block_of[source_ids]
  if (is.null(spec) || is.null(blocks)) {
    return(rep(0, length(source_ids)))
  }
  z <- spec$z[match(blocks, spec$blocks), , drop = FALSE]
  eta <- .beh_comp_eta(model, nm, engine, z)
  eta[is.na(eta)] <- 0
  eta
}

# A model that carries covariates and is asked to draw new teams without
# naming a course is doing something defensible but easy to misread, so it
# says so. Reusing the fitted teams needs no such warning: their courses are
# known, and their composition is put back in by hand above.
.beh_warn_unconditioned <- function(model, comp, new_teams) {
  if (!is.null(comp) || !new_teams || is.null(model$meta$composition)) {
    return(invisible(NULL))
  }
  n_cov <- nrow(model$meta$composition$covariates)
  cli::cli_warn(c(
    "!" = "This model carries {n_cov} block covariate{?s} and no
           {.arg composition} was given.",
    "i" = "The covariates are held at their fitted mean and the session
           effect is drawn from the total block SD, not the residual one, so
           a simulated group is an average course rather than a stated one.",
    "i" = "Pass {.arg composition} to condition on a course."
  ), class = "sandtablR_warning_behaviour")
}

# Which teams the simulation is built from. For `new_teams = FALSE` the fitted
# groups are reused as they are, so the result can be lined up against the
# observed frame one team at a time.
.beh_group_pool <- function(model, new_teams, n_groups, call) {
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
    ), class = "sandtablR_warning_behaviour")
    n_groups <- n_avail
  }
  list(n_groups = n_groups, ids = fitted_groups[seq_len(n_groups)])
}

.beh_simulate_one <- function(model, engine, cards, n_groups, new_teams,
                              pool, rep_id, comp = NULL) {
  # Each composition row is a course and gets `n_groups` teams of its own, so
  # a caller comparing two courses gets the same number of teams in each.
  n_comp <- if (is.null(comp)) 1L else nrow(comp$z)
  n_sim <- n_groups * n_comp
  comp_of <- rep(seq_len(n_comp), each = n_groups)
  ids <- sprintf("g%04d", (rep_id - 1L) * n_sim + seq_len(n_sim))
  n_cards <- length(cards)
  sim <- tibble::tibble(
    .replicate = rep(rep_id, n_sim * n_cards),
    sim_group = rep(ids, each = n_cards),
    card = rep(cards, times = n_sim)
  )
  labels <- if (is.null(comp)) NA_character_ else comp$labels[comp_of]
  if (!is.null(comp)) {
    sim$.composition <- rep(labels, each = n_cards)
  }
  idx <- rep(seq_len(n_sim), each = n_cards)
  cov_terms <- model$terms$cov_terms %||% character()
  draws <- list()
  for (nm in model$outcomes) {
    fit <- model$fits[[nm]]
    eta_card <- .beh_card_eta(fit, engine, cards, cov_terms)
    re <- .beh_draw_re(model, nm, engine, n_sim, new_teams, pool, comp,
                       comp_of)
    eta_comp <- if (!is.null(comp)) {
      .beh_comp_eta(model, nm, engine, comp$z)[comp_of]
    } else if (!new_teams && length(cov_terms)) {
      .beh_observed_comp_eta(model, nm, engine, re$source)
    } else {
      rep(0, n_sim)
    }
    if (anyNA(eta_card)) {
      gone <- cards[is.na(eta_card)]
      n_gone <- length(gone)
      cli::cli_warn(
        c("!" = "{.field {nm}} was never observed on {n_gone} card{?s}.",
          "x" = "No coefficient for: {.val {gone}}.",
          "i" = "Those cells are simulated as {.code NA} rather than
                 guessed."),
        .frequency = "once",
        .frequency_id = paste0("sdt_sim_card_", nm)
      )
    }
    eta <- eta_card[match(sim$card, cards)] + re$group[idx] +
      re$block[idx] + eta_comp[idx]
    # Draw only where the outcome has a coefficient. rbinom() on an NA
    # probability warns "NAs produced", which says nothing useful on top of
    # the warning above.
    y <- rep(NA, length(eta))
    ok <- !is.na(eta)
    y[ok] <- stats::rbinom(sum(ok), 1L, stats::plogis(eta[ok])) == 1L
    sim[[nm]] <- as.logical(y)
    draws[[nm]] <- tibble::tibble(
      .replicate = rep_id, sim_group = ids, outcome = nm,
      composition = labels,
      source_group = re$source %||% NA_character_,
      b_group = re$group, b_block = re$block, b_composition = eta_comp
    )
  }
  list(sim = sim, draws = dplyr::bind_rows(draws))
}

# The fixed-effect linear predictor for each card. Only `card` is a fixed
# effect, so this is a lookup; the design matrix is rebuilt from the fitted
# levels and checked against the coefficient names rather than assumed to
# match, so a future change to the formula fails loudly here.

# The card levels this particular fit actually carries. It is not always the
# frame's card set: an outcome that is missing for every group on some card
# loses that level to droplevels(), and the fit then has no coefficient for it.
.beh_fit_card_levels <- function(fit, engine) {
  mf <- if (identical(engine, "glmmTMB")) {
    fit$frame
  } else {
    methods::slot(fit, "frame")
  }
  levels(mf$.card)
}

.beh_card_eta <- function(fit, engine, cards, cov_terms = character()) {
  # Always build the design from the FIT's own levels rather than the frame's.
  # Using the frame's used to abort with "the model formula changed", which
  # named the wrong cause: the formula is fine, the outcome simply has no
  # observation on that card.
  levels_fit <- .beh_fit_card_levels(fit, engine)
  nd <- data.frame(.card = factor(levels_fit, levels = levels_fit))
  # Block covariates are centred, so holding them at zero evaluates the card
  # part at the average course. A course's own contribution is added
  # separately, which is what lets one fit serve many hypothetical courses.
  for (tm in cov_terms) nd[[tm]] <- 0
  x <- stats::model.matrix(.beh_design_formula(cov_terms), data = nd)
  beta <- .beh_fixef(fit, engine)
  if (!identical(sort(colnames(x)), sort(names(beta)))) {
    cli::cli_abort(
      c("Fitted fixed effects do not match the card design matrix.",
        "i" = "The model formula changed without the simulator following."),
      class = "sandtablR_error_behaviour"
    )
  }
  eta <- as.vector(x[, names(beta), drop = FALSE] %*% beta)
  # NA for a card this outcome was never fitted on. The simulator carries the
  # NA through rather than inventing a value, and the caller is told which
  # cards are affected.
  eta[match(cards, levels_fit)]
}

# Draw (or look up) the random effects for one outcome.
#
# Which session spread to draw from is the one real decision here.
# Conditioning on a composition removes the part of the session effect the
# covariates account for, so what is left to draw is the residual. Not
# conditioning leaves the whole of it in, which is the marginal spread of an
# unseen course and, for a model with no covariates, the only value there is.
.beh_draw_re <- function(model, nm, engine, n_groups, new_teams, pool,
                         comp = NULL, comp_of = NULL) {
  sds <- model$meta$sds
  i <- match(nm, sds$outcome)
  sd_g <- sds$sd_group[[i]]
  sd_b <- if (is.null(comp) && !is.null(sds$sd_block_total)) {
    sds$sd_block_total[[i]]
  } else {
    sds$sd_block[[i]]
  }
  if (is.na(sd_b)) sd_b <- 0
  if (new_teams) {
    # Order matters: the team effect is drawn first, as it always was, so an
    # unconditioned draw reproduces the pre-composition result seed for seed.
    g <- stats::rnorm(n_groups, sd = sd_g)
    b <- if (is.null(comp)) {
      stats::rnorm(n_groups, sd = sd_b)
    } else {
      # One session effect per course, shared by every team inside it: the
      # groups in one course played in one session, and pretending otherwise
      # would throw away the structure the covariate describes.
      stats::rnorm(nrow(comp$z), sd = sd_b)[comp_of]
    }
    return(list(group = g, block = b, source = NULL))
  }
  re <- .beh_ranef(model$fits[[nm]], engine)
  g <- re$group[pool$ids]
  g[is.na(g)] <- 0
  blocks <- model$meta$frame$block_of[pool$ids]
  b <- if (is.null(blocks)) rep(0, n_groups) else re$block[blocks]
  b[is.na(b)] <- 0
  list(group = unname(g), block = unname(b), source = pool$ids)
}

.beh_ranef <- function(fit, engine) {
  re <- if (engine == "lme4") lme4::ranef(fit) else glmmTMB::ranef(fit)$cond
  grab <- function(term) {
    if (is.null(re[[term]])) {
      return(stats::setNames(numeric(), character()))
    }
    stats::setNames(re[[term]][[1]], rownames(re[[term]]))
  }
  list(group = grab(".group"), block = grab(".block"))
}

#' @export
format.sdt_simulated_groups <- function(x, ...) {
  rlang::check_dots_empty()
  model <- attr(x, "model")
  outcomes <- if (is.null(model)) character() else model$outcomes
  n_g <- length(unique(x$sim_group))
  n_c <- length(unique(x$card))
  n_r <- length(unique(x$.replicate))
  kind <- if (isTRUE(attr(x, "new_teams"))) "new teams" else "refitted teams"
  comp <- attr(x, "composition")
  out <- c(
    sprintf("<sdt_simulated_groups> %d %s x %d card%s in %d replicate%s",
            n_g, kind, n_c, .beh_s(n_c), n_r, .beh_s(n_r)),
    sprintf("%d outcome%s: %s", length(outcomes), .beh_s(length(outcomes)),
            paste(outcomes, collapse = ", "))
  )
  if (!is.null(comp)) {
    n_k <- length(comp$labels)
    out <- c(out, sprintf(
      "conditioned on %d composition%s: %s", n_k, .beh_s(n_k),
      paste(utils::head(comp$labels, 4L), collapse = ", ")
    ))
  }
  out
}

#' @export
print.sdt_simulated_groups <- function(x, ...) {
  cat(format(x), sep = "\n")
  y <- x
  class(y) <- setdiff(class(y), "sdt_simulated_groups")
  print(y, ...)
  invisible(x)
}
