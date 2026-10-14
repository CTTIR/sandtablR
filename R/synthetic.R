# Synthetic cohort generation ------------------------------------------------
# Two independent jobs, deliberately kept apart:
#
#   sdt_synthetic_cohort()  De novo generation from marginals; no real data
#                           needed. The "base" backend is the always-available
#                           reference implementation and is behaviourally
#                           frozen by a golden snapshot test. "simstudy" is a
#                           richer optional backend with explicit correlation.
#
#   sdt_synthesise()        Synthesis *from* a real seed cohort via synthpop's
#                           sequential CART models, for when you have data you
#                           cannot share but want a releasable replica.
#
# Every distributional constant comes from inst/extdata/tdsc_reference.rds via
# sdt_reference(), so the generators and the simulation engine cannot drift
# apart.

#' Generate a synthetic mass-casualty cohort
#'
#' Produces a reproducible synthetic cohort, one row per casualty, with the
#' fields expected by [sdt_scenario()]. Marginals default to the published
#' reference distributions returned by [sdt_reference()], and can be
#' overridden.
#'
#' @param n Number of casualties to generate (positive whole number).
#' @param triage_prob Named numeric vector of sighting-category probabilities.
#'   Defaults to the published triage mix. Need not sum to 1; it is normalised.
#' @param seed Optional integer seed. Applied with [withr::local_seed()], so
#'   the caller's random stream is restored on exit.
#' @param backend Either `"base"` (always available, frozen behaviour) or
#'   `"simstudy"`, which draws correlated age, injury severity and life points
#'   through a \pkg{simstudy} definition table. If \pkg{simstudy} is not
#'   installed, `"simstudy"` warns and falls back to `"base"`.
#'
#' @return A `cohort` object (a [tibble::tibble()] subclass) with columns `id`,
#'   `age`, `sex`, `life_points`, `ventilated`, `injury_region`, `triage`,
#'   `therapy`, `arrival_order`. The `"simstudy"` backend additionally carries
#'   `iss`, an injury-severity proxy.
#' @seealso [sdt_synthesise()] to synthesise from a real seed cohort instead.
#' @export
#' @examples
#' sdt_synthetic_cohort(n = 10, seed = 42)
#'
#' # Override the mix to stress the red pathway.
#' sdt_synthetic_cohort(
#'   n = 10, triage_prob = c(red = 0.8, yellow = 0.2), seed = 1
#' )
sdt_synthetic_cohort <- function(n = 20L,
                                 triage_prob = NULL,
                                 seed = NULL,
                                 backend = c("base", "simstudy")) {
  n <- .check_count(n)
  backend <- rlang::arg_match(backend)
  triage_prob <- triage_prob %||% .ref_published("triage_mix")
  triage_prob <- .check_triage_prob(triage_prob)
  if (!is.null(seed)) {
    withr::local_seed(.check_count(seed))
  }

  if (identical(backend, "simstudy") &&
        !requireNamespace("simstudy", quietly = TRUE)) {
    cli::cli_warn(c(
      "!" = "Package {.pkg simstudy} is not installed; using {.val base}.",
      "i" = "Install it with {.code install.packages(\"simstudy\")}."
    ))
    backend <- "base"
  }

  out <- switch(backend,
    base = .cohort_base(n, triage_prob),
    simstudy = .cohort_simstudy(n, triage_prob)
  )
  sdt_cohort(out)
}

# Normalise and check a triage probability vector.
.check_triage_prob <- function(p, call = rlang::caller_env()) {
  if (!is.numeric(p) || is.null(names(p)) || anyNA(p)) {
    cli::cli_abort(
      "{.arg triage_prob} must be a named numeric vector with no {.val NA}.",
      call = call
    )
  }
  unknown <- setdiff(names(p), sdt_triage_levels())
  if (length(unknown)) {
    cli::cli_abort(
      c(
        "{.arg triage_prob} must be named after sighting categories.",
        "x" = "Unknown: {.val {unknown}}.",
        "i" = "Allowed: {.val {sdt_triage_levels()}}."
      ),
      call = call
    )
  }
  if (any(p < 0) || sum(p) <= 0) {
    cli::cli_abort(
      "{.arg triage_prob} must be non-negative and sum to more than zero.",
      call = call
    )
  }
  p / sum(p)
}

# The reference generator. Behaviourally frozen: the order of the random draws
# below is part of the contract and is pinned by a golden snapshot test. Add
# new fields at the end, never in the middle.
.cohort_base <- function(n, triage_prob) {
  triage <- sample(names(triage_prob), n, replace = TRUE, prob = triage_prob)
  life <- .life_points_for(triage)
  therapy <- .therapy_for(triage)
  age_par <- .ref_default("age")
  sex_mix <- .ref_default("sex_mix")
  p_vent <- .ref_default("p_ventilated_red")

  tibble::tibble(
    id = sprintf("P%03d", seq_len(n)),
    age = .clamp_age(stats::rnorm(n, age_par[["mean"]], age_par[["sd"]])),
    sex = sample(names(sex_mix), n, replace = TRUE, prob = sex_mix),
    life_points = life,
    ventilated = triage == "red" & stats::runif(n) < p_vent,
    injury_region = sample(sdt_injury_regions(), n, replace = TRUE),
    triage = triage,
    therapy = therapy,
    arrival_order = seq_len(n)
  )
}

# simstudy backend: age, an injury-severity proxy and life points come from an
# explicit correlation chain rather than being drawn independently, so a
# high-severity casualty is systematically older and lower on life points.
# Sighting category is then derived from life points *by rule*, which is what
# makes this cohort suitable for training a model that has to learn the rule.
.cohort_simstudy <- function(n, triage_prob) {
  age_par <- .ref_default("age")
  sex_mix <- .ref_default("sex_mix")
  p_vent <- .ref_default("p_ventilated_red")

  def <- simstudy::defData(
    varname = "age", dist = "normal",
    formula = age_par[["mean"]], variance = age_par[["sd"]]^2
  )
  # Injury-severity proxy on the conventional 1-75 scale, drifting up with age.
  def <- simstudy::defData(
    def, varname = "iss", dist = "normal",
    formula = "12 + 0.18 * age", variance = 90
  )
  # Life points fall as severity rises, on the card's 1-6 scale. The noise term
  # keeps the rule learnable without making it trivially invertible.
  def <- simstudy::defData(
    def, varname = "life_raw", dist = "normal",
    formula = "7.2 - 0.08 * iss", variance = 0.8
  )
  dt <- simstudy::genData(n, def)

  age <- .clamp_age(dt[["age"]])
  iss <- pmin(75L, pmax(1L, as.integer(round(dt[["iss"]]))))
  life <- .clamp_life(dt[["life_raw"]])
  # Derive the category from acuity, then nudge the realised mix toward the
  # requested marginals while keeping it monotone in life points.
  triage <- .rebalance_triage(life, triage_prob)

  tibble::tibble(
    id = sprintf("P%03d", seq_len(n)),
    age = age,
    sex = sample(names(sex_mix), n, replace = TRUE, prob = sex_mix),
    life_points = life,
    ventilated = triage == "red" & stats::runif(n) < p_vent,
    injury_region = sample(sdt_injury_regions(), n, replace = TRUE),
    triage = triage,
    therapy = .therapy_for(triage),
    arrival_order = seq_len(n),
    iss = iss
  )
}

.clamp_age <- function(x) as.integer(pmin(110L, pmax(0L, round(x))))

.clamp_life <- function(x) {
  rng <- .sdt_living_life_range()
  as.integer(pmin(rng[[2L]], pmax(rng[[1L]], round(x))))
}

# Assign sighting categories by acuity rank so the realised mix matches the
# requested marginals exactly, and the most critical casualties are the reds.
.rebalance_triage <- function(life, triage_prob) {
  n <- length(life)
  if (!n) {
    return(character())
  }
  levels <- sdt_triage_levels()
  target <- triage_prob[levels]
  target[is.na(target)] <- 0
  counts <- .fill_to(round(target * n), n)
  out <- character(n)
  # order(): lowest life points (most critical) first, ties by position.
  out[order(life, seq_len(n))] <- rep(levels, times = counts)
  out
}

# Make a vector of counts sum to exactly n by adjusting the largest cell.
.fill_to <- function(counts, n) {
  counts <- as.integer(counts)
  short <- n - sum(counts)
  if (short != 0L) {
    i <- which.max(counts)
    counts[[i]] <- counts[[i]] + short
  }
  pmax(0L, counts)
}

# Life points decrease with acuity (red most critical).
.life_points_for <- function(triage) {
  bands <- .ref_default("life_bands")
  fallback <- bands[["yellow"]]
  vapply(triage, function(t) {
    rng <- bands[[t]] %||% fallback
    sample(rng, 1L)
  }, integer(1), USE.NAMES = FALSE)
}

# Therapy concept conditioned on sighting category.
.therapy_for <- function(triage) {
  given <- .ref_default("therapy_given_triage")
  fallback <- given[["yellow"]]
  vapply(triage, function(t) {
    p <- given[[t]] %||% fallback
    sample(names(p), 1L, prob = p)
  }, character(1), USE.NAMES = FALSE)
}


# Synthesis from a real seed cohort ------------------------------------------

#' Synthesise a releasable replica of a real cohort
#'
#' Fits sequential CART models to a real casualty cohort with
#' [synthpop::syn()] and draws a synthetic replica from them. Use this when the
#' real data cannot be shared but its multivariate structure should survive
#' into something that can be.
#'
#' Unlike [sdt_synthetic_cohort()], which generates from marginals alone, this
#' reproduces the joint structure of the seed data, including relationships
#' nobody thought to specify. That is the point, and also the risk: a synthetic
#' record that lands too close to a real one is a disclosure. Score the replica
#' with [sdt_synthesis_utility()] before releasing it.
#'
#' @param data A real cohort, or any data frame carrying the cohort columns.
#' @param n Number of synthetic rows to draw. Defaults to `nrow(data)`.
#' @param method \pkg{synthpop} method, passed to [synthpop::syn()]. `"cart"`
#'   is the default and handles mixed types without distributional assumptions.
#' @param seed Optional integer seed, applied with [withr::local_seed()].
#' @param verbose If `TRUE`, let \pkg{synthpop} report its progress.
#'
#' @return A `cohort` object of `n` rows. Identifier and ordering columns are
#'   regenerated rather than synthesised, because carrying them over would leak
#'   the seed data's row identities.
#' @seealso [sdt_synthesis_utility()] to score the replica against its seed.
#' @export
#' @examples
#' seed_cohort <- sdt_synthetic_cohort(n = 60, seed = 1)
#' if (requireNamespace("synthpop", quietly = TRUE)) {
#'   replica <- sdt_synthesise(seed_cohort, n = 30, seed = 2)
#'   replica
#' }
sdt_synthesise <- function(data, n = NULL, method = "cart", seed = NULL,
                           verbose = FALSE) {
  .require_pkgs("synthpop")
  .check_string(method)
  .check_flag(verbose)
  data <- tibble::as_tibble(data)
  if (!nrow(data)) {
    cli::cli_abort("{.arg data} must have at least one row to synthesise from.")
  }
  n <- .check_count(n %||% nrow(data))
  if (!is.null(seed)) {
    withr::local_seed(.check_count(seed))
  }

  # Identifiers and arrival order are regenerated, never synthesised: they
  # carry row identity, not structure.
  carried <- setdiff(names(data), c("id", "arrival_order"))
  seed_df <- .as_synth_frame(data[, carried, drop = FALSE])

  syn <- .quietly(verbose, function() {
    synthpop::syn(seed_df, m = 1L, k = n, method = method, print.flag = verbose)
  })
  out <- .from_synth_frame(tibble::as_tibble(syn$syn))
  out$id <- sprintf("P%03d", seq_len(nrow(out)))
  out$arrival_order <- seq_len(nrow(out))
  sdt_cohort(out)
}

#' Score a synthetic replica against its seed cohort
#'
#' Reports how faithfully a synthetic cohort reproduces its seed, per variable,
#' so a replica can be judged before it is released. Categorical columns are
#' compared by total variation distance between their proportion tables;
#' numeric columns by standardised difference in means and by the
#' Kolmogorov-Smirnov statistic.
#'
#' @param original The real seed cohort.
#' @param synthetic A replica, e.g. from [sdt_synthesise()].
#'
#' @return A [tibble::tibble()] with one row per shared column and statistic:
#'   `variable`, `type`, `statistic`, `value`, and `flag`, which is `TRUE`
#'   where the divergence exceeds a conventional 0.1 and the column deserves a
#'   look. Returns a 0-row tibble of the same shape when nothing is shared.
#' @export
#' @examples
#' seed_cohort <- sdt_synthetic_cohort(n = 80, seed = 1)
#' replica <- sdt_synthetic_cohort(n = 80, seed = 2)
#' sdt_synthesis_utility(seed_cohort, replica)
sdt_synthesis_utility <- function(original, synthetic) {
  original <- tibble::as_tibble(original)
  synthetic <- tibble::as_tibble(synthetic)
  shared <- setdiff(
    intersect(names(original), names(synthetic)),
    c("id", "arrival_order")
  )
  empty <- tibble::tibble(
    variable = character(), type = character(),
    statistic = character(), value = numeric(), flag = logical()
  )
  if (!length(shared) || !nrow(original) || !nrow(synthetic)) {
    return(empty)
  }

  rows <- lapply(shared, function(v) {
    .utility_one(v, original[[v]], synthetic[[v]])
  })
  out <- dplyr::bind_rows(rows)
  out$flag <- out$value > 0.1
  out
}

.utility_one <- function(v, a, b) {
  if (is.numeric(a) && is.numeric(b)) {
    pooled <- stats::sd(c(a, b), na.rm = TRUE)
    std_diff <- if (isTRUE(pooled > 0)) {
      abs(mean(a, na.rm = TRUE) - mean(b, na.rm = TRUE)) / pooled
    } else {
      0
    }
    ks <- suppressWarnings(stats::ks.test(a, b))$statistic
    return(tibble::tibble(
      variable = v, type = "numeric",
      statistic = c("std_mean_diff", "ks"),
      value = c(std_diff, unname(ks))
    ))
  }
  lv <- union(unique(as.character(a)), unique(as.character(b)))
  pa <- prop.table(table(factor(as.character(a), levels = lv)))
  pb <- prop.table(table(factor(as.character(b), levels = lv)))
  tibble::tibble(
    variable = v, type = "categorical",
    statistic = "total_variation",
    value = sum(abs(pa - pb)) / 2
  )
}

# synthpop wants factors rather than characters, and a plain data frame.
.as_synth_frame <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  for (nm in names(x)) {
    if (is.character(x[[nm]])) x[[nm]] <- factor(x[[nm]])
  }
  x
}

# Put the character columns back the way they came in.
.from_synth_frame <- function(x) {
  for (nm in names(x)) {
    if (is.factor(x[[nm]])) x[[nm]] <- as.character(x[[nm]])
  }
  x
}

# synthpop prints in places regardless of print.flag; keep the console clean
# unless the caller asked for noise.
.quietly <- function(verbose, expr) {
  if (verbose) {
    return(expr())
  }
  out <- NULL
  utils::capture.output(out <- suppressMessages(expr()))
  out
}
