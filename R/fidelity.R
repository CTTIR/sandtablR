# Simulation fidelity ---------------------------------------------------------
# How faithful is a simulated cohort of teams to the real one? The question a
# generative model has to answer here is not "does the average come out right"
# -- a model with no team term gets the average right by construction -- but
# "do simulated groups differ from one another the way real groups do".
#
# So the headline number is a dispersion ratio: the between-group SD of a
# behavioural rate, observed over simulated. One means the simulator reproduces
# the real spread of teams; a number above one means it collapses the variance
# and manufactures teams that all behave alike; below one means it invents
# spread that is not there.
#
# Everything here is computed on a composition-matched frame, and that is not a
# stylistic choice. Groups are truncated at different points, so on the full
# deck every group is scored on a different card set and that composition
# variance is indistinguishable from behavioural variance. Measured on the 19
# real course exports: the observed between-group SD of the TASC rate is
# 0.1535 across the 149 groups on the full 46-card deck, and 0.1888 across the
# 140 groups that all saw the same 22 cards -- a 19% understatement of the
# team effect before a simulator is involved at all. `.fid_match()` therefore
# intersects the cards, keeps only the groups that saw all of them, and
# records what it dropped.

# The band a dispersion ratio has to land inside to pass. Wider above one than
# below because a simulator that slightly over-disperses is the safer error:
# it does not hide team-to-team variation, it exaggerates it.
.fid_band <- function() c(0.90, 1.15)

# Location tolerance, in units of the *observed* between-group SD. A simulator
# whose pooled rate sits more than a quarter of a real between-group SD away
# from the observed rate is getting the level wrong, however well it does on
# spread.
.fid_location_tol <- function() 0.25

# Outcomes that are properties of the patient card rather than of the team.
# They are the negative controls: see the "Negative controls" section of
# [sdt_fidelity()].
.fid_default_controls <- function() c("died", "ventilated")

# Column names that identify a row rather than describe a behaviour, and so
# are never scored as statistics.
.fid_group_names <- function() c("sim_group", "group_id", "group", "gid")
.fid_card_names <- function() c("card", "card_id")
.fid_rep_names <- function() {
  c(".replicate", "sim", ".sim", "replicate", "replication")
}
.fid_block_names <- function() c("session_id", "session", "block", "sid",
                                 "file")

.fid_key_names <- function() {
  c(.fid_group_names(), .fid_card_names(), .fid_rep_names(),
    .fid_block_names())
}

#' Score how faithful a simulation is to the real thing
#'
#' Compares simulated groups against observed groups on the statistics that
#' matter for a tabletop exercise, and reports one row per statistic. The
#' headline is the **dispersion ratio**: the between-group standard deviation
#' of a behavioural rate, observed divided by simulated. A simulator that
#' reproduces the average behaviour but makes every team look the same fails
#' here, which is the whole point of the function.
#'
#' @section What is compared:
#'
#' For each statistic, each group gets one number -- the share of its patient
#' cards on which the statistic was true. The spread of those group-level
#' rates is the quantity of interest, and the columns report:
#'
#' * `observed`, `simulated`: the between-group SD of the group rates.
#' * `ratio`: `observed / simulated`. `Inf` when the simulator produces no
#'   between-group spread at all, which is a real and reportable result.
#' * `ci_lower`, `ci_upper`: a percentile bootstrap interval on `ratio`,
#'   resampling groups on each side with the simulated cohort held fixed. It
#'   answers "how much would this move with a different sample of teams?".
#' * `rep_lower`, `rep_upper`, `rep_covers_one`: the interval across
#'   replicates -- `observed / SD(replicate)` for each simulated cohort, at the
#'   2.5th and 97.5th percentiles. It answers the other question, "how much
#'   would this move on a different RUN of the simulator?", which is the one
#'   that dominates for a rare behaviour and which the bootstrap cannot see.
#'   Needs at least ten replicates; `NA` below that.
#' * `observed_mean`, `simulated_mean`, `location_z`: the level check.
#'   `location_z` is the difference in pooled rates expressed in observed
#'   between-group SDs, so a model can also fail by getting the level wrong.
#' * `overlap`: the overlapping coefficient of the two distributions of
#'   group-level rates, from 0 (disjoint) to 1 (identical). Together with the
#'   `observed_p10`/`observed_p90`/`simulated_p10`/`simulated_p90` columns it
#'   shows how far the simulated teams reach, rather than only how wide they
#'   are on average.
#' * `verdict`: `"PASS"` only when the dispersion ratio lies inside
#'   0.90 and 1.15 **and** `abs(location_z)` is at most 0.25. `reason` names
#'   whichever check failed.
#'
#' The full per-group rates behind the summary are attached as the
#' `"group_rates"` attribute of the result, and the frame the comparison ran
#' on as `"frame"`.
#'
#' @section The composition-matched frame:
#'
#' Between-group dispersion is only meaningful when every group is scored on
#' the same cards. `sdt_fidelity()` enforces that: it intersects the card sets
#' of the two frames, then keeps only groups that saw every card in the
#' intersection, and records what it dropped in the `"frame"` attribute. On
#' the real course corpus, skipping this step reports no team signal at all,
#' because each truncated group saw a different deck and that composition
#' variance swamps the behavioural one.
#'
#' @section Negative controls:
#'
#' Outcomes fixed by the patient card rather than chosen by the team --
#' `died` and `ventilated` by default -- are marked `control = TRUE`. They are
#' the pipeline's own alarm, and they read backwards from the other rows: a
#' control is *expected* to pass even for a simulator with no team term at
#' all, so a **failing control means the frame or the pipeline is wrong, not
#' the model**. Check the matched frame before you believe anything else in
#' the table. The one exception is a simulator with no team-level randomness
#' at all: the discrete-event engine reads `died` and `ventilated` off the
#' patient card and never redraws them, so its controls are dead flat and
#' fail too. That is a finding about the engine -- it has no team layer of
#' any kind -- rather than about the frame, and the way to tell the two apart
#' is that a broken frame moves the controls a little while a missing team
#' layer flattens them completely.
#'
#' @param x The simulated groups: an `sdt_simulated_groups` object from
#'   [sdt_simulate_groups()], a `game_run` from [sdt_simulate()] (converted
#'   with [sdt_run_behaviour()]), or a plain data frame with one row per
#'   (group, card).
#' @param observed The observed groups, in the same shape. Optional when `x`
#'   carries its fitted model, in which case the model's own training frame is
#'   used.
#' @param statistics Which statistics to score. `NULL` (the default) scores
#'   every behavioural column the two frames share. A character vector selects
#'   columns by name. A named list of functions defines derived statistics;
#'   each is called with a data frame and must return one logical or numeric
#'   value per row, and each is applied to **both** frames, so the columns it
#'   uses must exist in both. A shared character or factor column is expanded
#'   into one statistic per level, named `column=level`.
#' @param n_boot Bootstrap replicates for the interval on `ratio`. `0` skips
#'   the bootstrap and returns `NA` bounds.
#' @param seed Optional integer seed, for a reproducible bootstrap.
#'
#' @return A [tibble][tibble::tibble] with one row per statistic, ordered with
#'   the failures first: `statistic`, `observed`, `simulated`, `ratio`,
#'   `ci_lower`, `ci_upper`, `verdict`, `control`, `n_obs`, `n_sim`, and then
#'   `observed_mean`, `simulated_mean`, `location_z`, `overlap`, the four
#'   percentile columns, `reason`, `n_obs_rows`, `n_sim_rows`,
#'   `n_obs_missing`, `n_sim_missing` and `n_sim_reps`. `n_obs` and `n_sim`
#'   count groups -- the sample size of each standard deviation, with `n_sim`
#'   counted per simulated replicate -- while the `_rows` columns count the
#'   (group, card) episodes behind them and the `_missing` columns count the
#'   episodes that statistic could not be computed on, so a rate built on a
#'   partly missing column cannot pass unnoticed.
#'
#' @section What the bootstrap resamples:
#'
#' Whole groups, with replacement, independently on each side, and the ratio
#' is recomputed from the resampled group rates. The group is the unit the
#' dispersion is a statement about and the unit that is exchangeable in both
#' frames; resampling individual (group, card) episodes instead would break
#' the very clustering being measured and would report an interval far too
#' narrow. Where the simulated frame carries several replicates, each is
#' resampled within itself and the resulting SDs averaged, exactly as the
#' point estimate is formed.
#'
#' @seealso [sdt_plot_fidelity()] to draw the result,
#'   [sdt_run_behaviour()] to score the discrete-event engine on the same
#'   statistics.
#' @export
#' @examples
#' set.seed(1)
#' cards <- sprintf("c%02d", 1:6)
#' # Ten observed groups that genuinely differ from one another.
#' obs <- expand.grid(group_id = sprintf("g%02d", 1:10), card = cards,
#'                    stringsAsFactors = FALSE)
#' team <- stats::setNames(stats::runif(10, 0.1, 0.9), sprintf("g%02d", 1:10))
#' obs$surgery <- stats::runif(nrow(obs)) < team[obs$group_id]
#'
#' # A simulator that gets the average right and the spread badly wrong.
#' sim <- expand.grid(sim_group = sprintf("s%02d", 1:10), card = cards,
#'                    stringsAsFactors = FALSE)
#' sim$surgery <- stats::runif(nrow(sim)) < mean(team)
#'
#' sdt_fidelity(sim, obs, n_boot = 50, seed = 1)
sdt_fidelity <- function(x, observed = NULL, statistics = NULL,
                         n_boot = 200L, seed = NULL) {
  n_boot <- .fid_check_boot(n_boot)
  if (!is.null(seed)) withr::local_seed(.check_count(seed))

  ctx <- .fid_context(x, observed)
  ctx <- .fid_match(ctx)
  specs <- .fid_statistics(ctx, statistics)

  rows <- lapply(names(specs), function(nm) {
    .fid_one(ctx, nm, specs[[nm]], n_boot)
  })
  out <- dplyr::bind_rows(lapply(rows, function(r) r$row))
  rates <- dplyr::bind_rows(lapply(rows, function(r) r$rates))

  out <- out[order(out$verdict != "FAIL", out$control, out$statistic), ]
  out <- tibble::as_tibble(out)
  attr(out, "novelty") <- .fid_novelty(ctx, names(specs))
  attr(out, "group_rates") <- rates
  attr(out, "frame") <- ctx$frame
  attr(out, "band") <- .fid_band()
  attr(out, "location_tol") <- .fid_location_tol()
  attr(out, "n_boot") <- n_boot
  out
}

#' Turn a discrete-event game run into a (group, card) behaviour frame
#'
#' Reshapes the event log of a [sdt_simulate()] run into the same one-row-per
#' (group, card) frame that [sdt_fidelity()] scores, so the discrete-event
#' engine can be held to exactly the same statistics as a fitted behaviour
#' model. Each replication is one group and each casualty `id` is one card,
#' which is what makes the comparison legitimate: groups within a real course
#' session play the same fixed deck, and so do replications.
#'
#' @section What each column means, and what it cannot mean:
#'
#' * `red`, `yellow`, `green`, `ETC`, `DCS`, `TASC` and `ventilated` are read
#'   off the cohort. The engine *routes on* them, it does not choose them, so
#'   they are identical in every replication and their between-group SD is
#'   exactly zero. That is not a bug in this function; it is the engine's
#'   structure made visible, and [sdt_fidelity()] will report it as an
#'   infinite dispersion ratio.
#' * `surgery`, `diagnostics`, `icu`, `ward`, `recovery`, `green_area` record
#'   whether the casualty ever entered that board area. These *can* vary
#'   between replications, through queueing and the horizon.
#' * `died` is `end_area == "morgue"`, and `not_seen` is a casualty the game
#'   ended before. The engine has no branch to the morgue, so `died` is
#'   always `FALSE`.
#'
#' The observed counterparts, for a course export, are `therapy == "TASC"`
#' and so on for the therapy concepts, `called_triage == "red"` for the
#' sighting categories, `therapy != "none"` for `surgery`, and the reported
#' end location for `icu`. There is deliberately no `eFAST` or `CT` column:
#' the engine has a single radiology area and no modality, so inventing one
#' would be inventing a comparison.
#'
#' @param run A `game_run` from [sdt_simulate()].
#' @param group Name for the group column in the result.
#' @param card Name for the card column in the result.
#'
#' @return A [tibble][tibble::tibble] with one row per (replication,
#'   casualty), the two key columns named by `group` and `card`, the cohort's
#'   `triage` and `therapy`, the casualty's `end_area`, and one logical column
#'   per behaviour described above.
#' @seealso [sdt_fidelity()], [summary.game_run()].
#' @export
#' @examples
#' co <- sdt_synthetic_cohort(n = 12, seed = 1)
#' run <- sdt_simulate(sdt_scenario(co), replications = 3, seed = 1,
#'                     verbose = FALSE)
#' sdt_run_behaviour(run)
sdt_run_behaviour <- function(run, group = "group_id", card = "card") {
  .check_class(run, "game_run")
  .check_string(group)
  .check_string(card)

  cohort <- run$scenario$cohort
  if (is.null(cohort) || !nrow(cohort)) {
    cli::cli_abort(
      c("{.arg run} carries no cohort, so there are no cards to score.",
        "i" = "A {.cls game_run} built by hand needs its {.field scenario}
               filled in before it can be compared with observed groups."),
      class = "sandtablR_error_fidelity"
    )
  }
  cohort <- tibble::as_tibble(cohort)
  reps <- seq_len(run$meta$replications %||% 1L)

  out <- tibble::tibble(
    .group = rep(sprintf("rep%03d", reps), each = nrow(cohort)),
    .card = rep(as.character(cohort$id), times = length(reps)),
    triage = rep(as.character(cohort$triage), times = length(reps)),
    therapy = rep(as.character(cohort$therapy), times = length(reps)),
    ventilated = rep(as.logical(cohort$ventilated), times = length(reps))
  )
  for (lvl in sdt_triage_levels()) out[[lvl]] <- out$triage == lvl
  for (lvl in setdiff(sdt_therapy_levels(), "none")) {
    out[[lvl]] <- out$therapy == lvl
  }

  key <- paste(out$.group, out$.card, sep = "\r")
  visited <- .fid_visited(run)
  areas <- c(surgery = "or", diagnostics = "diagnostics", icu = "icu_imc",
             ward = "ward", recovery = "recovery", green_area = "green_area")
  for (nm in names(areas)) {
    out[[nm]] <- key %in% visited[[areas[[nm]]]]
  }

  ends <- .end_states(run)
  ends$key <- paste(sprintf("rep%03d", ends$replication),
                    as.character(ends$id), sep = "\r")
  out$end_area <- ends$area[match(key, ends$key)]
  out$end_area[is.na(out$end_area)] <- "not_seen"
  out$died <- out$end_area == "morgue"
  out$not_seen <- out$end_area == "not_seen"

  names(out)[names(out) == ".group"] <- group
  names(out)[names(out) == ".card"] <- card
  out
}

# Which (group, card) keys ever entered each board area.
.fid_visited <- function(run) {
  ev <- run$events
  areas <- sdt_board_areas()
  if (!nrow(ev)) {
    return(stats::setNames(rep(list(character()), length(areas)), areas))
  }
  keys <- paste(sprintf("rep%03d", ev$replication), as.character(ev$id),
                sep = "\r")
  split(keys, factor(ev$area, levels = areas))
}

#' Draw a fidelity table
#'
#' A dot-and-range chart of the dispersion ratios: one row per statistic,
#' the ratio on the x axis, the passing band shaded, and the bootstrap
#' interval drawn as a range. Controls carry their own marker because they
#' are read backwards from the rest -- see the "Negative controls" section of
#' [sdt_fidelity()].
#'
#' A ratio of `Inf`, which is what a simulator that produces no between-group
#' spread at all earns, is drawn as an arrow pinned to the right-hand edge and
#' labelled, rather than being dropped from the plot or silently rescaling it.
#'
#' @param x A fidelity table from [sdt_fidelity()].
#' @return A \pkg{ggplot2} object.
#' @seealso [sdt_fidelity()].
#' @export
#' @examples
#' set.seed(1)
#' cards <- sprintf("c%02d", 1:6)
#' obs <- expand.grid(group_id = sprintf("g%02d", 1:10), card = cards,
#'                    stringsAsFactors = FALSE)
#' team <- stats::setNames(stats::runif(10, 0.1, 0.9), sprintf("g%02d", 1:10))
#' obs$surgery <- stats::runif(nrow(obs)) < team[obs$group_id]
#' sim <- expand.grid(sim_group = sprintf("s%02d", 1:10), card = cards,
#'                    stringsAsFactors = FALSE)
#' sim$surgery <- stats::runif(nrow(sim)) < mean(team)
#' sdt_plot_fidelity(sdt_fidelity(sim, obs, n_boot = 50, seed = 1))
sdt_plot_fidelity <- function(x) {
  needed <- c("statistic", "ratio", "ci_lower", "ci_upper", "verdict",
              "control")
  missing <- setdiff(needed, names(x))
  if (length(missing)) {
    n_missing <- length(missing)
    cli::cli_abort(c(
      "{.arg x} must be a fidelity table from {.fn sdt_fidelity}.",
      "x" = "Missing {n_missing} column{?s}: {.field {missing}}."
    ), class = "sandtablR_error_fidelity")
  }
  if (!nrow(x)) {
    return(.sdt_blank_plot("No statistics to score"))
  }

  band <- attr(x, "band") %||% .fid_band()
  df <- .fid_plot_data(tibble::as_tibble(x), band)
  lim <- attr(df, "limits")

  ggplot2::ggplot(df, ggplot2::aes(y = .data$label)) +
    ggplot2::annotate(
      "rect", xmin = band[[1]], xmax = band[[2]],
      ymin = -Inf, ymax = Inf, fill = "#27AE60", alpha = 0.13
    ) +
    ggplot2::geom_vline(xintercept = 1, colour = "#6C5B85",
                        linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_linerange(
      ggplot2::aes(xmin = .data$lo, xmax = .data$hi, colour = .data$verdict),
      linewidth = 0.9, na.rm = TRUE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = .data$xpos, colour = .data$verdict,
                   shape = .data$marker),
      size = 3.1, stroke = 0.9, fill = "white", na.rm = TRUE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = lim[["label"]], label = .data$tag),
      hjust = 0, size = 2.9, colour = "#3D1C5C", na.rm = TRUE
    ) +
    ggplot2::scale_colour_manual(
      values = c(PASS = "#27AE60", FAIL = "#C0392B"), name = NULL
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        "behaviour" = 21, "control" = 22,
        "no simulated spread" = 24, "control, no simulated spread" = 23
      ),
      name = NULL
    ) +
    ggplot2::scale_x_continuous(
      limits = c(lim[["min"]], lim[["max"]]),
      breaks = .fid_plot_breaks(lim)
    ) +
    ggplot2::labs(
      title = "Simulation fidelity",
      subtitle = .fid_plot_subtitle(x, band),
      x = "Observed between-group SD / simulated between-group SD",
      y = NULL
    ) +
    .sdt_theme() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

# The plot's data frame: labels, the drawing position for each ratio, and the
# marker that says whether a row is a control, off the scale, or both. A ratio
# of Inf is pinned just past the last finite value and drawn as an arrowhead,
# so a simulator with no between-group spread at all stays visible instead of
# being dropped or stretching the axis to nothing.
.fid_plot_data <- function(x, band) {
  off <- !is.finite(x$ratio)
  finite <- c(x$ratio[!off], x$ci_lower[is.finite(x$ci_lower)],
              x$ci_upper[is.finite(x$ci_upper)], band)
  finite <- finite[is.finite(finite)]
  lo <- max(0, min(finite) * 0.92)
  hi <- max(finite)
  span <- max(hi - lo, 0.3)
  cap <- hi + 0.06 * span
  span <- cap - lo
  lim <- c(min = lo - 0.02 * span, max = cap + 0.19 * span,
           cap = cap, label = cap + 0.035 * span)

  x$xpos <- ifelse(off, cap, x$ratio)
  x$lo <- pmin(pmax(x$ci_lower, lim[["min"]]), cap)
  x$hi <- pmin(pmax(x$ci_upper, lim[["min"]]), cap)
  x$lo[off] <- NA_real_
  x$hi[off] <- NA_real_
  marker <- ifelse(x$control, "control", "behaviour")
  marker[off] <- ifelse(x$control[off], "control, no simulated spread",
                        "no simulated spread")
  x$marker <- factor(
    marker,
    levels = c("behaviour", "control", "no simulated spread",
               "control, no simulated spread")
  )
  x$tag <- ifelse(
    off, "Inf",
    ifelse(is.na(x$ratio), "n/a", sprintf("%.2f", x$ratio))
  )
  x$label <- ifelse(x$control, paste0(x$statistic, " (c)"), x$statistic)
  x$label <- factor(x$label, levels = rev(x$label))
  x$verdict <- factor(x$verdict, levels = c("PASS", "FAIL"))
  attr(x, "limits") <- lim
  x
}

# One is always labelled; anything that would collide with it is dropped,
# because a tick reading 0.90 1.00 1.15 on top of itself is worse than no
# tick at all. The band is shaded, so its edges need no numbers.
.fid_plot_breaks <- function(lim) {
  span <- lim[["max"]] - lim[["min"]]
  breaks <- pretty(c(lim[["min"]], lim[["cap"]]), n = 5)
  breaks <- breaks[abs(breaks - 1) > 0.05 * span]
  breaks <- sort(c(1, breaks))
  breaks[breaks >= lim[["min"]] & breaks <= lim[["cap"]]]
}

.fid_plot_subtitle <- function(x, band) {
  n_fail <- sum(x$verdict == "FAIL", na.rm = TRUE)
  frame <- attr(x, "frame")
  where <- if (is.null(frame)) {
    ""
  } else {
    sprintf(" on %d cards by %d observed groups",
            frame$n_cards, frame$n_obs_groups)
  }
  ctrl <- if (any(x$control)) " (c) = control." else ""
  sprintf(
    "%d of %d statistic%s outside %.2f-%.2f%s.%s",
    n_fail, nrow(x), if (nrow(x) == 1L) "" else "s",
    band[[1]], band[[2]], where, ctrl
  )
}

# Resolving the inputs --------------------------------------------------------

.fid_check_boot <- function(x, call = rlang::caller_env()) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) && x >= 0 &&
    x == round(x)
  if (!ok) {
    cli::cli_abort(
      "{.arg n_boot} must be a single non-negative whole number.",
      class = "sandtablR_error_fidelity", call = call
    )
  }
  as.integer(x)
}

# Accept a simulated-groups tibble, a bare data frame, or a game_run. The
# game_run branch is what lets the discrete-event engine be scored on exactly
# the same statistics as a fitted model.
.fid_as_frame <- function(x, arg, call) {
  if (inherits(x, "game_run")) {
    return(sdt_run_behaviour(x))
  }
  if (is.data.frame(x)) {
    return(tibble::as_tibble(x))
  }
  cli::cli_abort(
    c("{.arg {arg}} must be a data frame or a {.cls game_run}.",
      "x" = "Got {.cls {class(x)[[1]]}} instead."),
    class = "sandtablR_error_fidelity", call = call
  )
}

# Pull a named term out of whatever shape the fitted model recorded them in,
# so this file does not have to assume one.
.fid_term <- function(model, what) {
  terms <- model$terms
  if (is.null(terms)) {
    return(NULL)
  }
  if (is.list(terms) && !is.null(terms[[what]])) {
    return(as.character(terms[[what]])[[1]])
  }
  if (is.character(terms) && what %in% names(terms)) {
    return(unname(terms[[what]]))
  }
  NULL
}

.fid_pick <- function(df, preferred, candidates, what, arg, call) {
  if (!is.null(preferred) && preferred %in% names(df)) {
    return(preferred)
  }
  hit <- intersect(candidates, names(df))
  if (length(hit)) {
    return(hit[[1]])
  }
  cli::cli_abort(
    c("Cannot find the {what} column in {.arg {arg}}.",
      "i" = "Expected one of {.field {candidates}}.",
      "x" = "Got {.field {names(df)}}."),
    class = "sandtablR_error_fidelity", call = call
  )
}

.fid_context <- function(x, observed, call = rlang::caller_env()) {
  sim <- .fid_as_frame(x, "x", call)
  model <- attr(x, "model") %||% attr(x, "behaviour_model")
  obs <- observed %||% attr(x, "observed") %||% model$data
  if (is.null(obs)) {
    cli::cli_abort(
      c("Cannot find observed data to score {.arg x} against.",
        "i" = "Pass it as {.arg observed}.",
        "i" = paste("An {.cls sdt_simulated_groups} object carries its",
                    "model, and so needs no {.arg observed}.")),
      class = "sandtablR_error_fidelity", call = call
    )
  }
  obs <- .fid_as_frame(obs, "observed", call)

  sim_group <- .fid_pick(sim, NULL, .fid_group_names(), "group", "x", call)
  obs_group <- .fid_pick(obs, .fid_term(model, "group"), .fid_group_names(),
                         "group", "observed", call)
  sim_card <- .fid_pick(sim, .fid_term(model, "card"), .fid_card_names(),
                        "card", "x", call)
  obs_card <- .fid_pick(obs, .fid_term(model, "card"), .fid_card_names(),
                        "card", "observed", call)

  rep_col <- intersect(.fid_rep_names(), names(sim))
  rep_col <- if (length(rep_col)) rep_col[[1]] else NULL

  sim[[sim_card]] <- as.character(sim[[sim_card]])
  obs[[obs_card]] <- as.character(obs[[obs_card]])
  # One internal key per simulated group, so that a `sim_group` label reused
  # across replicates is still counted as a distinct group.
  sim$.fid_group <- if (is.null(rep_col)) {
    as.character(sim[[sim_group]])
  } else {
    paste(sim[[rep_col]], sim[[sim_group]], sep = "\r")
  }
  sim$.fid_rep <- if (is.null(rep_col)) {
    "1"
  } else {
    as.character(sim[[rep_col]])
  }
  obs$.fid_group <- as.character(obs[[obs_group]])
  obs$.fid_rep <- "1"

  controls <- attr(x, "controls") %||% model$meta$controls %||%
    .fid_default_controls()
  list(
    sim = sim, obs = obs, sim_card = sim_card, obs_card = obs_card,
    sim_group = sim_group, obs_group = obs_group, rep_col = rep_col,
    controls = as.character(controls),
    outcomes = attr(x, "outcomes") %||% model$outcomes, call = call
  )
}

# The composition-matched frame -----------------------------------------------

.fid_match <- function(ctx) {
  cards <- intersect(unique(ctx$obs[[ctx$obs_card]]),
                     unique(ctx$sim[[ctx$sim_card]]))
  if (length(cards) < 2L) {
    n_cards <- length(cards)
    cli::cli_abort(
      c("The two frames share {n_cards} card{?s}.",
        "i" = "Between-group dispersion needs at least 2 shared cards.",
        "x" = "Check that {.arg x} and {.arg observed} use the same card
               labels."),
      class = "sandtablR_error_fidelity", call = ctx$call
    )
  }
  obs0 <- ctx$obs
  sim0 <- ctx$sim
  obs <- .fid_complete(obs0[obs0[[ctx$obs_card]] %in% cards, , drop = FALSE],
                       ctx$obs_card, cards)
  sim <- .fid_complete(sim0[sim0[[ctx$sim_card]] %in% cards, , drop = FALSE],
                       ctx$sim_card, cards)
  n_obs <- dplyr::n_distinct(obs$.fid_group)
  n_sim <- dplyr::n_distinct(sim$.fid_group)
  if (n_obs < 2L || n_sim < 2L) {
    cli::cli_abort(
      c("Too few complete groups to measure dispersion.",
        "x" = "{n_obs} observed and {n_sim} simulated group{?s} saw all
               {length(cards)} shared cards.",
        "i" = "Both sides need at least 2."),
      class = "sandtablR_error_fidelity", call = ctx$call
    )
  }
  obs$.fid_card <- as.character(obs[[ctx$obs_card]])
  sim$.fid_card <- as.character(sim[[ctx$sim_card]])
  ctx$obs <- obs
  ctx$sim <- sim
  ctx$frame <- list(
    cards = sort(cards),
    n_cards = length(cards),
    n_obs_groups = n_obs,
    n_sim_groups = n_sim,
    n_sim_reps = dplyr::n_distinct(sim$.fid_rep),
    dropped_obs_groups = dplyr::n_distinct(obs0$.fid_group) - n_obs,
    dropped_sim_groups = dplyr::n_distinct(sim0$.fid_group) - n_sim,
    dropped_obs_cards =
      length(setdiff(unique(obs0[[ctx$obs_card]]), cards)),
    dropped_sim_cards = length(setdiff(unique(sim0[[ctx$sim_card]]), cards))
  )
  ctx
}

# Keep only the groups that saw every shared card. A group scored on a subset
# of the deck carries composition variance that is indistinguishable from
# behavioural variance, which is the failure mode this whole file exists to
# avoid.
.fid_complete <- function(df, card, cards) {
  seen <- tapply(df[[card]], df$.fid_group, function(z) {
    length(unique(z))
  })
  keep <- names(seen)[!is.na(seen) & seen == length(cards)]
  df[df$.fid_group %in% keep, , drop = FALSE]
}

# Choosing the statistics -----------------------------------------------------

.fid_scorable <- function(v) is.logical(v) || is.numeric(v)
.fid_categorical <- function(v) is.character(v) || is.factor(v)

.fid_statistics <- function(ctx, statistics) {
  if (is.list(statistics) && !is.data.frame(statistics)) {
    return(.fid_function_specs(statistics, ctx$call))
  }
  shared <- setdiff(intersect(names(ctx$sim), names(ctx$obs)),
                    c(.fid_key_names(), ".fid_group", ".fid_rep",
                      ".fid_card"))
  if (!is.null(statistics)) {
    if (!is.character(statistics)) {
      cli::cli_abort(
        "{.arg statistics} must be a character vector or a named list.",
        class = "sandtablR_error_fidelity", call = ctx$call
      )
    }
    unknown <- setdiff(statistics, shared)
    if (length(unknown)) {
      n_unknown <- length(unknown)
      cli::cli_abort(
        c("{n_unknown} requested statistic{?s} not in both frames.",
          "x" = "Missing: {.field {unknown}}.",
          "i" = "Shared columns: {.field {shared}}."),
        class = "sandtablR_error_fidelity", call = ctx$call
      )
    }
    shared <- statistics
  } else if (!is.null(ctx$outcomes)) {
    named <- intersect(as.character(ctx$outcomes), shared)
    if (length(named)) shared <- named
  }
  specs <- .fid_column_specs(shared, ctx)
  if (!length(specs)) {
    cli::cli_abort(
      c("No statistic can be scored.",
        "i" = "{.arg x} and {.arg observed} share no logical, numeric or
               categorical behaviour column."),
      class = "sandtablR_error_fidelity", call = ctx$call
    )
  }
  specs
}

.fid_function_specs <- function(statistics, call) {
  nms <- names(statistics)
  ok <- !is.null(nms) && all(nzchar(nms)) &&
    all(vapply(statistics, is.function, logical(1)))
  if (!ok) {
    cli::cli_abort(
      "{.arg statistics} must be a fully named list of functions.",
      class = "sandtablR_error_fidelity", call = call
    )
  }
  statistics
}

# Turn shared columns into extractors. A logical or numeric column is itself a
# statistic; a categorical column becomes one statistic per level, because
# "which triage category" is several yes/no questions, not one number.
.fid_column_specs <- function(shared, ctx) {
  specs <- list()
  for (nm in shared) {
    v_obs <- ctx$obs[[nm]]
    v_sim <- ctx$sim[[nm]]
    if (.fid_scorable(v_obs) && .fid_scorable(v_sim)) {
      specs[[nm]] <- .fid_column_getter(nm)
    } else if (.fid_categorical(v_obs) && .fid_categorical(v_sim)) {
      levels <- sort(unique(as.character(v_obs[!is.na(v_obs)])))
      if (length(levels) > 12L) {
        n_lev <- length(levels)
        cli::cli_warn(c(
          "!" = "Skipping {.field {nm}}: {n_lev} levels is too many to
                 score as separate statistics.",
          "i" = "Select the ones you want with {.arg statistics}."
        ))
        next
      }
      for (lvl in levels) {
        specs[[paste0(nm, "=", lvl)]] <- .fid_level_getter(nm, lvl)
      }
    }
  }
  specs
}

.fid_column_getter <- function(nm) {
  force(nm)
  function(df) df[[nm]]
}

.fid_level_getter <- function(nm, lvl) {
  force(nm)
  force(lvl)
  function(df) as.character(df[[nm]]) == lvl
}

# Scoring one statistic -------------------------------------------------------

.fid_values <- function(df, getter, nm, call) {
  v <- getter(df)
  if (length(v) != nrow(df)) {
    n_got <- length(v)
    cli::cli_abort(
      c("Statistic {.field {nm}} returned {n_got} value{?s}.",
        "i" = "It must return one value per row, and the frame has
               {nrow(df)}."),
      class = "sandtablR_error_fidelity", call = call
    )
  }
  if (is.logical(v)) {
    return(as.numeric(v))
  }
  if (is.numeric(v)) {
    return(as.numeric(v))
  }
  cli::cli_abort(
    c("Statistic {.field {nm}} must be logical or numeric.",
      "x" = "Got {.cls {class(v)[[1]]}}."),
    class = "sandtablR_error_fidelity", call = call
  )
}

# The per-group rates, split by replicate. Everything downstream -- the point
# estimate, the bootstrap and the overlap -- is computed from these, so the
# raw frame is walked exactly once per statistic per side.
.fid_rates <- function(df, getter, nm, call, cards = NULL) {
  v <- .fid_values(df, getter, nm, call)
  keep <- !is.na(v)
  n_missing <- sum(!keep)

  # Composition matching has to hold for the VALUE, not only for the card.
  # A group that saw every shared card but has no recorded value on three of
  # them is scored over the cells it has, while the simulated side is scored
  # over all of them -- and the two denominators then differ. That inflates
  # the dispersion ratio: on this corpus `red` is missing in 105 of 3410
  # cells and reads 1.204 unmatched against 0.968 matched. So a group is
  # dropped here unless its non-missing cells cover every shared card.
  n_incomplete <- 0L
  if (!is.null(cards) && !is.null(df$.fid_card)) {
    covered <- tapply(
      df$.fid_card[keep], list(df$.fid_group[keep], df$.fid_rep[keep]),
      function(z) length(unique(z))
    )
    ok <- !is.na(covered) & covered == length(cards)
    good <- expand.grid(
      g = rownames(covered), r = colnames(covered),
      stringsAsFactors = FALSE
    )[as.vector(ok), , drop = FALSE]
    unit <- paste(df$.fid_group, df$.fid_rep, sep = "\r")
    keep <- keep & unit %in% paste(good$g, good$r, sep = "\r")
    n_incomplete <- sum(!ok, na.rm = TRUE)
  }

  v <- v[keep]
  g <- df$.fid_group[keep]
  r <- df$.fid_rep[keep]
  by_rep <- split(seq_along(v), r)
  rates <- lapply(by_rep, function(i) {
    means <- tapply(v[i], g[i], mean)
    stats::setNames(as.numeric(means), names(means))
  })
  list(rates = rates, n_rows = length(v), n_missing = n_missing,
       n_incomplete = n_incomplete)
}

# Are the "simulated" groups actually new, or a replay of the training teams?
#
# Every dispersion statistic here is satisfied perfectly by handing back the
# observed data, so on its own the table cannot tell a generative simulator
# from a copy -- and copying is the exact failure mode the feature guards
# against (a `new_teams` that forgets to redraw the random effect). This
# compares each simulated group's full outcome vector against the observed
# ones and reports the share that are exact duplicates.
.fid_novelty <- function(ctx, stats, warn = TRUE) {
  cols <- intersect(stats, intersect(names(ctx$sim), names(ctx$obs)))
  if (!length(cols)) {
    return(NULL)
  }
  sig <- function(df, card) {
    ord <- order(df$.fid_group, df$.fid_rep, df[[card]])
    d <- df[ord, , drop = FALSE]
    key <- paste(d$.fid_group, d$.fid_rep, sep = "\r")
    vals <- do.call(paste, c(lapply(cols, function(k) as.character(d[[k]])),
                             list(sep = "|")))
    vapply(split(vals, key), paste, character(1), collapse = ";")
  }
  s_sig <- sig(ctx$sim, ctx$sim_card)
  o_sig <- sig(ctx$obs, ctx$obs_card)
  dup <- sum(s_sig %in% o_sig)
  share <- if (length(s_sig)) dup / length(s_sig) else NA_real_
  if (warn && !is.na(share) && share > 0.5) {
    pct <- round(100 * share)
    cli::cli_warn(
      c("!" = "{pct}% of the simulated groups are exact copies of observed
               ones.",
        "i" = "Dispersion is reproduced trivially when the simulator replays
               its training data.",
        "i" = "Check that new teams are being drawn rather than reused.")
    )
  }
  list(n_sim_groups = length(s_sig), n_duplicated = dup, share = share,
       statistics = cols)
}

# Average the within-replicate SDs rather than pooling every simulated group
# into one vector. Each replicate redraws the session-level effects too, so a
# pooled SD would fold session variation into what is meant to be a
# between-group number and would flatter the simulator.
.fid_sd <- function(rates) {
  vals <- .fid_sd_replicates(rates)
  if (all(is.na(vals))) NA_real_ else mean(vals, na.rm = TRUE)
}

# The per-replicate SDs themselves. The mean of these is the point estimate
# above; their SPREAD is the simulator's own sampling distribution for the
# quantity, and discarding it was throwing away the only free measure of how
# precise that point estimate is. A ratio can sit outside the pass band while
# the observed SD sits comfortably inside the replicates that produced it -
# that is a statement about power, not about the simulator.
.fid_sd_replicates <- function(rates) {
  vapply(rates, function(v) {
    if (length(v) > 1L) stats::sd(v) else NA_real_
  }, numeric(1))
}

# Where the observed SD falls in the simulator's own replicate distribution,
# as a two-sided empirical p-value. Small p means the observed spread is
# genuinely unlike anything the simulator produces; a large p means the run
# cannot distinguish them, whatever the ratio says.
.fid_sd_pvalue <- function(obs_sd, rates) {
  vals <- .fid_sd_replicates(rates)
  vals <- vals[!is.na(vals)]
  if (is.na(obs_sd) || length(vals) < 2L) {
    return(NA_real_)
  }
  below <- sum(vals < obs_sd)
  above <- sum(vals > obs_sd)
  min(1, 2 * min(below, above) / length(vals))
}

# A second interval on the ratio, and a different question from the bootstrap.
#
# `.fid_boot_ci()` resamples GROUPS on each side and holds the simulator's draw
# fixed, so it answers "how much would the ratio move if these teams had been a
# different sample of teams?". At a 2% event rate that is not the question that
# matters. The dominant uncertainty is that a fresh RUN of the simulator gives
# a materially different simulated SD -- the replicate SDs for `xray` on this
# corpus span more than a factor of two -- and the bootstrap cannot see it,
# because it never redraws the cohort.
#
# This interval does. Each replicate is one complete simulated cohort, so
# `observed / SD(replicate r)` is the ratio that run would have reported, and
# the 2.5-97.5 percentiles of those are the range of answers the simulator
# itself produces. It is the interval form of `sd_p`: where that p-value is
# large, this interval covers 1.
#
# Neither replaces the other. The bootstrap is about the sample of teams; this
# is about the run. For a common behaviour they nearly coincide; for a rare one
# they do not, and the difference is the finding.
.fid_replicate_ci <- function(obs_sd, rates, level = 0.95) {
  vals <- .fid_sd_replicates(rates)
  vals <- vals[!is.na(vals) & vals > 0]
  if (is.na(obs_sd) || length(vals) < 10L) {
    return(c(NA_real_, NA_real_))
  }
  a <- (1 - level) / 2
  unname(stats::quantile(obs_sd / vals, c(a, 1 - a), names = FALSE))
}

# The binomial floor a between-group SD cannot go below, given each group's
# rate and how many cards it was scored on, and what is left once that floor
# is netted out. `11-gold-benchmark.R` already computes this; a ratio that
# ignores it compares two numbers that are mostly the same floor.
.fid_sd_floor <- function(rates, n_cards) {
  vals <- vapply(rates, function(v) {
    p <- mean(v, na.rm = TRUE)
    if (!is.finite(p) || !is.finite(n_cards) || n_cards < 1) {
      return(NA_real_)
    }
    sqrt(p * (1 - p) / n_cards)
  }, numeric(1))
  mean(vals, na.rm = TRUE)
}

.fid_sd_between <- function(sd_total, sd_floor) {
  if (is.na(sd_total) || is.na(sd_floor)) {
    return(NA_real_)
  }
  sqrt(max(0, sd_total^2 - sd_floor^2))
}

.fid_mean <- function(rates) {
  vals <- vapply(rates, mean, numeric(1))
  mean(vals, na.rm = TRUE)
}

.fid_resample <- function(rates) {
  lapply(rates, function(v) v[sample.int(length(v), replace = TRUE)])
}

.fid_ratio <- function(obs_sd, sim_sd) {
  if (is.na(obs_sd) || is.na(sim_sd)) {
    return(NA_real_)
  }
  if (sim_sd == 0) {
    return(if (obs_sd == 0) NA_real_ else Inf)
  }
  obs_sd / sim_sd
}

# The overlapping coefficient of the two distributions of group rates: bin
# both on a common grid and add up the smaller share in each bin. It answers
# the question a ratio cannot -- do the simulated teams land where the real
# ones do -- without needing a density estimate.
.fid_overlap <- function(obs, sim) {
  if (!length(obs) || !length(sim)) {
    return(NA_real_)
  }
  rng <- range(c(obs, sim), na.rm = TRUE)
  if (!all(is.finite(rng))) {
    return(NA_real_)
  }
  if (diff(rng) == 0) {
    return(1)
  }
  breaks <- seq(rng[[1]], rng[[2]], length.out = 21L)
  share <- function(x) {
    bin <- findInterval(x, breaks, rightmost.closed = TRUE, all.inside = TRUE)
    tabulate(bin, nbins = length(breaks) - 1L) / length(x)
  }
  sum(pmin(share(obs), share(sim)))
}

.fid_one <- function(ctx, nm, getter, n_boot) {
  cards <- ctx$frame$cards
  obs <- .fid_rates(ctx$obs, getter, nm, ctx$call, cards)
  sim <- .fid_rates(ctx$sim, getter, nm, ctx$call, cards)
  obs_flat <- unlist(obs$rates, use.names = FALSE)
  sim_flat <- unlist(sim$rates, use.names = FALSE)

  obs_sd <- .fid_sd(obs$rates)
  sim_sd <- .fid_sd(sim$rates)
  ratio <- .fid_ratio(obs_sd, sim_sd)
  obs_mean <- .fid_mean(obs$rates)
  sim_mean <- .fid_mean(sim$rates)
  location_z <- if (is.na(obs_sd) || obs_sd == 0) {
    NA_real_
  } else {
    (sim_mean - obs_mean) / obs_sd
  }

  ci <- .fid_boot_ci(obs$rates, sim$rates, n_boot)
  band <- .fid_band()

  # The diagnosis that sits behind the ratio. A rare behaviour's between-group
  # SD is mostly the binomial floor, so two SDs can differ by a lot while the
  # part a simulator is actually responsible for barely differs at all; and a
  # ratio can fall outside the band while the observed SD sits inside the
  # simulator's own replicate spread. Both are reported so a FAIL can be read
  # as "the simulator is wrong" or "this run cannot tell", which are different
  # findings.
  # Both sides are scored on the same matched card set, so one count serves
  # both floors -- that is the whole point of the composition-matched frame.
  n_cards <- ctx$frame$n_cards
  obs_floor <- .fid_sd_floor(obs$rates, n_cards)
  sim_floor <- .fid_sd_floor(sim$rates, n_cards)
  obs_between <- .fid_sd_between(obs_sd, obs_floor)
  sim_between <- .fid_sd_between(sim_sd, sim_floor)
  sd_p <- .fid_sd_pvalue(obs_sd, sim$rates)
  rep_ci <- .fid_replicate_ci(obs_sd, sim$rates)
  disp_ok <- !is.na(ratio) && is.finite(ratio) &&
    ratio >= band[[1]] && ratio <= band[[2]]
  loc_ok <- !is.na(location_z) &&
    abs(location_z) <= .fid_location_tol()
  reason <- c(if (!disp_ok) "dispersion", if (!loc_ok) "location")
  if (is.na(ratio)) reason <- "not evaluable"
  row <- tibble::tibble(
    statistic = nm,
    observed = obs_sd,
    simulated = sim_sd,
    ratio = ratio,
    ci_lower = ci[[1]],
    ci_upper = ci[[2]],
    verdict = if (disp_ok && loc_ok) "PASS" else "FAIL",
    control = tolower(nm) %in% tolower(ctx$controls),
    n_obs = length(obs_flat),
    n_sim = as.integer(round(length(sim_flat) /
                               max(1L, length(sim$rates)))),
    observed_mean = obs_mean,
    simulated_mean = sim_mean,
    location_z = location_z,
    overlap = .fid_overlap(obs_flat, sim_flat),
    observed_p10 = .fid_q(obs_flat, 0.1),
    observed_p90 = .fid_q(obs_flat, 0.9),
    simulated_p10 = .fid_q(sim_flat, 0.1),
    simulated_p90 = .fid_q(sim_flat, 0.9),
    # X0/X2 diagnostics. `sd_p` places the observed SD in the simulator's own
    # replicate distribution; `*_floor` and `*_between` net out the binomial
    # floor; `ci_overlaps_band` says whether the interval the row already
    # prints is even consistent with the verdict drawn from the point estimate.
    sd_p = sd_p,
    rep_lower = rep_ci[[1]],
    rep_upper = rep_ci[[2]],
    rep_covers_one = !is.na(rep_ci[[1]]) && !is.na(rep_ci[[2]]) &&
      rep_ci[[1]] <= 1 && rep_ci[[2]] >= 1,
    observed_floor = obs_floor,
    simulated_floor = sim_floor,
    observed_between = obs_between,
    simulated_between = sim_between,
    ratio_between = .fid_ratio(obs_between, sim_between),
    ci_overlaps_band = !is.na(ci[[1]]) && !is.na(ci[[2]]) &&
      ci[[1]] <= band[[2]] && ci[[2]] >= band[[1]],
    reason = paste(reason, collapse = ", "),
    n_obs_rows = obs$n_rows,
    n_sim_rows = sim$n_rows,
    n_obs_missing = obs$n_missing,
    n_sim_missing = sim$n_missing,
    n_sim_reps = length(sim$rates)
  )
  list(row = row, rates = .fid_rate_rows(nm, obs, sim))
}

.fid_q <- function(x, p) {
  if (!length(x)) NA_real_ else unname(stats::quantile(x, p, names = FALSE))
}

# The per-group distribution, kept alongside the summary so a reader can see
# the overlap rather than take the ratio on trust.
.fid_rate_rows <- function(nm, obs, sim) {
  one <- function(part, src) {
    dplyr::bind_rows(lapply(names(part$rates), function(r) {
      v <- part$rates[[r]]
      tibble::tibble(
        statistic = nm, source = src, replicate = r,
        group = sub("^[^\r]*\r", "", names(v)), rate = unname(v)
      )
    }))
  }
  dplyr::bind_rows(one(obs, "observed"), one(sim, "simulated"))
}

.fid_boot_ci <- function(obs_rates, sim_rates, n_boot) {
  if (n_boot < 1L) {
    return(c(NA_real_, NA_real_))
  }
  draws <- vapply(seq_len(n_boot), function(i) {
    .fid_ratio(.fid_sd(.fid_resample(obs_rates)),
               .fid_sd(.fid_resample(sim_rates)))
  }, numeric(1))
  # An all-infinite set of draws is not a missing interval: it is the honest
  # statement that the simulator produced no between-group spread in any
  # resample. Only genuinely undefined draws become NA.
  if (all(is.na(draws))) {
    return(c(NA_real_, NA_real_))
  }
  unname(stats::quantile(draws, c(0.025, 0.975), na.rm = TRUE, names = FALSE))
}
