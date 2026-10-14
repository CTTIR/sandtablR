# Triage error model, simulator and fidelity scorer.
#
# Every fixture here is synthetic and generated in-file. The module is meant
# for a corpus with a gold-standard answer key, and no such corpus ships with
# the package, so the tests fix the CONTRACT -- what the object carries, what
# it refuses, what it carries as NA rather than guessing -- and check
# recovery only where the fixture was built to make recovery possible.

skip_if_no_lme4 <- function() {
  testthat::skip_if_not_installed("lme4")
}

# A deck with a known truth per card, and teams that differ from one another
# by a real amount on the logit scale. `p_hit` is the population hit rate per
# true category and `sd_team` the between-team SD of it; `p_split` is the
# chance that an error goes to the more severe of the two wrong answers and
# `sd_split` the between-team SD of that. Both parts therefore carry a team
# effect, which is what the fit is supposed to find and the simulator to
# reproduce.
te_fixture <- function(n_groups = 24L, per_category = 5L,
                       p_hit = c(red = 0.9, yellow = 0.7, green = 0.6),
                       sd_team = 1, p_split = 0.65, sd_split = 0.8,
                       seed = 1L, n_blocks = 2L) {
  withr::local_seed(seed)
  levels <- sdt_triage_levels()
  cards <- sprintf("c%02d", seq_len(per_category * length(levels)))
  truth <- rep(levels, each = per_category)
  groups <- sprintf("g%02d", seq_len(n_groups))
  d <- expand.grid(card = cards, group_id = groups,
                   stringsAsFactors = FALSE)
  d$session_id <- sprintf("s%d", 1L + (match(d$group_id, groups) - 1L) %%
                            n_blocks)
  d$triage <- truth[match(d$card, cards)]
  b <- stats::setNames(stats::rnorm(n_groups, sd = sd_team), groups)
  bs <- stats::setNames(stats::rnorm(n_groups, sd = sd_split), groups)
  # A per-card offset so the card term has something to find.
  cardb <- stats::setNames(stats::rnorm(length(cards), sd = 0.5), cards)
  eta <- stats::qlogis(unname(p_hit[d$triage])) + b[d$group_id] +
    cardb[d$card]
  hit <- stats::runif(nrow(d)) < stats::plogis(eta)
  severe <- stats::runif(nrow(d)) <
    stats::plogis(stats::qlogis(p_split) + bs[d$group_id])
  wrong <- vapply(seq_len(nrow(d)), function(i) {
    .te_wrong(d$triage[[i]], levels)[[if (severe[[i]]) 1L else 2L]]
  }, character(1))
  d$called_triage <- ifelse(hit, d$triage, wrong)
  tibble::as_tibble(d)
}

# Most tests are not about the warnings, and a fixture with no team spread in
# a part legitimately produces one. The tests that ARE about a warning call
# the functions directly.
te_fit <- function(...) {
  suppressWarnings(sdt_fit_triage_error(te_fixture(...), verbose = FALSE))
}

# A deck on which every team behaves identically: team `i` errs on card `j`
# exactly when `(i + j) %% 5 == 0`, so every team's hit rate is the same and
# the between-team variance is zero by construction rather than by luck.
te_flat_fixture <- function(n_groups = 30L, per_category = 8L) {
  levels <- sdt_triage_levels()
  cards <- sprintf("c%02d", seq_len(per_category * length(levels)))
  truth <- rep(levels, each = per_category)
  groups <- sprintf("g%02d", seq_len(n_groups))
  d <- expand.grid(card = cards, group_id = groups,
                   stringsAsFactors = FALSE)
  d$session_id <- ifelse(match(d$group_id, groups) <= n_groups / 2,
                         "s1", "s2")
  d$triage <- truth[match(d$card, cards)]
  i <- match(d$group_id, groups)
  j <- match(d$card, cards)
  err <- (i + j) %% 5L == 0L
  severe <- (i + 2L * j) %% 2L == 0L
  wrong <- vapply(seq_len(nrow(d)), function(k) {
    .te_wrong(d$triage[[k]], levels)[[if (severe[[k]]) 1L else 2L]]
  }, character(1))
  d$called_triage <- ifelse(err, wrong, d$triage)
  tibble::as_tibble(d)
}


# Vocabulary ------------------------------------------------------------------

test_that("the wrong answers are ordered by severity", {
  expect_identical(.te_wrong("red"), c("yellow", "green"))
  expect_identical(.te_wrong("yellow"), c("red", "green"))
  expect_identical(.te_wrong("green"), c("red", "yellow"))
})

test_that("the structurally impossible statistics are the extremes", {
  expect_true(.te_structural("red", "over"))
  expect_true(.te_structural("green", "under"))
  expect_false(.te_structural("red", "under"))
  expect_false(.te_structural("green", "over"))
  expect_false(.te_structural("yellow", "over"))
  expect_false(.te_structural("yellow", "under"))
  expect_false(.te_structural("overall", "over"))
})

test_that("the card effect specification accepts every documented form", {
  expect_identical(.te_resolve_card_effect("none", NULL),
                   c(hit = "none", split = "none"))
  expect_identical(.te_resolve_card_effect(c("fixed", "random"), NULL),
                   c(hit = "fixed", split = "random"))
  expect_identical(
    .te_resolve_card_effect(c(split = "random"), NULL),
    c(hit = "fixed", split = "random")
  )
  expect_error(.te_resolve_card_effect("quadratic", NULL),
               class = "sandtablR_error_triage")
  expect_error(.te_resolve_card_effect(c("fixed", "none", "none"), NULL),
               class = "sandtablR_error_triage")
  expect_error(.te_resolve_card_effect(c(part = "none"), NULL),
               class = "sandtablR_error_triage")
})


# Fitting ---------------------------------------------------------------------

test_that("a well-behaved deck fits every cell", {
  skip_if_no_lme4()
  fit <- te_fit()
  expect_s3_class(fit, "sdt_triage_error_model")
  expect_identical(nrow(fit$cells), 6L)
  expect_setequal(
    .te_cell_key(fit$cells$category, fit$cells$part),
    c("red.hit", "red.split", "yellow.hit", "yellow.split",
      "green.hit", "green.split")
  )
  expect_identical(nrow(fit$skipped), 0L)
  expect_identical(names(fit$fits), names(fit$fits_null))
  expect_null(fit$joint)
})

test_that("the fitted object carries what the simulator needs", {
  skip_if_no_lme4()
  fit <- te_fit()
  expect_identical(fit$terms$levels, sdt_triage_levels())
  expect_identical(length(fit$terms$card_levels), 15L)
  expect_setequal(unname(fit$terms$card_truth), sdt_triage_levels())
  expect_identical(names(fit$terms$card_truth), fit$terms$card_levels)
  expect_identical(fit$terms$wrong$red, c("yellow", "green"))
  expect_true(fit$meta$n_rows > 0L)
  expect_identical(dim(fit$meta$confusion), c(3L, 3L))
  expect_identical(sum(fit$meta$confusion), fit$meta$n_rows)
})

test_that("the hit part carries a card term and the split part does not", {
  skip_if_no_lme4()
  fit <- te_fit()
  hit <- fit$cells[fit$cells$part == "hit", ]
  split <- fit$cells[fit$cells$part == "split", ]
  expect_true(all(hit$card_effect == "fixed"))
  expect_true(all(split$card_effect == "none"))
})

test_that("every cell is fitted a second time without the team term", {
  skip_if_no_lme4()
  fit <- te_fit(sd_team = 1.4, n_groups = 30L)
  expect_true(all(is.finite(fit$cells$aic_null)))
  expect_true(all(is.finite(fit$cells$delta_aic)))
  # The fixture puts a real team effect on the hit part, so keeping the term
  # has to pay there. The split part is generated without one, so it is left
  # alone.
  hit <- fit$cells[fit$cells$part == "hit", ]
  expect_true(all(hit$delta_aic < 0))
  expect_true(all(hit$p_value < 0.05))
  expect_true(all(hit$sd_group > 0.5))
})

test_that("teams that do not differ produce a singular cell, kept and
           flagged", {
  skip_if_no_lme4()
  expect_warning(
    fit <- sdt_fit_triage_error(te_flat_fixture(), verbose = FALSE),
    class = "sandtablR_warning_triage"
  )
  expect_true(any(fit$cells$singular))
  # Singular cells are kept, not skipped, and the flag is what says so.
  expect_true(all(fit$cells$sd_group[fit$cells$singular] < 1e-6))
  expect_true(all(!is.na(fit$cells$sd_group)))
  expect_true(all(.te_cell_key(fit$cells$category, fit$cells$part) %in%
                    names(fit$fits)))
})

test_that("a simulator built from a flat fit makes identical teams", {
  skip_if_no_lme4()
  fit <- suppressWarnings(
    sdt_fit_triage_error(te_flat_fixture(), verbose = FALSE)
  )
  sim <- suppressWarnings(
    sdt_simulate_triage(fit, n_groups = 12L, seed = 13L)
  )
  flat <- fit$cells[fit$cells$singular, ]
  key <- .te_cell_key(flat$category, flat$part)
  b <- attr(sim, "draws")
  b <- b[.te_cell_key(b$category, b$part) %in% key, ]
  # "Singular" is a fitted SD below 1e-6, not an exact zero, so the drawn
  # effects are that small rather than absent.
  expect_true(all(abs(b$b_group) < 1e-5))
})

test_that("a card term is only attempted where it could be estimated", {
  frame <- function(n_cards, n_minority, n_rows) {
    d <- data.frame(
      .card = factor(rep(sprintf("c%02d", seq_len(n_cards)),
                         length.out = n_rows)),
      .y = c(rep(1L, n_minority), rep(0L, n_rows - n_minority))
    )
    d
  }
  # More card levels than minority events: a coefficient per card would be an
  # interpolation of the cell rather than an estimate of anything.
  expect_false(.te_card_feasible(frame(15L, 10L, 200L), "fixed"))
  expect_false(.te_card_feasible(frame(15L, 10L, 200L), "random"))
  expect_true(.te_card_feasible(frame(5L, 40L, 200L), "fixed"))
  # "none" needs nothing, and a single card level cannot carry a term.
  expect_true(.te_card_feasible(frame(15L, 10L, 200L), "none"))
  expect_false(.te_card_feasible(frame(1L, 40L, 200L), "fixed"))
})

test_that("an infeasible card term demotes down the ladder and says why", {
  skip_if_no_lme4()
  withr::local_seed(21L)
  # 12 cards per category and a 0.97 hit rate: the split part sees a handful
  # of errors spread over many cards, so the fixed and random rungs are both
  # refused before the optimiser is asked.
  d <- te_fixture(n_groups = 40L, per_category = 12L, sd_team = 0.6,
                  p_hit = c(red = 0.97, yellow = 0.97, green = 0.97),
                  seed = 21L)
  fit <- suppressWarnings(
    sdt_fit_triage_error(d, card_effect = "fixed", verbose = FALSE)
  )
  split <- fit$cells[fit$cells$part == "split", ]
  expect_true(nrow(split) > 0L)
  expect_true(all(split$card_effect != "fixed"))
  expect_true(all(nzchar(split$demotions)))
  expect_true(all(grepl("^fixed:", split$demotions)))
  # Both rejection routes are exercised: a rung refused before the optimiser
  # was asked, and a rung whose fit came back at the boundary.
  expect_true(any(grepl("too few minority events", split$demotions)))
  expect_true(any(grepl("above the boundary", split$demotions)))
  # The hit part has plenty of data and keeps the rung it was asked for.
  hit <- fit$cells[fit$cells$part == "hit", ]
  expect_true(all(hit$card_effect == "fixed"))
  expect_true(all(!nzchar(hit$demotions)))
})

test_that("a constant cell is skipped with its reason, never dropped", {
  skip_if_no_lme4()
  d <- te_fixture(seed = 5L)
  # Force every red error to be yellow, so the red split is constant.
  red_wrong <- d$triage == "red" & d$called_triage != "red"
  d$called_triage[red_wrong] <- "yellow"
  fit <- sdt_fit_triage_error(d, verbose = FALSE)
  expect_true("red" %in% fit$skipped$category)
  row <- fit$skipped[fit$skipped$category == "red", ]
  expect_identical(row$part, "split")
  expect_match(row$reason, "constant")
  expect_false("red.split" %in% names(fit$fits))
})

test_that("fitting refuses a frame it cannot model", {
  skip_if_no_lme4()
  d <- te_fixture()
  expect_error(sdt_fit_triage_error(d[0, ], verbose = FALSE),
               class = "sandtablR_error_triage")
  expect_error(sdt_fit_triage_error(as.list(d), verbose = FALSE),
               class = "sandtablR_error_triage")
  expect_error(
    sdt_fit_triage_error(d, truth = "nope", verbose = FALSE),
    class = "sandtablR_error_triage"
  )
  bad <- d
  bad$triage[[1L]] <- "purple"
  expect_error(sdt_fit_triage_error(bad, verbose = FALSE),
               class = "sandtablR_error_triage")
  gone <- d
  gone$card[[1L]] <- NA_character_
  expect_error(sdt_fit_triage_error(gone, verbose = FALSE),
               class = "sandtablR_error_triage")
})

test_that("a card that carries two true categories is refused by name", {
  skip_if_no_lme4()
  d <- te_fixture()
  d$triage[d$card == "c01" & d$group_id == "g01"] <- "green"
  expect_error(sdt_fit_triage_error(d, verbose = FALSE),
               class = "sandtablR_error_triage")
})

test_that("rows with no truth or no call are dropped and counted", {
  skip_if_no_lme4()
  d <- te_fixture()
  d$called_triage[seq_len(20)] <- NA_character_
  fit <- suppressWarnings(sdt_fit_triage_error(d, verbose = FALSE))
  expect_identical(fit$meta$n_dropped_label, 20L)
  expect_identical(fit$meta$n_rows, nrow(d) - 20L)
})

test_that("the matched frame is available and narrows the deck", {
  skip_if_no_lme4()
  d <- te_fixture(n_groups = 20L)
  # Truncate half the teams so the deck is unbalanced.
  drop <- d$group_id %in% sprintf("g%02d", 1:10) & d$card > "c10"
  fit <- suppressWarnings(
    sdt_fit_triage_error(d[!drop, ], frame = "matched", verbose = FALSE)
  )
  expect_identical(fit$meta$frame$type, "matched")
  expect_true(fit$meta$n_cards <= 15L)
  expect_true(fit$meta$n_rows < sum(!drop))
})

test_that("the printed model reports every cell and every skip", {
  skip_if_no_lme4()
  fit <- te_fit()
  txt <- format.sdt_triage_error_model(fit)
  expect_true(any(grepl("sdt_triage_error_model", txt)))
  expect_true(all(vapply(
    c("red.hit", "red.split", "green.hit"),
    function(k) any(grepl(k, txt, fixed = TRUE)), logical(1)
  )))
  expect_true(any(grepl("sd_group", txt)))
  expect_output(out <- print.sdt_triage_error_model(fit), "sd_group")
  expect_identical(out, fit)
})

test_that("a fit with no session term drops it and still prints", {
  skip_if_no_lme4()
  d <- te_fixture(n_groups = 30L)
  fit <- suppressWarnings(
    sdt_fit_triage_error(d, block = NULL, verbose = FALSE)
  )
  expect_false(fit$terms$has_block)
  expect_true(all(is.na(fit$cells$sd_block)))
  txt <- format.sdt_triage_error_model(fit)
  expect_true(any(grepl("sd_block", txt)))
  sim <- suppressWarnings(sdt_simulate_triage(fit, n_groups = 6L, seed = 1L))
  expect_false(anyNA(sim$called_triage))
  expect_true(all(attr(sim, "draws")$b_block == 0))
})

test_that("one session is warned about and the term is dropped", {
  skip_if_no_lme4()
  d <- te_fixture(n_groups = 30L, n_blocks = 1L)
  seen <- list()
  fit <- withCallingHandlers(
    sdt_fit_triage_error(d, verbose = FALSE),
    warning = function(cond) {
      seen[[length(seen) + 1L]] <<- cond
      invokeRestart("muffleWarning")
    }
  )
  hit <- vapply(seen, function(cond) {
    inherits(cond, "sandtablR_warning_triage") &&
      grepl("Only 1 block", conditionMessage(cond))
  }, logical(1))
  expect_true(any(hit))
  expect_false(fit$terms$has_block)
})

test_that("the glmmTMB engine reaches the same object", {
  skip_if_not_installed("glmmTMB")
  fit <- suppressWarnings(
    sdt_fit_triage_error(te_fixture(n_groups = 30L), engine = "glmmTMB",
                         verbose = FALSE)
  )
  expect_identical(fit$meta$engine, "glmmTMB")
  expect_true(nrow(fit$cells) >= 3L)
  expect_true(all(is.finite(fit$cells$sd_group)))
  sim <- suppressWarnings(sdt_simulate_triage(fit, n_groups = 8L, seed = 1L))
  expect_false(anyNA(sim$called_triage))
})

test_that("the joint multinomial is fitted and reported when asked", {
  skip_if_no_lme4()
  fit <- suppressWarnings(
    sdt_fit_triage_error(te_fixture(n_groups = 20L, per_category = 4L),
                         joint = TRUE, verbose = FALSE)
  )
  expect_s3_class(fit$joint, "tbl_df")
  expect_identical(nrow(fit$joint), 3L)
  expect_setequal(fit$joint$category, sdt_triage_levels())
  expect_true(is.logical(fit$joint$converged))
  expect_true(all(is.na(fit$joint$reason) | !fit$joint$converged))
  txt <- format.sdt_triage_error_model(fit)
  expect_true(any(grepl("joint multinomial", txt)))
})


# Simulating ------------------------------------------------------------------

test_that("a simulation has one row per (team, card) and a call on each", {
  skip_if_no_lme4()
  fit <- te_fit()
  sim <- sdt_simulate_triage(fit, n_groups = 10L, seed = 1L)
  expect_s3_class(sim, "sdt_simulated_triage")
  expect_identical(nrow(sim), 10L * length(fit$terms$card_levels))
  expect_identical(dplyr::n_distinct(sim$sim_group), 10L)
  expect_setequal(unique(sim$card), fit$terms$card_levels)
  expect_false(anyNA(sim$called_triage))
  expect_true(all(sim$called_triage %in% sdt_triage_levels()))
  expect_identical(nrow(attr(sim, "gaps")), 0L)
})

test_that("the truth on a simulated row is the card's own truth", {
  skip_if_no_lme4()
  fit <- te_fit()
  sim <- sdt_simulate_triage(fit, n_groups = 6L, seed = 2L)
  expect_identical(sim$triage,
                   unname(fit$terms$card_truth[sim$card]))
})

test_that("hit, over and under follow the severity order and are exclusive", {
  skip_if_no_lme4()
  fit <- te_fit()
  sim <- sdt_simulate_triage(fit, n_groups = 8L, seed = 3L)
  expect_identical(sim$hit, sim$called_triage == sim$triage)
  expect_true(all(sim$hit + sim$over + sim$under == 1L))
  rank <- .triage_rank()
  expect_identical(sim$over,
                   unname(rank[sim$called_triage] < rank[sim$triage]))
  expect_null(names(sim$over))
  # Nothing can be over-triaged above red, or under-triaged below green.
  expect_false(any(sim$over[sim$triage == "red"]))
  expect_false(any(sim$under[sim$triage == "green"]))
})

test_that("a seed makes a simulation reproducible and a new seed does not", {
  skip_if_no_lme4()
  fit <- te_fit()
  a <- sdt_simulate_triage(fit, n_groups = 8L, seed = 4L)
  b <- sdt_simulate_triage(fit, n_groups = 8L, seed = 4L)
  c3 <- sdt_simulate_triage(fit, n_groups = 8L, seed = 5L)
  expect_identical(a$called_triage, b$called_triage)
  expect_false(identical(a$called_triage, c3$called_triage))
})

test_that("replicates stack and never share a team label", {
  skip_if_no_lme4()
  fit <- te_fit()
  sim <- sdt_simulate_triage(fit, n_groups = 5L, nsim = 3L, seed = 6L)
  expect_identical(sort(unique(sim$.replicate)), 1:3)
  expect_identical(dplyr::n_distinct(sim$sim_group), 15L)
  per_rep <- tapply(sim$sim_group, sim$.replicate, dplyr::n_distinct)
  expect_true(all(per_rep == 5L))
})

test_that("cards can be restricted, and an unknown card is refused", {
  skip_if_no_lme4()
  fit <- te_fit()
  keep <- fit$terms$card_levels[seq_len(4L)]
  sim <- sdt_simulate_triage(fit, n_groups = 4L, cards = keep, seed = 7L)
  expect_setequal(unique(sim$card), keep)
  expect_error(
    sdt_simulate_triage(fit, cards = c(keep, "nope"), seed = 1L),
    class = "sandtablR_error_triage"
  )
})

test_that("new_teams = FALSE replays the fitted teams and records them", {
  skip_if_no_lme4()
  fit <- te_fit()
  sim <- sdt_simulate_triage(fit, n_groups = 6L, seed = 8L,
                             new_teams = FALSE)
  draws <- attr(sim, "draws")
  expect_false(anyNA(draws$source_group))
  expect_true(all(draws$source_group %in% fit$meta$frame$groups))
  # The fitted conditional modes are looked up, not redrawn, so two runs with
  # different seeds use the same team effects.
  again <- sdt_simulate_triage(fit, n_groups = 6L, seed = 99L,
                               new_teams = FALSE)
  expect_equal(attr(again, "draws")$b_group, draws$b_group)
})

test_that("asking for more fitted teams than exist warns and shrinks", {
  skip_if_no_lme4()
  fit <- te_fit(n_groups = 8L)
  seen <- list()
  sim <- withCallingHandlers(
    sdt_simulate_triage(fit, n_groups = 50L, new_teams = FALSE, seed = 1L),
    warning = function(cond) {
      seen[[length(seen) + 1L]] <<- cond
      invokeRestart("muffleWarning")
    }
  )
  hit <- vapply(seen, function(cond) {
    inherits(cond, "sandtablR_warning_triage") &&
      grepl("reuses fitted groups", conditionMessage(cond))
  }, logical(1))
  expect_true(any(hit))
  expect_identical(dplyr::n_distinct(sim$sim_group), 8L)
})

test_that("team_effects = FALSE draws no team effect at all", {
  skip_if_no_lme4()
  fit <- te_fit(n_groups = 30L, sd_team = 1.2, seed = 9L)
  sim <- suppressWarnings(
    sdt_simulate_triage(fit, n_groups = 30L, nsim = 6L, seed = 9L,
                        team_effects = FALSE)
  )
  expect_true(all(attr(sim, "draws")$b_group == 0))
  expect_false(isTRUE(attr(sim, "team_effects")))
  with_team <- suppressWarnings(
    sdt_simulate_triage(fit, n_groups = 30L, nsim = 6L, seed = 9L)
  )
  # The spread is averaged within replicate, the way the fidelity scorer
  # forms it: one replicate of thirty teams is far too noisy to compare.
  spread <- function(x) {
    mean(vapply(split(x, x$.replicate), function(p) {
      stats::sd(tapply(p$hit, p$sim_group, mean))
    }, numeric(1)))
  }
  level <- function(x) mean(x$hit)
  expect_lt(spread(sim), spread(with_team))
  # The level survives; it is only the spread that goes.
  expect_lt(abs(level(sim) - level(with_team)), 0.05)
})

test_that("a cell that was never fitted is carried as NA, not guessed", {
  skip_if_no_lme4()
  d <- te_fixture(seed = 5L)
  red_wrong <- d$triage == "red" & d$called_triage != "red"
  d$called_triage[red_wrong] <- "yellow"
  fit <- sdt_fit_triage_error(d, verbose = FALSE)
  expect_warning(
    sim <- sdt_simulate_triage(fit, n_groups = 20L, seed = 10L),
    class = "sandtablR_warning_triage"
  )
  gaps <- attr(sim, "gaps")
  expect_identical(gaps$category, "red")
  expect_true(gaps$n_rows > 0L)
  # The correct red calls stand; only the errors have nothing to say.
  red <- sim[sim$triage == "red", ]
  expect_true(any(red$called_triage == "red", na.rm = TRUE))
  expect_true(all(is.na(red$called_triage[is.na(red$hit)])))
})

test_that("the simulation is refused anything that is not its own model", {
  skip_if_no_lme4()
  fit <- te_fit()
  expect_error(sdt_simulate_triage(list()), class = "rlang_error")
  expect_error(sdt_simulate_triage(fit, n_groups = 0L),
               class = "rlang_error")
  expect_error(sdt_simulate_triage(fit, nsim = -1L), class = "rlang_error")
})

test_that("a simulated frame scores through sdt_triage_accuracy unchanged", {
  skip_if_no_lme4()
  fit <- te_fit()
  sim <- sdt_simulate_triage(fit, n_groups = 12L, seed = 11L)
  acc <- sdt_triage_accuracy(sim)
  overall <- acc[acc$category == "overall", ]
  expect_equal(overall$hit_rate, mean(sim$hit), tolerance = 1e-12)
  expect_equal(overall$over_rate, mean(sim$over), tolerance = 1e-12)
})

test_that("the printed simulation names the teams and the gaps", {
  skip_if_no_lme4()
  fit <- te_fit()
  sim <- sdt_simulate_triage(fit, n_groups = 4L, seed = 12L)
  txt <- format.sdt_simulated_triage(sim)
  expect_true(any(grepl("sdt_simulated_triage", txt)))
  expect_true(any(grepl("new teams", txt)))
  expect_true(any(grepl("hit", txt)))
})


# Fidelity --------------------------------------------------------------------

te_scored <- function(n_groups = 24L, sd_team = 1.2, nsim = 8L, seed = 1L,
                      n_boot = 0L, ...) {
  d <- te_fixture(n_groups = n_groups, sd_team = sd_team, seed = seed, ...)
  fit <- suppressWarnings(sdt_fit_triage_error(d, verbose = FALSE))
  sim <- suppressWarnings(
    sdt_simulate_triage(fit, n_groups = n_groups, nsim = nsim,
                        seed = seed + 100L)
  )
  list(d = d, fit = fit, sim = sim,
       fid = suppressWarnings(
         sdt_error_fidelity(sim, n_boot = n_boot, seed = 3L)
       ))
}

test_that("the table scores a level and an SD for every statistic", {
  skip_if_no_lme4()
  out <- te_scored()
  f <- out$fid
  expect_s3_class(f, "sdt_error_fidelity")
  expect_setequal(unique(f$measure), c("level", "sd"))
  expect_setequal(unique(f$quantity), c("hit", "over", "under"))
  expect_setequal(unique(f$category),
                  c(sdt_triage_levels(), "overall"))
  # 4 categories x 3 quantities x 2 measures, less the two structural pairs.
  expect_identical(nrow(f), 20L)
  each <- table(f$statistic)
  expect_true(all(each == 1L))
})

test_that("the structurally impossible statistics are excluded, not failed", {
  skip_if_no_lme4()
  f <- te_scored()$fid
  expect_setequal(attr(f, "structural"), c("red over", "green under"))
  expect_false(any(grepl("^red over", f$statistic)))
  expect_false(any(grepl("^green under", f$statistic)))
})

test_that("the ratio is observed over simulated, and the band is shared", {
  skip_if_no_lme4()
  f <- te_scored()$fid
  expect_equal(f$ratio, f$observed / f$simulated, tolerance = 1e-12)
  expect_identical(attr(f, "band"), .fid_band())
  expect_identical(attr(f, "location_tol"), .fid_location_tol())
  band <- attr(f, "band")
  ok <- f$ratio >= band[[1L]] & f$ratio <= band[[2L]]
  expect_identical(f$verdict, ifelse(ok, "PASS", "FAIL"))
})

test_that("the table carries everything sdt_plot_fidelity() reads", {
  skip_if_no_lme4()
  f <- te_scored()$fid
  needed <- c("statistic", "ratio", "ci_lower", "ci_upper", "verdict",
              "control")
  expect_true(all(needed %in% names(f)))
  expect_type(f$control, "logical")
  expect_false(is.null(attr(f, "band")))
})

test_that("a simulator with a team term recovers the observed spread", {
  skip_if_no_lme4()
  f <- te_scored(n_groups = 40L, sd_team = 1.2, nsim = 12L, seed = 2L)$fid
  sds <- f[f$measure == "sd" & f$quantity == "hit", ]
  # Generous: the fixture has 40 teams and 5 cards per category, so a
  # between-team SD is itself estimated with real uncertainty.
  expect_true(all(sds$ratio > 0.6 & sds$ratio < 1.6))
  levels <- f[f$measure == "level" & f$quantity == "hit", ]
  expect_true(all(abs(levels$ratio - 1) < 0.15))
})

test_that("dropping the team term leaves the level alone and kills the
           spread", {
  skip_if_no_lme4()
  out <- te_scored(n_groups = 40L, sd_team = 1.5, nsim = 12L, seed = 4L)
  flat <- sdt_simulate_triage(out$fit, n_groups = 40L, nsim = 12L,
                              seed = 104L, team_effects = FALSE)
  f0 <- sdt_error_fidelity(flat, n_boot = 0L)
  key <- function(f, m) {
    x <- f[f$measure == m & f$quantity == "hit", ]
    stats::setNames(x$ratio, x$category)
  }
  # The level is reproduced either way; only the spread separates them.
  expect_true(all(abs(key(out$fid, "level") - 1) < 0.15))
  expect_true(all(abs(key(f0, "level") - 1) < 0.15))
  expect_gt(mean(key(f0, "sd")), mean(key(out$fid, "sd")))
  expect_gt(key(f0, "sd")[["overall"]], 1.15)
})

test_that("the bootstrap is optional, reproducible and brackets the point", {
  skip_if_no_lme4()
  out <- te_scored(n_boot = 0L)
  expect_true(all(is.na(out$fid$ci_lower)))
  a <- sdt_error_fidelity(out$sim, n_boot = 60L, seed = 5L)
  b <- sdt_error_fidelity(out$sim, n_boot = 60L, seed = 5L)
  expect_equal(a$ci_lower, b$ci_lower)
  expect_true(all(a$ci_lower <= a$ci_upper, na.rm = TRUE))
  expect_error(sdt_error_fidelity(out$sim, n_boot = -1L),
               class = "sandtablR_error_fidelity")
})

test_that("the matched frame is chosen from the observed side and reported", {
  skip_if_no_lme4()
  d <- te_fixture(n_groups = 24L, seed = 6L)
  # Truncate a third of the teams, as a clock would.
  short <- d$group_id %in% sprintf("g%02d", 1:8) & d$card > "c12"
  fit <- suppressWarnings(
    sdt_fit_triage_error(d[!short, ], verbose = FALSE)
  )
  sim <- suppressWarnings(
    sdt_simulate_triage(fit, n_groups = 24L, seed = 7L)
  )
  f <- suppressWarnings(
    sdt_error_fidelity(sim, observed = d[!short, ], n_boot = 0L)
  )
  fr <- attr(f, "frame")
  expect_identical(fr$n_shared_cards, 15L)
  expect_true(fr$n_cards <= 15L)
  expect_true(fr$n_obs_groups >= 2L)
  expect_identical(sum(fr$cards_per_category), fr$n_cards)
  # Naming the cards by hand overrides the choice.
  pick <- fit$terms$card_levels[seq_len(12L)]
  f2 <- suppressWarnings(
    sdt_error_fidelity(sim, observed = d[!short, ], cards = pick,
                       n_boot = 0L)
  )
  expect_identical(attr(f2, "frame")$n_cards, 12L)
  expect_error(
    sdt_error_fidelity(sim, observed = d[!short, ], cards = "nope"),
    class = "sandtablR_error_triage"
  )
})

test_that("a thin category is warned about rather than scored in silence", {
  skip_if_no_lme4()
  out <- te_scored(per_category = 2L, n_groups = 20L, seed = 8L)
  expect_warning(
    f <- sdt_error_fidelity(out$sim, n_boot = 0L),
    class = "sandtablR_warning_triage"
  )
  expect_true(all(attr(f, "frame")$cards_per_category == 2L))
})

test_that("two frames that disagree about the truth are refused", {
  skip_if_no_lme4()
  out <- te_scored()
  bad <- out$d
  bad$triage[bad$card == "c01"] <- "green"
  expect_error(sdt_error_fidelity(out$sim, observed = bad),
               class = "sandtablR_error_triage")
})

test_that("fidelity refuses inputs it cannot line up", {
  skip_if_no_lme4()
  out <- te_scored()
  expect_error(sdt_error_fidelity(list()),
               class = "sandtablR_error_triage")
  bare <- tibble::as_tibble(out$sim)
  attr(bare, "model") <- NULL
  class(bare) <- class(tibble::tibble())
  expect_error(sdt_error_fidelity(bare), class = "sandtablR_error_triage")
  other <- out$d
  other$card <- paste0("x", other$card)
  expect_error(sdt_error_fidelity(out$sim, observed = other),
               class = "sandtablR_error_triage")
  no_truth <- out$d
  no_truth$triage <- NULL
  expect_error(sdt_error_fidelity(out$sim, observed = no_truth),
               class = "sandtablR_error_triage")
})

test_that("the header names the frame, the failures and the exclusions", {
  skip_if_no_lme4()
  f <- te_scored()$fid
  head <- tbl_sum.sdt_error_fidelity(f)
  expect_true(any(grepl("outside", head)))
  expect_true(any(grepl("Matched frame", names(head))))
  expect_true(any(grepl("zero by construction", head)))
})

test_that("the per-team rates behind the summary are kept", {
  skip_if_no_lme4()
  out <- te_scored()
  rates <- attr(out$fid, "group_rates")
  expect_setequal(unique(rates$source), c("observed", "simulated"))
  expect_setequal(unique(rates$statistic), unique(sub(" \\(.*$", "",
                                                      out$fid$statistic)))
  obs <- rates[rates$source == "observed" &
                 rates$statistic == "overall hit", ]
  row <- out$fid[out$fid$statistic == "overall hit (sd)", ]
  expect_equal(stats::sd(obs$rate), row$observed, tolerance = 1e-12)
})

test_that("sdt_fidelity() structurally cannot score a conditional statistic", {
  skip_if_no_lme4()
  # This is the justification for the function existing, so it is tested
  # rather than asserted. `sdt_fidelity()` drops a group unless its
  # non-missing cells cover every shared card; a statistic conditional on the
  # true category is NA on every card of another category, so no group ever
  # qualifies and there is nothing left to compare.
  out <- te_scored()
  red_hit <- function(df) {
    ifelse(df$triage == "red", df$called_triage == "red", NA)
  }
  got <- suppressWarnings(
    sdt_fidelity(out$sim, out$d, statistics = list(red_hit = red_hit),
                 n_boot = 0L)
  )
  expect_identical(nrow(got), 1L)
  expect_identical(got$n_obs, 0L)
  expect_identical(got$n_obs_rows, 0L)
  expect_identical(got$n_sim_rows, 0L)
  expect_true(is.na(got$ratio))
  expect_identical(got$reason, "not evaluable")
  # The same quantity comes back from the conditional scorer, evaluated.
  f <- out$fid[out$fid$statistic == "red hit (sd)", ]
  expect_identical(nrow(f), 1L)
  expect_true(is.finite(f$observed))
  expect_true(f$n_obs > 0L)
})
