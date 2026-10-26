# The Generate panel, driven headlessly.
#
# The panel wraps three slow, stateful engine calls and one file writer, so
# these drive the whole path -- ingest, fit, simulate, score, download -- on a
# fixture small enough to fit in a unit test. The corpus the feature was built
# against is 4868 rows and 149 groups; nothing here loads it, because a test
# that takes ninety seconds is a test nobody runs.
#
# The failure paths matter more than the happy one. A panel like this is
# trusted, and the three ways it can quietly mislead are a file it read wrong,
# a simulator whose spread is not the real spread, and a control row read as a
# model failure when it is a frame failure. One test each.

skip_if_no_shiny <- function() {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("htmltools")
}

skip_if_no_engine <- function() {
  skip_if_no_shiny()
  skip_if_not_installed("lme4")
}

# A reactiveValues store standing in for the app's shared state. Deliberately
# the four keys the app really declares: the panel adds its own three, and
# that has to keep working against the state the workbench actually builds.
fake_state <- function(...) {
  shiny::reactiveValues(scenario = NULL, run = NULL, models = NULL,
                        bench = NULL, ...)
}

# renderUI() hands back the markup and its dependencies; nothing here cares
# about the dependencies.
gen_html <- function(x) {
  paste(as.character(x), collapse = " ")
}

# Twenty-four teams over eight cards, in two sessions.
#
# `did_efast` carries a real team effect, which is the quantity the model
# exists to reproduce. `died` is drawn from the card alone, which is what
# makes it a negative control: no team term, so a fit finds a zero team
# variance and a simulator still reproduces its binomial spread. `did_ct` is
# constant, so it cannot be fitted at all and has to turn up in the skipped
# table with a reason rather than vanishing.
gen_frame <- function(n_groups = 24L, n_cards = 8L, seed = 1L) {
  withr::with_seed(seed, {
    cards <- sprintf("c%02d", seq_len(n_cards))
    groups <- sprintf("g%02d", seq_len(n_groups))
    grid <- expand.grid(card = cards, group_id = groups,
                        stringsAsFactors = FALSE)
    half <- sprintf("g%02d", n_groups %/% 2L)
    grid$session_id <- ifelse(grid$group_id <= half, "s1", "s2")
    team <- stats::setNames(stats::rnorm(n_groups, sd = 1.1), groups)
    card_effect <- stats::setNames(stats::rnorm(n_cards, sd = 0.6), cards)
    eta <- card_effect[grid$card] + team[grid$group_id]
    grid$did_efast <- stats::runif(nrow(grid)) < stats::plogis(eta)
    p_died <- stats::setNames(rep(c(0.2, 0.55), length.out = n_cards), cards)
    grid$died <- stats::runif(nrow(grid)) < p_died[grid$card]
    grid$did_ct <- FALSE
    tibble::as_tibble(grid)
  })
}

gen_csv <- function(data = gen_frame(), envir = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = envir)
  utils::write.csv(data, path, row.names = FALSE)
  path
}

# What a fileInput hands the server.
gen_upload <- function(path, name = basename(path)) {
  data.frame(name = name, size = file.size(path), type = "text/csv",
             datapath = path, stringsAsFactors = FALSE)
}

# One fit for the whole file. Several tests need a fitted model to render or
# to draw from, and refitting for each would spend seconds proving the same
# thing twice.
gen_cache <- new.env(parent = emptyenv())

gen_model <- function() {
  if (is.null(gen_cache$model)) {
    # The zero team variance on `died` is the point of the control and the
    # engine says so out loud; the panel's own test asserts that the warning
    # reaches the page, so here it is only noise.
    gen_cache$model <- suppressWarnings(sdt_fit_behaviour(
      gen_frame(), outcomes = c("efast", "died"), verbose = FALSE
    ))
  }
  gen_cache$model
}

# A simulator with no team term: every cell drawn from the pooled rate. It
# gets the average right and the spread badly wrong, which is exactly the
# failure the dispersion ratio exists to catch.
gen_flat <- function(obs, frozen = FALSE, seed = 1L) {
  withr::with_seed(seed, {
    out <- tibble::tibble(sim_group = obs$group_id, card = obs$card)
    rate <- function(nm) unname(tapply(obs[[nm]], obs$card, mean)[out$card])
    # The card is modelled and the team is not, which is exactly the shape of
    # the discrete-event engine: every group gets the same probabilities, so
    # the only spread left is the coin toss.
    out$efast <- stats::runif(nrow(out)) < rate("efast")
    out$died <- if (frozen) {
      # Read straight off the card and never redrawn, as an engine with no
      # team layer of any kind does. The control then has no spread at all.
      rate("died") > 0.5
    } else {
      stats::runif(nrow(out)) < rate("died")
    }
    out
  })
}


# UI ---------------------------------------------------------------------

test_that("the UI names both buttons, every control and all four views", {
  skip_if_no_shiny()
  html <- as.character(htmltools::renderTags(mod_generate_ui("gen"))$html)
  expect_match(html, "Fit the behaviour model", fixed = TRUE)
  expect_match(html, "Simulate and score", fixed = TRUE)
  expect_match(html, "Download the bundle", fixed = TRUE)
  for (box in c("Fitting frame", "Outcomes modelled", "Statistics passed")) {
    expect_match(html, box, fixed = TRUE)
  }
  for (tab in c("Data", "Fit", "Simulated groups", "Fidelity")) {
    expect_match(html, tab, fixed = TRUE)
  }
  # Every control the server reads has to exist under the module's own
  # namespace, or the panel quietly runs on its defaults instead.
  for (id in c("fit", "simulate", "download", "source", "upload", "outcomes",
               "group", "card", "block", "frame", "min_coverage", "n_groups",
               "nsim", "seed", "n_boot", "new_teams", "format",
               "pseudonymise")) {
    expect_match(html, sprintf("gen-%s", id), fixed = TRUE)
  }
  # The upload has to accept all three shapes the reader can open.
  expect_match(html, ".csv,.rds,.parquet", fixed = TRUE)
})

test_that("the app UI carries the panel", {
  skip_if_no_shiny()
  html <- as.character(htmltools::renderTags(sdt_app_ui())$html)
  expect_match(html, "Generate", fixed = TRUE)
  expect_match(html, "generate-fit", fixed = TRUE)
})


# Reading a file ---------------------------------------------------------

test_that("the reader opens every shape it offers to accept", {
  frame <- gen_frame(n_groups = 6L, n_cards = 3L)
  csv <- gen_csv(frame)
  back <- .sdt_gen_read(csv, "decisions.csv")
  expect_s3_class(back, "tbl_df")
  expect_identical(nrow(back), nrow(frame))
  expect_true(all(c("group_id", "card", "did_efast") %in% names(back)))

  rds <- withr::local_tempfile(fileext = ".rds")
  saveRDS(frame, rds)
  expect_identical(nrow(.sdt_gen_read(rds, "decisions.rds")), nrow(frame))

  skip_if_not_installed("nanoparquet")
  pq <- withr::local_tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(as.data.frame(frame), pq)
  expect_identical(nrow(.sdt_gen_read(pq, "decisions.parquet")), nrow(frame))
})

test_that("a file the panel cannot use says so rather than failing bare", {
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(a = 1), path)
  expect_error(.sdt_gen_read(path, "notes.rds"),
               "does not hold a table", class = "sandtablR_error_generate")

  empty <- withr::local_tempfile(fileext = ".rds")
  saveRDS(gen_frame()[0, ], empty)
  expect_error(.sdt_gen_read(empty, "empty.rds"), "no rows",
               class = "sandtablR_error_generate")

  expect_error(.sdt_gen_read(path, "notes.docx"), "Cannot read",
               class = "sandtablR_error_generate")
})

test_that("the detector names the keys and only supportable outcomes", {
  det <- .sdt_gen_detect(gen_frame())
  expect_identical(det$group, "group_id")
  expect_identical(det$card, "card")
  expect_identical(det$block, "session_id")
  # efast and ct come from the did_* flags, died from its own column. The
  # flags themselves are not offered again under their own names, or the
  # same outcome would be fitted twice.
  expect_setequal(det$available, c("efast", "ct", "died"))
  expect_false("did_efast" %in% det$available)
  # Two behavioural outcomes and one control: small enough to sit behind a
  # button, and still enough for a fidelity table that can be read.
  expect_identical(det$default, c("efast", "ct", "died"))
  expect_match(names(det$choices)[[1L]], "eFAST", fixed = TRUE)

  # A frame with nothing recognisable in it is detected as such rather than
  # reported as an empty picker.
  bare <- tibble::tibble(a = 1:4, b = letters[1:4])
  expect_length(.sdt_gen_detect(bare)$available, 0L)
  expect_null(.sdt_gen_detect(bare)$group)
})

test_that("a missing column is named beside the columns that were found", {
  msg <- .sdt_gen_missing_column("group", c("team", "case", "did_efast"))
  expect_match(msg, "No group column", fixed = TRUE)
  expect_match(msg, "team, case, did_efast", fixed = TRUE)
  wide <- .sdt_gen_missing_column("card", sprintf("c%02d", seq_len(20)))
  expect_match(wide, "and 8 more", fixed = TRUE)
})


# Preconditions ----------------------------------------------------------

test_that("the panel says what it needs before either button works", {
  skip_if_no_shiny()
  state <- fake_state()
  shiny::testServer(mod_generate_server, args = list(state = state), {
    # The session path with nothing in the session: the panel has to name the
    # tab that fills it rather than presenting an empty picker.
    expect_false(can_fit())
    expect_match(gen_html(output$precondition), "Run the board on")
    expect_match(gen_html(output$data_ui), "Nothing to learn from yet")
    expect_match(gen_html(output$fit_ui), "No fitted model yet")
    expect_match(gen_html(output$sim_ui), "No simulated groups yet")
    expect_match(gen_html(output$fidelity_ui), "No fidelity table yet")
    # Every value box says something in every state; an empty body renders as
    # a titled grey slab and reads as broken rather than as waiting.
    expect_match(gen_html(output$vb_frame), "Nothing fitted yet")
    expect_match(gen_html(output$vb_fitted), "Nothing fitted yet")
    expect_identical(output$vb_pass, "—")
    expect_identical(gen_html(output$alerts), "")

    session$setInputs(source = "file")
    expect_match(gen_html(output$precondition), "Upload a csv, rds or parquet")
    session$setInputs(fit = 1, simulate = 1)
    expect_null(state$behaviour)
    expect_null(state$simulation)
  })
})

test_that("an upload missing the group column names it, and what it found", {
  skip_if_no_shiny()
  bad <- gen_frame()
  names(bad)[names(bad) == "group_id"] <- "who"
  path <- gen_csv(bad)
  shiny::testServer(mod_generate_server, args = list(state = fake_state()), {
    session$setInputs(source = "file", upload = gen_upload(path))
    expect_null(mapping()$group)
    expect_identical(mapping()$card, "card")
    # Named, with the columns that were there, and not thrown.
    msg <- problems()[[1L]]
    expect_match(msg, "No group column was recognised", fixed = TRUE)
    expect_match(msg, "who", fixed = TRUE)
    expect_match(msg, "did_efast", fixed = TRUE)
    expect_false(can_fit())
    session$setInputs(fit = 1)
    expect_null(state$behaviour)
    # The mapping is on the page as well as in the message, with an em dash
    # where the detector found nothing.
    expect_match(gen_html(output$mapping_table), "Group", fixed = TRUE)
    expect_match(gen_html(output$mapping_table), "—", fixed = TRUE)

    # Overriding the column by hand is what the picker is for, and it has to
    # unblock the fit without touching the file.
    session$setInputs(group = "who")
    expect_identical(mapping()$group, "who")
    expect_length(problems(), 0L)
  })
})

test_that("an unreadable upload is reported, not thrown", {
  skip_if_no_shiny()
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(a = 1), path)
  shiny::testServer(mod_generate_server, args = list(state = fake_state()), {
    session$setInputs(source = "file",
                      upload = gen_upload(path, "notes.rds"))
    expect_null(uploaded())
    expect_match(upload_problem(), "does not hold a table")
    expect_match(problems()[[1L]], "does not hold a table")
    expect_match(gen_html(output$ingest), "Not read", fixed = TRUE)
    expect_false(can_fit())
  })
})

test_that("the engine list names only packages that are absent", {
  # The panel's one refusal that has nothing to do with the data is read off
  # this list, so it must name nothing that is installed.
  missing <- .sdt_gen_missing_stack()
  expect_type(missing, "character")
  expect_true(all(missing %in% "lme4"))
  for (pkg in missing) {
    expect_false(requireNamespace(pkg, quietly = TRUE), info = pkg)
  }
  if (!length(missing)) {
    expect_true(requireNamespace("lme4", quietly = TRUE))
  }
})


# Fitting ----------------------------------------------------------------

test_that("fitting writes the model into state and reports the frame", {
  skip_if_no_engine()
  path <- gen_csv()
  state <- fake_state()
  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(source = "file", upload = gen_upload(path))
    expect_match(gen_html(output$precondition), "Ready: 24 groups, 8 cards")
    session$setInputs(fit = 1)

    expect_s3_class(state$behaviour, "sdt_behaviour_model")
    expect_setequal(state$behaviour$outcomes, c("efast", "died"))
    expect_identical(state$behaviour$meta$n_cards, 8L)
    expect_identical(state$behaviour$meta$n_groups, 24L)
    expect_match(gen_html(output$vb_frame), "8 × 24", fixed = TRUE)
    expect_match(gen_html(output$vb_frame), "matched frame", fixed = TRUE)
    # Three outcomes were asked for and one could not be fitted.
    expect_match(gen_html(output$vb_fitted), "2 of 3", fixed = TRUE)

    # The per-outcome table carries the quantity a simulated team is drawn
    # from, and the flag that qualifies it.
    fit_html <- gen_html(output$fit_table)
    expect_match(fit_html, "efast", fixed = TRUE)
    expect_match(fit_html, "Team SD", fixed = TRUE)
    expect_match(fit_html, "zero team variance", fixed = TRUE)
    # An outcome that could not be fitted is visible with its reason.
    skip_html <- gen_html(output$skip_table)
    expect_match(skip_html, "ct", fixed = TRUE)
    expect_match(skip_html, "constant", fixed = TRUE)
    # The engine reports a zero team variance as a warning; it has to survive
    # the press rather than scroll past in a console nobody is watching.
    expect_match(gen_html(output$fit_notes), "zero team variance")
  })
})

test_that("nothing is skipped in silence, and the table says so", {
  # The counterpart of the test above: with nothing to skip the card still
  # says something, rather than leaving a blank where a table would be.
  none <- .sdt_gen_skip_display(NULL)
  expect_identical(nrow(none), 1L)
  expect_match(none[[3L]][[1L]], "Every outcome asked for was modelled")
  some <- .sdt_gen_skip_display(tibble::tibble(
    outcome = c("ct", "icu"), stage = c("fit", "unavailable"),
    reason = c("constant (all FALSE in 192 rows)", "needs general_location")
  ))
  expect_identical(nrow(some), 2L)
  expect_identical(some$Stage, c("at the fit", "before the fit"))
})

test_that("a fit that cannot run leaves the state as it was", {
  skip_if_no_engine()
  path <- gen_csv()
  state <- fake_state()
  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(source = "file", upload = gen_upload(path),
                      min_coverage = 999, fit = 1)
    # The precondition is satisfied here, which is the point: this is the
    # error path where the engine itself aborts, and it has to be caught as
    # tidily as the one the guard turns away.
    expect_length(problems(), 0L)
    expect_null(state$behaviour)
    expect_match(gen_html(output$fit_ui), "No fitted model yet")
    expect_identical(output$vb_pass, "—")
  })
})

test_that("settings out of range are floored, not passed to the engine", {
  # A cleared numericInput arrives as NA and one the browser has not drawn
  # yet arrives as NULL. Both abort inside the engine rather than in the
  # panel, so neither is allowed past.
  expect_identical(.sdt_gen_count(NULL, 20L), 20L)
  expect_identical(.sdt_gen_count(NA, 20L), 20L)
  expect_identical(.sdt_gen_count(1, 20L, min = 2L), 20L)
  expect_identical(.sdt_gen_count(7.0, 20L), 7L)
  expect_identical(.sdt_gen_boot(NULL), 0L)
  expect_identical(.sdt_gen_boot(-5), 0L)
  expect_identical(.sdt_gen_boot(200), 200L)
  # Blank coverage means "choose it from the data", which is NULL to the
  # engine; a half-typed number must not become a coverage of 3.
  expect_null(.sdt_gen_coverage(NA))
  expect_null(.sdt_gen_coverage(NULL))
  expect_null(.sdt_gen_coverage(0))
  expect_identical(.sdt_gen_coverage(140), 140L)
})


# Simulating and scoring -------------------------------------------------

test_that("simulate and score fills the state, the boxes and the tabs", {
  skip_if_no_engine()
  state <- fake_state()
  state$behaviour <- gen_model()
  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(n_groups = 24, nsim = 2, seed = 1, n_boot = 50,
                      simulate = 1)
    sim <- state$simulation
    expect_s3_class(sim, "sdt_simulated_groups")
    expect_identical(dplyr::n_distinct(sim$sim_group), 48L)
    expect_identical(dplyr::n_distinct(sim$card), 8L)
    expect_true(all(c("efast", "died") %in% names(sim)))

    metrics <- state$fidelity
    expect_s3_class(metrics, "tbl_df")
    expect_setequal(metrics$statistic, c("efast", "died"))
    # died is a control, and the table has to say which rows are read
    # backwards from the rest.
    expect_true(metrics$control[metrics$statistic == "died"])
    expect_false(metrics$control[metrics$statistic == "efast"])

    expect_match(gen_html(output$sim_preview), "sim_group", fixed = TRUE)
    expect_match(gen_html(output$sim_ui), "48 simulated groups", fixed = TRUE)
    expect_match(gen_html(output$sim_ui), "new team in a new session")
    expect_match(gen_html(output$metrics_table), "PASS", fixed = TRUE)
    expect_match(gen_html(output$metrics_table), "control", fixed = TRUE)
    expect_match(gen_html(output$fidelity_ui), "0.90 to 1.15", fixed = TRUE)
    expect_match(gen_html(output$fidelity_ui), "50 times", fixed = TRUE)
    # The plot output cannot be read back in testServer; what feeds it can.
    expect_s3_class(sdt_plot_fidelity(metrics), "ggplot")

    # The model reproduces the spread of teams it was fitted on, which is the
    # whole claim the panel makes. Last, because these verdicts rest on one
    # seeded draw of two replicates, and the lme4 optimum that draw starts
    # from moves slightly between R and lme4 builds; R-devel lands on a draw
    # outside the band, so they are not compared there.
    skip_if(
      grepl("devel", R.version$status, ignore.case = TRUE),
      paste("fidelity verdicts rest on one seeded nsim = 2 draw, and the",
            "lme4 optimum differs by R/lme4 build")
    )
    expect_identical(output$vb_pass, "2 of 2")
    expect_true(all(metrics$verdict == "PASS"))
    expect_match(gen_html(output$alerts), "All passed", fixed = TRUE)
  })
})

test_that("what the draw itself reported reaches the page", {
  skip_if_no_engine()
  state <- fake_state()
  state$behaviour <- gen_model()
  shiny::testServer(mod_generate_server, args = list(state = state), {
    # Reusing the fitted teams cannot produce more groups than were fitted,
    # and the engine says so in a warning. A warning nobody sees is a panel
    # that quietly simulated something other than what was asked for.
    session$setInputs(n_groups = 40, nsim = 1, seed = 1, n_boot = 0,
                      new_teams = FALSE, simulate = 1)
    expect_identical(dplyr::n_distinct(state$simulation$sim_group), 24L)
    expect_true(any(grepl("24", score_notes())))
    notes <- gen_html(output$score_notes)
    expect_match(notes, "What the draw reported", fixed = TRUE)
    expect_match(notes, "reuses fitted groups")
    expect_match(gen_html(output$sim_ui), "check on the fit", fixed = TRUE)
  })
})

test_that("pressing simulate before fitting does nothing at all", {
  skip_if_no_shiny()
  state <- fake_state()
  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(simulate = 1)
    expect_null(state$simulation)
    expect_null(state$fidelity)
    expect_match(gen_html(output$sim_ui), "Press Fit the behaviour model")
  })
})

test_that("a new fit clears what was drawn from the old one", {
  skip_if_no_engine()
  path <- gen_csv()
  state <- fake_state()
  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(source = "file", upload = gen_upload(path),
                      n_groups = 8, nsim = 1, seed = 1, n_boot = 0)
    session$setInputs(fit = 1)
    session$setInputs(simulate = 1)
    expect_s3_class(state$simulation, "sdt_simulated_groups")
    # A simulation drawn from a model that no longer exists is worse on the
    # page than no simulation at all.
    session$setInputs(fit = 2)
    expect_null(state$simulation)
    expect_null(state$fidelity)
    expect_identical(output$vb_pass, "—")
  })
})


# The failure paths ------------------------------------------------------

test_that("a flat simulator fails on dispersion, and the panel says why", {
  skip_if_no_engine()
  model <- gen_model()
  flat <- gen_flat(model$data)
  metrics <- sdt_fidelity(flat, model$data, statistics = c("efast", "died"),
                          n_boot = 0L, seed = 1L)
  # No team term at all: the average is right and the spread is not, so the
  # behavioural row fails while the control still passes.
  efast <- metrics[metrics$statistic == "efast", ]
  expect_identical(efast$verdict, "FAIL")
  expect_gt(efast$ratio, 1.15)
  expect_identical(metrics$verdict[metrics$statistic == "died"], "PASS")

  state <- fake_state()
  state$behaviour <- model
  state$simulation <- flat
  state$fidelity <- metrics
  shiny::testServer(mod_generate_server, args = list(state = state), {
    expect_identical(output$vb_pass, "1 of 2")
    alerts <- gen_html(output$alerts)
    expect_match(alerts, "Dispersion off", fixed = TRUE)
    expect_match(alerts, "efast", fixed = TRUE)
    expect_match(alerts, "collapses the spread of teams", fixed = TRUE)
    # A control that passed is not reported as a problem.
    expect_false(grepl("Control failed", alerts, fixed = TRUE))
    expect_match(gen_html(output$metrics_table), "FAIL", fixed = TRUE)
  })
})

test_that("a failing control is called a broken frame, not a broken model", {
  skip_if_no_engine()
  model <- gen_model()
  frozen <- gen_flat(model$data, frozen = TRUE)
  metrics <- sdt_fidelity(frozen, model$data,
                          statistics = c("efast", "died"), n_boot = 0L,
                          seed = 1L)
  control <- metrics[metrics$statistic == "died", ]
  expect_true(control$control)
  expect_identical(control$verdict, "FAIL")
  # No spread at all on the control, which is a real and reportable result.
  expect_false(is.finite(control$ratio))

  state <- fake_state()
  state$behaviour <- model
  state$fidelity <- metrics
  shiny::testServer(mod_generate_server, args = list(state = state), {
    alerts <- gen_html(output$alerts)
    expect_match(alerts, "Control failed", fixed = TRUE)
    expect_match(alerts, "died", fixed = TRUE)
    # The whole point of the banner: a reader must not take this for a model
    # failure, because it is not one.
    expect_match(alerts, "the frame or the pipeline is wrong, not the model",
                 fixed = TRUE)
    expect_match(alerts, "fixed by the patient card", fixed = TRUE)
    expect_identical(output$vb_pass, "0 of 2")
    # Inf survives the table rather than becoming a blank cell.
    expect_match(gen_html(output$metrics_table), "Inf", fixed = TRUE)
  })
})

test_that("a simulator replaying its training data is called out", {
  skip_if_no_engine()
  model <- gen_model()
  replayed <- model$data
  metrics <- suppressWarnings(sdt_fidelity(
    replayed, model$data, statistics = c("efast", "died"), n_boot = 0L,
    seed = 1L
  ))
  novelty <- attr(metrics, "novelty")
  expect_identical(novelty$share, 1)
  # Every ratio is exactly one, so without the banner this reads as a perfect
  # simulator rather than as no simulator at all.
  expect_true(all(metrics$verdict == "PASS"))

  state <- fake_state()
  state$behaviour <- model
  state$fidelity <- metrics
  shiny::testServer(mod_generate_server, args = list(state = state), {
    alerts <- gen_html(output$alerts)
    expect_match(alerts, "Novelty", fixed = TRUE)
    expect_match(alerts, "100% of the simulated groups are exact copies",
                 fixed = TRUE)
    expect_match(alerts, "check that new teams are being drawn")
    expect_identical(output$vb_pass, "2 of 2")
  })
})


# The download -----------------------------------------------------------

test_that("the download is one archive holding the groups and the metrics", {
  skip_if_no_engine()
  state <- fake_state()
  state$behaviour <- gen_model()
  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(n_groups = 6, nsim = 1, seed = 1, n_boot = 0,
                      simulate = 1)
    session$setInputs(format = "csv", pseudonymise = TRUE)

    archive <- output$download
    expect_match(basename(archive), "^sandtablR-simulation-.*\\.zip$")
    expect_true(file.exists(archive))
    members <- basename(utils::unzip(archive, list = TRUE)$Name)
    # One file for the browser, holding the groups, the metrics that judge
    # them, the deck they range over and the caveats.
    for (member in c("simulated-groups.csv", "fidelity-metrics.csv",
                     "deck-cards.csv", "README.md", "manifest.json")) {
      expect_true(member %in% members, info = member)
    }
    # The crosswalk from pseudonym back to identifier never travels inside
    # the bundle; that is the whole point of pseudonymising it.
    expect_false(any(grepl("crosswalk|key\\.csv", members)))

    dir <- withr::local_tempdir()
    utils::unzip(archive, exdir = dir)
    rows <- utils::read.csv(
      list.files(dir, "simulated-groups.csv", recursive = TRUE,
                 full.names = TRUE)[[1L]]
    )
    expect_identical(dplyr::n_distinct(rows$sim_group), 6L)
    expect_true(all(c("efast", "died") %in% names(rows)))
    metrics <- utils::read.csv(
      list.files(dir, "fidelity-metrics.csv", recursive = TRUE,
                 full.names = TRUE)[[1L]]
    )
    expect_setequal(metrics$statistic, c("efast", "died"))
    readme <- readLines(
      list.files(dir, "README.md", recursive = TRUE, full.names = TRUE)[[1L]],
      warn = FALSE
    )
    expect_true(any(grepl("simulat", readme, ignore.case = TRUE)))
  })
})

test_that("the download format follows the picker", {
  skip_if_no_engine()
  state <- fake_state()
  state$behaviour <- gen_model()
  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(n_groups = 6, nsim = 1, seed = 1, n_boot = 0,
                      simulate = 1, format = "rds")
    members <- basename(utils::unzip(output$download, list = TRUE)$Name)
    expect_true("simulated-groups.rds" %in% members)
    expect_false("simulated-groups.csv" %in% members)
  })
})

test_that("only formats this session can write are offered", {
  formats <- .sdt_gen_formats()
  expect_true(all(c("csv", "rds") %in% formats))
  # nanoparquet is in Suggests, and offering a format that aborts inside the
  # download handler is worse than not offering it.
  expect_identical("parquet" %in% formats,
                   requireNamespace("nanoparquet", quietly = TRUE))
  expect_identical(.sdt_gen_format("rds"), "rds")
  expect_identical(.sdt_gen_format(NULL), "csv")
  expect_identical(.sdt_gen_format("sav"), "csv")
})

test_that("the panel says what pseudonymising does, and what it withholds", {
  skip_if_no_shiny()
  shiny::testServer(mod_generate_server, args = list(state = fake_state()), {
    on <- gen_html(output$privacy)
    expect_match(on, "Pseudonymised", fixed = TRUE)
    expect_match(on, "crosswalk from code back to identifier is never")
    expect_match(on, "One archive", fixed = TRUE)
    session$setInputs(pseudonymise = FALSE)
    off <- gen_html(output$privacy)
    expect_match(off, "Identifiers kept", fixed = TRUE)
    expect_match(off, "hospital, the venue, the city")
  })
})


# The session's own groups -----------------------------------------------

test_that("the session's run is offered as a frame of groups", {
  skip_if_no_shiny()
  skip_if_not_installed("simmer")
  cohort <- sdt_synthetic_cohort(n = 12, seed = 1)
  run <- sdt_simulate(sdt_scenario(cohort), replications = 6, seed = 1,
                      verbose = FALSE)
  state <- fake_state()
  state$run <- run
  shiny::testServer(mod_generate_server, args = list(state = state), {
    src <- ingested()
    expect_false(is.null(src))
    expect_identical(dplyr::n_distinct(src$data$group_id), 6L)
    expect_identical(mapping()$group, "group_id")
    expect_identical(mapping()$card, "card")
    expect_true(length(detected()$available) > 0L)
    expect_length(problems(), 0L)
    expect_match(gen_html(output$precondition), "Ready: 6 groups")
    expect_match(gen_html(output$ingest), "this session's run", fixed = TRUE)
    expect_match(gen_html(output$data_ui), "one card per casualty")
  })
})


# Tables and warnings ----------------------------------------------------

test_that("the metrics table survives Inf, NA and a missing bootstrap", {
  metrics <- tibble::tibble(
    statistic = c("efast", "died"), observed = c(0.30, 0.12),
    simulated = c(0.16, 0), ratio = c(1.88, Inf),
    ci_lower = c(NA_real_, NA_real_), ci_upper = c(NA_real_, NA_real_),
    verdict = c("FAIL", "FAIL"), control = c(FALSE, TRUE),
    location_z = c(0.02, NA_real_), reason = c("ratio 1.88", "")
  )
  shown <- .sdt_gen_metrics_display(metrics)
  expect_identical(shown$Verdict, c("FAIL", "FAIL"))
  expect_identical(shown$Role, c("behaviour", "control"))
  expect_identical(shown$Ratio, c("1.88", "Inf"))
  # A bootstrap that was never run reads as an em dash, not as NA to NA.
  expect_identical(unique(shown$Interval), "—")
  expect_identical(shown$Why, c("ratio 1.88", "—"))

  # A table built by hand, without every column sdt_fidelity() emits, still
  # renders: the panel is asked to show tables it did not compute.
  bare <- metrics[, c("statistic", "observed", "simulated", "ratio",
                      "verdict", "control")]
  expect_identical(nrow(.sdt_gen_metrics_display(bare)), 2L)

  chips <- .sdt_gen_metrics_chips(shown)
  expect_match(chips$Verdict[[1L]], "sdt-chip-red", fixed = TRUE)
  expect_match(chips$Role[[2L]], "sdt-chip-yellow", fixed = TRUE)
})

test_that("the panel mounts the richest table widget the session can draw", {
  skip_if_no_shiny()
  state <- fake_state()
  state$behaviour <- structure(list(), class = "sdt_behaviour_model")
  state$fidelity <- tibble::tibble(
    statistic = "efast", observed = 0.3, simulated = 0.3, ratio = 1,
    ci_lower = NA_real_, ci_upper = NA_real_, verdict = "PASS",
    control = FALSE, location_z = 0, reason = ""
  )
  shiny::testServer(mod_generate_server, args = list(state = state), {
    expected <- if (requireNamespace("reactable", quietly = TRUE)) {
      "metrics_reactable"
    } else if (requireNamespace("DT", quietly = TRUE)) {
      "metrics_dt"
    } else {
      "metrics_table"
    }
    # The element the UI emits and the renderer kept alive beside it are
    # decided from one value; a session that mounts one and renders the other
    # draws an empty tab.
    expect_identical(metrics_widget, expected)
    expect_true(expected %in% mounted)
    expect_match(gen_html(output$fidelity_ui), session$ns(expected),
                 fixed = TRUE)
    if (have_reactable) {
      expect_match(gen_html(output$metrics_reactable), "Reactable",
                   fixed = TRUE)
    }
    if (have_dt) {
      expect_match(gen_html(output$metrics_dt), "sdt-chip", fixed = TRUE)
    }
  })
})

test_that("the cost line warns before a long fit, and only then", {
  skip_if_no_shiny()
  path <- gen_csv()
  shiny::testServer(mod_generate_server, args = list(state = fake_state()), {
    session$setInputs(source = "file", upload = gen_upload(path))
    cheap <- gen_html(output$cost)
    expect_match(cheap, "3 outcomes selected", fixed = TRUE)
    expect_match(cheap, "91 seconds", fixed = TRUE)
    expect_false(grepl("sdt-chip-red", cheap, fixed = TRUE))
    session$setInputs(outcomes = c("efast", "ct", "died"))
    expect_length(chosen_outcomes(), 3L)
    # Four outcomes is where a press starts to look like a hung app.
    session$setInputs(outcomes = c("efast", "ct", "died", "nonsense"))
    expect_length(chosen_outcomes(), 3L)
  })
})

test_that("the preview shows the first rows and no more", {
  wide <- gen_frame()
  wide$extra <- as.list(seq_len(nrow(wide)))
  out <- .sdt_gen_preview(wide, rows = 4L, cols = 3L)
  expect_identical(dim(out), c(4L, 3L))
  # A list column renders as a mangled string in a plain table, so it is
  # shown as its type instead.
  narrow <- .sdt_gen_preview(wide[, c("card", "extra")], rows = 2L)
  expect_identical(narrow$extra, c("integer", "integer"))
})

test_that("the simulation summary counts groups, cards and replicates", {
  sim <- tibble::tibble(
    .replicate = rep(1:2, each = 6L),
    sim_group = rep(c("a", "b", "c", "d"), each = 3L),
    card = rep(c("c1", "c2", "c3"), 4L)
  )
  note <- .sdt_gen_sim_summary(sim)
  expect_match(note, "4 simulated groups over 3 cards in 2 replicates")
  expect_match(note, "12 rows", fixed = TRUE)
  expect_match(note, "new team in a new session")
  attr(sim, "new_teams") <- FALSE
  expect_match(.sdt_gen_sim_summary(sim), "check on the fit")
})

test_that("a cli warning becomes one readable line", {
  note <- .sdt_gen_one_line(c("first\nline  here", "second"))
  expect_identical(note, "first line here second")
  expect_length(.sdt_gen_one_line(character()), 1L)
})
