# The Simulate panel, driven headlessly.
#
# This is the panel that plays the board, and everything downstream -- the
# sequence analysis, the end-of-game comparison, the report -- reads the run it
# produces, so a regression here is expensive. The module server is driven
# directly with shiny::testServer(), the way test-app.R does it, and the
# assertions are on the reactives rather than on the widgets: a widget is only
# ever as right as the reactive underneath it.

skip_if_no_shiny <- function() {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("htmltools")
}

skip_if_no_board <- function() {
  skip_if_no_shiny()
  skip_if_not_installed("simmer")
}

# A reactiveValues store standing in for the app's shared state.
fake_state <- function(...) {
  shiny::reactiveValues(scenario = NULL, run = NULL, models = NULL,
                        bench = NULL, ...)
}

demo_scenario <- function(n = 15) {
  co <- sdt_add_decisions(sdt_synthetic_cohort(n = n, seed = 1), seed = 2)
  sdt_scenario(co, resources = list(or = 2, icu_imc = 4, ward = 20))
}

empty_scenario <- function() {
  sdt_scenario(sdt_cohort(sdt_synthetic_cohort(n = 3, seed = 1)[0, ]))
}

# One played board, built at most once. Most of what this file checks sits
# downstream of the run rather than of the run button, and replaying the board
# for every test is the difference between a suite that gets run and one that
# gets skipped.
demo_run <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- sdt_simulate(demo_scenario(), replications = 2,
                              horizon = 120, seed = 1, verbose = FALSE)
    }
    cached
  }
})

# Everything the panel reads before it will draw, named in one place so a test
# about one control does not have to restate the other six.
set_sim_inputs <- function(session, ...) {
  do.call(session$setInputs, utils::modifyList(
    list(reps = 2, horizon = 120, seed = 1, parallel = FALSE,
         replication = 1, at = 120, version = "2.0"),
    list(...)
  ))
}

# renderUI() and renderTable() hand back the markup plus its dependencies; the
# assertions here are all about the markup.
html_of <- function(x) paste(as.character(x), collapse = "")

# What the panel said out loud. MockShinySession has no notification stack, so
# a showNotification() call inside testServer goes nowhere and a panel that
# stopped explaining itself would still pass every other assertion here. The
# mock is registered in the caller's frame so it lasts the whole test.
catch_notifications <- function(env = parent.frame()) {
  caught <- new.env(parent = emptyenv())
  caught$notes <- list()
  local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") {
      caught$notes <- c(caught$notes, list(list(text = ui, type = type)))
      invisible(NULL)
    },
    .package = "shiny",
    .env = env
  )
  caught
}

# How many casualties the board reactive says are standing on the board. Read
# off the subtitle because that is the number the user actually sees.
on_board <- function(plot) {
  as.integer(sub("^.*\u2014 ([0-9]+) of.*$", "\\1", plot$labels$subtitle))
}


# Sidebar sanitisers -------------------------------------------------------

test_that("a cleared run setting falls back to the last good value", {
  # A cleared numericInput arrives as NA and a control the browser has not
  # drawn yet arrives as NULL. sdt_simulate() and sdt_plot_board() abort on
  # both, so without these a half-typed seed puts an error dialogue over the
  # panel instead of simply doing nothing yet.
  d <- .sdt_sim_defaults()
  expect_identical(.sdt_sim_num(NA, d$horizon), 270)
  expect_identical(.sdt_sim_num(NULL, d$horizon), 270)
  expect_identical(.sdt_sim_num(character(), d$horizon), 270)
  expect_identical(.sdt_sim_num(-5, d$horizon, min = 1), 270)
  # Minute zero is a legal place to park the clock, not a missing value.
  expect_identical(.sdt_sim_num(0, d$horizon), 0)
  expect_identical(.sdt_sim_count(NA, d$reps), 5L)
  expect_identical(.sdt_sim_count(0, d$reps), 5L)
  expect_identical(.sdt_sim_count(3, d$reps), 3L)
  # A slider the browser has not drawn yet is the same problem read the other
  # way round, and lands on the first replication.
  expect_identical(.sdt_sim_count(NULL, 1L), 1L)
})

test_that("the version picker only offers summary() a version it knows", {
  # summary.game_run() matches its argument strictly, so anything unexpected
  # has to become the default here rather than aborting there.
  expect_identical(.sdt_sim_version("1.0"), "1.0")
  expect_identical(.sdt_sim_version("2.0"), "2.0")
  expect_identical(.sdt_sim_version("1.5"), "2.0")
  expect_identical(.sdt_sim_version(NULL), "2.0")
})

test_that("missing end-of-game numbers print as the one no-value glyph", {
  # The value boxes use an em-dash for "no value"; the table has to agree, or
  # the same absence reads as two different things on one screen.
  expect_identical(.sdt_sim_pct(NA_real_), "\u2014")
  expect_identical(.sdt_sim_pct(0.083), "8.3%")
  expect_identical(.sdt_sim_pp(NA_real_), "\u2014")
  expect_identical(.sdt_sim_pp(0.05), "+5.0 pp")
  expect_identical(.sdt_sim_pp(-0.05), "-5.0 pp")
})

test_that("the board tooltip keeps the patient card's line breaks", {
  # ggiraph drops the tooltip into a plain element; without pre-line the card's
  # several short lines arrive as one run-on sentence.
  expect_match(.sdt_sim_tooltip_css(), "white-space:pre-line", fixed = TRUE)
})


# Gate and status ----------------------------------------------------------

test_that("the panel narrates where the session is and what it holds", {
  skip_if_no_board()
  # Before the gate existed the panel looked ready to use with no scenario
  # behind it, and the run button simply did nothing when pressed. The chips
  # answer the next question: whether the thing that is missing is the
  # scenario, the run, or an optional package.
  state <- fake_state()
  shiny::testServer(mod_simulate_server, args = list(state = state), {
    set_sim_inputs(session)
    expect_match(html_of(output$gate), "Build a scenario first")
    empty <- html_of(output$status)
    expect_match(empty, "No scenario", fixed = TRUE)
    expect_match(empty, "Not run yet", fixed = TRUE)

    state$scenario <- demo_scenario()
    session$flushReact()
    expect_match(html_of(output$gate), "Ready to run this board")
    expect_match(html_of(output$status), "Scenario: 15 casualties",
                 fixed = TRUE)

    state$run <- demo_run()
    session$flushReact()
    expect_match(html_of(output$gate), "Scrub the clock")
    ready <- html_of(output$status)
    expect_match(ready, "2 replications run", fixed = TRUE)

    # The widget chips let a user tell an absent optional package from a
    # broken panel, so they have to follow what is actually installed.
    expect_match(
      ready,
      if (requireNamespace("ggiraph", quietly = TRUE)) {
        "Board: interactive"
      } else {
        "Board: static"
      },
      fixed = TRUE
    )
  })
})


# The run button -----------------------------------------------------------

test_that("pressing run with no scenario says so rather than doing nothing", {
  skip_if_no_board()
  # A button that silently does nothing is indistinguishable from a broken
  # one; this path has to notify and leave the shared state untouched.
  said <- catch_notifications()
  state <- fake_state()
  shiny::testServer(mod_simulate_server, args = list(state = state), {
    set_sim_inputs(session, run = 1)
    expect_null(state$run)
    expect_length(said$notes, 1L)
    expect_match(said$notes[[1L]]$text, "Build a scenario", fixed = TRUE)
    expect_identical(said$notes[[1L]]$type, "warning")
    # The value boxes are deliberately not req()-ed on the run: a suspended
    # value box renders as a titled grey slab, which reads as broken.
    expect_identical(output$vb_rows, "\u2014")
    expect_identical(output$vb_events, "\u2014")
    expect_identical(output$vb_time, "\u2014")
  })
})

test_that("the run button plays the board and fills the shared state", {
  skip_if_no_board()
  state <- fake_state()
  state$scenario <- demo_scenario()
  shiny::testServer(mod_simulate_server, args = list(state = state), {
    set_sim_inputs(session, run = 1)
    expect_s3_class(state$run, "game_run")
    expect_identical(state$run$meta$replications, 2L)
    expect_identical(state$run$meta$horizon, 120)
    expect_false(state$run$meta$parallel)
    expect_gt(nrow(state$run$events), 0L)
    expect_identical(output$vb_rows, format(nrow(state$run$arrivals)))
    expect_identical(output$vb_events, format(nrow(state$run$events)))
    expect_match(output$vb_time, "^[0-9]+ min$")
    # Every tab has something to draw now, so none of them may still be
    # showing its empty state, and each one's plot has to build.
    for (panel in c("board_ui", "flow_ui", "util_ui", "end_ui",
                    "end_table_ui")) {
      expect_false(grepl("sdt-empty", html_of(output[[panel]]), fixed = TRUE),
                   info = panel)
    }
    expect_gt(flow_rows(), 0L)
    expect_s3_class(board_plot(), "ggplot")
    expect_s3_class(flow_plot(), "ggplot")
    expect_s3_class(util_plot(), "ggplot")
  })
})

test_that("an engine failure is reported rather than left as a blank panel", {
  skip_if_no_board()
  # The run is wrapped in tryCatch for a reason: an abort from the engine used
  # to take the observer down with it, leaving a panel that had lost its run
  # button and said nothing about why. What it says now is the engine's own
  # message: a generic "something went wrong" is not worth the dialogue.
  said <- catch_notifications()
  local_mocked_bindings(
    sdt_simulate = function(...) stop("the engine fell over"),
    .package = "sandtablR"
  )
  state <- fake_state()
  state$scenario <- demo_scenario(n = 6)
  shiny::testServer(mod_simulate_server, args = list(state = state), {
    set_sim_inputs(session, run = 1)
    expect_null(state$run)
    expect_length(said$notes, 1L)
    expect_identical(said$notes[[1L]]$text, "the engine fell over")
    expect_identical(said$notes[[1L]]$type, "error")
    expect_match(html_of(output$board_ui), "The board is empty", fixed = TRUE)
  })
})

test_that("cleared run settings still produce a run, on the defaults", {
  skip_if_no_board()
  # The sanitisers are only worth anything if they reach the engine: a user
  # who selects the seed box and hits run mid-edit must still get a board.
  state <- fake_state()
  state$scenario <- demo_scenario(n = 6)
  d <- .sdt_sim_defaults()
  shiny::testServer(mod_simulate_server, args = list(state = state), {
    set_sim_inputs(session, reps = NA, horizon = NA, seed = NA, run = 1)
    expect_s3_class(state$run, "game_run")
    expect_identical(state$run$meta$replications, d$reps)
    expect_identical(state$run$meta$horizon, d$horizon)
    expect_identical(state$run$meta$seed, d$seed)
  })
})

test_that("the parallel switch reaches the engine", {
  skip_if_no_board()
  # Without future.apply installed sdt_simulate() warns and falls back, which
  # would make this test noisy rather than informative.
  skip_if_not_installed("future.apply")
  # It also falls back, and records parallel = FALSE, when it would get fewer
  # than two workers. That is every R CMD check --as-cran run, where
  # _R_CHECK_LIMIT_CORES_ caps the cores at two and one is left free. Two
  # workers are asked for outright, which stays inside that cap.
  local_mocked_bindings(.sdt_workers = function(workers = NULL) 2L,
                        .package = "sandtablR")
  state <- fake_state()
  state$scenario <- demo_scenario(n = 8)
  shiny::testServer(mod_simulate_server, args = list(state = state), {
    set_sim_inputs(session, reps = 1, horizon = 60, parallel = TRUE, run = 1)
    expect_true(state$run$meta$parallel)
    expect_identical(state$run$meta$replications, 1L)
  })
})

test_that("a run rebounds the replication and the clock sliders", {
  skip_if_no_board()
  # A replication slider left at 5 by a previous run would ask this one for a
  # replication it never produced, and a clock still scaled to the old horizon
  # would scrub past the end of the game. MockShinySession swallows input
  # messages, so the only way to see them is to catch the call.
  sent <- list()
  local_mocked_bindings(
    updateSliderInput = function(session, id, ...) {
      sent[[id]] <<- list(...)
      invisible(NULL)
    },
    .package = "shiny"
  )
  state <- fake_state()
  state$scenario <- demo_scenario(n = 6)
  shiny::testServer(mod_simulate_server, args = list(state = state), {
    set_sim_inputs(session, reps = 3, horizon = 90, run = 1)
    expect_identical(sent$replication$max, 3L)
    expect_identical(sent$replication$value, 1)
    expect_identical(sent$at$min, 0)
    expect_identical(sent$at$max, 90)
    expect_identical(sent$at$value, 90)
  })
})

test_that("the replication shown is clamped to what was actually run", {
  skip_if_no_board()
  # Belt and braces for the slider bounds above: even if the widget is stale,
  # asking for replication nine of a two-replication run must not quietly draw
  # an empty board.
  state <- fake_state()
  state$scenario <- demo_scenario()
  state$run <- demo_run()
  shiny::testServer(mod_simulate_server, args = list(state = state), {
    set_sim_inputs(session, replication = 9)
    expect_identical(shown_rep(), 2L)
    # A request the run can honour is honoured, so this is a clamp and not a
    # reactive stuck on the last replication.
    session$setInputs(replication = 1)
    expect_identical(shown_rep(), 1L)
  })
})


# Before there is a run ----------------------------------------------------

test_that("every tab explains itself before there is a run", {
  skip_if_no_shiny()
  # A panel that renders blank leaves the user to guess whether the app is
  # broken, and they usually guess wrong. Each tab has to name what is missing
  # and which control produces it.
  state <- fake_state()
  state$scenario <- demo_scenario()
  shiny::testServer(mod_simulate_server, args = list(state = state), {
    set_sim_inputs(session)
    board <- html_of(output$board_ui)
    expect_match(board, "The board is empty", fixed = TRUE)
    expect_match(board, "scrub the clock", fixed = TRUE)
    expect_match(html_of(output$flow_ui), "No patient flow yet", fixed = TRUE)
    expect_match(html_of(output$util_ui), "No utilisation to show",
                 fixed = TRUE)
    expect_match(html_of(output$end_ui), "Nothing has ended yet", fixed = TRUE)
    expect_match(html_of(output$end_table_ui), "No end-of-game table yet",
                 fixed = TRUE)
  })
})


# The board ----------------------------------------------------------------

test_that("scrubbing the clock changes what the board reports", {
  skip_if_no_board()
  # The scrubber is the panel's one moving part. Minute zero is the awkward
  # case: an empty board carries no sighting values at all, which is what used
  # to make the legend vanish mid-animation, so it has to be drawn as happily
  # as a full one.
  state <- fake_state()
  state$scenario <- demo_scenario()
  state$run <- demo_run()
  shiny::testServer(mod_simulate_server, args = list(state = state), {
    set_sim_inputs(session, at = 0)
    start <- board_plot()
    expect_s3_class(start, "ggplot")
    expect_identical(on_board(start), 0L)

    session$setInputs(at = 60)
    middle <- board_plot()
    expect_s3_class(middle, "ggplot")

    session$setInputs(at = 120)
    finish <- board_plot()
    expect_s3_class(finish, "ggplot")

    # Casualties arrive in tranches and never leave the board, so the count is
    # non-decreasing and the game ends with the whole cohort placed.
    expect_lte(on_board(start), on_board(middle))
    expect_lte(on_board(middle), on_board(finish))
    expect_identical(on_board(finish), 15L)
  })
})


# Flow and utilisation -----------------------------------------------------

test_that("a board nobody stood on renders empty states, not errors", {
  skip_if_no_board()
  # A run can legitimately come back with an empty event log. sdt_plot_flow()
  # answers that with a bare titled panel, which reads as a broken plot rather
  # than as an empty board, so the panel has to intercept it and say what
  # happened.
  state <- fake_state()
  state$scenario <- empty_scenario()
  shiny::testServer(mod_simulate_server, args = list(state = state), {
    set_sim_inputs(session, reps = 1, horizon = 60, run = 1)
    expect_s3_class(state$run, "game_run")
    expect_identical(output$vb_rows, "0")
    expect_identical(output$vb_events, "0")
    # A board played with nobody on it is not the same as a board not yet
    # played, so this is a measured blank, not the missing-value glyph.
    expect_identical(output$vb_time, "--")
    expect_identical(flow_rows(), 0L)
    expect_match(html_of(output$flow_ui),
                 "This replication put nobody on the board", fixed = TRUE)
    expect_match(html_of(output$util_ui), "No area was ever occupied",
                 fixed = TRUE)
    # The end-of-game table still lists all eight locations, at zero.
    expect_match(html_of(output$end_table), "Not yet seen", fixed = TRUE)
  })
})


# End of game --------------------------------------------------------------

test_that("the version picker feeds through to the published column", {
  skip_if_no_board()
  # The two evaluated versions of the exercise report different end states, so
  # a picker that did not reach summary.game_run() would silently compare
  # every run against 2.0 while claiming otherwise.
  state <- fake_state()
  state$scenario <- demo_scenario()
  state$run <- demo_run()
  shiny::testServer(mod_simulate_server, args = list(state = state), {
    set_sim_inputs(session, version = "2.0")
    two <- end_summary()
    two_html <- html_of(output$end_table)

    session$setInputs(version = "1.0")
    one <- end_summary()
    one_html <- html_of(output$end_table)

    expect_identical(two$reference, unname(
      sdt_reference("published", version = "2.0")$end_state[two$area]
    ))
    expect_identical(one$reference, unname(
      sdt_reference("published", version = "1.0")$end_state[one$area]
    ))
    expect_false(isTRUE(all.equal(two$reference, one$reference)))
    # The simulated shares are unchanged, so only the published side moved.
    expect_identical(two$proportion, one$proportion)
    expect_false(identical(two_html, one_html))
    # 1.0 puts 15% of the cohort in screening and 2.0 puts 8%; neither number
    # can turn up in the simulated column, which moves in thirtieths.
    expect_match(one_html, "15.0%", fixed = TRUE)
    expect_false(grepl("15.0%", two_html, fixed = TRUE))
  })
})

test_that("the end-of-game table reads out the location, not the join key", {
  skip_if_no_board()
  # `area` is the key the published figures are joined on; nobody reading a
  # table should have to translate "icu_imc" back into a place.
  state <- fake_state()
  state$scenario <- demo_scenario()
  state$run <- demo_run()
  shiny::testServer(mod_simulate_server, args = list(state = state), {
    set_sim_inputs(session)
    table <- html_of(output$end_table)
    expect_false(grepl("icu_imc", table, fixed = TRUE))
    expect_false(grepl("not_seen", table, fixed = TRUE))
    expect_match(table, "ICU / IMC", fixed = TRUE)
    expect_match(table, "Not yet seen", fixed = TRUE)
    for (column in c("Location", "Casualties", "Simulated", "Published",
                     "Difference")) {
      expect_match(table, column, fixed = TRUE)
    }
    expect_match(table, "pp", fixed = TRUE)
  })
})


# Download -----------------------------------------------------------------

test_that("the event log downloads as a CSV carrying the log's columns", {
  skip_if_no_board()
  state <- fake_state()
  state$scenario <- demo_scenario()
  state$run <- demo_run()
  shiny::testServer(mod_simulate_server, args = list(state = state), {
    set_sim_inputs(session)
    csv <- utils::read.csv(output$download)
    expect_named(csv, names(.empty_events()))
    expect_identical(nrow(csv), nrow(state$run$events))
    expect_identical(sort(unique(csv$replication)), c(1L, 2L))
  })
})

test_that("a download taken before a run explains its own shape", {
  skip_if_no_shiny()
  # An empty log still carries its column names, so this is a header-only CSV
  # rather than a failed download.
  state <- fake_state()
  shiny::testServer(mod_simulate_server, args = list(state = state), {
    set_sim_inputs(session)
    csv <- utils::read.csv(output$download)
    expect_named(csv, names(.empty_events()))
    expect_identical(nrow(csv), 0L)
  })
})


# Without the interactive widgets ------------------------------------------

test_that("the panel falls back to static plots when the widgets are absent", {
  skip_if_no_board()
  # ggiraph and plotly are Suggests, so the fallback branch is the one that
  # never runs on a machine where both are installed -- which is every machine
  # this suite is normally run on. Faking their absence is the only way it is
  # ever exercised, and the failure it guards against is silent: the UI would
  # point at an output that was never defined and the tab would come up blank.
  present <- base::requireNamespace
  local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = FALSE) {
      if (package %in% c("ggiraph", "plotly")) {
        return(FALSE)
      }
      present(package, ..., quietly = quietly)
    },
    .package = "base"
  )
  state <- fake_state()
  state$scenario <- demo_scenario(n = 10)
  shiny::testServer(mod_simulate_server, args = list(state = state), {
    set_sim_inputs(session, reps = 1, horizon = 90, at = 90, run = 1)
    status <- html_of(output$status)
    expect_match(status, "Board: static", fixed = TRUE)
    expect_match(status, "Flow: static", fixed = TRUE)
    for (id in c("board_static", "flow_static", "util_static")) {
      expect_match(html_of(output[[sub("_static$", "_ui", id)]]), id,
                   fixed = TRUE)
    }
    # The static renderers have to exist, not merely be pointed at.
    expect_type(output$board_static$src, "character")
    expect_type(output$flow_static$src, "character")
    expect_type(output$util_static$src, "character")
  })
})
