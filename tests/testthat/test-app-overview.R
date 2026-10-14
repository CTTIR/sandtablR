# The overview panel, the shell it sits in, and the launcher.
#
# The overview is the panel the workbench opens on, which makes it the one
# panel routinely seen in the state nobody thinks to test: nothing built,
# nothing run, nothing trained. Most of this file is about that state, and
# about the single button that leaves it.
#
# The shell and the launcher are covered here too. They run before any module
# does, and they are what a deployment actually touches, so a fault in either
# only shows up once the app is served from somewhere other than the machine
# it was written on. What test-app.R already covers of the shell -- the six
# older panels, the logo, the brand, the report rendered directly -- is not
# repeated here.

skip_if_no_shiny <- function() {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("htmltools")
}

# A reactiveValues store standing in for the app's shared state.
fake_state <- function(...) {
  shiny::reactiveValues(scenario = NULL, run = NULL, models = NULL,
                        bench = NULL, ...)
}

# A renderUI() output is a list of the rendered HTML and its dependencies.
# Every assertion here is about the HTML, so take that and leave the rest.
render_html <- function(x) as.character(x$html)

# A leaderboard shaped like sdt_workflowset()'s: one row per engine per
# metric. The shape is the point -- see the models value box below.
fake_bench <- function(engines = c("tree", "ranger", "glmnet", "xgboost"),
                       metrics = c("accuracy", "kap", "roc_auc",
                                   "brier_class")) {
  tibble::tibble(
    engine = rep(engines, each = length(metrics)),
    class_box = rep(c("glass", "black"), length.out = length(engines) *
                      length(metrics)),
    .metric = rep(metrics, times = length(engines)),
    .estimator = "hand_till",
    .estimate = 0.5
  )
}


# The empty panel --------------------------------------------------------------

test_that("every value box says something before anything is built", {
  skip_if_no_shiny()
  shiny::testServer(mod_overview_server, args = list(state = fake_state()), {
    # bslib draws a value box with an empty body as a titled grey slab, which
    # reads as broken rather than as waiting. Each state-backed box therefore
    # has to emit the em-dash and the reason it is empty.
    for (nm in c("vb_scenario", "vb_run", "vb_models")) {
      html <- render_html(output[[nm]])
      expect_match(html, "—", fixed = TRUE, info = nm)
      expect_match(html, "sdt-metric-empty", fixed = TRUE, info = nm)
    }
    expect_match(render_html(output$vb_scenario), "Not built yet")
    expect_match(render_html(output$vb_run), "Not run yet")
    expect_match(render_html(output$vb_models), "None trained yet")
  })
})

test_that("the engines box reports the machine, not the session", {
  skip_if_no_shiny()
  shiny::testServer(mod_overview_server, args = list(state = fake_state()), {
    # This is the one box that must not be blank on an empty session: it
    # answers "what can this installation do", which is the question a user
    # has before they have done anything at all.
    html <- render_html(output$vb_engines)
    eng <- sdt_engines()
    expect_match(html, sprintf("%d of %d", sum(eng$installed), nrow(eng)),
                 fixed = TRUE)
    expect_match(html, "model engines installed", fixed = TRUE)
    for (e in eng$engine) expect_match(html, e, fixed = TRUE, info = e)
    # An engine with no package installed is greyed out rather than dropped,
    # so the chips always show the full bench and what is missing from it.
    expect_identical(grepl("sdt-chip-muted", html, fixed = TRUE),
                     any(!eng$installed))
  })
})

test_that("the design card reports the published exercise", {
  skip_if_no_shiny()
  shiny::testServer(mod_overview_server, args = list(state = fake_state()), {
    # These are the numbers from the paper's Methods, and they are the
    # package's claim to re-implement something published rather than
    # something invented here. A silent change to the bundled reference has
    # to fail a test.
    html <- render_html(output$design)
    expect_match(html, "90 minutes, 20 patient cards, played in 4 rounds",
                 fixed = TRUE)
    expect_match(html, "9 areas", fixed = TRUE)
    expect_match(html, "3 people", fixed = TRUE)
    expect_match(html, "Berlin hospital screening algorithm", fixed = TRUE)
  })
})

test_that("the provenance card carries the DOI", {
  skip_if_no_shiny()
  shiny::testServer(mod_overview_server, args = list(state = fake_state()), {
    # The marginals are someone else's data. The citation, the resolvable
    # DOI and the licence travel with them on the page itself, not only in
    # the documentation, because the page is what gets screenshotted.
    html <- render_html(output$provenance)
    expect_match(html, "10.1007/s00068-022-02217-1", fixed = TRUE)
    expect_match(html, "https://doi.org/10.1007/s00068-022-02217-1",
                 fixed = TRUE)
    expect_match(html, "CC BY 4.0", fixed = TRUE)
    # The card also says why those marginals are kept apart from the
    # package's own defaults, which is the reason it exists at all.
    expect_match(html, "against its own assumptions", fixed = TRUE)
  })
})


# Counting what has been trained -----------------------------------------------

test_that("the models box counts engines, not leaderboard rows", {
  skip_if_no_shiny()
  state <- fake_state()
  state$bench <- fake_bench()
  shiny::testServer(mod_overview_server, args = list(state = state), {
    # The bench carries one row per engine per metric, so four engines scored
    # on four metrics is a sixteen-row tibble. Counting rows reported "16
    # models compared" for a bench of four.
    html <- render_html(output$vb_models)
    expect_match(html, ">4<", fixed = TRUE)
    expect_false(grepl(">16<", html, fixed = TRUE))
    expect_match(html, "models compared", fixed = TRUE)
  })
})

test_that("a single trained model reads as one model", {
  skip_if_no_shiny()
  state <- fake_state()
  state$models <- list(tree = "fitted")
  shiny::testServer(mod_overview_server, args = list(state = state), {
    # The model lab writes one model before it writes a bench, so the box has
    # to report the singular case in the singular.
    html <- render_html(output$vb_models)
    expect_match(html, ">1<", fixed = TRUE)
    expect_match(html, "model trained", fixed = TRUE)
    expect_false(grepl("models compared", html, fixed = TRUE))
  })
})


# The one-click demo -----------------------------------------------------------

test_that("one press of the demo button fills the whole workbench", {
  skip_if_no_shiny()
  skip_if_not_installed("simmer")
  state <- fake_state()
  # Screening four engines with tuning on takes the better part of a minute
  # and is covered where the bench itself is tested. What matters here is
  # that the observer runs all three steps and writes each result into the
  # shared state, so the expensive step stands in for itself -- and says the
  # stack is present, so the write is exercised on a thin machine too.
  local_mocked_bindings(.sdt_tidymodels_pkgs = function() "parsnip",
                        sdt_workflowset = function(...) fake_bench())
  shiny::testServer(mod_overview_server, args = list(state = state), {
    session$setInputs(demo = 1)

    expect_s3_class(state$scenario, "scenario")
    expect_identical(state$scenario$meta$name, "reference")
    # The demo trains on the participant's calls, so the cohort it builds has
    # to carry them; the reference scenario on its own carries only the cards.
    expect_true("called_triage" %in% names(state$scenario$cohort))

    expect_s3_class(state$run, "game_run")
    expect_gt(nrow(state$run$events), 0L)

    # The leaderboard the training step returned is the one the panel shows.
    expect_identical(state$bench, fake_bench())

    # The boxes are the only feedback the press gives, so they have to have
    # caught up with the state by the time the observer returns.
    scenario_html <- render_html(output$vb_scenario)
    expect_match(scenario_html, sprintf(">%d<", nrow(state$scenario$cohort)),
                 fixed = TRUE)
    expect_match(scenario_html, "casualties on")
    expect_match(scenario_html, "reference", fixed = TRUE)
    expect_match(render_html(output$vb_run), ">5<", fixed = TRUE)
    expect_match(render_html(output$vb_run), "replications over")
    expect_match(render_html(output$vb_models), ">4<", fixed = TRUE)
  })
})

test_that("the demo stops short of a bench nothing can train", {
  skip_if_no_shiny()
  state <- fake_state()
  # With no tidymodels stack there is nothing to train, and that is a thin
  # workbench rather than a broken one: the step has to report that it did
  # nothing and leave the leaderboard empty for the value box to caption.
  local_mocked_bindings(.sdt_tidymodels_pkgs = function() character())
  shiny::isolate(expect_false(.sdt_demo_bench(state)))
  shiny::isolate(expect_null(state$bench))
})

test_that("rebuilding the demo scenario clears stale results", {
  skip_if_no_shiny()
  state <- fake_state()
  state$run <- "stale"
  state$models <- list(tree = "stale")
  state$bench <- "stale"
  # A run and a bench belong to the cohort they were made from. Leaving them
  # behind would show the previous board's numbers beside the new board's
  # scenario, which is the kind of wrong that looks right.
  shiny::isolate(expect_true(.sdt_demo_scenario(state)))
  shiny::isolate({
    expect_s3_class(state$scenario, "scenario")
    expect_null(state$run)
    expect_null(state$models)
    expect_null(state$bench)
  })
})


# The shell --------------------------------------------------------------------

test_that("the navbar carries the overview panel and its own controls", {
  skip_if_no_shiny()
  html <- as.character(htmltools::renderTags(sdt_app_ui())$html)
  # test-app.R walks the other six panels. Overview is the one the app opens
  # on; the report button and the dark-mode toggle live in the navbar rather
  # than in any module, so no module test would notice them going missing.
  # The toggle needs its id: plots are rendered server-side as images, so
  # they are the one thing on the page CSS cannot recolour.
  expect_match(html, "Overview", fixed = TRUE)
  expect_match(html, "id=\"report\"", fixed = TRUE)
  expect_match(html, "colour_mode", fixed = TRUE)
})

test_that("building the app object does not mount its assets", {
  skip_if_no_shiny()
  # Mount it first, so removing it starts from a known state rather than
  # warning about a resource that was never there.
  .sdt_mount_assets()
  shiny::removeResourcePath("sandtablR")

  app <- sdt_app()
  expect_s3_class(app, "shiny.appobj")
  # addResourcePath registers in whichever process calls it, and the browser
  # asks for the logo during page load. An app built in one process and
  # served from another would answer that request with a 404 while every
  # HTML-only test still passed, so the mount has to wait for onStart, which
  # runs in the serving process before it takes a request.
  expect_false("sandtablR" %in% names(shiny::resourcePaths()))
  app$onStart()
  expect_true("sandtablR" %in% names(shiny::resourcePaths()))
})

test_that("mounting the assets serves the directory the caller is given", {
  skip_if_no_shiny()
  path <- .sdt_mount_assets()
  # The caller gets the served directory back, so a deployment can hand it to
  # another route. If it is not the path Shiny registered, the two routes
  # serve different files.
  expect_true(dir.exists(path))
  expect_identical(normalizePath(shiny::resourcePaths()[["sandtablR"]]),
                   normalizePath(path))
})

test_that("the report button downloads from an entirely empty session", {
  skip_if_no_shiny()
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  shiny::testServer(sdt_app_server, {
    # The button is on the navbar from the first paint, so this is the state
    # it is most likely to be pressed in: every section missing its input.
    # test-app.R renders that report directly; what is tested here is the
    # handler around it -- the name the browser is given, and that a file
    # with the report in it is what arrives under that name.
    path <- output$report
    expect_identical(basename(path), "sandtablR-report.html")
    html <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_match(html, "No scenario was built", fixed = TRUE)
  })
})


# The launcher -----------------------------------------------------------------

test_that("the optional-package list is the absent packages, in order", {
  # Every optional package may well be installed on the machine running this,
  # which would leave a test of the real return value asserting nothing at
  # all. Standing in for the lookup is what makes the absent case reachable;
  # that the list never names an installed package is checked in test-app.R.
  local_mocked_bindings(
    requireNamespace = function(package, ...) {
      !package %in% c("DT", "TraMineR")
    },
    .package = "base"
  )
  expect_identical(.sdt_optional_app_pkgs(), c("DT", "TraMineR"))
})

test_that("the launcher names the missing packages before it starts", {
  skip_if_no_shiny()
  # runApp() blocks until the window is closed, so standing in for it is the
  # only way to reach the pre-flight message -- which is the whole of what
  # the launcher adds to shiny.
  local_mocked_bindings(runApp = function(appDir, ...) invisible(NULL),
                        .package = "shiny")
  local_mocked_bindings(.sdt_optional_app_pkgs = function() "notinstalled")
  expect_message(sdt_run_app(), "notinstalled")
})

test_that("the launcher hands runApp a fully wired app object", {
  skip_if_no_shiny()
  seen <- NULL
  local_mocked_bindings(
    runApp = function(appDir, ...) {
      seen <<- appDir
      invisible(NULL)
    },
    .package = "shiny"
  )
  local_mocked_bindings(.sdt_optional_app_pkgs = function() character())
  # Nothing is missing, so there is nothing to say: a launcher that talks on
  # a complete installation trains people to ignore it when it does not.
  expect_silent(sdt_run_app())
  # And it launches the object sdt_app() builds, not a directory, which is
  # what keeps the modules ordinary package functions.
  expect_s3_class(seen, "shiny.appobj")
})
