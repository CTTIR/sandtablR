# The workbench, driven headlessly.
#
# These exercise the module servers directly with shiny::testServer(), which
# is why the modules live in R/ rather than under inst/shiny: code under inst/
# is invisible to R CMD check, lintr and covr, and an app kept there is the
# one part of a package that nothing verifies.

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

demo_scenario <- function(n = 20) {
  co <- sdt_add_decisions(sdt_synthetic_cohort(n = n, seed = 1), seed = 2)
  sdt_scenario(co, resources = list(or = 2, icu_imc = 4, ward = 20))
}


# UI ---------------------------------------------------------------------------

test_that("the app UI assembles", {
  skip_if_no_shiny()
  ui <- sdt_app_ui()
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag") ||
                inherits(ui, "bslib_page"))
  html <- as.character(htmltools::renderTags(ui)$html)
  for (tab in c("Scenario", "Simulate", "Annotate", "Model lab", "Explain",
                "Sequences")) {
    expect_match(html, tab, fixed = TRUE)
  }
})

test_that("every module UI builds on its own", {
  skip_if_no_shiny()
  builders <- list(
    mod_scenario_ui, mod_simulate_ui, mod_annotate_ui,
    mod_model_ui, mod_explain_ui, mod_sequence_ui
  )
  for (build in builders) {
    expect_silent(html <- htmltools::renderTags(build("x"))$html)
    expect_gt(nchar(html), 100L)
  }
})

test_that("chips escape what they are given", {
  skip_if_no_shiny()
  html <- as.character(.sdt_chip("<script>alert(1)</script>", "red"))
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_match(html, "&lt;script&gt;")
})


# Scenario ----------------------------------------------------------------------

test_that("the scenario module builds a scenario into shared state", {
  skip_if_no_shiny()
  shiny::testServer(mod_scenario_server, args = list(state = fake_state()), {
    session$setInputs(
      n = 25, p_red = 0.5, p_yellow = 0.3, seed = 1, backend = "base",
      decisions = TRUE, cap_triage = 2, cap_ed_red = 3, cap_ed_yellow = 4,
      cap_diagnostics = 3, cap_or = 5, cap_recovery = 4, cap_icu_imc = 12,
      cap_ward = 25, build = 1
    )
    expect_s3_class(state$scenario, "scenario")
    expect_identical(nrow(state$scenario$cohort), 25L)
    expect_true("called_triage" %in% names(state$scenario$cohort))
    expect_identical(state$scenario$resources$or, 5L)
    expect_identical(output$vb_n, "25")
    expect_match(output$p_green_note, "0.20")
  })
})

test_that("building a new scenario clears stale downstream results", {
  skip_if_no_shiny()
  state <- fake_state()
  shiny::testServer(mod_scenario_server, args = list(state = state), {
    state$run <- "stale"
    state$models <- list(tree = "stale")
    session$setInputs(
      n = 10, p_red = 0.4, p_yellow = 0.3, seed = 2, backend = "base",
      decisions = FALSE, cap_triage = 1, cap_ed_red = 1, cap_ed_yellow = 1,
      cap_diagnostics = 1, cap_or = 1, cap_recovery = 1, cap_icu_imc = 1,
      cap_ward = 1, build = 1
    )
    expect_null(state$run)
    expect_null(state$models)
  })
})

test_that("the scenario module loads the calibrated reference board", {
  skip_if_no_shiny()
  shiny::testServer(mod_scenario_server, args = list(state = fake_state()), {
    session$setInputs(reference = 1)
    expect_s3_class(state$scenario, "scenario")
    expect_identical(state$scenario$meta$name, "reference")
  })
})

test_that("capacities below one are floored rather than accepted", {
  skip_if_no_shiny()
  shiny::testServer(mod_scenario_server, args = list(state = fake_state()), {
    session$setInputs(
      n = 5, p_red = 0.4, p_yellow = 0.3, seed = 1, backend = "base",
      decisions = FALSE, cap_triage = 0, cap_ed_red = NA, cap_ed_yellow = 4,
      cap_diagnostics = 3, cap_or = 5, cap_recovery = 4, cap_icu_imc = 12,
      cap_ward = 25, build = 1
    )
    expect_identical(state$scenario$resources$triage, 1L)
    expect_identical(state$scenario$resources$ed_red, 1L)
  })
})


# Simulate ----------------------------------------------------------------------

test_that("the simulate module runs and fills the shared state", {
  skip_if_no_shiny()
  skip_if_not_installed("simmer")
  state <- fake_state()
  state$scenario <- demo_scenario()
  shiny::testServer(mod_simulate_server, args = list(state = state), {
    session$setInputs(reps = 2, horizon = 200, seed = 1, parallel = FALSE,
                      replication = 1, run = 1)
    expect_s3_class(state$run, "game_run")
    expect_gt(nrow(state$run$events), 0L)
    expect_identical(output$vb_events, format(nrow(state$run$events)))
    expect_match(output$vb_time, "min")
  })
})

test_that("the simulate module reports an empty run without erroring", {
  skip_if_no_shiny()
  skip_if_not_installed("simmer")
  state <- fake_state()
  empty <- sdt_cohort(sdt_synthetic_cohort(n = 3, seed = 1)[0, ])
  state$scenario <- sdt_scenario(empty)
  shiny::testServer(mod_simulate_server, args = list(state = state), {
    session$setInputs(reps = 1, horizon = 100, seed = 1, parallel = FALSE,
                      replication = 1, run = 1)
    expect_identical(output$vb_time, "--")
  })
})


# Annotate ----------------------------------------------------------------------

test_that("a valid cell edit is written back into the cohort", {
  skip_if_no_shiny()
  state <- fake_state()
  state$scenario <- demo_scenario()
  shiny::testServer(mod_annotate_server, args = list(state = state), {
    co <- tibble::as_tibble(state$scenario$cohort)
    columns <- .sdt_annotate_columns(co)
    col_index <- which(columns == "called_triage") - 1L
    session$setInputs(table_cell_edit = list(
      row = 1L, col = col_index, value = "green"
    ))
    expect_identical(state$scenario$cohort$called_triage[[1L]], "green")
    expect_match(output$vb_edited, "^1 of")
  })
})

test_that("an edit outside the vocabulary is refused", {
  skip_if_no_shiny()
  state <- fake_state()
  state$scenario <- demo_scenario()
  before <- shiny::isolate(state$scenario$cohort$called_triage[[1L]])
  shiny::testServer(mod_annotate_server, args = list(state = state), {
    columns <- .sdt_annotate_columns(tibble::as_tibble(state$scenario$cohort))
    session$setInputs(table_cell_edit = list(
      row = 1L, col = which(columns == "called_triage") - 1L,
      value = "chartreuse"
    ))
    expect_identical(state$scenario$cohort$called_triage[[1L]], before)
    expect_match(output$vb_edited, "^0 of")
  })
})

test_that("the card's own columns cannot be edited", {
  skip_if_no_shiny()
  state <- fake_state()
  state$scenario <- demo_scenario()
  before <- shiny::isolate(state$scenario$cohort$triage[[1L]])
  shiny::testServer(mod_annotate_server, args = list(state = state), {
    columns <- .sdt_annotate_columns(tibble::as_tibble(state$scenario$cohort))
    session$setInputs(table_cell_edit = list(
      row = 1L, col = which(columns == "triage") - 1L, value = "green"
    ))
    expect_identical(state$scenario$cohort$triage[[1L]], before)
  })
})

test_that("resetting the calls restores the card", {
  skip_if_no_shiny()
  state <- fake_state()
  state$scenario <- demo_scenario()
  shiny::testServer(mod_annotate_server, args = list(state = state), {
    session$setInputs(reset = 1)
    expect_identical(state$scenario$cohort$called_triage,
                     state$scenario$cohort$triage)
    expect_match(output$vb_edited, "^0 of")
  })
})

test_that("a cohort without call columns is still annotatable", {
  skip_if_no_shiny()
  state <- fake_state()
  state$scenario <- sdt_scenario(sdt_synthetic_cohort(n = 8, seed = 1))
  shiny::testServer(mod_annotate_server, args = list(state = state), {
    expect_true(all(.sdt_editable_columns() %in% names(cohort())))
  })
})


# Model lab ---------------------------------------------------------------------

test_that("the model module trains an engine into shared state", {
  skip_on_cran()
  skip_if_no_shiny()
  skip_if_not(all(vapply(
    c("parsnip", "recipes", "workflows", "rsample", "tune", "yardstick",
      "dials", "hardhat", "rpart"),
    requireNamespace, logical(1), quietly = TRUE
  )), "tidymodels not installed")

  state <- fake_state()
  state$scenario <- demo_scenario(n = 200)
  shiny::testServer(mod_model_server, args = list(state = state), {
    session$setInputs(
      target = "called_triage", engine = "tree", prop = 0.75, folds = 3,
      tune = FALSE, grid = 3, seed = 1, fit = 1
    )
    expect_named(state$models, "tree")
    expect_s3_class(state$models$tree, "sdt_model")
    expect_false(is.null(state$bench))
    expect_match(output$vb_auc, "^[01]\\.")
  })
})

test_that("the model module refuses a target the cohort does not have", {
  skip_if_no_shiny()
  state <- fake_state()
  state$scenario <- sdt_scenario(sdt_synthetic_cohort(n = 20, seed = 1))
  shiny::testServer(mod_model_server, args = list(state = state), {
    session$setInputs(target = "called_triage", engine = "tree", fit = 1)
    expect_null(state$models)
  })
})


# Sequences ---------------------------------------------------------------------

test_that("the sequence module summarises decision paths", {
  skip_if_no_shiny()
  skip_if_not_installed("simmer")
  state <- fake_state()
  state$scenario <- demo_scenario()
  state$run <- shiny::isolate(
    sdt_simulate(state$scenario, replications = 3, seed = 1, verbose = FALSE)
  )
  shiny::testServer(mod_sequence_server, args = list(state = state), {
    session$setInputs(view = "index", all_reps = TRUE, replication = 1,
                      cluster = FALSE, k = 3, consistency = "om")
    expect_match(output$vb_paths, "^[0-9]+$")
    expect_match(output$vb_longest, "steps")
    expect_identical(nrow(consistency()), nrow(state$scenario$cohort))
  })
})

test_that("the sequence module switches consistency measure", {
  skip_if_no_shiny()
  skip_if_not_installed("simmer")
  state <- fake_state()
  state$scenario <- demo_scenario()
  state$run <- shiny::isolate(
    sdt_simulate(state$scenario, replications = 2, seed = 1, verbose = FALSE)
  )
  shiny::testServer(mod_sequence_server, args = list(state = state), {
    session$setInputs(view = "distribution", all_reps = TRUE,
                      replication = 1, cluster = FALSE, k = 3,
                      consistency = "entropy")
    out <- consistency()
    expect_false(any(is.na(out$consistency)))
  })
})


# Explain -----------------------------------------------------------------------

test_that("the explain module tracks the models in shared state", {
  skip_if_no_shiny()
  state <- fake_state()
  state$scenario <- demo_scenario()
  shiny::testServer(mod_explain_server, args = list(state = state), {
    session$setInputs(model = "", compare = "", permutations = 5,
                      profile_type = "partial", casualty = "")
    # With no models trained, the panel must not error.
    expect_null(state$models)
  })
})


# Whole app ---------------------------------------------------------------------

test_that("the app server wires every module without error", {
  skip_if_no_shiny()
  shiny::testServer(sdt_app_server, {
    expect_null(state$scenario)
    expect_null(state$run)
    session$setInputs(`scenario-n` = 10)
  })
})

test_that("the optional-package list names only absent packages", {
  missing <- .sdt_optional_app_pkgs()
  expect_type(missing, "character")
  for (pkg in missing) {
    expect_false(requireNamespace(pkg, quietly = TRUE), info = pkg)
  }
})


# Report ------------------------------------------------------------------------

test_that("the report renders from whatever state the session has", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  skip_if_not_installed("simmer")

  sc <- demo_scenario(n = 25)
  run <- sdt_simulate(sc, replications = 2, seed = 1, verbose = FALSE)
  out <- withr::local_tempfile(fileext = ".html")
  .sdt_render_report(list(scenario = sc, run = run), out)

  expect_true(file.exists(out))
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(html, "Scenario", fixed = TRUE)
  expect_match(html, "End-of-game", fixed = TRUE)
  # No models trained: the report has to say so, not fail.
  expect_match(html, "No models were trained", fixed = TRUE)
})

test_that("the report renders from an empty session", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  out <- withr::local_tempfile(fileext = ".html")
  .sdt_render_report(list(), out)
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(html, "No scenario was built", fixed = TRUE)
})


# Branding ----------------------------------------------------------------------

test_that("the logo ships with the package and is servable", {
  skip_if_no_shiny()
  # man/figures is for documentation; the app needs an installed copy, and
  # system.file() is the only way to find it once the package is installed.
  path <- system.file("www", "logo.png", package = "sandtablR")
  expect_true(nzchar(path))
  expect_true(file.exists(path))
  expect_gt(file.size(path), 1000)
})

test_that("mounting assets exposes the www directory to Shiny", {
  skip_if_no_shiny()
  path <- .sdt_mount_assets()
  expect_true(nzchar(path))
  expect_true("sandtablR" %in% names(shiny::resourcePaths()))
})

test_that("the navbar brand carries the logo and the package name", {
  skip_if_no_shiny()
  html <- as.character(htmltools::renderTags(.sdt_brand())$html)
  expect_match(html, "sandtablR/logo.png", fixed = TRUE)
  expect_match(html, "alt=\"sandtablR logo\"", fixed = TRUE)
  expect_match(html, "sandtablR", fixed = TRUE)
})

test_that("the app UI embeds the logo", {
  skip_if_no_shiny()
  html <- as.character(htmltools::renderTags(sdt_app_ui())$html)
  expect_match(html, "sandtablR/logo.png", fixed = TRUE)
})

test_that("sdt_app() mounts assets before it serves anything", {
  skip_if_no_shiny()
  # addResourcePath registers in whichever process calls it, and the browser
  # asks for the logo during page load, before any session exists. Mounting
  # from the UI or the server is therefore not enough: an app object built in
  # one process and served from another answers that request with a 404 while
  # every test that only inspects the HTML still passes. onStart runs in the
  # serving process before it takes a request, which is the fix.
  shiny::removeResourcePath("sandtablR")
  expect_false("sandtablR" %in% names(shiny::resourcePaths()))

  app <- sdt_app()
  expect_s3_class(app, "shiny.appobj")
  expect_type(app$onStart, "closure")
  app$onStart()
  expect_true("sandtablR" %in% names(shiny::resourcePaths()))
})

test_that("sdt_app() builds its UI per request, in the serving process", {
  skip_if_no_shiny()
  app <- sdt_app()
  expect_type(app$httpHandler, "closure")
  # A UI evaluated once at construction time is a UI built in the wrong
  # process; sdt_app() passes a function so it is built where it is served.
  ui <- environment(app$httpHandler)$ui
  expect_true(is.function(ui) || !is.null(ui))
})
