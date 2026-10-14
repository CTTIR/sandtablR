# The model lab, driven headlessly.
#
# This panel branches more per line than any other in the app: three
# leaderboard widgets, two ROC widgets, four states per value box and two
# separate reasons to refuse to train. Only the widget a given machine can
# actually mount is asserted on, and the chooser is asserted against the same
# requireNamespace() calls the module makes, so the file covers whatever is
# installed rather than pinning one particular install.

lab_needs_shiny <- function() {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("htmltools")
}

# Every engine and the whole tidymodels path sit in Suggests, so anything that
# actually trains has to be skipped wholesale where they are absent. That is
# the only reason anything here skips: a missing package is a fit that cannot
# be run, and skip_on_cran() would instead retire the seven tests that press
# the two buttons wherever NOT_CRAN is unset, which is most plain test runs.
# The whole file fits in about fifteen seconds, so there is nothing to buy.
lab_needs_engines <- function() {
  lab_needs_shiny()
  for (pkg in c("parsnip", "recipes", "workflows", "workflowsets", "rsample",
                "tune", "yardstick", "dials", "hardhat", "rpart")) {
    skip_if_not_installed(pkg)
  }
}

# A reactiveValues store standing in for the app's shared state.
lab_state <- function(...) {
  shiny::reactiveValues(scenario = NULL, run = NULL, models = NULL,
                        bench = NULL, ...)
}

# Sixty cards, not thirty. Thirty puts three casualties in the green stratum,
# the held-out quarter sees one of them, and every fit then spends its notes on
# classes too small to score rather than on anything the panel does. Sixty
# costs the same second to fit and leaves all three categories worth a metric.
lab_cohort <- function(n = 60) {
  sdt_add_decisions(sdt_synthetic_cohort(n = n, seed = 1), seed = 2)
}

lab_scenario <- function(n = 60) {
  sdt_scenario(lab_cohort(n))
}

# renderUI() hands back the markup and its dependencies; the assertions below
# only ever care about the markup.
lab_html <- function(x) {
  paste(as.character(x), collapse = " ")
}

# One real fit for the whole file. Several tests need a trained model to
# render from, and refitting for each of them would spend seconds proving the
# same thing.
lab_cache <- new.env(parent = emptyenv())

lab_model <- function() {
  if (is.null(lab_cache$model)) {
    lab_cache$model <- sdt_fit(
      tibble::as_tibble(lab_cohort()), target = "called_triage",
      engine = "tree", tune = FALSE, folds = 2L, prop = 0.75, seed = 1,
      verbose = FALSE
    )
  }
  lab_cache$model
}

# A leaderboard with one readable and one black-box engine on it, written out
# rather than fitted: the widgets and the cost arithmetic care about the shape
# of the board and the numbers in it, not about where they came from.
lab_board <- function(glass_auc = 0.62, black_auc = 0.71) {
  tibble::tibble(
    engine = rep(c("tree", "ranger"), each = 4L),
    class_box = rep(c("glass", "black"), each = 4L),
    .metric = rep(c("roc_auc", "accuracy", "kap", "f_meas"), 2L),
    .estimator = "multiclass",
    .estimate = c(glass_auc, 0.50, 0.20, 0.55,
                  black_auc, 0.60, 0.30, 0.62)
  )
}


# UI -----------------------------------------------------------------------

test_that("the UI names both buttons, every input and all three views", {
  lab_needs_shiny()
  html <- as.character(htmltools::renderTags(mod_model_ui("lab"))$html)
  # The two buttons come first because pressing them is the whole purpose of
  # the panel; losing one to a rename leaves nothing that can train.
  expect_match(html, "Train this engine", fixed = TRUE)
  expect_match(html, "Compare all engines", fixed = TRUE)
  for (box in c("Best engine", "Best ROC AUC", "Glass-box cost")) {
    expect_match(html, box, fixed = TRUE)
  }
  for (tab in c("Leaderboard", "ROC", "Confusion")) {
    expect_match(html, tab, fixed = TRUE)
  }
  # Every control the server reads has to exist under the module's own
  # namespace, or the panel quietly trains on its defaults instead.
  for (id in c("fit", "bench", "target", "engine", "prop", "folds", "tune",
               "grid", "seed", "roc_model")) {
    expect_match(html, sprintf("lab-%s", id), fixed = TRUE)
  }
})


# Preconditions ------------------------------------------------------------

test_that("the precondition says what is missing, then what is ready", {
  lab_needs_shiny()
  state <- lab_state()
  shiny::testServer(mod_model_server, args = list(state = state), {
    # A panel whose buttons do nothing until a scenario exists has to say so
    # where the buttons are, rather than only when one is pressed.
    expect_match(lab_html(output$precondition), "Build a scenario first")
    state$scenario <- lab_scenario()
    session$setInputs(target = "called_triage")
    ready <- lab_html(output$precondition)
    expect_match(ready, "Ready: 60 cards")
    expect_match(ready, "participant's called category")
  })
})

test_that("a target the cohort has no column for is refused", {
  lab_needs_shiny()
  state <- lab_state()
  # Built without decisions, so nothing was ever called by a participant.
  state$scenario <- sdt_scenario(sdt_synthetic_cohort(n = 30, seed = 1))
  shiny::testServer(mod_model_server, args = list(state = state), {
    session$setInputs(target = "called_triage", engine = "tree", tune = FALSE,
                      folds = 2, prop = 0.75, seed = 1, fit = 1)
    # sdt_fit() would abort on the missing column; the panel has to catch it
    # first and leave the shared state untouched. Asserting the guard itself
    # as well as its consequence is what separates "the panel refused" from
    # "the button happened not to be wired up".
    expect_false(can_train())
    expect_null(state$models)
    expect_null(state$bench)
    expect_match(lab_html(output$precondition),
                 "no participant's called category")
  })
})

test_that("neither button does anything before a scenario is built", {
  lab_needs_shiny()
  state <- lab_state()
  shiny::testServer(mod_model_server, args = list(state = state), {
    session$setInputs(target = "called_triage", engine = "tree",
                      fit = 1, bench = 1)
    # state$models was never set, so asserting it is still NULL only says the
    # presses did not write. The guard is what they went through, so assert it
    # directly too, and the panel's own account of why beside it.
    expect_false(can_train())
    expect_null(state$models)
    expect_null(state$bench)
    expect_match(lab_html(output$board_ui), "Build a scenario first")
    expect_match(lab_html(output$board_ui),
                 "learns the cohort you build on the Scenario tab")
  })
})

test_that("the missing-stack list is exactly the stack that is absent", {
  # Both refusals the panel can make are read off this one list, so it has to
  # cover the whole tidymodels path and name nothing that is installed. The
  # expectation is spelled out here rather than read back from the module: a
  # package quietly dropped from the list would otherwise still pass, because
  # a shorter list is a list that is still all-absent.
  stack <- c("parsnip", "recipes", "workflows", "rsample", "tune",
             "yardstick", "dials", "hardhat")
  absent <- stack[!vapply(stack, requireNamespace, logical(1),
                          quietly = TRUE)]
  missing <- .sdt_model_missing_stack()
  expect_type(missing, "character")
  expect_identical(missing, absent)
  for (pkg in missing) {
    expect_false(requireNamespace(pkg, quietly = TRUE), info = pkg)
  }
})


# Training -----------------------------------------------------------------

test_that("training one engine writes the model and its board into state", {
  lab_needs_engines()
  state <- lab_state()
  state$scenario <- lab_scenario()
  shiny::testServer(mod_model_server, args = list(state = state), {
    session$setInputs(target = "called_triage", engine = "tree", prop = 0.75,
                      folds = 2, tune = FALSE, grid = 2, seed = 1, fit = 1)
    expect_named(state$models, "tree")
    expect_s3_class(state$models$tree, "sdt_model")
    # A single fit has to fill the same leaderboard that comparing every
    # engine fills, or the two buttons would disagree about the same model.
    expect_identical(unique(state$bench$engine), "tree")
    expect_setequal(state$bench$.metric,
                    c("roc_auc", "accuracy", "kap", "f_meas"))

    auc <- state$bench$.estimate[state$bench$.metric == "roc_auc"]
    expect_identical(output$vb_auc, sprintf("%.3f", auc))
    expect_match(lab_html(output$vb_best), "Decision tree")
    expect_match(lab_html(output$vb_best), "best of 1 engine on the board")
    expect_match(lab_html(output$trained_status), "1 model in hand")
  })
})

test_that("a second engine joins the first rather than replacing it", {
  lab_needs_engines()
  skip_if_not_installed("glmnet")
  state <- lab_state()
  state$scenario <- lab_scenario()
  shiny::testServer(mod_model_server, args = list(state = state), {
    session$setInputs(target = "called_triage", engine = "tree", prop = 0.75,
                      folds = 2, tune = FALSE, grid = 2, seed = 1, fit = 1)
    session$setInputs(engine = "glmnet", fit = 2)
    # Two presses are two models in hand, and a board that names both.
    expect_setequal(names(state$models), c("tree", "glmnet"))
    expect_setequal(unique(state$bench$engine), c("tree", "glmnet"))
    expect_match(lab_html(output$trained_status), "2 models in hand")
    expect_match(lab_html(output$vb_best), "best of 2 engines on the board")
  })
})

test_that("comparing all engines fills one row per engine and metric", {
  lab_needs_engines()
  state <- lab_state()
  state$scenario <- lab_scenario()
  shiny::testServer(mod_model_server, args = list(state = state), {
    session$setInputs(target = "called_triage", prop = 0.75, folds = 2,
                      tune = FALSE, grid = 2, seed = 1, bench = 1)
    spec <- sdt_engines()
    usable <- spec$engine[spec$installed]
    expect_setequal(unique(state$bench$engine), usable)
    expect_setequal(names(state$models), usable)
    # Four metrics per engine, every engine scored on the same split: a board
    # missing a row is a like-for-like comparison that is not one.
    expect_identical(nrow(state$bench), 4L * length(usable))
    counts <- vapply(usable, function(e) sum(state$bench$engine == e),
                     integer(1))
    expect_true(all(counts == 4L))

    # The cost of a readable model, on a board that was actually fitted.
    auc <- state$bench[state$bench$.metric == "roc_auc", , drop = FALSE]
    cost_html <- lab_html(output$vb_cost)
    # Readability is graded, so the comparison is the best readable model
    # against the best of everything else, whatever grade that carries.
    if (any(auc$class_box == "glass") && any(auc$class_box != "glass")) {
      cost <- max(auc$.estimate[auc$class_box == "glass"]) -
        max(auc$.estimate[auc$class_box != "glass"])
      # The %+ carries the sign, so matching the whole string asserts it.
      expect_match(cost_html, sprintf("%+.3f", cost), fixed = TRUE)
      expect_false(grepl("\u2014", cost_html, fixed = TRUE))
    } else {
      expect_match(cost_html, "Needs a readable and an unreadable engine")
    }
  })
})

test_that("a fit that cannot run leaves the state as it was", {
  lab_needs_engines()
  co <- lab_cohort()
  # One class in the target: sdt_fit() refuses it, and the panel has to
  # surface that rather than half-write a model into the shared state.
  co$called_triage <- "red"
  state <- lab_state()
  state$scenario <- sdt_scenario(co)
  shiny::testServer(mod_model_server, args = list(state = state), {
    session$setInputs(target = "called_triage", engine = "tree", tune = FALSE,
                      folds = 2, prop = 0.75, seed = 1, fit = 1)
    # The precondition is satisfied here, which is the point: this is the
    # second error path, the one where sdt_fit() itself aborts, and it has to
    # be caught as tidily as the one the guard turns away.
    expect_true(can_train())
    expect_match(lab_html(output$precondition), "Ready: 60 cards")
    expect_null(state$models)
    expect_null(state$bench)
    expect_identical(output$vb_auc, "\u2014")
    # The other button has its own error path, and half-writing a board there
    # would leave the leaderboard describing models nobody has.
    session$setInputs(bench = 1)
    expect_null(state$models)
    expect_null(state$bench)
  })
})

test_that("training settings out of range are floored, not passed on", {
  lab_needs_shiny()
  shiny::testServer(mod_model_server, args = list(state = lab_state()), {
    # The accordion holding these inputs starts collapsed, so the panel has to
    # have a workable set of arguments before any of them is ever touched.
    expect_identical(fit_args()$folds, 5L)
    expect_identical(fit_args()$grid, 5L)
    expect_identical(fit_args()$seed, 1L)
    expect_identical(fit_args()$prop, 0.75)
    expect_identical(fit_args()$target, "called_triage")
    expect_false(fit_args()$tune)

    session$setInputs(folds = 1, grid = 0, seed = 3, prop = 0.6, tune = TRUE,
                      target = "therapy")
    args <- fit_args()
    # rsample cannot make a single fold and dials cannot make an empty grid;
    # both would abort inside sdt_fit() rather than in the panel.
    expect_identical(args$folds, 2L)
    expect_identical(args$grid, 1L)
    expect_identical(args$seed, 3L)
    expect_identical(args$prop, 0.6)
    expect_true(args$tune)
    expect_identical(args$target, "therapy")
  })
})


# Engine chips -------------------------------------------------------------

test_that("the engine chips separate installed engines from missing ones", {
  lab_needs_shiny()
  spec <- .sdt_model_engines()
  # An engine whose package is missing cannot be trained, and that has to be
  # legible beside the picker rather than discovered by pressing Train.
  chosen <- if (any(!spec$installed)) {
    spec$engine[!spec$installed][[1L]]
  } else {
    spec$engine[[1L]]
  }
  shiny::testServer(mod_model_server, args = list(state = lab_state()), {
    session$setInputs(engine = chosen)
    html <- lab_html(output$engine_status)
    for (i in seq_len(nrow(spec))) {
      chip <- as.character(.sdt_chip(
        .sdt_model_engine_label(spec$engine[[i]]),
        if (isTRUE(spec$installed[[i]])) spec$class_box[[i]] else "muted"
      ))
      expect_true(grepl(chip, html, fixed = TRUE), info = spec$engine[[i]])
    }
    row <- match(chosen, spec$engine)
    if (isTRUE(spec$installed[[row]])) {
      expect_match(html, sprintf("%d of %d engines installed",
                                 sum(spec$installed), nrow(spec)))
    } else {
      expect_match(html, sprintf("needs the %s package", spec$package[[row]]))
    }
  })
})


# Value boxes --------------------------------------------------------------

test_that("every value box says something before anything is trained", {
  lab_needs_shiny()
  state <- lab_state()
  state$scenario <- lab_scenario()
  shiny::testServer(mod_model_server, args = list(state = state), {
    # bslib draws a value box with an empty body as a titled grey slab, which
    # reads as broken rather than as waiting.
    expect_identical(output$vb_auc, "\u2014")
    expect_match(lab_html(output$vb_best), "Nothing trained yet")
    expect_match(lab_html(output$vb_cost), "Nothing trained yet")
    expect_match(lab_html(output$trained_status),
                 "Nothing trained in this session yet")
  })
})

test_that("the glass-box cost is the readable AUC less the black-box one", {
  lab_needs_shiny()
  state <- lab_state()
  state$scenario <- lab_scenario()
  state$bench <- lab_board(glass_auc = 0.62, black_auc = 0.71)
  shiny::testServer(mod_model_server, args = list(state = state), {
    # Reading the model costs 0.09 of ROC AUC here, and the box has to say so
    # with a sign: the whole point of the panel is that the number can be
    # negative, and a bare magnitude would hide which way round it went.
    behind <- lab_html(output$vb_cost)
    expect_match(behind, "-0.090", fixed = TRUE)
    expect_false(grepl("\u2014", behind, fixed = TRUE))
    expect_match(behind, "less the best unreadable one")

    state$bench <- lab_board(glass_auc = 0.74, black_auc = 0.71)
    session$flushReact()
    ahead <- lab_html(output$vb_cost)
    expect_match(ahead, "+0.030", fixed = TRUE)
    expect_false(grepl("\u2014", ahead, fixed = TRUE))
  })
})

test_that("the cost box waits until both kinds of engine are on the board", {
  lab_needs_shiny()
  board <- lab_board()
  state <- lab_state()
  state$scenario <- lab_scenario()
  state$bench <- board[board$class_box == "glass", , drop = FALSE]
  shiny::testServer(mod_model_server, args = list(state = state), {
    # max() of an empty vector is -Inf with a warning; the box has to notice
    # that rather than print it.
    html <- lab_html(output$vb_cost)
    expect_match(html, "Needs a readable and an unreadable engine")
    expect_match(html, "\u2014")
    expect_false(grepl("Inf", html, fixed = TRUE))
    # The other two boxes still read off the one-box board.
    expect_identical(output$vb_auc, "0.620")
    expect_match(lab_html(output$vb_best), "Decision tree")
  })
})


# Leaderboard --------------------------------------------------------------

test_that("the leaderboard mounts the richest widget the session can draw", {
  lab_needs_shiny()
  state <- lab_state()
  state$scenario <- lab_scenario()
  state$bench <- lab_board()
  shiny::testServer(mod_model_server, args = list(state = state), {
    expected <- if (requireNamespace("reactable", quietly = TRUE)) {
      "board_reactable"
    } else if (requireNamespace("DT", quietly = TRUE)) {
      "board_dt"
    } else {
      "board_table"
    }
    # The element the UI emits and the renderer kept alive beside it are
    # decided from one value; a session that mounts one and renders the other
    # draws an empty tab, which is what this pins down.
    expect_identical(board_widget, expected)
    expect_true(expected %in% mounted)
    expect_match(lab_html(output$board_ui), session$ns(expected),
                 fixed = TRUE)
    expect_match(lab_html(output$board_ui), "like-for-like comparison")
  })
})

test_that("every leaderboard renderer the session has draws the board", {
  lab_needs_shiny()
  state <- lab_state()
  state$scenario <- lab_scenario()
  state$bench <- lab_board()
  shiny::testServer(mod_model_server, args = list(state = state), {
    # The plain table is always defined: it is the fallback for a session with
    # neither widget package installed.
    plain <- lab_html(output$board_table)
    expect_match(plain, "Decision tree", fixed = TRUE)
    expect_match(plain, "Glass box", fixed = TRUE)
    expect_match(plain, "Cohen's kappa", fixed = TRUE)
    if (have_reactable) {
      rich <- lab_html(output$board_reactable)
      expect_match(rich, "Reactable", fixed = TRUE)
      expect_match(rich, "ROC AUC", fixed = TRUE)
    }
    if (have_dt) {
      # DT takes the chip as markup and escapes everything else.
      expect_match(lab_html(output$board_dt), "sdt-chip", fixed = TRUE)
    }
  })
})

test_that("a board carrying no ROC AUC still draws, and says so", {
  lab_needs_shiny()
  board <- lab_board()
  state <- lab_state()
  state$scenario <- lab_scenario()
  state$bench <- board[board$.metric != "roc_auc", , drop = FALSE]
  shiny::testServer(mod_model_server, args = list(state = state), {
    # reactable aborts on a defaultSorted naming a column that is not there,
    # and the value boxes rank on a metric the board no longer carries, so
    # every one of them has to cope with a board ranked on nothing.
    expect_false("roc_auc" %in% names(board_wide()))
    expect_identical(output$vb_auc, "\u2014")
    expect_match(lab_html(output$vb_best), "Nothing trained yet")
    expect_match(lab_html(output$vb_cost),
                 "Needs a readable and an unreadable engine")
    expect_match(lab_html(output$board_table), "Accuracy", fixed = TRUE)
    if (have_reactable) {
      expect_match(lab_html(output$board_reactable), "Reactable",
                   fixed = TRUE)
    }
  })
})

test_that("the empty tabs say whether a scenario or a model is missing", {
  lab_needs_shiny()
  state <- lab_state()
  state$scenario <- lab_scenario()
  shiny::testServer(mod_model_server, args = list(state = state), {
    # A panel that renders blank leaves the reader to guess whether the app is
    # broken, and the two reasons it can be empty need different answers.
    expect_match(lab_html(output$board_ui), "No leaderboard yet")
    expect_match(lab_html(output$roc_ui), "No curve to draw yet")
    expect_match(lab_html(output$confusion_ui),
                 "No model to score against the truth yet")
    expect_match(lab_html(output$board_ui), "Press Train this engine")
  })
})


# Curves and agreement -----------------------------------------------------

test_that("the ROC and confusion tabs mount once a model is in hand", {
  lab_needs_engines()
  state <- lab_state()
  state$scenario <- lab_scenario()
  state$models <- list(tree = lab_model())
  shiny::testServer(mod_model_server, args = list(state = state), {
    session$setInputs(roc_model = "tree")
    expected <- if (requireNamespace("plotly", quietly = TRUE)) {
      "roc_plotly"
    } else {
      "roc_static"
    }
    expect_identical(roc_widget, expected)
    expect_match(lab_html(output$roc_ui), session$ns(expected), fixed = TRUE)
    # The plot output itself cannot be read back in testServer; the reactive
    # that feeds it can.
    expect_s3_class(selected_model(), "sdt_model")

    expect_match(lab_html(output$confusion_ui), "Rows are what was recorded")
    conf <- lab_html(output$confusion)
    expect_match(conf, "Red (SK I)", fixed = TRUE)
    expect_match(conf, "Predicted: Yellow (SK II)", fixed = TRUE)
    expect_match(conf, "Cases", fixed = TRUE)
  })
})

test_that("the inspector falls back when its selection is not trained", {
  lab_needs_engines()
  state <- lab_state()
  state$scenario <- lab_scenario()
  state$models <- list(tree = lab_model())
  shiny::testServer(mod_model_server, args = list(state = state), {
    # The picker is refilled whenever the models change, so between the fit
    # and the update it can be holding an engine that is not in hand.
    session$setInputs(roc_model = "ranger")
    expect_identical(selected_model()$engine, "tree")
    session$setInputs(roc_model = "")
    expect_identical(selected_model()$engine, "tree")
    session$setInputs(roc_model = "tree")
    expect_identical(selected_model()$engine, "tree")
  })
})

test_that("clearing the models empties the inspector", {
  lab_needs_shiny()
  state <- lab_state()
  # Building a new scenario clears the models, and the picker has to follow it
  # down rather than keep offering an engine that no longer exists.
  state$models <- list(
    tree = structure(list(engine = "tree", class_box = "glass"),
                     class = "sdt_model"),
    ranger = structure(list(engine = "ranger", class_box = "black"),
                       class = "sdt_model")
  )
  shiny::testServer(mod_model_server, args = list(state = state), {
    expect_match(lab_html(output$trained_status), "2 models in hand")
    state$models <- NULL
    session$flushReact()
    expect_match(lab_html(output$trained_status),
                 "Nothing trained in this session yet")
    expect_match(lab_html(output$roc_ui), "Build a scenario first")
  })
})

test_that("the ROC plot says which engine drew it, in the panel's words", {
  lab_needs_engines()
  # sdt_roc() subtitles the plot with the raw engine and column names, which
  # are the one thing nothing on this page is allowed to say.
  plot <- .sdt_model_roc_plot(lab_model())
  expect_s3_class(plot, "ggplot")
  expect_match(plot$labels$subtitle, "Decision tree", fixed = TRUE)
  expect_match(plot$labels$subtitle, "glass box", fixed = TRUE)
  expect_match(plot$labels$subtitle, "participant's called category",
               fixed = TRUE)
})


# Vocabulary ---------------------------------------------------------------

test_that("the vocabulary leaves anything it does not know alone", {
  expect_identical(.sdt_model_engine_label("tree"), "Decision tree")
  expect_identical(.sdt_model_engine_label("telepathy"), "telepathy")
  expect_identical(.sdt_model_box_label(c("glass", "black")),
                   c("Glass box", "Black box"))
  expect_identical(.sdt_model_metric_label("kap"), "Cohen's kappa")
  expect_identical(.sdt_model_target_label("called_therapy"),
                   "participant's called therapy")
  expect_identical(.sdt_model_class_label("none"), "No operation")
  # ifelse() over an empty test returns logical(0), which would hand the page
  # a vector of the wrong type where it expects words.
  expect_identical(.sdt_model_engine_label(character()), character())
})

test_that("a list of packages is written as a sentence, whatever its length", {
  expect_identical(.sdt_model_and(character()), "")
  expect_identical(.sdt_model_and("rpart"), "rpart")
  expect_identical(.sdt_model_and(c("rpart", "glmnet")), "rpart and glmnet")
  expect_identical(.sdt_model_and(c("a", "b", "c")), "a, b and c")
})

test_that("classes are ordered as the room orders them, not alphabetically", {
  # A confusion matrix read alphabetically puts green above red, which inverts
  # the one ordering everybody in the room shares.
  expect_identical(.sdt_model_class_order(c("green", "yellow", "red")),
                   c("red", "yellow", "green"))
  expect_identical(.sdt_model_class_order(c("green", "banana")),
                   c("green", "banana"))
})

test_that("the engine picker offers every engine, glass box first", {
  choices <- .sdt_model_engine_choices()
  expect_setequal(unname(choices), sdt_engines()$engine)
  # A readable model is the right default for this bench, and the cheapest.
  expect_identical(unname(choices[[1L]]), "tree")
  expect_match(names(choices)[[1L]], "glass-box", fixed = TRUE)
})

test_that("a metric bar degrades to an em dash rather than a broken bar", {
  expect_match(as.character(.sdt_model_bar_cell(NA_real_)), "\u2014")
  expect_match(as.character(.sdt_model_bar_cell(numeric())), "\u2014")
  expect_match(as.character(.sdt_model_bar_cell("0.5")), "\u2014")
  expect_match(as.character(.sdt_model_bar_cell(0.5)), "0.500", fixed = TRUE)
  # A metric outside [0, 1] is clamped rather than drawn past its track.
  expect_match(as.character(.sdt_model_bar_cell(2)), "width: 100.0%",
               fixed = TRUE)
})

test_that("the leaderboard is one row per engine, ranked on ROC AUC", {
  wide <- .sdt_model_board_wide(lab_board())
  expect_identical(nrow(wide), 2L)
  # ROC AUC first because that is what the table is ranked on, and the board
  # arrives one row per engine and metric rather than one row per engine.
  expect_identical(names(wide), c("engine", "class_box", "roc_auc",
                                  "accuracy", "kap", "f_meas"))
  expect_identical(wide$engine[[1L]], "ranger")
})

test_that("the fallback tables carry the chip only where it can be markup", {
  wide <- .sdt_model_board_wide(lab_board())
  html <- .sdt_model_board_display(wide, html = TRUE)
  plain <- .sdt_model_board_display(wide, html = FALSE)
  expect_named(html, c("Engine", "Model class", "ROC AUC", "Accuracy",
                       "Cohen's kappa", "F-measure"))
  expect_match(html$`Model class`[[1L]], "sdt-chip", fixed = TRUE)
  # renderTable() escapes what it is given, so the plain fallback has to be
  # handed words rather than a span it would print at the reader.
  expect_identical(plain$`Model class`, c("Black box", "Glass box"))
  expect_identical(plain$Engine, c("Random forest", "Decision tree"))
})

test_that("a board built from no models at all is no board", {
  expect_null(.sdt_board_from_models(list()))
  expect_null(.sdt_board_from_models(NULL))
})

test_that("the confusion matrix labels both axes and counts each row", {
  out <- .sdt_model_confusion(c("red", "red", "yellow"),
                              c("red", "green", "yellow"))
  expect_identical(out$Observed,
                   c("Red (SK I)", "Yellow (SK II)", "Green (SK III)"))
  expect_identical(out$Cases, c(2L, 1L, 0L))
  expect_identical(out$Correct[[1L]], "50%")
  # A class nobody was recorded as has no accuracy to report, and 0 / 0 would
  # otherwise reach the page as NaN%.
  expect_identical(out$Correct[[3L]], "\u2014")
})
