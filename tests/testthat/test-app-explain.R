# The Explain panel, driven headlessly, and the explanations behind it.
#
# This is the one panel that interrogates a model rather than scoring it, so
# the failures worth ruling out are all failures of explanation: a picker that
# offers row numbers instead of casualties, a side-by-side comparison that
# quietly shows a single model, a value box that renders as a titled grey
# slab, and an optional package that empties the panel without saying which
# one is missing.

skip_if_no_shiny <- function() {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("htmltools")
}

# The explanation backend is optional, and every view has to survive without
# it, so the tests that need it past the guard say so.
skip_if_no_dalex <- function() {
  skip_if_no_shiny()
  skip_if_not_installed("DALEX")
  skip_if_not_installed("DALEXtra")
}

# So is every piece of tidymodels needed to have a model worth explaining in
# the first place.
skip_if_no_explain <- function() {
  skip_if_no_dalex()
  for (pkg in c("parsnip", "recipes", "workflows", "rsample", "tune",
                "yardstick", "dials", "hardhat", "rpart", "ranger")) {
    skip_if_not_installed(pkg)
  }
}

# A reactiveValues store standing in for the app's shared state.
fake_state <- function(...) {
  shiny::reactiveValues(scenario = NULL, run = NULL, models = NULL,
                        bench = NULL, ...)
}

demo_scenario <- function(n = 12) {
  co <- sdt_add_decisions(sdt_synthetic_cohort(n = n, seed = 1), seed = 2)
  sdt_scenario(co, resources = list(or = 2, icu_imc = 4, ward = 20))
}

# The panel's furniture -- pickers, headers, value boxes, empty states --
# reads four fields off a model and nothing else. Standing in for the fitted
# object here is what keeps those views testable on a machine with no
# modelling stack installed, which is exactly the machine they exist for.
stub_model <- function(engine, box) {
  list(engine = engine, class_box = box, target = "called_triage",
       predictors = c("age", "life_points", "ventilated"))
}

stub_models <- function() {
  list(tree = stub_model("tree", "glass"),
       ranger = stub_model("ranger", "black"))
}

# Fitting is the only slow thing in this file, so the pair is fitted once and
# read by every test that needs a real explainer. Two engines, deliberately:
# the point of the panel is the glass-box against black-box comparison, and
# one model cannot show it.
fitted_cache <- new.env(parent = emptyenv())

real_models <- function() {
  if (is.null(fitted_cache$models)) {
    co <- sdt_add_decisions(sdt_synthetic_cohort(n = 120, seed = 3), seed = 4)
    fit_one <- function(engine) {
      sdt_fit(co, target = "called_triage", engine = engine, tune = FALSE,
              folds = 2L, prop = 0.6, seed = 1, verbose = FALSE)
    }
    fitted_cache$models <- list(tree = fit_one("tree"),
                                ranger = fit_one("ranger"))
  }
  fitted_cache$models
}

# testServer hands a renderUI() back as html plus its dependencies, and only
# the html is worth asserting on. A render that returned NULL gives "".
ui_html <- function(x) {
  html <- if (length(x)) x[["html"]] else NULL
  if (is.null(html)) "" else as.character(html)
}

# A plot render comes back as a base64 PNG, and that string is the only
# evidence that anything was drawn: a render that produced an empty device
# still returns cleanly, so "did not error" is not an assertion.
plot_src <- function(x) {
  src <- if (is.list(x)) x[["src"]] else NULL
  if (is.null(src)) "" else as.character(src)
}

# What an empty value box shows. Built rather than typed, so this file stays
# ASCII: a stray non-ASCII byte in a package's own sources is a portability
# note waiting to happen.
em_dash <- intToUtf8(8212L)


# Human labels -----------------------------------------------------------------

test_that("the casualty picker names a casualty rather than a row", {
  co <- tibble::as_tibble(sdt_synthetic_cohort(n = 6, seed = 1))
  choices <- .sdt_explain_casualty_choices(co)

  # The value is the id, because that is what the attribution is looked up
  # by; the label is the card, because choosing between "P007" and "P012" is
  # guesswork and choosing between two casualties is a decision.
  expect_identical(unname(choices), as.character(co$id))
  for (i in seq_along(choices)) {
    label <- names(choices)[[i]]
    expect_match(label, as.character(co$id[[i]]), fixed = TRUE)
    expect_match(label, format(co$age[[i]]), fixed = TRUE)
    expect_match(label, "triage ", fixed = TRUE)
    expect_match(label, as.character(co$triage[[i]]), fixed = TRUE)
    expect_match(
      label,
      .sdt_explain_relabel(co$sex[[i]], .sdt_explain_sex_labels()),
      fixed = TRUE
    )
  }
  # A bare row number is the failure this replaces.
  expect_false(any(names(choices) == as.character(seq_along(choices))))
})

test_that("an empty cohort empties the picker instead of erroring", {
  co <- tibble::as_tibble(sdt_synthetic_cohort(n = 4, seed = 1))
  expect_identical(.sdt_explain_casualty_choices(co[0, ]), character())
  expect_identical(.sdt_explain_casualty_choices(NULL), character())
})

test_that("an unfamiliar value is left as it came in, not blanked", {
  # A cohort can be loaded from a file, and an empty cell on screen is worse
  # than an unfamiliar word.
  sexes <- .sdt_explain_relabel(c("f", "m", "d", "x"),
                                .sdt_explain_sex_labels())
  expect_identical(sexes, c("female", "male", "diverse", "x"))
  expect_identical(
    .sdt_explain_relabel("none", .sdt_explain_therapy_labels()),
    "no operation"
  )
  expect_identical(
    .sdt_explain_relabel("extremity", .sdt_explain_region_labels()),
    "extremity"
  )
})

test_that("a predictor is spelled as words even when it is unlisted", {
  expect_identical(.sdt_explain_label_field("life_points"), "Life points")
  expect_identical(.sdt_explain_label_field("triage"),
                   "Category on the card")
  # Predictors are not a closed vocabulary: a loaded cohort may carry a
  # column this package has never seen, and a snake_case name on screen tells
  # a clinician nothing.
  expect_identical(.sdt_explain_label_field("shock_index"), "Shock index")
})

test_that("the model picker says which models can be read", {
  choices <- .sdt_explain_model_choices(stub_models())
  expect_identical(unname(choices), c("tree", "ranger"))
  expect_identical(names(choices), c("tree (glass-box)", "ranger (black-box)"))
  expect_identical(.sdt_explain_model_choices(list()), character())
})

test_that("the offered contrast is a model of the other kind", {
  models <- stub_models()
  expect_identical(.sdt_explain_contrast(models, "tree"), "ranger")
  expect_identical(.sdt_explain_contrast(models, "ranger"), "tree")
  # Two models of the same kind show nothing about the price of readability,
  # so none is offered.
  same <- list(tree = stub_model("tree", "glass"),
               glmnet = stub_model("glmnet", "glass"))
  expect_null(.sdt_explain_contrast(same, "tree"))
  expect_null(.sdt_explain_contrast(models["tree"], "tree"))
  expect_null(.sdt_explain_contrast(models, "nothing"))
})


# The optional backend ---------------------------------------------------------

test_that("the panel's guard names the package it cannot find", {
  skip_if_no_shiny()
  # DALEX cannot be uninstalled inside a test run, so the guard every view
  # goes through is driven with a package that certainly is not there. What
  # matters is the sentence: a panel that empties itself without saying which
  # package is missing reads as a broken app.
  told <- tryCatch(
    .sdt_need_pkgs(c("notarealpackage.sdt"), "Model explanations"),
    error = conditionMessage
  )
  expect_type(told, "character")
  expect_match(told, "notarealpackage.sdt", fixed = TRUE)
  expect_match(told, "Model explanations", fixed = TRUE)
  expect_match(told, "install.packages", fixed = TRUE)
})

test_that("the exported explanations name the package they need", {
  # The same requirement, on the other side of the app boundary:
  # sdt_explain() and friends are callable from a script, where a validate()
  # message would never be seen.
  told <- tryCatch(
    .require_pkgs(c("notarealpackage.sdt")),
    error = conditionMessage
  )
  expect_type(told, "character")
  expect_match(told, "notarealpackage.sdt", fixed = TRUE)
  expect_match(told, "install.packages", fixed = TRUE)
})

test_that("the hint is read off the guard rather than restated", {
  skip_if_no_dalex()
  # With the backend installed there is nothing standing in the way.
  expect_null(.sdt_explain_missing_hint())

  # And when it is absent the sentence shown is the guard's own, character
  # for character. Asserting merely that it mentions DALEX would pass just as
  # happily on a second copy of the message written out in the panel, which is
  # the drift this exists to rule out. DALEX cannot be uninstalled mid-run, so
  # the guard is stood in for.
  said <- "Model explanations need the DALEX package. Install DALEX."
  local_mocked_bindings(.sdt_need_pkgs = function(pkgs, what) {
    shiny::validate(shiny::need(FALSE, said))
  })
  expect_identical(.sdt_explain_missing_hint(), said)
})

test_that("a missing backend empties every pane and names the package", {
  skip_if_no_shiny()
  # The failure worth ruling out is a panel that goes quietly blank when an
  # optional package is absent: all four views refuse in the same words, and
  # the sidebar says so too, so nobody is left deciding whether the app broke.
  said <- "Model explanations need the DALEX package. Install DALEX."
  local_mocked_bindings(.sdt_need_pkgs = function(pkgs, what) {
    shiny::validate(shiny::need(FALSE, said))
  })
  state <- fake_state()
  state$scenario <- demo_scenario()
  state$models <- stub_models()
  shiny::testServer(mod_explain_server, args = list(state = state), {
    session$setInputs(model = "tree", compare = "ranger", permutations = 2,
                      profile_type = "partial", casualty = "",
                      variable = "age")
    for (pane in c("importance_ui", "importance_compare_ui", "profile_ui",
                   "shap_ui")) {
      html <- ui_html(output[[pane]])
      expect_match(html, "Explanations need one more package.", fixed = TRUE)
      expect_match(html, said, fixed = TRUE)
      # And the right refusal: a model exists and is chosen, so blaming the
      # missing model would send the reader to the wrong tab.
      expect_false(grepl("No model", html, fixed = TRUE))
    }
    expect_match(ui_html(output$status), "DALEX missing", fixed = TRUE)
    # The model box reports on models rather than on packages, so it still
    # has an answer here.
    expect_match(ui_html(output$vb_model), "glass-box", fixed = TRUE)
  })
})

test_that("the backend box reports the version it is explaining with", {
  skip_if_no_dalex()
  html <- as.character(
    htmltools::renderTags(.sdt_explain_backend_metric())$html
  )
  expect_match(html, "DALEX", fixed = TRUE)
  expect_match(html, format(utils::packageVersion("DALEX")), fixed = TRUE)
})


# The headline feature ---------------------------------------------------------

test_that("the headline feature ignores DALEX's own bookkeeping rows", {
  # `_full_model_` carries the largest loss of all and is not a predictor;
  # reading it out would name the model as its own most important feature.
  plot <- list(data = data.frame(
    variable = c("_full_model_", "life_points", "age", "_baseline_"),
    dropout_loss = c(9, 2, 1, 8)
  ))
  expect_identical(.sdt_explain_top_feature(plot), "life_points")
})

test_that("an unrecognised importance shape yields no headline", {
  # ingredients has carried the loss column under more than one name across
  # versions and guarantees neither, so a shape we do not know has to give a
  # placeholder rather than a wrong answer.
  expect_null(.sdt_explain_top_feature(NULL))
  expect_null(.sdt_explain_top_feature(list(data = data.frame(x = 1))))
  expect_null(.sdt_explain_top_feature(list(data = "not a data frame")))
  expect_null(.sdt_explain_top_feature(list(data = data.frame(
    variable = "age", dropout_loss = "big"
  ))))
  expect_null(.sdt_explain_top_feature(list(data = data.frame(
    variable = "_full_model_", dropout_loss = 1
  ))))
  # The other spelling of the loss column is understood.
  expect_identical(
    .sdt_explain_top_feature(list(data = data.frame(
      variable = c("age", "sex"), dropout_loss.x = c(1, 4)
    ))),
    "sex"
  )
})


# Panel furniture --------------------------------------------------------------

test_that("the attribution card reads as a card, not as a row of data", {
  skip_if_no_shiny()
  co <- tibble::as_tibble(sdt_synthetic_cohort(n = 8, seed = 1))
  row <- co[co$ventilated, ][1L, , drop = FALSE]
  html <- as.character(htmltools::renderTags(.sdt_explain_card(row))$html)

  expect_match(html, as.character(row$id), fixed = TRUE)
  expect_match(html, "years", fixed = TRUE)
  expect_match(html, "life points", fixed = TRUE)
  expect_match(html, ", ventilated", fixed = TRUE)
  expect_match(html, paste0("sdt-chip-", row$triage), fixed = TRUE)
  # Nothing on this panel may show a column name.
  expect_false(grepl("life_points", html, fixed = TRUE))
  expect_false(grepl("injury_region", html, fixed = TRUE))
})

test_that("a triage value off the card still gets a chip", {
  skip_if_no_shiny()
  row <- tibble::as_tibble(sdt_synthetic_cohort(n = 4, seed = 1))[1L, ]
  row$triage <- "unknown"
  html <- as.character(htmltools::renderTags(.sdt_explain_card(row))$html)
  # A loaded cohort can carry a category this package does not know, and a
  # chip with no colour class at all would render as unstyled text.
  expect_match(html, "sdt-chip-muted", fixed = TRUE)
  expect_match(html, "triage unknown", fixed = TRUE)
})

test_that("the guide stops talking once there is nothing left to do", {
  skip_if_no_shiny()
  models <- stub_models()
  expect_match(
    as.character(htmltools::renderTags(
      .sdt_explain_guide(NULL, NULL, NULL)
    )$html),
    "Model lab", fixed = TRUE
  )
  expect_match(
    as.character(htmltools::renderTags(
      .sdt_explain_guide(models, NULL, NULL)
    )$html),
    "Choose the model to explain", fixed = TRUE
  )
  expect_match(
    as.character(htmltools::renderTags(
      .sdt_explain_guide(models, models$tree, NULL)
    )$html),
    "Compare against", fixed = TRUE
  )
  # A permanent instruction bar is a row of pixels the reader learns to skip.
  expect_null(.sdt_explain_guide(models, models$tree, models$ranger))
})


# The module: an empty session -------------------------------------------------

test_that("the value boxes emit an em-dash before a model exists", {
  skip_if_no_shiny()
  state <- fake_state()
  state$scenario <- demo_scenario()
  shiny::testServer(mod_explain_server, args = list(state = state), {
    session$setInputs(model = "", compare = "", permutations = 10,
                      profile_type = "partial", casualty = "", variable = "")
    # bslib renders a value box with a NULL body as a titled grey slab, which
    # reads as broken rather than as waiting for a model. Every box therefore
    # emits something, and says why it is empty.
    expect_match(ui_html(output$vb_model), em_dash, fixed = TRUE)
    expect_match(ui_html(output$vb_model), "Model lab", fixed = TRUE)
    expect_match(ui_html(output$vb_top), em_dash, fixed = TRUE)
    expect_match(ui_html(output$vb_top), "Appears with the importance plot",
                 fixed = TRUE)
    # The third box reports on packages rather than on models, so it has an
    # answer even here.
    expect_match(ui_html(output$vb_backend), "DALEX", fixed = TRUE)
  })
})

test_that("every pane says a model is missing and where to train one", {
  skip_if_no_shiny()
  state <- fake_state()
  state$scenario <- demo_scenario()
  shiny::testServer(mod_explain_server, args = list(state = state), {
    session$setInputs(model = "", compare = "", permutations = 10,
                      profile_type = "partial", casualty = "", variable = "")
    # A pane that renders blank leaves the user to guess whether the app is
    # broken, and they usually guess wrong. All four refuse for the same
    # reason, in the same words.
    for (pane in c("importance_ui", "importance_compare_ui", "profile_ui",
                   "shap_ui")) {
      html <- ui_html(output[[pane]])
      expect_match(html, "No model has been trained yet", fixed = TRUE)
      expect_match(html, "Model lab", fixed = TRUE)
    }
    expect_match(ui_html(output$guide), "Model lab", fixed = TRUE)
    expect_match(ui_html(output$status), "no models yet", fixed = TRUE)
    expect_match(ui_html(output$head_primary), "Explaining", fixed = TRUE)
    expect_match(ui_html(output$head_compare), "Compared with", fixed = TRUE)
    # A header with no model must not claim one.
    expect_false(grepl("-box", ui_html(output$head_primary), fixed = TRUE))
  })
})

test_that("a casualty is only asked for once there is a card to ask about", {
  skip_if_no_shiny()
  state <- fake_state()
  shiny::testServer(mod_explain_server, args = list(state = state), {
    session$setInputs(model = "", compare = "", permutations = 10,
                      profile_type = "partial", casualty = "", variable = "")
    # With no scenario the card halts rather than rendering a row of NAs.
    # The pane beside it is what explains the absence.
    expect_error(output$casualty_card, class = "shiny.silent.error")
    expect_match(ui_html(output$shap_ui), "No model has been trained yet",
                 fixed = TRUE)
  })
})

test_that("a cleared permutation box does not reach the explainer", {
  skip_if_no_shiny()
  state <- fake_state()
  shiny::testServer(mod_explain_server, args = list(state = state), {
    session$setInputs(model = "", compare = "", profile_type = "partial",
                      casualty = "", variable = "", permutations = 4)
    expect_identical(permutations(), 4L)
    # A numericInput hands back whatever was typed, including NA from a
    # cleared box, and this goes straight into a permutation count.
    session$setInputs(permutations = NA)
    expect_identical(permutations(), 10L)
    session$setInputs(permutations = 1)
    expect_identical(permutations(), 2L)
    session$setInputs(permutations = 5000)
    expect_identical(permutations(), 50L)
  })
})


# The module: models in the session --------------------------------------------

test_that("the side-by-side comparison says it needs a second model", {
  skip_if_no_dalex()
  state <- fake_state()
  state$scenario <- demo_scenario()
  state$models <- stub_models()["tree"]
  shiny::testServer(mod_explain_server, args = list(state = state), {
    session$setInputs(model = "tree", compare = "", permutations = 2,
                      profile_type = "partial", casualty = "", variable = "")
    # Putting a glass-box ordering next to a black-box one is the point of
    # the panel; with one model there is nothing to put next to it, and the
    # pane has to say so rather than repeat the first plot or go blank.
    html <- ui_html(output$importance_compare_ui)
    expect_match(html, "No second model yet", fixed = TRUE)
    expect_match(html, "Compare against", fixed = TRUE)
    expect_match(html, "a model you can read", fixed = TRUE)
    expect_match(ui_html(output$head_compare), "Compared with", fixed = TRUE)
    # The primary side is unaffected: one model is enough to explain one.
    expect_match(ui_html(output$importance_ui), "shiny-plot-output",
                 fixed = TRUE)
    expect_match(ui_html(output$guide), "Compare against", fixed = TRUE)
  })
})

test_that("a session with models but none chosen asks for a choice", {
  skip_if_no_dalex()
  state <- fake_state()
  state$scenario <- demo_scenario()
  state$models <- stub_models()
  shiny::testServer(mod_explain_server, args = list(state = state), {
    session$setInputs(model = "", compare = "", permutations = 2,
                      profile_type = "partial", casualty = "", variable = "")
    expect_match(ui_html(output$importance_ui), "No model chosen",
                 fixed = TRUE)
    expect_match(ui_html(output$guide), "Choose the model to explain",
                 fixed = TRUE)
    expect_match(ui_html(output$vb_model), em_dash, fixed = TRUE)
    # A model named in the session but not chosen is still worth reporting.
    expect_match(ui_html(output$status), "tree", fixed = TRUE)
    expect_match(ui_html(output$status), "ranger", fixed = TRUE)
    expect_match(ui_html(output$status), "DALEX ready", fixed = TRUE)
  })
})

test_that("the panes ask for the one thing each still needs", {
  skip_if_no_dalex()
  state <- fake_state()
  state$models <- stub_models()
  shiny::testServer(mod_explain_server, args = list(state = state), {
    session$setInputs(model = "tree", compare = "ranger", permutations = 2,
                      profile_type = "partial", casualty = "", variable = "")
    expect_match(ui_html(output$profile_ui), "No predictor chosen",
                 fixed = TRUE)
    # An attribution needs the casualty it is explaining, and this session
    # has no scenario at all.
    expect_match(ui_html(output$shap_ui), "No scenario in the session",
                 fixed = TRUE)
    expect_match(ui_html(output$shap_ui), "Scenario tab", fixed = TRUE)

    state$scenario <- demo_scenario()
    session$flushReact()
    expect_match(ui_html(output$shap_ui), "No casualty chosen", fixed = TRUE)

    session$setInputs(variable = "life_points")
    expect_match(ui_html(output$profile_ui), "shiny-plot-output",
                 fixed = TRUE)
    # Both models chosen, so the instruction bar has nothing left to say.
    expect_null(output$guide)
  })
})

test_that("the headers name both models and their kinds", {
  skip_if_no_dalex()
  state <- fake_state()
  state$scenario <- demo_scenario()
  state$models <- stub_models()
  shiny::testServer(mod_explain_server, args = list(state = state), {
    session$setInputs(model = "tree", compare = "ranger", permutations = 2,
                      profile_type = "partial", casualty = "", variable = "")
    primary <- ui_html(output$head_primary)
    compare <- ui_html(output$head_compare)
    expect_match(primary, "Explaining: tree", fixed = TRUE)
    expect_match(primary, "glass-box", fixed = TRUE)
    expect_match(compare, "Compared with: ranger", fixed = TRUE)
    expect_match(compare, "black-box", fixed = TRUE)
    # The value box spells the target rather than naming its column.
    expect_match(ui_html(output$vb_model), "glass-box, predicting called",
                 fixed = TRUE)
    expect_false(grepl("called_triage", ui_html(output$vb_model),
                       fixed = TRUE))
  })
})

test_that("a model that has gone leaves nothing selected behind it", {
  skip_if_no_dalex()
  state <- fake_state()
  state$scenario <- demo_scenario()
  state$models <- stub_models()
  shiny::testServer(mod_explain_server, args = list(state = state), {
    session$setInputs(model = "tree", compare = "ranger", permutations = 2,
                      profile_type = "partial", casualty = "", variable = "")
    expect_match(ui_html(output$head_primary), "tree", fixed = TRUE)
    # Building a new scenario clears the models. Every view has to fall back
    # to its empty state rather than keep explaining a model that is gone.
    state$models <- NULL
    session$flushReact()
    expect_match(ui_html(output$importance_ui),
                 "No model has been trained yet", fixed = TRUE)
    expect_match(ui_html(output$vb_model), em_dash, fixed = TRUE)
    expect_match(ui_html(output$status), "no models yet", fixed = TRUE)
  })
})

test_that("choosing a casualty shows the card being attributed", {
  skip_if_no_dalex()
  state <- fake_state()
  state$scenario <- demo_scenario()
  state$models <- stub_models()
  shiny::testServer(mod_explain_server, args = list(state = state), {
    id <- as.character(state$scenario$cohort$id[[2L]])
    session$setInputs(model = "tree", compare = "ranger", permutations = 2,
                      profile_type = "partial", variable = "age",
                      casualty = id)
    card <- ui_html(output$casualty_card)
    expect_match(card, id, fixed = TRUE)
    expect_match(card, "years", fixed = TRUE)
    expect_match(ui_html(output$shap_ui), "shiny-plot-output", fixed = TRUE)

    # An id that is not in this cohort is not an error: the scenario can be
    # rebuilt under a selection that was valid a moment ago.
    session$setInputs(casualty = "nobody")
    expect_identical(ui_html(output$casualty_card), "")
  })
})

test_that("the pickers offer readable choices and a contrasting model", {
  skip_if_no_shiny()
  # updateSelectInput talks to a browser that a headless session does not
  # have, so what it sends is captured here. Without this the observers that
  # fill the four pickers are unobservable, and a picker still offering a
  # model that no longer exists is the failure they are there to prevent.
  calls <- new.env(parent = emptyenv())
  calls$log <- list()
  local_mocked_bindings(
    updateSelectInput = function(session, input_id, label = NULL,
                                 choices = NULL, selected = NULL, ...) {
      calls$log[[length(calls$log) + 1L]] <- list(
        id = input_id, choices = choices, selected = selected
      )
      invisible(NULL)
    },
    .package = "shiny"
  )
  last_call <- function(id) {
    hits <- Filter(function(call) identical(call$id, id), calls$log)
    if (!length(hits)) NULL else hits[[length(hits)]]
  }

  state <- fake_state()
  state$scenario <- demo_scenario(n = 6)
  state$models <- stub_models()
  shiny::testServer(mod_explain_server, args = list(state = state), {
    session$setInputs(model = "tree", compare = "", permutations = 2,
                      profile_type = "partial", casualty = "", variable = "")
    cohort <- state$scenario$cohort

    casualty <- last_call("casualty")
    expect_identical(unname(casualty$choices), as.character(cohort$id))
    expect_match(names(casualty$choices)[[1L]],
                 as.character(cohort$id[[1L]]), fixed = TRUE)
    expect_match(names(casualty$choices)[[1L]], "triage ", fixed = TRUE)

    expect_match(names(last_call("model")$choices), "-box\\)$", all = TRUE)
    # The predictor picker is spelled for a reader, not for the data frame.
    expect_true("Life points" %in% names(last_call("variable")$choices))

    # The most useful second model is one of the other kind, and offering it
    # is the difference between a comparison that exists and one that is seen.
    offered <- vapply(calls$log, function(call) {
      identical(call$id, "compare") && identical(call$selected, "ranger")
    }, logical(1))
    expect_true(any(offered))
  })
})


# The exported explanations ----------------------------------------------------

test_that("sdt_explain() wraps a fitted model over its held-out split", {
  skip_if_no_explain()
  fit <- real_models()$tree
  explainer <- sdt_explain(fit)
  expect_s3_class(explainer, "explainer")
  expect_setequal(names(explainer$data), fit$predictors)
  expect_identical(explainer$label, "tree (glass-box)")
})

test_that("sdt_explain() says what it needs when the outcome is absent", {
  skip_if_no_explain()
  fit <- real_models()$tree
  held <- tibble::as_tibble(rsample::testing(fit$split))
  without <- held[, setdiff(names(held), fit$target), drop = FALSE]
  expect_error(sdt_explain(fit, data = without), "called_triage")
  # Passing the outcome separately is the documented way out.
  expect_s3_class(
    sdt_explain(fit, data = without, y = held[[fit$target]]),
    "explainer"
  )
})

test_that("every explanation entry point returns a plot", {
  skip_if_no_explain()
  fit <- real_models()$tree
  explainer <- sdt_explain(fit)
  held <- tibble::as_tibble(rsample::testing(fit$split))

  importance <- sdt_importance(explainer, n_permutations = 2L)
  expect_s3_class(importance, "ggplot")
  expect_identical(importance$labels$title,
                   "Permutation variable importance")

  profile <- sdt_profile(explainer, variables = "life_points")
  expect_s3_class(profile, "ggplot")
  expect_identical(profile$labels$title, "Partial dependence")

  shap <- sdt_shap(explainer, held$id[[1L]], data = held,
                   n_permutations = 2L)
  expect_s3_class(shap, "ggplot")
  expect_match(shap$labels$subtitle, "contribution")
})

test_that("the profile is titled for the method that produced it", {
  skip_if_no_explain()
  # Accumulated local effects are the more trustworthy of the two when
  # predictors are correlated, which on a casualty card they are, so which
  # one is on screen has to be readable off the plot.
  profile <- sdt_profile(sdt_explain(real_models()$tree),
                         variables = "age", type = "accumulated")
  expect_identical(profile$labels$title, "Accumulated local effects")
})

test_that("the headline feature is read off a real importance plot", {
  skip_if_no_explain()
  # The extraction has to survive whatever ingredients calls its loss column,
  # which is the reason the panel reads the headline off the plot it already
  # has rather than permuting a second time for one word.
  fit <- real_models()$tree
  top <- .sdt_explain_top_feature(
    sdt_importance(sdt_explain(fit), n_permutations = 2L)
  )
  expect_true(top %in% fit$predictors)
})


# The module, with real models -------------------------------------------------

test_that("a trained pair resolves every pane to a plot", {
  skip_if_no_explain()
  state <- fake_state()
  state$scenario <- demo_scenario()
  state$models <- real_models()
  shiny::testServer(mod_explain_server, args = list(state = state), {
    id <- as.character(state$scenario$cohort$id[[1L]])
    session$setInputs(model = "tree", compare = "ranger", permutations = 2,
                      profile_type = "partial", variable = "life_points",
                      casualty = id, refresh = 1)
    for (pane in c("importance_ui", "importance_compare_ui", "profile_ui",
                   "shap_ui")) {
      expect_match(ui_html(output[[pane]]), "shiny-plot-output", fixed = TRUE)
    }
    expect_s3_class(explainer(), "explainer")
    expect_s3_class(explainer_compare(), "explainer")
    expect_s3_class(importance_primary(), "ggplot")
    # The top-feature box is filled from the importance plot rather than from
    # a second permutation run, and it is spelled as words.
    expect_true(top_feature() %in% state$models$tree$predictors)
    expect_match(ui_html(output$vb_top),
                 .sdt_explain_label_field(top_feature()), fixed = TRUE)
    expect_match(ui_html(output$vb_top), "Largest loss increase",
                 fixed = TRUE)
  })
})

test_that("pressing Explain again rebuilds the explainer", {
  skip_if_no_dalex()
  # Two builds of the same explainer are indistinguishable by value, so the
  # builder is counted instead. Standing in for it also keeps this test off
  # the modelling stack, which has nothing to do with what is being checked.
  built <- new.env(parent = emptyenv())
  built$n <- 0L
  local_mocked_bindings(sdt_explain = function(model, ...) {
    built$n <- built$n + 1L
    structure(list(label = model$engine), class = "explainer")
  })
  state <- fake_state()
  state$scenario <- demo_scenario()
  state$models <- stub_models()
  shiny::testServer(mod_explain_server, args = list(state = state), {
    session$setInputs(model = "tree", compare = "", permutations = 2,
                      profile_type = "partial", casualty = "",
                      variable = "age", refresh = 1)
    expect_s3_class(explainer(), "explainer")
    built_once <- built$n
    expect_gt(built_once, 0L)
    # An explainer is expensive and is shared by all four views, so asking
    # for it again must not build a second one.
    expect_s3_class(explainer(), "explainer")
    expect_identical(built$n, built_once)
    # The button is the panel's only way to redo the work on otherwise
    # unchanged inputs, and a refresh that rebuilds nothing is a dead control.
    session$setInputs(refresh = 2)
    expect_s3_class(explainer(), "explainer")
    expect_gt(built$n, built_once)
  })
})

test_that("every plot pane draws an image, not an empty device", {
  skip_if_no_explain()
  skip_if_not(capabilities("png"), "no raster device")
  state <- fake_state()
  state$scenario <- demo_scenario()
  state$models <- real_models()
  shiny::testServer(mod_explain_server, args = list(state = state), {
    id <- as.character(state$scenario$cohort$id[[1L]])
    session$setInputs(model = "tree", compare = "ranger", permutations = 2,
                      profile_type = "accumulated", variable = "life_points",
                      casualty = id, refresh = 1)
    # These are the four renders the panel exists for, including the
    # accumulated-effects branch of the profile. Checking only that they do
    # not error would pass on a plot that drew nothing at all, so what is
    # asserted is the image: an empty device encodes to a few hundred bytes,
    # each of these to tens of thousands.
    for (pane in c("importance", "importance_compare", "profile", "shap")) {
      src <- plot_src(output[[pane]])
      expect_match(src, "^data:image/png;base64,")
      expect_gt(nchar(src), 5000L)
    }
  })
})
