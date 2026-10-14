# Overview module -------------------------------------------------------------
# The panel the workbench opens on.
#
# It exists because of a specific failure: every other panel needs a scenario,
# and an app whose first screen is a form with an inert chart beside it leaves
# a new user guessing which of the six tabs to press first. This one says what
# the workbench is for, names the four steps in order, and offers a single
# button that runs all of them, so the app is populated within seconds of being
# opened and every other panel has something to show.

mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(7, 5),

    htmltools::div(
      bslib::card(
        bslib::card_header("A mass-casualty exercise, as a workbench"),
        bslib::card_body(
          htmltools::p(
            "Generate a casualty cohort, run it across a hospital board,",
            "capture the decisions that were made, then model and explain",
            "them. Every panel here calls a function you can also call from",
            "the console; nothing in the workbench is app-only."
          ),
          .sdt_step(1, paste(
            "Scenario \u2014 build a cohort and set the board's capacities."
          )),
          .sdt_step(2, paste(
            "Simulate \u2014 run the board and watch the casualties move."
          )),
          .sdt_step(3, paste(
            "Annotate \u2014 record what a participant decided for each card."
          )),
          .sdt_step(4, paste(
            "Model lab, Explain, Sequences \u2014 learn those decisions,",
            "read the model, and compare the pathways."
          ))
        ),
        bslib::card_footer(
          shiny::actionButton(
            ns("demo"), "Run the whole thing on the reference board",
            class = "btn-primary", icon = shiny::icon("play")
          ),
          htmltools::span(
            class = "sdt-metric-note",
            "Builds the calibrated scenario, runs it, and trains the bench."
          )
        )
      ),
      bslib::card(
        bslib::card_header("Where the numbers come from"),
        bslib::card_body(shiny::uiOutput(ns("provenance")))
      )
    ),

    htmltools::div(
      bslib::layout_columns(
        fill = FALSE,
        bslib::value_box("Scenario", shiny::uiOutput(ns("vb_scenario"))),
        bslib::value_box("Game run", shiny::uiOutput(ns("vb_run")))
      ),
      bslib::layout_columns(
        fill = FALSE,
        bslib::value_box("Models", shiny::uiOutput(ns("vb_models"))),
        bslib::value_box("Engines", shiny::uiOutput(ns("vb_engines")))
      ),
      bslib::card(
        bslib::card_header("The published exercise"),
        bslib::card_body(shiny::uiOutput(ns("design")))
      )
    )
  )
}

mod_overview_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {

    # The one-click path. Each step reports its own progress, because the whole
    # sequence takes long enough that a frozen screen would read as a hang, and
    # the model bench is by far the slowest part.
    shiny::observeEvent(input$demo, {
      shiny::withProgress(message = "Setting up the reference board", {
        ok <- .sdt_demo_scenario(state)
        shiny::incProgress(0.3, message = "Running the board")
        if (ok) ok <- .sdt_demo_run(state)
        shiny::incProgress(0.4, message = "Training the model bench")
        if (ok) .sdt_demo_bench(state)
        shiny::incProgress(0.3, message = "Done")
      })
      if (!is.null(state$run)) {
        shiny::showNotification(
          "Reference board built, run and modelled. Open Simulate to watch it.",
          type = "message", duration = 8
        )
      }
    })

    output$vb_scenario <- shiny::renderUI({
      sc <- state$scenario
      if (is.null(sc)) {
        return(.sdt_metric(NULL, "Not built yet"))
      }
      .sdt_metric(
        nrow(sc$cohort),
        sprintf("casualties on \u201c%s\u201d", sc$meta$name %||% "unnamed")
      )
    })

    output$vb_run <- shiny::renderUI({
      run <- state$run
      if (is.null(run)) {
        return(.sdt_metric(NULL, "Not run yet"))
      }
      .sdt_metric(
        run$meta$replications,
        sprintf("replications over %.0f min", run$meta$horizon)
      )
    })

    output$vb_models <- shiny::renderUI({
      bench <- state$bench
      models <- state$models
      if (is.null(bench) && is.null(models)) {
        return(.sdt_metric(NULL, "None trained yet"))
      }
      # The bench carries one row per engine per metric, so counting rows
      # would report four engines as sixteen models.
      n <- if (is.null(bench)) 1L else length(unique(bench$engine))
      .sdt_metric(n, if (n == 1L) "model trained" else "models compared")
    })

    output$vb_engines <- shiny::renderUI({
      eng <- .sdt_safely(sdt_engines())
      if (is.null(eng)) {
        return(.sdt_metric(NULL, "Unavailable"))
      }
      htmltools::tagList(
        .sdt_metric(
          sprintf("%d of %d", sum(eng$installed), nrow(eng)),
          "model engines installed"
        ),
        htmltools::div(lapply(seq_len(nrow(eng)), function(i) {
          .sdt_chip(
            eng$engine[[i]],
            if (isTRUE(eng$installed[[i]])) eng$class_box[[i]] else "muted"
          )
        }))
      )
    })

    output$design <- shiny::renderUI({
      d <- .sdt_safely(sdt_reference("design"))
      shiny::req(d)
      htmltools::tags$dl(
        class = "row mb-0",
        .sdt_dl_row("Scenario", d$scenario),
        .sdt_dl_row("Exercise", sprintf(
          "%.0f minutes, %d patient cards, played in %d rounds",
          d$duration, d$n_cards, d$card_rounds
        )),
        .sdt_dl_row("Team", sprintf(
          "%d people: %s", d$team_size,
          paste(names(d$roles), collapse = ", ")
        )),
        .sdt_dl_row("Board", sprintf("%d areas", nrow(d$board))),
        .sdt_dl_row("Sighting", d$triage_algorithm)
      )
    })

    output$provenance <- shiny::renderUI({
      meta <- .sdt_safely(sdt_reference("meta"))
      shiny::req(meta)
      htmltools::tagList(
        htmltools::p(
          class = "mb-2",
          "Reference marginals come from the published evaluation of the",
          "table-top exercise. The package separates them from its own",
          "modelling choices, so a calibration check never tests the package",
          "against its own assumptions."
        ),
        htmltools::p(
          class = "mb-2 sdt-metric-note", meta$source,
          htmltools::br(),
          htmltools::a(
            href = paste0("https://doi.org/", meta$doi),
            target = "_blank", rel = "noopener",
            paste0("doi:", meta$doi)
          ),
          sprintf(" \u00b7 %s", meta$licence)
        ),
        htmltools::div(class = "sdt-metric-note", meta$note)
      )
    })

    invisible(NULL)
  })
}

# A definition-list row, used by the design summary.
.sdt_dl_row <- function(label, value) {
  htmltools::tagList(
    htmltools::tags$dt(class = "col-4 text-body-secondary fw-normal", label),
    htmltools::tags$dd(class = "col-8", value)
  )
}

# The three steps of the demo, kept out of the observer so each can fail on its
# own terms and be tested without a Shiny session.
#
# They return TRUE only when the step actually produced something, because a
# bench trained on a scenario that failed to build would raise a second, less
# informative error on top of the first.

.sdt_demo_scenario <- function(state) {
  sc <- .sdt_try_notify(sdt_reference_scenario(), "Could not build the board")
  if (is.null(sc)) {
    return(FALSE)
  }
  # The demo trains a model on the participant's calls, so the cohort needs
  # them; the reference scenario alone carries only the cards.
  co <- .sdt_try_notify(
    sdt_add_decisions(tibble::as_tibble(sc$cohort), seed = 2L),
    "Could not simulate participant calls"
  )
  if (!is.null(co)) {
    sc <- sdt_scenario(
      co, resources = sc$resources, name = "reference",
      seed = sc$meta$seed
    )
  }
  state$scenario <- sc
  state$run <- NULL
  state$models <- NULL
  state$bench <- NULL
  TRUE
}

.sdt_demo_run <- function(state) {
  run <- .sdt_try_notify(
    sdt_simulate(state$scenario, replications = 5L, seed = 1L, verbose = FALSE),
    "Could not run the board"
  )
  state$run <- run
  !is.null(run)
}

# The bench is best-effort: with no model engine installed there is nothing to
# train, and that is a thin workbench rather than a broken one.
.sdt_demo_bench <- function(state) {
  if (!length(.sdt_tidymodels_pkgs())) {
    return(FALSE)
  }
  bench <- .sdt_try_notify(
    sdt_workflowset(
      tibble::as_tibble(state$scenario$cohort), target = "called_triage",
      seed = 1L
    ),
    "Could not train the model bench"
  )
  state$bench <- bench
  !is.null(bench)
}

# The tidymodels stack the bench needs, or an empty vector if any is missing.
.sdt_tidymodels_pkgs <- function() {
  need <- c("parsnip", "recipes", "workflows", "workflowsets", "rsample",
            "tune", "yardstick", "dials", "hardhat")
  if (all(vapply(need, requireNamespace, logical(1), quietly = TRUE))) {
    need
  } else {
    character()
  }
}

# Run an expression, and turn a failure into a notification naming the step
# rather than an unhandled error that takes the session down.
.sdt_try_notify <- function(expr, what) {
  tryCatch(
    expr,
    error = function(e) {
      shiny::showNotification(
        sprintf("%s: %s", what, conditionMessage(e)),
        type = "error", duration = 10
      )
      NULL
    }
  )
}
