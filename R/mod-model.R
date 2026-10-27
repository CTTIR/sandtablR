# Model lab module ------------------------------------------------------------
# Train one engine, or screen them all, and read the result off a leaderboard
# that keeps glass-box and black-box models side by side. That comparison is
# the point of the panel: the interesting question is not which model wins but
# how much is given up by choosing one you can read.
#
# The sidebar is ordered by what the panel is for. The two buttons come first,
# because pressing them is the whole purpose; the target and the engine follow,
# because they decide what gets trained; and the split, the folds and the seed
# are folded away, because they are almost always left alone and a wall of
# numeric inputs above the primary action pushes that action below the fold.
#
# The engine chips sit directly under the engine picker rather than at the
# bottom of the sidebar. An engine whose package is missing cannot be trained,
# and that has to be legible where the engine is chosen.

mod_model_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    # The tabs hold a four-row table and one curve. Left fillable, the card
    # stretches to the height of the viewport and leaves most of a tall screen
    # empty, so the main area is sized by its content instead.
    fillable = FALSE,

    sidebar = bslib::sidebar(
      width = 320,
      shiny::actionButton(
        ns("fit"), "Train this engine", class = "btn-primary w-100",
        icon = shiny::icon("play")
      ),
      shiny::actionButton(
        ns("bench"), "Compare all engines",
        class = "btn-outline-secondary w-100 mt-2"
      ),
      # What the panel needs before either button does anything, stated where
      # the buttons are rather than discovered by pressing one.
      shiny::uiOutput(ns("precondition"), class = "mt-3"),

      shiny::selectInput(
        ns("target"), "Predict",
        c("Participant's called category" = "called_triage",
          "Participant's called therapy" = "called_therapy",
          "True category (card)" = "triage",
          "True therapy (card)" = "therapy")
      ),
      htmltools::div(
        class = "sdt-metric-note mb-3",
        "The card's true values follow from the life points by construction,",
        "so every engine scores near 1 on them. The participant's call is the",
        "target worth benchmarking."
      ),
      shiny::selectInput(ns("engine"), "Engine",
                         .sdt_model_engine_choices()),
      shiny::uiOutput(ns("engine_status"), class = "mb-3"),

      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Training settings",
          shiny::sliderInput(ns("prop"), "Training share", 0.5, 0.9, 0.75,
                             step = 0.05),
          shiny::numericInput(ns("folds"), "Cross-validation folds", value = 5,
                              min = 2, max = 10),
          shiny::checkboxInput(ns("tune"), "Tune hyperparameters",
                               value = FALSE),
          shiny::numericInput(ns("grid"), "Grid size", value = 5, min = 1,
                              max = 20),
          shiny::numericInput(ns("seed"), "Seed", value = 1, min = 1),
          # Off by default, like every other parallel switch in the package.
          # It pays off on a tuned fit or an engine screen and costs a little
          # on a single untuned fit, so it is offered rather than assumed.
          shiny::checkboxInput(ns("parallel"), "Fit in parallel",
                               value = FALSE)
        )
      ),

      shiny::hr(),
      # Still called "roc_model" although it now picks the model for the
      # confusion view as well: the ID is how the headless tests and the rest
      # of the app address this control, and renaming it would break them.
      shiny::selectInput(ns("roc_model"), "Inspect", choices = character()),
      shiny::uiOutput(ns("trained_status"))
    ),

    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box("Best engine", shiny::uiOutput(ns("vb_best"))),
      # The one box whose body is a text output: the headless test asserts it
      # as a string. Wrapping it in .sdt_metric() gives it the typography and
      # the caption of the two boxes either side, and the render emits an
      # em dash rather than nothing when there is no board yet.
      bslib::value_box(
        "Best ROC AUC",
        .sdt_metric(shiny::textOutput(ns("vb_auc"), inline = TRUE),
                    "Held-out, best engine on the board")
      ),
      bslib::value_box("Glass-box cost", shiny::uiOutput(ns("vb_cost")))
    ),

    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::nav_panel("Leaderboard", shiny::uiOutput(ns("board_ui"))),
      bslib::nav_panel("ROC", shiny::uiOutput(ns("roc_ui"))),
      bslib::nav_panel("Confusion", shiny::uiOutput(ns("confusion_ui")))
    )
  )
}

mod_model_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {

    # Which widget each tab can actually mount, decided once. The renderer for
    # an absent package is never defined, so reactable::renderReactable() and
    # friends are only ever named inside these guards; naming one unguarded
    # would take down the whole app as the module server runs, rather than
    # degrading one panel.
    have_reactable <- requireNamespace("reactable", quietly = TRUE)
    have_dt <- requireNamespace("DT", quietly = TRUE)
    have_plotly <- requireNamespace("plotly", quietly = TRUE)
    board_widget <- if (have_reactable) {
      "board_reactable"
    } else if (have_dt) {
      "board_dt"
    } else {
      "board_table"
    }
    roc_widget <- if (have_plotly) "roc_plotly" else "roc_static"

    # Preconditions -----------------------------------------------------------

    target_of <- shiny::reactive(input$target %||% "called_triage")

    training_data <- shiny::reactive({
      shiny::req(state$scenario)
      tibble::as_tibble(state$scenario$cohort)
    })

    # Checked before either button does any work. A press that silently does
    # nothing is indistinguishable from a broken panel, so every reason the
    # bench cannot run is said out loud once, here.
    can_train <- function() {
      absent <- .sdt_model_missing_stack()
      if (length(absent)) {
        shiny::showNotification(
          sprintf("Model training needs %s. Install %s to use this panel.",
                  .sdt_model_and(absent),
                  if (length(absent) == 1L) "it" else "them"),
          type = "error", duration = 10
        )
        return(FALSE)
      }
      if (is.null(state$scenario)) {
        shiny::showNotification(
          "Build a scenario on the Scenario tab before training on it.",
          type = "warning", duration = 8
        )
        return(FALSE)
      }
      if (!target_of() %in% names(state$scenario$cohort)) {
        shiny::showNotification(
          paste(
            sprintf("This cohort has no column for the %s.",
                    .sdt_model_target_label(target_of())),
            "Record the calls on the Annotate tab, or predict a card value."
          ),
          type = "warning", duration = 10
        )
        return(FALSE)
      }
      TRUE
    }

    output$precondition <- shiny::renderUI({
      absent <- .sdt_model_missing_stack()
      if (length(absent)) {
        return(.sdt_step("!", sprintf(
          "Training needs %s, which is not installed.",
          .sdt_model_and(absent)
        )))
      }
      if (is.null(state$scenario)) {
        return(.sdt_step(1, paste(
          "Build a scenario first. This panel learns the cohort you build on",
          "the Scenario tab."
        )))
      }
      cohort <- state$scenario$cohort
      if (!target_of() %in% names(cohort)) {
        return(.sdt_step(3, sprintf(
          "This cohort has no %s. Record it on the Annotate tab.",
          .sdt_model_target_label(target_of())
        )))
      }
      .sdt_step(4, sprintf(
        "Ready: %d cards, predicting the %s.",
        nrow(cohort), .sdt_model_target_label(target_of())
      ))
    })

    output$engine_status <- shiny::renderUI({
      spec <- .sdt_safely(.sdt_model_engines())
      shiny::req(spec)
      chosen <- input$engine %||% ""
      row <- match(chosen, spec$engine)
      note <- if (!is.na(row) && !isTRUE(spec$installed[[row]])) {
        sprintf("%s needs the %s package, which is not installed.",
                .sdt_model_engine_label(chosen), spec$package[[row]])
      } else {
        paste(
          sprintf("%d of %d engines installed;", sum(spec$installed),
                  nrow(spec)),
          "a missing one is skipped when comparing them all."
        )
      }
      htmltools::tagList(
        htmltools::div(lapply(seq_len(nrow(spec)), function(i) {
          .sdt_chip(
            .sdt_model_engine_label(spec$engine[[i]]),
            if (isTRUE(spec$installed[[i]])) spec$class_box[[i]] else "muted"
          )
        })),
        htmltools::div(class = "sdt-metric-note", note)
      )
    })

    output$trained_status <- shiny::renderUI({
      models <- state$models %||% list()
      if (!length(models)) {
        return(htmltools::div(
          class = "sdt-metric-note",
          "Nothing trained in this session yet."
        ))
      }
      htmltools::tagList(
        htmltools::div(
          class = "sdt-metric-note",
          sprintf("%d model%s in hand:", length(models),
                  if (length(models) == 1L) "" else "s")
        ),
        htmltools::div(lapply(names(models), function(nm) {
          .sdt_chip(.sdt_model_engine_label(nm),
                    models[[nm]]$class_box %||% "muted")
        }))
      )
    })

    # Training ----------------------------------------------------------------

    fit_args <- function() {
      list(
        target = target_of(),
        tune = isTRUE(input$tune),
        grid = max(1L, as.integer(input$grid %||% 5L)),
        folds = max(2L, as.integer(input$folds %||% 5L)),
        prop = input$prop %||% 0.75,
        parallel = isTRUE(input$parallel),
        seed = as.integer(input$seed %||% 1L)
      )
    }

    shiny::observeEvent(input$fit, {
      if (!can_train()) {
        return(invisible(NULL))
      }
      engine <- input$engine %||% "tree"
      data <- training_data()
      args <- fit_args()
      shiny::withProgress(message = "Training", value = 0.2, {
        model <- tryCatch(
          do.call(sdt_fit, c(
            list(data = data, engine = engine, verbose = FALSE), args
          )),
          error = function(e) {
            shiny::showNotification(conditionMessage(e), type = "error",
                                    duration = 10)
            NULL
          }
        )
        shiny::incProgress(0.8)
        shiny::req(model)
        models <- state$models %||% list()
        models[[engine]] <- model
        state$models <- models
        state$bench <- .sdt_board_from_models(models)
      })
    })

    shiny::observeEvent(input$bench, {
      if (!can_train()) {
        return(invisible(NULL))
      }
      data <- training_data()
      args <- fit_args()
      shiny::withProgress(message = "Screening engines", value = 0.1, {
        board <- tryCatch(
          do.call(sdt_workflowset, c(
            list(data = data, verbose = FALSE), args
          )),
          error = function(e) {
            shiny::showNotification(conditionMessage(e), type = "error",
                                    duration = 10)
            NULL
          }
        )
        shiny::incProgress(0.9)
        shiny::req(board)
        state$models <- attr(board, "models")
        state$bench <- board
        skipped <- attr(board, "skipped")
        if (length(skipped)) {
          shiny::showNotification(
            .sdt_model_skip_note(skipped, sdt_engines()),
            type = "warning", duration = 8
          )
        }
      })
    })

    # The picker follows what has been trained, and keeps its selection when
    # a second engine joins the first. ignoreNULL = FALSE so that clearing the
    # models, which happens whenever a new scenario is built, empties it too.
    shiny::observeEvent(state$models, {
      names_of <- names(state$models %||% list())
      if (!length(names_of)) {
        shiny::updateSelectInput(session, "roc_model", choices = character())
        return(invisible(NULL))
      }
      current <- input$roc_model %||% ""
      shiny::updateSelectInput(
        session, "roc_model",
        choices = stats::setNames(names_of,
                                  .sdt_model_engine_label(names_of)),
        selected = if (current %in% names_of) current else names_of[[1L]]
      )
    }, ignoreNULL = FALSE)

    # Summary boxes -----------------------------------------------------------

    # Deliberately not written with req(): a value box whose body stops
    # rendering is drawn as a titled grey slab, which reads as broken rather
    # than as waiting. Every box below returns something in every state.
    best_row <- shiny::reactive({
      board <- state$bench
      if (is.null(board) || !nrow(board)) {
        return(NULL)
      }
      auc <- board[board$.metric == "roc_auc", , drop = FALSE]
      if (!nrow(auc)) {
        return(NULL)
      }
      auc[which.max(auc$.estimate), , drop = FALSE]
    })

    output$vb_best <- shiny::renderUI({
      row <- best_row()
      if (is.null(row)) {
        return(.sdt_metric(NULL, "Nothing trained yet"))
      }
      n <- length(unique(state$bench$engine))
      .sdt_metric(
        .sdt_model_engine_label(row$engine),
        htmltools::tagList(
          .sdt_chip(.sdt_model_box_label(row$class_box), row$class_box),
          sprintf("best of %d engine%s on the board", n,
                  if (n == 1L) "" else "s")
        )
      )
    })

    output$vb_auc <- shiny::renderText({
      row <- best_row()
      if (is.null(row)) "\u2014" else sprintf("%.3f", row$.estimate)
    })

    output$vb_cost <- shiny::renderUI({
      board <- state$bench
      if (is.null(board) || !nrow(board)) {
        return(.sdt_metric(NULL, "Nothing trained yet"))
      }
      auc <- board[board$.metric == "roc_auc", , drop = FALSE]
      # Everything that is not glass is the comparison, whatever grade it
      # carries. The question is what readability costs, and a grey box is
      # still not one you can read.
      glass <- suppressWarnings(max(auc$.estimate[auc$class_box == "glass"]))
      black <- suppressWarnings(max(auc$.estimate[auc$class_box != "glass"]))
      if (!is.finite(glass) || !is.finite(black)) {
        return(.sdt_metric(NULL, "Needs a readable and an unreadable engine"))
      }
      .sdt_metric(
        sprintf("%+.3f", glass - black),
        "Best readable model's ROC AUC, less the best unreadable one"
      )
    })

    # Leaderboard -------------------------------------------------------------

    board_wide <- shiny::reactive({
      board <- state$bench
      shiny::req(board, nrow(board) > 0L)
      .sdt_safely(.sdt_model_board_wide(board))
    })

    # One renderUI mounts the richest widget the session can draw. The choice
    # is board_widget, made once above, so the element this emits and the
    # renderer kept alive at the foot of the server cannot drift apart.
    output$board_ui <- shiny::renderUI({
      board <- state$bench
      if (is.null(board) || !nrow(board)) {
        return(.sdt_model_empty(!is.null(state$scenario), "leaderboard"))
      }
      widget <- switch(
        board_widget,
        board_reactable = reactable::reactableOutput(
          session$ns("board_reactable")
        ),
        board_dt = DT::DTOutput(session$ns("board_dt")),
        shiny::tableOutput(session$ns("board_table"))
      )
      htmltools::tagList(widget, .sdt_model_board_note())
    })

    if (have_reactable) {
      output$board_reactable <- reactable::renderReactable({
        wide <- board_wide()
        shiny::req(wide)
        .sdt_safely(reactable::reactable(
          wide,
          # Only when the board carries the column it is ranked on: reactable
          # aborts on a defaultSorted naming a column that is not there.
          defaultSorted = if ("roc_auc" %in% names(wide)) {
            list(roc_auc = "desc")
          },
          columns = .sdt_model_board_columns(wide),
          defaultColDef = reactable::colDef(vAlign = "center"),
          theme = .sdt_model_board_theme(),
          highlight = TRUE, compact = TRUE, fullWidth = TRUE,
          sortable = TRUE, borderless = TRUE
        ))
      })
    }

    if (have_dt) {
      output$board_dt <- DT::renderDT({
        wide <- board_wide()
        shiny::req(wide)
        shown <- .sdt_model_board_display(wide, html = TRUE)
        metrics <- setdiff(names(shown), c("Engine", "Model class"))
        .sdt_safely(DT::formatRound(
          DT::datatable(
            shown, rownames = FALSE, selection = "none",
            # Only the chip column carries markup; everything else is escaped.
            escape = setdiff(names(shown), "Model class"),
            options = list(dom = "t", paging = FALSE, order = list())
          ),
          columns = metrics, digits = 3
        ))
      }, server = FALSE)
    }

    output$board_table <- shiny::renderTable({
      wide <- board_wide()
      shiny::req(wide)
      .sdt_model_board_display(wide, html = FALSE)
    }, digits = 3, striped = TRUE, width = "100%")

    # ROC ---------------------------------------------------------------------

    selected_model <- shiny::reactive({
      models <- state$models
      shiny::req(models, length(models) > 0L)
      name <- input$roc_model %||% ""
      if (!nzchar(name) || !name %in% names(models)) {
        name <- names(models)[[1L]]
      }
      models[[name]]
    })

    output$roc_ui <- shiny::renderUI({
      if (!length(state$models %||% list())) {
        return(.sdt_model_empty(!is.null(state$scenario), "curve to draw"))
      }
      widget <- if (identical(roc_widget, "roc_plotly")) {
        plotly::plotlyOutput(session$ns("roc_plotly"), height = "460px")
      } else {
        shiny::plotOutput(session$ns("roc_static"), height = "460px")
      }
      htmltools::tagList(
        widget,
        htmltools::div(
          class = "sdt-metric-note mt-2",
          "One curve per class, one against all the others, on the held-out",
          "half of the split. The diagonal is what guessing would give you."
        )
      )
    })

    if (have_plotly) {
      output$roc_plotly <- plotly::renderPlotly({
        .sdt_safely(plotly::ggplotly(
          .sdt_app_plot(.sdt_model_roc_plot(selected_model()))
        ))
      })
    }

    output$roc_static <- .sdt_render_plot({
      .sdt_safely(.sdt_model_roc_plot(selected_model()))
    })

    # Confusion ---------------------------------------------------------------

    output$confusion_ui <- shiny::renderUI({
      if (!length(state$models %||% list())) {
        return(.sdt_model_empty(!is.null(state$scenario),
                                "model to score against the truth"))
      }
      htmltools::tagList(
        htmltools::div(
          class = "sdt-metric-note mb-2",
          "Rows are what was recorded for the casualty; columns are what the",
          "model called it, on the held-out half of the split. The diagonal",
          "is agreement, and everything off it is a disagreement worth",
          "reading: confusing a red for a yellow is not the same mistake as",
          "the other way round."
        ),
        shiny::tableOutput(session$ns("confusion"))
      )
    })

    output$confusion <- shiny::renderTable({
      preds <- .sdt_safely(sdt_predictions(selected_model()))
      shiny::req(preds)
      .sdt_model_confusion(preds$truth, preds$.pred_class)
    }, digits = 0, striped = TRUE, width = "100%")

    # The whole panel is a nav_panel of the app's own tabset, and the ROC and
    # Confusion views are nav_panels again inside it, so on first paint every
    # output here is hidden and a suspended output renders blank on arrival:
    # a reader opening the Model lab would find the sidebar, the value boxes
    # and the tab bodies empty. Only the widget the UI actually mounts is kept
    # alive alongside them; unsuspending the fallback as well would redraw a
    # table and a curve nobody can see on every fit.
    mounted <- c(
      "precondition", "engine_status", "trained_status",
      "vb_best", "vb_auc", "vb_cost",
      "board_ui", "roc_ui", "confusion_ui", "confusion",
      board_widget, roc_widget
    )
    for (nm in mounted) {
      shiny::outputOptions(output, nm, suspendWhenHidden = FALSE)
    }
    invisible(NULL)
  })
}


# Vocabulary ------------------------------------------------------------------
# Nothing on the page is allowed to be a column name or a package name. These
# turn the internal vocabulary into the words the panel actually says, and
# leave anything unrecognised as it came in rather than as NA.

.sdt_model_relabel <- function(x, labels) {
  x <- as.character(x)
  # ifelse() returns logical(0) for an empty test, which would hand a caller a
  # vector of the wrong type where it expects words.
  if (!length(x)) {
    return(character())
  }
  out <- unname(labels[x])
  ifelse(is.na(out), x, out)
}

.sdt_model_engine_label <- function(x) {
  .sdt_model_relabel(x, c(
    multinom = "Multinomial logistic", glmnet = "Penalised regression",
    tree = "Decision tree", mars = "Spline model (MARS)",
    xgboost = "Gradient boosting", ranger = "Random forest",
    svm_rbf = "Support vector machine", mlp = "Neural network"
  ))
}

.sdt_model_box_label <- function(x) {
  .sdt_model_relabel(
    x, c(glass = "Glass box", grey = "Grey box", black = "Black box",
         closed = "Closed box")
  )
}

.sdt_model_metric_label <- function(x) {
  .sdt_model_relabel(x, c(
    roc_auc = "ROC AUC", accuracy = "Accuracy", kap = "Cohen's kappa",
    f_meas = "F-measure"
  ))
}

.sdt_model_target_label <- function(x) {
  .sdt_model_relabel(x, c(
    called_triage = "participant's called category",
    called_therapy = "participant's called therapy",
    triage = "card's true category",
    therapy = "card's true therapy"
  ))
}

# Sighting categories carry their SK number, which is what the board is
# labelled with; the therapy concepts are acronyms the exercise uses as they
# are, except the card's "no operation" option.
.sdt_model_class_label <- function(x) {
  .sdt_model_relabel(x, c(
    red = "Red (SK I)", yellow = "Yellow (SK II)", green = "Green (SK III)",
    none = "No operation"
  ))
}

# A confusion matrix read alphabetically puts green above red, which inverts
# the one ordering everybody in the room shares. Put the classes back in the
# order the vocabulary declares them, and leave anything unknown at the end.
.sdt_model_class_order <- function(classes) {
  known <- c(sdt_triage_levels(), sdt_therapy_levels())
  c(intersect(known, classes), setdiff(classes, known))
}

# "a, b and c", for a sentence rather than a vector printed at the reader.
# Why the board left each engine out. sdt_workflowset() skips an engine whose
# package is missing, and the transformer when keras3 is installed but has no
# working backend. Those are different fixes, so each gets its own sentence.
.sdt_model_skip_note <- function(skipped, spec) {
  has_pkg <- spec$installed[match(skipped, spec$engine)] %in% TRUE
  say <- function(x, why) {
    sprintf("Skipped %s: %s.", .sdt_model_and(.sdt_model_engine_label(x)), why)
  }
  paste(c(
    if (any(!has_pkg)) say(skipped[!has_pkg], "no package installed"),
    if (any(has_pkg)) say(skipped[has_pkg], "no working keras3 backend")
  ), collapse = " ")
}

.sdt_model_and <- function(x) {
  n <- length(x)
  if (n < 2L) {
    return(paste(x, collapse = ""))
  }
  paste(paste(x[-n], collapse = ", "), "and", x[[n]])
}


# Engines ---------------------------------------------------------------------

# The order the picker offers, glass-box first: a readable model is the right
# default for this bench, and it is also the cheapest to fit.
# The registry is already ordered by readability - glass, then grey, then
# black - which is the order this picker wants to present. Taking it from
# there rather than restating it here means an engine added to
# .sdt_engines() appears in the app without a second edit; a hard-coded list
# silently sorted every new engine to the bottom.
.sdt_model_engines <- function() {
  spec <- sdt_engines()
  spec[order(match(spec$class_box, .sdt_box_levels())), , drop = FALSE]
}

.sdt_model_engine_choices <- function() {
  spec <- .sdt_model_engines()
  stats::setNames(
    spec$engine,
    sprintf("%s (%s-box)", .sdt_model_engine_label(spec$engine),
            spec$class_box)
  )
}

# The tidymodels stack every engine needs whatever the engine, listed so the
# panel can say what is missing instead of failing at the first fit.
.sdt_model_missing_stack <- function() {
  pkgs <- c("parsnip", "recipes", "workflows", "rsample", "tune", "yardstick",
            "dials", "hardhat")
  pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
}


# Empty states ----------------------------------------------------------------

# Every panel here can be empty for exactly two reasons, and they need
# different answers: there is no cohort to learn, or there is one and nothing
# has been trained on it yet. Saying which is the difference between a panel
# that looks broken and one that looks like it is waiting.
.sdt_model_empty <- function(has_scenario, what) {
  if (!has_scenario) {
    return(.sdt_empty(
      "Build a scenario first.",
      paste("The model lab learns the cohort you build on the Scenario tab.",
            "Nothing here can be computed until one exists.")
    ))
  }
  .sdt_empty(
    sprintf("No %s yet.", what),
    paste("Press Train this engine to fit the engine chosen in the sidebar,",
          "or Compare all engines to screen every installed one on the same",
          "split.")
  )
}


# Leaderboard -----------------------------------------------------------------

.sdt_model_metric_order <- function() {
  c("roc_auc", "accuracy", "kap", "f_meas")
}

# The board arrives one row per engine and metric. The leaderboard wants one
# row per engine, ROC AUC first because that is what it is ranked on.
.sdt_model_board_wide <- function(board) {
  long <- as.data.frame(
    board[, c("engine", "class_box", ".metric", ".estimate"), drop = FALSE]
  )
  wide <- stats::reshape(
    long, idvar = c("engine", "class_box"), timevar = ".metric",
    direction = "wide"
  )
  names(wide) <- sub("^\\.estimate\\.", "", names(wide))
  metrics <- intersect(.sdt_model_metric_order(), names(wide))
  metrics <- c(metrics,
               setdiff(names(wide), c("engine", "class_box", metrics)))
  wide <- wide[, c("engine", "class_box", metrics), drop = FALSE]
  if ("roc_auc" %in% names(wide)) {
    wide <- wide[order(-wide$roc_auc), , drop = FALSE]
  }
  rownames(wide) <- NULL
  wide
}

# A metric drawn as a number and a bar. Four engines scoring 0.71, 0.74, 0.75
# and 0.78 are four numbers in a column and one picture of how little there is
# between them.
.sdt_model_bar_cell <- function(value) {
  if (!is.numeric(value) || !length(value) || !is.finite(value)) {
    return(htmltools::span(class = "sdt-metric-empty", "\u2014"))
  }
  width <- 100 * max(0, min(1, value))
  htmltools::div(
    style = "display: flex; align-items: center; gap: 0.6rem;",
    htmltools::span(
      style = "font-variant-numeric: tabular-nums; min-width: 3rem;",
      formatC(value, format = "f", digits = 3)
    ),
    htmltools::div(
      style = paste(
        "flex: 1 1 auto; height: 0.4rem; border-radius: 999px;",
        "background: var(--sdt-line);"
      ),
      htmltools::div(style = sprintf(
        "%s width: %.1f%%;",
        "height: 100%; border-radius: 999px; background: var(--sdt-accent);",
        width
      ))
    )
  )
}

.sdt_model_board_columns <- function(wide) {
  cols <- list(
    engine = reactable::colDef(
      name = "Engine", minWidth = 150,
      cell = function(value) .sdt_model_engine_label(value)
    ),
    class_box = reactable::colDef(
      name = "Model class", minWidth = 120,
      cell = function(value) {
        .sdt_chip(.sdt_model_box_label(value), value)
      }
    )
  )
  for (nm in setdiff(names(wide), c("engine", "class_box"))) {
    cols[[nm]] <- reactable::colDef(
      name = .sdt_model_metric_label(nm), minWidth = 160,
      cell = function(value) .sdt_model_bar_cell(value)
    )
  }
  cols
}

# reactable ships its own light palette, which is a white table on a dark page
# in dark mode. Binding it to the app's custom properties instead means it
# follows the theme toggle like everything else.
.sdt_model_board_theme <- function() {
  reactable::reactableTheme(
    color = "inherit",
    backgroundColor = "transparent",
    borderColor = "var(--sdt-line)",
    highlightColor = "var(--sdt-accent-soft)",
    headerStyle = list(color = "var(--sdt-muted)", fontWeight = 600)
  )
}

# The same board for the fallbacks, which have no cell renderer: DT takes the
# chip as markup, the plain table takes the words.
.sdt_model_board_display <- function(wide, html = FALSE) {
  metrics <- setdiff(names(wide), c("engine", "class_box"))
  box <- if (html) {
    vapply(
      wide$class_box,
      function(x) as.character(.sdt_chip(.sdt_model_box_label(x), x)),
      character(1)
    )
  } else {
    .sdt_model_box_label(wide$class_box)
  }
  out <- data.frame(
    Engine = .sdt_model_engine_label(wide$engine),
    `Model class` = unname(box),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  for (nm in metrics) {
    out[[.sdt_model_metric_label(nm)]] <- wide[[nm]]
  }
  out
}

.sdt_model_board_note <- function() {
  htmltools::div(
    class = "sdt-metric-note mt-3",
    "Every engine is scored on the same held-out split, so the column is a",
    "like-for-like comparison. Cohen's kappa sits beside accuracy on purpose:",
    "on a cohort that is 44% red, a model that always calls red is right 44%",
    "of the time and has learnt nothing."
  )
}


# Curves and agreement --------------------------------------------------------

# sdt_roc() subtitles the plot with the raw target column and keys the legend
# on the raw class level, so the curve says "red" where the confusion matrix
# beside it says "Red (SK I)", listed alphabetically so green comes first. Both
# are column-level vocabulary rather than something to show a reader, so the
# module relabels them on the way to the page.
.sdt_model_roc_plot <- function(model) {
  plot <- sdt_roc(model) +
    ggplot2::labs(subtitle = sprintf(
      "%s (%s), predicting the %s",
      .sdt_model_engine_label(model$engine),
      tolower(.sdt_model_box_label(model$class_box)),
      .sdt_model_target_label(model$target)
    ))
  # Two classes are drawn as one curve with no colour key, and a failed fit is
  # drawn as an empty placeholder; neither has a legend to relabel.
  if (!".level" %in% names(plot$data)) {
    return(plot)
  }
  classes <- .sdt_model_class_order(unique(as.character(plot$data$.level)))
  plot + ggplot2::scale_colour_discrete(
    limits = classes, labels = .sdt_model_class_label(classes)
  )
}

# The confusion matrix, with both axes written out. Counts alone hide the
# asymmetry that matters here, so each row carries how many casualties it
# covers and how often the model got them right.
.sdt_model_confusion <- function(truth, predicted) {
  classes <- .sdt_model_class_order(
    union(levels(factor(truth)), levels(factor(predicted)))
  )
  counts <- table(
    factor(truth, levels = classes), factor(predicted, levels = classes)
  )
  cases <- as.integer(rowSums(counts))
  hits <- as.integer(diag(counts))
  cells <- as.data.frame.matrix(counts)
  names(cells) <- sprintf("Predicted: %s", .sdt_model_class_label(classes))
  data.frame(
    Observed = .sdt_model_class_label(classes),
    cells,
    Cases = cases,
    Correct = ifelse(cases > 0L, sprintf("%.0f%%", 100 * hits / cases),
                     "\u2014"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}


# Rebuild the leaderboard from whatever models are in hand, so training a
# single engine updates the same table that "compare all" fills.
.sdt_board_from_models <- function(models) {
  if (!length(models)) {
    return(NULL)
  }
  rows <- lapply(names(models), function(engine) {
    m <- sdt_metrics(models[[engine]])
    m$engine <- engine
    m$class_box <- models[[engine]]$class_box
    m[, c("engine", "class_box", ".metric", ".estimator", ".estimate")]
  })
  .rank_by_auc(dplyr::bind_rows(rows))
}
