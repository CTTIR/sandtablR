# Explain module --------------------------------------------------------------
# Interrogate a trained model from the outside. Every explanation here is
# model-agnostic, so the same three views work on a decision tree and on a
# random forest. That is what makes the side-by-side importance view the point
# of the panel rather than a decoration: a glass-box model can be read
# directly, a black-box one cannot, and putting the two importance orderings
# next to each other is the only way to see what readability actually costs.
#
# DALEX and DALEXtra are optional, so every view has to survive their absence
# and say exactly what to install. That sentence is read back off
# .sdt_need_pkgs() rather than written out again here, so the message on
# screen and the requirement in the render cannot drift apart.

# Human labels ----------------------------------------------------------------
# Nothing on this panel may show a column name. "life_points" is an identifier
# in the data, not a word, and a permutation-importance headline that reads
# "life_points" tells a clinician less than the plot it came from.

.sdt_explain_field_labels <- function() {
  c(
    id = "Casualty id", age = "Age", sex = "Sex",
    life_points = "Life points", ventilated = "Ventilated",
    injury_region = "Injury region", triage = "Category on the card",
    therapy = "Therapy on the card", arrival_order = "Arrival order",
    called_triage = "Called category", called_therapy = "Called therapy"
  )
}

.sdt_explain_sex_labels <- function() {
  c(f = "female", m = "male", d = "diverse")
}

.sdt_explain_therapy_labels <- function() {
  c(ETC = "ETC", DCS = "DCS", TASC = "TASC", none = "no operation")
}

.sdt_explain_region_labels <- function() {
  c(
    head = "head", thorax = "thorax", abdomen = "abdomen",
    extremity = "extremity", multiple = "multiple regions"
  )
}

# Look a vector up in a label table, leaving anything unrecognised as it came
# in rather than turning it into NA: a cohort can be loaded from a file, and an
# empty cell on screen is worse than an unfamiliar word.
.sdt_explain_relabel <- function(x, labels) {
  out <- unname(labels[as.character(x)])
  ifelse(is.na(out), as.character(x), out)
}

# Predictors are not a closed vocabulary -- a loaded cohort may carry columns
# this package has never seen -- so anything unlisted is at least spelled as
# words instead of shown as a snake_case name.
.sdt_explain_label_field <- function(x) {
  out <- unname(.sdt_explain_field_labels()[as.character(x)])
  spelled <- gsub("_", " ", as.character(x), fixed = TRUE)
  substr(spelled, 1L, 1L) <- toupper(substr(spelled, 1L, 1L))
  ifelse(is.na(out), spelled, out)
}

# A casualty picker that identifies a person rather than a row. The id alone
# is unreadable: choosing between "C007" and "C012" is guesswork, choosing
# between two cards is a decision.
.sdt_explain_casualty_choices <- function(cohort) {
  if (is.null(cohort) || !nrow(cohort)) {
    return(character())
  }
  labels <- sprintf(
    "%s: %s y, %s, triage %s, %s",
    as.character(cohort$id), format(cohort$age),
    .sdt_explain_relabel(cohort$sex, .sdt_explain_sex_labels()),
    as.character(cohort$triage),
    .sdt_explain_relabel(cohort$therapy, .sdt_explain_therapy_labels())
  )
  stats::setNames(as.character(cohort$id), labels)
}

.sdt_explain_model_choices <- function(models) {
  if (!length(models)) {
    return(character())
  }
  box <- vapply(models, function(m) m$class_box %||% "", character(1))
  stats::setNames(
    names(models), sprintf("%s (%s-box)", names(models), box)
  )
}

# The most useful second model is one of the other kind. Picking it for the
# user is the difference between a comparison that is offered and one that is
# actually seen.
.sdt_explain_contrast <- function(models, name) {
  if (length(models) < 2L || !name %in% names(models)) {
    return(NULL)
  }
  box <- vapply(models, function(m) m$class_box %||% "", character(1))
  other <- names(models)[box != box[[name]] & names(models) != name]
  if (!length(other)) {
    return(NULL)
  }
  other[[1L]]
}

# Fill-aware building blocks --------------------------------------------------

# A tab body that grows with the card instead of sitting at a fixed height
# above a band of empty space. The choice between a plot and an empty state
# has to be made server-side, so the pane is a uiOutput -- and a uiOutput is
# an ordinary div, which bslib will not stretch unless it says it can be
# stretched. That declaration is the whole fix for the dead vertical space.
.sdt_explain_pane <- function(id) {
  bslib::as_fill_carrier(shiny::uiOutput(id))
}

# Empty states are stretched to the pane as well, so their own centring puts
# the message in the middle of the space rather than at the top of it.
.sdt_explain_blank <- function(message, hint = NULL) {
  bslib::as_fill_item(.sdt_empty(message, hint))
}

# The install line, taken from the shared guard rather than restated. It
# signals through shiny::validate(), so the sentence is read back off the
# condition it raises; one place decides what to install.
.sdt_explain_missing_hint <- function() {
  tryCatch(
    {
      .sdt_need_pkgs(c("DALEX", "DALEXtra"), "Model explanations")
      NULL
    },
    error = function(e) conditionMessage(e)
  )
}

# The single place that decides whether a view can draw anything, so all four
# refuse for the same reasons, in the same order, in the same words. Returns
# NULL when there is nothing standing in the way.
.sdt_explain_gate <- function(models, model) {
  if (!length(models)) {
    return(.sdt_explain_blank(
      "No model has been trained yet.",
      paste(
        "Train an engine on the Model lab tab.",
        "Explanations read whatever it leaves in the session."
      )
    ))
  }
  hint <- .sdt_explain_missing_hint()
  if (!is.null(hint)) {
    return(.sdt_explain_blank("Explanations need one more package.", hint))
  }
  if (is.null(model)) {
    return(.sdt_explain_blank(
      "No model chosen.",
      "Pick one under Model to explain, on the left."
    ))
  }
  NULL
}

.sdt_explain_head <- function(model, prefix) {
  if (is.null(model)) {
    return(htmltools::span(class = "text-muted", prefix))
  }
  htmltools::tagList(
    htmltools::span(class = "me-2", paste0(prefix, ": ", model$engine)),
    .sdt_chip(paste0(model$class_box, "-box"), model$class_box)
  )
}

# UI ---------------------------------------------------------------------------

mod_explain_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      # The action and the one control it needs come first. The settings
      # below are set once and rarely touched, and stacking them above the
      # button pushed the only thing worth pressing off the bottom of the
      # sidebar.
      shiny::selectInput(ns("model"), "Model to explain",
                         choices = character()),
      shiny::actionButton(ns("refresh"), "Explain this model",
                          class = "btn-primary w-100"),
      htmltools::div(
        class = "sdt-sidebar-group mt-3",
        shiny::selectInput(ns("compare"), "Compare against",
                           choices = c("None" = "")),
        shiny::helpText(
          "A glass-box model beside a black-box one shows what readability",
          "costs, instead of asserting it."
        )
      ),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Explanation settings",
          shiny::numericInput(ns("permutations"), "Permutation rounds",
                              value = 10, min = 2, max = 50),
          shiny::helpText("More rounds are steadier and slower."),
          shiny::selectInput(ns("variable"), "Profile variable",
                             choices = character()),
          shiny::radioButtons(
            ns("profile_type"), "Profile",
            c("Partial dependence" = "partial",
              "Accumulated local effects" = "accumulated")
          )
        )
      ),
      htmltools::div(class = "sdt-sidebar-group",
                     shiny::uiOutput(ns("status")))
    ),

    shiny::uiOutput(ns("guide")),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box("Model explained", shiny::uiOutput(ns("vb_model"))),
      bslib::value_box("Top feature", shiny::uiOutput(ns("vb_top"))),
      bslib::value_box("Explainer backend", shiny::uiOutput(ns("vb_backend")))
    ),
    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::nav_panel(
        "Importance",
        bslib::layout_columns(
          col_widths = c(6, 6),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              shiny::uiOutput(ns("head_primary"), inline = TRUE)
            ),
            .sdt_explain_pane(ns("importance_ui"))
          ),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              shiny::uiOutput(ns("head_compare"), inline = TRUE)
            ),
            .sdt_explain_pane(ns("importance_compare_ui"))
          )
        )
      ),
      bslib::nav_panel(
        "Profiles",
        .sdt_explain_pane(ns("profile_ui"))
      ),
      bslib::nav_panel(
        "Attribution",
        shiny::selectInput(ns("casualty"), "Casualty",
                           choices = character(), width = "100%"),
        shiny::uiOutput(ns("casualty_card")),
        .sdt_explain_pane(ns("shap_ui"))
      )
    )
  )
}

# Server -----------------------------------------------------------------------

mod_explain_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {

    # Choices -----------------------------------------------------------------
    # ignoreNULL = FALSE throughout: building a new scenario clears the models,
    # and a picker still offering a model that no longer exists is worse than
    # an empty one.

    shiny::observeEvent(state$models, ignoreNULL = FALSE, {
      choices <- .sdt_explain_model_choices(state$models)
      keep <- function(current) {
        if (isTRUE(current %in% choices)) current else NULL
      }
      shiny::updateSelectInput(session, "model", choices = choices,
                               selected = keep(input$model))
      shiny::updateSelectInput(session, "compare",
                               choices = c("None" = "", choices),
                               selected = keep(input$compare))
    })

    # Offer the comparison once, and only once: a user who sets it back to
    # None has said what they want, and re-imposing it on the next model
    # change would be the app arguing with them.
    auto_compared <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$model, ignoreNULL = FALSE, {
      models <- state$models
      name <- input$model %||% ""
      # Halting here would leave the predictor picker listing the columns of
      # a model that has just been discarded, which is the same stale-choice
      # failure the observer above exists to prevent.
      if (!length(models) || !nzchar(name) || !name %in% names(models)) {
        shiny::updateSelectInput(session, "variable", choices = character())
        return()
      }
      predictors <- models[[name]]$predictors %||% character()

      shiny::updateSelectInput(
        session, "variable",
        choices = stats::setNames(predictors,
                                  .sdt_explain_label_field(predictors)),
        selected = if (isTRUE(input$variable %in% predictors)) input$variable
      )

      if (!auto_compared() && !nzchar(input$compare %||% "")) {
        other <- .sdt_explain_contrast(models, name)
        if (!is.null(other)) {
          auto_compared(TRUE)
          shiny::updateSelectInput(session, "compare", selected = other)
        }
      }
    })

    shiny::observeEvent(state$scenario, ignoreNULL = FALSE, {
      cohort <- state$scenario$cohort
      shiny::updateSelectInput(
        session, "casualty",
        choices = .sdt_explain_casualty_choices(cohort),
        selected = if (isTRUE(input$casualty %in% cohort$id)) input$casualty
      )
    })

    # Explainers --------------------------------------------------------------

    # Clamped rather than trusted: a numericInput hands back whatever was
    # typed, including NA from a cleared box, and every explanation below
    # passes this straight to a permutation count.
    permutations <- shiny::reactive({
      value <- suppressWarnings(as.integer(input$permutations %||% 10L))
      if (length(value) != 1L || is.na(value)) {
        return(10L)
      }
      max(2L, min(50L, value))
    })

    explain_ready <- function() {
      .sdt_need_pkgs(c("DALEX", "DALEXtra"), "Model explanations")
    }

    # A model without halting the caller. The value boxes and the empty states
    # need this: a req() here would leave a box as a titled grey slab, which
    # is the very failure those boxes exist to report.
    peek_model <- function(which) {
      models <- state$models
      if (!length(models)) {
        return(NULL)
      }
      name <- if (identical(which, "primary")) input$model else input$compare
      if (!length(name) || !nzchar(name) || !name %in% names(models)) {
        return(NULL)
      }
      models[[name]]
    }

    model_of <- function(which) {
      model <- peek_model(which)
      shiny::req(model)
      model
    }

    # Explainers are expensive and are reused across all four views.
    explainer <- shiny::reactive({
      input$refresh
      explain_ready()
      .sdt_safely(sdt_explain(model_of("primary")))
    })

    explainer_compare <- shiny::reactive({
      input$refresh
      explain_ready()
      .sdt_safely(sdt_explain(model_of("compare")))
    })

    importance_primary <- shiny::reactive({
      explain_ready()
      .sdt_safely(sdt_importance(explainer(),
                                 n_permutations = permutations()))
    })

    # The importance plot is the most expensive object on the panel, so the
    # headline feature is read back off it rather than permuting a second time
    # for one word. A shape we do not recognise gives NULL and the box says so,
    # because none of this is a documented interface.
    top_feature <- shiny::reactive({
      plot <- tryCatch(importance_primary(), error = function(e) NULL)
      .sdt_explain_top_feature(plot)
    })

    # Summary boxes -----------------------------------------------------------

    output$vb_model <- shiny::renderUI({
      .sdt_safely(.sdt_explain_model_metric(peek_model("primary")))
    })

    output$vb_top <- shiny::renderUI({
      .sdt_safely({
        feature <- top_feature()
        if (is.null(feature)) {
          .sdt_metric(NULL, "Appears with the importance plot.")
        } else {
          .sdt_metric(.sdt_explain_label_field(feature),
                      "Largest loss increase when shuffled")
        }
      })
    })

    output$vb_backend <- shiny::renderUI({
      .sdt_safely(.sdt_explain_backend_metric())
    })

    output$guide <- shiny::renderUI({
      .sdt_safely(.sdt_explain_guide(state$models, peek_model("primary"),
                                     peek_model("compare")))
    })

    output$status <- shiny::renderUI({
      .sdt_safely(.sdt_explain_status(state$models))
    })

    output$head_primary <- shiny::renderUI({
      .sdt_safely(.sdt_explain_head(peek_model("primary"), "Explaining"))
    })

    output$head_compare <- shiny::renderUI({
      .sdt_safely(.sdt_explain_head(peek_model("compare"), "Compared with"))
    })

    # Panes -------------------------------------------------------------------
    # Each view decides between a plot and an empty state here, so a plot that
    # has nothing to draw explains itself exactly as a table would.

    output$importance_ui <- shiny::renderUI({
      gate <- .sdt_explain_gate(state$models, peek_model("primary"))
      if (!is.null(gate)) {
        return(gate)
      }
      shiny::plotOutput(session$ns("importance"), height = "100%")
    })

    output$importance_compare_ui <- shiny::renderUI({
      gate <- .sdt_explain_gate(state$models, peek_model("primary"))
      if (!is.null(gate)) {
        return(gate)
      }
      if (is.null(peek_model("compare"))) {
        return(.sdt_explain_blank(
          "No second model yet.",
          paste(
            "Choose one under Compare against.",
            "Reading the two orderings together is what shows the price of",
            "a model you can read."
          )
        ))
      }
      shiny::plotOutput(session$ns("importance_compare"), height = "100%")
    })

    output$profile_ui <- shiny::renderUI({
      gate <- .sdt_explain_gate(state$models, peek_model("primary"))
      if (!is.null(gate)) {
        return(gate)
      }
      if (!length(input$variable) || !nzchar(input$variable)) {
        return(.sdt_explain_blank(
          "No predictor chosen.",
          "Pick one under Explanation settings, on the left."
        ))
      }
      shiny::plotOutput(session$ns("profile"), height = "100%")
    })

    output$shap_ui <- shiny::renderUI({
      gate <- .sdt_explain_gate(state$models, peek_model("primary"))
      if (!is.null(gate)) {
        return(gate)
      }
      if (is.null(state$scenario)) {
        return(.sdt_explain_blank(
          "No scenario in the session.",
          paste(
            "Build one on the Scenario tab.",
            "An attribution needs the casualty it is explaining."
          )
        ))
      }
      if (!length(input$casualty) || !nzchar(input$casualty)) {
        return(.sdt_explain_blank(
          "No casualty chosen.",
          "Pick one above to see what moved this model's prediction."
        ))
      }
      shiny::plotOutput(session$ns("shap"), height = "100%")
    })

    # Plots -------------------------------------------------------------------

    output$importance <- .sdt_render_plot({
      shiny::req(state$models)
      shiny::withProgress(message = "Permuting predictors", value = 0.3, {
        importance_primary()
      })
    })

    output$importance_compare <- .sdt_render_plot({
      shiny::req(state$models)
      explain_ready()
      shiny::withProgress(message = "Permuting predictors", value = 0.3, {
        .sdt_safely(sdt_importance(explainer_compare(),
                                   n_permutations = permutations()))
      })
    })

    output$profile <- .sdt_render_plot({
      shiny::req(state$models, input$variable)
      explain_ready()
      .sdt_safely(sdt_profile(
        explainer(), variables = input$variable,
        type = input$profile_type %||% "partial"
      ))
    })

    output$casualty_card <- shiny::renderUI({
      shiny::req(state$scenario, input$casualty)
      co <- tibble::as_tibble(state$scenario$cohort)
      row <- co[co$id == input$casualty, , drop = FALSE]
      if (!nrow(row)) {
        return(NULL)
      }
      .sdt_safely(.sdt_explain_card(row[1L, , drop = FALSE]))
    })

    output$shap <- .sdt_render_plot({
      shiny::req(state$models, state$scenario, input$casualty)
      explain_ready()
      co <- tibble::as_tibble(state$scenario$cohort)
      note <- "Attributing the prediction"
      shiny::withProgress(message = note, value = 0.3, {
        .sdt_safely(sdt_shap(
          explainer(), input$casualty, data = co,
          n_permutations = permutations()
        ))
      })
    })

    # The whole panel is a nav_panel, and its three views are nav_panels
    # again, so on arrival every output here is hidden -- and a suspended
    # output renders blank. The pane switches matter most: suspending one
    # withholds the plot and its empty state together, which is the one
    # failure mode this panel is built to rule out.
    for (nm in c("guide", "status", "vb_model", "vb_top", "vb_backend",
                 "head_primary", "head_compare", "importance_ui",
                 "importance_compare_ui", "profile_ui", "shap_ui",
                 "importance", "importance_compare", "profile",
                 "casualty_card", "shap")) {
      shiny::outputOptions(output, nm, suspendWhenHidden = FALSE)
    }
    invisible(NULL)
  })
}

# Panel furniture -------------------------------------------------------------

.sdt_explain_model_metric <- function(model) {
  if (is.null(model)) {
    return(.sdt_metric(NULL, "Train a model on the Model lab tab."))
  }
  .sdt_metric(
    model$engine,
    sprintf(
      "%s-box, predicting %s", model$class_box,
      tolower(.sdt_explain_label_field(model$target))
    )
  )
}

.sdt_explain_backend_metric <- function() {
  pkgs <- c("DALEX", "DALEXtra")
  have <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  if (!all(have)) {
    return(.sdt_metric(
      NULL, paste("Not installed:", paste(pkgs[!have], collapse = ", "))
    ))
  }
  .sdt_metric(
    "DALEX",
    sprintf(
      "version %s, explaining the held-out split",
      format(utils::packageVersion("DALEX"))
    )
  )
}

# What to do next, and nothing once there is nothing left to do: a permanent
# instruction bar is just a row of pixels the reader learns to skip.
.sdt_explain_guide <- function(models, primary, compare) {
  if (!length(models)) {
    return(.sdt_step(1, paste(
      "Train a model on the Model lab tab.",
      "Explanations read whatever it leaves in the session."
    )))
  }
  if (is.null(primary)) {
    return(.sdt_step(2, "Choose the model to explain, on the left."))
  }
  if (is.null(compare)) {
    return(.sdt_step(3, paste(
      "Choose a second model under Compare against to see what a readable",
      "model costs."
    )))
  }
  NULL
}

# Which models are in hand and whether they can be explained at all. Both sit
# in the sidebar rather than three scrolls down a tab, because both change
# what the panel is able to do.
.sdt_explain_status <- function(models) {
  backend <- if (is.null(.sdt_explain_missing_hint())) {
    .sdt_chip("DALEX ready", "green")
  } else {
    .sdt_chip("DALEX missing", "red")
  }
  trained <- if (!length(models)) {
    htmltools::span(class = "text-muted small", "no models yet")
  } else {
    lapply(names(models), function(nm) {
      .sdt_chip(nm, models[[nm]]$class_box %||% "muted")
    })
  }
  htmltools::tagList(
    htmltools::tags$small(class = "text-muted", "Status"),
    htmltools::br(), backend, trained
  )
}

# The casualty being attributed, as a card rather than as a row of the data
# frame it came from.
.sdt_explain_card <- function(row) {
  kind <- if (row$triage %in% sdt_triage_levels()) row$triage else "muted"
  therapy <- .sdt_explain_relabel(row$therapy, .sdt_explain_therapy_labels())
  region <- .sdt_explain_relabel(
    row$injury_region, .sdt_explain_region_labels()
  )
  detail <- sprintf(
    "%s years, %s, %s, life points %s%s",
    format(row$age),
    .sdt_explain_relabel(row$sex, .sdt_explain_sex_labels()),
    region, format(row$life_points),
    if (isTRUE(row$ventilated)) ", ventilated" else ""
  )
  htmltools::div(
    class = "d-flex flex-wrap align-items-center gap-2 mb-2",
    htmltools::tags$strong(as.character(row$id)),
    .sdt_chip(paste("triage", as.character(row$triage)), kind),
    .sdt_chip(therapy, "muted"),
    htmltools::span(class = "text-muted small", detail)
  )
}

# The single most important predictor, read off the importance plot. DALEX
# builds that plot through ingredients, which has carried the loss column
# under more than one name across versions and guarantees neither; an
# unfamiliar shape returns NULL so the box shows a placeholder instead of a
# wrong answer. The extraction is wrapped rather than tested with is.list()
# because a ggplot2 4.0 object is an S7 object, not a list, and only the
# element lookup itself is portable across both.
.sdt_explain_top_feature <- function(plot) {
  if (is.null(plot)) {
    return(NULL)
  }
  data <- tryCatch(plot[["data"]], error = function(e) NULL)
  if (!is.data.frame(data) || !"variable" %in% names(data)) {
    return(NULL)
  }
  loss <- data[["dropout_loss"]] %||% data[["dropout_loss.x"]]
  if (!is.numeric(loss) || length(loss) != nrow(data)) {
    return(NULL)
  }
  keep <- !as.character(data$variable) %in% c("_full_model_", "_baseline_") &
    is.finite(loss)
  if (!any(keep)) {
    return(NULL)
  }
  as.character(data$variable[keep][[which.max(loss[keep])]])
}
