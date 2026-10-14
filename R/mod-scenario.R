# Scenario module -------------------------------------------------------------
# Build a cohort and a board, and see immediately what you built. Everything
# downstream reads state$scenario, so this is the only place a scenario is
# created.
#
# That makes this panel the entry point, and the layout is built around it.
# The two controls that actually produce a scenario sit at the very top of the
# sidebar, ahead of every setting; the settings are folded into an accordion so
# that eleven numeric inputs cannot push the primary action below the fold. The
# main area leads with the step line, because a first-time reader arriving on a
# panel of empty cards has no way to tell "nothing built yet" from "broken".

.sdt_capacity_inputs <- function() {
  list(
    triage = 2L, ed_red = 3L, ed_yellow = 4L, diagnostics = 3L,
    or = 5L, recovery = 4L, icu_imc = 12L, ward = 25L
  )
}

mod_scenario_ui <- function(id) {
  ns <- shiny::NS(id)
  caps <- .sdt_capacity_inputs()

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,

      # Primary action first, then the one alternative to it, then a line
      # saying whether either has been used. Everything below this point is a
      # setting, and settings are only interesting once the reader knows what
      # the button does.
      shiny::actionButton(
        ns("build"), "Build scenario",
        class = "btn-primary w-100"
      ),
      shiny::actionButton(
        ns("reference"), "Load reference board",
        class = "btn-outline-secondary w-100 mt-2"
      ),
      htmltools::div(class = "mt-2 mb-1", shiny::uiOutput(ns("status"))),

      bslib::accordion(
        # Only the stream is open. The capacities are set once per board and
        # then left alone, so they cost nothing folded away and cost the whole
        # fold when they are not.
        open = "Casualty stream",
        class = "sdt-sidebar-group",
        bslib::accordion_panel(
          "Casualty stream",
          shiny::numericInput(ns("n"), "Casualties", value = 40, min = 1,
                              max = 500, step = 5),
          shiny::sliderInput(ns("p_red"), "P(red)", 0, 1, 0.44, step = 0.01),
          shiny::sliderInput(ns("p_yellow"), "P(yellow)", 0, 1, 0.24,
                             step = 0.01),
          shiny::helpText(shiny::textOutput(ns("p_green_note"),
                                            inline = TRUE)),
          shiny::selectInput(ns("backend"), "Generator",
                             c("base", "simstudy")),
          shiny::numericInput(ns("seed"), "Seed", value = 1, min = 1),
          shiny::checkboxInput(
            ns("decisions"), "Add simulated participant calls", value = TRUE
          )
        ),
        bslib::accordion_panel(
          "Board capacity",
          shiny::helpText("Places available in each area of the board."),
          lapply(names(caps), function(area) {
            shiny::numericInput(
              ns(paste0("cap_", area)),
              # The published board's own wording. A reader should never have
              # to work out that "icu_imc" is the ICU / IMC bay.
              .sdt_label_area(area),
              value = caps[[area]], min = 1, step = 1
            )
          })
        )
      ),

      shiny::hr(),
      shiny::downloadButton(ns("save"), "Save scenario",
                            class = "btn-outline-secondary w-100"),
      shiny::fileInput(ns("load"), "Load scenario", accept = ".rds")
    ),

    .sdt_step(
      1,
      paste(
        "Step 1 of 4: build a cohort and a board.",
        "Simulate plays the game on it, Model lab trains on what the game",
        "produced, and Explain reads the fitted model back."
      )
    ),

    # The metric shells are built here rather than in the render, so a caption
    # explaining each number is on screen at first paint. Before a scenario
    # exists the outputs emit an em-dash: a value box with an empty body
    # renders as a titled grey slab and reads as a failure.
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        "Casualties",
        .sdt_metric(
          shiny::textOutput(ns("vb_n"), inline = TRUE), "in the cohort"
        ),
        theme = "primary"
      ),
      bslib::value_box(
        "Triage mix", shiny::uiOutput(ns("vb_mix"))
      ),
      bslib::value_box(
        "Mean life points",
        .sdt_metric(
          shiny::textOutput(ns("vb_life"), inline = TRUE),
          "1 = most critical, 6 = least"
        )
      ),
      bslib::value_box(
        "Board capacity",
        .sdt_metric(
          shiny::textOutput(ns("vb_caps"), inline = TRUE),
          sprintf("%s and %s places", .sdt_label_area("or"),
                  .sdt_label_area("icu_imc"))
        )
      )
    ),

    bslib::layout_columns(
      col_widths = c(5, 7),
      fill = FALSE,
      bslib::card(
        height = "340px",
        full_screen = TRUE,
        bslib::card_header("Triage mix"),
        shiny::uiOutput(ns("mix_ui"))
      ),
      bslib::card(
        height = "340px",
        bslib::card_header("Against the published board"),
        shiny::uiOutput(ns("marginals"))
      )
    ),

    # Last, and the only card left to fill the window: the table is the one
    # thing here that genuinely benefits from whatever height remains.
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Cohort"),
      shiny::uiOutput(ns("cohort_ui"))
    )
  )
}

mod_scenario_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {

    output$p_green_note <- shiny::renderText({
      green <- max(0, 1 - (input$p_red %||% 0) - (input$p_yellow %||% 0))
      sprintf("P(green) = %.2f", green)
    })

    capacities <- shiny::reactive({
      areas <- names(.sdt_capacity_inputs())
      caps <- lapply(areas, function(a) {
        value <- input[[paste0("cap_", a)]]
        if (is.null(value) || is.na(value)) 1L else max(1L, as.integer(value))
      })
      stats::setNames(caps, areas)
    })

    build <- function(cohort, name, seed) {
      scenario <- sdt_scenario(cohort, resources = capacities(),
                               name = name, seed = seed)
      state$scenario <- scenario
      # A new cohort invalidates everything downstream. Leaving a stale run or
      # model on screen beside a new scenario is worse than showing nothing.
      state$run <- NULL
      state$models <- NULL
      state$bench <- NULL
    }

    shiny::observeEvent(input$build, {
      shiny::req(input$n)
      green <- max(0, 1 - input$p_red - input$p_yellow)
      probs <- c(red = input$p_red, yellow = input$p_yellow, green = green)
      result <- tryCatch({
        cohort <- sdt_synthetic_cohort(
          n = input$n, triage_prob = probs, seed = input$seed,
          backend = input$backend
        )
        if (isTRUE(input$decisions)) {
          cohort <- sdt_add_decisions(cohort, seed = input$seed + 1L)
        }
        cohort
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error",
                                duration = 8)
        NULL
      })
      shiny::req(result)
      build(result, "workbench", input$seed)
    })

    shiny::observeEvent(input$reference, {
      scenario <- sdt_reference_scenario()
      cohort <- sdt_add_decisions(scenario$cohort, seed = 2L)
      state$scenario <- sdt_scenario(
        cohort, resources = scenario$resources, name = "reference",
        seed = scenario$meta$seed
      )
      state$run <- NULL
      state$models <- NULL
      state$bench <- NULL
      shiny::showNotification(
        "Loaded the board the engine is calibrated on.", type = "message"
      )
    })

    shiny::observeEvent(input$load, {
      shiny::req(input$load)
      loaded <- tryCatch(readRDS(input$load$datapath), error = function(e) {
        shiny::showNotification(
          paste("Could not read that file:", conditionMessage(e)),
          type = "error", duration = 8
        )
        NULL
      })
      shiny::req(loaded)
      if (!inherits(loaded, "scenario")) {
        shiny::showNotification(
          "That file does not contain a scenario.", type = "error"
        )
        return(invisible(NULL))
      }
      state$scenario <- loaded
      state$run <- NULL
      state$models <- NULL
      state$bench <- NULL
    })

    output$save <- shiny::downloadHandler(
      filename = function() {
        name <- state$scenario$meta$name %||% "scenario"
        paste0("sandtablR-", name, ".rds")
      },
      content = function(file) saveRDS(state$scenario, file)
    )

    # Derived cohort summaries ------------------------------------------------

    # NULL rather than a zero table when there is nothing to count, so every
    # consumer has one thing to test for. The chips and the comparison against
    # the published board both read this, and they must never disagree.
    triage_counts <- shiny::reactive({
      if (is.null(state$scenario)) {
        return(NULL)
      }
      counts <- table(factor(state$scenario$cohort$triage,
                             levels = sdt_triage_levels()))
      if (!sum(counts)) NULL else counts
    })

    # Value boxes -------------------------------------------------------------
    #
    # These stay text outputs feeding the metric shell built in the UI, so the
    # em-dash placeholder is emitted here. It is deliberate that they render
    # something rather than nothing before a scenario exists.

    output$vb_n <- shiny::renderText({
      if (is.null(state$scenario)) {
        return("\u2014")
      }
      format(nrow(state$scenario$cohort))
    })

    output$vb_mix <- shiny::renderUI({
      .sdt_safely({
        counts <- triage_counts()
        chips <- if (is.null(counts)) {
          NULL
        } else {
          htmltools::tagList(lapply(sdt_triage_levels(), function(level) {
            .sdt_chip(paste0(substr(level, 1, 1), ": ", counts[[level]]),
                      level)
          }))
        }
        .sdt_metric(chips, "casualties by sighting category")
      })
    })

    output$vb_life <- shiny::renderText({
      .sdt_safely({
        cohort <- state$scenario$cohort
        if (is.null(cohort) || !nrow(cohort)) {
          return("\u2014")
        }
        sprintf("%.1f", mean(cohort$life_points))
      }, fallback = "\u2014")
    })

    output$vb_caps <- shiny::renderText({
      if (is.null(state$scenario)) {
        return("\u2014")
      }
      res <- state$scenario$resources
      sprintf("%s / %s", res$or, res$icu_imc)
    })

    output$status <- shiny::renderUI({
      .sdt_safely({
        if (is.null(state$scenario)) {
          return(htmltools::div(
            class = "small",
            .sdt_chip("No scenario yet", "muted"),
            "every other panel waits on this one"
          ))
        }
        htmltools::div(
          class = "small",
          .sdt_chip(state$scenario$meta$name %||% "scenario", "green"),
          sprintf("%d casualties on the board",
                  nrow(state$scenario$cohort))
        )
      })
    })

    # Plots and tables --------------------------------------------------------

    # A plot output cannot carry its own empty state, so the choice between the
    # plot and the placeholder is made here. Without this the card renders as a
    # tall blank box beside a table that does explain itself.
    output$mix_ui <- shiny::renderUI({
      if (is.null(triage_counts())) {
        return(.sdt_empty(
          "Nothing to plot yet.",
          paste(
            "Build a scenario, or load the reference board, to see how the",
            "cohort splits across the three sighting categories."
          )
        ))
      }
      shiny::plotOutput(session$ns("mix"), height = "250px")
    })

    output$mix <- .sdt_render_plot({
      # The same gate as mix_ui, not just req(state$scenario): this output is
      # never suspended, so it draws even while the placeholder is the thing
      # on screen, and an all-empty cohort makes the fill scale complain.
      shiny::req(state$scenario, triage_counts())
      # The card header already names the plot; a second title inside it only
      # costs height in a 340px card.
      .sdt_safely(
        sdt_plot_triage_mix(state$scenario) + ggplot2::labs(title = NULL)
      )
    })

    # The published marginals are known before anything is built, so this card
    # is never empty: it shows what the exercise reported, and fills the second
    # column in once a cohort exists. Seeing the target beside the draw is what
    # tells a user whether the sliders produced a plausible board.
    output$marginals <- shiny::renderUI({
      .sdt_safely({
        published <- sdt_reference("published")$triage_mix
        counts <- triage_counts()
        realised <- if (is.null(counts)) NULL else counts / sum(counts)

        rows <- lapply(sdt_triage_levels(), function(level) {
          htmltools::tags$tr(
            htmltools::tags$td(.sdt_chip(level, level)),
            htmltools::tags$td(
              class = "text-end",
              sprintf("%.1f%%", 100 * published[[level]])
            ),
            htmltools::tags$td(
              class = "text-end",
              if (is.null(realised)) {
                "\u2014"
              } else {
                sprintf("%.1f%%", 100 * realised[[level]])
              }
            )
          )
        })

        htmltools::tagList(
          htmltools::tags$table(
            class = "table table-sm mb-1",
            htmltools::tags$thead(htmltools::tags$tr(
              htmltools::tags$th("Category"),
              htmltools::tags$th(class = "text-end", "Published"),
              htmltools::tags$th(class = "text-end", "This cohort")
            )),
            htmltools::tags$tbody(rows)
          ),
          shiny::helpText(
            "Published shares are the marginals reported for the exercise;",
            "the reference board reproduces them."
          )
        )
      })
    })

    output$cohort_ui <- shiny::renderUI({
      if (is.null(state$scenario)) {
        return(.sdt_empty(
          "No cohort yet.",
          paste(
            "Build a scenario to browse the patient cards, or load the",
            "reference board to start from the published one."
          )
        ))
      }
      if (!requireNamespace("DT", quietly = TRUE)) {
        return(.sdt_empty(
          "The cohort table needs the DT package.",
          "Install it with install.packages(\"DT\")."
        ))
      }
      DT::DTOutput(session$ns("cohort"))
    })

    # Defined conditionally, not guarded inside: DT::renderDT() is evaluated
    # when this server function runs, so referencing it without the package
    # would take the whole app down rather than this one card.
    if (requireNamespace("DT", quietly = TRUE)) {
      output$cohort <- DT::renderDT({
        shiny::req(state$scenario)
        co <- tibble::as_tibble(state$scenario$cohort)
        # The cohort's column names are the data model, not English.
        # .sdt_annotate_label() carries a written-out header for every one of
        # them, so this browser and the editor on the Annotate tab name the
        # same column the same way rather than one of them printing
        # "life_points" at the reader.
        .sdt_safely(DT::datatable(
          co,
          colnames = .sdt_annotate_label(names(co)),
          rownames = FALSE,
          options = list(pageLength = 10, scrollX = TRUE)
        ))
      })
    }

    # Tabs are hidden until selected, and a suspended output never renders, so
    # a reader arriving on this panel for the first time would find the whole
    # of it blank. The cohort table only exists when DT is installed.
    always <- c("p_green_note", "status", "vb_n", "vb_mix", "vb_life",
                "vb_caps", "mix_ui", "mix", "marginals", "cohort_ui")
    optional <- if (requireNamespace("DT", quietly = TRUE)) "cohort" else NULL
    for (nm in c(always, optional)) {
      shiny::outputOptions(output, nm, suspendWhenHidden = FALSE)
    }
    invisible(NULL)
  })
}
