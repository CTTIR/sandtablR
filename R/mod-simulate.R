# Simulate module -------------------------------------------------------------
# Run the board and look at what happened: where every casualty stood at any
# minute of the game, how hard each area was worked, and how the end state
# compares with the published one.
#
# The panel is ordered by what a user does, not by what the engine needs. The
# run button is the first thing in the sidebar and the board is the first tab,
# because those are the two things anyone opening this panel came for. The
# knobs that are set once a session are folded into a closed accordion: a
# sidebar that opens on four numeric inputs pushes its own primary action below
# the fold, which is the surest way to make a panel look like it does nothing.

# Run settings, named once so the sidebar's defaults and the sanitisers that
# guard the engine cannot disagree about what "unset" means.
.sdt_sim_defaults <- function() {
  list(reps = 5L, horizon = 270, seed = 1L)
}

# A number arriving from a control, sanitised.
#
# A cleared numericInput arrives as NA and a control the browser has not drawn
# yet arrives as NULL. sdt_simulate() and sdt_plot_board() abort on both, so
# neither is allowed past this point: a half-typed seed should not raise an
# error dialogue, it should mean "the last good value".
.sdt_sim_num <- function(x, fallback, min = 0) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) && x >= min
  if (ok) as.numeric(x) else as.numeric(fallback)
}

.sdt_sim_count <- function(x, fallback) {
  as.integer(.sdt_sim_num(x, fallback, min = 1))
}

# Which evaluated version of the exercise the end state is compared against.
# summary.game_run() matches its argument strictly, so anything unexpected is
# turned into the default here rather than being passed on to fail there.
.sdt_sim_version <- function(x) {
  if (identical(x, "1.0")) "1.0" else "2.0"
}

# Proportions and gaps as printed in the end-of-game table. Missing values get
# the same em-dash the value boxes use, so one glyph means "no value" across
# the whole workbench.
.sdt_sim_pct <- function(x) {
  ifelse(is.na(x), "\u2014", sprintf("%.1f%%", 100 * x))
}

.sdt_sim_pp <- function(x) {
  ifelse(is.na(x), "\u2014", sprintf("%+.1f pp", 100 * x))
}

# The board tooltips are patient cards: several short lines separated by
# newlines. ggiraph puts them in a plain element, so without pre-line the card
# arrives as one run-on sentence.
.sdt_sim_tooltip_css <- function() {
  paste0(
    "background:#2B2B2B;color:#fff;padding:6px 9px;border-radius:5px;",
    "font-size:12px;line-height:1.35;white-space:pre-line;"
  )
}

mod_simulate_ui <- function(id) {
  ns <- shiny::NS(id)
  d <- .sdt_sim_defaults()

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,

      # The one thing this panel exists to do, first and full width.
      htmltools::div(
        class = "sdt-sidebar-group",
        shiny::actionButton(ns("run"), "Run simulation",
                            class = "btn-primary w-100"),
        shiny::uiOutput(ns("gate"))
      ),

      # A viewing control rather than a setting: it changes which replication
      # is drawn, is reached constantly, and so stays in the open.
      shiny::sliderInput(ns("replication"), "Replication shown",
                         min = 1, max = 1, value = 1, step = 1),

      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Run settings",
          shiny::numericInput(ns("reps"), "Replications", value = d$reps,
                              min = 1, max = 100, step = 1),
          shiny::numericInput(ns("horizon"), "Game length (min)",
                              value = d$horizon, min = 30, max = 1440,
                              step = 30),
          shiny::numericInput(ns("seed"), "Seed", value = d$seed, min = 1),
          shiny::checkboxInput(ns("parallel"),
                               "Run replications in parallel", value = FALSE)
        )
      ),

      shiny::downloadButton(ns("download"), "Download event log",
                            class = "btn-outline-secondary w-100"),

      shiny::uiOutput(ns("status"))
    ),

    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        "Casualty records",
        .sdt_metric(
          shiny::textOutput(ns("vb_rows"), inline = TRUE),
          note = "One per casualty that cleared the board, per replication"
        )
      ),
      bslib::value_box(
        "Area events",
        .sdt_metric(
          shiny::textOutput(ns("vb_events"), inline = TRUE),
          note = "One row per board area entered"
        )
      ),
      bslib::value_box(
        "Median time in system",
        .sdt_metric(
          shiny::textOutput(ns("vb_time"), inline = TRUE),
          note = "Arrival on the board to leaving it"
        )
      )
    ),

    bslib::navset_card_tab(
      selected = "Board",
      full_screen = TRUE,

      # The board is the centrepiece: the nine areas of the published board
      # with the casualties standing on them, and a clock to scrub.
      bslib::nav_panel(
        "Board",
        shiny::sliderInput(
          ns("at"), "Minute of the game", min = 0, max = d$horizon,
          value = d$horizon, step = 10, width = "100%", post = " min",
          animate = shiny::animationOptions(interval = 600)
        ),
        shiny::uiOutput(ns("board_ui"), fill = TRUE)
      ),

      bslib::nav_panel(
        "Patient flow",
        shiny::helpText(
          "The forty earliest arrivals of the selected replication."
        ),
        shiny::uiOutput(ns("flow_ui"), fill = TRUE)
      ),

      bslib::nav_panel(
        "Utilisation",
        shiny::uiOutput(ns("util_ui"), fill = TRUE)
      ),

      bslib::nav_panel(
        "End of game",
        shiny::radioButtons(
          ns("version"), "Compare against evaluated version",
          choices = c("2.0", "1.0"), selected = "2.0", inline = TRUE
        ),
        bslib::layout_columns(
          col_widths = c(7, 5),
          shiny::uiOutput(ns("end_ui"), fill = TRUE),
          shiny::uiOutput(ns("end_table_ui"))
        )
      )
    )
  )
}

mod_simulate_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Asked once, and used both to choose the widget and to decide which
    # render*() to define. Asking twice risks the UI pointing at an output
    # that was never created, which fails silently as an empty panel.
    have_ggiraph <- requireNamespace("ggiraph", quietly = TRUE)
    have_plotly <- requireNamespace("plotly", quietly = TRUE)

    # Sidebar -----------------------------------------------------------------

    # States the precondition. Before this line existed the panel looked ready
    # to use with no scenario behind it, and the run button simply did nothing.
    output$gate <- shiny::renderUI({
      if (is.null(state$scenario)) {
        .sdt_step(1, "Build a scenario first, on the Scenario tab.")
      } else if (is.null(state$run)) {
        .sdt_step(2, "Ready to run this board.")
      } else {
        .sdt_step(3, "Scrub the clock on the Board tab.")
      }
    })

    output$status <- shiny::renderUI({
      .sdt_safely({
        cohort <- if (is.null(state$scenario)) {
          NULL
        } else {
          state$scenario$cohort
        }
        htmltools::tagList(
          htmltools::tags$hr(),
          htmltools::div(class = "sdt-metric-note mb-1", "Status"),
          if (is.null(cohort)) {
            .sdt_chip("No scenario", "red")
          } else {
            .sdt_chip(sprintf("Scenario: %d casualties", nrow(cohort)),
                      "green")
          },
          if (is.null(state$run)) {
            .sdt_chip("Not run yet", "muted")
          } else {
            .sdt_chip(
              sprintf("%d replications run",
                      .sdt_sim_count(state$run$meta$replications, 1L)),
              "green"
            )
          },
          .sdt_chip(
            if (have_ggiraph) "Board: interactive" else "Board: static",
            if (have_ggiraph) "green" else "muted"
          ),
          .sdt_chip(
            if (have_plotly) "Flow: interactive" else "Flow: static",
            if (have_plotly) "green" else "muted"
          )
        )
      })
    })

    shiny::observeEvent(input$run, {
      # Said out loud rather than dropped by req(): a button that does nothing
      # when pressed is the complaint this notification answers.
      if (is.null(state$scenario)) {
        shiny::showNotification(
          "Build a scenario on the Scenario tab before running the board.",
          type = "warning", duration = 6
        )
        return(invisible(NULL))
      }

      d <- .sdt_sim_defaults()
      reps <- .sdt_sim_count(input$reps, d$reps)
      horizon <- .sdt_sim_num(input$horizon, d$horizon, min = 1)
      seed <- .sdt_sim_count(input$seed, d$seed)
      n <- nrow(state$scenario$cohort)

      # The steps say what is happening rather than counting to a hundred: a
      # replication takes long enough that "please wait" is not an answer.
      run <- shiny::withProgress(message = "Running the board", value = 0, {
        shiny::incProgress(0.05, detail = sprintf(
          "%d casualties, %d replications, %.0f minutes", n, reps, horizon
        ))
        out <- tryCatch(
          sdt_simulate(
            state$scenario,
            replications = reps,
            horizon = horizon,
            parallel = isTRUE(input$parallel),
            seed = seed,
            verbose = FALSE
          ),
          error = function(e) {
            shiny::showNotification(conditionMessage(e), type = "error",
                                    duration = 8)
            NULL
          }
        )
        shiny::incProgress(0.7, detail = "Collecting the event log")
        if (!is.null(out)) {
          shiny::incProgress(0.15, detail = sprintf(
            "%d area events recorded", nrow(out$events)
          ))
        }
        shiny::incProgress(0.1, detail = "Drawing the board")
        out
      })

      shiny::req(run)
      state$run <- run
      shiny::updateSliderInput(session, "replication", max = reps, value = 1)
      shiny::updateSliderInput(session, "at", min = 0, max = horizon,
                               value = horizon)
    })

    # Value boxes -------------------------------------------------------------
    #
    # Deliberately not req()-ed on the run. A suspended value box renders as a
    # titled grey slab with no body, which reads as broken rather than as
    # waiting; these say what they would say and show an em-dash until there
    # is something to count.

    output$vb_rows <- shiny::renderText({
      .sdt_safely({
        if (is.null(state$run)) "\u2014" else format(nrow(state$run$arrivals))
      }, fallback = "\u2014")
    })

    output$vb_events <- shiny::renderText({
      .sdt_safely({
        if (is.null(state$run)) "\u2014" else format(nrow(state$run$events))
      }, fallback = "\u2014")
    })

    output$vb_time <- shiny::renderText({
      .sdt_safely({
        arrivals <- if (is.null(state$run)) NULL else state$run$arrivals
        if (is.null(arrivals)) {
          "\u2014"
        } else if (!nrow(arrivals)) {
          # A board played with nobody on it is not the same as a board not
          # yet played, so this is a measured blank rather than the
          # missing-value placeholder.
          "--"
        } else {
          sprintf("%.0f min", stats::median(arrivals$end - arrivals$start,
                                            na.rm = TRUE))
        }
      }, fallback = "\u2014")
    })

    # Which replication is on screen. Clamped to what was actually run, so a
    # slider left at 5 from a previous run cannot ask for a replication that
    # does not exist and quietly draw an empty board.
    shown_rep <- shiny::reactive({
      shiny::req(state$run)
      reps <- .sdt_sim_count(state$run$meta$replications, 1L)
      min(.sdt_sim_count(input$replication, 1L), reps)
    })

    # Board -------------------------------------------------------------------

    board_plot <- shiny::reactive({
      shiny::req(state$run)
      # An empty board -- minute zero, and any moment before the first
      # casualty is sighted -- carries no sighting values at all, so the
      # palette matches nothing and the key vanishes mid-animation. Fixing the
      # scale's limits keeps all three categories in the legend for the whole
      # scrub; ggplot2 notes the replacement, which is not news here.
      suppressMessages(
        sdt_plot_board(
          state$run,
          at = .sdt_sim_num(input$at, state$run$meta$horizon),
          replication = shown_rep(),
          interactive = TRUE
        ) +
          ggplot2::scale_fill_manual(
            values = .sdt_triage_palette(), drop = FALSE, name = "Sighting",
            limits = sdt_triage_levels()
          )
      )
    })

    output$board_ui <- shiny::renderUI({
      if (is.null(state$run)) {
        return(.sdt_empty(
          "The board is empty.",
          paste(
            "Run the simulation to place every casualty in the area it had",
            "reached, then scrub the clock to watch the stream move."
          )
        ))
      }
      if (have_ggiraph) {
        ggiraph::girafeOutput(ns("board_girafe"), height = "100%")
      } else {
        shiny::plotOutput(ns("board_static"), height = "100%")
      }
    })

    # Defined conditionally, not guarded inside: ggiraph::renderGirafe() is
    # evaluated the moment the module server runs, so referring to it when
    # ggiraph is absent would take down the whole app rather than one panel.
    if (have_ggiraph) {
      output$board_girafe <- ggiraph::renderGirafe({
        shiny::req(state$run)
        .sdt_safely(ggiraph::girafe(
          ggobj = .sdt_app_plot(board_plot()),
          bg = "transparent",
          width_svg = 9, height_svg = 4.6,
          options = list(
            ggiraph::opts_toolbar(saveaspng = FALSE),
            ggiraph::opts_hover(css = "stroke:#3D1C5C;stroke-width:1.2px;"),
            ggiraph::opts_tooltip(css = .sdt_sim_tooltip_css())
          )
        ))
      })
    }

    if (!have_ggiraph) {
      output$board_static <- .sdt_render_plot({
        shiny::req(state$run)
        .sdt_safely(board_plot())
      })
    }

    # Patient flow ------------------------------------------------------------

    flow_plot <- shiny::reactive({
      shiny::req(state$run)
      # The published board areas carry column names; nobody reading a legend
      # should have to translate "icu_imc". Replacing the scale says so out
      # loud, and ggplot2 notes the replacement, which is not news here.
      suppressMessages(
        sdt_plot_flow(state$run, replication = shown_rep()) +
          ggplot2::scale_colour_viridis_d(
            drop = FALSE, option = "D", labels = .sdt_label_area
          )
      )
    })

    # How many events the shown replication produced. A run can come back with
    # an empty log -- a cohort with nobody in it, or a replication nobody
    # reached -- and sdt_plot_flow() answers that with a bare titled panel,
    # which reads as a broken plot rather than as an empty one.
    flow_rows <- shiny::reactive({
      shiny::req(state$run)
      sum(state$run$events$replication == shown_rep())
    })

    output$flow_ui <- shiny::renderUI({
      if (is.null(state$run)) {
        return(.sdt_empty(
          "No patient flow yet.",
          "Run the simulation to see who was in which area, and for how long."
        ))
      }
      if (!flow_rows()) {
        return(.sdt_empty(
          "This replication put nobody on the board.",
          paste(
            "The run finished with an empty event log. Check that the",
            "scenario's cohort has casualties in it."
          )
        ))
      }
      if (have_plotly) {
        plotly::plotlyOutput(ns("flow_plotly"), height = "100%")
      } else {
        shiny::plotOutput(ns("flow_static"), height = "100%")
      }
    })

    if (have_plotly) {
      output$flow_plotly <- plotly::renderPlotly({
        shiny::req(state$run)
        .sdt_safely(plotly::ggplotly(.sdt_app_plot(flow_plot())))
      })
    }

    if (!have_plotly) {
      output$flow_static <- .sdt_render_plot({
        shiny::req(state$run)
        .sdt_safely(flow_plot())
      })
    }

    # Utilisation -------------------------------------------------------------

    util_plot <- shiny::reactive({
      shiny::req(state$run)
      sdt_plot_utilisation(state$run) +
        ggplot2::scale_colour_hue(labels = .sdt_label_area)
    })

    output$util_ui <- shiny::renderUI({
      if (is.null(state$run)) {
        return(.sdt_empty(
          "No utilisation to show.",
          "Run the simulation to see how hard each area was worked."
        ))
      }
      # simmer writes a resource record only when a casualty releases a
      # resource, so a board nobody reached leaves this table empty.
      if (!nrow(state$run$resources)) {
        return(.sdt_empty(
          "No area was ever occupied.",
          paste(
            "Nothing was released by any board area in this run, so there",
            "is no utilisation to draw."
          )
        ))
      }
      if (have_ggiraph) {
        ggiraph::girafeOutput(ns("util_girafe"), height = "100%")
      } else {
        shiny::plotOutput(ns("util_static"), height = "100%")
      }
    })

    if (have_ggiraph) {
      output$util_girafe <- ggiraph::renderGirafe({
        shiny::req(state$run)
        .sdt_safely(ggiraph::girafe(
          ggobj = .sdt_app_plot(util_plot()),
          bg = "transparent",
          options = list(ggiraph::opts_toolbar(saveaspng = FALSE))
        ))
      })
    }

    if (!have_ggiraph) {
      output$util_static <- .sdt_render_plot({
        shiny::req(state$run)
        .sdt_safely(util_plot())
      })
    }

    # End of game -------------------------------------------------------------

    end_summary <- shiny::reactive({
      shiny::req(state$run)
      summary(state$run, reference = TRUE,
              version = .sdt_sim_version(input$version))
    })

    output$end_ui <- shiny::renderUI({
      if (is.null(state$run)) {
        return(.sdt_empty(
          "Nothing has ended yet.",
          paste(
            "Run the simulation to see where the casualties stood when the",
            "clock stopped, against the published distribution."
          )
        ))
      }
      shiny::plotOutput(ns("end_state"), height = "100%")
    })

    output$end_state <- .sdt_render_plot({
      shiny::req(state$run)
      .sdt_safely(sdt_plot_end_state(
        state$run, version = .sdt_sim_version(input$version)
      ))
    })

    output$end_table_ui <- shiny::renderUI({
      if (is.null(state$run)) {
        return(.sdt_empty(
          "No end-of-game table yet.",
          "The eight published reporting locations are listed here after a run."
        ))
      }
      shiny::tableOutput(ns("end_table"))
    })

    # The tibble is folded onto the eight locations the source reports, and it
    # is shown by its display label: the `area` column is the join key, not
    # something to read.
    output$end_table <- shiny::renderTable({
      shiny::req(state$run)
      .sdt_safely({
        out <- end_summary()
        data.frame(
          Location = out$label,
          Casualties = out$n,
          Simulated = .sdt_sim_pct(out$proportion),
          Published = .sdt_sim_pct(out$reference),
          Difference = .sdt_sim_pp(out$difference),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      })
    }, striped = TRUE, width = "100%", align = "lrrrr")

    output$download <- shiny::downloadHandler(
      filename = function() "sandtablR-events.csv",
      content = function(file) {
        # An empty log still carries its column names, so a download taken
        # before a run explains its own shape rather than failing.
        events <- if (is.null(state$run)) .empty_events() else state$run$events
        utils::write.csv(events, file, row.names = FALSE)
      }
    )

    # The whole workbench is a page_navbar, so this panel is itself a hidden
    # tab until the user opens it, and its own tabs are hidden inside that. A
    # suspended output never renders, so everything the panel is expected to
    # have ready on arrival is kept alive here -- the sidebar, the value boxes,
    # the empty-state wrappers and the widgets inside them.
    always <- c(
      "gate", "status", "vb_rows", "vb_events", "vb_time",
      "board_ui", "flow_ui", "util_ui", "end_ui", "end_table_ui",
      "end_state", "end_table"
    )
    # Only the renderer the UI actually mounts exists at all: defining the
    # unused twin would redraw a plot nobody can see on every scrub.
    mounted <- c(
      always,
      if (have_ggiraph) "board_girafe" else "board_static",
      if (have_plotly) "flow_plotly" else "flow_static",
      if (have_ggiraph) "util_girafe" else "util_static"
    )
    for (nm in mounted) {
      shiny::outputOptions(output, nm, suspendWhenHidden = FALSE)
    }
    invisible(NULL)
  })
}
