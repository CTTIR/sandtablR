# Generate module -------------------------------------------------------------
# Create further simulated groups from decisions already made, score them, and
# hand the whole thing over as one file.
#
# Three exported calls carry the feature. sdt_fit_behaviour() learns how teams
# differ from one another, sdt_simulate_groups() draws teams that were never in
# the data, and sdt_fidelity() says whether the draw is faithful. Everything
# else on this panel is the accounting a reader needs before believing the
# answer: which columns were read, which outcomes could not be fitted and why,
# how many statistics passed, and what the download does and does not carry.
#
# Two things here are unlike the rest of the workbench.
#
# The fit is slow. It is one mixed model per outcome, and the fourteen-outcome
# fit on the validation corpus takes ninety seconds. A button that stalls for a
# minute and a half with no warning reads as a hung app, so the outcome picker
# defaults to a small set, says what a longer one costs before the press, and
# the elapsed time of the last fit is reported afterwards.
#
# And the data need not come from this session at all. The point of the feature
# is to take somebody else's course export and hand back further groups, so
# ingestion is written to name the column that is missing and list the ones
# that were found, rather than to abort. A panel that answers a real export
# with "argument is of length zero" is a panel nobody uses twice.

mod_generate_ui <- function(id) {
  ns <- shiny::NS(id)
  d <- .sdt_gen_defaults()

  bslib::layout_sidebar(
    # Four tables and one chart, all sized by their contents. Left fillable
    # the card stretches to the viewport and leaves a tall screen mostly
    # empty, as in the model lab.
    fillable = FALSE,

    sidebar = bslib::sidebar(
      width = 340,

      # The two presses the panel exists for, first and full width. Everything
      # below them is either what they read or what they write.
      shiny::actionButton(
        ns("fit"), "Fit the behaviour model", class = "btn-primary w-100",
        icon = shiny::icon("play")
      ),
      shiny::actionButton(
        ns("simulate"), "Simulate and score",
        class = "btn-outline-secondary w-100 mt-2"
      ),
      shiny::uiOutput(ns("precondition"), class = "mt-3"),

      shiny::radioButtons(
        ns("source"), "Learn from",
        c("This session's groups" = "session", "An uploaded file" = "file")
      ),
      shiny::fileInput(
        ns("upload"), "Decision file",
        accept = c(".csv", ".rds", ".parquet")
      ),
      shiny::uiOutput(ns("ingest"), class = "mb-3"),

      bslib::accordion(
        # Only the outcome picker is open. It is the one control that decides
        # both what the panel can report and how long the fit takes.
        open = "Outcomes",
        class = "sdt-sidebar-group",
        bslib::accordion_panel(
          "Outcomes",
          shiny::selectInput(
            ns("outcomes"), "Model these", choices = character(),
            multiple = TRUE
          ),
          shiny::uiOutput(ns("cost"))
        ),
        bslib::accordion_panel(
          "Columns",
          shiny::selectInput(ns("group"), "Group", choices = character()),
          shiny::selectInput(ns("card"), "Card", choices = character()),
          shiny::selectInput(ns("block"), "Session", choices = character()),
          htmltools::div(
            class = "sdt-metric-note",
            "One row per group and card. The session column may be left",
            "unset; the fit then has no session term to separate a course",
            "from the teams that played it."
          )
        ),
        bslib::accordion_panel(
          "Fit settings",
          shiny::radioButtons(
            ns("frame"), "Fitting frame",
            c("Composition matched" = "matched", "Every row" = "all")
          ),
          shiny::numericInput(
            ns("min_coverage"), "Minimum card coverage", value = NA,
            min = 1, step = 1
          ),
          htmltools::div(
            class = "sdt-metric-note",
            "Play is truncated by the clock, so groups see different cards.",
            "The matched frame keeps the cards enough groups reached and the",
            "groups that saw all of them, so every group answered the same",
            "questions. Leave the coverage blank to have it chosen from the",
            "data."
          )
        ),
        bslib::accordion_panel(
          "Simulation settings",
          shiny::numericInput(ns("n_groups"), "Simulated groups",
                              value = d$n_groups, min = 2, step = 1),
          shiny::numericInput(ns("nsim"), "Replicates", value = d$nsim,
                              min = 1, step = 1),
          shiny::numericInput(ns("seed"), "Seed", value = d$seed, min = 1),
          shiny::numericInput(ns("n_boot"), "Bootstrap replicates",
                              value = d$n_boot, min = 0, step = 50),
          shiny::checkboxInput(
            ns("new_teams"), "Draw new teams", value = TRUE
          ),
          htmltools::div(
            class = "sdt-metric-note",
            "Unticked, the fitted teams play their cards again. That is a",
            "check on the fit, not a generator of new groups."
          )
        )
      ),

      shiny::hr(),
      shiny::radioButtons(ns("format"), "Download format",
                          .sdt_gen_formats(), inline = TRUE),
      shiny::checkboxInput(ns("pseudonymise"),
                           "Pseudonymise identifiers", value = TRUE),
      shiny::downloadButton(ns("download"), "Download the bundle",
                            class = "btn-outline-secondary w-100"),
      shiny::uiOutput(ns("privacy"))
    ),

    .sdt_step(1, paste(
      "Fit a model of how teams differ, draw further groups from it, then",
      "download the groups, the metrics that judge them and a README saying",
      "what they are not. Every button here calls a function you can also",
      "call from a script."
    )),

    # Above the boxes rather than inside a tab: a failing control and a
    # simulator replaying its training data are the two results a reader must
    # not be able to miss.
    shiny::uiOutput(ns("alerts")),

    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box("Fitting frame", shiny::uiOutput(ns("vb_frame"))),
      bslib::value_box("Outcomes modelled", shiny::uiOutput(ns("vb_fitted"))),
      bslib::value_box(
        "Statistics passed",
        .sdt_metric(
          shiny::textOutput(ns("vb_pass"), inline = TRUE),
          "dispersion ratio inside the passing band"
        )
      )
    ),

    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::nav_panel("Data", shiny::uiOutput(ns("data_ui"))),
      bslib::nav_panel("Fit", shiny::uiOutput(ns("fit_ui"))),
      bslib::nav_panel("Simulated groups", shiny::uiOutput(ns("sim_ui"))),
      bslib::nav_panel("Fidelity", shiny::uiOutput(ns("fidelity_ui")))
    )
  )
}

mod_generate_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {

    # Which table widget this session can mount, decided once. The renderer
    # for an absent package is never defined, so reactable::renderReactable()
    # and DT::renderDT() are only ever named inside these guards; naming one
    # unguarded would take the whole app down as this server runs, rather than
    # degrading one card.
    have_reactable <- requireNamespace("reactable", quietly = TRUE)
    have_dt <- requireNamespace("DT", quietly = TRUE)
    metrics_widget <- if (have_reactable) {
      "metrics_reactable"
    } else if (have_dt) {
      "metrics_dt"
    } else {
      "metrics_table"
    }

    uploaded <- shiny::reactiveVal(NULL)
    upload_problem <- shiny::reactiveVal(NULL)
    fit_notes <- shiny::reactiveVal(character())
    score_notes <- shiny::reactiveVal(character())

    # Ingestion ---------------------------------------------------------------

    shiny::observeEvent(input$upload, {
      shiny::req(input$upload)
      upload <- input$upload
      data <- tryCatch(
        .sdt_gen_read(upload$datapath[[1L]], upload$name[[1L]]),
        error = function(e) {
          upload_problem(conditionMessage(e))
          NULL
        }
      )
      if (is.null(data)) {
        uploaded(NULL)
        return(invisible(NULL))
      }
      upload_problem(NULL)
      uploaded(list(data = data, label = upload$name[[1L]],
                    note = "Read from the uploaded file."))
    })

    # The two paths into the panel, resolved to one shape. NULL means there is
    # nothing to learn from yet, and the reason is said in `problems()`.
    ingested <- shiny::reactive({
      if (identical(input$source %||% "session", "file")) {
        return(uploaded())
      }
      .sdt_gen_session_frame(state$run)
    })

    detected <- shiny::reactive({
      src <- ingested()
      if (is.null(src)) {
        return(NULL)
      }
      .sdt_safely(.sdt_gen_detect(src$data))
    })

    # The pickers follow the data. Every one of them carries an explicit
    # "unset" entry, so a column the detector could not find leaves the
    # mapping empty and is reported, rather than silently becoming whichever
    # column happened to come first.
    shiny::observeEvent(detected(), {
      det <- detected()
      cols <- if (is.null(det)) character() else det$columns
      shiny::updateSelectInput(
        session, "group", choices = c("Choose a column" = "", cols),
        selected = det$group %||% ""
      )
      shiny::updateSelectInput(
        session, "card", choices = c("Choose a column" = "", cols),
        selected = det$card %||% ""
      )
      shiny::updateSelectInput(
        session, "block", choices = c("No session column" = "", cols),
        selected = det$block %||% ""
      )
      shiny::updateSelectInput(
        session, "outcomes", choices = det$choices %||% character(),
        selected = det$default %||% character()
      )
    }, ignoreNULL = FALSE)

    # A control the browser has not drawn yet reads as NULL, which is not the
    # same as a control the user cleared; the first falls back to what was
    # detected, the second means "unset".
    mapping <- shiny::reactive({
      det <- detected()
      if (is.null(det)) {
        return(NULL)
      }
      pick <- function(nm, fallback) {
        value <- input[[nm]]
        if (is.null(value)) {
          return(fallback)
        }
        if (!nzchar(value) || !value %in% det$columns) NULL else value
      }
      list(group = pick("group", det$group), card = pick("card", det$card),
           block = pick("block", det$block))
    })

    chosen_outcomes <- shiny::reactive({
      det <- detected()
      if (is.null(det)) {
        return(character())
      }
      chosen <- input$outcomes
      if (is.null(chosen)) det$default else intersect(chosen, det$available)
    })

    # Everything that stops a fit, in the order a user meets it, said once so
    # the notification, the sidebar line and the empty states cannot disagree.
    problems <- shiny::reactive({
      absent <- .sdt_gen_missing_stack()
      if (length(absent)) {
        return(sprintf(
          "Fitting needs the %s package, which is not installed.",
          paste(absent, collapse = ", ")
        ))
      }
      from_file <- identical(input$source %||% "session", "file")
      if (from_file && !is.null(upload_problem())) {
        return(upload_problem())
      }
      src <- ingested()
      if (is.null(src)) {
        return(if (from_file) {
          "Upload a csv, rds or parquet file of decisions."
        } else {
          paste(
            "This session has nothing to learn from yet. Run the board on",
            "the Simulate tab, where each replication becomes a group, or",
            "upload a file of real decisions."
          )
        })
      }
      det <- detected()
      map <- mapping()
      msgs <- character()
      if (is.null(map$group)) {
        msgs <- c(msgs, .sdt_gen_missing_column("group", det$columns))
      }
      if (is.null(map$card)) {
        msgs <- c(msgs, .sdt_gen_missing_column("card", det$columns))
      }
      if (!length(det$available)) {
        msgs <- c(msgs, .sdt_gen_no_outcome(det$columns))
      } else if (!length(chosen_outcomes())) {
        msgs <- c(msgs, "Choose at least one outcome to model.")
      }
      msgs
    })

    # Checked before the button does any work: a press that silently does
    # nothing is indistinguishable from a broken panel.
    can_fit <- function() {
      msgs <- problems()
      if (!length(msgs)) {
        return(TRUE)
      }
      shiny::showNotification(msgs[[1L]], type = "warning", duration = 12)
      FALSE
    }

    # Fitting -----------------------------------------------------------------

    shiny::observeEvent(input$fit, {
      if (!can_fit()) {
        return(invisible(NULL))
      }
      src <- ingested()
      map <- mapping()
      outcomes <- chosen_outcomes()
      shiny::withProgress(
        message = "Fitting the behaviour model", value = 0.15,
        detail = .sdt_gen_fit_detail(outcomes), {
          notes <- character()
          model <- withCallingHandlers(
            tryCatch(
              sdt_fit_behaviour(
                src$data, outcomes = outcomes, group = map$group,
                block = map$block, card = map$card,
                frame = input$frame %||% "matched",
                min_coverage = .sdt_gen_coverage(input$min_coverage),
                verbose = FALSE
              ),
              error = function(e) {
                shiny::showNotification(conditionMessage(e), type = "error",
                                        duration = 14)
                NULL
              }
            ),
            # A zero team variance and a team SD at the boundary are findings
            # about the outcome rather than failures, and the engine reports
            # them as warnings. Muffled here and shown on the Fit tab, so they
            # survive the press instead of scrolling past in a console.
            warning = function(w) {
              notes <<- c(notes, conditionMessage(w))
              invokeRestart("muffleWarning")
            }
          )
          shiny::incProgress(0.85)
          fit_notes(notes)
          shiny::req(model)
          state$behaviour <- model
          # A new fit invalidates what was drawn from the old one. Leaving a
          # stale simulation beside a new model is worse than showing none.
          state$simulation <- NULL
          state$fidelity <- NULL
          score_notes(character())
          if (length(model$outcomes) < length(outcomes)) {
            shiny::showNotification(
              sprintf("%d of %d outcomes could not be fitted; see the Fit tab.",
                      length(outcomes) - length(model$outcomes),
                      length(outcomes)),
              type = "warning", duration = 10
            )
          }
        }
      )
    })

    # Simulating and scoring --------------------------------------------------

    # One press, two calls. Scoring a simulation is not optional -- an
    # unscored draw is exactly the artefact this feature exists to stop
    # anybody shipping -- so the button does both and reports them separately.
    shiny::observeEvent(input$simulate, {
      model <- state$behaviour
      if (is.null(model)) {
        shiny::showNotification(
          "Fit the behaviour model before drawing groups from it.",
          type = "warning", duration = 8
        )
        return(invisible(NULL))
      }
      shiny::withProgress(message = "Simulating groups", value = 0.2, {
        seed <- .sdt_gen_count(input$seed, 1L)
        notes <- character()
        # Both calls warn about things a reader needs: a card the outcome was
        # never observed on, a reused team, and above all a simulator whose
        # groups are copies of its training data. Muffled here and listed on
        # the Fidelity tab rather than left in a console.
        keep <- function(w) {
          notes <<- c(notes, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
        sim <- withCallingHandlers(
          tryCatch(
            sdt_simulate_groups(
              model,
              n_groups = .sdt_gen_count(input$n_groups, 20L, min = 2L),
              seed = seed, new_teams = !identical(input$new_teams, FALSE),
              nsim = .sdt_gen_count(input$nsim, 1L)
            ),
            error = function(e) {
              shiny::showNotification(conditionMessage(e), type = "error",
                                      duration = 14)
              NULL
            }
          ),
          warning = keep
        )
        score_notes(notes)
        shiny::req(sim)
        state$simulation <- sim
        state$fidelity <- NULL

        shiny::incProgress(0.5, message = "Scoring fidelity")
        metrics <- withCallingHandlers(
          tryCatch(
            sdt_fidelity(sim, n_boot = .sdt_gen_boot(input$n_boot),
                         seed = seed),
            error = function(e) {
              shiny::showNotification(conditionMessage(e), type = "error",
                                      duration = 14)
              NULL
            }
          ),
          warning = keep
        )
        score_notes(notes)
        shiny::req(metrics)
        state$fidelity <- metrics
      })
    })

    # The download ------------------------------------------------------------

    output$download <- shiny::downloadHandler(
      filename = function() {
        sprintf("sandtablR-simulation-%s.zip", format(Sys.Date()))
      },
      content = function(file) {
        sim <- state$simulation
        if (is.null(sim)) {
          shiny::showNotification(
            "Simulate some groups before downloading them.",
            type = "warning", duration = 8
          )
        }
        shiny::req(sim)
        shiny::withProgress(message = "Writing the bundle", value = 0.4, {
          sdt_export_simulation(
            sim, file, format = .sdt_gen_format(input$format),
            metrics = state$fidelity,
            pseudonymise = !identical(input$pseudonymise, FALSE),
            # The browser is handed exactly one path, so the archive is the
            # only shape a download can take. It is Shiny's own temporary
            # file, never a directory the user chose, which is why the git
            # guard is stood down here: it can only produce a spurious
            # refusal on a path nothing will ever be committed from.
            zip = TRUE, overwrite = TRUE, allow_git = TRUE, verbose = FALSE
          )
        })
      }
    )

    # What the sidebar says ---------------------------------------------------

    output$precondition <- shiny::renderUI({
      msgs <- problems()
      if (length(msgs)) {
        return(.sdt_step("!", msgs[[1L]]))
      }
      src <- ingested()
      map <- mapping()
      n_out <- length(chosen_outcomes())
      .sdt_step(2, sprintf(
        "Ready: %d groups, %d cards, %d outcome%s selected.",
        dplyr::n_distinct(src$data[[map$group]]),
        dplyr::n_distinct(src$data[[map$card]]),
        n_out, if (n_out == 1L) "" else "s"
      ))
    })

    output$ingest <- shiny::renderUI({
      problem <- upload_problem()
      from_file <- identical(input$source %||% "session", "file")
      if (from_file && !is.null(problem)) {
        return(htmltools::div(
          class = "sdt-metric-note",
          .sdt_chip("Not read", "red"), problem
        ))
      }
      src <- ingested()
      if (is.null(src)) {
        return(htmltools::div(
          class = "sdt-metric-note",
          .sdt_chip("No data", "muted"),
          "nothing has been read yet"
        ))
      }
      det <- detected()
      htmltools::tagList(
        htmltools::div(
          class = "sdt-metric-note",
          .sdt_chip(src$label, "green"),
          sprintf("%d rows, %d columns", nrow(src$data), ncol(src$data))
        ),
        htmltools::div(lapply(det$available %||% character(), function(nm) {
          .sdt_chip(nm, if (nm %in% .fid_default_controls()) "muted" else
            "glass")
        })),
        htmltools::div(
          class = "sdt-metric-note",
          sprintf("%d outcome%s this frame can support.",
                  length(det$available),
                  if (length(det$available) == 1L) "" else "s")
        )
      )
    })

    output$cost <- shiny::renderUI({
      n <- length(chosen_outcomes())
      slow <- n > .sdt_gen_slow_at()
      htmltools::div(
        class = "sdt-metric-note",
        if (slow) .sdt_chip("Slow", "red"),
        sprintf(
          paste("%d outcome%s selected. Each one is a separate mixed model:",
                "three of them take about 20 seconds on the 4868-row course",
                "corpus, and all fourteen take 91 seconds."),
          n, if (n == 1L) "" else "s"
        )
      )
    })

    output$privacy <- shiny::renderUI({
      on <- !identical(input$pseudonymise, FALSE)
      htmltools::div(
        class = "sdt-metric-note",
        .sdt_chip(if (on) "Pseudonymised" else "Identifiers kept",
                  if (on) "green" else "red"),
        if (on) {
          paste(
            "Course, session and group identifiers are replaced by stable",
            "codes throughout the tables, the README and the manifest. The",
            "crosswalk from code back to identifier is never written into",
            "the archive, so the download cannot be re-identified from",
            "itself. One archive, holding the groups, the metrics, the deck,",
            "a README and a manifest."
          )
        } else {
          paste(
            "Identifiers travel with the file. A course identifier carries",
            "the hospital, the venue, the city and the exact date, so leave",
            "this ticked unless the recipient is entitled to all four."
          )
        }
      )
    })

    # Headline ----------------------------------------------------------------
    #
    # None of these is written with req(): a value box whose body stops
    # rendering is drawn as a titled grey slab, which reads as broken rather
    # than as waiting. Every box returns something in every state.

    output$vb_frame <- shiny::renderUI({
      model <- state$behaviour
      if (is.null(model)) {
        return(.sdt_metric(NULL, "Nothing fitted yet"))
      }
      meta <- model$meta
      .sdt_metric(
        sprintf("%d \u00d7 %d", meta$n_cards, meta$n_groups),
        sprintf(
          "cards \u00d7 groups on the %s frame, from %d groups offered; %.1fs",
          if (identical(model$frame, "matched")) "matched" else "full",
          meta$n_groups_input, meta$elapsed
        )
      )
    })

    output$vb_fitted <- shiny::renderUI({
      model <- state$behaviour
      if (is.null(model)) {
        return(.sdt_metric(NULL, "Nothing fitted yet"))
      }
      n_fit <- length(model$outcomes)
      n_skip <- nrow(model$skipped)
      .sdt_metric(
        sprintf("%d of %d", n_fit, n_fit + n_skip),
        "outcomes modelled; every other one is listed with its reason"
      )
    })

    output$vb_pass <- shiny::renderText({
      metrics <- state$fidelity
      if (is.null(metrics) || !nrow(metrics)) {
        return("\u2014")
      }
      sprintf("%d of %d", sum(metrics$verdict %in% "PASS"), nrow(metrics))
    })

    # The two results a reader must not be able to miss, plus a plain summary
    # of the rest so that the strip is never silently empty after a score.
    output$alerts <- shiny::renderUI({
      metrics <- state$fidelity
      if (is.null(metrics) || !nrow(metrics)) {
        return(NULL)
      }
      items <- list()
      # %in% rather than ==, so a verdict or a control flag that arrived as NA
      # leaves the banner silent instead of subscripting the table with NA.
      failing <- metrics$verdict %in% "FAIL"
      is_control <- metrics$control %in% TRUE
      controls <- metrics$statistic[is_control & failing]
      if (length(controls)) {
        items <- c(items, list(.sdt_gen_alert(
          "red", "Control failed",
          sprintf(
            paste("%s %s fixed by the patient card, not chosen by the team,",
                  "so a control is expected to pass even for a simulator",
                  "with no team term at all. A failing control means the",
                  "frame or the pipeline is wrong, not the model: check the",
                  "matched frame before reading anything else in the table.",
                  "The one exception is a simulator with no team layer of",
                  "any kind, which flattens its controls completely."),
            paste(controls, collapse = ", "),
            if (length(controls) == 1L) "is" else "are"
          )
        )))
      }
      novelty <- attr(metrics, "novelty")
      if (!is.null(novelty) && isTRUE(novelty$share > 0.5)) {
        items <- c(items, list(.sdt_gen_alert(
          "red", "Novelty",
          sprintf(
            paste("%.0f%% of the simulated groups are exact copies of",
                  "observed ones. Dispersion reproduced by replaying the",
                  "training data is not evidence of anything; check that new",
                  "teams are being drawn rather than reused."),
            100 * novelty$share
          )
        )))
      }
      failed <- metrics$statistic[!is_control & failing]
      if (length(failed)) {
        items <- c(items, list(.sdt_gen_alert(
          "yellow", "Dispersion off",
          sprintf(
            paste("%s outside the passing band. A ratio above one means the",
                  "simulator collapses the spread of teams; below one, that",
                  "it invents spread that is not there."),
            paste(failed, collapse = ", ")
          )
        )))
      }
      if (!length(items)) {
        items <- list(.sdt_gen_alert(
          "green", "All passed",
          paste("Every statistic sits inside the passing band and within a",
                "quarter of an observed between-group SD of the observed",
                "level.")
        ))
      }
      htmltools::tagList(items)
    })

    # Data --------------------------------------------------------------------

    output$data_ui <- shiny::renderUI({
      src <- ingested()
      if (is.null(src)) {
        return(.sdt_empty(
          "Nothing to learn from yet.",
          paste("Choose this session's groups, or upload a csv, rds or",
                "parquet file holding one row per group and card. The panel",
                "names the column it cannot find rather than refusing the",
                "file.")
        ))
      }
      htmltools::tagList(
        htmltools::div(
          class = "sdt-metric-note mb-2",
          sprintf("%d rows and %d columns from %s. %s", nrow(src$data),
                  ncol(src$data), src$label, src$note %||% "")
        ),
        shiny::tableOutput(session$ns("mapping_table")),
        htmltools::div(
          class = "sdt-metric-note mb-3",
          "Detected is what the column names suggested; Column is what the",
          "fit will use. Override either under Columns in the sidebar."
        ),
        shiny::tableOutput(session$ns("preview")),
        htmltools::div(
          class = "sdt-metric-note",
          "The first rows, at most eight columns wide."
        )
      )
    })

    output$mapping_table <- shiny::renderTable({
      det <- detected()
      map <- mapping()
      shiny::req(det, map)
      .sdt_gen_mapping_display(det, map)
    }, striped = TRUE, width = "100%", na = "\u2014")

    output$preview <- shiny::renderTable({
      src <- ingested()
      shiny::req(src)
      .sdt_gen_preview(src$data)
    }, striped = TRUE, width = "100%", digits = 3, na = "\u2014")

    # Fit ---------------------------------------------------------------------

    output$fit_ui <- shiny::renderUI({
      model <- state$behaviour
      if (is.null(model)) {
        return(.sdt_gen_empty(
          if (is.null(ingested())) "data" else "fit", "fitted model"
        ))
      }
      htmltools::tagList(
        shiny::tableOutput(session$ns("fit_table")),
        htmltools::div(
          class = "sdt-metric-note mb-3",
          "The team SD is the spread of teams on the logit scale, and it is",
          "the quantity a simulated group is drawn from: a zero means the",
          "teams did not differ and every simulated group will be identical,",
          "which is a finding about the outcome rather than a failure of the",
          "fit. A team SD above 5 is separation, and simulated teams will be",
          "over-dispersed in the tail."
        ),
        shiny::tableOutput(session$ns("skip_table")),
        shiny::uiOutput(session$ns("fit_notes"))
      )
    })

    output$fit_table <- shiny::renderTable({
      model <- state$behaviour
      shiny::req(model)
      .sdt_gen_fit_display(model)
    }, striped = TRUE, width = "100%", na = "\u2014")

    # Always rendered, and never empty: an outcome that could not be fitted
    # has to be visible with its reason, and "nothing was skipped" is itself
    # worth saying rather than leaving a blank where a table would be.
    output$skip_table <- shiny::renderTable({
      model <- state$behaviour
      shiny::req(model)
      .sdt_gen_skip_display(model$skipped)
    }, striped = TRUE, width = "100%", na = "\u2014")

    output$fit_notes <- shiny::renderUI({
      model <- state$behaviour
      if (is.null(model)) {
        return(NULL)
      }
      notes <- fit_notes()
      if (!length(notes)) {
        return(htmltools::div(
          class = "sdt-metric-note",
          sprintf("The fit reported nothing unusual. Elapsed %.1fs.",
                  model$meta$elapsed %||% NA_real_)
        ))
      }
      htmltools::div(
        class = "sdt-metric-note",
        htmltools::div(.sdt_chip("What the fit reported", "muted")),
        htmltools::tags$ul(lapply(notes, function(note) {
          htmltools::tags$li(.sdt_gen_one_line(note))
        }))
      )
    })

    # Simulated groups --------------------------------------------------------

    output$sim_ui <- shiny::renderUI({
      sim <- state$simulation
      if (is.null(sim)) {
        return(.sdt_gen_empty(
          if (is.null(state$behaviour)) "fit" else "sim", "simulated groups"
        ))
      }
      htmltools::tagList(
        htmltools::div(
          class = "sdt-metric-note mb-2", .sdt_gen_sim_summary(sim)
        ),
        shiny::tableOutput(session$ns("sim_preview")),
        htmltools::div(
          class = "sdt-metric-note",
          "The first rows of the draw. Every simulated group is dealt every",
          "card in the fitted frame, and a group label is unique across the",
          "replicates, so grouping by it never merges two teams."
        )
      )
    })

    output$sim_preview <- shiny::renderTable({
      sim <- state$simulation
      shiny::req(sim)
      .sdt_gen_preview(sim, rows = 12L)
    }, striped = TRUE, width = "100%", na = "\u2014")

    # Fidelity ----------------------------------------------------------------

    output$fidelity_ui <- shiny::renderUI({
      metrics <- state$fidelity
      if (is.null(metrics) || !nrow(metrics)) {
        return(.sdt_gen_empty(
          if (is.null(state$behaviour)) "fit" else "sim", "fidelity table"
        ))
      }
      widget <- switch(
        metrics_widget,
        metrics_reactable = reactable::reactableOutput(
          session$ns("metrics_reactable")
        ),
        metrics_dt = DT::DTOutput(session$ns("metrics_dt")),
        shiny::tableOutput(session$ns("metrics_table"))
      )
      htmltools::tagList(
        widget,
        htmltools::div(
          class = "sdt-metric-note mt-2 mb-3",
          .sdt_gen_band_note(metrics)
        ),
        shiny::plotOutput(session$ns("fidelity_plot"), height = "420px"),
        htmltools::div(
          class = "sdt-metric-note mt-2",
          "A control row is read backwards from the others: see the banner",
          "above the boxes. A ratio of Inf is a simulator with no",
          "between-group spread at all, drawn as an arrow at the right-hand",
          "edge rather than dropped from the chart."
        ),
        shiny::uiOutput(session$ns("score_notes"))
      )
    })

    metrics_display <- shiny::reactive({
      metrics <- state$fidelity
      shiny::req(metrics, nrow(metrics) > 0L)
      .sdt_safely(.sdt_gen_metrics_display(metrics))
    })

    if (have_reactable) {
      output$metrics_reactable <- reactable::renderReactable({
        shown <- metrics_display()
        shiny::req(shown)
        .sdt_safely(reactable::reactable(
          shown,
          columns = .sdt_gen_metrics_columns(shown),
          defaultColDef = reactable::colDef(vAlign = "center"),
          theme = .sdt_model_board_theme(),
          highlight = TRUE, compact = TRUE, fullWidth = TRUE,
          sortable = TRUE, borderless = TRUE
        ))
      })
    }

    if (have_dt) {
      output$metrics_dt <- DT::renderDT({
        shown <- metrics_display()
        shiny::req(shown)
        marked <- .sdt_gen_metrics_chips(shown)
        .sdt_safely(DT::datatable(
          marked, rownames = FALSE, selection = "none",
          # Only the two chip columns carry markup; everything else, the
          # reason a statistic failed included, is escaped.
          escape = setdiff(names(marked), c("Verdict", "Role")),
          options = list(dom = "t", paging = FALSE, order = list())
        ))
      }, server = FALSE)
    }

    output$metrics_table <- shiny::renderTable({
      shown <- metrics_display()
      shiny::req(shown)
      shown
    }, striped = TRUE, width = "100%", na = "\u2014")

    output$score_notes <- shiny::renderUI({
      notes <- score_notes()
      if (!length(notes)) {
        return(NULL)
      }
      htmltools::div(
        class = "sdt-metric-note",
        htmltools::div(.sdt_chip("What the draw reported", "muted")),
        htmltools::tags$ul(lapply(notes, function(note) {
          htmltools::tags$li(.sdt_gen_one_line(note))
        }))
      )
    })

    output$fidelity_plot <- .sdt_render_plot({
      metrics <- state$fidelity
      shiny::req(metrics, nrow(metrics) > 0L)
      .sdt_safely(sdt_plot_fidelity(metrics))
    })

    # The panel is a nav_panel of the app's own tabset, and four of its cards
    # are nav_panels again inside it, so on first paint every output here is
    # hidden and a suspended output renders blank on arrival. Only the widget
    # the UI actually mounts is kept alive beside the rest; unsuspending the
    # fallbacks as well would redraw a table nobody can see on every press.
    mounted <- c(
      "precondition", "ingest", "cost", "privacy", "alerts",
      "vb_frame", "vb_fitted", "vb_pass",
      "data_ui", "mapping_table", "preview",
      "fit_ui", "fit_table", "skip_table", "fit_notes",
      "sim_ui", "sim_preview",
      "fidelity_ui", "score_notes", "fidelity_plot", metrics_widget
    )
    for (nm in mounted) {
      shiny::outputOptions(output, nm, suspendWhenHidden = FALSE)
    }
    invisible(NULL)
  })
}


# Settings --------------------------------------------------------------------

# Named once, so the sidebar's defaults and the sanitisers that guard the
# engine cannot disagree about what "unset" means.
.sdt_gen_defaults <- function() {
  list(n_groups = 20L, nsim = 1L, seed = 1L, n_boot = 100L)
}

# Above this many outcomes the fit is worth warning about before it is asked
# for. Three take about twenty seconds on the course corpus, which a progress
# bar carries; beyond that a press starts to look like a hung app.
.sdt_gen_slow_at <- function() 3L

# A number arriving from a control, sanitised. A cleared numericInput arrives
# as NA and one the browser has not drawn yet arrives as NULL; the engine
# aborts on both, so neither is allowed past this point.
.sdt_gen_count <- function(x, fallback, min = 1L) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) && x >= min
  as.integer(if (ok) x else fallback)
}

.sdt_gen_boot <- function(x) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) && x >= 0
  as.integer(if (ok) x else 0L)
}

# Blank means "choose it from the data", which is what sdt_fit_behaviour()
# reads NULL as. A half-typed number must not become a coverage of 3.
.sdt_gen_coverage <- function(x) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) && x >= 1
  if (ok) as.integer(x) else NULL
}

# Only the formats this session can actually write. nanoparquet is in
# Suggests, and offering a format that aborts in the download handler is
# worse than not offering it.
.sdt_gen_formats <- function() {
  formats <- c(CSV = "csv", RDS = "rds")
  if (requireNamespace("nanoparquet", quietly = TRUE)) {
    formats <- c(formats, Parquet = "parquet")
  }
  formats
}

.sdt_gen_format <- function(x) {
  if (is.character(x) && length(x) == 1L && x %in% .sdt_gen_formats()) {
    x
  } else {
    "csv"
  }
}

# The mixed-model engine, which is in Suggests. Reported rather than assumed,
# so a session without it finds a panel that says why instead of an app that
# fails to start.
.sdt_gen_missing_stack <- function() {
  pkgs <- "lme4"
  pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
}

# A cli condition is several lines of bullets; a notification and a list item
# both want one.
.sdt_gen_one_line <- function(x) {
  substr(gsub("\\s+", " ", paste(x, collapse = " ")), 1L, 240L)
}


# Ingestion -------------------------------------------------------------------

.sdt_gen_group_names <- function() {
  c("group_id", "sim_group", "group", "team_id", "team", "gid")
}

.sdt_gen_card_names <- function() {
  c("card", "card_id", "casualty_id", "casualty", "patient_id", "id")
}

.sdt_gen_block_names <- function() {
  c("session_id", "session", "block", "course_id", "course", "sid")
}

# Case-insensitive, because a course export writes "Group" as often as
# "group_id", but the column that comes back is the one the frame really has.
.sdt_gen_first_of <- function(nms, candidates) {
  hit <- stats::na.omit(match(tolower(candidates), tolower(nms)))
  if (length(hit)) nms[[hit[[1L]]]] else NULL
}

.sdt_gen_read <- function(path, name, call = rlang::caller_env()) {
  ext <- tolower(sub(".*\\.", "", name))
  data <- switch(
    ext,
    csv = utils::read.csv(path, stringsAsFactors = FALSE,
                          check.names = FALSE),
    rds = readRDS(path),
    parquet = {
      .require_pkgs("nanoparquet", call = call)
      nanoparquet::read_parquet(path)
    },
    cli::cli_abort(
      c("Cannot read a {.file {name}}.",
        "i" = "The panel reads {.file .csv}, {.file .rds} and
               {.file .parquet}."),
      class = "sandtablR_error_generate", call = call
    )
  )
  if (!is.data.frame(data)) {
    got <- class(data)[[1L]]
    cli::cli_abort(
      c("That file does not hold a table.",
        "x" = "It contains a {.cls {got}}.",
        "i" = "The panel needs one row per group and card."),
      class = "sandtablR_error_generate", call = call
    )
  }
  if (!nrow(data)) {
    cli::cli_abort(
      c("That file holds a table with no rows.",
        "i" = "The panel needs one row per group and card."),
      class = "sandtablR_error_generate", call = call
    )
  }
  tibble::as_tibble(data)
}

# The session's own path into the panel. A cohort is one group and cannot
# carry a between-group term, so what this reads is the run: each replication
# of the board is a group and each casualty is a card, which is exactly the
# shape sdt_fit_behaviour() wants.
.sdt_gen_session_frame <- function(run) {
  if (is.null(run)) {
    return(NULL)
  }
  frame <- tryCatch(sdt_run_behaviour(run), error = function(e) NULL)
  if (is.null(frame) || !nrow(frame)) {
    return(NULL)
  }
  list(
    data = frame, label = "this session's run",
    note = paste("One group per replication of the run on the Simulate tab,",
                 "one card per casualty. The engine reads the sighting",
                 "category off the card rather than choosing it, so those",
                 "outcomes have no team spread to learn.")
  )
}

# Which outcomes this frame can actually support: everything the catalogue can
# build from the columns present, plus any other logical or 0/1 column. A
# column the catalogue already reads is not offered twice under its own name.
.sdt_gen_outcome_choices <- function(data, keys) {
  nms <- names(data)
  specs <- .beh_catalogue()
  named <- vapply(specs, `[[`, character(1), "name")
  ok <- vapply(specs, .beh_available, logical(1), nms = nms)
  sources <- unique(unlist(lapply(specs[ok], function(s) unlist(s$needs))))
  plain <- setdiff(nms, c(keys, named, sources))
  if (length(plain)) {
    flags <- vapply(plain, function(nm) !is.null(.beh_flag(data[[nm]])),
                    logical(1))
    plain <- plain[flags]
  }
  c(named[ok], plain)
}

.sdt_gen_outcome_labels <- function(available) {
  specs <- .beh_catalogue()
  names(specs) <- vapply(specs, `[[`, character(1), "name")
  labels <- vapply(available, function(nm) {
    spec <- specs[[nm]]
    if (is.null(spec)) nm else sprintf("%s \u2014 %s", nm, spec$label)
  }, character(1))
  stats::setNames(available, unname(labels))
}

# Two behavioural outcomes and one negative control. Small enough to sit
# behind a button, and still enough for a fidelity table that can be read:
# without a control there is nothing to tell a wrong frame from a wrong model.
.sdt_gen_default_outcomes <- function(available) {
  controls <- .fid_default_controls()
  behaviour <- setdiff(available, controls)
  c(utils::head(behaviour, 2L), utils::head(intersect(controls, available),
                                            1L))
}

.sdt_gen_detect <- function(data) {
  nms <- names(data)
  group <- .sdt_gen_first_of(nms, .sdt_gen_group_names())
  card <- .sdt_gen_first_of(nms, .sdt_gen_card_names())
  block <- .sdt_gen_first_of(nms, .sdt_gen_block_names())
  available <- .sdt_gen_outcome_choices(data, c(group, card, block))
  list(
    columns = nms, group = group, card = card, block = block,
    available = available, choices = .sdt_gen_outcome_labels(available),
    default = .sdt_gen_default_outcomes(available)
  )
}

# A missing column is named, and so is everything that was found. A panel that
# answers a real course export with a bare error is a panel nobody uses twice.
.sdt_gen_missing_column <- function(role, columns) {
  shown <- utils::head(columns, 12L)
  more <- length(columns) - length(shown)
  sprintf(
    "No %s column was recognised. Choose one under Columns. Found: %s%s.",
    role, paste(shown, collapse = ", "),
    if (more > 0L) sprintf(" and %d more", more) else ""
  )
}

.sdt_gen_no_outcome <- function(columns) {
  sprintf(
    paste("No outcome column was recognised. The panel reads therapy,",
          "called_triage, general_location, ventilated, died and the did_*",
          "flags, or any logical column. Found: %s."),
    paste(utils::head(columns, 12L), collapse = ", ")
  )
}

.sdt_gen_fit_detail <- function(outcomes) {
  sprintf("%d outcome%s, one mixed model each", length(outcomes),
          if (length(outcomes) == 1L) "" else "s")
}


# Empty states and alerts -----------------------------------------------------

# Each card here can be empty for one of three reasons, and they need
# different answers. Saying which is the difference between a panel that looks
# broken and one that looks like it is waiting.
.sdt_gen_empty <- function(stage, what) {
  hint <- switch(
    stage,
    data = paste("Choose this session's groups, or upload a file of",
                 "decisions. The panel reads csv, rds and parquet, and names",
                 "the column it cannot find rather than refusing the file."),
    fit = paste("Press Fit the behaviour model to learn how the teams in",
                "this frame differ from one another."),
    paste("Press Simulate and score to draw teams that were never in the",
          "data, and judge them against the ones that were.")
  )
  .sdt_empty(sprintf("No %s yet.", what), hint)
}

# A coloured banner built from the workbench's own chip and step styles, so a
# result that must not be missed is loud without a new stylesheet.
.sdt_gen_alert <- function(kind, title, text) {
  htmltools::div(
    class = "sdt-step",
    .sdt_chip(title, kind),
    htmltools::span(text)
  )
}


# Tables ----------------------------------------------------------------------

# One glyph means "no value" across the whole workbench.
.sdt_gen_dash <- function() "\u2014"

.sdt_gen_fmt <- function(x, digits = 3L) {
  ifelse(is.na(x), .sdt_gen_dash(),
         formatC(as.numeric(x), format = "f", digits = digits))
}

.sdt_gen_interval <- function(lo, hi) {
  ifelse(is.na(lo) | is.na(hi), .sdt_gen_dash(),
         sprintf("%s to %s", .sdt_gen_fmt(lo, 2L), .sdt_gen_fmt(hi, 2L)))
}

# A column a hand-built table may not carry, so that the panel can render a
# metrics table it did not compute itself.
.sdt_gen_col <- function(x, nm, fallback) {
  if (is.null(x[[nm]])) rep(fallback, nrow(x)) else x[[nm]]
}

.sdt_gen_mapping_display <- function(det, map) {
  value <- function(x) if (is.null(x)) .sdt_gen_dash() else x
  data.frame(
    Role = c("Group", "Card", "Session"),
    Column = c(value(map$group), value(map$card), value(map$block)),
    Detected = c(value(det$group), value(det$card), value(det$block)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# A peek at what was read, not a browsable table: the first rows and the first
# few columns. Anything that is not an atomic vector is shown as its type,
# because renderTable() prints a list column as a mangled string.
.sdt_gen_preview <- function(data, rows = 6L, cols = 8L) {
  out <- as.data.frame(data)
  out <- out[seq_len(min(nrow(out), rows)),
             seq_len(min(ncol(out), cols)), drop = FALSE]
  for (nm in names(out)) {
    if (!is.atomic(out[[nm]])) {
      out[[nm]] <- vapply(out[[nm]], function(v) class(v)[[1L]],
                          character(1))
    }
  }
  rownames(out) <- NULL
  out
}

# One row per modelled outcome. The team SD is the quantity a simulated group
# is drawn from, so the two flags that qualify it travel beside it rather than
# in a warning that scrolled past.
.sdt_gen_fit_display <- function(model) {
  sds <- model$meta$sds
  flag <- ifelse(
    sds$singular, "zero team variance",
    ifelse(sds$boundary, "separation", .sdt_gen_dash())
  )
  data.frame(
    Outcome = sds$outcome,
    What = sds$label,
    Episodes = sds$n_obs,
    Missing = sds$n_missing,
    Groups = sds$n_groups,
    Prevalence = sprintf("%.1f%%", 100 * sds$prevalence),
    `Team SD` = .sdt_gen_fmt(sds$sd_group, 2L),
    `Session SD` = .sdt_gen_fmt(sds$sd_block, 2L),
    Flag = flag,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# Never empty. An outcome that could not be fitted has to be visible with its
# reason, and "nothing was skipped" is itself worth saying rather than leaving
# a blank where a table would be.
.sdt_gen_skip_display <- function(skipped) {
  if (is.null(skipped) || !nrow(skipped)) {
    return(data.frame(
      Outcome = .sdt_gen_dash(), Stage = "none",
      `Why it was not modelled` =
        "Every outcome asked for was modelled.",
      check.names = FALSE, stringsAsFactors = FALSE
    ))
  }
  data.frame(
    Outcome = skipped$outcome,
    Stage = ifelse(skipped$stage == "fit", "at the fit", "before the fit"),
    `Why it was not modelled` = skipped$reason,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

.sdt_gen_sim_summary <- function(sim) {
  n_groups <- dplyr::n_distinct(sim$sim_group)
  n_cards <- dplyr::n_distinct(sim$card)
  n_reps <- if (".replicate" %in% names(sim)) {
    dplyr::n_distinct(sim$.replicate)
  } else {
    1L
  }
  new_teams <- !identical(attr(sim, "new_teams"), FALSE)
  sprintf(
    paste("%d simulated group%s over %d cards in %d replicate%s: %d rows.",
          "%s"),
    n_groups, if (n_groups == 1L) "" else "s", n_cards, n_reps,
    if (n_reps == 1L) "" else "s", nrow(sim),
    if (new_teams) {
      paste("Each group is a new team in a new session, with its own random",
            "effect drawn afresh.")
    } else {
      paste("The fitted teams are playing their cards again, which is a",
            "check on the fit rather than a draw of new teams.")
    }
  )
}

.sdt_gen_band_note <- function(metrics) {
  band <- attr(metrics, "band") %||% c(0.90, 1.15)
  tol <- attr(metrics, "location_tol") %||% 0.25
  n_boot <- attr(metrics, "n_boot") %||% 0L
  frame <- attr(metrics, "frame")
  where <- if (is.null(frame)) {
    ""
  } else {
    sprintf(
      " Scored on %d cards shared by %d observed and %d simulated groups.",
      frame$n_cards, frame$n_obs_groups, frame$n_sim_groups
    )
  }
  interval <- if (n_boot > 0L) {
    sprintf(
      " The interval resamples whole groups %d times on each side.", n_boot
    )
  } else {
    " No bootstrap was asked for, so the interval is blank."
  }
  sprintf(
    paste("A statistic passes when the observed between-group SD divided by",
          "the simulated one lies inside %.2f to %.2f, and the pooled rate",
          "is within %.2f observed between-group SDs of the observed",
          "level.%s%s"),
    band[[1L]], band[[2L]], tol, where, interval
  )
}

# The fidelity table as words. All three widgets show the same columns; only
# the chips differ, because renderTable() escapes what it is given and would
# print a span at the reader.
.sdt_gen_metrics_display <- function(metrics) {
  x <- tibble::as_tibble(metrics)
  reason <- as.character(.sdt_gen_col(x, "reason", ""))
  data.frame(
    Statistic = as.character(x$statistic),
    Role = ifelse(as.logical(x$control), "control", "behaviour"),
    `Observed SD` = .sdt_gen_fmt(x$observed),
    `Simulated SD` = .sdt_gen_fmt(x$simulated),
    Ratio = .sdt_gen_fmt(x$ratio, 2L),
    Interval = .sdt_gen_interval(.sdt_gen_col(x, "ci_lower", NA_real_),
                                 .sdt_gen_col(x, "ci_upper", NA_real_)),
    `Location z` = .sdt_gen_fmt(.sdt_gen_col(x, "location_z", NA_real_), 2L),
    Verdict = as.character(x$verdict),
    Why = ifelse(is.na(reason) | !nzchar(reason), .sdt_gen_dash(), reason),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

.sdt_gen_metrics_chips <- function(shown) {
  shown$Verdict <- vapply(shown$Verdict, function(v) {
    as.character(.sdt_chip(v, if (identical(v, "PASS")) "green" else "red"))
  }, character(1), USE.NAMES = FALSE)
  shown$Role <- vapply(shown$Role, function(v) {
    as.character(.sdt_chip(v, if (identical(v, "control")) "yellow" else
      "glass"))
  }, character(1), USE.NAMES = FALSE)
  shown
}

.sdt_gen_metrics_columns <- function(shown) {
  cols <- list(
    Statistic = reactable::colDef(minWidth = 130),
    Role = reactable::colDef(
      minWidth = 110,
      cell = function(value) {
        .sdt_chip(value, if (identical(value, "control")) "yellow" else
          "glass")
      }
    ),
    Verdict = reactable::colDef(
      minWidth = 100,
      cell = function(value) {
        .sdt_chip(value, if (identical(value, "PASS")) "green" else "red")
      }
    ),
    Why = reactable::colDef(minWidth = 190)
  )
  cols[intersect(names(cols), names(shown))]
}
