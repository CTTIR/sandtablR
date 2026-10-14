# Sequence module -------------------------------------------------------------
# Decision paths as state sequences: the shape of individual pathways, the
# shape of the cohort, the characteristic groupings it falls into, and how
# repeatably each casualty was handled.
#
# Nothing in this file writes the state alphabet out. It is sdt_board_areas(),
# which grew a tenth state, "not_seen", for casualties the game was called
# before reaching. A panel carrying its own nine-state copy would have gone on
# reporting the old denominator without ever failing, so every count, legend
# and chip here derives from the vocabulary function.

mod_sequence_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,

      # The panel does exactly one expensive thing, so it is the first control
      # rather than the last. Everything below it is a knob on a view that
      # already draws itself, and a knob has no business outranking the action.
      shiny::actionButton(
        ns("cluster_go"), "Group pathways into clusters",
        class = "btn-primary w-100"
      ),
      shiny::downloadButton(
        ns("download"), "Download paths",
        class = "btn-outline-secondary w-100 mt-2"
      ),
      shiny::radioButtons(
        ns("view"), "View",
        c("Index plot" = "index", "State distribution" = "distribution")
      ),

      # The settings fold away. Left open they push the primary action off a
      # laptop screen, which is exactly where it stops reading as the obvious
      # next thing to press.
      bslib::accordion(
        open = FALSE,
        class = "sdt-sidebar-group",
        bslib::accordion_panel(
          "Replications",
          shiny::checkboxInput(ns("all_reps"), "Pool all replications",
                               value = TRUE),
          shiny::numericInput(ns("replication"), "Replication", value = 1,
                              min = 1)
        ),
        bslib::accordion_panel(
          "Clustering",
          shiny::checkboxInput(ns("cluster"), "Show clusters", value = FALSE),
          shiny::sliderInput(ns("k"), "Clusters", min = 2, max = 8,
                             value = 3, step = 1)
        ),
        bslib::accordion_panel(
          "Consistency",
          shiny::radioButtons(
            ns("consistency"), "Measure",
            c("Optimal matching" = "om", "Longitudinal entropy" = "entropy")
          )
        )
      ),
      shiny::uiOutput(ns("status"))
    ),

    shiny::uiOutput(ns("guide")),

    # The value is a text output wrapped by .sdt_metric() rather than a whole
    # rendered metric: only the number changes, and the caption beside it is
    # fixed, so there is nothing to gain from re-sending it as HTML. The
    # placeholder for "no data yet" therefore comes from the render below.
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        "Distinct pathways",
        .sdt_metric(
          shiny::textOutput(ns("vb_paths"), inline = TRUE),
          note = "Different routes taken through the board"
        )
      ),
      bslib::value_box(
        "Median consistency",
        .sdt_metric(
          shiny::textOutput(ns("vb_consistency"), inline = TRUE),
          note = "1 = handled the same way in every replication"
        )
      ),
      bslib::value_box(
        "Longest pathway",
        .sdt_metric(
          shiny::textOutput(ns("vb_longest"), inline = TRUE),
          note = "Most positions any one casualty passed through"
        )
      ),
      bslib::value_box(
        .sdt_label_area("not_seen"),
        .sdt_metric(
          shiny::textOutput(ns("vb_unseen"), inline = TRUE),
          note = "Casualties the game was called before reaching"
        )
      )
    ),

    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::nav_panel(
        "Sequences",
        shiny::uiOutput(ns("sequences_ui"), fill = TRUE)
      ),
      bslib::nav_panel(
        "Consistency",
        shiny::uiOutput(ns("consistency_ui"))
      ),
      bslib::nav_panel(
        "Clusters",
        shiny::selectInput(ns("cluster_pick"), "Cluster",
                           choices = character()),
        shiny::uiOutput(ns("cluster_summary")),
        shiny::uiOutput(ns("cluster_paths_ui"))
      ),
      bslib::nav_panel(
        "Casualty",
        shiny::selectInput(ns("casualty"), "Casualty", choices = character()),
        shiny::uiOutput(ns("drill_ui"))
      )
    )
  )
}

mod_sequence_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {

    # Clustering is armed by the primary button and disarmed by the checkbox
    # beside the cluster count. Both write one flag, so the sidebar can never
    # claim clusters are showing while the plots disagree.
    armed <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$cluster_go, {
      armed(TRUE)
      shiny::updateCheckboxInput(session, "cluster", value = TRUE)
    })

    shiny::observeEvent(input$cluster, {
      armed(isTRUE(input$cluster))
    })

    # Computed once and shared: five outputs want the same long-format paths,
    # and rebuilding them per output would run the unseen-casualty fill five
    # times over.
    paths_long <- shiny::reactive({
      shiny::req(state$run)
      .sdt_safely(sdt_decision_paths(state$run))
    })

    # One readable pathway per sequence, and the only place a pathway string
    # is built. The areas go through .sdt_label_area() here, not on the way
    # out of a table cell, so no route to the page can show a reader "ed_red".
    #
    # The distinct-pathway count is taken from these strings too. Counting
    # them off sdt_decision_paths(format = "wide") is the obvious alternative
    # and the wrong one: the wide layout cannot be built at all when every
    # path is a single step, which is exactly the shape a run dominated by
    # "not_seen" has, and the count would then read as an error message in a
    # value box. The labels are a bijection over the alphabet, so counting
    # the labelled strings gives the same number either way.
    pathway_strings <- shiny::reactive({
      long <- paths_long()
      shiny::req(long, nrow(long) > 0L)
      grouped <- dplyr::group_by(long, .data$replication, .data$id)
      dplyr::summarise(
        grouped,
        pathway = paste(.sdt_label_area(.data$area[order(.data$step)]),
                        collapse = " > "),
        .groups = "drop"
      )
    })

    shiny::observeEvent(state$run, {
      shiny::req(state$run)
      # Casualty choices come from the paths, not from the event log: a
      # casualty that was never seen has no events but does have a pathway,
      # and it is the one most worth looking at.
      long <- paths_long()
      shiny::req(long)
      shiny::updateSelectInput(session, "casualty",
                               choices = sort(unique(long$id)))
      shiny::updateNumericInput(session, "replication",
                                max = state$run$meta$replications)
    })

    # A cleared numeric input reads as NA, not NULL, so `%||%` never sees it.
    # Left alone it reaches the filter below as `== NA`, which matches every
    # row weakly and none of them truly: the panel then holds a table of NAs
    # it believes is populated. NA is kept rather than defaulted so the empty
    # state still fires and says which replications exist.
    which_replication <- shiny::reactive({
      if (isTRUE(input$all_reps %||% TRUE)) {
        return(NULL)
      }
      rep <- suppressWarnings(as.integer(input$replication %||% 1L))
      if (length(rep) == 1L) rep else NA_integer_
    })

    shown_paths <- shiny::reactive({
      long <- paths_long()
      shiny::req(long)
      rep <- which_replication()
      if (is.null(rep)) {
        return(long)
      }
      keep <- !is.na(long$replication) & long$replication %in% rep
      long[keep, , drop = FALSE]
    })

    # The state distribution is a share per position taken over the whole
    # cohort, so there is no cluster column left by the time it is drawn and
    # only the index plot can be split by group. The panel says so above the
    # plot rather than handing the reader a facetting error.
    faceted_view <- shiny::reactive({
      identical(input$view %||% "index", "index")
    })

    clusters <- shiny::reactive({
      input$cluster_go
      shiny::req(state$run)
      if (!isTRUE(armed())) {
        return(NULL)
      }
      .sdt_need_pkgs(c("TraMineR", "WeightedCluster"), "Pathway clustering")
      # sdt_seq_cluster() checks k and aborts on anything that is not a whole
      # number, so a slider that has not reported yet must not reach it.
      k <- suppressWarnings(as.integer(input$k %||% 3L))
      if (length(k) != 1L || is.na(k)) {
        k <- 3L
      }
      .sdt_safely(sdt_seq_cluster(state$run, k = max(2L, k)))
    })

    consistency <- shiny::reactive({
      shiny::req(state$run)
      .sdt_safely(sdt_decision_consistency(
        state$run, method = input$consistency %||% "om"
      ))
    })

    shiny::observeEvent(clusters(), {
      cl <- clusters()
      groups <- if (is.null(cl)) integer() else sort(unique(cl$cluster))
      # paste() over an empty vector still returns one string, so the empty
      # case is built rather than named: turning clustering off must clear the
      # picker, not leave it offering a cluster that no longer exists.
      choices <- if (length(groups)) {
        stats::setNames(as.character(groups), paste("Cluster", groups))
      } else {
        character()
      }
      shiny::updateSelectInput(session, "cluster_pick", choices = choices)
    }, ignoreNULL = FALSE)

    # Where you are, and what to press next ------------------------------------

    output$guide <- shiny::renderUI({
      if (is.null(state$run)) {
        return(.sdt_step(1, paste(
          "Simulate a game run first. Every pathway below is read straight",
          "off its event log, so this panel has nothing to say until one",
          "exists."
        )))
      }
      if (!isTRUE(armed())) {
        return(.sdt_step(2, paste(
          "Group the pathways into clusters to see the handful of",
          "characteristic routes the cohort actually took."
        )))
      }
      .sdt_step(3, paste(
        "Open the Clusters tab to read a group's representative pathway and",
        "how many casualties fell into it."
      ))
    })

    output$status <- shiny::renderUI({
      pkgs <- c("TraMineR", "WeightedCluster")
      have <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
      run <- state$run
      htmltools::tagList(
        htmltools::tags$small(class = "text-muted", "Run"),
        htmltools::div(
          if (is.null(run)) {
            .sdt_chip("no run yet", "muted")
          } else {
            htmltools::tagList(
              .sdt_chip(
                paste(nrow(run$scenario$cohort), "casualties"), "green"
              ),
              .sdt_chip(
                paste(run$meta$replications %||% 1L, "replications"), "glass"
              )
            )
          }
        ),
        htmltools::tags$small(class = "text-muted", "Clustering engines"),
        htmltools::div(lapply(pkgs, function(pkg) {
          .sdt_chip(
            if (have[[pkg]]) pkg else paste(pkg, "missing"),
            if (have[[pkg]]) "green" else "yellow"
          )
        })),
        if (!all(have)) {
          htmltools::div(
            class = "sdt-empty-hint",
            sprintf(
              "install.packages(c(\"%s\"))",
              paste(pkgs[!have], collapse = "\", \"")
            )
          )
        },
        htmltools::div(
          class = "sdt-metric-note",
          sprintf("Alphabet: %d board states.", length(sdt_board_areas()))
        )
      )
    })

    # Summary boxes -----------------------------------------------------------

    output$vb_paths <- shiny::renderText({
      if (is.null(state$run)) {
        return(.sdt_seq_dash())
      }
      long <- paths_long()
      if (is.null(long) || !nrow(long)) {
        return(.sdt_seq_dash())
      }
      format(length(unique(pathway_strings()$pathway)))
    })

    output$vb_consistency <- shiny::renderText({
      if (is.null(state$run)) {
        return(.sdt_seq_dash())
      }
      out <- consistency()
      if (is.null(out) || !nrow(out)) {
        return(.sdt_seq_dash())
      }
      value <- stats::median(out$consistency, na.rm = TRUE)
      if (is.na(value)) .sdt_seq_dash() else sprintf("%.2f", value)
    })

    output$vb_longest <- shiny::renderText({
      if (is.null(state$run)) {
        return(.sdt_seq_dash())
      }
      long <- paths_long()
      if (is.null(long) || !nrow(long)) {
        return(.sdt_seq_dash())
      }
      steps <- max(long$step)
      paste(steps, if (steps == 1L) "step" else "steps")
    })

    output$vb_unseen <- shiny::renderText({
      if (is.null(state$run)) {
        return(.sdt_seq_dash())
      }
      long <- paths_long()
      if (is.null(long) || !nrow(long)) {
        return(.sdt_seq_dash())
      }
      format(length(unique(long$id[long$area == "not_seen"])))
    })

    # Sequence plot -----------------------------------------------------------

    # The plot is mounted from a renderUI so that "nothing to draw" is a
    # deliberate placeholder rather than a blank rectangle. A plotOutput cannot
    # say why it is empty, and a blank card is read as a broken one.
    output$sequences_ui <- shiny::renderUI({
      if (is.null(state$run)) {
        return(.sdt_empty(
          "No decision paths yet.",
          paste("Simulate a game run on the Simulate panel. Every casualty's",
                "route across the board is read from its event log.")
        ))
      }
      long <- shown_paths()
      if (is.null(long) || !nrow(long)) {
        reps <- state$run$meta$replications %||% 1L
        return(.sdt_empty(
          "Nothing recorded for that replication.",
          sprintf(
            "The run holds %d replication%s. Pool them, or pick one in range.",
            reps, if (reps == 1L) "" else "s"
          )
        ))
      }
      pooled <- isTRUE(armed()) && !faceted_view()
      htmltools::tagList(
        if (pooled) {
          htmltools::div(
            class = "sdt-empty-hint mb-2",
            paste("Pooled over every cluster. Switch to the index plot to",
                  "see the groups side by side.")
          )
        },
        shiny::plotOutput(session$ns("sequences"), height = "100%")
      )
    })

    output$sequences <- .sdt_render_plot({
      shiny::req(state$run)
      # The same test the placeholder above makes. Suspension is switched off
      # for this output, so it draws whether or not it is mounted, and an
      # out-of-range replication would otherwise reach the plot function and
      # abort behind a placeholder the user is already reading.
      shiny::req(nrow(shown_paths()) > 0L)
      groups <- if (faceted_view()) clusters() else NULL
      .sdt_safely(.sdt_seq_relabel(
        sdt_plot_sequences(
          state$run,
          type = input$view %||% "index",
          replication = which_replication(),
          clusters = groups
        ),
        faceted = !is.null(groups)
      ))
    })

    # Consistency -------------------------------------------------------------

    output$consistency_ui <- shiny::renderUI({
      if (is.null(state$run)) {
        return(.sdt_empty(
          "No run to score.",
          paste("Decision consistency compares a casualty's pathways with",
                "one another, so it needs a simulated run first.")
        ))
      }
      out <- consistency()
      if (is.null(out) || !nrow(out)) {
        return(.sdt_empty(
          "Nothing to score.",
          "The run holds no decision paths to compare."
        ))
      }
      # Optimal matching compares a casualty's paths with one another, so a
      # single-replication run scores every casualty NA -- and one replication
      # is the default. Rendering that as a full table of blanks reads as a
      # broken measure rather than an inapplicable one.
      if (all(is.na(out$consistency))) {
        return(.sdt_empty(
          "Nothing to compare in a single replication.",
          paste("Optimal matching asks whether a casualty is handled the same",
                "way twice, so it needs at least two replications. Re-run the",
                "simulation with more, or switch the measure to longitudinal",
                "entropy.")
        ))
      }
      if (requireNamespace("reactable", quietly = TRUE)) {
        reactable::reactableOutput(session$ns("consistency_reactable"))
      } else {
        shiny::tableOutput(session$ns("consistency_table"))
      }
    })

    consistency_shown <- shiny::reactive({
      out <- consistency()
      shiny::req(out, nrow(out) > 0L)
      out <- out[order(out$consistency), , drop = FALSE]
      data.frame(
        Casualty = out$id,
        Paths = out$n_paths,
        Distinct = out$n_distinct,
        Consistency = out$consistency,
        stringsAsFactors = FALSE
      )
    })

    if (requireNamespace("reactable", quietly = TRUE)) {
      output$consistency_reactable <- reactable::renderReactable({
        .sdt_safely(reactable::reactable(
          consistency_shown(),
          defaultSorted = list(Consistency = "asc"),
          columns = list(
            Consistency = reactable::colDef(
              format = reactable::colFormat(digits = 3)
            )
          ),
          highlight = TRUE, compact = TRUE, fullWidth = TRUE,
          defaultPageSize = 15
        ))
      })
      shiny::outputOptions(output, "consistency_reactable",
                           suspendWhenHidden = FALSE)
    }

    output$consistency_table <- shiny::renderTable({
      utils::head(consistency_shown(), 20L)
    }, digits = 3, striped = TRUE, width = "100%")

    # Cluster drill-down ------------------------------------------------------

    # One gate for both halves of the tab. .sdt_need_pkgs() stops the render
    # outright rather than returning a placeholder, which is right: a panel
    # that cannot be computed at all is not an empty one, and the message
    # names the install command.
    cluster_gate <- function() {
      if (is.null(state$run)) {
        return(.sdt_empty(
          "No run to cluster.",
          "Simulate a game run, then group its pathways."
        ))
      }
      .sdt_need_pkgs(c("TraMineR", "WeightedCluster"), "Pathway clustering")
      if (!isTRUE(armed())) {
        return(.sdt_empty(
          "Pathways are not grouped yet.",
          paste("Press Group pathways into clusters in the sidebar. Nothing",
                "else in the panel changes.")
        ))
      }
      NULL
    }

    # The same question without the refusal. cluster_gate() answers with a
    # placeholder or a validation message, which belongs above the table once
    # and not a second time beneath it.
    cluster_ready <- function() {
      have <- vapply(c("TraMineR", "WeightedCluster"), requireNamespace,
                     logical(1), quietly = TRUE)
      !is.null(state$run) && isTRUE(armed()) && all(have)
    }

    cluster_detail <- shiny::reactive({
      cl <- clusters()
      shiny::req(cl, nrow(cl) > 0L)
      pick <- suppressWarnings(as.integer(input$cluster_pick %||% NA))
      if (is.na(pick) || !pick %in% cl$cluster) {
        pick <- min(cl$cluster)
      }
      rows <- cl[cl$cluster == pick, , drop = FALSE]
      dplyr::left_join(rows, pathway_strings(), by = c("replication", "id"))
    })

    output$cluster_summary <- shiny::renderUI({
      gate <- cluster_gate()
      if (!is.null(gate)) {
        return(gate)
      }
      detail <- cluster_detail()
      shiny::req(nrow(detail) > 0L)
      share <- 100 * nrow(detail) / nrow(clusters())
      medoid <- .sdt_seq_medoid_areas(detail, paths_long())
      htmltools::tagList(
        htmltools::div(
          .sdt_chip(paste(length(unique(detail$id)), "casualties"), "glass"),
          .sdt_chip(paste(nrow(detail), "sequences"), "muted"),
          .sdt_chip(sprintf("%.0f%% of all paths", share), "muted")
        ),
        htmltools::tags$small(class = "text-muted",
                              "Representative pathway (cluster medoid)"),
        htmltools::div(
          class = "mb-3",
          lapply(medoid, function(area) {
            # "not_seen" is the one state that is not a place on the board,
            # so it is chipped differently: reading it as an area would make
            # a never-screened casualty look like a routed one.
            .sdt_chip(
              .sdt_label_area(area),
              if (identical(area, "not_seen")) "yellow" else "muted"
            )
          })
        )
      )
    })

    output$cluster_paths_ui <- shiny::renderUI({
      if (!cluster_ready()) {
        return(NULL)
      }
      if (requireNamespace("reactable", quietly = TRUE)) {
        reactable::reactableOutput(session$ns("cluster_paths_reactable"))
      } else {
        shiny::tableOutput(session$ns("cluster_paths_table"))
      }
    })

    cluster_paths <- shiny::reactive({
      detail <- cluster_detail()
      shiny::req(nrow(detail) > 0L)
      grouped <- dplyr::group_by(detail, .data$pathway)
      out <- dplyr::summarise(
        grouped,
        sequences = dplyr::n(),
        casualties = dplyr::n_distinct(.data$id),
        representative = any(.data$medoid),
        .groups = "drop"
      )
      out <- out[order(-out$sequences), , drop = FALSE]
      data.frame(
        Pathway = out$pathway,
        Sequences = out$sequences,
        Casualties = out$casualties,
        Representative = ifelse(out$representative, "yes", ""),
        stringsAsFactors = FALSE
      )
    })

    if (requireNamespace("reactable", quietly = TRUE)) {
      output$cluster_paths_reactable <- reactable::renderReactable({
        .sdt_safely(reactable::reactable(
          cluster_paths(),
          defaultSorted = list(Sequences = "desc"),
          columns = list(
            Pathway = reactable::colDef(minWidth = 260)
          ),
          highlight = TRUE, compact = TRUE, fullWidth = TRUE,
          defaultPageSize = 10
        ))
      })
      shiny::outputOptions(output, "cluster_paths_reactable",
                           suspendWhenHidden = FALSE)
    }

    output$cluster_paths_table <- shiny::renderTable({
      utils::head(cluster_paths(), 12L)
    }, striped = TRUE, width = "100%")

    # Casualty drill-down -----------------------------------------------------

    output$drill_ui <- shiny::renderUI({
      if (is.null(state$run)) {
        return(.sdt_empty(
          "No pathway to open.",
          "Simulate a run, then pick a casualty to follow step by step."
        ))
      }
      long <- paths_long()
      # A new run repopulates the picker, but the browser still holds the
      # casualty chosen against the old one until it does. Mounting the table
      # for an id this run has never heard of leaves an empty frame behind.
      known <- !is.null(long) && nzchar(input$casualty %||% "") &&
        any(long$id == input$casualty)
      if (!known) {
        return(.sdt_empty(
          "No casualty selected.",
          "Pick one above to see every area it passed through."
        ))
      }
      shiny::tableOutput(session$ns("drill"))
    })

    output$drill <- shiny::renderTable({
      shiny::req(state$run, input$casualty)
      long <- paths_long()
      shiny::req(long)
      out <- long[long$id == input$casualty, , drop = FALSE]
      shiny::req(nrow(out) > 0L)
      data.frame(
        Replication = out$replication,
        Step = out$step,
        Area = .sdt_label_area(out$area),
        Start = round(out$start, 1),
        End = round(out$end, 1),
        Minutes = round(out$duration, 1),
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, width = "100%")

    output$download <- shiny::downloadHandler(
      filename = function() "sandtablR-decision-paths.csv",
      content = function(file) {
        shiny::req(state$run)
        utils::write.csv(sdt_decision_paths(state$run), file,
                         row.names = FALSE)
      }
    )

    # The whole module sits in a nav_panel, and the value boxes sit inside a
    # navset again, so every output here is hidden when the app opens. They
    # are listed exhaustively rather than by what looked visible: the four
    # headline numbers were the ones a reader notices arriving late.
    for (nm in c("guide", "status", "vb_paths", "vb_consistency",
                 "vb_longest", "vb_unseen", "sequences_ui", "sequences",
                 "consistency_ui", "consistency_table", "cluster_summary",
                 "cluster_paths_ui", "cluster_paths_table", "drill_ui",
                 "drill")) {
      shiny::outputOptions(output, nm, suspendWhenHidden = FALSE)
    }
    invisible(NULL)
  })
}

# The em-dash .sdt_metric() shows for a missing value. The metrics here render
# only the number, so the placeholder has to come from the render rather than
# from the wrapper, and it has to be the same character.
.sdt_seq_dash <- function() "\u2014"

# The areas of a cluster's medoid, in order.
#
# sdt_seq_cluster() marks the medoid rather than describing it, because the
# description depends on what you want to show. Here it is the ordered states,
# which is the shortest honest answer to "what does this cluster look like".
.sdt_seq_medoid_areas <- function(detail, long) {
  rows <- detail[detail$medoid %in% TRUE, , drop = FALSE]
  if (!nrow(rows)) {
    rows <- detail[1L, , drop = FALSE]
  }
  hit <- long$replication == rows$replication[[1L]] &
    long$id == rows$id[[1L]]
  long$area[hit][order(long$step[hit])]
}

# Relabel a sequence plot on its way to the page.
#
# sdt_plot_sequences() colours by the stored alphabet, and the stored alphabet
# is column-shaped: "ed_red", "icu_imc". The legend is where a reader meets
# those strings, so the scale is relabelled here rather than in the plot
# function, which the console and the report share and which has no reason to
# prefer the workbench's wording. drop = FALSE is kept so all ten states stay
# in the legend even when a run never reached one.
.sdt_seq_relabel <- function(plot, faceted = FALSE) {
  plot <- suppressMessages(
    plot + ggplot2::scale_fill_viridis_d(
      drop = FALSE, labels = .sdt_label_area
    )
  )
  if (!faceted) {
    return(plot)
  }
  plot + ggplot2::facet_wrap(
    ggplot2::vars(.data$cluster),
    labeller = ggplot2::labeller(
      cluster = function(x) paste("Cluster", x)
    )
  )
}
