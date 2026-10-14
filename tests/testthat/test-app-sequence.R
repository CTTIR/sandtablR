# The sequence panel, driven headlessly.
#
# This module's whole subject is a vocabulary: ten board states, labelled for
# a reader, counted in a value box and grouped into characteristic routes.
# Everything below asks whether it still speaks that vocabulary. A nine-state
# denominator, a raw column name reaching the page, or a cluster count that
# disagrees with the slider would all leave R CMD check at zero notes and be
# wrong in front of a game leader.

skip_if_no_shiny <- function() {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("htmltools")
}

skip_if_no_clustering <- function() {
  skip_if_not_installed("TraMineR")
  skip_if_not_installed("WeightedCluster")
}

# A reactiveValues store standing in for the app's shared state.
fake_state <- function(...) {
  shiny::reactiveValues(scenario = NULL, run = NULL, models = NULL,
                        bench = NULL, ...)
}

# renderUI hands back the tags *and* their HTML dependencies, so as.character()
# returns more than one string and expect_match() would quietly demand that
# every one of them match. Flattening first keeps the assertions about markup.
as_html <- function(x) paste(as.character(x), collapse = " ")

seq_scenario <- function(n = 20) {
  co <- sdt_add_decisions(sdt_synthetic_cohort(n = n, seed = 1), seed = 2)
  sdt_scenario(co, resources = list(or = 2, icu_imc = 4, ward = 20))
}

# The two-replication run most of the file works from, simulated once. A dozen
# tests want the same run, and the module never writes to state$run, so paying
# for it once is the difference between a fast file and a slow one.
seq_run <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- sdt_simulate(seq_scenario(), replications = 2, seed = 1,
                              verbose = FALSE)
    }
    cached
  }
})

# A one-minute game: one replication, and every casualty still standing at
# screening when the clock stops, so every pathway is a single step.
seq_run_short <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- sdt_simulate(seq_scenario(), replications = 1, seed = 1,
                              horizon = 1, verbose = FALSE)
    }
    cached
  }
})

# A run the game was called before reaching two of its casualties.
#
# sdt_simulate() cannot produce one: .arrival_times() scales the tranche
# schedule by the horizon, so a shorter game moves every arrival earlier with
# it and nobody is ever left unplayed. The state is still exactly what an
# event log means -- a casualty with no record in it was never seen -- so the
# fixture takes two out of the log, which is the gap sdt_decision_paths()
# exists to fill with "not_seen".
seq_run_unseen <- function(n_unseen = 2L) {
  run <- seq_run()
  gone <- sort(unique(run$scenario$cohort$id))[seq_len(n_unseen)]
  run$events <- run$events[!run$events$id %in% gone, , drop = FALSE]
  run
}

# The inputs the panel carries, at their defaults. Named so a test can say
# which one it is changing and leave the rest alone; testServer starts every
# input at NULL, and a module read through NULL defaults is not the module the
# browser drives.
seq_defaults <- function(...) {
  utils::modifyList(
    list(view = "index", all_reps = TRUE, replication = 1, cluster = FALSE,
         k = 3, consistency = "om", casualty = "", cluster_pick = ""),
    list(...)
  )
}


# Empty state ------------------------------------------------------------------

test_that("every output renders before a run exists", {
  skip_if_no_shiny()
  # This is the panel on first open, and it is listed exhaustively on purpose:
  # a NULL where an empty state belongs renders as a blank card, which readers
  # report as a broken app rather than as an unfinished workflow.
  shiny::testServer(mod_sequence_server, args = list(state = fake_state()), {
    do.call(session$setInputs, seq_defaults())
    dash <- "\u2014"
    # These four boxes render only the number, so their placeholder is written
    # in the module rather than taken from .sdt_metric(). It has to be the same
    # character, or the panel's empty state disagrees with every other metric
    # in the workbench by one glyph.
    expect_identical(.sdt_seq_dash(), dash)
    expect_match(as_html(.sdt_metric(NULL)), dash, fixed = TRUE)
    expect_identical(output$vb_paths, dash)
    expect_identical(output$vb_consistency, dash)
    expect_identical(output$vb_longest, dash)
    expect_identical(output$vb_unseen, dash)
    expect_match(as_html(output$guide), "Simulate a game run first")
    expect_match(as_html(output$status), "no run yet")
    expect_match(as_html(output$sequences_ui), "No decision paths yet")
    expect_match(as_html(output$consistency_ui), "No run to score")
    expect_match(as_html(output$cluster_summary), "No run to cluster")
    expect_null(output$cluster_paths_ui)
    expect_match(as_html(output$drill_ui), "No pathway to open")
  })
})

test_that("an empty run reports nothing rather than failing", {
  skip_if_no_shiny()
  skip_if_not_installed("simmer")
  # A scenario can legitimately hold no casualties, and the panel then has a
  # run object whose tables are all zero rows. Every summary has to fall back
  # to the same placeholder the no-run case uses instead of taking max() or
  # median() over nothing.
  empty <- sdt_scenario(sdt_cohort(sdt_synthetic_cohort(n = 3, seed = 1)[0, ]))
  state <- fake_state()
  state$scenario <- empty
  state$run <- sdt_simulate(empty, replications = 1, seed = 1, verbose = FALSE)
  shiny::testServer(mod_sequence_server, args = list(state = state), {
    do.call(session$setInputs, seq_defaults())
    dash <- "\u2014"
    expect_identical(output$vb_paths, dash)
    expect_identical(output$vb_longest, dash)
    expect_identical(output$vb_unseen, dash)
    expect_identical(output$vb_consistency, dash)
    expect_match(as_html(output$consistency_ui), "Nothing to score")
    # One replication, so the hint has to say "replication", not
    # "replications": the sentence is built by hand and pluralised by hand.
    expect_match(as_html(output$sequences_ui), "1 replication\\.")
  })
})


# The alphabet -----------------------------------------------------------------

test_that("the panel reads its alphabet from the vocabulary", {
  skip_if_no_shiny()
  # The board has grown twice: "not_seen" made it ten, and "morgue" and
  # "external" made it twelve. A panel carrying its own copy of the alphabet
  # would go on rendering and go on reporting the old denominator, so the
  # count is asserted against sdt_board_areas() itself.
  expect_length(sdt_board_areas(), 12L)
  ui <- as_html(htmltools::renderTags(mod_sequence_ui("seq"))$html)
  expect_match(ui, .sdt_label_area("not_seen"), fixed = TRUE)
  expect_false(grepl("not_seen", ui, fixed = TRUE))

  shiny::testServer(mod_sequence_server, args = list(state = fake_state()), {
    do.call(session$setInputs, seq_defaults())
    expect_match(
      as_html(output$status),
      sprintf("Alphabet: %d board states", length(sdt_board_areas()))
    )
    expect_match(as_html(output$status), "TraMineR")
    expect_match(as_html(output$status), "WeightedCluster")
  })
})


# Summary boxes ----------------------------------------------------------------

test_that("the summary boxes count pathways, positions and consistency", {
  skip_if_no_shiny()
  skip_if_not_installed("simmer")
  state <- fake_state()
  state$scenario <- seq_scenario()
  state$run <- seq_run()
  shiny::testServer(mod_sequence_server, args = list(state = state), {
    do.call(session$setInputs, seq_defaults())
    # The count comes off the labelled pathway strings rather than the wide
    # layout, so the two must agree on what a distinct pathway is.
    expect_identical(output$vb_paths,
                     format(length(unique(pathway_strings()$pathway))))
    expect_match(output$vb_longest, "^[0-9]+ steps$")
    expect_identical(output$vb_unseen, "0")
    # The headline and the table are two reads of one measure, and the box is
    # the one a reader trusts without opening the tab.
    scored <- consistency_shown()$Consistency
    expect_false(any(is.na(scored)))
    expect_identical(output$vb_consistency,
                     sprintf("%.2f", stats::median(scored)))
    expect_match(output$vb_consistency, "^[01]\\.[0-9]{2}$")
    expect_match(output$consistency_table, "Casualty", fixed = TRUE)
    expect_match(output$consistency_table, "Consistency", fixed = TRUE)
  })
})

test_that("a run of single-step pathways still reports a count", {
  skip_if_no_shiny()
  skip_if_not_installed("simmer")
  # Counting distinct pathways off sdt_decision_paths(format = "wide") is the
  # obvious implementation and the wrong one: the wide layout cannot be built
  # when every path is one step long, which is the shape a game called early
  # has, and the value box would then read as an error message.
  expect_identical(max(sdt_decision_paths(seq_run_short())$step), 1L)
  state <- fake_state()
  state$run <- seq_run_short()
  shiny::testServer(mod_sequence_server, args = list(state = state), {
    do.call(session$setInputs, seq_defaults())
    # Every casualty stopped at the same first position, so there is exactly
    # one distinct route, and the box has to say so rather than fall back to
    # a placeholder because the wide layout could not be built.
    expect_identical(
      output$vb_paths,
      format(length(unique(sdt_decision_paths(seq_run_short())$area)))
    )
    expect_identical(output$vb_paths, "1")
    # Singular, because "1 steps" is the kind of thing a reader notices.
    expect_identical(output$vb_longest, "1 step")
  })
})

test_that("casualties the game never reached are counted, not dropped", {
  skip_if_no_shiny()
  skip_if_not_installed("simmer")
  run <- seq_run_unseen()
  long <- sdt_decision_paths(run)
  unseen <- unique(long$id[long$area == "not_seen"])
  expect_gt(length(unseen), 0L)

  state <- fake_state()
  state$run <- run
  shiny::testServer(mod_sequence_server, args = list(state = state), {
    do.call(session$setInputs, seq_defaults())
    # A casualty with no events is the one most worth looking at, so it has to
    # survive both the count and the picker rather than being filtered out
    # with the event log it is missing from.
    expect_identical(output$vb_unseen, format(length(unseen)))
    session$setInputs(casualty = unseen[[1L]])
    expect_false(grepl("sdt-empty", as_html(output$drill_ui), fixed = TRUE))
    expect_match(output$drill, .sdt_label_area("not_seen"), fixed = TRUE)
    expect_false(grepl("not_seen", output$drill, fixed = TRUE))
  })
})


# Replications -----------------------------------------------------------------

test_that("pooling and picking a replication select different paths", {
  skip_if_no_shiny()
  skip_if_not_installed("simmer")
  state <- fake_state()
  state$run <- seq_run()
  shiny::testServer(mod_sequence_server, args = list(state = state), {
    do.call(session$setInputs, seq_defaults())
    pooled <- nrow(shown_paths())
    expect_null(which_replication())

    session$setInputs(all_reps = FALSE, replication = 2)
    expect_identical(which_replication(), 2L)
    expect_lt(nrow(shown_paths()), pooled)
    expect_identical(unique(shown_paths()$replication), 2L)

    # Out of range is not an error, it is a question with no answer, and the
    # placeholder has to name how many replications there actually are.
    session$setInputs(replication = 99)
    expect_identical(nrow(shown_paths()), 0L)
    expect_match(as_html(output$sequences_ui), "Nothing recorded")
    expect_match(as_html(output$sequences_ui), "2 replications")
  })
})

test_that("a cleared replication box selects nothing, not everything", {
  skip_if_no_shiny()
  skip_if_not_installed("simmer")
  state <- fake_state()
  state$run <- seq_run()
  shiny::testServer(mod_sequence_server, args = list(state = state), {
    do.call(session$setInputs, seq_defaults(all_reps = FALSE))
    # An emptied numericInput arrives as NA, which `%||%` never sees. Left
    # alone it reaches the filter as `== NA` and matches every row weakly:
    # the panel would then show a table of NAs it believed was populated.
    session$setInputs(replication = NA)
    expect_identical(which_replication(), NA_integer_)
    expect_identical(nrow(shown_paths()), 0L)
    session$setInputs(replication = numeric(0))
    expect_identical(which_replication(), NA_integer_)
    expect_identical(nrow(shown_paths()), 0L)
  })
})


# Consistency ------------------------------------------------------------------

test_that("switching the consistency measure changes the number", {
  skip_if_no_shiny()
  skip_if_not_installed("simmer")
  state <- fake_state()
  state$run <- seq_run()
  shiny::testServer(mod_sequence_server, args = list(state = state), {
    do.call(session$setInputs, seq_defaults(consistency = "om"))
    om <- consistency()
    om_headline <- output$vb_consistency

    session$setInputs(consistency = "entropy")
    entropy <- consistency()
    # The two measures answer different questions -- agreement between a
    # casualty's paths, versus the shape of one path -- so a radio button that
    # was not wired through would show as identical scores.
    expect_false(identical(om$consistency, entropy$consistency))
    expect_false(identical(om_headline, output$vb_consistency))
    # Worst first: the table is there to be read from the top.
    expect_false(is.unsorted(consistency_shown()$Consistency))
  })
})

test_that("a single-replication run says there is nothing to compare", {
  skip_if_no_shiny()
  skip_if_not_installed("simmer")
  state <- fake_state()
  state$run <- seq_run_short()
  shiny::testServer(mod_sequence_server, args = list(state = state), {
    do.call(session$setInputs, seq_defaults())
    # Optimal matching over one replication scores every casualty NA, and one
    # replication is the default. A full table of blanks reads as a broken
    # measure rather than an inapplicable one, so the panel says which it is.
    expect_true(all(is.na(consistency()$consistency)))
    expect_identical(output$vb_consistency, "\u2014")
    expect_match(as_html(output$consistency_ui), "Nothing to compare")
    expect_match(as_html(output$consistency_ui), "longitudinal")
  })
})


# Clustering -------------------------------------------------------------------

test_that("clustering produces as many groups as the slider asks for", {
  skip_if_no_shiny()
  skip_if_no_clustering()
  skip_if_not_installed("simmer")
  state <- fake_state()
  state$run <- seq_run()
  shiny::testServer(mod_sequence_server, args = list(state = state), {
    do.call(session$setInputs, seq_defaults(k = 2))
    expect_null(clusters())
    expect_match(as_html(output$guide), "Group the pathways into clusters")
    expect_match(as_html(output$cluster_summary), "not grouped yet")

    session$setInputs(cluster_go = 1)
    two <- clusters()
    expect_length(unique(two$cluster), 2L)
    expect_match(as_html(output$guide), "Open the Clusters tab")

    session$setInputs(k = 3)
    three <- clusters()
    expect_length(unique(three$cluster), 3L)

    # The drill-down has to report the membership of the cluster that was
    # picked, not of the first one it found.
    session$setInputs(cluster_pick = "2")
    detail <- cluster_detail()
    expect_identical(nrow(detail), sum(three$cluster == 2L))
    expect_match(as_html(output$cluster_summary),
                 sprintf("%d sequences", nrow(detail)))
    expect_match(as_html(output$cluster_summary),
                 sprintf("%d casualties", length(unique(detail$id))))
    expect_identical(sum(cluster_paths()$Sequences), nrow(detail))
    expect_true(any(nzchar(cluster_paths()$Representative)))
  })
})

test_that("an unknown cluster and an unset count fall back", {
  skip_if_no_shiny()
  skip_if_no_clustering()
  skip_if_not_installed("simmer")
  state <- fake_state()
  state$run <- seq_run()
  shiny::testServer(mod_sequence_server, args = list(state = state), {
    # A slider that has not reported yet reads as NA, and sdt_seq_cluster()
    # aborts on anything that is not a whole number, so the module has to
    # substitute its own default before calling it.
    do.call(session$setInputs, seq_defaults(k = NA))
    session$setInputs(cluster_go = 1)
    expect_length(unique(clusters()$cluster), 3L)

    # The browser holds the previously chosen cluster while the picker is
    # being repopulated, so a selection this run has never heard of has to
    # resolve to a real group rather than an empty table.
    session$setInputs(cluster_pick = "99")
    expect_identical(unique(cluster_detail()$cluster), min(clusters()$cluster))
  })
})

test_that("the checkbox arms and disarms clustering on its own", {
  skip_if_no_shiny()
  skip_if_no_clustering()
  skip_if_not_installed("simmer")
  state <- fake_state()
  state$run <- seq_run()
  shiny::testServer(mod_sequence_server, args = list(state = state), {
    # Button and checkbox write one flag between them. Two flags, and the
    # sidebar could claim clusters were showing while the plots disagreed.
    do.call(session$setInputs, seq_defaults(k = 2, cluster = TRUE))
    expect_true(armed())
    expect_length(unique(clusters()$cluster), 2L)
    # Mounted, and mounted as this tab's table: a non-NULL renderUI that put
    # up the wrong output would read as a working panel.
    expect_match(as_html(output$cluster_paths_ui), "cluster_paths")

    session$setInputs(cluster = FALSE)
    expect_false(armed())
    expect_null(clusters())
    expect_null(output$cluster_paths_ui)
    expect_match(as_html(output$cluster_summary), "not grouped yet")
  })
})


# Labels -----------------------------------------------------------------------

test_that("pathways reach the page as board labels, never column names", {
  skip_if_no_shiny()
  skip_if_no_clustering()
  skip_if_not_installed("simmer")
  state <- fake_state()
  state$run <- seq_run()
  shiny::testServer(mod_sequence_server, args = list(state = state), {
    do.call(session$setInputs, seq_defaults(k = 2))
    session$setInputs(cluster_go = 1)
    paths <- pathway_strings()$pathway
    # "green_area" is the stored area and "Green area" is the board's own
    # wording. Relabelling on the way out of a table cell instead of here is
    # how one of several routes to the page ends up showing the column name.
    expect_true(any(grepl("Green area", paths, fixed = TRUE)))
    expect_false(any(grepl("green_area", paths, fixed = TRUE)))

    for (html in c(output$cluster_paths_table,
                   as_html(output$cluster_summary))) {
      expect_false(grepl("green_area", html, fixed = TRUE))
      expect_false(grepl("ed_red", html, fixed = TRUE))
    }
    expect_match(output$cluster_paths_table, "Pathway", fixed = TRUE)

    green <- unique(paths_long()$id[paths_long()$area == "green_area"])[[1L]]
    session$setInputs(casualty = green)
    expect_match(output$drill, "Green area", fixed = TRUE)
    expect_false(grepl("green_area", output$drill, fixed = TRUE))
  })
})


# Casualty drill-down ----------------------------------------------------------

test_that("the drill-down opens only on an id this run has", {
  skip_if_no_shiny()
  skip_if_not_installed("simmer")
  state <- fake_state()
  state$run <- seq_run()
  shiny::testServer(mod_sequence_server, args = list(state = state), {
    do.call(session$setInputs, seq_defaults())
    expect_match(as_html(output$drill_ui), "No casualty selected")

    # A new run repopulates the picker, but the browser still holds the
    # casualty chosen against the old one until it does. Mounting the table
    # for an id this run has never heard of leaves an empty frame behind.
    session$setInputs(casualty = "P999")
    expect_match(as_html(output$drill_ui), "No casualty selected")

    session$setInputs(casualty = sort(unique(paths_long()$id))[[1L]])
    expect_false(grepl("sdt-empty", as_html(output$drill_ui), fixed = TRUE))
    for (column in c("Replication", "Step", "Area", "Minutes")) {
      expect_match(output$drill, column, fixed = TRUE)
    }
  })
})


# Download ---------------------------------------------------------------------

test_that("the paths download as the table the panel was read from", {
  skip_if_no_shiny()
  skip_if_not_installed("simmer")
  state <- fake_state()
  state$run <- seq_run()
  shiny::testServer(mod_sequence_server, args = list(state = state), {
    do.call(session$setInputs, seq_defaults())
    path <- output$download
    expect_identical(basename(path), "sandtablR-decision-paths.csv")
    csv <- utils::read.csv(path, stringsAsFactors = FALSE)
    # The whole long table, both replications: a download filtered to whatever
    # the sidebar happened to be showing is a different dataset with the same
    # name, which is worse than no download at all.
    expect_identical(nrow(csv), nrow(sdt_decision_paths(state$run)))
    expect_true(all(csv$area %in% sdt_board_areas()))
  })
})


# Plots ------------------------------------------------------------------------

test_that("the sequence plot draws, pooled and by cluster", {
  skip_if_no_shiny()
  skip_if_no_clustering()
  skip_if_not_installed("simmer")
  skip_if_not(isTRUE(capabilities("png")), "no PNG device")
  state <- fake_state()
  state$run <- seq_run()
  shiny::testServer(mod_sequence_server, args = list(state = state), {
    do.call(session$setInputs, seq_defaults(k = 2))
    expect_true(faceted_view())
    # renderPlot hands back the encoded device output, so the assertion can be
    # about the image itself. A render that drew nothing at all still returns
    # a list, and "it did not error" would accept it.
    pooled <- output$sequences$src
    expect_match(pooled, "^data:image/png;base64,")
    expect_gt(nchar(pooled), 5000L)
    # Same inputs, same bytes. That is what makes the two comparisons below
    # evidence that a control reached the plot rather than noise.
    expect_identical(output$sequences$src, pooled)

    # Only the index plot can be split by group: the state distribution is a
    # share per position over the whole cohort, so there is no cluster column
    # left by the time it is drawn. The panel says so rather than handing the
    # reader a facetting error.
    session$setInputs(cluster_go = 1, view = "distribution")
    expect_false(faceted_view())
    expect_match(as_html(output$sequences_ui), "Pooled over every cluster")
    distribution <- output$sequences$src
    expect_match(distribution, "^data:image/png;base64,")
    expect_false(identical(distribution, pooled))

    session$setInputs(view = "index")
    # Clustering is armed now, so this is the same run drawn in facets. An
    # unfacetted image here means the cluster assignment never reached
    # sdt_plot_sequences() and the reader is looking at the pooled plot under
    # a heading that promises groups.
    faceted <- output$sequences$src
    expect_match(faceted, "^data:image/png;base64,")
    expect_false(identical(faceted, pooled))
  })
})

test_that("the legend keeps every board state, relabelled", {
  skip_if_no_shiny()
  skip_if_not_installed("simmer")
  # sdt_plot_sequences() colours by the stored alphabet, which is
  # column-shaped, and the legend is where a reader meets those strings. The
  # scale is replaced on the way to the page; drop = FALSE is what keeps a
  # state the run never reached in the legend, so the ten stay comparable
  # between runs.
  plot <- .sdt_seq_relabel(sdt_plot_sequences(seq_run()))
  scale <- plot$scales$get_scales("fill")
  expect_false(scale$drop)
  expect_identical(scale$labels("green_area"), "Green area")
  expect_identical(scale$labels("not_seen"), .sdt_label_area("not_seen"))
})

test_that("a cluster with no medoid flagged still describes itself", {
  skip_if_not_installed("simmer")
  # sdt_seq_cluster() always marks one sequence per cluster, but the summary
  # reads the flag off a joined frame. A join that lost it would otherwise
  # take the panel out with a zero-row subscript rather than degrade.
  long <- sdt_decision_paths(seq_run())
  detail <- long[long$step == 1L, c("replication", "id")][seq_len(2L), ]
  detail$medoid <- FALSE
  areas <- .sdt_seq_medoid_areas(detail, long)
  # The fallback is the first row of the frame, whole and in step order. A
  # summary describing the second sequence, or one position of the first,
  # renders just as happily.
  target <- long[long$replication == detail$replication[[1L]] &
                   long$id == detail$id[[1L]], , drop = FALSE]
  expect_length(areas, nrow(target))
  expect_identical(areas[[1L]], target$area[target$step == 1L])
  expect_true(all(areas %in% sdt_board_areas()))
})


# Interactive tables -----------------------------------------------------------

test_that("the interactive tables render when reactable is installed", {
  skip_if_no_shiny()
  skip_if_no_clustering()
  skip_if_not_installed("simmer")
  skip_if_not_installed("reactable")
  state <- fake_state()
  state$run <- seq_run()
  shiny::testServer(mod_sequence_server, args = list(state = state), {
    do.call(session$setInputs,
            seq_defaults(k = 2, consistency = "entropy"))
    session$setInputs(cluster_go = 1)
    # Both tabs mount a reactable when the package is there and a plain table
    # when it is not, and the widget is built inside .sdt_safely(): a failure
    # there is swallowed and surfaces as an empty tab, so the assertions are
    # on what the widget actually carries rather than on it not erroring.
    expect_match(as_html(output$consistency_ui), "reactable")
    scores <- as.character(output$consistency_reactable)
    expect_match(scores, "Consistency", fixed = TRUE)
    expect_match(scores, consistency_shown()$Casualty[[1L]], fixed = TRUE)

    expect_match(as_html(output$cluster_paths_ui), "reactable")
    routes <- as.character(output$cluster_paths_reactable)
    expect_match(routes, "Pathway", fixed = TRUE)
    expect_match(routes, cluster_paths()$Pathway[[1L]], fixed = TRUE)
    expect_false(grepl("green_area", routes, fixed = TRUE))
  })
})
