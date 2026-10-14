# Annotate module -------------------------------------------------------------
# Capture human decisions against the cohort, so a participant's calls can be
# compared with the card and used as a supervised target. Edits are written
# straight back into state$scenario$cohort, validated first: an invalid edit is
# refused and explained rather than quietly corrupting the cohort everything
# downstream reads.
#
# The panel also has to answer the question that decides whether an annotation
# is worth modelling at all: does it behave like the course cohort? That is why
# the published per-category hit rates sit beside the observed ones rather than
# in the vignette. A participant who calls red perfectly and yellow at chance
# has produced a target the model bench cannot learn much from, and it is
# cheaper to see that here than after an hour of training.

# Only these columns may be edited. The card is the ground truth and stays
# read-only; what a participant decides is a separate pair of columns.
.sdt_editable_columns <- function() c("called_triage", "called_therapy")

.sdt_allowed_values <- function(column) {
  switch(column,
    called_triage = sdt_triage_levels(),
    called_therapy = sdt_therapy_levels(),
    character()
  )
}

mod_annotate_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,

      # The primary action first. Everything else on this panel edits what
      # this button produces, and a sidebar that opens with settings buries it
      # under controls nobody touches twice.
      shiny::actionButton(
        ns("simulate"), "Simulate a participant",
        class = "btn-primary w-100"
      ),
      htmltools::div(
        class = "sdt-metric-note sdt-sidebar-group",
        "Draws a call for every card at the published hit rates, so there is",
        "something to edit."
      ),
      shiny::actionButton(
        ns("reset"), "Reset calls to the card",
        class = "btn-outline-secondary w-100"
      ),
      shiny::downloadButton(
        ns("download"), "Download annotations",
        class = "btn-outline-secondary w-100 mt-2"
      ),

      # The rarely-touched settings, folded away. Open by default they push
      # the two buttons above out of sight on a laptop screen.
      bslib::accordion(
        open = FALSE,
        class = "sdt-sidebar-group mt-3",
        bslib::accordion_panel(
          "Simulated participant",
          shiny::sliderInput(
            ns("therapy_hit"), "Therapy agreement",
            min = 0, max = 1, value = 0.75, step = 0.05
          ),
          shiny::numericInput(ns("sim_seed"), "Seed", value = 1, min = 1,
                              step = 1),
          htmltools::div(
            class = "sdt-metric-note",
            "Sighting calls always use the published per-category hit rates,",
            "so a simulated participant stays comparable with the course",
            "cohort."
          )
        ),
        bslib::accordion_panel(
          "What may be entered",
          .sdt_annotate_vocabulary()
        )
      ),
      shiny::uiOutput(ns("status"))
    ),

    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        "Cards annotated",
        # Deliberately a text output rather than a rendered metric: the tally
        # is asserted on directly by the module's tests. The em-dash for the
        # empty case is produced by the render itself instead.
        .sdt_metric(
          shiny::textOutput(ns("vb_edited"), inline = TRUE),
          "edited since the cohort was built"
        )
      ),
      bslib::value_box(
        "Agreement with the card",
        shiny::uiOutput(ns("vb_kappa"))
      ),
      bslib::value_box(
        "Sighting hit rate",
        shiny::uiOutput(ns("vb_hits"))
      )
    ),

    bslib::layout_columns(
      col_widths = c(8, 4),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Cohort"),
        bslib::card_body(
          min_height = "460px",
          .sdt_step(3, paste(
            "Click a call to change it. The card's own values are read-only,",
            "so the decision and the truth stay side by side."
          )),
          shiny::uiOutput(ns("table_ui"), fill = TRUE)
        )
      ),
      bslib::card(
        bslib::card_header("Against the published cohort"),
        bslib::card_body(shiny::uiOutput(ns("hits_ui")))
      )
    )
  )
}

mod_annotate_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    edited <- shiny::reactiveVal(character())
    cohort_signature <- shiny::reactiveVal(NULL)

    # Which table widget this session can offer, decided once. Both the status
    # chip, the widget the UI mounts and the renderer that is kept alive have
    # to agree, and re-asking per render invites them to drift apart.
    have_dt <- requireNamespace("DT", quietly = TRUE)
    have_reactable <- requireNamespace("reactable", quietly = TRUE)

    # The tally of annotated rows must survive an annotation. Every edit
    # writes state$scenario back, so a plain observer on it would clear the
    # count the instant it was incremented and the badge would always read
    # zero. Reset only when the cohort itself is replaced, which is what
    # changes the signature; editing a call does not.
    shiny::observeEvent(state$scenario, {
      signature <- if (is.null(state$scenario)) {
        NULL
      } else {
        co <- state$scenario$cohort
        paste(state$scenario$meta$name, nrow(co),
              paste(co$id, collapse = ","), sep = "|")
      }
      if (!identical(signature, cohort_signature())) {
        cohort_signature(signature)
        edited(character())
      }
    }, ignoreNULL = FALSE)

    cohort <- shiny::reactive({
      shiny::req(state$scenario)
      co <- tibble::as_tibble(state$scenario$cohort)
      # A cohort built without simulated calls still has to be annotatable.
      for (column in .sdt_editable_columns()) {
        if (!column %in% names(co)) {
          co[[column]] <- NA_character_
        }
      }
      co
    })

    # How the calls compare with the published cohort. Computed once and read
    # by both the value box and the panel beside the table, so the two cannot
    # quote different numbers at the same reader.
    hit_rates <- shiny::reactive({
      co <- cohort()
      levels <- sdt_triage_levels()
      counts <- function(keep) {
        vapply(levels, function(level) {
          sum(keep & co$triage == level, na.rm = TRUE)
        }, integer(1))
      }
      n <- counts(rep(TRUE, nrow(co)))
      called <- counts(!is.na(co$called_triage))
      hit <- counts(!is.na(co$called_triage) & co$triage == co$called_triage)
      tibble::tibble(
        level = levels,
        n = unname(n),
        # Cards nobody has called yet are not misses, so they stay out of the
        # denominator; counting them as wrong would read as a collapse in
        # performance every time a fresh cohort is built.
        called = unname(called),
        hit = unname(hit),
        observed = unname(ifelse(called > 0L, hit / called, NA_real_)),
        published = .sdt_published_rate(.sdt_published_calls()$triage_hit,
                                        levels)
      )
    })

    # Writing an edit back ----------------------------------------------------

    shiny::observeEvent(input$table_cell_edit, {
      info <- input$table_cell_edit
      shiny::req(state$scenario, info)
      co <- cohort()
      shown <- .sdt_annotate_columns(co)
      # DT reports a zero-based column index into the table it was given.
      index <- info$col + 1L
      row <- info$row
      # An edit can arrive after the cohort it was made against has been
      # replaced. Writing at that index would silently extend a column rather
      # than fail, so an out-of-range edit is dropped instead. The shape of
      # the message is checked too: an observer that errors takes the session
      # down with it, and this one is driven from the browser.
      if (length(index) != 1L || length(row) != 1L ||
            is.na(index) || is.na(row) ||
            index < 1L || index > length(shown) ||
            row < 1L || row > nrow(co)) {
        return(invisible(NULL))
      }
      column <- shown[[index]]

      if (!column %in% .sdt_editable_columns()) {
        shiny::showNotification(
          sprintf("%s comes from the card and cannot be edited.",
                  .sdt_annotate_label(column)),
          type = "warning"
        )
        return(invisible(NULL))
      }
      allowed <- .sdt_allowed_values(column)
      value <- trimws(as.character(info$value))
      if (!value %in% allowed) {
        shiny::showNotification(
          .sdt_annotate_refusal(column, allowed),
          type = "error", duration = 8
        )
        return(invisible(NULL))
      }

      co[[column]][[row]] <- value
      # The row counts as annotated only once the cohort carrying it has been
      # accepted, so a refused edit cannot inflate the tally.
      shiny::req(.sdt_annotate_write(state, co))
      edited(union(edited(), co$id[[row]]))
    })

    shiny::observeEvent(input$reset, {
      shiny::req(state$scenario)
      co <- cohort()
      co$called_triage <- co$triage
      co$called_therapy <- co$therapy
      .sdt_annotate_write(state, co)
      edited(character())
    })

    shiny::observeEvent(input$simulate, {
      shiny::req(state$scenario)
      # The sidebar controls are read defensively: this observer also runs
      # under testServer(), where nothing but the button has been set.
      therapy_hit <- .sdt_annotate_number(input$therapy_hit, 0.75)
      seed <- .sdt_annotate_number(input$sim_seed, NA_real_)
      co <- tryCatch(
        sdt_add_decisions(
          tibble::as_tibble(state$scenario$cohort),
          therapy_hit = therapy_hit,
          seed = if (is.na(seed)) NULL else as.integer(seed)
        ),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error",
                                  duration = 8)
          NULL
        }
      )
      shiny::req(co)
      .sdt_annotate_write(state, co)
      edited(character())
    })

    # Summary boxes -----------------------------------------------------------

    # No req() here, on purpose. A value box whose body never renders is the
    # grey titled slab that reads as a broken panel; the empty case is a
    # dash and a reason instead.
    output$vb_edited <- shiny::renderText({
      .sdt_safely({
        if (is.null(state$scenario)) {
          "\u2014"
        } else {
          sprintf("%d of %d", length(edited()), nrow(cohort()))
        }
      }, fallback = "\u2014")
    })

    output$vb_kappa <- shiny::renderUI({
      .sdt_safely({
        if (is.null(state$scenario)) {
          .sdt_metric(NULL, "no cohort to score yet")
        } else if (!requireNamespace("yardstick", quietly = TRUE)) {
          .sdt_metric(NULL, "needs the yardstick package")
        } else {
          co <- cohort()
          score <- sdt_consistency_score(co$triage, co$called_triage)
          estimate <- score$.estimate
          if (length(estimate) != 1L || is.na(estimate)) {
            # Kappa is also undefined when every call used the same category,
            # which is not the same thing as having no calls at all. Saying
            # "no calls yet" to someone looking at a full column of calls
            # sends them hunting for a bug that is not there.
            .sdt_metric(NULL, if (isTRUE(score$n > 0L)) {
              "one category used throughout, so kappa is undefined"
            } else {
              "no calls recorded yet"
            })
          } else {
            .sdt_metric(sprintf("%.2f", estimate),
                        "Cohen's kappa, calls against the card")
          }
        }
      })
    })

    output$vb_hits <- shiny::renderUI({
      .sdt_safely({
        if (is.null(state$scenario)) {
          .sdt_metric(NULL, "no cohort to score yet")
        } else {
          rates <- hit_rates()
          overall <- .sdt_overall_rate(rates)
          published <- .sdt_published_calls()$triage_hit_overall %||% NA_real_
          htmltools::tagList(
            .sdt_metric(
              if (is.na(overall)) NULL else .sdt_percent(overall),
              if (is.na(published)) {
                "of the calls made match the card"
              } else {
                sprintf("published %s across the course cohort",
                        .sdt_percent(published))
              }
            ),
            htmltools::div(lapply(seq_len(nrow(rates)), function(i) {
              .sdt_chip(
                sprintf(
                  "%s %s / %s",
                  toupper(substr(rates$level[[i]], 1L, 1L)),
                  .sdt_percent(rates$observed[[i]]),
                  .sdt_percent(rates$published[[i]])
                ),
                rates$level[[i]]
              )
            })),
            htmltools::div(class = "sdt-metric-note", "observed / published")
          )
        }
      })
    })

    output$status <- shiny::renderUI({
      .sdt_safely({
        htmltools::tagList(
          htmltools::div(class = "sdt-metric-note", "This panel"),
          if (is.null(state$scenario)) {
            .sdt_chip("No scenario yet", "muted")
          } else {
            .sdt_chip(sprintf("%d cards", nrow(state$scenario$cohort)),
                      "green")
          },
          if (have_dt) {
            .sdt_chip("Calls editable", "green")
          } else if (have_reactable) {
            .sdt_chip("Read-only: install DT", "yellow")
          } else {
            .sdt_chip("No table package", "red")
          },
          if (requireNamespace("yardstick", quietly = TRUE)) {
            NULL
          } else {
            .sdt_chip("No kappa: install yardstick", "muted")
          }
        )
      })
    })

    # The comparison with the published cohort --------------------------------

    output$hits_ui <- shiny::renderUI({
      .sdt_safely({
        if (is.null(state$scenario)) {
          .sdt_empty(
            "Nothing to compare yet.",
            paste("Build a cohort on the Scenario tab, then simulate a",
                  "participant or type the calls in yourself.")
          )
        } else {
          rates <- hit_rates()
          if (sum(rates$called) == 0L) {
            .sdt_empty(
              "No calls recorded yet.",
              paste("Press \u201cSimulate a participant\u201d, or edit a call",
                    "in the table, and the agreement appears here.")
            )
          } else {
            .sdt_hit_panel(rates)
          }
        }
      })
    })

    # Table -------------------------------------------------------------------

    # DT leads here although the workbench prefers reactable elsewhere: DT is
    # the only one of the two that can edit a cell, which is the whole point
    # of this panel. reactable is the read-only fallback, and an empty state
    # explains the panel when neither is installed.
    output$table_ui <- shiny::renderUI({
      .sdt_safely({
        if (is.null(state$scenario)) {
          .sdt_empty(
            "No cohort to annotate yet.",
            paste("Build one on the Scenario tab. The cards arrive here with",
                  "the card's own values beside the calls.")
          )
        } else if (have_dt) {
          DT::DTOutput(session$ns("table"), fill = TRUE)
        } else if (have_reactable) {
          htmltools::tagList(
            htmltools::div(
              class = "sdt-metric-note mb-2",
              "Read-only: install the DT package to edit the calls in place."
            ),
            reactable::reactableOutput(session$ns("table_reactable"))
          )
        } else {
          .sdt_empty(
            "The cohort cannot be shown here.",
            paste("Install DT to edit the calls, or reactable to read them.",
                  "The cohort itself is unaffected and still downloads.")
          )
        }
      })
    })

    # Defined conditionally, not guarded inside: DT::renderDT() is evaluated
    # when this server function runs, so referencing it without the package
    # would take the whole app down rather than this one panel.
    if (have_dt) {
      output$table <- DT::renderDT(
        {
          shiny::req(state$scenario)
          co <- cohort()
          shown <- .sdt_annotate_columns(co)
          editable_at <- which(shown %in% .sdt_editable_columns()) - 1L
          .sdt_safely(DT::datatable(
            .sdt_annotate_data(co, shown),
            colnames = .sdt_annotate_label(shown),
            rownames = FALSE,
            selection = "none",
            editable = list(target = "cell", disable = list(
              columns = setdiff(seq_along(shown) - 1L, editable_at)
            )),
            options = list(pageLength = 15, scrollX = TRUE,
                           lengthChange = FALSE),
            fillContainer = TRUE
          ))
        },
        server = TRUE
      )
    }

    if (have_reactable) {
      output$table_reactable <- reactable::renderReactable({
        shiny::req(state$scenario)
        co <- cohort()
        shown <- .sdt_annotate_columns(co)
        .sdt_safely(reactable::reactable(
          as.data.frame(co[, shown, drop = FALSE]),
          columns = stats::setNames(
            lapply(shown, function(column) {
              reactable::colDef(name = .sdt_annotate_label(column))
            }),
            shown
          ),
          highlight = TRUE, compact = TRUE, fullWidth = TRUE,
          defaultPageSize = 15
        ))
      })
    }

    output$download <- shiny::downloadHandler(
      filename = function() "sandtablR-annotations.csv",
      content = function(file) {
        # A download taken before a cohort exists writes the column names on
        # their own rather than failing: an error page from a button that
        # looked enabled reads as a broken app.
        co <- if (is.null(state$scenario)) .sdt_annotate_blank() else cohort()
        utils::write.csv(co, file, row.names = FALSE)
      }
    )

    # Tabs are hidden until selected, and a suspended output never renders, so
    # a reader arriving on this panel for the first time would find the whole
    # of it blank. Only the table renderer the UI actually mounts is kept
    # alive: unsuspending the other would rebuild a widget nobody can see on
    # every edit.
    mounted <- c(
      "status", "vb_edited", "vb_kappa", "vb_hits", "hits_ui", "table_ui",
      if (have_dt) "table" else if (have_reactable) "table_reactable"
    )
    for (nm in mounted) {
      shiny::outputOptions(output, nm, suspendWhenHidden = FALSE)
    }

    invisible(NULL)
  })
}

# Display helpers --------------------------------------------------------------

# The card's own columns, in reading order.
.sdt_annotate_front <- function() {
  c("id", "age", "sex", "life_points", "ventilated",
    "injury_region", "triage", "therapy")
}

# Card columns first, then the calls, so the two sit side by side.
.sdt_annotate_columns <- function(cohort) {
  intersect(
    c(.sdt_annotate_front(), .sdt_editable_columns()), names(cohort)
  )
}

# The annotation frame with no rows in it, so a download taken before a cohort
# exists still explains its own shape instead of erroring.
.sdt_annotate_blank <- function() {
  columns <- c(.sdt_annotate_front(), .sdt_editable_columns())
  tibble::as_tibble(
    stats::setNames(rep(list(character()), length(columns)), columns)
  )
}

# Column headers, written out. "called_triage" asks the reader to translate
# before they can annotate, and it is also the one thing on the page that
# leaks the data model at them.
.sdt_annotate_headers <- function() {
  c(
    id = "Card", age = "Age", sex = "Sex", life_points = "Life points",
    ventilated = "Ventilated", injury_region = "Injury region",
    triage = "Card sighting", therapy = "Card therapy",
    called_triage = "Called sighting", called_therapy = "Called therapy",
    arrival_order = "Arrival"
  )
}

# Sentence case, used only as the fallback for a column this module has no
# written-out header for. Kept module-scoped on purpose: a general prettifier
# in the theme would invite raw names onto the page from everywhere, and the
# fix for those is a written label, not a regular expression.
.sdt_annotate_case <- function(x) {
  sub("^(.)", "\\U\\1", gsub("_", " ", x, fixed = TRUE), perl = TRUE)
}

.sdt_annotate_label <- function(x) {
  headers <- .sdt_annotate_headers()
  out <- unname(headers[x])
  ifelse(is.na(out), .sdt_annotate_case(x), out)
}

# The vocabulary, shown beside the controls rather than in the vignette. The
# therapy scale carries "none" -- the card's "no operation" -- and a
# participant who does not know that types something the editor refuses.
.sdt_annotate_vocabulary <- function() {
  htmltools::tagList(
    htmltools::div(class = "sdt-metric-note", "Sighting categories"),
    htmltools::div(lapply(sdt_triage_levels(), function(level) {
      .sdt_chip(level, level)
    })),
    htmltools::div(class = "sdt-metric-note mt-2", "Therapy concepts"),
    htmltools::div(lapply(sdt_therapy_levels(), function(level) {
      .sdt_chip(level, if (identical(level, "none")) "muted" else "glass")
    })),
    htmltools::div(
      class = "sdt-metric-note mt-2",
      "\u201cnone\u201d is the card's no-operation option: worked up, no",
      "procedure performed."
    )
  )
}

# Why an edit was refused. Spelled out rather than assembled at the call site
# because the therapy scale gained a fourth level, and a message listing three
# would send the reader looking for a bug that is not there.
.sdt_annotate_refusal <- function(column, allowed) {
  message <- sprintf(
    "%s must be one of: %s.", .sdt_annotate_label(column),
    paste(allowed, collapse = ", ")
  )
  if (identical(column, "called_therapy")) {
    message <- paste(message, "Use \u201cnone\u201d when the casualty was",
                     "worked up and no operation was indicated.")
  }
  message
}

# The frame handed to DT.
#
# The editable columns go in as factors where they already fit the vocabulary,
# because DT then edits them with a dropdown of the allowed levels instead of
# a free text box. Where a value sits outside the vocabulary the column stays
# character: coercing it would blank the very value the reader needs to see to
# fix it.
.sdt_annotate_data <- function(cohort, shown) {
  data <- as.data.frame(cohort[, shown, drop = FALSE])
  for (column in intersect(shown, .sdt_editable_columns())) {
    allowed <- .sdt_allowed_values(column)
    values <- as.character(data[[column]])
    if (all(is.na(values) | values %in% allowed)) {
      data[[column]] <- factor(values, levels = allowed)
    }
  }
  data
}

# Published marginals, or NULL when the bundled reference cannot be read. The
# comparison is a nicety; annotating must not stop because it is missing.
.sdt_published_calls <- function() {
  tryCatch(sdt_reference("published"), error = function(e) NULL)
}

.sdt_published_rate <- function(published, levels) {
  if (is.null(published)) {
    return(rep(NA_real_, length(levels)))
  }
  as.numeric(unname(published[levels]))
}

# Hit rate over every category at once, on the cards that were actually
# called.
.sdt_overall_rate <- function(rates) {
  called <- sum(rates$called)
  if (called == 0L) NA_real_ else sum(rates$hit) / called
}

# One place that turns a proportion into text, so a missing rate reads as a
# dash everywhere instead of as "NA%" in one panel and "--" in the next.
.sdt_percent <- function(x) {
  if (length(x) != 1L || is.na(x)) "\u2014" else sprintf("%.0f%%", 100 * x)
}

# The observed rates beside the published ones, category by category. Kept out
# of the render so the shape of the table is readable in one place.
.sdt_hit_panel <- function(rates) {
  rows <- lapply(seq_len(nrow(rates)), function(i) {
    htmltools::tags$tr(
      htmltools::tags$td(
        .sdt_chip(.sdt_annotate_case(rates$level[[i]]), rates$level[[i]])
      ),
      htmltools::tags$td(
        class = "text-end",
        sprintf("%d of %d", rates$called[[i]], rates$n[[i]])
      ),
      htmltools::tags$td(class = "text-end",
                         .sdt_percent(rates$observed[[i]])),
      htmltools::tags$td(class = "text-end text-body-secondary",
                         .sdt_percent(rates$published[[i]]))
    )
  })
  published <- .sdt_published_calls()$triage_hit_overall %||% NA_real_
  htmltools::tagList(
    htmltools::tags$table(
      class = "table table-sm align-middle mb-2",
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th("Category"),
        htmltools::tags$th(class = "text-end", "Called"),
        htmltools::tags$th(class = "text-end", "Observed"),
        htmltools::tags$th(class = "text-end", "Published")
      )),
      htmltools::tags$tbody(rows),
      htmltools::tags$tfoot(htmltools::tags$tr(
        htmltools::tags$th("All cards"),
        htmltools::tags$th(
          class = "text-end",
          sprintf("%d of %d", sum(rates$called), sum(rates$n))
        ),
        htmltools::tags$th(class = "text-end",
                           .sdt_percent(.sdt_overall_rate(rates))),
        htmltools::tags$th(class = "text-end fw-normal",
                           .sdt_percent(published))
      ))
    ),
    htmltools::div(
      class = "sdt-metric-note",
      "Published rates are the per-category agreement reported for the",
      "evaluated exercise \u2014",
      htmltools::tags$code("sdt_reference(\"published\")$triage_hit"),
      htmltools::br(),
      "Yellow is the category the course cohort found hardest, so an",
      "annotation that scores it far higher is probably not a participant."
    )
  )
}

# Read a sidebar control that may not exist. testServer() sets only the inputs
# a test names, and a cleared numeric input arrives as NA rather than as its
# default.
.sdt_annotate_number <- function(value, fallback) {
  if (length(value) != 1L || is.na(value)) fallback else as.numeric(value)
}

# Write an edited cohort back into shared state, keeping the scenario's board
# and seed. Failure is reported where the edit was made rather than thrown,
# because a refused cohort must leave the previous one standing.
.sdt_annotate_write <- function(state, cohort) {
  updated <- tryCatch(
    sdt_scenario(cohort, resources = state$scenario$resources,
                 name = state$scenario$meta$name,
                 seed = state$scenario$meta$seed),
    error = function(e) {
      shiny::showNotification(conditionMessage(e), type = "error",
                              duration = 8)
      NULL
    }
  )
  if (!is.null(updated)) {
    state$scenario <- updated
  }
  invisible(updated)
}
