# The scenario panel, and the theme helpers every panel is drawn with.
#
# This panel is the app's entry point: nothing downstream exists until it has
# run, so a silent failure here reads as five broken tabs rather than one. It
# is driven through shiny::testServer() because the module lives in R/ and can
# therefore be reached directly, without a browser.
#
# The theme helpers are exercised on their own at the bottom. They are the
# reason six panels look like one app, and most of them are pure functions of
# their arguments, so they do not need a session to be pinned down.

skip_if_no_shiny <- function() {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("htmltools")
}

# A reactiveValues store standing in for the app's shared state.
fake_state <- function(...) {
  shiny::reactiveValues(scenario = NULL, run = NULL, models = NULL,
                        bench = NULL, ...)
}

# The eight capacity inputs, every one a different number: a transposed
# mapping between input and board area is invisible when they all read 1.
scenario_caps <- function() {
  list(cap_triage = 2, cap_ed_red = 3, cap_ed_yellow = 4,
       cap_diagnostics = 6, cap_or = 5, cap_recovery = 7,
       cap_icu_imc = 12, cap_ward = 25)
}

# Everything the sidebar sends when "Build scenario" is pressed, with any
# argument overriding the default for that one input.
build_inputs <- function(...) {
  utils::modifyList(
    c(list(n = 20, p_red = 0.5, p_yellow = 0.3, seed = 1, backend = "base",
           decisions = TRUE, build = 1), scenario_caps()),
    list(...)
  )
}

# A scenario whose cohort has no rows. A board exists, so every "is there a
# scenario" gate passes, but there is still nothing to average, count or plot.
empty_scenario <- function() {
  sdt_scenario(sdt_cohort(sdt_synthetic_cohort(n = 3, seed = 1)[0, ]),
               name = "empty")
}

# renderUI() hands back markup and its HTML dependencies; only the markup is
# ever asserted on, and pasting flattens the pair to something grepl() reads.
ui_html <- function(x) paste(as.character(x), collapse = " ")

# How many times a fixed string occurs. gregexpr() reports "no match" as a
# single -1, so counting with length() alone reads zero matches as one, and a
# helper that emitted nothing at all would pass a test asserting it emitted
# one thing. Every count in this file goes through here.
count_in <- function(needle, haystack) {
  hits <- gregexpr(needle, haystack, fixed = TRUE)[[1L]]
  if (identical(hits[[1L]], -1L)) 0L else length(hits)
}

# The em-dash placeholder every empty value box falls back to, built from its
# code point so that this file stays ASCII.
dash <- intToUtf8(0x2014L)


# The green remainder ----------------------------------------------------------

test_that("the P(green) note is whatever the two sliders leave over", {
  skip_if_no_shiny()
  shiny::testServer(mod_scenario_server, args = list(state = fake_state()), {
    # Before the sliders have reported, both read as zero rather than as an
    # error: this note is on screen at first paint.
    expect_identical(output$p_green_note, "P(green) = 1.00")

    session$setInputs(p_red = 0.44, p_yellow = 0.24)
    expect_identical(output$p_green_note, "P(green) = 0.32")

    # Recomputed from each slider independently. A note that only watched
    # p_red would keep saying 0.32 here.
    session$setInputs(p_yellow = 0.06)
    expect_identical(output$p_green_note, "P(green) = 0.50")
    session$setInputs(p_red = 0.10)
    expect_identical(output$p_green_note, "P(green) = 0.84")
  })
})

test_that("the P(green) note never goes negative", {
  skip_if_no_shiny()
  shiny::testServer(mod_scenario_server, args = list(state = fake_state()), {
    # Nothing stops a user putting both sliders high. Printing "P(green) =
    # -0.40" at them would be nonsense, and the build path floors the same
    # number, so the note has to agree with what would actually be drawn.
    session$setInputs(p_red = 0.9, p_yellow = 0.5)
    expect_identical(output$p_green_note, "P(green) = 0.00")
  })
})


# The empty panel --------------------------------------------------------------

test_that("every card carries a placeholder before a scenario exists", {
  skip_if_no_shiny()
  shiny::testServer(mod_scenario_server, args = list(state = fake_state()), {
    # bslib draws a value box with an empty body as a titled grey slab, which
    # reads as broken rather than as waiting. Every one of these has to emit
    # something on first paint.
    expect_identical(output$vb_n, dash)
    expect_identical(output$vb_life, dash)
    expect_identical(output$vb_caps, dash)
    expect_true(grepl(dash, ui_html(output$vb_mix), fixed = TRUE))

    status <- ui_html(output$status)
    expect_true(grepl("No scenario yet", status, fixed = TRUE))
    expect_true(grepl("sdt-chip-muted", status, fixed = TRUE))

    # The two placeholders that say which control to press.
    mix <- ui_html(output$mix_ui)
    expect_true(grepl("Nothing to plot yet", mix, fixed = TRUE))
    expect_true(grepl("sdt-empty-hint", mix, fixed = TRUE))
    cohort <- ui_html(output$cohort_ui)
    expect_true(grepl("No cohort yet", cohort, fixed = TRUE))
    expect_true(grepl("sdt-empty-hint", cohort, fixed = TRUE))

    expect_null(triage_counts())
  })
})


# Building ---------------------------------------------------------------------

test_that("every card carries a real value once a scenario exists", {
  skip_if_no_shiny()
  shiny::testServer(mod_scenario_server, args = list(state = fake_state()), {
    do.call(session$setInputs, build_inputs())

    expect_identical(output$vb_n, "20")
    expect_match(output$vb_life, "^[0-9]\\.[0-9]$")
    # The two areas the caption names, in the order it names them.
    expect_identical(output$vb_caps, "5 / 12")

    mix <- ui_html(output$vb_mix)
    for (initial in c("r: ", "y: ", "g: ")) {
      expect_true(grepl(initial, mix, fixed = TRUE))
    }
    expect_false(grepl(dash, mix, fixed = TRUE))
    expect_identical(sum(triage_counts()), 20L)

    status <- ui_html(output$status)
    expect_true(grepl("workbench", status, fixed = TRUE))
    expect_true(grepl("20 casualties on the board", status, fixed = TRUE))

    # The placeholders have to give way to the real outputs, not sit beside
    # them: both cards swap their whole contents rather than appending.
    expect_true(grepl("shiny-plot-output", ui_html(output$mix_ui),
                      fixed = TRUE))
    expect_false(grepl("sdt-empty", ui_html(output$mix_ui), fixed = TRUE))
  })
})

test_that("all eight capacity inputs reach the board", {
  skip_if_no_shiny()
  shiny::testServer(mod_scenario_server, args = list(state = fake_state()), {
    do.call(session$setInputs, build_inputs())
    caps <- scenario_caps()
    for (input_id in names(caps)) {
      area <- sub("^cap_", "", input_id)
      expect_identical(state$scenario$resources[[area]],
                       as.integer(caps[[input_id]]), info = area)
    }
  })
})

test_that("the call checkbox decides whether participant calls exist", {
  skip_if_no_shiny()
  shiny::testServer(mod_scenario_server, args = list(state = fake_state()), {
    do.call(session$setInputs, build_inputs(n = 10, decisions = FALSE))
    expect_false("called_triage" %in% names(state$scenario$cohort))

    # A second press with the box ticked has to add them, not leave the first
    # cohort in place: the Model lab has nothing to train on without these.
    do.call(session$setInputs, build_inputs(n = 10, decisions = TRUE,
                                            build = 2))
    expect_true("called_triage" %in% names(state$scenario$cohort))
    expect_true("called_therapy" %in% names(state$scenario$cohort))
  })
})

test_that("the seed makes a build reproducible and a new seed changes it", {
  skip_if_no_shiny()
  shiny::testServer(mod_scenario_server, args = list(state = fake_state()), {
    do.call(session$setInputs, build_inputs(n = 15, seed = 7))
    first <- state$scenario$cohort$life_points
    # Recorded so a board can be rebuilt from its metadata alone.
    expect_equal(state$scenario$meta$seed, 7)

    do.call(session$setInputs, build_inputs(n = 15, seed = 7, build = 2))
    expect_identical(state$scenario$cohort$life_points, first)

    do.call(session$setInputs, build_inputs(n = 15, seed = 8, build = 3))
    expect_false(identical(state$scenario$cohort$life_points, first))
  })
})

test_that("a cohort that cannot be built is reported, not thrown", {
  skip_if_no_shiny()
  shiny::testServer(mod_scenario_server, args = list(state = fake_state()), {
    # Nothing stops the numeric input reaching zero, and zero casualties is a
    # hard error in the generator. It has to surface as a notification and
    # leave the previous state alone, or the whole session dies on a typo.
    expect_no_error(do.call(session$setInputs, build_inputs(n = 0)))
    expect_null(state$scenario)
    expect_identical(output$vb_n, dash)
  })
})

test_that("the simstudy backend builds through the panel", {
  skip_if_no_shiny()
  skip_if_not_installed("simstudy")
  shiny::testServer(mod_scenario_server, args = list(state = fake_state()), {
    do.call(session$setInputs, build_inputs(n = 15, backend = "simstudy"))
    expect_s3_class(state$scenario, "scenario")
    expect_identical(nrow(state$scenario$cohort), 15L)
    # The correlated backend carries an injury-severity proxy the base one
    # does not; its absence would mean the generator silently fell back.
    expect_true("iss" %in% names(state$scenario$cohort))
  })
})


# Against the published board --------------------------------------------------

test_that("the marginals card prints the published triage mix", {
  skip_if_no_shiny()
  published <- sdt_reference("published")$triage_mix
  shiny::testServer(mod_scenario_server, args = list(state = fake_state()), {
    # This card is the only one that is never empty, because the target is
    # known before anything is built. It must read from sdt_reference()
    # rather than from a second copy of the marginals kept in the module.
    html <- ui_html(output$marginals)
    for (level in sdt_triage_levels()) {
      expect_true(
        grepl(sprintf("%.1f%%", 100 * published[[level]]), html, fixed = TRUE),
        info = level
      )
      expect_true(grepl(paste0("sdt-chip-", level), html, fixed = TRUE))
    }
    # The realised column is the only em-dash on the card, and it is there
    # exactly three times while there is no cohort to compare against.
    expect_identical(count_in(dash, html), 3L)

    do.call(session$setInputs, build_inputs())
    filled <- ui_html(output$marginals)
    expect_false(grepl(dash, filled, fixed = TRUE))
    # Published shares stay put beside the realised ones.
    expect_true(grepl(sprintf("%.1f%%", 100 * published[["red"]]), filled,
                      fixed = TRUE))
  })
})


# The reference board ----------------------------------------------------------

test_that("the reference button loads the calibrated board and its calls", {
  skip_if_no_shiny()
  state <- fake_state()
  shiny::testServer(mod_scenario_server, args = list(state = state), {
    state$run <- "stale"
    state$models <- list(tree = "stale")
    state$bench <- "stale"
    session$setInputs(reference = 1)

    expect_identical(state$scenario$meta$name, "reference")
    # The reference board is what the Model lab is demonstrated on, so it has
    # to arrive with participant calls already attached.
    expect_true("called_triage" %in% names(state$scenario$cohort))
    expect_null(state$run)
    expect_null(state$models)
    expect_null(state$bench)
    expect_true(grepl("reference", ui_html(output$status), fixed = TRUE))
  })
})


# Saving and loading -----------------------------------------------------------

test_that("the save button writes a scenario named after the board", {
  skip_if_no_shiny()
  shiny::testServer(mod_scenario_server, args = list(state = fake_state()), {
    # With nothing built the handler still has to produce a file; a download
    # that errors leaves the browser on a broken page with no explanation.
    expect_match(basename(output$save), "^sandtablR-scenario\\.rds$")

    do.call(session$setInputs, build_inputs(n = 12))
    path <- output$save
    expect_match(basename(path), "^sandtablR-workbench\\.rds$")
    expect_s3_class(readRDS(path), "scenario")
    expect_identical(nrow(readRDS(path)$cohort), 12L)
  })
})

test_that("a saved scenario round-trips through the load control", {
  skip_if_no_shiny()
  saved <- sdt_scenario(
    sdt_add_decisions(sdt_synthetic_cohort(n = 12, seed = 1), seed = 2),
    resources = list(or = 3), name = "from-disk"
  )
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(saved, path)

  state <- fake_state(run = "stale", models = list(tree = "stale"),
                      bench = "stale")
  shiny::testServer(mod_scenario_server, args = list(state = state), {
    session$setInputs(load = list(name = "board.rds", size = 1L, type = "",
                                  datapath = path))
    expect_s3_class(state$scenario, "scenario")
    expect_identical(state$scenario$meta$name, "from-disk")
    expect_identical(state$scenario$resources$or, 3L)
    expect_identical(state$scenario$cohort, saved$cohort)
    # A loaded board is a new board, so anything computed from the old one
    # has to go with it.
    expect_null(state$run)
    expect_null(state$models)
    expect_null(state$bench)

    expect_identical(output$vb_n, "12")
    expect_true(grepl("from-disk", ui_html(output$status), fixed = TRUE))
  })
})

test_that("a file that is not a scenario leaves the board alone", {
  skip_if_no_shiny()
  wrong <- withr::local_tempfile(fileext = ".rds")
  saveRDS(data.frame(a = 1), wrong)
  unreadable <- withr::local_tempfile(fileext = ".rds")
  writeLines("this is not an RDS file", unreadable)

  shiny::testServer(mod_scenario_server, args = list(state = fake_state()), {
    do.call(session$setInputs, build_inputs(n = 10))
    before <- state$scenario

    # Two different failures, one rule: say so and keep what is on screen.
    # Replacing a working board with NULL because a file was wrong would lose
    # a cohort the user may have spent the session annotating.
    session$setInputs(load = list(name = "wrong.rds", size = 1L, type = "",
                                  datapath = wrong))
    expect_identical(state$scenario, before)

    session$setInputs(load = list(name = "junk.rds", size = 1L, type = "",
                                  datapath = unreadable))
    expect_identical(state$scenario, before)
    expect_identical(output$vb_n, "10")
  })
})


# Degenerate cohorts -----------------------------------------------------------

test_that("an empty cohort empties the cards instead of erroring", {
  skip_if_no_shiny()
  state <- fake_state()
  state$scenario <- empty_scenario()
  shiny::testServer(mod_scenario_server, args = list(state = state), {
    # A scenario exists, so the "no scenario" gates all pass, but there is
    # still nothing to average or to plot. mean() of nothing is NaN and the
    # fill scale complains about an empty cohort, so both are caught here.
    expect_identical(output$vb_n, "0")
    expect_identical(output$vb_life, dash)
    expect_null(triage_counts())
    expect_true(grepl(dash, ui_html(output$vb_mix), fixed = TRUE))
    expect_true(grepl("Nothing to plot yet", ui_html(output$mix_ui),
                      fixed = TRUE))
    # The table still offers itself; an empty board is a board, and the card
    # must not fall back to "no cohort yet" when one exists.
    skip_if_not_installed("DT")
    expect_false(grepl("No cohort yet", ui_html(output$cohort_ui),
                       fixed = TRUE))
  })
})

test_that("the mix plot is gated on its counts, not on the scenario", {
  skip_if_no_shiny()
  state <- fake_state()
  state$scenario <- empty_scenario()
  shiny::testServer(mod_scenario_server, args = list(state = state), {
    # The plot output is never suspended, so it is evaluated even while the
    # placeholder is the thing on screen. A scenario exists here, so a gate
    # written as req(state$scenario) would let an empty cohort through to the
    # fill scale; only the gate mix_ui uses stops it. Stopping on req() is a
    # silent validation condition, which is what distinguishes "nothing to
    # draw" from "the draw failed".
    stopped <- tryCatch(output$mix, error = function(e) e)
    expect_s3_class(stopped, "shiny.silent.error")

    # Past the gate it is a real PNG rather than a caught condition: the
    # renderPlot() wrapper recolours the plot, and a failure in there would
    # otherwise surface as a blank card nothing tests.
    do.call(session$setInputs, build_inputs(n = 12))
    drawn <- output$mix
    expect_match(drawn$src, "^data:image/png;base64,")
    expect_gt(nchar(drawn$src), 1000L)
    expect_gt(drawn$width, 0)
  })
})


# Theme helpers ----------------------------------------------------------------

test_that(".sdt_metric() falls back to an em-dash for anything empty", {
  skip_if_not_installed("htmltools")
  for (value in list(NULL, NA, NA_character_, "", character(0))) {
    html <- ui_html(.sdt_metric(value, "a caption"))
    expect_true(grepl(dash, html, fixed = TRUE))
    expect_true(grepl("sdt-metric-empty", html, fixed = TRUE))
  }

  html <- ui_html(.sdt_metric("42", "a caption"))
  expect_true(grepl(">42<", html, fixed = TRUE))
  expect_false(grepl("sdt-metric-empty", html, fixed = TRUE))
  expect_true(grepl("a caption", html, fixed = TRUE))

  # The caption is optional, and asking for none must not leave an empty
  # note div taking up space under every value box that has no room for it.
  expect_false(grepl("sdt-metric-note", ui_html(.sdt_metric("42")),
                     fixed = TRUE))
})

test_that(".sdt_empty() names the state and, when given one, the fix", {
  skip_if_not_installed("htmltools")
  bare <- ui_html(.sdt_empty("Nothing to plot yet."))
  expect_true(grepl("Nothing to plot yet.", bare, fixed = TRUE))
  expect_true(grepl("sdt-empty-mark", bare, fixed = TRUE))
  expect_false(grepl("sdt-empty-hint", bare, fixed = TRUE))

  # The hint is the half that says which control produces the missing thing;
  # without it a reader has to guess, and they usually guess "broken".
  hinted <- ui_html(.sdt_empty("Nothing to plot yet.", "Press build.",
                               mark = "!"))
  expect_true(grepl("sdt-empty-hint", hinted, fixed = TRUE))
  expect_true(grepl("Press build.", hinted, fixed = TRUE))
  expect_true(grepl(">!<", hinted, fixed = TRUE))
})

test_that(".sdt_skeleton() draws one bar per line asked for", {
  skip_if_not_installed("htmltools")
  bars <- function(lines) {
    count_in("class=\"sdt-skeleton\"", ui_html(.sdt_skeleton(lines)))
  }
  expect_identical(bars(1L), 1L)
  expect_identical(bars(3L), 3L)
  # More bars than there are widths: the cycle has to wrap rather than index
  # past the end of the width vector and emit "width: NA%".
  expect_identical(bars(7L), 7L)
  expect_false(grepl("NA", ui_html(.sdt_skeleton(7L)), fixed = TRUE))
  expect_true(grepl("height: 2rem", ui_html(.sdt_skeleton(1L, "2rem")),
                    fixed = TRUE))
})

test_that(".sdt_step() numbers the step it labels", {
  skip_if_not_installed("htmltools")
  html <- ui_html(.sdt_step(3, "Train a model on what the game produced."))
  expect_true(grepl("sdt-step-num", html, fixed = TRUE))
  expect_true(grepl(">3<", html, fixed = TRUE))
  expect_true(grepl("Train a model", html, fixed = TRUE))
})

# .sdt_chip() itself is covered in test-app.R ("chips escape what they are
# given"); its colour classes are asserted here through the two call sites
# that matter, the status line and the marginals card.

test_that("the stylesheet is built on the accent the theme declares", {
  skip_if_no_shiny()
  css <- .sdt_app_css()
  # Two copies of the same colour, one in Sass and one in CSS, is exactly the
  # kind of thing that drifts unnoticed.
  expect_true(grepl(paste0("--sdt-accent: ", .sdt_app_accent(), ";"), css,
                    fixed = TRUE))
  # The sticker's blue is deliberately not the suite accent; recording it
  # here keeps that a decision rather than an accident.
  expect_false(identical(.sdt_app_accent(), .sdt_logo_accent()))
  expect_true(grepl("prefers-reduced-motion", css, fixed = TRUE))
})


# Plot recolouring -------------------------------------------------------------

test_that(".sdt_app_plot() leaves its argument alone without a session", {
  # Called from a test or the console there is no container to match, so the
  # plot has to come back exactly as it went in.
  expect_identical(.sdt_app_plot("not a plot"), "not a plot")
  expect_null(.sdt_app_plot(NULL))

  plot <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3),
                          ggplot2::aes(x, y)) + ggplot2::geom_point()
  expect_identical(.sdt_app_plot(plot), plot)
  expect_null(.sdt_plot_ink())
  expect_null(.sdt_colour_mode())
})

test_that(".sdt_recolour() leaves a blanked element blank", {
  # The board map blanks its axis text because the axes are a layout grid,
  # not a scale anyone should read. An earlier version of this helper put
  # element_text() back in its place and resurrected the numbers down the
  # side of the board.
  expect_null(.sdt_recolour(ggplot2::element_blank(), "#DEDAE6"))
  expect_null(.sdt_recolour_line(ggplot2::element_blank(), "#DEDAE6"))

  # Everything else about a text element survives being recoloured.
  element <- .sdt_recolour(ggplot2::element_text(size = 9), "#DEDAE6")
  expect_s3_class(element, "element_text")
  expect_identical(element$colour, "#DEDAE6")
  expect_identical(element$size, 9)
  expect_s3_class(.sdt_recolour(NULL, "#DEDAE6"), "element_text")
  # Handed the wrong kind of element, each helper declines rather than
  # returning something ggplot2 would reject at draw time.
  expect_null(.sdt_recolour(ggplot2::element_line(), "#DEDAE6"))

  line <- .sdt_recolour_line(ggplot2::element_line(linewidth = 2), "#DEDAE6")
  expect_s3_class(line, "element_line")
  expect_identical(line$colour, "#DEDAE6")
  expect_s3_class(.sdt_recolour_line(NULL, "#DEDAE6"), "element_line")
  expect_null(.sdt_recolour_line(ggplot2::element_text(), "#DEDAE6"))
})

test_that(".sdt_ink_theme() recolours the ink without unblanking the axes", {
  plot <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3),
                          ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank())

  theme <- .sdt_ink_theme(plot$theme, "#DEDAE6")
  expect_identical(theme$text$colour, "#DEDAE6")
  expect_identical(theme$plot.title$colour, "#DEDAE6")
  # The blanked parent does not come back, and the children the helper does
  # set still inherit their parent's blank, so the axis stays unreadable on
  # purpose rather than being recoloured into legibility.
  expect_false("axis.text" %in% names(theme))
  expect_true(theme$axis.text.x$inherit.blank)

  # A grid line blanked in its own right is dropped the same way.
  blanked_grid <- .sdt_ink_theme(
    ggplot2::theme(panel.grid.major = ggplot2::element_blank()), "#DEDAE6"
  )
  expect_false("panel.grid.major" %in% names(blanked_grid))

  # With no theme at all every listed element is created from scratch.
  bare <- .sdt_ink_theme(NULL, "#DEDAE6")
  expect_true(all(.sdt_ink_elements() %in% names(bare)))
  expect_s3_class(bare$panel.grid.major, "element_line")
})

test_that("a recoloured plot still resolves its blanked axis to blank", {
  # The end of the same story, read off the plot ggplot2 would actually
  # draw rather than off the partial theme: it is the resolved element that
  # decides whether the numbers appear down the side of the board.
  skip_if_not_installed("ggplot2", "3.5.1")
  plot <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3),
                          ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = ggplot2::element_blank())

  recoloured <- plot + .sdt_ink_theme(plot$theme, "#DEDAE6")
  resolved <- ggplot2::complete_theme(recoloured$theme)
  expect_s3_class(ggplot2::calc_element("axis.text.x", resolved),
                  "element_blank")
  expect_s3_class(ggplot2::calc_element("axis.text.y", resolved),
                  "element_blank")
  expect_identical(
    ggplot2::calc_element("axis.title.x", resolved)$colour, "#DEDAE6"
  )
})

test_that(".sdt_app_plot() recolours once the page reports a colour mode", {
  skip_if_no_shiny()
  plot <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3),
                          ggplot2::aes(x, y)) + ggplot2::geom_point()
  # The dark-mode toggle is the fallback source of ink, and it is why the
  # toggle in R/app.R carries an id; reading it is what keeps ggiraph and
  # plotly panels legible on the dark theme.
  shiny::testServer(function(input, output, session) {
    ink <- shiny::reactive(.sdt_plot_ink())
    recoloured <- shiny::reactive(.sdt_app_plot(plot))
  }, {
    expect_null(ink())
    # Untouched while there is nothing to match: no session-level toggle has
    # reported, so there is no container colour to draw the ink in.
    expect_identical(recoloured(), plot)

    session$setInputs(colour_mode = "dark")
    expect_identical(ink(), "#DEDAE6")
    # The ink actually reaches the plot, rather than the plot merely coming
    # back different from the one that went in.
    expect_identical(recoloured()$theme$text$colour, "#DEDAE6")

    session$setInputs(colour_mode = "light")
    expect_identical(ink(), "#2B2530")
    expect_identical(recoloured()$theme$text$colour, "#2B2530")
    # An empty toggle value is not a colour mode.
    session$setInputs(colour_mode = "")
    expect_null(ink())
  })
})

test_that(".sdt_output_colour() only trusts a usable colour", {
  skip_if_not_installed("shiny")
  # getCurrentOutputInfo() hands back reactives, is absent outside a render
  # context, and can carry an empty string when the CSS has not resolved.
  expect_null(.sdt_output_colour(NULL, "fg"))
  expect_null(.sdt_output_colour(list(bg = function() "#000000"), "fg"))
  expect_identical(
    .sdt_output_colour(list(fg = function() "#123456"), "fg"), "#123456"
  )
  expect_null(.sdt_output_colour(list(fg = function() ""), "fg"))
  expect_null(.sdt_output_colour(list(fg = function() stop("no")), "fg"))
})


# Render guards ----------------------------------------------------------------

test_that(".sdt_need_pkgs() names the packages it is missing", {
  skip_if_not_installed("shiny")
  expect_true(.sdt_need_pkgs("stats", "The cohort table"))
  # The message has to be actionable: a panel that says only "not available"
  # sends the reader to the issue tracker instead of to install.packages().
  expect_error(
    .sdt_need_pkgs("notarealpackage99", "The cohort table"),
    "notarealpackage99"
  )
})

test_that(".sdt_safely() reports a failure instead of taking the panel down", {
  skip_if_not_installed("shiny")
  expect_identical(.sdt_safely(1 + 1), 2)
  # Outside a render context the validation condition surfaces as an error
  # carrying the original message; inside one it becomes the panel's text.
  expect_error(.sdt_safely(stop("the fill scale is empty")),
               "the fill scale is empty")
})
