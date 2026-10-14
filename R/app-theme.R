# Workbench theme and shared UI helpers ---------------------------------------
#
# The app's module functions live in R/ rather than under inst/shiny. That is
# deliberate: code under inst/ is invisible to R CMD check, lintr and covr, so
# an app kept there is the one part of a package nothing verifies. Here the
# modules are checked like any other function and driven headlessly by
# shiny::testServer() in the test suite. inst/shiny/sandtablR/app.R stays as a
# thin launcher so the app can still be deployed as a directory.
#
# Everything visual is defined once here. A module that wants a metric, an
# empty state or a coloured chip asks for it rather than writing its own, so
# the six panels cannot drift into six different-looking apps.

# Suite accent, matching _pkgdown.yml.
#
# This is the suite-wide accent, and it is not the logo's. The hex sticker is
# built around #1565C0 on a near-black #08101B field. Both are named here so
# the difference is a recorded decision rather than something noticed later on
# a rendered page.
.sdt_app_accent <- function() "#5E2C8E"

.sdt_logo_accent <- function() "#1565C0"

# Serve the package's own assets. The logo is installed under inst/www rather
# than read from man/figures, because man/ is documentation and is not laid
# out predictably in an installed package.
.sdt_mount_assets <- function() {
  path <- system.file("www", package = "sandtablR")
  if (nzchar(path) && dir.exists(path)) {
    shiny::addResourcePath("sandtablR", path)
  }
  invisible(path)
}

# The navbar brand: the sticker beside the package name.
.sdt_brand <- function() {
  htmltools::div(
    class = "d-flex align-items-center gap-2",
    htmltools::img(
      src = "sandtablR/logo.png", height = "34", alt = "sandtablR logo"
    ),
    htmltools::span("sandtablR", class = "fw-semibold")
  )
}

.sdt_app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    primary = .sdt_app_accent(),
    base_font = bslib::font_google("Inter"),
    code_font = bslib::font_google("JetBrains Mono")
  )
}

# The stylesheet.
#
# Colours are declared once as custom properties and overridden wholesale for
# dark mode, rather than being repeated per rule. bslib's dark-mode toggle sets
# data-bs-theme on <html>, so that attribute is the switch; the
# prefers-color-scheme block covers the first paint before the toggle has run.
.sdt_app_css <- function() {
  paste(
    ":root {",
    "  --sdt-accent: #5E2C8E;",
    "  --sdt-accent-soft: #F1ECF7;",
    "  --sdt-accent-text: #3D1C5C;",
    "  --sdt-muted: #6B6478;",
    "  --sdt-line: #E3DEEA;",
    "  --sdt-surface: #FBFAFD;",
    "  --sdt-skeleton: #E9E5F0;",
    "  --sdt-skeleton-hi: #F5F2F9;",
    "}",
    "[data-bs-theme=\"dark\"] {",
    "  --sdt-accent: #B79AE0;",
    "  --sdt-accent-soft: #2A1F3A;",
    "  --sdt-accent-text: #D9C9F0;",
    "  --sdt-muted: #9A93A8;",
    "  --sdt-line: #3A3346;",
    "  --sdt-surface: #1C1922;",
    "  --sdt-skeleton: #2B2634;",
    "  --sdt-skeleton-hi: #363040;",
    "}",
    "@media (prefers-color-scheme: dark) {",
    "  :root:not([data-bs-theme=\"light\"]) {",
    "    --sdt-accent: #B79AE0;",
    "    --sdt-accent-soft: #2A1F3A;",
    "    --sdt-accent-text: #D9C9F0;",
    "    --sdt-muted: #9A93A8;",
    "    --sdt-line: #3A3346;",
    "    --sdt-surface: #1C1922;",
    "    --sdt-skeleton: #2B2634;",
    "    --sdt-skeleton-hi: #363040;",
    "  }",
    "}",

    # Numbers line up under one another wherever they are compared.
    ".sdt-metric {",
    "  font-variant-numeric: tabular-nums; font-size: 1.6rem;",
    "  font-weight: 600; line-height: 1.1;",
    "}",
    ".sdt-metric-empty { color: var(--sdt-muted); font-weight: 400; }",
    ".sdt-metric-note {",
    "  font-size: 0.78rem; color: var(--sdt-muted); margin-top: 0.15rem;",
    "}",

    ".sdt-chip {",
    "  display: inline-block; padding: 0.1rem 0.5rem; border-radius: 999px;",
    "  font-size: 0.75rem; font-weight: 600; letter-spacing: 0.02em;",
    "  margin: 0 0.15rem 0.2rem 0;",
    "}",
    ".sdt-chip-red { background: #C0392B; color: #fff; }",
    ".sdt-chip-yellow { background: #E4A700; color: #1a1a1a; }",
    ".sdt-chip-green { background: #27AE60; color: #fff; }",
    ".sdt-chip-glass { background: #E8E1F2; color: #3D1C5C; }",
    ".sdt-chip-grey { background: #8A7FA0; color: #fff; }",
    ".sdt-chip-black { background: #2B2B2B; color: #fff; }",
    ".sdt-chip-muted {",
    "  background: var(--sdt-accent-soft); color: var(--sdt-accent-text);",
    "}",

    # The empty state. Every panel that can have nothing to show uses this, so
    # "nothing here yet" always looks deliberate and always says what to do.
    ".sdt-empty {",
    "  display: flex; flex-direction: column; align-items: center;",
    "  justify-content: center; gap: 0.4rem; padding: 2.5rem 1.5rem;",
    "  text-align: center; min-height: 180px;",
    "}",
    ".sdt-empty-mark {",
    "  width: 2.4rem; height: 2.4rem; border-radius: 50%;",
    "  background: var(--sdt-accent-soft); color: var(--sdt-accent-text);",
    "  display: flex; align-items: center; justify-content: center;",
    "  font-size: 1.1rem; font-weight: 700;",
    "}",
    ".sdt-empty-title { font-weight: 600; }",
    ".sdt-empty-hint {",
    "  color: var(--sdt-muted); font-size: 0.88rem; max-width: 34rem;",
    "}",

    # Shown while a long computation is running, in place of a blank card.
    ".sdt-skeleton {",
    "  border-radius: 6px; margin-bottom: 0.6rem;",
    "  background: linear-gradient(90deg, var(--sdt-skeleton) 25%,",
    "    var(--sdt-skeleton-hi) 37%, var(--sdt-skeleton) 63%);",
    "  background-size: 400% 100%;",
    "  animation: sdt-shimmer 1.4s ease-in-out infinite;",
    "}",
    "@keyframes sdt-shimmer {",
    "  0% { background-position: 100% 50%; }",
    "  100% { background-position: 0 50%; }",
    "}",
    "@media (prefers-reduced-motion: reduce) {",
    "  .sdt-skeleton { animation: none; }",
    "}",

    # A short line under a panel heading saying what to do next.
    ".sdt-step {",
    "  display: flex; align-items: center; gap: 0.5rem;",
    "  padding: 0.5rem 0.85rem; border-radius: 6px;",
    "  background: var(--sdt-accent-soft); color: var(--sdt-accent-text);",
    "  font-size: 0.85rem; margin-bottom: 0.75rem;",
    "}",
    ".sdt-step-num {",
    "  flex: 0 0 auto; width: 1.4rem; height: 1.4rem; border-radius: 50%;",
    "  background: var(--sdt-accent); color: #fff; font-weight: 700;",
    "  font-size: 0.75rem; display: flex; align-items: center;",
    "  justify-content: center;",
    "}",

    ".card-header { font-weight: 600; }",
    # Outline buttons are unreadable against the dark surface at Bootstrap's
    # defaults: secondary grey on near-black. Bind them to the accent instead.
    ".btn-outline-secondary {",
    "  color: var(--sdt-accent-text); border-color: var(--sdt-line);",
    "}",
    ".btn-outline-secondary:hover {",
    "  background: var(--sdt-accent-soft); color: var(--sdt-accent-text);",
    "  border-color: var(--sdt-accent);",
    "}",
    ".navbar .btn-outline-secondary { color: inherit; }",
    # Sidebars stack a lot of controls; group them so the primary action is
    # not pushed below the fold by a wall of numeric inputs.
    ".sdt-sidebar-group { margin-bottom: 0.35rem; }",
    ".sdt-sidebar-actions { position: sticky; bottom: 0; padding-top: 0.5rem;",
    "  background: inherit; }",
    sep = "\n"
  )
}

# A coloured chip. The label is passed as a text child, which htmltools
# escapes: a cohort can be loaded from a file, so nothing reaching the page is
# assumed to be free of markup. Escaping it again by hand would only produce
# visibly mangled text.
.sdt_chip <- function(label, kind) {
  htmltools::span(class = paste0("sdt-chip sdt-chip-", kind), label)
}

# A placeholder shown wherever a panel has nothing to draw yet.
#
# It always says three things: that this is expected, what is missing, and
# which control produces it. A panel that renders blank leaves the user to
# guess whether the app is broken, and they usually guess wrong.
.sdt_empty <- function(message, hint = NULL, mark = "\u2192") {
  htmltools::div(
    class = "sdt-empty",
    htmltools::div(class = "sdt-empty-mark", mark),
    htmltools::div(class = "sdt-empty-title", message),
    if (!is.null(hint)) htmltools::div(class = "sdt-empty-hint", hint)
  )
}

# Grey shimmering bars standing in for content that is on its way. Used where
# a computation takes long enough that an empty card would read as a failure.
.sdt_skeleton <- function(lines = 3L, height = "1.1rem") {
  htmltools::div(
    class = "sdt-skeleton-wrap",
    lapply(seq_len(lines), function(i) {
      htmltools::div(
        class = "sdt-skeleton",
        style = sprintf(
          "height: %s; width: %d%%;", height,
          c(100L, 92L, 78L, 85L, 60L)[[(i - 1L) %% 5L + 1L]]
        )
      )
    })
  )
}

# The "you are here, do this next" line under a panel heading.
.sdt_step <- function(number, text) {
  htmltools::div(
    class = "sdt-step",
    htmltools::span(class = "sdt-step-num", as.character(number)),
    htmltools::span(text)
  )
}

# A metric with a caption, for use inside bslib::value_box().
#
# The empty case is the point. bslib renders a value box with a NULL body as a
# titled grey slab, which reads as broken rather than as waiting, so this
# always emits something: an em-dash and the reason it is empty.
.sdt_metric <- function(value, note = NULL) {
  empty <- is.null(value) || !length(value) || all(is.na(value)) ||
    identical(value, "")
  htmltools::tagList(
    htmltools::div(
      class = if (empty) "sdt-metric sdt-metric-empty" else "sdt-metric",
      if (empty) "\u2014" else value
    ),
    if (!is.null(note)) htmltools::div(class = "sdt-metric-note", note)
  )
}

# Guard an optional dependency inside a render context. Returns TRUE when the
# packages are present; otherwise stops the render with a message naming what
# to install, rather than letting the panel fail obscurely.
.sdt_need_pkgs <- function(pkgs, what) {
  have <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  shiny::validate(shiny::need(
    all(have),
    sprintf(
      "%s needs the %s package%s. Install %s.",
      what,
      paste(pkgs[!have], collapse = ", "),
      if (sum(!have) == 1L) "" else "s",
      paste0("install.packages(c(\"", paste(pkgs[!have], collapse = "\", \""),
             "\"))")
    )
  ))
  TRUE
}

# shiny::renderPlot(), with the plot recoloured to match its card.
#
# Every static plot in the workbench goes through this rather than through
# renderPlot() directly, so no panel can be added later that renders dark text
# on a dark card. The expression is captured as a quosure and evaluated inside
# the reactive context, so reactive dependencies register exactly as they would
# have.
.sdt_render_plot <- function(expr, ...) {
  quo <- rlang::enquo(expr)
  shiny::renderPlot(
    {
      .sdt_app_plot(rlang::eval_tidy(quo))
    },
    # The device canvas is transparent, so the card's own colour shows through
    # and the plot reads as part of the card rather than as a white rectangle
    # punched into it. The plot theme leaves its backgrounds unfilled for the
    # same reason; both halves are needed, and either alone shows white.
    bg = "transparent",
    ...
  )
}

# Recolour a plot's ink to match the card it is being drawn on.
#
# The package's plots are built for a page: dark text, light grid. Dropped
# unchanged onto the workbench's dark theme they are unreadable, and a plot
# that has to be squinted at is worse than no plot. Shiny reports the output
# container's resolved foreground and background through
# shiny::getCurrentOutputInfo(), so the colours come from the live CSS rather
# than from a second copy of the palette kept here and drifting.
#
# Called outside a render context -- from a test, or from the console -- it
# returns the plot untouched, because there is no container to match.
.sdt_app_plot <- function(plot) {
  if (!inherits(plot, "ggplot")) {
    return(plot)
  }
  fg <- .sdt_plot_ink()
  if (is.null(fg)) {
    return(plot)
  }
  plot + .sdt_ink_theme(plot$theme, fg)
}

# The text elements worth recolouring. Anything not listed inherits from
# `text`, and anything a plot has deliberately blanked is left blanked -- see
# .sdt_recolour().
.sdt_ink_elements <- function() {
  c("text", "axis.text", "axis.text.x", "axis.text.y", "axis.title",
    "axis.title.x", "axis.title.y", "plot.title", "plot.subtitle",
    "plot.caption", "legend.text", "legend.title", "strip.text",
    "strip.text.x", "strip.text.y")
}

.sdt_ink_theme <- function(theme, fg) {
  parts <- list()
  for (key in .sdt_ink_elements()) {
    element <- if (is.null(theme)) NULL else theme[[key]]
    recoloured <- .sdt_recolour(element, fg)
    if (!is.null(recoloured)) {
      parts[[key]] <- recoloured
    }
  }
  grid <- .sdt_recolour_line(
    if (is.null(theme)) NULL else theme[["panel.grid.major"]],
    grDevices::adjustcolor(fg, alpha.f = 0.18)
  )
  if (!is.null(grid)) {
    parts[["panel.grid.major"]] <- grid
  }
  do.call(ggplot2::theme, parts)
}

# Recolour one text element, preserving everything else about it.
#
# A blanked element stays blanked. Replacing it with element_text() would put
# back exactly what the plot went out of its way to remove: the board map
# blanks its axis text because the axes are a layout grid, not a scale anyone
# should read, and an earlier version of this helper resurrected the numbers
# 0.0 to 2.0 down the side of it.
.sdt_recolour <- function(element, colour) {
  if (inherits(element, "element_blank")) {
    return(NULL)
  }
  if (inherits(element, "element_text")) {
    element$colour <- colour
    return(element)
  }
  if (is.null(element)) {
    # inherit.blank, so an element the plot's theme blanked further up stays
    # blanked. Without it this puts back a grid the board deliberately removed:
    # the element created here has nothing to inherit from and wins.
    return(ggplot2::element_text(colour = colour, inherit.blank = TRUE))
  }
  NULL
}

.sdt_recolour_line <- function(element, colour) {
  if (inherits(element, "element_blank")) {
    return(NULL)
  }
  if (inherits(element, "element_line")) {
    element$colour <- colour
    return(element)
  }
  if (is.null(element)) {
    return(ggplot2::element_line(colour = colour, inherit.blank = TRUE))
  }
  NULL
}

# The colour to draw a plot's text and grid in, or NULL when there is nothing
# to match -- outside a Shiny session, or in a test.
#
# Two sources, in order. shiny::getCurrentOutputInfo() resolves the container's
# actual CSS and is the better answer, but it is only populated for
# renderPlot(): a ggiraph or plotly output gets nothing from it, and those are
# exactly the panels a reader looks at first. The dark-mode toggle is the
# fallback, and it is why the toggle in R/app.R carries an id.
.sdt_plot_ink <- function() {
  info <- tryCatch(shiny::getCurrentOutputInfo(), error = function(e) NULL)
  fg <- .sdt_output_colour(info, "fg")
  if (!is.null(fg)) {
    return(fg)
  }
  mode <- .sdt_colour_mode()
  if (is.null(mode)) {
    return(NULL)
  }
  if (identical(mode, "dark")) "#DEDAE6" else "#2B2530"
}

# Which mode the page is in, read from the navbar toggle on the root session.
# Modules see their own session, so the lookup has to climb to the top: the
# toggle is not namespaced into any module.
.sdt_colour_mode <- function() {
  session <- shiny::getDefaultReactiveDomain()
  if (is.null(session)) {
    return(NULL)
  }
  root <- tryCatch(session$rootScope(), error = function(e) NULL)
  if (is.null(root)) {
    return(NULL)
  }
  value <- tryCatch(root$input[["colour_mode"]], error = function(e) NULL)
  if (is.character(value) && length(value) == 1L && nzchar(value)) {
    value
  } else {
    NULL
  }
}

# getCurrentOutputInfo() hands back reactive values, so each field has to be
# called to be read. It is also absent outside a render context.
.sdt_output_colour <- function(info, field) {
  if (is.null(info) || is.null(info[[field]])) {
    return(NULL)
  }
  value <- tryCatch(info[[field]](), error = function(e) NULL)
  usable <- is.character(value) && length(value) == 1L && nzchar(value)
  if (usable) value else NULL
}

# Every render in the app goes through this. A failure in one panel reports
# itself there and leaves the rest of the workbench usable, which matters when
# a long simulation is already loaded.
.sdt_safely <- function(expr, fallback = NULL) {
  tryCatch(
    expr,
    error = function(e) {
      shiny::validate(shiny::need(FALSE, conditionMessage(e)))
      fallback
    }
  )
}
