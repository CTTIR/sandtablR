# Visualisation ---------------------------------------------------------------
# All plot functions return ggplot2 objects; the caller decides when to render.
# The Shiny workbench wraps these rather than building its own plots, so what
# you see in the app is exactly what you get from the console.

# Sighting-category colours, used wherever triage appears.
.sdt_triage_palette <- function() {
  c(red = "#C0392B", yellow = "#E4A700", green = "#27AE60")
}

# Suite accent, used for single-series plots.
.sdt_accent <- function() "#5E2C8E"

# The shared plot theme.
#
# The backgrounds are transparent rather than white. On a page that is the same
# thing, but in the workbench it is what lets a plot sit on a dark card instead
# of punching a white rectangle through it: Shiny paints the plot device in the
# output container's own colour, and an opaque theme background covers that up.
# .sdt_app_plot() then recolours the ink to match.
.sdt_theme <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = NA, colour = NA),
      panel.background = ggplot2::element_rect(fill = NA, colour = NA),
      legend.background = ggplot2::element_rect(fill = NA, colour = NA),
      legend.key = ggplot2::element_rect(fill = NA, colour = NA)
    )
}

.sdt_blank_plot <- function(title) {
  ggplot2::ggplot() +
    ggplot2::labs(title = title) +
    .sdt_theme()
}

#' Draw the board, with the casualties standing on it
#'
#' The picture the exercise is played on: the nine areas of the published board
#' laid out as in the source's Fig. 1, with every casualty drawn as a point in
#' the area it had reached by a given moment. Scrub `at` across the game and
#' you watch the casualty stream move over the board, which is the one view
#' that makes a decision path legible without reading a table.
#'
#' A casualty's area at time `at` is the last area it had *entered* by then,
#' which is the same rule [summary.game_run()] uses. That matters at the end of
#' the game: someone still being operated on when the clock stops is in
#' theatre, not back where they came from.
#'
#' @param run A `game_run` from [sdt_simulate()].
#' @param at Time in minutes to draw. Defaults to the end of the game.
#' @param replication Which replication to draw.
#' @param interactive If `TRUE` and \pkg{ggiraph} is installed, draw the
#'   casualties as interactive points carrying a tooltip with the patient card.
#'   Wrap the result in [ggiraph::girafe()] to get the interactivity; it
#'   renders as an ordinary plot otherwise.
#'
#' @return A \pkg{ggplot2} object.
#' @seealso [sdt_plot_flow()] for the same event log as a Gantt chart.
#' @export
#' @examples
#' run <- sdt_simulate(sdt_reference_scenario(), replications = 1,
#'                     seed = 1, verbose = FALSE)
#' sdt_plot_board(run)
#'
#' # Halfway through the game.
#' sdt_plot_board(run, at = sdt_reference_horizon() / 2)
sdt_plot_board <- function(run, at = NULL, replication = 1L,
                           interactive = FALSE) {
  .check_class(run, "game_run")
  .check_flag(interactive)
  replication <- .check_count(replication)
  at <- at %||% run$meta$horizon
  if (!is.numeric(at) || length(at) != 1L || is.na(at)) {
    cli::cli_abort("{.arg at} must be a single number.")
  }

  board <- .sdt_board_cells(run, at, replication)
  occupancy <- .sdt_board_occupancy(run, at, replication)
  points <- .sdt_board_points(occupancy, board)

  gg <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = board,
      ggplot2::aes(
        xmin = .data$xmin, xmax = .data$xmax,
        ymin = .data$ymin, ymax = .data$ymax
      ),
      fill = "#F6F3FA", colour = "#C6BBD6", linewidth = 0.4
    ) +
    # A tinted strip along the top of each cell, so the label sits on its own
    # band and the area reads as a room rather than as an empty rectangle.
    ggplot2::geom_rect(
      data = board,
      ggplot2::aes(
        xmin = .data$xmin, xmax = .data$xmax,
        ymin = .data$ymax - 0.17, ymax = .data$ymax
      ),
      fill = "#E4DCEF", colour = NA
    ) +
    ggplot2::geom_text(
      data = board,
      ggplot2::aes(
        x = .data$xmin + 0.18, y = .data$ymax - 0.085,
        label = paste0(.data$position, "  ", .data$short)
      ),
      hjust = 0, vjust = 0.5, size = 2.7, colour = "#3D1C5C", fontface = "bold"
    ) +
    # The headcount, right-aligned in the same band. Reading it off the label
    # is faster than counting dots once an area is busy.
    ggplot2::geom_text(
      data = board,
      ggplot2::aes(
        x = .data$xmax - 0.18, y = .data$ymax - 0.085,
        label = ifelse(.data$n > 0L, as.character(.data$n), "")
      ),
      hjust = 1, vjust = 0.5, size = 2.7, colour = "#6C5B85"
    )

  gg <- gg + .sdt_board_point_layer(points, interactive)

  gg +
    ggplot2::scale_fill_manual(
      values = .sdt_triage_palette(), drop = FALSE, name = "Sighting"
    ) +
    ggplot2::coord_equal(ratio = 3.6, expand = FALSE, clip = "off") +
    ggplot2::labs(
      title = "The board",
      subtitle = .sdt_board_subtitle(run, at, replication, nrow(points)),
      x = NULL, y = NULL
    ) +
    .sdt_theme() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

# Say how many casualties are on the board, and account for the rest. A board
# holding 33 of 40 casualties is either mid-game or congested, and the caption
# should not leave the reader to work out which.
.sdt_board_subtitle <- function(run, at, replication, shown) {
  total <- nrow(tibble::as_tibble(run$scenario$cohort))
  waiting <- max(0L, total - shown)
  sprintf(
    "Replication %d at %.0f min \u2014 %d of %d casualties on the board%s",
    replication, at, shown, total,
    if (waiting) sprintf(", %d not yet arrived", waiting) else ""
  )
}

# The board's cells, with the short display label and the headcount at `at`
# joined on. Built here rather than in the plot so the counting rule and the
# drawing rule cannot drift apart.
.sdt_board_cells <- function(run, at, replication) {
  board <- .ref_design("board")
  board$short <- .sdt_label_area(board$area)
  occupancy <- .sdt_board_occupancy(run, at, replication)
  counts <- table(factor(occupancy$area, levels = board$area))
  board$n <- as.integer(counts[board$area])
  board$n[is.na(board$n)] <- 0L
  board
}

# The casualty points, interactive when ggiraph is there to make them so.
# Split out because the two branches must produce the same aesthetics, and a
# conditional inside the ggplot call would make that easy to get wrong.
.sdt_board_point_layer <- function(points, interactive) {
  mapping <- ggplot2::aes(
    x = .data$x, y = .data$y, fill = .data$triage
  )
  if (!nrow(points)) {
    return(ggplot2::geom_blank(data = points, mapping = mapping))
  }
  if (interactive && requireNamespace("ggiraph", quietly = TRUE)) {
    return(ggiraph::geom_point_interactive(
      data = points,
      mapping = utils::modifyList(
        mapping,
        ggplot2::aes(tooltip = .data$tooltip, data_id = .data$id)
      ),
      shape = 21, size = 3.2, stroke = 0.4, colour = "white"
    ))
  }
  ggplot2::geom_point(
    data = points, mapping = mapping,
    shape = 21, size = 3.2, stroke = 0.4, colour = "white"
  )
}

# Where each casualty stood at time `at`: the last area it had entered by then.
# A casualty that had not yet reached triage is not on the board at all, which
# is why this filters rather than defaulting them into an area.
.sdt_board_occupancy <- function(run, at, replication) {
  ev <- run$events
  ev <- ev[ev$replication == replication & ev$start <= at, , drop = FALSE]
  if (!nrow(ev)) {
    return(tibble::tibble(id = character(), area = character(),
                          triage = character(), tooltip = character()))
  }
  grouped <- dplyr::group_by(ev, .data$id)
  latest <- dplyr::slice_max(grouped, .data$start, n = 1L, with_ties = FALSE)
  latest <- dplyr::ungroup(latest)

  co <- tibble::as_tibble(run$scenario$cohort)
  out <- dplyr::left_join(
    latest[, c("id", "area")],
    co[, intersect(
      c("id", "triage", "therapy", "age", "sex", "life_points", "ventilated",
        "injury_region"),
      names(co)
    )],
    by = "id"
  )
  out$triage <- factor(out$triage, levels = sdt_triage_levels())
  out$tooltip <- .sdt_board_tooltip(out)
  out
}

.sdt_board_tooltip <- function(x) {
  sprintf(
    "%s\n%s, %s\n%s, life %s%s\n%s",
    x$id,
    paste0(x$age, "y"), x$sex,
    x$injury_region, x$life_points,
    ifelse(isTRUE(x$ventilated), ", ventilated", ""),
    paste0("triage ", as.character(x$triage), ", therapy ", x$therapy)
  )
}

# Lay the casualties out inside their area's rectangle as a tidy grid, so a
# crowded area reads as crowded rather than as one overplotted dot.
.sdt_board_points <- function(occupancy, board) {
  if (!nrow(occupancy)) {
    occupancy$x <- numeric()
    occupancy$y <- numeric()
    return(occupancy)
  }
  rows <- lapply(split(occupancy, occupancy$area), function(part) {
    cell <- board[board$area == part$area[[1L]], , drop = FALSE]
    if (!nrow(cell)) {
      part$x <- NA_real_
      part$y <- NA_real_
      return(part)
    }
    n <- nrow(part)
    width <- cell$xmax - cell$xmin
    height <- cell$ymax - cell$ymin
    # Choose a column count that keeps the grid roughly as wide as the cell.
    # The usable floor is what is left below the label band.
    floor_top <- cell$ymax - 0.17
    floor_height <- floor_top - cell$ymin
    cols <- max(1L, min(n, ceiling(sqrt(n * width / (floor_height * 3.6)))))
    nrows <- ceiling(n / cols)
    col <- (seq_len(n) - 1L) %% cols
    row <- (seq_len(n) - 1L) %/% cols
    part$x <- cell$xmin + width * (col + 0.5) / cols
    part$y <- floor_top - floor_height * (row + 0.5) / nrows
    part
  })
  out <- dplyr::bind_rows(rows)
  out[order(out$id), , drop = FALSE]
}

#' Plot the triage mix of a scenario
#'
#' @param scenario A `scenario` object.
#' @return A \pkg{ggplot2} object.
#' @export
#' @examples
#' sdt_plot_triage_mix(sdt_scenario(sdt_synthetic_cohort(20, seed = 1)))
sdt_plot_triage_mix <- function(scenario) {
  .check_class(scenario, "scenario")
  df <- dplyr::count(tibble::as_tibble(scenario$cohort), .data$triage,
                     name = "n")
  df$triage <- factor(df$triage, levels = sdt_triage_levels())
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$triage, y = .data$n, fill = .data$triage)
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(
      values = .sdt_triage_palette(), drop = FALSE
    ) +
    ggplot2::labs(
      title = "Triage category mix",
      x = "Sighting category", y = "Casualties", fill = "Triage"
    ) +
    .sdt_theme()
}

#' Plot resource utilisation from a game run
#'
#' @param run A `game_run` from [sdt_simulate()].
#' @return A \pkg{ggplot2} object.
#' @export
#' @examples
#' co <- sdt_synthetic_cohort(20, seed = 1)
#' sc <- sdt_scenario(co, resources = list(or = 2))
#' run <- sdt_simulate(sc, replications = 1, verbose = FALSE)
#' sdt_plot_utilisation(run)
sdt_plot_utilisation <- function(run) {
  .check_class(run, "game_run")
  res <- run$resources
  if (!nrow(res)) {
    return(.sdt_blank_plot("No resource records"))
  }
  ggplot2::ggplot(
    res,
    ggplot2::aes(x = .data$time, y = .data$server, colour = .data$resource)
  ) +
    ggplot2::geom_step() +
    ggplot2::facet_wrap(ggplot2::vars(.data$replication), ncol = 1L) +
    ggplot2::labs(
      title = "Resource utilisation over time",
      x = "Time (min)", y = "In use", colour = "Board area"
    ) +
    .sdt_theme()
}

#' Plot the end-of-game distribution against the published reference
#'
#' Draws the observed proportion per reporting location as bars, with the
#' published reference proportion overlaid as a point and the gap drawn as a
#' segment between them. It is the honest version of a calibration claim: the
#' gap is visible rather than asserted.
#'
#' @param run A `game_run` from [sdt_simulate()].
#' @param version Which evaluated version of the exercise to compare against,
#'   `"2.0"` or `"1.0"`.
#' @return A \pkg{ggplot2} object.
#' @seealso [summary.game_run()], which produces the underlying tibble.
#' @export
#' @examples
#' run <- sdt_simulate(sdt_reference_scenario(), replications = 4,
#'                     seed = 1, verbose = FALSE)
#' sdt_plot_end_state(run)
#' sdt_plot_end_state(run, version = "1.0")
sdt_plot_end_state <- function(run, version = c("2.0", "1.0")) {
  .check_class(run, "game_run")
  version <- rlang::arg_match(version)
  df <- summary(run, reference = TRUE, version = version)
  df$label <- factor(df$label, levels = .sdt_label_area(.sdt_end_state_areas()))

  ggplot2::ggplot(df, ggplot2::aes(x = .data$label, y = .data$proportion)) +
    ggplot2::geom_col(fill = .sdt_accent(), width = 0.7, alpha = 0.85) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = .data$label, yend = .data$reference),
      colour = "#C0392B", linewidth = 0.4, linetype = "dotted", na.rm = TRUE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$reference),
      colour = "#C0392B", size = 2.5, na.rm = TRUE
    ) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
    ggplot2::labs(
      title = "End-of-game location",
      subtitle = sprintf(
        "Bars: simulated. Points: published reference, version %s.", version
      ),
      x = NULL, y = "Share of casualties"
    ) +
    .sdt_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
    )
}

#' Plot patient flow through the board
#'
#' A Gantt chart of one replication: each casualty is a row, each board area a
#' coloured bar spanning the time it was there. This is the picture of the
#' event log, and the fastest way to see where a board is congested.
#'
#' @param run A `game_run` from [sdt_simulate()].
#' @param replication Which replication to draw.
#' @param max_casualties Cap on the number of casualties shown, to keep the
#'   chart readable. The earliest arrivals are kept.
#'
#' @return A \pkg{ggplot2} object.
#' @export
#' @examples
#' co <- sdt_synthetic_cohort(15, seed = 1)
#' run <- sdt_simulate(sdt_scenario(co), replications = 1, verbose = FALSE)
#' sdt_plot_flow(run)
sdt_plot_flow <- function(run, replication = 1L, max_casualties = 40L) {
  .check_class(run, "game_run")
  replication <- .check_count(replication)
  max_casualties <- .check_count(max_casualties)

  ev <- run$events[run$events$replication == replication, , drop = FALSE]
  if (!nrow(ev)) {
    return(.sdt_blank_plot("No events for that replication"))
  }

  first_seen <- stats::aggregate(start ~ id, data = ev, FUN = min)
  keep <- utils::head(
    first_seen$id[order(first_seen$start)], max_casualties
  )
  ev <- ev[ev$id %in% keep, , drop = FALSE]
  ev$id <- factor(ev$id, levels = rev(keep))
  ev$area <- factor(ev$area, levels = sdt_board_areas())

  ggplot2::ggplot(ev) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$start, xend = .data$end,
        y = .data$id, yend = .data$id, colour = .data$area
      ),
      linewidth = 3
    ) +
    ggplot2::scale_colour_viridis_d(drop = FALSE, option = "D") +
    ggplot2::labs(
      title = "Patient flow",
      subtitle = paste("Replication", replication),
      x = "Time (min)", y = NULL, colour = "Board area"
    ) +
    .sdt_theme()
}
