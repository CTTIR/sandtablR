# scenario -------------------------------------------------------------------
# A mass-casualty scenario: an ordered cohort of casualties plus the board
# resources (bed / OR / ICU counts) available to treat them.

new_scenario <- function(cohort = tibble::tibble(),
                         resources = list(),
                         meta = list()) {
  structure(
    list(cohort = cohort, resources = resources, meta = meta),
    class = "scenario"
  )
}

#' Build a mass-casualty scenario
#'
#' Assembles an ordered casualty cohort and the board resource capacities into
#' a validated `scenario` object, the central input to the simulation engine.
#'
#' @param cohort A data frame / tibble of casualties, one row per patient, with
#'   at least the columns produced by [sdt_patient_card()]
#'   (`id`, `age`, `sex`, `life_points`, `ventilated`, `injury_region`,
#'   `triage`, `therapy`, `arrival_order`).
#' @param resources Named list of integer capacities for board areas, e.g.
#'   `list(triage = 1, or = 2, icu_imc = 6, ed_red = 2, ed_yellow = 4,
#'   ward = 20)`. Missing areas default to `1`.
#' @param name Optional scenario name.
#' @param seed Optional integer seed recorded in metadata for reproducibility.
#'
#' @return An S3 object of class `scenario`.
#' @export
#' @examples
#' co <- sdt_synthetic_cohort(n = 20, seed = 1)
#' sdt_scenario(co, resources = list(or = 2, icu_imc = 6), name = "concert")
sdt_scenario <- function(cohort, resources = list(), name = NULL, seed = NULL) {
  cohort <- tibble::as_tibble(cohort)
  required <- c("id", "triage", "therapy", "arrival_order")
  missing <- setdiff(required, names(cohort))
  if (length(missing)) {
    n_missing <- length(missing)
    cli::cli_abort(c(
      "{.arg cohort} is missing {n_missing} required column{?s}.",
      "x" = "Missing: {.field {missing}}."
    ))
  }
  if (!is.list(resources)) {
    cli::cli_abort("{.arg resources} must be a named list of capacities.")
  }
  unknown <- setdiff(names(resources), sdt_board_areas())
  if (length(unknown)) {
    cli::cli_abort(c(
      "{.arg resources} names must be board areas.",
      "x" = "Unknown: {.val {unknown}}.",
      "i" = "Allowed: {.val {sdt_board_areas()}}."
    ))
  }
  # Fill defaults for any unspecified board area.
  defaults <- stats::setNames(
    as.list(rep(1L, length(sdt_board_areas()))), sdt_board_areas()
  )
  resources <- utils::modifyList(defaults, resources)
  resources <- lapply(resources, function(x) max(1L, as.integer(x)))

  # One source of truth for the column contract: sdt_cohort() validates every
  # vocabulary and range, and returns the rows ordered by arrival_order.
  new_scenario(
    cohort = sdt_cohort(cohort),
    resources = resources,
    meta = list(name = name %||% "unnamed", seed = seed, n = nrow(cohort))
  )
}

#' @export
format.scenario <- function(x, ...) {
  rlang::check_dots_empty()
  mix <- table(factor(x$cohort$triage, levels = sdt_triage_levels()))
  c(
    sprintf(
      "<scenario> %s: %d casualt%s",
      x$meta$name, x$meta$n, if (identical(x$meta$n, 1L)) "y" else "ies"
    ),
    paste0(
      "triage mix \u2014 ",
      paste(names(mix), as.integer(mix), sep = ": ", collapse = ", ")
    ),
    paste0(
      "resources \u2014 ",
      paste(names(x$resources), unlist(x$resources),
            sep = ": ", collapse = ", ")
    )
  )
}

#' @export
print.scenario <- function(x, ...) {
  rlang::check_dots_empty()
  cat(format(x), sep = "\n")
  invisible(x)
}

#' @export
summary.scenario <- function(object, ...) {
  rlang::check_dots_empty()
  object$cohort |>
    dplyr::count(.data$triage, .data$therapy, name = "n")
}

#' Coerce a scenario to a tibble
#'
#' Returns the scenario's casualty cohort. Provided so a scenario can be piped
#' straight into any function that expects a data frame.
#'
#' @param x A `scenario` object.
#' @param ... These dots are for future extensions and must be empty.
#' @return The `cohort` tibble held by the scenario.
#' @exportS3Method tibble::as_tibble
#' @examples
#' sc <- sdt_scenario(sdt_synthetic_cohort(n = 5, seed = 1))
#' tibble::as_tibble(sc)
as_tibble.scenario <- function(x, ...) {
  rlang::check_dots_empty()
  x$cohort
}

#' Plot a scenario
#'
#' The [ggplot2::autoplot()] method for a `scenario`; dispatches to
#' [sdt_plot_triage_mix()].
#'
#' @param object A `scenario` object.
#' @param ... These dots are for future extensions and must be empty.
#' @return A \pkg{ggplot2} object.
#' @exportS3Method ggplot2::autoplot
#' @examples
#' sc <- sdt_scenario(sdt_synthetic_cohort(n = 20, seed = 1))
#' ggplot2::autoplot(sc)
autoplot.scenario <- function(object, ...) {
  rlang::check_dots_empty()
  sdt_plot_triage_mix(object)
}
