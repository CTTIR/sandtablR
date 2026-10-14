# cohort ---------------------------------------------------------------------
# An ordered set of casualties: the tabular counterpart of a list of
# patient_card objects. Validation is vectorised over columns rather than
# looping over row objects, so a 500-casualty cohort is checked in one pass and
# every offending row is reported at once instead of only the first.

# Column contract. Order matters: it is the column order sdt_cohort() enforces.
.cohort_columns <- function() {
  c("id", "age", "sex", "life_points", "ventilated",
    "injury_region", "triage", "therapy", "arrival_order")
}

# Low-level constructor (trusted input, no validation).
new_cohort <- function(x = tibble::tibble()) {
  tibble::new_tibble(x, nrow = nrow(x), class = "cohort")
}

#' Build a validated casualty cohort
#'
#' Coerces a data frame of casualties into a `cohort`, a [tibble::tibble()]
#' subclass carrying the column contract shared by [sdt_patient_card()] and
#' [sdt_scenario()]. Validation is vectorised: every column is checked in one
#' pass and *all* offending rows are reported, rather than aborting on the
#' first bad one.
#'
#' @param data A data frame with the columns produced by
#'   [sdt_patient_card()]: `id`, `age`, `sex`, `life_points`, `ventilated`,
#'   `injury_region`, `triage`, `therapy`, `arrival_order`. Extra columns are
#'   kept and carried through untouched.
#' @param ... These dots are for future extensions and must be empty.
#'
#' @return A `cohort` object: a tibble subclass with the contract columns
#'   first, ordered by `arrival_order`. A zero-row input yields a zero-row
#'   cohort with the correct column types.
#' @seealso [validate_cohort()] to check an existing cohort,
#'   [sdt_as_patient_cards()] to explode one into cards.
#' @export
#' @examples
#' co <- sdt_cohort(sdt_synthetic_cohort(n = 5, seed = 1))
#' co
#'
#' # Every bad row is named at once, not just the first.
#' bad <- sdt_synthetic_cohort(n = 4, seed = 1)
#' bad$triage[c(2, 4)] <- "purple"
#' try(sdt_cohort(bad))
sdt_cohort <- function(data, ...) {
  rlang::check_dots_empty()
  if (inherits(data, "scenario")) {
    cli::cli_abort(c(
      "{.arg data} must be a data frame, not a {.cls scenario}.",
      "i" = "Did you mean {.code sdt_cohort(x$cohort)}?"
    ))
  }
  data <- tibble::as_tibble(data)

  missing <- setdiff(.cohort_columns(), names(data))
  if (length(missing)) {
    n_missing <- length(missing)
    cli::cli_abort(c(
      "{.arg data} is missing {n_missing} required column{?s}.",
      "x" = "Missing: {.field {missing}}."
    ))
  }

  data <- .cohort_coerce(data)
  # Contract columns first, anything the caller brought along after.
  data <- data[, c(.cohort_columns(), setdiff(names(data), .cohort_columns()))]
  out <- new_cohort(data)
  validate_cohort(out)
  if (nrow(out)) out <- out[order(out$arrival_order), ]
  new_cohort(out)
}

# Type coercion is separate from validation so the error messages can talk
# about values ("purple is not a triage level") rather than storage modes.
.cohort_coerce <- function(data) {
  as_int <- function(x) {
    if (is.numeric(x)) suppressWarnings(as.integer(x)) else x
  }
  data$id <- if (is.character(data$id)) data$id else as.character(data$id)
  data$age <- as_int(data$age)
  data$sex <- as.character(data$sex)
  data$life_points <- as_int(data$life_points)
  data$injury_region <- as.character(data$injury_region)
  data$triage <- as.character(data$triage)
  data$therapy <- as.character(data$therapy)
  data$arrival_order <- as_int(data$arrival_order)
  data
}

#' Validate a casualty cohort
#'
#' Checks a `cohort` against the domain invariants: controlled vocabularies for
#' `sex`, `injury_region`, `triage` and `therapy`; `life_points` inside
#' `1:6`; unique non-missing `id`; and a positive whole `arrival_order`. All
#' violations across all columns are collected and reported together.
#'
#' @param x A `cohort` from [sdt_cohort()].
#' @param call The calling environment, used to attribute the error.
#'
#' @return `x`, invisibly, on success; otherwise an error of class
#'   `sandtablR_error_cohort`.
#' @export
#' @examples
#' co <- sdt_cohort(sdt_synthetic_cohort(n = 5, seed = 1))
#' validate_cohort(co)
validate_cohort <- function(x, call = rlang::caller_env()) {
  .check_class(x, "cohort", call = call)

  lo <- sdt_life_range()[[1L]]
  hi <- sdt_life_range()[[2L]]
  lp <- x$life_points
  in_set <- function(field) {
    allowed <- switch(field,
      sex = sdt_sex_levels(),
      injury_region = sdt_injury_regions(),
      triage = sdt_triage_levels(),
      therapy = sdt_therapy_levels()
    )
    value <- x[[field]]
    list(
      field = field,
      detail = cli::format_inline("must be one of {.val {allowed}}"),
      rows = which(is.na(value) | !value %in% allowed)
    )
  }

  checks <- c(
    lapply(c("sex", "injury_region", "triage", "therapy"), in_set),
    list(
      list(field = "life_points",
           detail = cli::format_inline("must lie in {lo}:{hi}"),
           rows = which(is.na(lp) | lp < lo | lp > hi)),
      list(field = "age", detail = "must be a non-missing age in 0:130",
           rows = which(is.na(x$age) | x$age < 0L | x$age > 130L)),
      list(field = "ventilated",
           detail = "must be {.code TRUE} or {.code FALSE}",
           rows = which(!is.logical(x$ventilated) | is.na(x$ventilated))),
      list(field = "arrival_order",
           detail = "must be a positive whole number",
           rows = which(is.na(x$arrival_order) | x$arrival_order < 1L)),
      list(field = "id", detail = "must not be missing",
           rows = which(is.na(x$id) | !nzchar(x$id))),
      list(field = "id", detail = "must be unique",
           rows = which(duplicated(x$id) & !is.na(x$id)))
    )
  )

  problems <- unlist(lapply(checks, .field_problem), use.names = FALSE)
  if (length(problems)) {
    bullets <- stats::setNames(problems, rep("x", length(problems)))
    cli::cli_abort(
      c("{.arg x} is not a valid cohort.", bullets),
      class = "sandtablR_error_cohort",
      call = call
    )
  }
  invisible(x)
}

#' @exportS3Method tibble::tbl_sum
tbl_sum.cohort <- function(x, ...) {
  mix <- table(factor(x$triage, levels = sdt_triage_levels()))
  c(
    "A cohort" = paste(nrow(x), "casualties"),
    "Triage" = paste(names(mix), as.integer(mix), sep = ": ", collapse = "  ")
  )
}

#' Explode a cohort into patient cards
#'
#' Turns each row of a `cohort` back into a `patient_card`. The inverse
#' of stacking cards into a cohort; useful when code wants to work with one
#' casualty at a time.
#'
#' @param x A `cohort` from [sdt_cohort()].
#' @return A list of `patient_card` objects, named by casualty `id`.
#' @export
#' @examples
#' co <- sdt_cohort(sdt_synthetic_cohort(n = 3, seed = 1))
#' cards <- sdt_as_patient_cards(co)
#' cards[[1]]
sdt_as_patient_cards <- function(x) {
  .check_class(x, "cohort")
  out <- lapply(seq_len(nrow(x)), function(i) {
    new_patient_card(
      id = x$id[[i]], age = x$age[[i]], sex = x$sex[[i]],
      life_points = x$life_points[[i]], ventilated = x$ventilated[[i]],
      injury_region = x$injury_region[[i]], triage = x$triage[[i]],
      therapy = x$therapy[[i]], arrival_order = x$arrival_order[[i]]
    )
  })
  stats::setNames(out, x$id)
}
