# patient_card ---------------------------------------------------------------
# One casualty. Fields mirror the TDSC v2.0 patient card: demographics,
# physiological "life points", ventilation status, injury region, the true
# triage category, the true therapy concept, and arrival order.

# Low-level constructor (trusted input, no validation).
new_patient_card <- function(id = character(),
                             age = integer(),
                             sex = character(),
                             life_points = integer(),
                             ventilated = logical(),
                             injury_region = character(),
                             triage = character(),
                             therapy = character(),
                             arrival_order = integer()) {
  structure(
    list(
      id = id, age = age, sex = sex, life_points = life_points,
      ventilated = ventilated, injury_region = injury_region,
      triage = triage, therapy = therapy, arrival_order = arrival_order
    ),
    class = "patient_card"
  )
}

#' Create a patient card
#'
#' Constructs a single validated `patient_card` describing one casualty in a
#' mass-casualty scenario. Fields follow the TDSC tabletop patient-card model.
#'
#' @param id Character scalar identifier for the casualty.
#' @param age Whole number, patient age in years.
#' @param sex One of `"f"`, `"m"`, `"d"`.
#' @param life_points Whole number in `1:6`; lower is more critical.
#' @param ventilated Logical; whether the casualty requires ventilation.
#' @param injury_region Leading injury region; one of
#'   [sdt_injury_regions()].
#' @param triage True sighting category; one of [sdt_triage_levels()].
#' @param therapy True therapy concept; one of [sdt_therapy_levels()].
#' @param arrival_order Whole number giving position in the arrival sequence.
#'
#' @return An S3 object of class `patient_card`.
#' @export
#' @examples
#' sdt_patient_card(
#'   id = "P01", age = 34, sex = "m", life_points = 4L,
#'   ventilated = TRUE, injury_region = "thorax",
#'   triage = "red", therapy = "DCS", arrival_order = 1L
#' )
sdt_patient_card <- function(id, age, sex, life_points, ventilated,
                             injury_region, triage, therapy,
                             arrival_order) {
  card <- new_patient_card(
    id = as.character(id),
    age = as.integer(age),
    sex = as.character(sex),
    life_points = as.integer(life_points),
    ventilated = as.logical(ventilated),
    injury_region = as.character(injury_region),
    triage = as.character(triage),
    therapy = as.character(therapy),
    arrival_order = as.integer(arrival_order)
  )
  validate_patient_card(card)
}

#' Validate a patient card
#'
#' Checks that a `patient_card` satisfies the domain invariants: the controlled
#' vocabularies for `sex`, `injury_region`, `triage` and `therapy`, and
#' `life_points` inside `1:6`.
#'
#' @param x A `patient_card` object.
#' @param call The calling environment, used to attribute the error.
#' @return The validated `x`, invisibly on success; otherwise an error of class
#'   `sandtablR_error_card`.
#' @seealso [validate_cohort()], which applies the same rules to a whole cohort
#'   in one vectorised pass.
#' @export
#' @examples
#' card <- sdt_patient_card(
#'   "P01", 34, "m", 4L, TRUE, "thorax", "red", "DCS", 1L
#' )
#' validate_patient_card(card)
validate_patient_card <- function(x, call = rlang::caller_env()) {
  .check_class(x, "patient_card", call = call)

  levels_of <- list(
    sex = sdt_sex_levels(),
    injury_region = sdt_injury_regions(),
    triage = sdt_triage_levels(),
    therapy = sdt_therapy_levels()
  )
  for (field in names(levels_of)) {
    value <- x[[field]]
    allowed <- levels_of[[field]]
    if (length(value) && (is.na(value) || !value %in% allowed)) {
      cli::cli_abort(
        c(
          "{.field {field}} must be one of {.val {allowed}}.",
          "x" = "Got {.val {value}}."
        ),
        class = "sandtablR_error_card",
        call = call
      )
    }
  }

  lp <- x$life_points
  lo <- sdt_life_range()[[1L]]
  hi <- sdt_life_range()[[2L]]
  if (length(lp) && (is.na(lp) || lp < lo || lp > hi)) {
    cli::cli_abort(
      c(
        "{.field life_points} must lie in {lo}:{hi}.",
        "x" = "Got {.val {lp}}."
      ),
      class = "sandtablR_error_card",
      call = call
    )
  }
  invisible(x)
}

#' Format a patient card
#'
#' Renders a `patient_card` as a single-line character summary. [print()] shows
#' exactly this string, so the two cannot drift apart.
#'
#' @param x A `patient_card` object.
#' @param ... These dots are for future extensions and must be empty.
#' @return A length-1 character vector.
#' @export
#' @examples
#' card <- sdt_patient_card(
#'   "P01", 34, "m", 4L, TRUE, "thorax", "red", "DCS", 1L
#' )
#' format(card)
format.patient_card <- function(x, ...) {
  rlang::check_dots_empty()
  vent <- if (isTRUE(x$ventilated)) ", ventilated" else ""
  sprintf(
    "<patient_card> %s: %sy %s, %s, triage %s, therapy %s, life %s%s",
    x$id, x$age, x$sex, x$injury_region, x$triage, x$therapy,
    x$life_points, vent
  )
}

#' @export
print.patient_card <- function(x, ...) {
  rlang::check_dots_empty()
  cat(format(x), sep = "\n")
  invisible(x)
}
