# Domain vocabulary ----------------------------------------------------------
# Values follow the TDSC tabletop model (Berlin triage algorithm; ETC/DCS/TASC
# therapy concepts; the 9-area published board, plus the three states real
# exercises reach that it does not report). See the vignette for provenance.

#' Controlled vocabularies for the mass-casualty domain
#'
#' Character vectors giving the allowed levels for the core categorical fields
#' used throughout the package. Exposed so downstream code and the Shiny app can
#' validate against a single source of truth.
#'
#' @format Each is a character vector:
#' \describe{
#'   \item{`sdt_triage_levels()`}{Sighting categories `"red"`, `"yellow"`,
#'     `"green"` (SK I / II / III).}
#'   \item{`sdt_therapy_levels()`}{Therapy concepts `"ETC"`, `"DCS"`, `"TASC"`,
#'     and `"none"` for the card's "no operation" option.}
#'   \item{`sdt_diagnostics()`}{Diagnostic modalities printed on the card:
#'     `"efast"`, `"xray"`, `"ct"`.}
#'   \item{`sdt_board_areas()`}{The twelve board states a casualty can occupy:
#'     the nine of the published board, `"morgue"` and `"external"` for the two
#'     terminal states real exercises produce but the published figure does not
#'     report (died on the board, and transferred out of the hospital), and
#'     `"not_seen"` for a casualty the game ended before. The engine has no
#'     branch to `"morgue"` or `"external"`; they exist so that observed data
#'     can be represented without being folded into a state it is not.}
#'   \item{`sdt_life_range()`}{The inclusive range of life points, `0:6`. Zero
#'     is the death marker printed on the version 2.0 card: a casualty who ran
#'     out of life points during play. Generators draw from `1:6` only, since
#'     death is a state reached in play rather than a card attribute.}
#'   \item{`sdt_injury_regions()`}{Leading injury region recorded on the card.}
#'   \item{`sdt_sex_levels()`}{Recorded sex: `"f"`, `"m"`, `"d"` (diverse).}
#' }
#' @return A character vector of allowed levels.
#' @seealso [sdt_reference()] for the published marginals over these levels.
#' @examples
#' sdt_triage_levels()
#' sdt_therapy_levels()
#' sdt_diagnostics()
#' sdt_board_areas()
#' sdt_life_range()
#' sdt_injury_regions()
#' sdt_sex_levels()
#' @name sdt_vocabularies
NULL

#' @rdname sdt_vocabularies
#' @export
sdt_triage_levels <- function() c("red", "yellow", "green")

# "none" is the patient card's "keine OP": the casualty was worked up and no
# operation was indicated. The published therapy mix is over procedures
# performed, so it does not carry this level, which is why it sits last.
#' @rdname sdt_vocabularies
#' @export
sdt_therapy_levels <- function() c("ETC", "DCS", "TASC", "none")

#' @rdname sdt_vocabularies
#' @export
sdt_diagnostics <- function() c("efast", "xray", "ct")

#' @rdname sdt_vocabularies
#' @export
sdt_board_areas <- function() {
  c(
    "triage", "ed_red", "ed_yellow", "diagnostics",
    "or", "recovery", "icu_imc", "ward", "green_area",
    "morgue", "external", "not_seen"
  )
}

#' @rdname sdt_vocabularies
#' @export
sdt_life_range <- function() c(0L, 6L)

#' @rdname sdt_vocabularies
#' @export
sdt_injury_regions <- function() {
  c("head", "thorax", "abdomen", "extremity", "multiple")
}

#' @rdname sdt_vocabularies
#' @export
sdt_sex_levels <- function() c("f", "m", "d")

# The scale a *generator* may draw from: 1 (most critical) to 6 (least). This
# is deliberately narrower than sdt_life_range(): 0 means the casualty died on
# the board, which is a state reached during play, never a card attribute, and
# the engine has no decay model that could produce one. Keeping the two ranges
# apart is what stops an extreme draw silently manufacturing a dead casualty.
.sdt_living_life_range <- function() c(1L, 6L)

# Human-readable board labels, taken from the published board. Used by the
# workbench and the plots so an area is never shown to a reader as a column
# name.
.sdt_area_labels <- function() {
  c(
    triage = "Screening", ed_red = "ED red", ed_yellow = "ED yellow",
    diagnostics = "Radiology", or = "Operating room",
    recovery = "Recovery", icu_imc = "ICU / IMC", ward = "Ward",
    green_area = "Green area", morgue = "Mortuary",
    external = "Transferred out", not_seen = "Not yet seen",
    # The folded reporting location, used only by summary.game_run().
    ed = "Emergency department"
  )
}

# Label a vector of board areas for display, leaving anything unrecognised as
# it came in rather than turning it into NA.
.sdt_label_area <- function(x) {
  labels <- .sdt_area_labels()
  out <- unname(labels[x])
  ifelse(is.na(out), x, out)
}

#' Display labels for board areas
#'
#' Turns board-area names into the labels used on figures and in the
#' workbench, so a legend never shows a column name such as `"icu_imc"`.
#' Anything unrecognised is passed through unchanged rather than becoming
#' `NA`, which keeps the function total over user-supplied vocabularies.
#'
#' @param areas Character vector of board areas. Defaults to
#'   [sdt_board_areas()].
#' @return A character vector of labels, the same length as `areas`.
#' @seealso [sdt_board_areas()], [sdt_report_area()].
#' @export
#' @examples
#' sdt_area_labels()
#' sdt_area_labels(c("or", "morgue", "nowhere"))
sdt_area_labels <- function(areas = sdt_board_areas()) {
  .sdt_label_area(as.character(areas))
}
