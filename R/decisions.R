# Participant decisions -------------------------------------------------------
# The card carries the *true* sighting category. What a participant actually
# calls is a different thing, and the published evaluation reports how often
# the two agree: 88% for red, 59% for yellow, 71% for green.
#
# That gap is the interesting quantity in this domain, and it is also what
# makes the model bench meaningful. Predicting the true category from the card
# is trivial -- life points determine it by construction, and every engine
# scores a perfect 1.0, which tells you nothing about glass-box versus
# black-box. Predicting what a participant will call is a real problem: partly
# learnable, partly noise, exactly the situation where model choice matters.

#' Add participant decisions to a cohort
#'
#' Simulates the sighting category and therapy concept a participant assigns
#' to each casualty, using the published per-category hit rates. Where the
#' participant is wrong, the miscall is drawn from the neighbouring categories,
#' since confusing red with yellow is far more likely than confusing red with
#' green.
#'
#' @param cohort A `cohort` from [sdt_cohort()] or [sdt_synthetic_cohort()].
#' @param hit Named numeric vector of per-category hit rates. Defaults to the
#'   published `triage_hit`.
#' @param therapy_hit Scalar probability that the therapy concept is called
#'   correctly.
#' @param seed Optional integer seed, applied with [withr::local_seed()].
#'
#' @return The cohort with two added columns: `called_triage` and
#'   `called_therapy`, the participant's calls.
#' @seealso [sdt_consistency_score()] to score the calls against the truth.
#' @export
#' @examples
#' co <- sdt_synthetic_cohort(n = 200, seed = 1)
#' called <- sdt_add_decisions(co, seed = 2)
#' sdt_consistency_score(called$triage, called$called_triage)
#'
#' # Hit rate per true category, against the published targets.
#' table(called$triage, called$called_triage)
sdt_add_decisions <- function(cohort, hit = NULL, therapy_hit = 0.75,
                              seed = NULL) {
  cohort <- sdt_cohort(tibble::as_tibble(cohort))
  hit <- hit %||% .ref_published("triage_hit")
  if (!is.numeric(hit) || is.null(names(hit)) || anyNA(hit) ||
        any(hit < 0) || any(hit > 1)) {
    cli::cli_abort(
      "{.arg hit} must be a named numeric vector of probabilities."
    )
  }
  unknown <- setdiff(names(hit), sdt_triage_levels())
  if (length(unknown)) {
    cli::cli_abort(c(
      "{.arg hit} must be named after sighting categories.",
      "x" = "Unknown: {.val {unknown}}."
    ))
  }
  if (!is.numeric(therapy_hit) || length(therapy_hit) != 1L ||
        is.na(therapy_hit) || therapy_hit < 0 || therapy_hit > 1) {
    cli::cli_abort("{.arg therapy_hit} must be a single probability.")
  }
  if (!is.null(seed)) {
    withr::local_seed(.check_count(seed))
  }
  if (!nrow(cohort)) {
    cohort$called_triage <- character()
    cohort$called_therapy <- character()
    return(sdt_cohort(cohort))
  }

  cohort$called_triage <- .miscall(
    cohort$triage, sdt_triage_levels(), hit, adjacent = TRUE
  )
  cohort$called_therapy <- .miscall(
    cohort$therapy, sdt_therapy_levels(),
    stats::setNames(rep(therapy_hit, length(sdt_therapy_levels())),
                    sdt_therapy_levels()),
    adjacent = FALSE
  )
  sdt_cohort(cohort)
}

# Draw a called label per casualty. `adjacent = TRUE` weights a miscall toward
# neighbouring levels on an ordered scale, which is how triage errors actually
# behave: red is mistaken for yellow, rarely for green.
.miscall <- function(truth, levels, hit, adjacent) {
  n <- length(truth)
  correct <- stats::runif(n) < unname(hit[truth])
  out <- truth
  wrong <- which(!correct & !is.na(correct))
  if (!length(wrong)) {
    return(out)
  }
  index <- match(truth[wrong], levels)
  out[wrong] <- vapply(index, function(i) {
    others <- setdiff(seq_along(levels), i)
    weight <- if (adjacent) {
      1 / abs(others - i)
    } else {
      rep(1, length(others))
    }
    levels[sample(others, 1L, prob = weight)]
  }, character(1))
  out
}
