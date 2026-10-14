# Course and group metadata ---------------------------------------------------
# Who was in the room. The TDSC participant register records one row per
# person per course, and this module reads it, normalises the free text into
# controlled vocabularies, states its missingness instead of hiding it, and
# aggregates it to the level the register can actually support.
#
# THE CONSTRAINT THAT SHAPES EVERYTHING BELOW. The register links a person to
# a COURSE. It carries no group, team or "Gruppe" column, so a participant
# cannot be placed in group A rather than group B. Every group inside one
# course therefore shares one and the same composition row. The behaviour
# model in `behaviour-model.R` fits
#
#     y ~ card + (1 | block) + (1 | group)
#
# with `block` the session and `group` the team. Composition is a block-level
# covariate. It can compete with, and possibly explain, the SESSION random
# effect. It can never explain the TEAM random effect -- within a course it is
# constant, so it has no within-course variance to spend. Any analysis that
# regresses a team-level outcome on composition is really a between-course
# regression with 19 points, and must be read as one.
#
# The second constraint is missingness. Four of the nineteen courses have no
# participants at all, and several covariates are missing for a sixth or more
# of the people who are recorded. Missingness is the common case here, so the
# module implements both halves of a missingness policy:
#
#   * imputation for modelling -- a stated fill rule plus an indicator column,
#     so an imputed value is never indistinguishable from an observed one; and
#   * honest descriptives -- every summary carries its denominator and its
#     missing count, and never silently drops or fills.

# The raw covariate columns, in workbook order. One source of truth: the
# indicator column sets and the missingness report are all generated from it.
.participant_fields <- function() {
  c("anrede", "titel", "fachbereich", "spezialisiert_seit",
    "verbandsmitglied", "funktion", "weiterbildungsjahre")
}

# The columns the workbook must supply, keyed by the lower-case name this
# module uses internally.
.participant_columns <- function() {
  c(
    record_id = "Participant_Record_ID",
    short_course_id = "Short_Course_ID",
    file = "JSON_File_Name",
    course_name = "Participant_Course_Name",
    course_date = "Participant_Course_Date",
    anrede = "Anrede",
    titel = "Titel",
    fachbereich = "Fachbereich",
    spezialisiert_seit = "Spezialisiert_seit",
    verbandsmitglied = "Verbandsmitglied",
    funktion = "Funktion",
    weiterbildungsjahre = "Weiterbildungsjahre",
    source_file = "Source_File"
  )
}

.course_index_columns <- function() {
  c(
    short_course_id = "Short_Course_ID",
    file = "JSON_File_Name",
    course_name = "Participant_Course_Name",
    course_date = "Participant_Course_Date",
    relation_status = "Relation_Status",
    relation_flag = "Relation_Flag",
    match_method = "Match_Method",
    session_start = "Session_Start_UTC",
    session_end = "Session_End_UTC",
    course_sheet = "Course_Sheet"
  )
}

# Fold a free-text answer onto a lookup key: case, punctuation and spacing all
# stop mattering, so "Dr. med.", "Dr.med." and "Dr. Med." are one entry in the
# dictionary rather than three.
.norm_key <- function(x) {
  x <- gsub("[^\\p{L}\\p{N}]+", " ", as.character(x), perl = TRUE)
  trimws(tolower(x))
}

# Blank-ish answers are missing, whatever shape the blank arrived in.
.blank_to_na <- function(x) {
  x <- as.character(x)
  x[!is.na(x) & !nzchar(trimws(x))] <- NA_character_
  x
}

# The dictionary's checks are keyed by offending VALUE, not by row, so the
# shared .field_problem() wording ("in row urology") would misdescribe them.
# Same contract: every offender at once, never just the first.
.value_problem <- function(check) {
  values <- check$rows
  if (!length(values)) {
    return(NULL)
  }
  field <- check$field
  detail <- check$detail
  shown <- utils::head(values, 5L)
  hidden <- length(values) - length(shown)
  more <- if (hidden > 0L) cli::format_inline(" (+{hidden} more)") else ""
  cli::format_inline("{.field {field}}: {detail} -- {.val {shown}}{more}.")
}

# The shipped normalisation maps ----------------------------------------------
# Held as data, not as code, so a spelling that turns up in the next export is
# one argument to sdt_participant_dictionary() rather than a package patch.

.dict_funktion <- function() {
  list(
    map = list(
      "assistenzarzt" = "resident",
      "facharzt" = "specialist",
      "oberarzt" = "consultant",
      "chefarzt" = "chief",
      "andere" = "other",
      "nichts angegeben" = "not_stated"
    ),
    # The ordered seniority ladder. `other` and `not_stated` are deliberately
    # off it: they are answers, but not positions on it.
    levels = c("resident", "specialist", "consultant", "chief",
               "other", "not_stated"),
    ladder = c("resident", "specialist", "consultant", "chief"),
    score = c(resident = 1, specialist = 2, consultant = 3, chief = 4),
    # Oberarzt and Chefarzt: the two grades that carry decision authority.
    senior = c("consultant", "chief"),
    # A declared non-answer is missing information, not a sixth grade.
    not_stated = "not_stated"
  )
}

.dict_titel <- function() {
  list(
    # Exact hits on the normalised key. Every spelling observed in the TDSC
    # register is listed, so the mapping is auditable by reading it.
    map = list(
      "dr" = "doctorate",
      "dr med" = "doctorate",
      "dr med univ" = "doctorate",
      "dr dr med univ" = "doctorate",
      "dr med mhba" = "doctorate",
      "dr rer pol" = "doctorate",
      "mudr" = "doctorate",
      "pd dr" = "habilitation",
      "pd dr dr" = "habilitation",
      "pd dr med" = "habilitation",
      "pd dr med habil" = "habilitation",
      "priv doz" = "habilitation",
      "priv doz dr" = "habilitation",
      "priv doz dr med" = "habilitation",
      "prof dr" = "professor",
      "prof dr med" = "professor",
      "prof dr med dr" = "professor",
      "univ prof dr med" = "professor",
      # Written into the title field but not academic titles: a job grade, a
      # professional designation or non-doctoral degree: no academic title.
      "assistenzarzt" = "none",
      "chefarzt" = "none",
      "ofa" = "none",
      "mba" = "none"
    ),
    # Safety net for spellings nobody has seen yet. Every pattern is tested
    # and the HIGHEST matching rung wins, so the net is order-independent:
    # "Prof. Dr. med." matches both the professor and the doctorate pattern
    # and resolves to professor either way.
    patterns = list(
      professor = "prof",
      habilitation = "(^|[^a-z])pd([^a-z]|$)|priv doz|habil",
      doctorate = "(^|[^a-z])dr([^a-z]|$)|mudr|phd"
    ),
    levels = c("none", "doctorate", "habilitation", "professor"),
    score = c(none = 0, doctorate = 1, habilitation = 2, professor = 3),
    # An empty title box on a registration form is a claim of no title, not a
    # refusal to answer -- so it is filled, and every filled row is flagged.
    blank_is_none = TRUE
  )
}

.dict_fachbereich <- function() {
  list(
    map = list(
      "unfallchirurgie" = "trauma_surgery",
      "spezielle unfallchirurgie" = "trauma_surgery",
      "spezieller unfallchirurgie" = "trauma_surgery",
      "an\u00e4sthesie intensivmedizin" = "anaesthesiology_icu",
      "notfallmedizin" = "emergency_medicine",
      "allgemein gef\u00e4\u00dfchirurgie" = "general_surgery",
      "chirurgie" = "general_surgery",
      "orthop\u00e4die" = "orthopaedics",
      "orthop\u00e4die unfallchirurgie" = "orthopaedics_trauma",
      "innere medizin" = "internal_medicine",
      "plastische chirurgie" = "plastic_surgery",
      "gyn\u00e4kologie" = "gynaecology",
      "neurologie" = "neurology",
      "p\u00e4diatrie" = "paediatrics"
    ),
    # Free-text answers that name several specialties at once. The documented
    # collapse rule is "the primary specialty is the one mentioned first", so
    # the pattern whose match starts earliest in the string wins. Ties go to
    # the order below.
    patterns = list(
      general_surgery = "allgemein|(^| )chirurg",
      trauma_surgery = "unfallchirurg",
      orthopaedics = "orthop",
      anaesthesiology_icu = "an\u00e4sthes|intensivmedizin",
      emergency_medicine = "notfallmedizin|akut",
      internal_medicine = "innere medizin",
      plastic_surgery = "plastische",
      gynaecology = "gyn\u00e4kolog",
      neurology = "neurolog",
      paediatrics = "p\u00e4diatr"
    ),
    # The surgical / non-surgical split. Gynaecology sits on the surgical side
    # because Frauenheilkunde und Geburtshilfe is an operative discipline;
    # that is a judgement, and it is a one-argument judgement to reverse.
    group = list(
      trauma_surgery = "surgical",
      general_surgery = "surgical",
      orthopaedics = "surgical",
      orthopaedics_trauma = "surgical",
      plastic_surgery = "surgical",
      gynaecology = "surgical",
      anaesthesiology_icu = "non_surgical",
      emergency_medicine = "non_surgical",
      internal_medicine = "non_surgical",
      neurology = "non_surgical",
      paediatrics = "non_surgical"
    )
  )
}

.dict_anrede <- function() {
  list(
    map = list(herr = "male", frau = "female"),
    levels = c("female", "male")
  )
}

#' Normalisation dictionary for participant metadata
#'
#' The TDSC participant register stores four covariates as German free text.
#' This function holds the maps that fold that text onto controlled
#' vocabularies **as data**, so extending the package for a spelling it has
#' not seen is one argument rather than a code change: whatever you pass is
#' [utils::modifyList()]-merged, recursively, onto the shipped default.
#'
#' The four vocabularies are:
#'
#' * `funktion` — an ordered seniority ladder,
#'   `resident < specialist < consultant < chief`
#'   (Assistenzarzt, Facharzt, Oberarzt, Chefarzt), scored 1–4. `other`
#'   ("Andere") is a real answer that sits off the ladder and scores `NA`;
#'   `not_stated` ("Nichts angegeben") is a declared non-answer and is
#'   treated as **missing**, not as a sixth grade.
#' * `titel` — the academic ladder `none < doctorate < habilitation <
#'   professor`, scored 0–3. Matching is exact on a punctuation- and
#'   case-insensitive key first, then falls back to `patterns`, where every
#'   rung is tested and the highest match wins. An empty title box is filled
#'   with `none` when `blank_is_none` is `TRUE` — and every filled row is
#'   flagged, see [sdt_read_participants()].
#' * `fachbereich` — a controlled specialty list plus a surgical /
#'   non-surgical `group`. Multi-specialty free text ("Chirurgie,
#'   Unfallchirurgie") is collapsed by the stated rule *the primary specialty
#'   is the one mentioned first*: the pattern whose match starts earliest in
#'   the string wins.
#' * `anrede` — salutation to sex. This is a **proxy**: it records how a
#'   person was addressed on a registration form, not a gender identity.
#'
#' @param funktion,titel,fachbereich,anrede Optional named lists merged onto
#'   the corresponding shipped slot. Each slot has a `map` (normalised key to
#'   level), and, depending on the covariate, `patterns`, `levels`, `score`
#'   and `group`. Adding one spelling means passing
#'   `funktion = list(map = list("leitender oberarzt" = "consultant"))`.
#'
#' @return An S3 object of class `sdt_participant_dictionary`: a list of four
#'   slots, with `print()` and `format()` methods.
#' @seealso [sdt_read_participants()], which applies it.
#' @export
#' @examples
#' d <- sdt_participant_dictionary()
#' d
#' d$funktion$score
#'
#' # One new spelling, no package patch.
#' d2 <- sdt_participant_dictionary(
#'   funktion = list(map = list("leitender oberarzt" = "consultant"))
#' )
#' d2$funktion$map[["leitender oberarzt"]]
sdt_participant_dictionary <- function(funktion = NULL,
                                       titel = NULL,
                                       fachbereich = NULL,
                                       anrede = NULL) {
  slots <- list(
    funktion = .dict_funktion(),
    titel = .dict_titel(),
    fachbereich = .dict_fachbereich(),
    anrede = .dict_anrede()
  )
  extra <- list(funktion = funktion, titel = titel,
                fachbereich = fachbereich, anrede = anrede)
  for (nm in names(extra)) {
    add <- extra[[nm]]
    if (is.null(add)) next
    if (!is.list(add) || (length(add) && is.null(names(add)))) {
      cli::cli_abort(
        c("{.arg {nm}} must be a named list.",
          "i" = "It is merged onto the shipped slot with
                 {.fn utils::modifyList}."),
        class = "sandtablR_error_dictionary"
      )
    }
    slots[[nm]] <- utils::modifyList(slots[[nm]], add)
  }
  .validate_dictionary(slots)
  structure(slots, class = "sdt_participant_dictionary")
}

# Report every inconsistency at once rather than aborting on the first: a
# hand-written extension usually goes wrong in more than one place.
.validate_dictionary <- function(slots, call = rlang::caller_env()) {
  checks <- list()
  fun <- slots$funktion
  bad <- setdiff(unlist(fun$map, use.names = FALSE), fun$levels)
  checks[[1]] <- list(field = "funktion$map", rows = bad,
                      detail = "value is not a funktion level")
  bad <- setdiff(fun$ladder, names(fun$score))
  checks[[2]] <- list(field = "funktion$score", rows = bad,
                      detail = "ladder rung has no score")
  tit <- slots$titel
  bad <- setdiff(unlist(tit$map, use.names = FALSE), tit$levels)
  checks[[3]] <- list(field = "titel$map", rows = bad,
                      detail = "value is not a titel level")
  bad <- setdiff(names(tit$patterns), tit$levels)
  checks[[4]] <- list(field = "titel$patterns", rows = bad,
                      detail = "pattern names a level that does not exist")
  fach <- slots$fachbereich
  known <- names(fach$group)
  bad <- setdiff(unlist(fach$map, use.names = FALSE), known)
  checks[[5]] <- list(field = "fachbereich$map", rows = bad,
                      detail = "specialty has no surgical grouping")
  bad <- setdiff(names(fach$patterns), known)
  checks[[6]] <- list(field = "fachbereich$patterns", rows = bad,
                      detail = "specialty has no surgical grouping")
  bad <- setdiff(unlist(fach$group, use.names = FALSE),
                 c("surgical", "non_surgical"))
  checks[[7]] <- list(field = "fachbereich$group", rows = bad,
                      detail = "grouping is not surgical/non_surgical")
  bad <- setdiff(unlist(slots$anrede$map, use.names = FALSE),
                 slots$anrede$levels)
  checks[[8]] <- list(field = "anrede$map", rows = bad,
                      detail = "value is not an anrede level")

  problems <- unlist(lapply(checks, .value_problem), use.names = FALSE)
  if (length(problems)) {
    n_bad <- length(problems)
    cli::cli_abort(
      c("The participant dictionary is inconsistent.",
        stats::setNames(problems, rep("x", n_bad)),
        "i" = "{n_bad} check{?s} disagree{?s/} with the slot's own
               vocabulary."),
      class = "sandtablR_error_dictionary",
      call = call
    )
  }
  invisible(slots)
}

#' @export
format.sdt_participant_dictionary <- function(x, ...) {
  rlang::check_dots_empty()
  n_spec <- length(unique(unlist(x$fachbereich$map, use.names = FALSE)))
  surg <- sum(unlist(x$fachbereich$group, use.names = FALSE) == "surgical")
  c(
    "<sdt_participant_dictionary>",
    sprintf("funktion    %d spelling%s -> %s",
            length(x$funktion$map),
            if (length(x$funktion$map) == 1L) "" else "s",
            paste(x$funktion$ladder, collapse = " < ")),
    sprintf("titel       %d spelling%s + %d pattern%s -> %s",
            length(x$titel$map),
            if (length(x$titel$map) == 1L) "" else "s",
            length(x$titel$patterns),
            if (length(x$titel$patterns) == 1L) "" else "s",
            paste(x$titel$levels, collapse = " < ")),
    sprintf("fachbereich %d spelling%s -> %d specialt%s (%d surgical)",
            length(x$fachbereich$map),
            if (length(x$fachbereich$map) == 1L) "" else "s",
            n_spec, if (n_spec == 1L) "y" else "ies", surg),
    sprintf("anrede      %s (a salutation proxy, not gender identity)",
            paste(x$anrede$levels, collapse = " / "))
  )
}

#' @export
print.sdt_participant_dictionary <- function(x, ...) {
  rlang::check_dots_empty()
  cat(format(x), sep = "\n")
  invisible(x)
}

# Applying the dictionary -----------------------------------------------------

# Exact map first, then the ladder-aware pattern net. `fallback` is the level
# assigned to a non-empty answer that matches nothing: for the academic
# ladder that is "none" (the person wrote something, and it is not a title),
# for a specialty it is NA (we do not know what they meant).
.map_ladder <- function(x, slot, fallback = NA_character_) {
  key <- .norm_key(x)
  out <- rep(NA_character_, length(key))
  hit <- unlist(slot$map, use.names = TRUE)[key]
  out[!is.na(hit)] <- unname(hit[!is.na(hit)])
  todo <- which(!is.na(key) & nzchar(key) & is.na(out))
  if (length(todo) && length(slot$patterns)) {
    ranks <- match(names(slot$patterns), slot$levels)
    for (i in todo) {
      matched <- vapply(
        slot$patterns,
        function(p) grepl(p, key[[i]], perl = TRUE),
        logical(1)
      )
      if (any(matched)) {
        out[[i]] <- slot$levels[[max(ranks[matched])]]
      }
    }
  }
  out[!is.na(key) & nzchar(key) & is.na(out)] <- fallback
  out[is.na(key) | !nzchar(key)] <- NA_character_
  out
}

# Exact map first, then "the primary specialty is the one mentioned first":
# the pattern whose match starts earliest in the string wins, ties resolved by
# the order the patterns are written in.
.map_first_mentioned <- function(x, slot) {
  key <- .norm_key(x)
  out <- rep(NA_character_, length(key))
  hit <- unlist(slot$map, use.names = TRUE)[key]
  out[!is.na(hit)] <- unname(hit[!is.na(hit)])
  todo <- which(!is.na(key) & nzchar(key) & is.na(out))
  for (i in todo) {
    pos <- vapply(
      slot$patterns,
      function(p) as.integer(regexpr(p, key[[i]], perl = TRUE)),
      integer(1)
    )
    pos[pos < 0L] <- NA_integer_
    if (all(is.na(pos))) next
    out[[i]] <- names(slot$patterns)[[which.min(pos)]]
  }
  out
}

# German day-first dates. Anything that is not exactly dd.mm.yyyy stays NA
# rather than being coerced into a date that was never written down.
.parse_de_date <- function(x) {
  x <- trimws(as.character(x))
  ok <- !is.na(x) & grepl("^\\d{2}\\.\\d{2}\\.\\d{4}$", x)
  out <- rep(as.Date(NA), length(x))
  out[ok] <- as.Date(x[ok], format = "%d.%m.%Y")
  out
}

# A free-text number field. "," and ",," are not zero, they are noise.
.parse_years <- function(x) {
  x <- trimws(as.character(x))
  ok <- !is.na(x) & grepl("^\\d+([.,]\\d+)?$", x)
  out <- rep(NA_real_, length(x))
  out[ok] <- as.numeric(sub(",", ".", x[ok], fixed = TRUE))
  out
}

# Reading the register --------------------------------------------------------

new_sdt_participants <- function(x, course_index, dictionary, meta) {
  tibble::new_tibble(
    x, nrow = nrow(x), class = "sdt_participants",
    course_index = course_index, dictionary = dictionary, meta = meta
  )
}

# The workbook's per-course sheets are read as text throughout. Two columns
# arrive as all-NA logicals in some sheets and as character in others, so
# guessing types per sheet produces a bind that fails or silently coerces.
.read_course_sheet <- function(path, sheet) {
  d <- readxl::read_excel(path, sheet = sheet, col_types = "text",
                          .name_repair = "minimal")
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  wanted <- .participant_columns()
  # Accept the space-separated spelling used by the merged register sheet
  # ("Spezialisiert seit") as well as the underscore one.
  names(d) <- gsub(" ", "_", names(d), fixed = TRUE)
  present <- intersect(unname(wanted), names(d))
  out <- d[, present, drop = FALSE]
  names(out) <- names(wanted)[match(present, unname(wanted))]
  for (nm in setdiff(names(wanted), names(out))) out[[nm]] <- NA_character_
  out <- out[, names(wanted), drop = FALSE]
  out$course_sheet <- sheet
  out
}

# The widest answer each field can plausibly hold. Deliberately generous for
# `fachbereich`, which really does contain sentence-length answers listing
# three specialties and the years they were obtained -- an earlier version of
# this check used one width for every field and silently destroyed two of
# them, which is exactly the failure mode the module exists to prevent.
.leak_width <- function() {
  c(anrede = 20L, titel = 40L, fachbereich = 250L,
    spezialisiert_seit = 40L, verbandsmitglied = 60L,
    funktion = 40L, weiterbildungsjahre = 20L)
}

# A value is an error-string leak when the extractor wrote a diagnostic into a
# data cell. Three independent tests, because relying on any one of them would
# miss the next leak: an exact match against the Relation_Flag texts the index
# already carries; a mention of the machinery ("JSON"), which no clinical
# answer contains; or an answer wider than the field can plausibly hold.
.detect_leaks <- function(x, flags, max_width = 60L) {
  if (!length(x)) {
    return(logical(0))
  }
  flags <- unique(flags[!is.na(flags)])
  known <- !is.na(x)
  is_flag <- known & x %in% flags
  is_diag <- known & grepl("\\bJSON\\b", x, ignore.case = TRUE, perl = TRUE)
  is_wide <- known & nchar(x, type = "chars") > max_width
  is_flag | is_diag | is_wide
}

#' Read the TDSC course and group metadata register
#'
#' Reads the participant workbook, binds its per-course sheets, applies
#' [sdt_participant_dictionary()] and returns one row per participant record
#' with the raw and the normalised columns side by side.
#'
#' # What this data can and cannot do
#'
#' The register links a person to a **course**. It carries no group, team or
#' "Gruppe" column, so a participant cannot be placed in group A rather than
#' group B. Every group inside one course therefore shares the same
#' composition row. In the behaviour model
#' `y ~ card + (1 | block) + (1 | group)` these covariates are block-level:
#' they can compete with the **session** random effect, and can never explain
#' the **team** random effect, which has no composition variance to spend.
#' Treat any composition analysis as a between-course regression on as many
#' points as there are courses.
#'
#' # Missingness
#'
#' Courses with no linked participants are kept, not dropped: the
#' `course_index` attribute carries every course in the workbook with its
#' `n_participants` and `has_participants`, so a caller sees the empty courses
#' rather than inferring them from an absence.
#'
#' Rows the extractor wrote a diagnostic into instead of an answer are
#' detected and neutralised to `NA` before anything is normalised, and a row
#' with nothing but such a diagnostic is a course placeholder rather than a
#' person: it is removed from the participant rows and counted in
#' `course_index$n_placeholder_rows`.
#'
#' Two logical column families record what happened to every raw field:
#'
#' * `.missing_<field>` — the information is not available. For `funktion`
#'   this includes the declared non-answer "Nichts angegeben", which is
#'   missing information rather than a grade; for `weiterbildungsjahre` it
#'   includes unparseable free text such as `","`.
#' * `.imputed_<field>` — the normalised value was filled rather than
#'   observed. Only `titel` has a participant-level fill rule by default
#'   (an empty title box becomes `none`, see
#'   [sdt_participant_dictionary()]); the rest are filled, if at all, at
#'   course level by [sdt_course_composition()]. An imputed value is never
#'   indistinguishable from an observed one.
#'
#' @param path Path to the participant workbook (`.xlsx`). Read-only; nothing
#'   is written to it or beside it.
#' @param dictionary A `sdt_participant_dictionary`.
#' @param sheets Optional character vector of per-course sheet names to
#'   restrict to. `NULL`, the default, reads every sheet named in the
#'   workbook's `Course_Index`.
#' @param verbose Report row counts, neutralised leaks and per-covariate
#'   missingness while reading.
#'
#' @return A `sdt_participants` object: a [tibble::tibble()] subclass, one row
#'   per participant record, with attributes `course_index` (one row per
#'   course, including the empty ones), `dictionary` and `meta`.
#' @seealso [sdt_course_composition()] to aggregate to course level,
#'   [sdt_participant_summary()] for descriptives that state their
#'   denominators.
#' @export
#' @examples
#' \dontrun{
#' p <- sdt_read_participants("TDSC_course_participant_relations.xlsx")
#' p
#' attr(p, "course_index")
#' sdt_course_composition(p)
#' }
sdt_read_participants <- function(path,
                                  dictionary = sdt_participant_dictionary(),
                                  sheets = NULL,
                                  verbose = TRUE) {
  .check_string(path)
  .check_flag(verbose)
  .check_class(dictionary, "sdt_participant_dictionary")
  .require_pkgs("readxl")
  if (!file.exists(path)) {
    cli::cli_abort(
      c("Cannot read the participant register.",
        "x" = "No file at {.file {path}}."),
      class = "sandtablR_error_participants_file"
    )
  }

  index <- .read_course_index(path)
  sheets <- .resolve_sheets(path, index, sheets)

  raw <- do.call(rbind, lapply(sheets, .read_course_sheet, path = path))
  raw <- tibble::as_tibble(raw)
  n_source <- nrow(raw)

  # Neutralise the leak before anything reads a value as data.
  fields <- .participant_fields()
  widths <- .leak_width()
  leaked <- stats::setNames(integer(length(fields)), fields)
  for (nm in fields) {
    hit <- .detect_leaks(raw[[nm]], index$relation_flag, widths[[nm]])
    leaked[[nm]] <- sum(hit)
    raw[[nm]][hit] <- NA_character_
    raw[[nm]] <- .blank_to_na(raw[[nm]])
  }

  # A row that carries no covariate at all and names no source row is the
  # extractor's placeholder for a course it could not match, not a person.
  empty <- rowSums(!is.na(as.data.frame(raw[, fields]))) == 0L
  placeholder <- empty & is.na(raw$source_file)
  ph_by_course <- table(factor(raw$file[placeholder], levels = index$file))
  raw <- raw[!placeholder, , drop = FALSE]

  .validate_participants(raw, index)
  out <- .normalise_participants(raw, dictionary, index)

  n_by_course <- table(factor(out$file, levels = index$file))
  index$n_source_rows <- as.integer(
    table(factor(raw$file, levels = index$file))
  ) + as.integer(ph_by_course)
  index$n_placeholder_rows <- as.integer(ph_by_course)
  index$n_participants <- as.integer(n_by_course)
  # A course whose sheet was not read is not an empty course. Without this a
  # `sheets =` subset would come back looking like fifteen cancelled courses.
  index$sheet_read <- index$course_sheet %in% sheets
  index$n_participants[!index$sheet_read] <- NA_integer_
  index$n_source_rows[!index$sheet_read] <- NA_integer_
  index$n_placeholder_rows[!index$sheet_read] <- NA_integer_
  index$has_participants <- index$sheet_read &
    !is.na(index$n_participants) & index$n_participants > 0L

  meta <- list(
    path = path, read_at = Sys.time(), n_source_rows = n_source,
    n_placeholder_rows = sum(placeholder), n_leaks = leaked,
    leaks_by_field = leaked[leaked > 0L],
    n_courses = nrow(index), sheets = sheets
  )
  out <- new_sdt_participants(out, index, dictionary, meta)
  if (verbose) .report_read(out)
  out
}

.read_course_index <- function(path, call = rlang::caller_env()) {
  available <- readxl::excel_sheets(path)
  if (!"Course_Index" %in% available) {
    cli::cli_abort(
      c("The workbook has no {.field Course_Index} sheet.",
        "i" = "Found {length(available)} sheet{?s}."),
      class = "sandtablR_error_participants_sheet", call = call
    )
  }
  idx <- readxl::read_excel(path, sheet = "Course_Index",
                            .name_repair = "minimal")
  wanted <- .course_index_columns()
  missing <- setdiff(unname(wanted), names(idx))
  if (length(missing)) {
    n_missing <- length(missing)
    cli::cli_abort(
      c("{.field Course_Index} is missing {n_missing} required column{?s}.",
        "x" = "Missing: {.field {missing}}."),
      class = "sandtablR_error_participants_sheet", call = call
    )
  }
  idx <- idx[, unname(wanted), drop = FALSE]
  names(idx) <- names(wanted)
  idx$session_start <- as.Date(idx$session_start)
  idx$session_end <- as.Date(idx$session_end)
  tibble::as_tibble(idx)
}

.resolve_sheets <- function(path, index, sheets, call = rlang::caller_env()) {
  available <- readxl::excel_sheets(path)
  declared <- index$course_sheet
  absent <- setdiff(declared, available)
  if (length(absent)) {
    n_absent <- length(absent)
    cli::cli_abort(
      c("{.field Course_Index} names {n_absent} sheet{?s} the workbook
         does not contain.",
        "x" = "Missing: {.val {absent}}."),
      class = "sandtablR_error_participants_sheet", call = call
    )
  }
  if (is.null(sheets)) {
    return(declared)
  }
  if (!is.character(sheets) || !length(sheets)) {
    cli::cli_abort(
      "{.arg sheets} must be a non-empty character vector or {.code NULL}.",
      class = "sandtablR_error_participants_sheet", call = call
    )
  }
  unknown <- setdiff(sheets, declared)
  if (length(unknown)) {
    n_unknown <- length(unknown)
    cli::cli_abort(
      c("{.arg sheets} names {n_unknown} sheet{?s} that is not a course
         sheet.",
        "x" = "Unknown: {.val {unknown}}.",
        "i" = "Allowed: {.val {declared}}."),
      class = "sandtablR_error_participants_sheet", call = call
    )
  }
  sheets
}

# Every offending row is collected and reported together: a register that has
# gone wrong once has usually gone wrong more than once, and fixing them one
# abort at a time is the slowest possible way to find that out.
.validate_participants <- function(x, index, call = rlang::caller_env()) {
  checks <- list(
    list(field = "JSON_File_Name",
         rows = which(is.na(x$file)),
         detail = "no join key"),
    list(field = "JSON_File_Name",
         rows = which(!is.na(x$file) & !x$file %in% index$file),
         detail = "join key is not in Course_Index"),
    list(field = "Participant_Record_ID",
         rows = which(is.na(x$record_id)),
         detail = "no record identifier")
  )
  problems <- unlist(lapply(checks, .field_problem), use.names = FALSE)
  if (length(problems)) {
    n_bad <- length(problems)
    cli::cli_abort(
      c("The participant register cannot be joined to the corpus.",
        stats::setNames(problems, rep("x", n_bad)),
        "i" = "{n_bad} problem{?s} found; all offending rows are listed."),
      class = "sandtablR_error_participants", call = call
    )
  }
  dup <- unique(x$record_id[duplicated(x$record_id)])
  if (length(dup)) {
    n_dup <- length(dup)
    courses <- unique(x$short_course_id[x$record_id %in% dup])
    n_course <- length(courses)
    cli::cli_warn(
      c("{n_dup} participant record{?s} appear{?s/} in more than one course.",
        "i" = "Affected {n_course} course{?s}: {.val {courses}}.",
        "i" = "They are kept: one person may attend twice, but a
               course-level share then counts them twice."),
      class = "sandtablR_warning_participants"
    )
  }
  invisible(x)
}

.normalise_participants <- function(x, dictionary, index) {
  fun <- dictionary$funktion
  tit <- dictionary$titel
  fach <- dictionary$fachbereich

  out <- x
  out$sex <- factor(
    .map_ladder(x$anrede, dictionary$anrede),
    levels = dictionary$anrede$levels
  )

  title_level <- .map_ladder(x$titel, tit, fallback = "none")
  imputed_titel <- isTRUE(tit$blank_is_none) & is.na(x$titel)
  title_level[imputed_titel] <- "none"
  out$title_level <- factor(title_level, levels = tit$levels, ordered = TRUE)
  out$title_score <- unname(tit$score[title_level])

  specialty <- .map_first_mentioned(x$fachbereich, fach)
  out$specialty <- factor(specialty, levels = names(fach$group))
  out$specialty_group <- factor(
    unlist(fach$group, use.names = TRUE)[specialty],
    levels = c("surgical", "non_surgical")
  )

  seniority <- .map_ladder(x$funktion, fun)
  # A declared non-answer is missing information, not a grade: it keeps its
  # raw value in `funktion` and drops out of the ladder here.
  seniority[!is.na(seniority) & seniority %in% fun$not_stated] <- NA_character_
  out$seniority <- factor(seniority, levels = fun$levels, ordered = FALSE)
  out$seniority_score <- unname(fun$score[seniority])
  out$is_senior <- ifelse(is.na(seniority), NA, seniority %in% fun$senior)

  out$specialised_since <- .parse_de_date(x$spezialisiert_seit)
  anchor <- index$session_start[match(x$file, index$file)]
  out$years_specialised <- as.numeric(anchor - out$specialised_since) / 365.25
  out$training_years <- .parse_years(x$weiterbildungsjahre)

  # Missingness, one column per raw field. `funktion` and
  # `weiterbildungsjahre` are wider than "the cell was blank", by design.
  out$.missing_anrede <- is.na(out$sex)
  out$.missing_titel <- is.na(x$titel)
  out$.missing_fachbereich <- is.na(out$specialty)
  out$.missing_spezialisiert_seit <- is.na(out$specialised_since)
  out$.missing_verbandsmitglied <- is.na(x$verbandsmitglied)
  out$.missing_funktion <- is.na(seniority)
  out$.missing_weiterbildungsjahre <- is.na(out$training_years)

  # Imputation, one column per raw field. Only `titel` has a participant-level
  # fill rule; the others are left to sdt_course_composition(), which fills at
  # course level and flags there.
  out$.imputed_titel <- imputed_titel
  for (nm in setdiff(.participant_fields(), "titel")) {
    out[[paste0(".imputed_", nm)]] <- rep(FALSE, nrow(out))
  }

  keep <- c(
    "record_id", "file", "short_course_id", "course_sheet",
    "course_name", "course_date",
    .participant_fields(),
    "sex", "title_level", "title_score", "specialty", "specialty_group",
    "seniority", "seniority_score", "is_senior",
    "specialised_since", "years_specialised", "training_years",
    "source_file",
    paste0(".missing_", .participant_fields()),
    paste0(".imputed_", .participant_fields())
  )
  out[, keep, drop = FALSE]
}

# Reading is a data-quality event, so say what happened: how many rows came
# in, how many were diagnostics rather than people, which courses ended up
# empty, and how complete each covariate is.
.report_read <- function(x) {
  meta <- attr(x, "meta")
  idx <- attr(x, "course_index")
  n_course <- sum(idx$sheet_read)
  n_empty <- sum(idx$sheet_read & !idx$has_participants)
  n_skipped <- sum(!idx$sheet_read)
  cli::cli_inform(c(
    "v" = "Read {nrow(x)} participant record{?s} from {n_course} course{?s}.",
    "i" = "{meta$n_source_rows} source row{?s} read;
           {meta$n_placeholder_rows} course placeholder{?s} removed."
  ))
  if (n_skipped) {
    cli::cli_inform(c(
      "i" = "{n_skipped} course sheet{?s} not read, and marked
             {.code sheet_read = FALSE} rather than empty."
    ))
  }
  if (sum(meta$n_leaks)) {
    n_leak <- sum(meta$n_leaks)
    hit <- names(meta$leaks_by_field)
    cli::cli_inform(c(
      "!" = "{n_leak} extractor diagnostic{?s} found in data cells and
             neutralised to missing.",
      "i" = "In {.field {hit}}."
    ))
  }
  if (n_empty) {
    # `sheet_read &`, to match the count on the line that gates this block. A
    # course whose sheet was never read is not an empty course, and without
    # the guard a `sheets =` subset reports "1 course" and then names all 19.
    empty <- idx$short_course_id[idx$sheet_read & !idx$has_participants]
    cli::cli_inform(c(
      "!" = "{n_empty} course{?s} {?has/have} no linked participants:
             {.val {empty}}.",
      "i" = "They are kept in the {.field course_index} attribute so a
             caller sees them rather than inferring them."
    ))
  }
  miss <- vapply(
    .participant_fields(),
    function(f) mean(x[[paste0(".missing_", f)]]),
    numeric(1)
  )
  cli::cli_inform(c("i" = "Missing per covariate (n = {nrow(x)}):"))
  cli::cli_inform(stats::setNames(
    sprintf("%-20s %5.1f%%", names(miss), 100 * miss),
    rep("*", length(miss))
  ))
  cli::cli_inform(c(
    "i" = "Composition is a COURSE-level covariate: the register has no
           group column, so every group inside one course shares one
           composition row."
  ))
  invisible(x)
}

#' @exportS3Method tibble::tbl_sum
tbl_sum.sdt_participants <- function(x, ...) {
  idx <- attr(x, "course_index")
  n_empty <- sum(idx$sheet_read & !idx$has_participants)
  c(
    "Participants" = paste(nrow(x), "records"),
    "Courses" = sprintf("%d read of %d (%d with no participants)",
                        sum(idx$sheet_read), nrow(idx), n_empty),
    "Level" = "course, not group"
  )
}

# Which raw field a normalised covariate was derived from, so a summary can
# report the missingness of the answer rather than the completeness of the
# fill.
.summary_source <- function() {
  c(sex = "anrede", title_level = "titel", title_score = "titel",
    specialty = "fachbereich", specialty_group = "fachbereich",
    seniority = "funktion", seniority_score = "funktion",
    is_senior = "funktion")
}

#' Descriptives that state their denominator
#'
#' Tabulates the normalised participant covariates without silently dropping
#' or filling anything. Every row carries both denominators and both kinds of
#' uncertainty, so a reader cannot see a share without seeing what it rests
#' on:
#'
#' * `n` — rows at this level, filled rows included.
#' * `n_filled` — how many of those `n` came from a participant-level fill
#'   rather than from an answer somebody wrote down. Read `n - n_filled` as
#'   the observed count.
#' * `n_records` — every row in the group, the denominator of
#'   `share_records`.
#' * `n_observed` — rows where the raw field was actually recorded, taken
#'   from the `.missing_` indicator, and the denominator of
#'   `share_observed`, which counts observed rows only.
#' * `n_missing`, `share_missing` — what is not there, in the same row as the
#'   shares it is missing from.
#'
#' This is the honest half of the missingness policy. The imputing half lives
#' in [sdt_course_composition()], which fills and flags.
#'
#' @param x A `sdt_participants` object.
#' @param fields Optional character vector of covariates to report. Defaults
#'   to every categorical covariate the dictionary normalises.
#' @param by Optional column name to report within, typically `"file"` or
#'   `"short_course_id"`. `NULL`, the default, pools the whole register.
#'
#' @return A tibble with one row per level, carrying `n`, `n_filled`,
#'   `n_observed`, `n_records`, `share_observed`, `share_records`,
#'   `n_missing` and `share_missing`.
#' @seealso [sdt_read_participants()], [sdt_course_composition()].
#' @export
#' @examples
#' \dontrun{
#' p <- sdt_read_participants("TDSC_course_participant_relations.xlsx")
#' sdt_participant_summary(p)
#' sdt_participant_summary(p, fields = "seniority", by = "short_course_id")
#' }

sdt_participant_summary <- function(x, fields = NULL, by = NULL) {
  .check_class(x, "sdt_participants")
  known <- c("sex", "title_level", "specialty", "specialty_group",
             "seniority")
  fields <- fields %||% known
  unknown <- setdiff(fields, names(x))
  if (length(unknown)) {
    n_unknown <- length(unknown)
    cli::cli_abort(
      c("{.arg fields} names {n_unknown} column{?s} the register does
         not have.",
        "x" = "Unknown: {.field {unknown}}."),
      class = "sandtablR_error_participants"
    )
  }
  if (!is.null(by)) {
    .check_string(by)
    if (!by %in% names(x)) {
      cli::cli_abort(
        c("{.arg by} must name a column of the register.",
          "x" = "{.field {by}} is not one."),
        class = "sandtablR_error_participants"
      )
    }
  }
  groups <- if (is.null(by)) list(all = seq_len(nrow(x))) else {
    split(seq_len(nrow(x)), factor(x[[by]]))
  }
  src <- .summary_source()
  rows <- list()
  for (g in names(groups)) {
    idx <- groups[[g]]
    for (f in fields) {
      v <- x[[f]][idx]
      lv <- if (is.factor(v)) levels(v) else sort(unique(v[!is.na(v)]))
      fv <- factor(as.character(v), levels = lv)
      counts <- as.integer(table(fv))
      # Missingness is a property of the answer, not of the filled column.
      raw <- src[[f]]
      gone <- if (!is.na(raw) && paste0(".missing_", raw) %in% names(x)) {
        x[[paste0(".missing_", raw)]][idx]
      } else {
        is.na(v)
      }
      filled <- if (!is.na(raw) && paste0(".imputed_", raw) %in% names(x)) {
        x[[paste0(".imputed_", raw)]][idx]
      } else {
        rep(FALSE, length(idx))
      }
      n_fill <- as.integer(table(fv[filled])[lv])
      n_fill[is.na(n_fill)] <- 0L
      n_all <- length(idx)
      n_obs <- n_all - sum(gone)
      rows[[length(rows) + 1L]] <- tibble::tibble(
        group = g, field = f, level = lv, n = counts, n_filled = n_fill,
        n_observed = n_obs, n_records = n_all,
        share_observed = if (n_obs) (counts - n_fill) / n_obs else NA_real_,
        share_records = counts / n_all,
        n_missing = sum(gone),
        share_missing = sum(gone) / n_all
      )
    }
  }
  out <- do.call(rbind, rows)
  if (is.null(by)) out$group <- NULL else names(out)[[1L]] <- by
  out
}

# Course composition ----------------------------------------------------------

# Every covariate is a per-participant numeric vector plus a name, so the
# aggregation, the denominators, the flags and the fills are all one code
# path rather than seven copies of one.
.composition_covariates <- function() {
  as_num <- function(v) {
    out <- as.numeric(v)
    out[is.na(v)] <- NA_real_
    out
  }
  none <- function(d) rep(FALSE, nrow(d))
  # `value` is the per-participant number the course mean is taken over;
  # `filled` says which of those numbers came from a participant-level fill
  # rather than from an answer somebody actually wrote down. The academic
  # ladder is the only covariate with such a fill by default (an empty title
  # box becomes "none"), and the course row has to keep saying so -- otherwise
  # the denominator claims 16 observations when 5 of them were assumed.
  list(
    share_female = list(
      value = function(d) as_num(d$sex == "female"), filled = none
    ),
    share_consultant = list(
      value = function(d) as_num(d$is_senior), filled = none
    ),
    share_surgical = list(
      value = function(d) as_num(d$specialty_group == "surgical"),
      filled = none
    ),
    share_doctorate = list(
      value = function(d) as_num(d$title_score >= 1),
      filled = function(d) d$.imputed_titel
    ),
    share_professor = list(
      value = function(d) as_num(d$title_score >= 3),
      filled = function(d) d$.imputed_titel
    ),
    seniority_mean = list(
      value = function(d) as.numeric(d$seniority_score), filled = none
    )
  )
}

#' Aggregate participant metadata to course composition
#'
#' Collapses a `sdt_participants` register to one row per **course**, keyed by
#' the JSON `file` name so it joins straight onto the corpus, and carries the
#' composition covariates a model can use.
#'
#' # Why course level, and what that costs
#'
#' The register has no group, team or "Gruppe" column, so a participant cannot
#' be placed in group A rather than group B. Composition is therefore a
#' block-level covariate: in `y ~ card + (1 | block) + (1 | group)` it can
#' compete with the **session** random effect and can never explain the
#' **team** random effect. Every group inside one course gets the same row.
#' Fitting on this table is a between-course regression with as many points as
#' there are courses, and a covariate that barely varies between courses
#' cannot explain anything at all — check the spread before using one.
#'
#' # The two halves of the missingness policy
#'
#' Every covariate comes with three companions: `n_<covariate>`, the number of
#' participants the course mean was taken over; `n_imputed_<covariate>`, how
#' many of those numbers came from a participant-level fill rather than an
#' answer somebody wrote down (only the academic ladder has one by default);
#' and `<covariate>_missing`, whether the course value itself was filled. The
#' two count columns always tell the truth, whatever `impute` is set to.
#'
#' * `impute = "flag"` (the default) fills a missing covariate with the
#'   corpus-wide value — the pooled mean over **all** participants, not the
#'   mean of course means — and sets the indicator. A model can then carry
#'   "this was missing" as information instead of pretending it was observed.
#' * `impute = "median"` fills with the between-course median and does **not**
#'   set the indicator. It exists because a downstream pipeline can refuse a
#'   column that is perfectly collinear with `has_participants`, which is
#'   exactly what the indicator is when the only empty courses are the ones
#'   with no participants; dropping the indicator then buys rank instead of
#'   information. It is not the default because it makes an imputed value
#'   indistinguishable from an observed one, which is the thing this module
#'   exists to prevent. Only `n_<covariate>` still gives it away.
#' * `impute = "none"` leaves `NA` and still sets the indicator.
#'
#' A course with fewer than `min_participants` recorded people is marked in
#' `below_min_participants`. Its observed value is **kept**, not overwritten:
#' the flag is what stops it being read as if it were solid, and
#' `n_participants` is there to weight or filter on. A share computed from two
#' people is not missing, it is noisy, and those are different problems.
#'
#' @param x A `sdt_participants` object from [sdt_read_participants()].
#' @param impute How to handle a covariate a course has no observation for:
#'   `"flag"`, `"median"` or `"none"`. See the details.
#' @param min_participants Below this many recorded participants a course is
#'   marked as too small to average.
#'
#' @return A `sdt_course_composition` object: a tibble with one row per
#'   course, including courses with no participants at all, which come through
#'   with `has_participants = FALSE`, `n_participants = 0` and every covariate
#'   `NA` or imputed-and-flagged. A course whose sheet was not read carries
#'   `sheet_read = FALSE` and `n_participants = NA`, so a `sheets =` subset
#'   never masquerades as a set of cancelled courses.
#' @seealso [sdt_read_participants()], [sdt_participant_summary()].
#' @export
#' @examples
#' \dontrun{
#' p <- sdt_read_participants("TDSC_course_participant_relations.xlsx")
#' comp <- sdt_course_composition(p)
#' comp[, c("short_course_id", "n_participants", "share_consultant")]
#'
#' # How much does each covariate actually vary between courses?
#' vapply(comp[, c("share_female", "share_consultant")],
#'        stats::sd, numeric(1), na.rm = TRUE)
#' }
sdt_course_composition <- function(x,
                                   impute = c("flag", "median", "none"),
                                   min_participants = 3L) {
  .check_class(x, "sdt_participants")
  impute <- rlang::arg_match(impute)
  min_participants <- .check_count(min_participants)
  idx <- attr(x, "course_index")
  if (is.null(idx)) {
    cli::cli_abort(
      c("{.arg x} has lost its {.field course_index} attribute.",
        "i" = "Pass the object {.fn sdt_read_participants} returned; a
               {.pkg dplyr} verb will have dropped it."),
      class = "sandtablR_error_participants"
    )
  }

  out <- tibble::tibble(
    file = idx$file,
    short_course_id = idx$short_course_id,
    course_name = idx$course_name,
    relation_status = idx$relation_status,
    session_start = idx$session_start,
    n_source_rows = idx$n_source_rows,
    n_placeholder_rows = idx$n_placeholder_rows,
    n_participants = idx$n_participants,
    sheet_read = idx$sheet_read,
    has_participants = idx$has_participants,
    below_min_participants = !idx$has_participants |
      idx$n_participants < min_participants
  )

  by_course <- split(seq_len(nrow(x)), factor(x$file, levels = idx$file))
  covs <- .composition_covariates()
  fills <- stats::setNames(rep(NA_real_, length(covs)), names(covs))

  for (nm in names(covs)) {
    v <- covs[[nm]]$value(x)
    was_filled <- covs[[nm]]$filled(x) & !is.na(v)
    value <- vapply(by_course, function(i) {
      vi <- v[i]
      if (!length(vi) || all(is.na(vi))) NA_real_ else mean(vi, na.rm = TRUE)
    }, numeric(1))
    n_obs <- vapply(by_course, function(i) sum(!is.na(v[i])), integer(1))
    n_fill <- vapply(by_course, function(i) sum(was_filled[i]), integer(1))
    observed <- !is.na(value)
    fills[[nm]] <- switch(
      impute,
      flag = mean(v, na.rm = TRUE),
      median = stats::median(value[observed], na.rm = TRUE),
      none = NA_real_
    )
    if (impute != "none") value[!observed] <- fills[[nm]]
    out[[nm]] <- unname(value)
    out[[paste0("n_", nm)]] <- unname(n_obs)
    out[[paste0("n_imputed_", nm)]] <- unname(n_fill)
    # Under "median" the fill is deliberately not flagged. That is the whole
    # hazard of the mode, and the reason it is not the default.
    out[[paste0(nm, "_missing")]] <- if (identical(impute, "median")) {
      rep(FALSE, nrow(out))
    } else {
      unname(!observed)
    }
  }

  if (identical(impute, "median")) {
    n_fill <- sum(!out$has_participants)

    cli::cli_warn(
      c("{.code impute = \"median\"} fills without flagging.",
        "!" = "{n_fill} course{?s} have no participants at all and their
               filled values are now indistinguishable from observed ones.",
        "i" = "Only {.field n_<covariate>} still records the denominator."),
      class = "sandtablR_warning_unflagged_imputation"
    )
  }

  tibble::new_tibble(
    out, nrow = nrow(out), class = "sdt_course_composition",
    impute = impute, min_participants = min_participants,
    fill = fills, covariates = names(covs)
  )
}

#' @exportS3Method tibble::tbl_sum
tbl_sum.sdt_course_composition <- function(x, ...) {
  n_empty <- sum(x$sheet_read & !x$has_participants)
  n_small <- sum(x$below_min_participants & x$has_participants)
  c(
    "Course composition" = sprintf(
      "%d courses (%d with no participants, %d small but non-empty)",
      nrow(x), n_empty, n_small
    ),
    "Imputation" = paste0(
      attr(x, "impute"),
      if (identical(attr(x, "impute"), "median")) " (UNFLAGGED)" else ""
    ),
    "Level" = "course, not group"
  )
}
