# Gold-standard triage codebook -----------------------------------------------
# The exercise deck's own answer key: one row per patient card, carrying the
# sighting category the card was written to carry, together with the card's
# recorded sex, age, clinical findings and vital signs.
#
# WHY THIS FILE EXISTS. Every triage number computable from a course export
# alone is inter-team agreement, not accuracy: the export records what teams
# did, never what the card was. The field that looks like an answer key
# (`IdealSk`) is a round-robin placeholder. The codebook read here *is* the
# answer key, which turns "was this call right?" into a question with an
# answer.
#
# THREE COLUMNS OF THE COHORT CONTRACT CHANGE STATUS when it is applied:
#   triage  surrogate (inter-team consensus, circular) -> ground truth
#   age     placeholder (a constant)                   -> the card's own age
#   sex     inferred (a gendered word in the log)      -> the card's record
# `sdt_apply_gold_standard()` keeps each replaced column as `<col>_original`,
# so the inference can still be scored against the truth that replaced it.
#
# THE SHEET IS NOT A CLEAN RECTANGLE. Below the deck sit duplicate test rows
# and a column-shifted legend block. Both are removed by rule -- a card id
# that is not a number, or a number outside the deck -- never by row position,
# because row positions are exactly what an edited workbook changes. The
# legend is then read back and used as an independent check on the parse.

# The deck. Fixed by the exercise, not by the workbook, so a codebook that
# does not describe all 46 cards is a reading error rather than a short deck.
.gold_deck_size <- function() 46L

# Column contract. Each field lists the normalised header keys accepted for
# it, most canonical first; see `.gold_key()` for the normalisation.
#
# Two workbook generations are in circulation and both must read. The regraded
# codebook writes "Sichtungskategorie" where the earlier one wrote
# "Sichtungskathegorie", and suffixes its two free-text columns
# ("Code 001_Befund Sichtung"). Matching is exact on the normalised key rather
# than by prefix, so a new spelling is one entry here -- and a column that is
# genuinely absent still aborts instead of binding to something close.
.gold_columns <- function() {
  list(
    card = c("patnraustdsc", "patnrtdsc", "patientenid", "patientennr"),
    sex = "geschlecht",
    age = "alter",
    sk_raw = c("sichtungskathegorie", "sichtungskategorie", "sk"),
    findings = c("code001befundsichtung", "code001befund", "befund"),
    vitals = c("code001vitalwertesichtung", "code001vitalwerte", "vitalwerte")
  )
}

# The columns a caller gets back, in order.
.gold_output_columns <- function() {
  c("card", "triage", "sex", "age", "findings", "vitals", "sk_raw")
}

# Fold a header onto a lookup key so case, spacing and punctuation stop
# mattering: "Pat. Nr aus TDSC", "pat_nr_aus_tdsc" and "PatNrAusTDSC" are one
# entry rather than three.
.gold_key <- function(x) gsub("[^a-z0-9]+", "", tolower(as.character(x)))

# Sighting category as the codebook writes it -> the package vocabulary.
# Arabic in the deck rows, Roman in the legend block.
.gold_sk_map <- function() {
  c("1" = "red", "2" = "yellow", "3" = "green",
    "i" = "red", "ii" = "yellow", "iii" = "green",
    "ski" = "red", "skii" = "yellow", "skiii" = "green")
}

# Recorded sex, keyed on the first letter so that "w" and "weiblich" fold
# together without the source's umlauts having to survive normalisation.
.gold_sex_map <- function() c(m = "m", w = "f", f = "f", d = "d")

.gold_sk_triage <- function(x) {
  unname(.gold_sk_map()[.gold_key(x)])
}

.gold_sex <- function(x) {
  first <- substr(tolower(trimws(as.character(x))), 1L, 1L)
  unname(.gold_sex_map()[first])
}

# Text that is only whitespace is missing, not a value.
.gold_chr <- function(x) {
  x <- trimws(as.character(x))
  x[!is.na(x) & !nzchar(x)] <- NA_character_
  x
}

# A whole number or nothing. "SK I" in an age cell is noise, never an age.
.gold_int <- function(x) {
  x <- .gold_chr(x)
  ok <- !is.na(x) & grepl("^-?[0-9]+$", x)
  out <- rep(NA_integer_, length(x))
  out[ok] <- as.integer(x[ok])
  out
}

# Card ids are compared and joined as zero-padded strings, because that is how
# the course export writes them.
.gold_card_label <- function(x) {
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  out[ok] <- sprintf("%02d", as.integer(x[ok]))
  out
}

# Coerce any user-supplied card id onto the same padded form: an integer
# column, a bare "7", and "07" all become "07".
.gold_card_key <- function(x) {
  if (is.numeric(x)) {
    return(.gold_card_label(suppressWarnings(as.integer(x))))
  }
  x <- .gold_chr(x)
  numeric_like <- !is.na(x) & grepl("^[0-9]+$", x)
  x[numeric_like] <- .gold_card_label(as.integer(x[numeric_like]))
  x
}

# Reading the codebook --------------------------------------------------------

new_sdt_gold_standard <- function(x, meta) {
  tibble::new_tibble(
    x, nrow = nrow(x), class = "sdt_gold_standard", meta = meta
  )
}

#' Read the gold-standard triage codebook
#'
#' Reads the exercise deck's answer key: one row per patient card, giving the
#' sighting category the card was written to carry, plus the card's recorded
#' sex, age, clinical findings and vital signs.
#'
#' This is the ground truth the course exports do not contain. An export
#' records the calls teams made, so any triage number computed from it alone
#' is inter-team agreement; the field that looks like an answer key is a
#' round-robin placeholder. Pair this reader with
#' [sdt_apply_gold_standard()] and [sdt_triage_accuracy()] to score calls
#' against the deck rather than against each other.
#'
#' # The sheet is not a clean rectangle
#'
#' Codebooks acquire duplicate test rows and legend blocks below the data, and
#' a legend written into shifted columns parses as data if it is not caught.
#' Three guards remove them, all by rule rather than by row position, since row
#' positions are the first thing an edited workbook changes:
#'
#' * `blank` — every contract column is empty.
#' * `non_numeric_card` — the card id is not a number, which is what catches
#'   both duplicate test rows carrying a letter id and the shifted legend.
#' * `card_out_of_range` — the card id is a number outside the deck.
#'
#' How many rows each guard removed is reported when `verbose = TRUE` and kept
#' in the `meta` attribute, so a silent change in the workbook shows up as a
#' changed count rather than as a changed result.
#'
#' # Cross-checks rather than a preference
#'
#' Two independent statements of the same counts are read back and compared
#' with the parse: the sheet's own `SK I`/`SK II`/`SK III` legend, whose totals
#' include the duplicate rows the guards removed, and any earlier working sheet
#' in the same workbook. Disagreements are reported, never resolved by quietly
#' preferring one source.
#'
#' @param path Path to the codebook workbook (`.xlsx`). Read-only; nothing is
#'   written to it or beside it.
#' @param sheet Sheet to read. `NULL`, the default, takes the sheet whose name
#'   begins with `"Verteilung"`, or the only sheet if the workbook has one.
#' @param verbose Report the parsed deck, the guard drops and the cross-checks
#'   while reading.
#'
#' @return An `sdt_gold_standard` object: a [tibble::tibble()] subclass with
#'   one row per card and the columns `card` (`"01"`–`"46"`), `triage`, `sex`,
#'   `age`, `findings`, `vitals` and `sk_raw`. The `meta` attribute carries the
#'   path, the sheet, the guard drops, the legend cross-check and the working
#'   sheet cross-check.
#' @seealso [sdt_apply_gold_standard()] to write it into a cohort,
#'   [sdt_triage_accuracy()] to score calls against it.
#' @export
#' @examples
#' \dontrun{
#' gold <- sdt_read_gold_standard("Patienten Codebook.xlsx")
#' gold
#' attr(gold, "meta")$guards
#' }
sdt_read_gold_standard <- function(path, sheet = NULL, verbose = TRUE) {
  .check_string(path)
  .check_flag(verbose)
  if (!is.null(sheet)) .check_string(sheet)
  .require_pkgs("readxl")
  if (!file.exists(path)) {
    cli::cli_abort(
      c("Cannot read the gold-standard codebook.",
        "x" = "No file at {.file {path}}."),
      class = "sandtablR_error_gold"
    )
  }

  sheets <- readxl::excel_sheets(path)
  sheet <- .gold_resolve_sheet(sheets, sheet)
  raw <- readxl::read_excel(path, sheet = sheet, col_types = "text",
                            .name_repair = "minimal")
  raw <- tibble::as_tibble(raw)
  n_source <- nrow(raw)

  mapped <- .gold_map_columns(raw)
  guarded <- .gold_guards(mapped)
  out <- .gold_parse(guarded$kept)
  .gold_validate(out)

  legend <- .gold_legend(raw)
  legend_check <- .gold_check_legend(out, guarded$dropped, legend)
  working <- .gold_check_working(path, sheets, sheet, out)

  meta <- list(
    path = path, sheet = sheet, read_at = Sys.time(),
    n_source_rows = n_source, n_cards = nrow(out),
    guards = guarded$guards, dropped = guarded$dropped,
    legend = legend_check, working_sheet = working,
    deck_size = .gold_deck_size()
  )
  out <- new_sdt_gold_standard(out, meta)
  if (verbose) .gold_report(out)
  out
}

.gold_resolve_sheet <- function(sheets, sheet, call = rlang::caller_env()) {
  if (!is.null(sheet)) {
    if (!sheet %in% sheets) {
      cli::cli_abort(
        c("The workbook has no sheet {.val {sheet}}.",
          "i" = "It has: {.val {sheets}}."),
        class = "sandtablR_error_gold", call = call
      )
    }
    return(sheet)
  }
  hit <- grep("^Verteilung", sheets)
  if (length(hit)) {
    return(sheets[[hit[[1L]]]])
  }
  if (length(sheets) == 1L) {
    return(sheets[[1L]])
  }
  n_sheets <- length(sheets)
  cli::cli_abort(
    c("Cannot tell which sheet holds the codebook.",
      "x" = "{n_sheets} sheet{?s} found, none named {.val Verteilung...}.",
      "i" = "Name one with {.arg sheet}: {.val {sheets}}."),
    class = "sandtablR_error_gold", call = call
  )
}

# Pull the contract columns out of the sheet by header, not by position, and
# say which ones are missing all at once.
.gold_map_columns <- function(raw, call = rlang::caller_env()) {
  wanted <- .gold_columns()
  keys <- .gold_key(names(raw))
  found <- vapply(
    wanted,
    function(accepted) {
      hit <- match(accepted, keys)
      hit <- hit[!is.na(hit)]
      if (length(hit)) hit[[1L]] else NA_integer_
    },
    integer(1)
  )
  missing <- names(found)[is.na(found)]
  if (length(missing)) {
    n_missing <- length(missing)
    cli::cli_abort(
      c("The codebook sheet is missing {n_missing} required column{?s}.",
        "x" = "Missing: {.field {missing}}.",
        "i" = "Found: {.field {names(raw)}}."),
      class = "sandtablR_error_gold", call = call
    )
  }
  out <- raw[, unname(found), drop = FALSE]
  names(out) <- names(found)
  for (nm in names(out)) out[[nm]] <- .gold_chr(out[[nm]])
  out
}

# The three guards. Each is a rule over the values, and each reports what it
# removed, so that a workbook edit shows up as a changed count.
.gold_guards <- function(x) {
  fields <- names(.gold_columns())
  filled <- rowSums(!is.na(as.data.frame(x[, fields])))
  blank <- filled == 0L

  id <- x$card
  non_numeric <- !blank & (is.na(id) | !grepl("^[0-9]+$", id))

  num <- suppressWarnings(as.integer(id))
  out_of_range <- !blank & !non_numeric &
    (is.na(num) | num < 1L | num > .gold_deck_size())

  reason <- rep(NA_character_, nrow(x))
  reason[out_of_range] <- "card_out_of_range"
  reason[non_numeric] <- "non_numeric_card"
  reason[blank] <- "blank"

  drop <- !is.na(reason)
  dropped <- x[drop, , drop = FALSE]
  dropped$sheet_row <- which(drop)
  dropped$guard <- reason[drop]

  levels <- c("blank", "non_numeric_card", "card_out_of_range")
  guards <- tibble::tibble(
    guard = levels,
    n_dropped = as.integer(table(factor(reason, levels = levels))),
    ids = vapply(
      levels,
      function(g) {
        vals <- dropped$card[dropped$guard == g]
        vals[is.na(vals)] <- "<NA>"
        if (!length(vals)) "" else paste(utils::head(vals, 6L), collapse = ", ")
      },
      character(1),
      USE.NAMES = FALSE
    )
  )
  list(kept = x[!drop, , drop = FALSE], dropped = dropped, guards = guards)
}

.gold_parse <- function(x) {
  num <- suppressWarnings(as.integer(x$card))
  out <- tibble::tibble(
    card = .gold_card_label(num),
    triage = .gold_sk_triage(x$sk_raw),
    sex = .gold_sex(x$sex),
    age = .gold_int(x$age),
    findings = x$findings,
    vitals = x$vitals,
    sk_raw = x$sk_raw
  )
  out[, .gold_output_columns()]
}

# Every offending row at once, in the shape `validate_cohort()` uses, so a
# codebook with four bad cells is reported once rather than four times.
.gold_validate <- function(x, call = rlang::caller_env()) {
  levels <- sdt_triage_levels()
  sexes <- sdt_sex_levels()
  checks <- list(
    list(field = "card", detail = "must be a card id",
         rows = which(is.na(x$card))),
    list(field = "card", detail = "must be unique",
         rows = which(duplicated(x$card) & !is.na(x$card))),
    list(field = "triage",
         detail = cli::format_inline("must be one of {.val {levels}}"),
         rows = which(is.na(x$triage) | !x$triage %in% levels)),
    list(field = "sex",
         detail = cli::format_inline("must be one of {.val {sexes}}"),
         rows = which(is.na(x$sex) | !x$sex %in% sexes)),
    list(field = "age", detail = "must be a non-missing age in 0:130",
         rows = which(is.na(x$age) | x$age < 0L | x$age > 130L))
  )
  problems <- unlist(lapply(checks, .field_problem), use.names = FALSE)

  n_deck <- .gold_deck_size()
  n_have <- length(unique(stats::na.omit(x$card)))
  if (n_have != n_deck) {
    problems <- c(
      problems,
      cli::format_inline(
        "{.field card}: expected {n_deck} unique cards, found {n_have}."
      )
    )
  }
  if (length(problems)) {
    bullets <- stats::setNames(problems, rep("x", length(problems)))
    cli::cli_abort(
      c("The gold-standard codebook is not valid.", bullets),
      class = "sandtablR_error_gold", call = call
    )
  }
  invisible(x)
}

# Cross-checks ----------------------------------------------------------------

# The legend block states the deck's category counts in its own words. It is
# written in shifted columns, so it is found by scanning every cell for an
# "SK I"-style label and taking the first number to its right on the same row.
.gold_legend <- function(raw) {
  cells <- as.matrix(as.data.frame(raw))
  if (!length(cells)) {
    return(NULL)
  }
  cells[] <- trimws(cells)
  # `grepl()` drops the dims, so they are put back before `which()` is asked
  # for row/column positions.
  flag <- matrix(
    grepl("^SK\\s*I{1,3}$", cells, ignore.case = TRUE),
    nrow = nrow(cells), ncol = ncol(cells)
  )
  hits <- which(flag, arr.ind = TRUE)
  if (!nrow(hits)) {
    return(NULL)
  }
  count_right <- function(i, j) {
    if (j >= ncol(cells)) {
      return(NA_integer_)
    }
    rest <- cells[i, seq.int(j + 1L, ncol(cells))]
    rest <- rest[!is.na(rest) & nzchar(rest)]
    num <- .gold_int(rest)
    num <- num[!is.na(num)]
    if (length(num)) num[[1L]] else NA_integer_
  }
  tibble::tibble(
    label = cells[hits],
    triage = .gold_sk_triage(cells[hits]),
    n_legend = vapply(
      seq_len(nrow(hits)),
      function(k) count_right(hits[k, 1L], hits[k, 2L]),
      integer(1)
    )
  )
}

# Compare the parse with the legend. The legend counts the sheet as it stands,
# duplicate rows included, so the comparable quantity is the parsed count plus
# whatever the guards removed that still carried a readable category.
.gold_check_legend <- function(gold, dropped, legend) {
  levels <- sdt_triage_levels()
  parsed <- as.integer(table(factor(gold$triage, levels = levels)))
  dropped_triage <- .gold_sk_triage(dropped$sk_raw)
  n_dropped <- as.integer(table(factor(dropped_triage, levels = levels)))
  out <- tibble::tibble(
    triage = levels,
    n_parsed = parsed,
    n_dropped = n_dropped,
    n_with_dropped = parsed + n_dropped,
    n_legend = NA_integer_
  )
  if (!is.null(legend)) {
    out$n_legend <- legend$n_legend[match(levels, legend$triage)]
  }
  out$agrees <- !is.na(out$n_legend) & out$n_legend == out$n_with_dropped
  out
}

# Any other sheet in the same workbook is an independent statement of the same
# deck. Read it, compare card by card, and report where the two disagree.
.gold_check_working <- function(path, sheets, sheet, gold) {
  others <- setdiff(sheets, sheet)
  if (!length(others)) {
    return(NULL)
  }
  name <- others[[1L]]
  raw <- tryCatch(
    readxl::read_excel(path, sheet = name, col_types = "text",
                       .name_repair = "minimal"),
    error = function(e) NULL
  )
  if (is.null(raw) || !nrow(raw)) {
    return(list(sheet = name, usable = FALSE, note = "sheet is empty"))
  }
  keys <- .gold_key(names(raw))
  id_col <- match(c("patientenid", "patnraustdsc", "patientennr"), keys)
  sk_col <- match(c("sk", "sichtungskathegorie", "sichtungskategorie"), keys)
  id_col <- id_col[!is.na(id_col)]
  sk_col <- sk_col[!is.na(sk_col)]
  if (!length(id_col) || !length(sk_col)) {
    return(list(sheet = name, usable = FALSE,
                note = "no card id and category columns"))
  }
  id <- .gold_chr(raw[[id_col[[1L]]]])
  sk <- .gold_chr(raw[[sk_col[[1L]]]])
  keep <- !is.na(id) & grepl("^[0-9]+$", id)
  num <- suppressWarnings(as.integer(id[keep]))
  keep2 <- !is.na(num) & num >= 1L & num <= .gold_deck_size()
  cmp <- tibble::tibble(
    card = .gold_card_label(num[keep2]),
    triage_working = .gold_sk_triage(sk[keep][keep2])
  )
  cmp <- cmp[!duplicated(cmp$card), , drop = FALSE]
  cmp$triage_gold <- gold$triage[match(cmp$card, gold$card)]
  cmp <- cmp[!is.na(cmp$triage_gold) & !is.na(cmp$triage_working), ,
             drop = FALSE]
  disagree <- cmp[cmp$triage_gold != cmp$triage_working, , drop = FALSE]
  list(
    sheet = name, usable = TRUE, n_compared = nrow(cmp),
    n_agree = nrow(cmp) - nrow(disagree),
    disagreements = disagree[, c("card", "triage_gold", "triage_working")]
  )
}

# Reading is a data-quality event, so say what happened.
.gold_report <- function(x) {
  meta <- attr(x, "meta")
  mix <- table(factor(x$triage, levels = sdt_triage_levels()))
  n_female <- sum(x$sex == "f")
  age_range <- range(x$age)
  cli::cli_inform(c(
    "v" = "Read {nrow(x)} card{?s} from sheet {.val {meta$sheet}}.",
    "i" = "Triage: {paste(names(mix), as.integer(mix), sep = ' ',
           collapse = ', ')}.",
    "i" = "Sex: {n_female} female of {nrow(x)}; age {age_range[[1]]}
           to {age_range[[2]]}."
  ))
  g <- meta$guards
  drops <- g[g$n_dropped > 0L, , drop = FALSE]
  if (nrow(drops)) {
    lines <- sprintf(
      "%-18s %2d row(s)%s", drops$guard, drops$n_dropped,
      ifelse(nzchar(drops$ids), paste0(" [", drops$ids, "]"), "")
    )
    n_drop <- sum(drops$n_dropped)
    cli::cli_inform(c("i" = "{n_drop} row{?s} removed by rule:"))
    cli::cli_inform(stats::setNames(lines, rep("*", length(lines))))
  }
  .gold_report_legend(meta$legend)
  .gold_report_working(meta$working_sheet)
  invisible(x)
}

.gold_report_legend <- function(legend) {
  if (is.null(legend) || all(is.na(legend$n_legend))) {
    cli::cli_inform(c("!" = "No legend block found; parse not cross-checked."))
    return(invisible(NULL))
  }
  lines <- sprintf(
    "%-7s parsed %2d + dropped %d = %2d, legend %s",
    legend$triage, legend$n_parsed, legend$n_dropped,
    legend$n_with_dropped,
    ifelse(is.na(legend$n_legend), "--", as.character(legend$n_legend))
  )
  cli::cli_inform(c("i" = "Legend cross-check:"))
  cli::cli_inform(stats::setNames(lines, rep("*", length(lines))))
  bad <- legend[!legend$agrees & !is.na(legend$n_legend), , drop = FALSE]
  if (nrow(bad)) {
    n_bad <- nrow(bad)
    cli::cli_inform(c(
      "!" = "The legend disagrees on {n_bad} categor{?y/ies}:
             {.val {bad$triage}}."
    ))
  }
  invisible(NULL)
}

.gold_report_working <- function(w) {
  if (is.null(w)) {
    return(invisible(NULL))
  }
  if (!isTRUE(w$usable)) {
    cli::cli_inform(c(
      "i" = "Sheet {.val {w$sheet}} not usable as a cross-check: {w$note}."
    ))
    return(invisible(NULL))
  }
  cli::cli_inform(c(
    "i" = "Sheet {.val {w$sheet}}: {w$n_agree}/{w$n_compared} cards agree."
  ))
  if (nrow(w$disagreements)) {
    lines <- sprintf(
      "card %s  codebook %s  %s %s", w$disagreements$card,
      w$disagreements$triage_gold, w$sheet, w$disagreements$triage_working
    )
    n_bad <- nrow(w$disagreements)
    cli::cli_inform(c(
      "!" = "Disagreement on {n_bad} card{?s}; neither source is preferred:"
    ))
    cli::cli_inform(stats::setNames(lines, rep("*", length(lines))))
  }
  invisible(NULL)
}

#' @exportS3Method tibble::tbl_sum
tbl_sum.sdt_gold_standard <- function(x, ...) {
  meta <- attr(x, "meta")
  mix <- table(factor(x$triage, levels = sdt_triage_levels()))
  c(
    "Gold standard" = paste(nrow(x), "cards"),
    "Triage" = paste(names(mix), as.integer(mix), sep = ": ", collapse = "  "),
    "Source" = basename(meta$path %||% "<unknown>")
  )
}

# Wiring the truth into the data model ----------------------------------------

#' Overwrite a cohort's placeholders with the gold standard
#'
#' Joins the gold-standard codebook onto a cohort, or onto any frame carrying a
#' card id, and replaces the named columns with the deck's own values. The
#' replaced columns are kept as `<col>_original`, so the provenance is not lost
#' and the inference that was there before can still be scored against the
#' truth that replaced it.
#'
#' In the TDSC corpus the three replaceable columns start out as a circular
#' surrogate (`triage`, the inter-team consensus call), a constant
#' (`age`) and an inference from the log's gendered wording (`sex`). After this
#' function they are the card's own values, and `triage_source` reads `"gold"`
#' so a downstream result can say which label it routed on. The engine routes
#' on `triage`, so which label sits there decides which question a simulation
#' answers.
#'
#' @param cohort A `cohort` from [sdt_cohort()], or any data frame with a card
#'   id column.
#' @param gold An `sdt_gold_standard` from [sdt_read_gold_standard()].
#' @param on Name of the card id column in `cohort`. Values may be padded
#'   (`"07"`), unpadded (`"7"`) or numeric; all three join.
#' @param replace Which columns to overwrite: any of `"triage"`, `"age"` and
#'   `"sex"`. Pass `character()` to join nothing and only stamp
#'   `triage_source`.
#' @param keep_original Keep each replaced column as `<col>_original`.
#'
#' @return `cohort` with the named columns replaced, the originals kept when
#'   asked for, and a `triage_source` column reading `"gold"`. A `cohort` comes
#'   back a validated `cohort`; anything else comes back a
#'   [tibble::tibble()]. The `gold_applied` attribute records what was done.
#'
#' @section Provenance:
#' `triage_source` names where the label sitting in `triage` came from, so it
#' reads `"gold"` only when `triage` was among the columns replaced. Ask for a
#' subset that leaves `triage` alone and any existing source label is left
#' alone with it: a surrogate must never end up wearing the truth's label.
#' @seealso [sdt_read_gold_standard()], [sdt_triage_accuracy()].
#' @export
#' @examples
#' \dontrun{
#' gold <- sdt_read_gold_standard("Patienten Codebook.xlsx")
#' truth <- sdt_apply_gold_standard(pooled, gold)
#' mean(truth$triage == truth$triage_original)
#' }
sdt_apply_gold_standard <- function(cohort, gold, on = "card",
                                    replace = c("triage", "age", "sex"),
                                    keep_original = TRUE) {
  .check_class(gold, "sdt_gold_standard")
  .check_string(on)
  .check_flag(keep_original)
  if (!is.data.frame(cohort)) {
    cli::cli_abort(
      c("{.arg cohort} must be a data frame.",
        "x" = "Got {.cls {class(cohort)[[1]]}} instead."),
      class = "sandtablR_error_gold"
    )
  }
  allowed <- c("triage", "age", "sex")
  if (is.null(replace)) replace <- character()
  bad <- setdiff(replace, allowed)
  if (!is.character(replace) || length(bad)) {
    cli::cli_abort(
      c("{.arg replace} must name columns the codebook carries.",
        "x" = "Unknown: {.val {bad}}.",
        "i" = "Allowed: {.val {allowed}}."),
      class = "sandtablR_error_gold"
    )
  }
  if (!on %in% names(cohort)) {
    cli::cli_abort(
      c("{.arg cohort} has no column {.field {on}} to join on.",
        "i" = "It has: {.field {names(cohort)}}."),
      class = "sandtablR_error_gold"
    )
  }

  key <- .gold_card_key(cohort[[on]])
  absent <- sort(unique(key[!key %in% gold$card]))
  if (length(absent)) {
    n_absent <- length(absent)
    shown <- utils::head(absent, 10L)
    cli::cli_abort(
      c("{n_absent} card{?s} in {.arg cohort} are not in the codebook.",
        "x" = "Missing: {.val {shown}}.",
        "i" = "The codebook covers {nrow(gold)} card{?s}."),
      class = "sandtablR_error_gold"
    )
  }

  idx <- match(key, gold$card)
  was_cohort <- inherits(cohort, "cohort")
  out <- tibble::as_tibble(cohort)
  kept <- character()
  for (nm in replace) {
    if (keep_original && nm %in% names(out)) {
      out[[paste0(nm, "_original")]] <- out[[nm]]
      kept <- c(kept, nm)
    }
    out[[nm]] <- gold[[nm]][idx]
  }
  # The column says where the label in `triage` came from, so it is only
  # stamped when `triage` was actually replaced. Claiming "gold" over a
  # surrogate that is still sitting in the column would be the precise
  # mislabelling this whole file exists to end.
  if ("triage" %in% replace) {
    out$triage_source <- "gold"
  } else if (!"triage_source" %in% names(out)) {
    out$triage_source <- NA_character_
  }

  attr(out, "gold_applied") <- list(
    on = on, replaced = replace, kept_original = kept,
    n_rows = nrow(out), n_cards = length(unique(key)),
    source = attr(gold, "meta")$path
  )
  if (was_cohort) {
    out <- new_cohort(out)
    validate_cohort(out)
  }
  out
}

# Scoring a call against the truth --------------------------------------------

# Clinical severity order. Over- and under-triage are only meaningful on it:
# calling a green casualty red is an over-triage, the reverse is an
# under-triage, and the two carry very different costs.
.triage_rank <- function() {
  stats::setNames(seq_along(sdt_triage_levels()), sdt_triage_levels())
}

.triage_confusion <- function(truth, estimate) {
  levels <- sdt_triage_levels()
  out <- table(
    truth = factor(truth, levels = levels),
    estimate = factor(estimate, levels = levels)
  )
  unclass(out)
}

# Cohen's kappa: agreement above what the two marginals would give by chance.
.triage_kappa <- function(cm) {
  n <- sum(cm)
  if (n == 0) {
    return(NA_real_)
  }
  po <- sum(diag(cm)) / n
  pe <- sum(rowSums(cm) * colSums(cm)) / (n * n)
  if (pe >= 1) {
    return(NA_real_)
  }
  (po - pe) / (1 - pe)
}

.rate <- function(num, den) ifelse(den > 0L, num / den, NA_real_)

# One accuracy block: a row per truth category plus an "overall" row. Kappa is
# a property of the whole table, so it is reported on the overall row only
# rather than repeated meaninglessly down the column.
.triage_block <- function(truth, estimate) {
  levels <- sdt_triage_levels()
  rank <- .triage_rank()
  hit <- truth == estimate
  over <- rank[estimate] < rank[truth]
  under <- rank[estimate] > rank[truth]
  cm <- .triage_confusion(truth, estimate)

  rows <- lapply(c(levels, "overall"), function(l) {
    i <- if (identical(l, "overall")) rep(TRUE, length(truth)) else truth == l
    n <- sum(i)
    tibble::tibble(
      category = l, n = as.integer(n),
      n_hit = as.integer(sum(hit[i])),
      hit_rate = .rate(sum(hit[i]), n),
      n_over = as.integer(sum(over[i])),
      over_rate = .rate(sum(over[i]), n),
      n_under = as.integer(sum(under[i])),
      under_rate = .rate(sum(under[i]), n),
      kappa = if (identical(l, "overall")) .triage_kappa(cm) else NA_real_
    )
  })
  list(table = do.call(rbind, rows), confusion = cm)
}

#' Score triage calls against the truth
#'
#' Hit rate per sighting category and overall, with the confusion matrix, and
#' with the two directions of error kept apart. This is the function that only
#' becomes answerable once a gold standard exists: without one, the same
#' arithmetic measures inter-team agreement.
#'
#' # Direction matters
#'
#' A triage error is not symmetric. On the severity order
#' `red > yellow > green`, calling a casualty *more* urgent than they are is an
#' **over-triage**: it wastes a scarce resource. Calling them *less* urgent is
#' an **under-triage**: it can cost a life. A single accuracy number hides
#' which of the two a team is doing, so both rates are reported alongside the
#' hit rate, per category and overall.
#'
#' Cohen's kappa is reported on the overall row: it is a property of the whole
#' table, and repeating it on every category row would state it four times
#' without adding anything.
#'
#' @param x A data frame carrying both labels.
#' @param truth Column holding the true category. With a cohort that has been
#'   through [sdt_apply_gold_standard()], this is `"triage"`.
#' @param estimate Column holding the called category.
#' @param by Optional character vector of columns to split by, for example
#'   `"group"` or `c("file", "group")`. `NULL`, the default, pools everything.
#'
#' @return An `sdt_triage_accuracy` object: a [tibble::tibble()] subclass with
#'   the `by` columns, then `category` (each triage level plus `"overall"`),
#'   `n`, `n_hit`, `hit_rate`, `n_over`, `over_rate`, `n_under`, `under_rate`
#'   and `kappa`. The `confusion` attribute holds the matrix (truth in rows,
#'   estimate in columns), or a named list of them when `by` is given; `meta`
#'   records the columns used and how many rows were unusable.
#' @seealso [sdt_read_gold_standard()], [sdt_apply_gold_standard()].
#' @export
#' @examples
#' calls <- data.frame(
#'   triage = c("red", "red", "yellow", "green", "green"),
#'   called_triage = c("red", "yellow", "yellow", "yellow", "green")
#' )
#' acc <- sdt_triage_accuracy(calls)
#' acc
#' attr(acc, "confusion")
sdt_triage_accuracy <- function(x, truth = "triage",
                                estimate = "called_triage", by = NULL) {
  .check_string(truth)
  .check_string(estimate)
  if (!is.data.frame(x)) {
    cli::cli_abort(
      c("{.arg x} must be a data frame.",
        "x" = "Got {.cls {class(x)[[1]]}} instead."),
      class = "sandtablR_error_triage_accuracy"
    )
  }
  needed <- c(truth, estimate, by)
  missing <- setdiff(needed, names(x))
  if (length(missing)) {
    n_missing <- length(missing)
    cli::cli_abort(
      c("{.arg x} is missing {n_missing} named column{?s}.",
        "x" = "Missing: {.field {missing}}.",
        "i" = "It has: {.field {names(x)}}."),
      class = "sandtablR_error_triage_accuracy"
    )
  }
  if (!is.null(by) && !is.character(by)) {
    cli::cli_abort(
      "{.arg by} must be a character vector of column names or {.code NULL}.",
      class = "sandtablR_error_triage_accuracy"
    )
  }

  levels <- sdt_triage_levels()
  t_val <- as.character(x[[truth]])
  e_val <- as.character(x[[estimate]])
  unknown <- setdiff(
    unique(c(t_val[!is.na(t_val)], e_val[!is.na(e_val)])), levels
  )
  if (length(unknown)) {
    n_unknown <- length(unknown)
    shown <- utils::head(unknown, 5L)
    cli::cli_abort(
      c("{.arg truth} and {.arg estimate} must hold triage levels.",
        "x" = "{n_unknown} unrecognised value{?s}: {.val {shown}}.",
        "i" = "Allowed: {.val {levels}}."),
      class = "sandtablR_error_triage_accuracy"
    )
  }
  usable <- !is.na(t_val) & !is.na(e_val)
  n_dropped <- sum(!usable)
  if (!any(usable)) {
    cli::cli_abort(
      c("No row carries both a truth and an estimate.",
        "i" = "{nrow(x)} row{?s} seen, all with a missing label."),
      class = "sandtablR_error_triage_accuracy"
    )
  }

  meta <- list(
    truth = truth, estimate = estimate, by = by, levels = levels,
    n_input = nrow(x), n_used = sum(usable), n_dropped = n_dropped
  )
  if (is.null(by)) {
    block <- .triage_block(t_val[usable], e_val[usable])
    return(new_sdt_triage_accuracy(block$table, block$confusion, meta))
  }

  keys <- lapply(by, function(b) as.character(x[[b]])[usable])
  label <- do.call(paste, c(keys, list(sep = " | ")))
  order <- unique(label)
  parts <- split(seq_along(label), factor(label, levels = order))
  blocks <- lapply(parts, function(i) .triage_block(t_val[usable][i],
                                                    e_val[usable][i]))
  tables <- lapply(order, function(k) {
    tab <- blocks[[k]]$table
    first <- parts[[k]][[1L]]
    id <- lapply(keys, function(v) rep(v[[first]], nrow(tab)))
    names(id) <- by
    tibble::as_tibble(c(id, as.list(tab)))
  })
  confusion <- stats::setNames(lapply(order, function(k) blocks[[k]]$confusion),
                               order)
  new_sdt_triage_accuracy(do.call(rbind, tables), confusion, meta)
}

new_sdt_triage_accuracy <- function(x, confusion, meta) {
  tibble::new_tibble(
    x, nrow = nrow(x), class = "sdt_triage_accuracy",
    confusion = confusion, meta = meta
  )
}

#' @exportS3Method tibble::tbl_sum
tbl_sum.sdt_triage_accuracy <- function(x, ...) {
  meta <- attr(x, "meta")
  overall <- x[x$category == "overall", , drop = FALSE]
  head <- c(
    "Triage accuracy" = sprintf(
      "%s vs %s, n = %d", meta$truth, meta$estimate, meta$n_used
    )
  )
  if (is.null(meta$by) && nrow(overall) == 1L) {
    head <- c(head, "Overall" = sprintf(
      "hit %.3f  over %.3f  under %.3f  kappa %.3f",
      overall$hit_rate, overall$over_rate, overall$under_rate, overall$kappa
    ))
  }
  if (!is.null(meta$by)) {
    head <- c(head, "Split by" = paste(meta$by, collapse = ", "))
  }
  if (meta$n_dropped > 0L) {
    head <- c(head, "Unusable" = sprintf(
      "%d row(s) with a missing label", meta$n_dropped
    ))
  }
  head
}
