# Descriptives that state their denominator -----------------------------------
# The reporting half of the missingness policy `participants.R` sets out.
# That file reads the course register, normalises its free text and fills what
# it can defensibly fill; this file reports what is actually there, and it
# cannot be told to stop reporting it.
#
# Three rules hold everywhere below.
#
#   1. Every count states its denominator. A categorical level carries the
#      percent over answered rows AND the percent over every row, so a reader
#      never has to guess which of the two a share rests on.
#   2. Missingness is never optional. `na_report` moves it -- into its own
#      row, into a column, or into a footnote -- and nothing removes it.
#   3. An imputed value is never counted as an observed one. Wherever the
#      input carries indicators, filled values are tabulated separately and
#      both summaries are shown, so a descriptive computed after imputation
#      can never be mistaken for one computed on observed data.
#
# The constraint from `participants.R` carries over unchanged: the register
# links a person to a COURSE and has no group, team or "Gruppe" column, so a
# descriptive split by course is a between-course description, and a
# descriptive split by group is not available at all.

# Which raw field a normalised column was answered from. Extends the map
# `sdt_participant_summary()` uses with the columns it does not report, so a
# descriptive of `years_specialised` reads the missingness of the date
# somebody wrote down rather than the completeness of the arithmetic.
.describe_source <- function() {
  c(
    .summary_source(),
    specialised_since = "spezialisiert_seit",
    years_specialised = "spezialisiert_seit",
    training_years = "weiterbildungsjahre"
  )
}

# The indicator column describing one variable, under any of the naming
# conventions this package and its inputs use: the leading-dot form written by
# sdt_read_participants(), the trailing form written by
# sdt_course_composition(), and the plain prefix form. NA when there is none.
.describe_indicator <- function(nms, word, var, source = NULL) {
  cand <- c(paste0(".", word, "_", var),
            paste0(var, "_", word),
            paste0(word, "_", var))
  if (!is.null(source) && !is.na(source)) {
    cand <- c(cand, paste0(".", word, "_", source),
              paste0(source, "_", word))
  }
  hit <- intersect(cand, nms)
  if (length(hit)) hit[[1L]] else NA_character_
}

# Split a variable's rows into three disjoint, exhaustive states.
#
#   observed -- somebody answered, and the value rests on that answer.
#   imputed  -- the value is present, but it was filled rather than answered.
#   missing  -- there is no value, or the row's own indicator says the value
#               is not an observation and nothing says it was filled.
#
# The zero-denominator rule is what stops an unflagged imputation getting past
# this function. `sdt_course_composition(impute = "median")` fills a course
# value and deliberately does not set the flag, but it cannot make
# `n_<covariate>` anything other than zero, and a mean over zero participants
# is not an observation whatever a flag says. Those rows are counted as
# imputed and reported as unflagged.
.describe_states <- function(v, x, var, source = NULL) {
  n <- length(v)
  nms <- names(x)
  pick <- function(word) {
    col <- .describe_indicator(nms, word, var, source)
    if (is.na(col)) {
      return(rep(FALSE, n))
    }
    ind <- suppressWarnings(as.logical(x[[col]]))
    ind[is.na(ind)] <- FALSE
    ind
  }
  mis_ind <- pick("missing")
  imp_ind <- pick("imputed")
  n_col <- paste0("n_", var)
  zero <- if (n_col %in% nms && is.numeric(x[[n_col]])) {
    !is.na(x[[n_col]]) & x[[n_col]] == 0
  } else {
    rep(FALSE, n)
  }
  present <- !is.na(v)
  imputed <- present & (imp_ind | zero)
  missing <- !present | (mis_ind & !imputed)
  list(
    observed = present & !imputed & !missing,
    imputed = imputed,
    missing = missing,
    # Imputed, but nothing in the table admitted it.
    unflagged = imputed & !mis_ind & !imp_ind
  )
}

# Dates are summarised on their numeric scale and printed back as dates, so
# the tibble stays arithmetic and the report stays readable.
.describe_type <- function(v) {
  if (inherits(v, "Date")) {
    return("date")
  }
  if (is.numeric(v)) {
    return("numeric")
  }
  "categorical"
}

# A factor's declared vocabulary is reported in full, unused levels included:
# a level nobody chose is a finding. For everything else the levels are the
# values actually observed, so a value that only ever occurs in rows the
# indicators call missing is reported as missing rather than as a level.
.describe_levels <- function(v, keep) {
  if (is.factor(v)) {
    return(levels(v))
  }
  if (is.logical(v)) {
    return(c("FALSE", "TRUE"))
  }
  sort(unique(as.character(v[keep & !is.na(v)])))
}

.describe_stats <- function(z) {
  z <- z[!is.na(z)]
  if (!length(z)) {
    return(list(mean = NA_real_, sd = NA_real_, median = NA_real_,
                q25 = NA_real_, q75 = NA_real_, iqr = NA_real_,
                min = NA_real_, max = NA_real_))
  }
  q <- unname(stats::quantile(z, c(0.25, 0.75), names = FALSE))
  list(
    mean = mean(z),
    sd = if (length(z) > 1L) stats::sd(z) else NA_real_,
    median = stats::median(z),
    q25 = q[[1L]], q75 = q[[2L]], iqr = q[[2L]] - q[[1L]],
    min = min(z), max = max(z)
  )
}

# One source of truth for the column set, so every row -- level, statistic or
# missing -- has the same shape and the table can be rbound in one call.
.describe_template <- function(n) {
  chr <- rep(NA_character_, n)
  int <- rep(NA_integer_, n)
  dbl <- rep(NA_real_, n)
  tibble::tibble(
    variable = chr, type = chr, level = chr, missing_row = rep(FALSE, n),
    n = int, n_level_observed = int, n_level_imputed = int,
    n_observed = int, n_imputed = int, n_missing = int,
    n_valid = int, n_records = int,
    pct_observed = dbl, pct_valid = dbl, pct_total = dbl, pct_missing = dbl,
    mean = dbl, sd = dbl, median = dbl, q25 = dbl, q75 = dbl, iqr = dbl,
    min = dbl, max = dbl,
    mean_observed = dbl, sd_observed = dbl, median_observed = dbl
  )
}

# The leading dot on `.rows` is not decoration: every column of the template
# is passed by name, `n` among them, and a formal called `n` would swallow the
# row count by exact-name matching.
.describe_fill <- function(.rows, ...) {
  out <- .describe_template(.rows)
  dots <- list(...)
  for (nm in names(dots)) {
    val <- dots[[nm]]
    if (length(val) == 1L && .rows != 1L) val <- rep(val, .rows)
    out[[nm]] <- val
  }
  out
}

# The tidy numbers without the class or the attributes, for the renderers and
# for tibble::as_tibble().
.describe_bare <- function(x) {
  d <- unclass(x)
  attributes(d) <- list(names = names(x))
  tibble::as_tibble(d)
}

# The variables a table describes when the caller names none. The two classes
# this package produces get a curated set; anything else gets every column
# that is not itself an indicator for another one.
.describe_default_vars <- function(x, by) {
  if (inherits(x, "sdt_participants")) {
    want <- c("sex", "title_level", "specialty", "specialty_group",
              "seniority", "seniority_score", "years_specialised",
              "training_years")
    return(intersect(want, names(x)))
  }
  if (inherits(x, "sdt_course_composition")) {
    want <- c("n_participants", attr(x, "covariates"))
    return(intersect(want, names(x)))
  }
  nms <- names(x)
  stem <- sub("_(missing|imputed)$", "", nms)
  drop <- grepl("^\\.(missing|imputed)_", nms) |
    (grepl("_(missing|imputed)$", nms) & stem %in% nms) |
    nms %in% by
  keep <- nms[!drop]
  keep[vapply(x[keep], function(v) !is.list(v), logical(1))]
}

#' Describe a table without hiding what is missing
#'
#' A descriptive table over a `sdt_participants` register, a
#' `sdt_course_composition` table, or any data frame. Every variable reports
#' its counts, its missingness and the summary its type deserves -- counts and
#' percents for a categorical variable, mean, SD, median, IQR and range for a
#' numeric one -- optionally split `by` a grouping column.
#'
#' # Every percent states its denominator
#'
#' A categorical level carries three shares, and all three are in the returned
#' tibble whatever the report style:
#'
#' * `pct_observed` -- over answered rows only. Its denominator excludes both
#'   missing rows and filled ones.
#' * `pct_valid` -- over rows that carry a value. Its denominator excludes
#'   missing rows and includes filled ones.
#' * `pct_total` -- over every row in the group. Its denominator includes
#'   everything, missing rows included.
#'
#' `pct_valid` and `pct_total` are the two a reader usually means by "the
#' percent", and they differ exactly when something is missing. `pct_observed`
#' differs from `pct_valid` exactly when something was imputed.
#'
#' # Missingness cannot be switched off
#'
#' `na_report` decides **where** missingness is surfaced, never whether it is:
#'
#' * `"row"` -- each categorical variable gains a `(missing)` row, and each
#'   numeric variable a missing line of its own.
#' * `"column"` -- the missing count and percent appear beside every row.
#' * `"footnote"` -- they are collected under each block, complete variables
#'   named as such so silence is never ambiguous.
#'
#' `n_missing` and `pct_missing` are columns of the returned tibble in all
#' three cases.
#'
#' # Imputed values are flagged, not absorbed
#'
#' Where the input carries indicators -- `.missing_<field>` and
#' `.imputed_<field>` from [sdt_read_participants()], `<covariate>_missing`
#' and `n_<covariate>` from [sdt_course_composition()] -- each row is placed
#' in exactly one of three states: observed, imputed or missing. Imputed rows
#' are counted separately, never as observations, and numeric variables carry
#' `mean_observed`, `sd_observed` and `median_observed` alongside the
#' all-values statistics so both are visible at once.
#'
#' For a variable with a fill rule, `n_imputed + n_missing` is the number of
#' rows nobody answered: some of them were filled and some were not, and the
#' two are reported apart rather than added up for the reader. That is why
#' `title_level` reports no missing values and 99 imputed ones on the TDSC
#' register -- an empty title box is filled with `none`, and the fill is
#' visible in every share it touches.
#'
#' A value that is present in a row whose own missing indicator is set is not
#' an observation. It is counted as imputed when an imputation indicator or a
#' zero denominator (`n_<covariate>` of `0`) says it was filled, and as
#' missing otherwise. That zero-denominator rule is what catches
#' `sdt_course_composition(impute = "median")`, which fills without flagging:
#' the fill is reported as imputed anyway, and named as unflagged.
#'
#' # Level, not group
#'
#' The participant register has no group column, so a composition descriptive
#' split by course is a between-course description with as many points as
#' there are courses. There is no split by group to be had.
#'
#' @param x A `sdt_participants` object, a `sdt_course_composition` object, or
#'   any data frame.
#' @param by Optional single column name to split by. `NULL`, the default,
#'   pools every row. Rows whose grouping value is missing form their own
#'   group rather than disappearing.
#' @param vars Optional character vector of columns to describe. Defaults to a
#'   curated set for the two classes this package produces, and to every
#'   non-indicator column otherwise.
#' @param na_report Where to surface missingness: `"row"`, `"column"` or
#'   `"footnote"`. It cannot be switched off.
#' @param digits Decimal places for the printed statistics. Percentages use
#'   one fewer, since a percent already carries two significant figures more
#'   than the proportion it comes from. The returned tibble is never rounded.
#'
#' @return A `sdt_describe` object: a [tibble::tibble()] subclass with one row
#'   per group, variable and level. Two families of counts share every row and
#'   are named apart so they cannot be confused. `n`, `n_level_observed` and
#'   `n_level_imputed` belong to the row -- to one level of one categorical
#'   variable, or to the values of one numeric variable. `n_observed`,
#'   `n_imputed`, `n_missing`, `n_valid` and `n_records` belong to the
#'   variable, and are repeated on each of its rows, so that
#'   `n_observed + n_imputed + n_missing == n_records` holds on every row of
#'   the table. The three percents, `pct_missing` and the numeric statistics
#'   complete it. [tibble::as_tibble()] returns it bare;
#'   [sdt_describe_render()] returns the display-ready shape.
#' @seealso [sdt_describe_participants()] for the participant defaults,
#'   [sdt_participant_summary()] for the participant-only summary this
#'   generalises, [sdt_course_composition()] for the imputing half of the
#'   policy.
#' @export
#' @examples
#' d <- data.frame(
#'   grade = c("consultant", "resident", "consultant", NA),
#'   years = c(11, 2, 7, NA),
#'   .missing_grade = c(FALSE, FALSE, FALSE, TRUE),
#'   .imputed_grade = c(FALSE, FALSE, FALSE, FALSE)
#' )
#' sdt_describe(d, vars = c("grade", "years"))
#' tibble::as_tibble(sdt_describe(d, vars = "grade"))
sdt_describe <- function(x, by = NULL, vars = NULL,
                         na_report = c("row", "column", "footnote"),
                         digits = 2) {
  if (!is.data.frame(x)) {
    cli::cli_abort(
      c("{.arg x} must be a data frame.",
        "x" = "Got {.cls {class(x)[[1]]}} instead."),
      class = "sandtablR_error_describe"
    )
  }
  na_report <- rlang::arg_match(na_report)
  ok <- is.numeric(digits) && length(digits) == 1L && !is.na(digits) &&
    digits >= 0 && digits == round(digits)
  if (!ok) {
    cli::cli_abort(
      "{.arg digits} must be a single non-negative whole number.",
      class = "sandtablR_error_describe"
    )
  }
  digits <- as.integer(digits)
  if (!is.null(by)) {
    .check_string(by)
    if (!by %in% names(x)) {
      cli::cli_abort(
        c("{.arg by} must name a column of {.arg x}.",
          "x" = "{.field {by}} is not one."),
        class = "sandtablR_error_describe"
      )
    }
  }
  vars <- vars %||% .describe_default_vars(x, by)
  if (!is.character(vars) || !length(vars)) {
    cli::cli_abort(
      c("{.arg vars} must name at least one column.",
        "i" = "There was nothing left to describe."),
      class = "sandtablR_error_describe"
    )
  }
  unknown <- setdiff(vars, names(x))
  if (length(unknown)) {
    n_unknown <- length(unknown)
    cli::cli_abort(
      c("{.arg vars} names {n_unknown} column{?s} {.arg x} does not have.",
        "x" = "Unknown: {.field {unknown}}."),
      class = "sandtablR_error_describe"
    )
  }
  bad <- vars[vapply(x[vars], is.list, logical(1))]
  if (length(bad)) {
    n_bad <- length(bad)
    cli::cli_abort(
      c("{.arg vars} names {n_bad} list column{?s}, which cannot be
         summarised.",
        "x" = "Offending: {.field {bad}}."),
      class = "sandtablR_error_describe"
    )
  }

  groups <- .describe_groups(x, by)
  src <- .describe_source()
  states <- list()
  types <- character()
  levels_of <- list()
  for (v in vars) {
    col <- x[[v]]
    source <- if (v %in% names(src)) src[[v]] else NULL
    st <- .describe_states(col, x, v, source)
    states[[v]] <- st
    types[[v]] <- .describe_type(col)
    levels_of[[v]] <- .describe_levels(col, !st$missing)
  }

  rows <- list()
  for (g in names(groups)) {
    idx <- groups[[g]]
    for (v in vars) {
      block <- .describe_block(
        x[[v]], states[[v]], idx, v, types[[v]], levels_of[[v]], na_report
      )
      block$.group <- g
      rows[[length(rows) + 1L]] <- block
    }
  }
  out <- if (length(rows)) {
    do.call(rbind, rows)
  } else {
    empty <- .describe_template(0L)
    empty$.group <- character()
    empty
  }
  out <- out[, c(".group", setdiff(names(out), ".group")), drop = FALSE]
  names(out)[[1L]] <- by %||% "group"

  unflagged <- vapply(
    states, function(s) sum(s$unflagged), integer(1)
  )
  tibble::new_tibble(
    out, nrow = nrow(out), class = "sdt_describe",
    by = by, vars = vars, na_report = na_report, digits = digits,
    group_levels = names(groups),
    n_records = vapply(groups, length, integer(1)),
    unflagged = unflagged[unflagged > 0L],
    source_class = class(x)[[1L]],
    note = character()
  )
}

# Rows whose grouping value is missing become their own group. Dropping them
# would be the one silent deletion this module exists to prevent.
.describe_groups <- function(x, by) {
  if (is.null(by)) {
    return(list(`(all)` = seq_len(nrow(x))))
  }
  key <- x[[by]]
  f <- factor(key)
  if (anyNA(key)) {
    f <- addNA(f)
    levels(f)[is.na(levels(f))] <- "(missing group)"
  }
  split(seq_len(nrow(x)), f)
}

# One variable in one group: the level rows or the statistic row, plus the
# missing row when the report style asks for one.
.describe_block <- function(v, st, idx, var, type, lv, na_report) {
  n_rec <- length(idx)
  obs <- st$observed[idx]
  imp <- st$imputed[idx]
  mis <- st$missing[idx]
  n_obs <- sum(obs)
  n_imp <- sum(imp)
  n_mis <- sum(mis)
  n_val <- n_obs + n_imp
  pct_mis <- if (n_rec) n_mis / n_rec else NA_real_
  vg <- v[idx]

  if (identical(type, "categorical")) {
    fv <- factor(as.character(vg), levels = lv)
    fv[mis] <- NA
    n_lvl <- as.integer(table(fv))
    n_lvl_imp <- as.integer(table(fv[imp]))
    n_lvl_obs <- n_lvl - n_lvl_imp
    out <- .describe_fill(
      length(lv),
      variable = var, type = type, level = lv,
      n = n_lvl, n_level_observed = n_lvl_obs, n_level_imputed = n_lvl_imp,
      n_observed = n_obs, n_imputed = n_imp,
      n_missing = n_mis, n_valid = n_val, n_records = n_rec,
      pct_observed = if (n_obs) n_lvl_obs / n_obs else NA_real_,
      pct_valid = if (n_val) n_lvl / n_val else NA_real_,
      pct_total = if (n_rec) n_lvl / n_rec else NA_real_,
      pct_missing = pct_mis
    )
    if (identical(na_report, "row")) {
      out <- rbind(out, .describe_fill(
        1L,
        variable = var, type = type, level = "(missing)", missing_row = TRUE,
        n = n_mis, n_level_observed = 0L, n_level_imputed = 0L,
        n_observed = n_obs, n_imputed = n_imp, n_missing = n_mis,
        n_valid = n_val, n_records = n_rec,
        pct_total = pct_mis, pct_missing = pct_mis
      ))
    }
    return(out)
  }

  num <- as.numeric(vg)
  all_stats <- .describe_stats(num[!mis])
  obs_stats <- .describe_stats(num[obs])
  .describe_fill(
    1L,
    variable = var, type = type,
    n = n_val, n_level_observed = n_obs, n_level_imputed = n_imp,
    n_observed = n_obs, n_imputed = n_imp, n_missing = n_mis,
    n_valid = n_val, n_records = n_rec, pct_missing = pct_mis,
    mean = all_stats$mean, sd = all_stats$sd, median = all_stats$median,
    q25 = all_stats$q25, q75 = all_stats$q75, iqr = all_stats$iqr,
    min = all_stats$min, max = all_stats$max,
    mean_observed = obs_stats$mean, sd_observed = obs_stats$sd,
    median_observed = obs_stats$median
  )
}

#' Describe a participant register
#'
#' [sdt_describe()] with the defaults a `sdt_participants` register wants:
#' split by course, and reporting the normalised covariates rather than the
#' raw German free text they were derived from.
#'
#' Courses with no linked participants have no rows in the register, so they
#' cannot appear as groups. They are named in the printed header instead,
#' read from the `course_index` attribute, so their absence is stated rather
#' than left for the reader to notice.
#'
#' @param x A `sdt_participants` object from [sdt_read_participants()].
#' @param by Column to split by. Defaults to `"short_course_id"`; `NULL`
#'   pools the whole register.
#' @param ... Passed to [sdt_describe()], including `vars`, `na_report` and
#'   `digits`.
#'
#' @return A `sdt_describe` object.
#' @seealso [sdt_describe()], [sdt_read_participants()].
#' @export
#' @examples
#' \dontrun{
#' p <- sdt_read_participants("TDSC_course_participant_relations.xlsx")
#' sdt_describe_participants(p)
#' sdt_describe_participants(p, by = NULL, na_report = "footnote")
#' }
sdt_describe_participants <- function(x, by = "short_course_id", ...) {
  .check_class(x, "sdt_participants")
  out <- sdt_describe(x, by = by, ...)
  idx <- attr(x, "course_index")
  note <- character()
  if (!is.null(idx) && !is.null(by) && by %in% names(idx)) {
    empty <- idx[[by]][idx$sheet_read & !idx$has_participants]
    if (length(empty)) {
      note <- sprintf(
        paste("%d of %d courses have no participant rows and cannot",
              "appear as groups: %s."),
        length(empty), nrow(idx), paste(empty, collapse = ", ")
      )
    }
  }
  note <- c(
    note,
    paste("Composition is a course-level covariate: the register has no",
          "group column.")
  )
  attr(out, "note") <- note
  out
}

# Rendering -------------------------------------------------------------------

.describe_s <- function(n) if (identical(as.integer(n), 1L)) "" else "s"

.describe_num <- function(v, digits) {
  ifelse(is.na(v), "-", formatC(v, format = "f", digits = digits))
}

.describe_pct <- function(v, digits) {
  d <- max(0L, digits - 1L)
  ifelse(is.na(v), "-", formatC(100 * v, format = "f", digits = d))
}

# Dates were summarised on their numeric scale; put them back.
.describe_stat_chr <- function(v, type, digits) {
  if (identical(type, "date")) {
    return(ifelse(is.na(v), "-",
                  format(as.Date(v, origin = "1970-01-01"), "%Y-%m-%d")))
  }
  .describe_num(v, digits)
}

# A spread is a duration, not a date, so it keeps its unit rather than being
# printed as a bare number beside three calendar dates.
.describe_spread_chr <- function(v, type, digits) {
  out <- .describe_num(v, digits)
  if (identical(type, "date")) {
    out <- ifelse(is.na(v), out, paste(out, "days"))
  }
  out
}

.describe_clip <- function(s, width) {
  ifelse(nchar(s) > width, paste0(substr(s, 1L, width - 1L), "\u2026"), s)
}

.describe_cols <- function(show_obs, show_miss) {
  cols <- c("n", "imp", "val%", "all%")
  if (show_obs) cols <- c(cols[1:2], "obs%", cols[3:4])
  if (show_miss) cols <- c(cols, "miss", "mis%")
  cols
}

# One variable's block, for one group.
.describe_lines_cat <- function(b, digits, na_report, max_levels) {
  show_obs <- any(b$n_imputed > 0L, na.rm = TRUE)
  show_miss <- identical(na_report, "column")
  cols <- .describe_cols(show_obs, show_miss)
  head <- sprintf("  %-20s%s", .describe_clip(b$variable[[1L]], 20L),
                  paste0(sprintf("%8s", cols), collapse = ""))
  keep <- seq_len(nrow(b))
  hidden <- 0L
  if (nrow(b) > max_levels) {
    last <- nrow(b)
    keep <- unique(c(seq_len(max_levels - 1L),
                     if (b$missing_row[[last]]) last))
    hidden <- nrow(b) - length(keep)
  }
  body <- character()
  for (i in keep) {
    vals <- c(
      format(b$n[[i]]),
      if (b$missing_row[[i]]) "-" else format(b$n_level_imputed[[i]])
    )
    if (show_obs) {
      vals <- c(vals, if (b$missing_row[[i]]) "-" else {
        .describe_pct(b$pct_observed[[i]], digits)
      })
    }
    vals <- c(
      vals,
      if (b$missing_row[[i]]) "-" else .describe_pct(b$pct_valid[[i]], digits),
      .describe_pct(b$pct_total[[i]], digits)
    )
    if (show_miss) {
      vals <- c(vals, format(b$n_missing[[i]]),
                .describe_pct(b$pct_missing[[i]], digits))
    }
    body <- c(body, sprintf(
      "    %-18s%s", .describe_clip(b$level[[i]], 18L),
      paste0(sprintf("%8s", vals), collapse = "")
    ))
  }
  if (hidden > 0L) {
    body <- c(body, sprintf("    ... %d more level%s", hidden,
                            .describe_s(hidden)))
  }
  c(head, body)
}

.describe_lines_num <- function(b, digits, na_report) {
  type <- b$type[[1L]]
  f <- function(v) .describe_stat_chr(v, type, digits)
  counts <- sprintf("    n %d   imputed %d", b$n[[1L]], b$n_imputed[[1L]])
  if (identical(na_report, "column")) {
    counts <- sprintf("%s   missing %d (%s%%)", counts, b$n_missing[[1L]],
                      .describe_pct(b$pct_missing[[1L]], digits))
  }
  out <- c(sprintf("  %s", b$variable[[1L]]), counts)
  if (identical(na_report, "row")) {
    out <- c(out, sprintf("    missing %d of %d (%s%%)", b$n_missing[[1L]],
                          b$n_records[[1L]],
                          .describe_pct(b$pct_missing[[1L]], digits)))
  }
  centre <- sprintf("    mean %s (SD %s)", f(b$mean[[1L]]),
                    .describe_spread_chr(b$sd[[1L]], type, digits))
  spread <- sprintf("median %s [%s, %s]", f(b$median[[1L]]),
                    f(b$q25[[1L]]), f(b$q75[[1L]]))
  joined <- paste0(centre, "   ", spread)
  out <- c(out, if (nchar(joined) <= 78L) {
    joined
  } else {
    c(centre, paste0("    ", spread))
  })
  out <- c(out, sprintf("    range %s to %s", f(b$min[[1L]]),
                        f(b$max[[1L]])))
  if (isTRUE(b$n_imputed[[1L]] > 0L)) {
    out <- c(out, sprintf(
      "    observed only (n %d): mean %s (SD %s)   median %s",
      b$n_observed[[1L]], f(b$mean_observed[[1L]]),
      .describe_spread_chr(b$sd_observed[[1L]], type, digits),
      f(b$median_observed[[1L]])
    ))
  }
  out
}

.describe_footnote <- function(block, digits) {
  first <- !duplicated(block$variable)
  miss <- block[first & block$n_missing > 0L, , drop = FALSE]
  full <- block$variable[first & block$n_missing == 0L]
  out <- character()
  if (nrow(miss)) {
    out <- c(out, paste0(
      "Missing: ",
      paste(sprintf("%s %d (%s%%)", miss$variable, miss$n_missing,
                    .describe_pct(miss$pct_missing, digits)),
            collapse = ", ")
    ))
  }
  if (length(full)) {
    out <- c(out, paste0("Complete: ", paste(full, collapse = ", "), "."))
  }
  strwrap(out, width = 78, indent = 2L, exdent = 4L)
}

#' @param ... These dots are for future extensions and must be empty.
#' @param max_groups Groups to print before the rest are summarised in one
#'   line. The object keeps every group either way.
#' @param max_levels Levels to print per categorical variable.
#' @rdname sdt_describe
#' @export
format.sdt_describe <- function(x, ..., max_groups = 8L, max_levels = 12L) {
  rlang::check_dots_empty()
  by <- attr(x, "by")
  digits <- attr(x, "digits")
  na_report <- attr(x, "na_report")
  vars <- attr(x, "vars")
  nrec <- attr(x, "n_records")
  glev <- attr(x, "group_levels")
  n_var <- length(vars)
  n_rec <- sum(nrec)
  out <- sprintf(
    "<sdt_describe> %d variable%s over %d record%s%s",
    n_var, .describe_s(n_var), n_rec, .describe_s(n_rec),
    if (is.null(by)) {
      ""
    } else {
      sprintf(" in %d group%s (%s)", length(glev), .describe_s(length(glev)),
              by)
    }
  )
  out <- c(out, paste(
    "Denominators: val% excludes missing, all% includes it;",
    "obs% excludes fills too."
  ))
  out <- c(out, sprintf(
    "Missingness: reported as %s, and never suppressed.",
    switch(na_report, row = "its own row", column = "a column",
           footnote = "a footnote")
  ))
  d <- .describe_bare(x)
  real <- !d$missing_row
  n_imp <- sum(d$n_level_imputed[real], na.rm = TRUE)
  if (n_imp > 0L) {
    hit <- unique(d$variable[real & d$n_level_imputed > 0L])
    out <- c(out, strwrap(sprintf(
      paste("Imputed: %d value%s in %s %s filled rather than answered,",
            "and never counted as observed."),
      n_imp, .describe_s(n_imp), paste(hit, collapse = ", "),
      if (n_imp == 1L) "was" else "were"
    ), width = 78, exdent = 2L))
  }
  unflagged <- attr(x, "unflagged")
  if (length(unflagged)) {
    out <- c(out, strwrap(sprintf(
      paste("! %d value%s in %s %s imputed but not flagged as such;",
            "detected from a zero denominator."),
      sum(unflagged), .describe_s(sum(unflagged)),
      paste(names(unflagged), collapse = ", "),
      if (sum(unflagged) == 1L) "is" else "are"
    ), width = 78, exdent = 2L))
  }
  for (n in attr(x, "note")) {
    out <- c(out, strwrap(paste0("i ", n), width = 78, exdent = 2L))
  }

  shown <- utils::head(glev, max_groups)
  for (g in shown) {
    block <- d[d[[1L]] == g, , drop = FALSE]
    if (!is.null(by)) {
      label <- sprintf("-- %s (n = %d) ", g, nrec[[g]])
      pad <- max(0L, 72L - nchar(label))
      out <- c(out, "", paste0(label, strrep("-", pad)))
    } else {
      out <- c(out, "")
    }
    for (v in vars) {
      b <- block[block$variable == v, , drop = FALSE]
      if (!nrow(b)) next
      out <- c(out, if (identical(b$type[[1L]], "categorical")) {
        .describe_lines_cat(b, digits, na_report, max_levels)
      } else {
        .describe_lines_num(b, digits, na_report)
      })
    }
    if (identical(na_report, "footnote")) {
      out <- c(out, .describe_footnote(block, digits))
    }
  }
  hidden <- length(glev) - length(shown)
  if (hidden > 0L) {
    out <- c(out, "", sprintf(
      "... %d more group%s; raise max_groups or use tibble::as_tibble().",
      hidden, .describe_s(hidden)
    ))
  }
  out
}

#' @rdname sdt_describe
#' @export
print.sdt_describe <- function(x, ..., max_groups = 8L, max_levels = 12L) {
  cat(format(x, max_groups = max_groups, max_levels = max_levels), sep = "\n")
  invisible(x)
}

#' @exportS3Method tibble::as_tibble
as_tibble.sdt_describe <- function(x, ...) {
  .describe_bare(x)
}

#' The display-ready shape of a descriptive table
#'
#' Renders a [sdt_describe()] object as a flat table of character columns,
#' one row per level or statistic, ready for [knitr::kable()], `DT` or any
#' other table renderer. The numbers are formatted with the object's `digits`;
#' the object itself keeps them unrounded.
#'
#' Missingness follows the object's `na_report`: a `(missing)` row, a pair of
#' `missing` columns, or a footnote. The legend naming each percent's
#' denominator is always the first footnote, so a rendered table can never
#' arrive without one.
#'
#' @param x A `sdt_describe` object.
#'
#' @return A [tibble::tibble()] of character columns with a `footnotes`
#'   attribute.
#' @seealso [sdt_describe()].
#' @export
#' @examples
#' d <- data.frame(grade = c("consultant", "resident", NA), years = c(9, 3, 5))
#' sdt_describe_render(sdt_describe(d))
sdt_describe_render <- function(x) {
  .check_class(x, "sdt_describe")
  digits <- attr(x, "digits")
  na_report <- attr(x, "na_report")
  by <- attr(x, "by")
  d <- .describe_bare(x)
  show_obs <- any(d$n_imputed > 0L, na.rm = TRUE)
  show_miss <- identical(na_report, "column")
  rows <- list()
  for (i in seq_len(nrow(d))) {
    rows[[length(rows) + 1L]] <- .describe_render_rows(
      d[i, , drop = FALSE], digits, na_report, show_obs, show_miss
    )
  }
  out <- if (length(rows)) {
    do.call(rbind, rows)
  } else {
    .describe_render_rows(NULL, digits, na_report, show_obs, show_miss)
  }
  out <- cbind(group = rep(d[[1L]], vapply(rows, nrow, integer(1))), out)
  names(out)[[1L]] <- by %||% "group"
  if (is.null(by)) out[[1L]] <- NULL
  foot <- c(
    paste("Percent denominators: val% over rows with a value,",
          "all% over every row,",
          "obs% over answered rows only (fills excluded)."),
    attr(x, "note")
  )
  if (identical(na_report, "footnote")) {
    foot <- c(foot, .describe_footnote(d, digits))
  }
  real <- !d$missing_row
  n_imp <- sum(d$n_level_imputed[real], na.rm = TRUE)
  if (n_imp > 0L) {
    foot <- c(foot, sprintf(
      "Imputed: %d value%s filled rather than answered, counted apart.",
      n_imp, .describe_s(n_imp)
    ))
  }
  if (length(attr(x, "unflagged"))) {
    foot <- c(foot, sprintf(
      "Imputed but not flagged: %s.",
      paste(sprintf("%s (%d)", names(attr(x, "unflagged")),
                    attr(x, "unflagged")), collapse = ", ")
    ))
  }
  structure(tibble::as_tibble(out), footnotes = trimws(foot))
}

# One row of the tidy table becomes one row (categorical) or several
# (numeric), so that `what` always names a level or a statistic and `value`
# always carries the summary.
.describe_render_rows <- function(r, digits, na_report, show_obs,
                                  show_miss) {
  cols <- c("variable", "what", "value", "n", "imputed")
  if (show_obs) cols <- c(cols, "obs%")
  cols <- c(cols, "val%", "all%")
  if (show_miss) cols <- c(cols, "missing", "mis%")
  empty <- stats::setNames(
    rep(list(character()), length(cols)), cols
  )
  if (is.null(r)) {
    return(as.data.frame(empty, check.names = FALSE,
                         stringsAsFactors = FALSE))
  }
  mk <- function(what, value, n = "", imputed = "", obs = "", val = "",
                 all = "") {
    d <- data.frame(
      variable = rep(r$variable, length(what)), what = what, value = value,
      n = n, imputed = imputed, stringsAsFactors = FALSE
    )
    if (show_obs) d[["obs%"]] <- obs
    d[["val%"]] <- val
    d[["all%"]] <- all
    if (show_miss) {
      d[["missing"]] <- as.character(r$n_missing)
      d[["mis%"]] <- .describe_pct(r$pct_missing, digits)
    }
    d
  }
  if (identical(r$type, "categorical")) {
    if (isTRUE(r$missing_row)) {
      return(mk("(missing)", sprintf("%d (%s%%)", r$n,
                                     .describe_pct(r$pct_total, digits)),
                n = as.character(r$n), imputed = "-", obs = "-", val = "-",
                all = .describe_pct(r$pct_total, digits)))
    }
    return(mk(
      r$level,
      sprintf("%d (%s%%)", r$n, .describe_pct(r$pct_valid, digits)),
      n = as.character(r$n), imputed = as.character(r$n_level_imputed),
      obs = .describe_pct(r$pct_observed, digits),
      val = .describe_pct(r$pct_valid, digits),
      all = .describe_pct(r$pct_total, digits)
    ))
  }
  f <- function(v) .describe_stat_chr(v, r$type, digits)
  spread <- function(v) .describe_spread_chr(v, r$type, digits)
  what <- c("mean (SD)", "median [IQR]", "range")
  value <- c(
    sprintf("%s (%s)", f(r$mean), spread(r$sd)),
    sprintf("%s [%s, %s]", f(r$median), f(r$q25), f(r$q75)),
    sprintf("%s to %s", f(r$min), f(r$max))
  )
  if (isTRUE(r$n_imputed > 0L)) {
    what <- c(what, "observed only, mean (SD)")
    value <- c(value, sprintf("%s (%s)", f(r$mean_observed),
                              spread(r$sd_observed)))
  }
  if (identical(na_report, "row")) {
    what <- c(what, "(missing)")
    value <- c(value, sprintf("%d (%s%%)", r$n_missing,
                              .describe_pct(r$pct_missing, digits)))
  }
  mk(what, value,
     n = c(as.character(r$n), rep("", length(what) - 1L)),
     imputed = c(as.character(r$n_imputed), rep("", length(what) - 1L)))
}

#' @exportS3Method knitr::knit_print
knit_print.sdt_describe <- function(x, ...) {
  .require_pkgs("knitr")
  tab <- sdt_describe_render(x)
  foot <- attr(tab, "footnotes")
  txt <- c(
    knitr::kable(tab, format = "pipe"), "",
    paste0("*", foot, "*")
  )
  knitr::asis_output(paste(txt, collapse = "\n"))
}
