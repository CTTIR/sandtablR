# Descriptives that state their denominator.
#
# Every fixture here is synthetic. The real register is personal data and
# lives outside the repository, so the shapes that matter -- a blank box
# filled and flagged, a declared non-answer, a course with nobody in it, and
# an imputation that was deliberately not flagged -- are all reconstructed
# from scratch.

# S3 registration lives in NAMESPACE, which roxygen2 regenerates; call the
# methods by name so these tests hold whether or not the file has been
# rebuilt yet. Dispatch itself is asserted once, at the bottom.
fmt <- function(x, ...) format.sdt_describe(x, ...)

toy <- function() {
  data.frame(
    grade = c("consultant", "resident", "consultant", NA, "chief"),
    title = c("none", "doctorate", "none", "doctorate", "none"),
    years = c(11, 2, 7, NA, 20),
    site = c("A", "A", "B", "B", "A"),
    .missing_grade = c(FALSE, FALSE, FALSE, TRUE, FALSE),
    .imputed_grade = rep(FALSE, 5L),
    .missing_title = c(TRUE, FALSE, TRUE, FALSE, FALSE),
    .imputed_title = c(TRUE, FALSE, TRUE, FALSE, FALSE),
    .missing_years = c(FALSE, FALSE, FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
}

test_that("the three row states are disjoint and exhaustive", {
  x <- sdt_describe(toy(), vars = c("grade", "title", "years"))
  # The variable-level trio is exhaustive on every row of the table.
  expect_identical(
    x$n_observed + x$n_imputed + x$n_missing,
    x$n_records
  )
  # The level-level pair splits that row's own count.
  lev <- x[!x$missing_row, , drop = FALSE]
  expect_identical(lev$n, lev$n_level_observed + lev$n_level_imputed)
  # And the levels of a categorical variable add up to its valid count.
  cat_lev <- lev[lev$type == "categorical", , drop = FALSE]
  totals <- tapply(cat_lev$n, cat_lev$variable, sum)
  valid <- tapply(cat_lev$n_valid, cat_lev$variable, unique)
  expect_identical(as.integer(totals), as.integer(valid))
})

test_that("both denominators are reported and they differ on missingness", {
  x <- sdt_describe(toy(), vars = "grade")
  lev <- x[!x$missing_row, , drop = FALSE]
  expect_identical(sum(lev$n), 4L)
  # Four answered rows of five records.
  expect_equal(lev$pct_valid, lev$n / 4)
  expect_equal(lev$pct_total, lev$n / 5)
  expect_false(isTRUE(all.equal(lev$pct_valid, lev$pct_total)))
  expect_true(all(x$n_missing == 1L))
  expect_equal(unique(x$pct_missing), 0.2)
})

test_that("an imputed value is never counted as an observed one", {
  x <- sdt_describe(toy(), vars = "title")
  none <- x[x$level == "none", , drop = FALSE]
  expect_identical(none$n, 3L)
  expect_identical(none$n_level_imputed, 2L)
  expect_identical(none$n_level_observed, 1L)
  expect_identical(none$n_imputed, 2L)
  expect_identical(none$n_observed, 3L)
  # 1 of the 3 answers actually given, but 3 of the 5 values present.
  expect_equal(none$pct_observed, 1 / 3)
  expect_equal(none$pct_valid, 3 / 5)
  expect_false(isTRUE(all.equal(none$pct_observed, none$pct_valid)))
  # An imputed row is not missing: it has a value, and the value is flagged.
  expect_identical(unique(x$n_missing), 0L)
})

test_that("numeric summaries carry an observed-only twin", {
  d <- toy()
  d$score <- c(1, 2, 3, 4, 100)
  d$.imputed_score <- c(FALSE, FALSE, FALSE, FALSE, TRUE)
  x <- sdt_describe(d, vars = "score")
  expect_identical(x$type, "numeric")
  expect_equal(x$mean, mean(c(1, 2, 3, 4, 100)))
  expect_equal(x$mean_observed, mean(c(1, 2, 3, 4)))
  expect_equal(x$median, 3)
  expect_equal(x$q25, 2)
  expect_equal(x$q75, 4)
  expect_equal(x$iqr, 2)
  expect_equal(x$min, 1)
  expect_equal(x$max, 100)
  expect_identical(x$n_imputed, 1L)
  expect_identical(x$n_observed, 4L)
})

test_that("missingness moves between report styles but never leaves", {
  styles <- c("row", "column", "footnote")
  tabs <- lapply(styles, function(s) {
    sdt_describe(toy(), vars = c("grade", "years"), na_report = s)
  })
  for (tab in tabs) {
    expect_true(all(c("n_missing", "pct_missing") %in% names(tab)))
    expect_identical(unique(tab$n_missing), 1L)
  }
  # Only the row style materialises a (missing) row.
  expect_identical(sum(tabs[[1L]]$missing_row), 1L)
  expect_identical(sum(tabs[[2L]]$missing_row), 0L)
  expect_identical(sum(tabs[[3L]]$missing_row), 0L)
  # The numbers themselves are identical whichever style is chosen.
  drop_missing_row <- function(t) t[!t$missing_row, "n", drop = TRUE]
  expect_identical(drop_missing_row(tabs[[1L]]), drop_missing_row(tabs[[2L]]))
  # Every style names the missingness in print.
  for (tab in tabs) {
    expect_match(paste(fmt(tab), collapse = " "), "never suppressed")
  }
})

test_that("grouping splits the records and never drops a row", {
  x <- sdt_describe(toy(), by = "site", vars = "grade")
  expect_identical(names(x)[[1L]], "site")
  expect_identical(sum(x$n[!x$missing_row]), 4L)
  expect_identical(sort(unique(attr(x, "n_records"))), c(2L, 3L))

  d <- toy()
  d$site[[1L]] <- NA
  y <- sdt_describe(d, by = "site", vars = "grade")
  expect_true("(missing group)" %in% attr(y, "group_levels"))
  expect_identical(sum(attr(y, "n_records")), 5L)
})

test_that("levels seen only in missing rows are reported as missing", {
  d <- data.frame(
    funktion = c("Oberarzt", "Nichts angegeben", "Oberarzt"),
    .missing_funktion = c(FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  x <- sdt_describe(d, vars = "funktion")
  expect_false("Nichts angegeben" %in% x$level)
  expect_identical(x$n_missing[[1L]], 1L)
  expect_identical(x$n[x$level == "Oberarzt"], 2L)
})

test_that("a declared factor level with no rows is still reported", {
  d <- data.frame(g = factor(c("a", "a"), levels = c("a", "b")))
  x <- sdt_describe(d, vars = "g", na_report = "column")
  expect_identical(x$level, c("a", "b"))
  expect_identical(x$n, c(2L, 0L))
})

test_that("a zero denominator exposes an imputation nobody flagged", {
  d <- data.frame(share = c(0.5, 0.5, 0.3), n_share = c(4L, 2L, 0L))
  x <- sdt_describe(d, vars = "share")
  expect_identical(x$n_imputed, 1L)
  expect_identical(x$n_observed, 2L)
  expect_identical(unname(attr(x, "unflagged")), 1L)
  expect_match(paste(fmt(x), collapse = " "), "not flagged")
})

test_that("every value missing is summarised, not an error", {
  d <- data.frame(g = c(NA_character_, NA_character_), v = c(NA_real_, NA))
  x <- sdt_describe(d, vars = c("g", "v"))
  expect_identical(x$n_missing, c(2L, 2L))
  expect_true(all(is.na(x$mean)))
  expect_equal(unique(x$pct_missing), 1)
})

test_that("the arguments are checked and every offender named at once", {
  d <- toy()
  expect_error(sdt_describe(1:5), class = "sandtablR_error_describe")
  expect_error(sdt_describe(d, vars = c("nope", "also_nope")),
               class = "sandtablR_error_describe")
  expect_error(sdt_describe(d, vars = c("nope", "also_nope")), "also_nope")
  expect_error(sdt_describe(d, by = "nope"),
               class = "sandtablR_error_describe")
  expect_error(sdt_describe(d, vars = "grade", digits = -1),
               class = "sandtablR_error_describe")
  d$lst <- I(replicate(5L, 1:2, simplify = FALSE))
  expect_error(sdt_describe(d, vars = "lst"),
               class = "sandtablR_error_describe")
  expect_error(sdt_describe(d, vars = "grade", na_report = "off"))
})

test_that("printing states the denominators and returns invisibly", {
  x <- sdt_describe(toy(), vars = c("title", "years"))
  txt <- paste(fmt(x), collapse = "\n")
  expect_match(txt, "val% excludes missing", fixed = TRUE)
  expect_match(txt, "all% includes it", fixed = TRUE)
  expect_match(txt, "obs%", fixed = TRUE)
  expect_match(txt, "filled rather than answered")
  expect_output(out <- withVisible(print.sdt_describe(x)))
  expect_false(out$visible)
})

test_that("large tables are truncated in print, never in the object", {
  d <- data.frame(g = letters[1:20], s = rep(c("x", "y"), 10L))
  x <- sdt_describe(d, by = "s", vars = "g")
  expect_identical(nrow(x), 2L * 21L)
  txt <- paste(fmt(x, max_groups = 1L, max_levels = 5L),
               collapse = "\n")
  expect_match(txt, "more level")
  expect_match(txt, "more group")
})

test_that("the rendered shape is flat, character and footnoted", {
  x <- sdt_describe(toy(), vars = c("title", "years"))
  r <- sdt_describe_render(x)
  expect_s3_class(r, "tbl_df")
  expect_true(all(vapply(r, is.character, logical(1))))
  expect_true(all(c("variable", "what", "value", "n") %in% names(r)))
  expect_match(paste(attr(r, "footnotes"), collapse = " "),
               "Percent denominators")
  expect_match(paste(attr(r, "footnotes"), collapse = " "), "Imputed")
  # The statistic rows of a numeric variable are named, not positional.
  expect_true(all(c("mean (SD)", "median [IQR]", "range") %in% r$what))
})

test_that("as_tibble returns the numbers without the class", {
  x <- sdt_describe(toy(), vars = "grade")
  bare <- as_tibble.sdt_describe(x)
  expect_false(inherits(bare, "sdt_describe"))
  expect_null(attr(bare, "na_report"))
  expect_identical(nrow(bare), nrow(x))
})

test_that("the methods dispatch once NAMESPACE knows about them", {
  x <- sdt_describe(toy(), vars = "grade")
  env <- list2env(list(x = x), parent = asNamespace("sandtablR"))
  expect_match(evalq(format(x)[[1L]], env), "<sdt_describe>")
  expect_false(inherits(evalq(tibble::as_tibble(x), env), "sdt_describe"))
})

test_that("knit_print emits a rendered table", {
  skip_if_not_installed("knitr")
  x <- sdt_describe(toy(), vars = "grade")
  out <- knit_print.sdt_describe(x)
  expect_s3_class(out, "knit_asis")
  expect_match(paste(out, collapse = " "), "Percent denominators")
})

# End to end, on a workbook written from scratch -------------------------------

skip_if_no_xlsx <- function() {
  testthat::skip_if_not_installed("readxl")
  testthat::skip_if_not_installed("writexl")
}

describe_fixture <- function(dir) {
  flag <- "No participant course date contains JSON play date 2025-07-20"
  index <- data.frame(
    Short_Course_ID = c("AAA", "BBB"),
    JSON_File_Name = c("session-AAA.json", "session-BBB.json"),
    Participant_Course_Name = "Course",
    Participant_Course_Date = c("01. - 02.03.2024", NA),
    Relation_Status = c("MATCHED", "UNCLEAR"),
    Relation_Flag = c(NA, flag),
    Match_Method = "test",
    Session_Start_UTC = as.Date(c("2024-03-01", "2024-04-01")),
    Session_End_UTC = as.Date(c("2024-03-02", "2024-04-02")),
    Course_Sheet = c("01_AAA", "02_BBB"),
    stringsAsFactors = FALSE
  )
  person <- function(id, anrede, titel, fach, funktion) {
    data.frame(
      Participant_Record_ID = id, Short_Course_ID = "AAA",
      JSON_File_Name = "session-AAA.json", Participant_Course_Name = "Course",
      Participant_Course_Date = "01. - 02.03.2024", Anrede = anrede,
      Titel = titel, Fachbereich = fach, Spezialisiert_seit = "01.01.2018",
      Verbandsmitglied = NA_character_, Funktion = funktion,
      Weiterbildungsjahre = NA_character_, Source_File = "src.csv",
      Source_Row = "1", stringsAsFactors = FALSE
    )
  }
  aaa <- rbind(
    person("P1", "Frau", "Dr. med.", "Unfallchirurgie", "Oberarzt"),
    person("P2", "Herr", NA, "Notfallmedizin", "Nichts angegeben"),
    person("P3", "Herr", NA, NA, "Chefarzt")
  )
  # The extractor's placeholder for a course it could not match: a
  # diagnostic where the salutation belongs, and nothing else at all.
  bbb <- person("P4", flag, NA, NA, NA)
  bbb$Short_Course_ID <- "BBB"
  bbb$JSON_File_Name <- "session-BBB.json"
  bbb$Spezialisiert_seit <- NA_character_
  bbb$Source_File <- NA_character_
  bbb$Source_Row <- NA_character_
  path <- file.path(dir, "describe-fixture.xlsx")
  writexl::write_xlsx(
    list(Course_Index = index, `01_AAA` = aaa, `02_BBB` = bbb), path
  )
  path
}

test_that("a participant register describes itself honestly", {
  skip_if_no_xlsx()
  dir <- withr::local_tempdir()
  p <- sdt_read_participants(describe_fixture(dir), verbose = FALSE)
  x <- sdt_describe_participants(p)
  expect_s3_class(x, "sdt_describe")
  expect_identical(names(x)[[1L]], "short_course_id")
  # The empty course cannot be a group, so it is named instead.
  expect_match(paste(attr(x, "note"), collapse = " "), "BBB")
  expect_match(paste(attr(x, "note"), collapse = " "), "no group column")

  pooled <- sdt_describe_participants(p, by = NULL)
  title <- pooled[pooled$variable == "title_level", , drop = FALSE]
  none <- title[title$level == "none", , drop = FALSE]
  expect_identical(none$n, 2L)
  expect_identical(none$n_level_imputed, 2L)
  expect_equal(none$pct_observed, 0)
  sen <- pooled[pooled$variable == "seniority", , drop = FALSE]
  expect_identical(unique(sen$n_missing), 1L)
  expect_error(sdt_describe_participants(mtcars), "sdt_participants")
})

test_that("course composition is described at course level, fills flagged", {
  skip_if_no_xlsx()
  dir <- withr::local_tempdir()
  p <- sdt_read_participants(describe_fixture(dir), verbose = FALSE)

  comp <- sdt_course_composition(p, min_participants = 2L)
  x <- sdt_describe(comp)
  fem <- x[x$variable == "share_female", , drop = FALSE]
  expect_identical(fem$n_records, 2L)
  expect_identical(fem$n_imputed, 1L)
  expect_identical(fem$n_observed, 1L)
  expect_false(is.na(fem$mean_observed))

  none <- sdt_course_composition(p, min_participants = 2L, impute = "none")
  y <- sdt_describe(none)
  fem0 <- y[y$variable == "share_female", , drop = FALSE]
  expect_identical(fem0$n_missing, 1L)
  expect_identical(fem0$n_observed, 1L)

  med <- suppressWarnings(
    sdt_course_composition(p, min_participants = 2L, impute = "median")
  )
  z <- sdt_describe(med)
  femm <- z[z$variable == "share_female", , drop = FALSE]
  # The mode fills without flagging; the zero denominator says so anyway.
  expect_identical(femm$n_imputed, 1L)
  expect_true("share_female" %in% names(attr(z, "unflagged")))
})
