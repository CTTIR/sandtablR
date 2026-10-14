# Course and group metadata.
#
# The real register is personal data and lives outside the repository, so
# every fixture here is written from scratch into a temporary workbook. It
# reproduces the shapes that matter -- an unmatched course whose only row is
# an extractor diagnostic, a blank title box, a declared non-answer, and a
# free-text answer naming three specialties at once -- without carrying a
# single real participant.

skip_if_no_xlsx <- function() {
  testthat::skip_if_not_installed("readxl")
  testthat::skip_if_not_installed("writexl")
}

fixture_workbook <- function(dir) {
  flag <- "No participant course date contains JSON play date 2025-07-20"
  index <- data.frame(
    Short_Course_ID = c("AAA", "BBB", "CCC"),
    JSON_File_Name = c("session-AAA.json", "session-BBB.json",
                       "session-CCC.json"),
    Participant_Course_Name = "Course",
    Participant_Course_Date = c("01. - 02.03.2024", "01. - 02.04.2024", NA),
    Relation_Status = c("MATCHED", "MATCHED", "UNCLEAR"),
    Relation_Flag = c(NA, NA, flag),
    Match_Method = "test",
    Session_Start_UTC = as.Date(c("2024-03-01", "2024-04-01", "2024-05-01")),
    Session_End_UTC = as.Date(c("2024-03-02", "2024-04-02", "2024-05-02")),
    Course_Sheet = c("01_AAA", "02_BBB", "03_CCC"),
    stringsAsFactors = FALSE
  )
  person <- function(id, course, file, anrede, titel, fach, funktion,
                     seit = NA) {
    data.frame(
      Participant_Record_ID = id, Short_Course_ID = course,
      JSON_File_Name = file, Participant_Course_Name = "Course",
      Participant_Course_Date = "01. - 02.03.2024", Anrede = anrede,
      Titel = titel, Fachbereich = fach, Spezialisiert_seit = seit,
      Verbandsmitglied = NA_character_, Funktion = funktion,
      Weiterbildungsjahre = NA_character_, Source_File = "src.csv",
      Source_Row = "1", stringsAsFactors = FALSE
    )
  }
  aaa <- rbind(
    person("P1", "AAA", "session-AAA.json", "Frau", "Dr. med.",
           "Unfallchirurgie", "Oberarzt", "01.01.2018"),
    person("P2", "AAA", "session-AAA.json", "Herr", NA,
           "Chirurgie, Unfallchirurgie", "Nichts angegeben"),
    person("P3", "AAA", "session-AAA.json", "Herr", "Univ.-Prof. Dr. med.",
           "An\u00e4sthesie&Intensivmedizin", "Chefarzt"),
    person("P4", "AAA", "session-AAA.json", "Herr", "Priv.-Doz.",
           NA, "Assistenzarzt")
  )
  bbb <- rbind(
    person("P5", "BBB", "session-BBB.json", "Frau", "Dr.", "Notfallmedizin",
           "Facharzt"),
    person("P6", "BBB", "session-BBB.json", "Herr", "Dr.med.",
           "Orthop\u00e4die/Unfallchirurgie", "Oberarzt")
  )
  ccc <- person("P7", "CCC", "session-CCC.json", flag, NA, NA, NA)
  ccc$Source_File <- NA_character_
  ccc$Source_Row <- NA_character_
  path <- file.path(dir, "fixture.xlsx")
  writexl::write_xlsx(
    list(Course_Index = index, `01_AAA` = aaa, `02_BBB` = bbb,
         `03_CCC` = ccc),
    path
  )
  path
}

test_that("the shipped dictionary is internally consistent", {
  d <- sdt_participant_dictionary()
  expect_s3_class(d, "sdt_participant_dictionary")
  expect_setequal(names(d), c("funktion", "titel", "fachbereich", "anrede"))
  expect_identical(d$funktion$ladder,
                   c("resident", "specialist", "consultant", "chief"))
  expect_identical(d$titel$levels,
                   c("none", "doctorate", "habilitation", "professor"))
  expect_true(all(unlist(d$fachbereich$map) %in% names(d$fachbereich$group)))
  expect_output(print(d), "sdt_participant_dictionary")
})

test_that("one new spelling is one argument, not a package patch", {
  d <- sdt_participant_dictionary(
    funktion = list(map = list("leitender oberarzt" = "consultant")),
    fachbereich = list(map = list("urologie" = "urology"),
                       group = list(urology = "surgical"))
  )
  expect_identical(d$funktion$map[["leitender oberarzt"]], "consultant")
  # The merge is additive: the shipped entries survive.
  expect_identical(d$funktion$map[["oberarzt"]], "consultant")
  expect_identical(d$fachbereich$map[["unfallchirurgie"]], "trauma_surgery")
})

test_that("an extension that breaks the vocabulary is refused", {
  expect_error(
    sdt_participant_dictionary(
      fachbereich = list(map = list("urologie" = "urology"))
    ),
    class = "sandtablR_error_dictionary"
  )
  expect_error(
    sdt_participant_dictionary(funktion = "oberarzt"),
    class = "sandtablR_error_dictionary"
  )
})

test_that("the academic ladder takes the highest matching rung", {
  d <- sdt_participant_dictionary()
  # Exact map, punctuation- and case-insensitive.
  got <- sandtablR:::.map_ladder(
    c("Dr. med.", "Dr.med.", "Dr. Med.", "Dr"), d$titel, fallback = "none"
  )
  expect_identical(got, rep("doctorate", 4L))
  # Pattern net: a professorship also matches the doctorate pattern, and the
  # higher rung has to win whichever order the patterns are written in.
  got <- sandtablR:::.map_ladder(
    c("Prof. Dr. rer. nat.", "PD Dr. sc. hum."), d$titel, fallback = "none"
  )
  expect_identical(got, c("professor", "habilitation"))
  # Something written in the box that is not a title at all is "none".
  expect_identical(
    sandtablR:::.map_ladder("Oberfeldarzt", d$titel, fallback = "none"),
    "none"
  )
  # A blank box stays missing here; the fill happens in the reader, flagged.
  expect_true(is.na(
    sandtablR:::.map_ladder(NA_character_, d$titel, fallback = "none")
  ))
})

test_that("multi-specialty free text collapses to the one named first", {
  d <- sdt_participant_dictionary()
  got <- sandtablR:::.map_first_mentioned(
    c("Chirurgie, Unfallchirurgie",
      "Unfallchirurgie, Allgemeinchirurgie",
      "Allgemein Chirurgie 2000 / Unfallchirurgie 2005"),
    d$fachbereich
  )
  expect_identical(got, c("general_surgery", "trauma_surgery",
                          "general_surgery"))
})

test_that("leak detection survives a long but legitimate answer", {
  flag <- "No participant course date contains JSON play date 2025-07-20"
  widths <- sandtablR:::.leak_width()
  expect_true(sandtablR:::.detect_leaks(flag, flag, widths[["anrede"]]))
  # The specialty field really does hold sentence-length answers. An earlier
  # single width for every field destroyed two of them.
  long <- paste("Allgemein Chirurgie 2000 / Unfallchirurgie 2005 akute und",
                "klinische Notfallmedizin 2024")
  expect_false(
    sandtablR:::.detect_leaks(long, flag, widths[["fachbereich"]])
  )
  expect_true(
    sandtablR:::.detect_leaks(long, flag, widths[["anrede"]])
  )
})

test_that("free-text numbers and dates refuse junk instead of coercing it", {
  expect_identical(sandtablR:::.parse_years(c("5", ",", ",,", "3,5")),
                   c(5, NA, NA, 3.5))
  got <- sandtablR:::.parse_de_date(c("01.02.2018", "2018-02-01", "irgendwann"))
  expect_identical(got, as.Date(c("2018-02-01", NA, NA)))
})

test_that("the reader neutralises the leak and drops the placeholder row", {
  skip_if_no_xlsx()
  path <- fixture_workbook(withr::local_tempdir())
  p <- suppressMessages(sdt_read_participants(path, verbose = FALSE))

  expect_s3_class(p, "sdt_participants")
  # Seven source rows, one of which is a diagnostic rather than a person.
  expect_identical(nrow(p), 6L)
  expect_identical(attr(p, "meta")$n_placeholder_rows, 1L)
  expect_identical(sum(attr(p, "meta")$n_leaks), 1L)
  # No error string survives anywhere in the raw columns.
  expect_false(any(grepl("JSON", unlist(p[sandtablR:::.participant_fields()]),
                         fixed = TRUE), na.rm = TRUE))

  idx <- attr(p, "course_index")
  expect_identical(nrow(idx), 3L)
  expect_identical(idx$has_participants, c(TRUE, TRUE, FALSE))
  expect_identical(idx$n_participants, c(4L, 2L, 0L))
})

test_that("an imputed value is never indistinguishable from an observed one", {
  skip_if_no_xlsx()
  path <- fixture_workbook(withr::local_tempdir())
  p <- suppressMessages(sdt_read_participants(path, verbose = FALSE))

  blank <- p$record_id == "P2"
  expect_true(is.na(p$titel[blank]))
  expect_identical(as.character(p$title_level[blank]), "none")
  expect_true(p$.imputed_titel[blank])
  expect_true(p$.missing_titel[blank])
  # Everything else observed keeps its flag down.
  expect_identical(sum(p$.imputed_titel), 1L)

  # A declared non-answer is missing information, not a grade.
  expect_true(is.na(p$seniority[blank]))
  expect_identical(p$funktion[blank], "Nichts angegeben")
  expect_true(p$.missing_funktion[blank])
})

test_that("a join key the corpus does not know is refused, all rows at once", {
  skip_if_no_xlsx()
  dir <- withr::local_tempdir()
  path <- fixture_workbook(dir)
  sheet <- readxl::read_excel(path, sheet = "01_AAA", col_types = "text")
  sheet$JSON_File_Name <- "session-NOPE.json"
  index <- readxl::read_excel(path, sheet = "Course_Index")
  index <- index[index$Course_Sheet == "01_AAA", ]
  bad <- file.path(dir, "bad.xlsx")
  writexl::write_xlsx(list(Course_Index = index, `01_AAA` = sheet), bad)
  expect_error(
    suppressMessages(sdt_read_participants(bad, verbose = FALSE)),
    class = "sandtablR_error_participants"
  )
})

test_that("a missing file and an unknown sheet are named, not guessed at", {
  skip_if_no_xlsx()
  expect_error(
    sdt_read_participants(file.path(tempdir(), "nope.xlsx")),
    class = "sandtablR_error_participants_file"
  )
  path <- fixture_workbook(withr::local_tempdir())
  expect_error(
    sdt_read_participants(path, sheets = "99_NOPE"),
    class = "sandtablR_error_participants_sheet"
  )
})

test_that("descriptives state both denominators and the fill count", {
  skip_if_no_xlsx()
  path <- fixture_workbook(withr::local_tempdir())
  p <- suppressMessages(sdt_read_participants(path, verbose = FALSE))
  s <- sdt_participant_summary(p, fields = "title_level")

  expect_identical(unique(s$n_records), 6L)
  # One blank title box: five observed, one filled.
  expect_identical(unique(s$n_observed), 5L)
  expect_identical(unique(s$n_missing), 1L)
  none <- s[s$level == "none", ]
  expect_identical(none$n, 1L)
  expect_identical(none$n_filled, 1L)
  # The observed share of "none" is zero: the only one was assumed.
  expect_identical(none$share_observed, 0)
  expect_identical(sum(s$n), 6L)
})

test_that("empty courses come through as flagged rows, never as absences", {
  skip_if_no_xlsx()
  path <- fixture_workbook(withr::local_tempdir())
  p <- suppressMessages(sdt_read_participants(path, verbose = FALSE))
  comp <- sdt_course_composition(p)

  expect_s3_class(comp, "sdt_course_composition")
  expect_identical(nrow(comp), 3L)
  empty <- comp[!comp$has_participants, ]
  expect_identical(nrow(empty), 1L)
  expect_identical(empty$n_participants, 0L)
  expect_true(empty$below_min_participants)
  for (k in attr(comp, "covariates")) {
    # Filled from the corpus-wide value, and flagged for having been filled.
    expect_false(is.na(empty[[k]]))
    expect_true(empty[[paste0(k, "_missing")]])
    expect_identical(empty[[paste0("n_", k)]], 0L)
  }
})

test_that("the three imputation modes differ exactly where they should", {
  skip_if_no_xlsx()
  path <- fixture_workbook(withr::local_tempdir())
  p <- suppressMessages(sdt_read_participants(path, verbose = FALSE))

  none <- sdt_course_composition(p, impute = "none")
  expect_true(is.na(none$share_female[!none$has_participants]))
  expect_true(none$share_female_missing[!none$has_participants])

  flag <- sdt_course_composition(p, impute = "flag")
  pooled <- mean(as.numeric(p$sex == "female"), na.rm = TRUE)
  expect_equal(flag$share_female[!flag$has_participants], pooled)
  expect_true(flag$share_female_missing[!flag$has_participants])

  # "median" is the unflagged mode and says so out loud.
  expect_warning(
    med <- sdt_course_composition(p, impute = "median"),
    class = "sandtablR_warning_unflagged_imputation"
  )
  expect_false(any(med$share_female_missing))
  # The denominator is the one thing it cannot hide.
  expect_identical(med$n_share_female[!med$has_participants], 0L)
})

test_that("a sheets subset is not mistaken for a set of empty courses", {
  skip_if_no_xlsx()
  path <- fixture_workbook(withr::local_tempdir())
  p <- suppressMessages(
    sdt_read_participants(path, sheets = "01_AAA", verbose = FALSE)
  )
  idx <- attr(p, "course_index")
  expect_identical(idx$sheet_read, c(TRUE, FALSE, FALSE))
  expect_identical(idx$n_participants, c(4L, NA_integer_, NA_integer_))
  comp <- sdt_course_composition(p)
  expect_identical(comp$sheet_read, c(TRUE, FALSE, FALSE))
})

test_that("the empty-course report names only courses whose sheet was read", {
  skip_if_no_xlsx()
  path <- fixture_workbook(withr::local_tempdir())

  # AAA has people, CCC is the UNCLEAR placeholder-only course, BBB is not
  # read at all. Only CCC is empty; BBB is skipped.
  msg <- paste(
    testthat::capture_messages(
      sdt_read_participants(path, sheets = c("01_AAA", "03_CCC"),
                            verbose = TRUE)
    ),
    collapse = ""
  )
  line <- grep("no linked participants", strsplit(msg, "\n")[[1]],
               value = TRUE)

  expect_length(line, 1L)
  expect_match(line, "CCC", fixed = TRUE)
  # The count and the list are computed separately. Without the `sheet_read`
  # guard on the list they disagree: "1 course" followed by two names.
  expect_false(grepl("BBB", msg, fixed = TRUE))
  expect_match(line, "1 course has", fixed = TRUE)
})

test_that("composition is course level, shared by every group in a course", {
  skip_if_no_xlsx()
  path <- fixture_workbook(withr::local_tempdir())
  p <- suppressMessages(sdt_read_participants(path, verbose = FALSE))
  comp <- sdt_course_composition(p)
  # One row per course, keyed by the JSON file name the corpus joins on.
  expect_identical(comp$file, attr(p, "course_index")$file)
  expect_identical(anyDuplicated(comp$file), 0L)
  # There is no group column to key on, in the register or in the result.
  # `specialty_group` is the surgical / non-surgical split, not a team.
  keys <- c("group", "gruppe", "team", "group_id", "team_id")
  expect_false(any(tolower(names(p)) %in% keys))
  expect_false(any(tolower(names(comp)) %in% keys))
})
