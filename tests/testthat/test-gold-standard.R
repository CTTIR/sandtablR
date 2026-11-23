# The gold-standard triage codebook.
#
# The real codebook lives outside the repository, so every fixture here is
# written from scratch into a temporary workbook. It reproduces the shapes
# that matter -- duplicate test rows carrying a letter id, blank rows, a
# column-shifted legend block, and an earlier working sheet that disagrees --
# with invented cards and placeholder text, never a line of the real deck.
#
# The junk rows are deliberately interleaved *between* deck rows rather than
# parked at the bottom. A reader that dropped rows by position would pass a
# fixture shaped like the real file and fail here.

skip_if_no_xlsx <- function() {
  testthat::skip_if_not_installed("readxl")
  testthat::skip_if_not_installed("writexl")
}

# Neutral English headers, plus one column the contract does not use.
gold_headers <- function() {
  c("Card", "Sex", "Age", "SK", "Notes", "Findings", "Vitals")
}

# The same seven columns under German spellings the contract also accepts.
# Same contract, so the reader must read both to the same table.
gold_headers_german <- function() {
  c("Patienten-Nr.", "Geschlecht", "Alter", "Sichtungskategorie",
    "Notiz", "Befund", "Vitalwerte")
}

# One sheet row, given as seven cells. Everything is character, which is how
# a sheet reads once a shifted legend has forced text on a number column.
gold_row <- function(...) {
  cells <- c(...)
  stopifnot(length(cells) == 7L)
  out <- as.data.frame(as.list(as.character(cells)), stringsAsFactors = FALSE)
  names(out) <- gold_headers()
  out
}

# An invented 24-card deck: findings and vitals are placeholders, and the
# 10 / 6 / 8 split is arbitrary. Nothing here should depend on it, and a test
# that starts to is testing the fixture rather than the reader.
gold_deck_rows <- function(sk = NULL, sex = NULL, age = NULL) {
  n <- 24L
  if (is.null(sk)) sk <- c(rep("1", 10L), rep("2", 6L), rep("3", 8L))
  if (is.null(sex)) sex <- rep(c("m", "w"), length.out = n)
  if (is.null(age)) {
    age <- as.character(seq.int(20L, by = 1L, length.out = n))
  }
  rows <- lapply(seq_len(n), function(i) {
    gold_row(
      as.character(i), sex[[i]], age[[i]], sk[[i]],
      paste("code000", i), paste("findings", i), paste("vitals", i)
    )
  })
  do.call(rbind, rows)
}

gold_blank_row <- function() {
  gold_row(NA, NA, NA, NA, NA, NA, NA)
}

# Duplicate test rows of the kind a codebook can carry: a letter id, and a
# category that the legend has already counted.
gold_dup_rows <- function() {
  rbind(
    gold_row("A", "m", "25", "1", "c", "f", "v"),
    gold_row("A", "m", "25", "1", "c", "f", "v")
  )
}

# The legend, written one column to the left of where the data sits, so that
# its labels land in the age column and its counts in the category column.
gold_legend_rows <- function(counts = c(12L, 6L, 8L)) {
  rbind(
    gold_row("m", "28", "SK I", as.character(counts[[1]]), NA, NA, NA),
    gold_row("w", "20", "SK II", as.character(counts[[2]]), NA, NA, NA),
    gold_row(NA, NA, "SK III", as.character(counts[[3]]), NA, NA, NA)
  )
}

# An earlier working sheet: the same deck, junk legend rows of its own, and
# one card whose category was changed before the codebook was finalised.
gold_working_sheet <- function(deck, disagree_card = "7") {
  ids <- deck[["Card"]]
  sk <- deck[["SK"]]
  sk[ids == disagree_card] <- if (sk[ids == disagree_card] == "1") "3" else "1"
  data.frame(
    Card = c(ids, "A", NA, "SKI", "SKII", "SKIII"),
    SK = c(sk, "1", NA, "11", "6", "8"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

write_codebook <- function(dir, deck = gold_deck_rows(),
                           legend = gold_legend_rows(),
                           working = NULL, extra = NULL,
                           sheet = "Codebook",
                           headers = NULL,
                           file = "codebook.xlsx") {
  # Junk interleaved with data, never only below it.
  # Trailing all-blank rows are not written to the file at all, so the blank
  # rows sit between blocks.
  body <- rbind(
    gold_blank_row(),
    deck[seq_len(20L), , drop = FALSE],
    legend,
    gold_blank_row(),
    deck[seq.int(21L, nrow(deck)), , drop = FALSE],
    gold_blank_row(),
    gold_dup_rows()
  )
  if (!is.null(extra)) body <- rbind(body, extra)
  # Renamed last, so `deck`, `legend` and `extra` are all still built and
  # indexed under the canonical header names.
  if (!is.null(headers)) names(body) <- headers
  sheets <- stats::setNames(list(body), sheet)
  if (!is.null(working)) sheets[["Working"]] <- working
  path <- file.path(dir, file)
  writexl::write_xlsx(sheets, path)
  path
}

# Reading ---------------------------------------------------------------------

test_that("the reader returns one validated row per card", {
  skip_if_no_xlsx()
  path <- write_codebook(withr::local_tempdir())

  gold <- sdt_read_gold_standard(path, verbose = FALSE)

  expect_s3_class(gold, "sdt_gold_standard")
  expect_s3_class(gold, "tbl_df")
  expect_identical(nrow(gold), 24L)
  expect_identical(
    names(gold),
    c("card", "triage", "sex", "age", "findings", "vitals", "sk_raw")
  )
  expect_identical(gold$card, sprintf("%02d", seq_len(24)))
  expect_identical(anyDuplicated(gold$card), 0L)
  expect_true(all(gold$triage %in% sdt_triage_levels()))
  expect_true(all(gold$sex %in% sdt_sex_levels()))
  expect_type(gold$age, "integer")
  expect_true(all(gold$age >= 0L & gold$age <= 130L))
  expect_false(anyNA(gold$findings))
  expect_false(anyNA(gold$vitals))
})

test_that("English and German header spellings read to the same table", {
  skip_if_no_xlsx()
  dir <- withr::local_tempdir()

  english <- sdt_read_gold_standard(
    write_codebook(dir), verbose = FALSE
  )
  german <- sdt_read_gold_standard(
    write_codebook(dir, headers = gold_headers_german(),
                   file = "codebook-german.xlsx"),
    verbose = FALSE
  )

  # The header names are the only difference, so the parse must be identical
  # down to the free text. Compared column by column, because `meta`
  # legitimately records a different path and read time.
  cells <- function(x) {
    stats::setNames(lapply(names(x), function(nm) x[[nm]]), names(x))
  }
  expect_identical(cells(german), cells(english))
  expect_false(anyNA(german$findings))
  expect_false(anyNA(german$vitals))
})

test_that("every accepted header spelling maps onto its field", {
  # The contract lists several normalised keys per field; each must be found
  # on its own, so a codebook using any one of them reads.
  for (field in names(sandtablR:::.gold_columns())) {
    for (key in sandtablR:::.gold_columns()[[field]]) {
      expect_identical(sandtablR:::.gold_key(key), key, info = key)
    }
  }
  headers <- gold_headers()
  raw <- stats::setNames(
    as.data.frame(as.list(letters[1:7]), stringsAsFactors = FALSE), headers
  )
  mapped <- sandtablR:::.gold_map_columns(raw)
  expect_identical(names(mapped), names(sandtablR:::.gold_columns()))
})

test_that("an unmatched free-text column is named, not silently dropped", {
  skip_if_no_xlsx()
  headers <- gold_headers_german()
  headers[[6]] <- "Kommentar"

  expect_error(
    sdt_read_gold_standard(
      write_codebook(withr::local_tempdir(), headers = headers),
      verbose = FALSE
    ),
    class = "sandtablR_error_gold"
  )
})

test_that("the category and sex codes are folded onto the vocabularies", {
  skip_if_no_xlsx()
  path <- write_codebook(withr::local_tempdir())

  gold <- sdt_read_gold_standard(path, verbose = FALSE)

  expect_identical(gold$triage[gold$sk_raw == "1"][[1]], "red")
  expect_identical(gold$triage[gold$sk_raw == "2"][[1]], "yellow")
  expect_identical(gold$triage[gold$sk_raw == "3"][[1]], "green")
  expect_identical(
    as.integer(table(factor(gold$triage, levels = sdt_triage_levels()))),
    c(10L, 6L, 8L)
  )
  # "w" is weiblich, and must not survive into the cohort as its own level.
  expect_identical(sort(unique(gold$sex)), c("f", "m"))
  expect_identical(sum(gold$sex == "f"), 12L)
})

test_that("guards drop by rule, not by row position", {
  skip_if_no_xlsx()
  path <- write_codebook(withr::local_tempdir())

  gold <- sdt_read_gold_standard(path, verbose = FALSE)
  guards <- attr(gold, "meta")$guards

  expect_identical(guards$guard,
                   c("blank", "non_numeric_card", "card_out_of_range"))
  # 3 blank rows; 2 duplicate "A" rows plus the 3 shifted legend rows.
  expect_identical(guards$n_dropped, c(3L, 5L, 0L))
  expect_identical(attr(gold, "meta")$n_source_rows, 32L)
  expect_true(grepl("A", guards$ids[guards$guard == "non_numeric_card"]))
})

test_that("a card id outside a stated deck is dropped and counted", {
  skip_if_no_xlsx()
  extra <- rbind(
    gold_row("99", "m", "30", "1", "c", "f", "v"),
    gold_row("0", "m", "30", "2", "c", "f", "v")
  )
  path <- write_codebook(withr::local_tempdir(), extra = extra)

  gold <- sdt_read_gold_standard(path, verbose = FALSE, deck_size = 24L)
  guards <- attr(gold, "meta")$guards

  expect_identical(nrow(gold), 24L)
  expect_identical(attr(gold, "meta")$deck_size, 24L)
  expect_identical(attr(gold, "meta")$deck_size_source, "argument")
  expect_identical(guards$n_dropped[guards$guard == "card_out_of_range"], 2L)
  expect_identical(
    guards$ids[guards$guard == "card_out_of_range"], "99, 0"
  )
})

test_that("the legend is read back and compared with the parse", {
  skip_if_no_xlsx()
  path <- write_codebook(withr::local_tempdir())

  gold <- sdt_read_gold_standard(path, verbose = FALSE)
  legend <- attr(gold, "meta")$legend

  expect_identical(legend$triage, sdt_triage_levels())
  expect_identical(legend$n_parsed, c(10L, 6L, 8L))
  # The legend counts the duplicates, so the comparable number adds them back.
  expect_identical(legend$n_dropped, c(2L, 0L, 0L))
  expect_identical(legend$n_with_dropped, c(12L, 6L, 8L))
  expect_identical(legend$n_legend, c(12L, 6L, 8L))
  expect_true(all(legend$agrees))
})

test_that("a legend that disagrees is reported, not silently preferred", {
  skip_if_no_xlsx()
  path <- write_codebook(withr::local_tempdir(),
                         legend = gold_legend_rows(c(10L, 8L, 8L)))

  expect_message(
    gold <- sdt_read_gold_standard(path, verbose = TRUE),
    "legend disagrees on"
  )
  legend <- attr(gold, "meta")$legend

  # The parse still stands; the disagreement is reported beside it.
  expect_identical(nrow(gold), 24L)
  expect_identical(legend$n_legend, c(10L, 8L, 8L))
  expect_identical(legend$agrees, c(FALSE, FALSE, TRUE))
})

test_that("an earlier working sheet is cross-checked card by card", {
  skip_if_no_xlsx()
  deck <- gold_deck_rows()
  path <- write_codebook(withr::local_tempdir(),
                         deck = deck,
                         working = gold_working_sheet(deck, "7"))

  gold <- sdt_read_gold_standard(path, verbose = FALSE)
  work <- attr(gold, "meta")$working_sheet

  expect_true(work$usable)
  expect_identical(work$sheet, "Working")
  expect_identical(work$n_compared, 24L)
  expect_identical(work$n_agree, 23L)
  expect_identical(work$disagreements$card, "07")
  expect_identical(work$disagreements$triage_gold, "red")
  expect_identical(work$disagreements$triage_working, "green")
})

test_that("a workbook with no second sheet simply has no cross-check", {
  skip_if_no_xlsx()
  path <- write_codebook(withr::local_tempdir())

  gold <- sdt_read_gold_standard(path, verbose = FALSE)

  expect_null(attr(gold, "meta")$working_sheet)
})

test_that("verbose reading reports the deck, the drops and the checks", {
  skip_if_no_xlsx()
  deck <- gold_deck_rows()
  path <- write_codebook(withr::local_tempdir(),
                         deck = deck,
                         working = gold_working_sheet(deck, "7"))

  expect_message(sdt_read_gold_standard(path), "Read 24 cards")
  expect_message(sdt_read_gold_standard(path), "removed by rule")
  expect_message(sdt_read_gold_standard(path), "Legend cross-check")
  expect_message(sdt_read_gold_standard(path), "23/24 cards agree")
  expect_silent(sdt_read_gold_standard(path, verbose = FALSE))
})

test_that("the sheet can be named, and an unknown name is refused", {
  skip_if_no_xlsx()
  path <- write_codebook(withr::local_tempdir(), sheet = "Deck")

  # Only one sheet, so it is unambiguous whatever it is called.
  expect_identical(nrow(sdt_read_gold_standard(path, verbose = FALSE)), 24L)
  expect_identical(
    nrow(sdt_read_gold_standard(path, sheet = "Deck", verbose = FALSE)), 24L
  )
  expect_error(
    sdt_read_gold_standard(path, sheet = "nope", verbose = FALSE),
    class = "sandtablR_error_gold"
  )
})

test_that("the codebook sheet is found by its columns, not its name", {
  skip_if_no_xlsx()
  deck <- gold_deck_rows()
  # A working sheet carries only an id and a category, so it is no candidate,
  # whatever either sheet is called.
  path <- write_codebook(withr::local_tempdir(), deck = deck,
                         working = gold_working_sheet(deck), sheet = "Deck")
  gold <- sdt_read_gold_standard(path, verbose = FALSE)
  expect_identical(attr(gold, "meta")$sheet, "Deck")
  expect_identical(nrow(gold), 24L)
})

test_that("an ambiguous workbook asks which sheet to read", {
  skip_if_no_xlsx()
  dir <- withr::local_tempdir()
  # Two sheets that both carry every contract column.
  path <- file.path(dir, "two.xlsx")
  writexl::write_xlsx(
    list(First = gold_deck_rows(), Second = gold_deck_rows()), path
  )

  expect_error(
    sdt_read_gold_standard(path, verbose = FALSE),
    class = "sandtablR_error_gold"
  )
  expect_identical(
    nrow(sdt_read_gold_standard(path, sheet = "Second", verbose = FALSE)), 24L
  )
})

test_that("a missing file and a missing column are named, not guessed", {
  skip_if_no_xlsx()
  dir <- withr::local_tempdir()
  expect_error(
    sdt_read_gold_standard(file.path(dir, "absent.xlsx"), verbose = FALSE),
    class = "sandtablR_error_gold"
  )

  deck <- gold_deck_rows()
  deck[["SK"]] <- NULL
  path <- file.path(dir, "short.xlsx")
  writexl::write_xlsx(list(Codebook = deck), path)
  expect_error(
    sdt_read_gold_standard(path, verbose = FALSE),
    class = "sandtablR_error_gold"
  )
  expect_error(
    sdt_read_gold_standard(path, verbose = FALSE), "sk_raw"
  )
})

test_that("every invalid cell is reported at once", {
  skip_if_no_xlsx()
  sk <- c(rep("1", 10L), rep("2", 6L), rep("3", 8L))
  sk[[2]] <- "9"
  sex <- rep(c("m", "w"), length.out = 24L)
  sex[[4]] <- "unbekannt"
  age <- as.character(seq.int(20L, by = 1L, length.out = 24L))
  age[[6]] <- "999"
  age[[8]] <- "keine Angabe"
  path <- write_codebook(
    withr::local_tempdir(),
    deck = gold_deck_rows(sk = sk, sex = sex, age = age)
  )

  err <- expect_error(
    sdt_read_gold_standard(path, verbose = FALSE),
    class = "sandtablR_error_gold"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "triage")
  expect_match(msg, "sex")
  expect_match(msg, "age")
  # Rows are reported, and all of them, not just the first bad column.
  expect_match(msg, "row 2")
  expect_match(msg, "6 and 8")
})

test_that("a short deck is a reading error, not a short deck", {
  skip_if_no_xlsx()
  dir <- withr::local_tempdir()
  # Truncated: only a stated deck size can tell the last card is missing.
  deck <- gold_deck_rows()[seq_len(23L), , drop = FALSE]
  path <- write_codebook(dir, deck = deck)
  expect_error(
    sdt_read_gold_standard(path, verbose = FALSE, deck_size = 24L),
    "expected 24 unique cards, found 23"
  )
  # A gap shows without one: the deck runs to the highest card id.
  gap <- gold_deck_rows()[-7L, , drop = FALSE]
  path <- write_codebook(dir, deck = gap, file = "gap.xlsx")
  expect_error(
    sdt_read_gold_standard(path, verbose = FALSE),
    "expected 24 unique cards, found 23"
  )
})

test_that("the deck size defaults to the highest card id on the sheet", {
  skip_if_no_xlsx()
  dir <- withr::local_tempdir()
  plain <- write_codebook(dir)
  gold <- sdt_read_gold_standard(plain, verbose = FALSE)
  expect_identical(attr(gold, "meta")$deck_size, 24L)
  expect_identical(attr(gold, "meta")$deck_size_source, "highest card id")
  # A card missing from the end of the deck cannot show without a stated
  # size, so the reader says where the size came from, and only then.
  expect_message(sdt_read_gold_standard(plain),
                 "Deck size 24, taken from the highest card id")
  stated <- testthat::capture_messages(
    sdt_read_gold_standard(plain, deck_size = 24L)
  )
  expect_false(any(grepl("highest card id", stated)))
  truncated <- write_codebook(dir, deck = gold_deck_rows()[seq_len(23L), ],
                              file = "truncated.xlsx")
  expect_message(sdt_read_gold_standard(truncated),
                 "Deck size 23, taken from the highest card id")
  # A stray numbered row beyond the deck enlarges the derived deck, which is
  # then incomplete: an error that says to state the size, never a silent
  # extra card.
  extra <- gold_row("99", "m", "30", "1", "c", "f", "v")
  path <- write_codebook(dir, extra = extra, file = "stray.xlsx")
  expect_error(
    sdt_read_gold_standard(path, verbose = FALSE),
    "pass deck_size"
  )
  expect_error(
    sdt_read_gold_standard(path, verbose = FALSE, deck_size = 0L),
    "deck_size"
  )
})

# Wiring it into the data model -----------------------------------------------

fixture_gold <- function() {
  skip_if_no_xlsx()
  sdt_read_gold_standard(write_codebook(withr::local_tempdir()),
                         verbose = FALSE)
}

fixture_cohort <- function(cards = sprintf("%02d", 1:6)) {
  co <- sdt_cohort(sdt_synthetic_cohort(n = length(cards), seed = 1))
  co$card <- cards
  co$called_triage <- rep(sdt_triage_levels(), length.out = length(cards))
  co
}

test_that("applying the gold standard overwrites and keeps the original", {
  gold <- fixture_gold()
  co <- fixture_cohort()

  out <- sdt_apply_gold_standard(co, gold)

  expect_s3_class(out, "cohort")
  expect_silent(validate_cohort(out))
  expect_identical(nrow(out), nrow(co))
  expect_true(all(c("triage_original", "age_original", "sex_original",
                    "triage_source") %in% names(out)))
  idx <- match(out$card, gold$card)
  expect_identical(out$triage, gold$triage[idx])
  expect_identical(out$age, gold$age[idx])
  expect_identical(out$sex, gold$sex[idx])
  expect_identical(out$triage_original, co$triage)
  expect_identical(out$age_original, co$age)
  expect_identical(out$sex_original, co$sex)
  expect_identical(unique(out$triage_source), "gold")

  applied <- attr(out, "gold_applied")
  expect_identical(applied$on, "card")
  expect_identical(applied$replaced, c("triage", "age", "sex"))
  expect_identical(applied$n_cards, 6L)
})

test_that("padded, unpadded and numeric card ids all join", {
  gold <- fixture_gold()
  padded <- sdt_apply_gold_standard(fixture_cohort(), gold)
  bare <- sdt_apply_gold_standard(fixture_cohort(as.character(1:6)), gold)
  num <- fixture_cohort()
  num$card <- 1:6
  numeric <- sdt_apply_gold_standard(num, gold)

  expect_identical(padded$triage, bare$triage)
  expect_identical(padded$triage, numeric$triage)
  expect_identical(padded$age, numeric$age)
})

test_that("a card the codebook does not carry is named, not dropped", {
  gold <- fixture_gold()
  co <- fixture_cohort(c("01", "02", "77", "78", "05", "06"))

  err <- expect_error(
    sdt_apply_gold_standard(co, gold),
    class = "sandtablR_error_gold"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "77")
  expect_match(msg, "78")
})

test_that("keep_original and replace are honoured", {
  gold <- fixture_gold()
  co <- fixture_cohort()

  bare <- sdt_apply_gold_standard(co, gold, keep_original = FALSE)
  expect_false(any(grepl("_original$", names(bare))))
  expect_identical(bare$triage, gold$triage[match(bare$card, gold$card)])

  only_triage <- sdt_apply_gold_standard(co, gold, replace = "triage")
  expect_identical(only_triage$age, co$age)
  expect_identical(only_triage$sex, co$sex)
  expect_false("age_original" %in% names(only_triage))

  # Nothing was replaced, so nothing may claim the truth's provenance.
  none <- sdt_apply_gold_standard(co, gold, replace = character())
  expect_identical(none$triage, co$triage)
  expect_identical(unique(none$triage_source), NA_character_)
  expect_identical(unique(only_triage$triage_source), "gold")

  labelled <- co
  labelled$triage_source <- "consensus"
  kept <- sdt_apply_gold_standard(labelled, gold, replace = c("age", "sex"))
  expect_identical(unique(kept$triage_source), "consensus")

  expect_error(
    sdt_apply_gold_standard(co, gold, replace = "life_points"),
    class = "sandtablR_error_gold"
  )
})

test_that("any frame with a card column can be joined, not only a cohort", {
  gold <- fixture_gold()
  plays <- data.frame(
    group = rep(c("A", "B"), each = 3),
    card = rep(c("01", "02", "03"), times = 2),
    called_triage = c("red", "red", "red", "green", "yellow", "yellow"),
    stringsAsFactors = FALSE
  )

  out <- sdt_apply_gold_standard(plays, gold, on = "card")

  expect_s3_class(out, "tbl_df")
  expect_false(inherits(out, "cohort"))
  expect_identical(out$triage, gold$triage[match(out$card, gold$card)])
  # Nothing to keep: the frame had no triage column of its own.
  expect_false("triage_original" %in% names(out))
})

test_that("the join column must exist and the codebook must be a codebook", {
  gold <- fixture_gold()
  co <- fixture_cohort()

  expect_error(
    sdt_apply_gold_standard(co, gold, on = "patient"),
    class = "sandtablR_error_gold"
  )
  expect_error(
    sdt_apply_gold_standard(co, as.data.frame(gold)),
    "sdt_gold_standard"
  )
  expect_error(sdt_apply_gold_standard("not a frame", gold),
               class = "sandtablR_error_gold")
})

# Scoring ---------------------------------------------------------------------

scoring_frame <- function() {
  data.frame(
    truth = c("red", "red", "red", "yellow", "yellow",
              "green", "green", "green", "green"),
    est = c("red", "yellow", "green", "yellow", "green",
            "red", "green", "green", "yellow"),
    stringsAsFactors = FALSE
  )
}

test_that("hit, over- and under-triage are computed per category", {
  acc <- sdt_triage_accuracy(scoring_frame(), truth = "truth",
                             estimate = "est")

  expect_s3_class(acc, "sdt_triage_accuracy")
  expect_identical(acc$category, c("red", "yellow", "green", "overall"))
  expect_identical(acc$n, c(3L, 2L, 4L, 9L))
  expect_identical(acc$n_hit, c(1L, 1L, 2L, 4L))
  expect_equal(acc$hit_rate, c(1 / 3, 1 / 2, 1 / 2, 4 / 9))
  # Red can only be under-triaged, green can only be over-triaged.
  expect_identical(acc$n_over, c(0L, 0L, 2L, 2L))
  expect_identical(acc$n_under, c(2L, 1L, 0L, 3L))
  expect_equal(acc$over_rate, c(0, 0, 1 / 2, 2 / 9))
  expect_equal(acc$under_rate, c(2 / 3, 1 / 2, 0, 1 / 3))
})

test_that("kappa is reported once, on the overall row", {
  acc <- sdt_triage_accuracy(scoring_frame(), truth = "truth",
                             estimate = "est")

  expect_true(all(is.na(acc$kappa[acc$category != "overall"])))
  # po = 4/9, pe = (3*2 + 2*3 + 4*4)/81 = 28/81, kappa = 8/53.
  expect_equal(acc$kappa[acc$category == "overall"], 8 / 53)
})

test_that("the confusion matrix is the full table, truth in rows", {
  acc <- sdt_triage_accuracy(scoring_frame(), truth = "truth",
                             estimate = "est")
  cm <- attr(acc, "confusion")

  expect_true(is.matrix(cm))
  expect_identical(dimnames(cm),
                   list(truth = sdt_triage_levels(),
                        estimate = sdt_triage_levels()))
  expect_identical(as.integer(cm),
                   as.integer(c(1, 0, 1, 1, 1, 1, 1, 1, 2)))
  expect_identical(sum(cm), 9L)
  expect_equal(rowSums(cm), c(red = 3, yellow = 2, green = 4))
})

test_that("perfect agreement scores one, chance-corrected too", {
  perfect <- data.frame(
    truth = rep(sdt_triage_levels(), each = 4),
    est = rep(sdt_triage_levels(), each = 4),
    stringsAsFactors = FALSE
  )

  acc <- sdt_triage_accuracy(perfect, truth = "truth", estimate = "est")

  expect_equal(acc$hit_rate, rep(1, 4))
  expect_identical(acc$n_over, rep(0L, 4))
  expect_identical(acc$n_under, rep(0L, 4))
  expect_equal(acc$kappa[acc$category == "overall"], 1)
})

test_that("rows with a missing label are dropped and counted", {
  x <- scoring_frame()
  x$est[c(1, 2)] <- NA_character_
  x$truth[9] <- NA_character_

  acc <- sdt_triage_accuracy(x, truth = "truth", estimate = "est")
  meta <- attr(acc, "meta")

  expect_identical(meta$n_input, 9L)
  expect_identical(meta$n_used, 6L)
  expect_identical(meta$n_dropped, 3L)
  expect_identical(acc$n[acc$category == "overall"], 6L)
})

test_that("splitting by a grouping column gives one block per group", {
  x <- scoring_frame()
  x$group <- rep(c("A", "B", "C"), each = 3)

  acc <- sdt_triage_accuracy(x, truth = "truth", estimate = "est",
                             by = "group")

  expect_identical(names(acc)[[1]], "group")
  expect_identical(nrow(acc), 12L)
  expect_identical(unique(acc$group), c("A", "B", "C"))
  expect_identical(sum(acc$n[acc$category == "overall"]), 9L)

  cm <- attr(acc, "confusion")
  expect_type(cm, "list")
  expect_identical(names(cm), c("A", "B", "C"))
  expect_identical(sum(cm[["A"]]), 3L)
  # Pooling the blocks must give back the ungrouped matrix.
  pooled <- attr(sdt_triage_accuracy(x, truth = "truth", estimate = "est"),
                 "confusion")
  expect_identical(Reduce(`+`, cm), pooled)
})

test_that("splitting by several columns keys the blocks by all of them", {
  x <- scoring_frame()
  x$file <- rep(c("s1", "s2"), length.out = 9)
  x$group <- rep(c("A", "B"), each = 5, length.out = 9)

  acc <- sdt_triage_accuracy(x, truth = "truth", estimate = "est",
                             by = c("file", "group"))

  expect_identical(names(acc)[1:2], c("file", "group"))
  expect_true(all(grepl(" \\| ", names(attr(acc, "confusion")))))
  expect_identical(sum(acc$n[acc$category == "overall"]), 9L)
})

test_that("unusable input is refused with a named class", {
  x <- scoring_frame()

  expect_error(
    sdt_triage_accuracy(x, truth = "nope", estimate = "est"),
    class = "sandtablR_error_triage_accuracy"
  )
  expect_error(
    sdt_triage_accuracy(x, truth = "truth", estimate = "est", by = "nope"),
    class = "sandtablR_error_triage_accuracy"
  )
  expect_error(
    sdt_triage_accuracy("not a frame"),
    class = "sandtablR_error_triage_accuracy"
  )

  bad <- x
  bad$est[[1]] <- "purple"
  expect_error(
    sdt_triage_accuracy(bad, truth = "truth", estimate = "est"),
    class = "sandtablR_error_triage_accuracy"
  )

  empty <- x
  empty$est <- NA_character_
  expect_error(
    sdt_triage_accuracy(empty, truth = "truth", estimate = "est"),
    class = "sandtablR_error_triage_accuracy"
  )
})

test_that("the default columns are the ones the wiring produces", {
  gold <- fixture_gold()
  co <- sdt_apply_gold_standard(fixture_cohort(), gold)

  acc <- sdt_triage_accuracy(co)
  meta <- attr(acc, "meta")

  expect_identical(meta$truth, "triage")
  expect_identical(meta$estimate, "called_triage")
  expect_identical(acc$n[acc$category == "overall"], 6L)
})

test_that("the header states what was scored against what", {
  acc <- sdt_triage_accuracy(scoring_frame(), truth = "truth",
                             estimate = "est")

  # Called directly: the S3 method is registered by roxygen, and the test
  # must pass before NAMESPACE is regenerated.
  header <- tbl_sum.sdt_triage_accuracy(acc)
  expect_match(header[["Triage accuracy"]], "truth vs est, n = 9")
  expect_match(header[["Overall"]], "kappa")
})
