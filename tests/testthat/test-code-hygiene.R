# Static checks over the package's own source.
#
# These run against R/ and inst/shiny/, which only exist when testing from a
# source tree, so they skip during R CMD check on an installed package. They
# are cheap insurance against mistakes that are invisible until a specific
# error path or a specific R version is hit.

pkg_root <- function() {
  root <- testthat::test_path("..", "..")
  if (dir.exists(file.path(root, "R"))) root else NA_character_
}

source_files <- function(subdir = "R") {
  root <- pkg_root()
  if (is.na(root)) {
    return(character())
  }
  list.files(
    file.path(root, subdir),
    pattern = "\\.R$", full.names = TRUE, recursive = TRUE
  )
}

read_lines_of <- function(files) {
  stats::setNames(lapply(files, readLines, warn = FALSE), basename(files))
}

test_that("no cli format string interpolates a dot-prefixed expression", {
  files <- source_files("R")
  skip_if(!length(files), "not running from a source tree")

  # cli >= 3.4 reads `{.foo` as the style `foo`. An internal helper such as
  # `{.sdt_life_range()}` therefore aborts at format time -- and only on the
  # error path, which is exactly where it will not be noticed. The fix is
  # always to bind the value to a local first.
  styles <- c(
    "and", "arg", "cls", "code", "dd", "dt", "email", "emph", "envvar",
    "field", "file", "fn", "fun", "help", "href", "key", "kbd",
    "obj_type_friendly", "or", "path", "pkg", "q", "run", "str", "strong",
    "timestamp", "topic", "type", "url", "val", "var", "vignette"
  )

  offenders <- character()
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    hits <- regmatches(lines, gregexpr("\\{\\.[A-Za-z_][A-Za-z0-9_.]*", lines))
    for (i in seq_along(hits)) {
      tokens <- sub("^\\{\\.", "", hits[[i]])
      bad <- setdiff(tokens, styles)
      if (length(bad)) {
        offenders <- c(
          offenders,
          sprintf("%s:%d: {.%s", basename(f), i, bad)
        )
      }
    }
  }
  expect_identical(offenders, character())
})

test_that("every cli pluralisation has a quantity to work from", {
  files <- source_files("R")
  skip_if(!length(files), "not running from a source tree")

  # `column{?s}` with no preceding `{quantity}` in the same string aborts with
  # "Cannot pluralize without a quantity" -- again only on the error path.
  offenders <- character()
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    for (i in seq_along(lines)) {
      if (!grepl("\\{\\?", lines[[i]], perl = TRUE)) next
      before <- sub("\\{\\?.*$", "", lines[[i]])
      if (!grepl("\\{[^?}]+\\}", before, perl = TRUE)) {
        offenders <- c(offenders, sprintf("%s:%d", basename(f), i))
      }
    }
  }
  expect_identical(offenders, character())
})

test_that("package and app code never calls library() or require()", {
  files <- c(source_files("R"), source_files("inst/shiny"))
  skip_if(!length(files), "not running from a source tree")

  offenders <- character()
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    code <- sub("#.*$", "", lines)
    hit <- grep("(^|[^a-zA-Z._])(library|require)\\s*\\(", code, perl = TRUE)
    if (length(hit)) {
      offenders <- c(offenders, sprintf("%s:%d", basename(f), hit))
    }
  }
  expect_identical(offenders, character())
})

test_that("no source line exceeds 80 characters", {
  files <- c(source_files("R"), source_files("inst/shiny"))
  skip_if(!length(files), "not running from a source tree")

  offenders <- character()
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    long <- which(nchar(lines, type = "chars") > 80L)
    if (length(long)) {
      offenders <- c(offenders, sprintf("%s:%d", basename(f), long))
    }
  }
  expect_identical(offenders, character())
})

test_that("code uses TRUE/FALSE and seq_len(), not T/F and 1:n", {
  files <- c(source_files("R"), source_files("inst/shiny"))
  skip_if(!length(files), "not running from a source tree")

  offenders <- character()
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    code <- sub("#.*$", "", lines)
    bare_tf <- grep("(^|[^a-zA-Z0-9._\"'])(T|F)([^a-zA-Z0-9._\"']|$)",
                    code, perl = TRUE)
    # `1:n` and `1:length(x)`; `1:3` on a literal is fine.
    one_to_n <- grep("(^|[^a-zA-Z0-9._])1:(length|nrow|ncol|n\\b)",
                     code, perl = TRUE)
    for (i in c(bare_tf, one_to_n)) {
      offenders <- c(offenders, sprintf("%s:%d: %s", basename(f), i,
                                        trimws(lines[[i]])))
    }
  }
  expect_identical(offenders, character())
})
