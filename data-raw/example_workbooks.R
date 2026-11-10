# Synthetic example workbooks used by the help-page examples.
#
# The real participant register and the real gold-standard codebook are
# personal or study data and are never shipped. The two workbooks written here
# reuse the invented fixtures of the test suite (tests/testthat), so they have
# the same shape as the real files without carrying any real content:
#
#   inst/extdata/example-participants.xlsx  for sdt_read_participants()
#   inst/extdata/example-codebook.xlsx      for sdt_read_gold_standard()
#
# Run from the package root: Rscript data-raw/example_workbooks.R

stopifnot(requireNamespace("writexl", quietly = TRUE))

# Evaluate only the top-level function definitions of a test file, so the
# fixtures are taken from one place and no test is run.
load_fixtures <- function(file, env) {
  exprs <- parse(file)
  for (e in exprs) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) &&
        is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) {
      eval(e, env)
    }
  }
  invisible(env)
}

fx <- new.env()
load_fixtures("tests/testthat/test-participants.R", fx)
load_fixtures("tests/testthat/test-gold-standard.R", fx)

out <- "inst/extdata"
tmp <- tempfile("sandtablR-workbooks-")
dir.create(tmp)

file.copy(fx$fixture_workbook(tmp),
          file.path(out, "example-participants.xlsx"), overwrite = TRUE)
file.copy(fx$write_codebook(tmp, file = "codebook.xlsx"),
          file.path(out, "example-codebook.xlsx"), overwrite = TRUE)
unlink(tmp, recursive = TRUE)
