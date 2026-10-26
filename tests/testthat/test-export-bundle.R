# The bundle writer is the artefact that leaves the building, so these tests
# lean on the refusals and on the honesty of what gets written, not only on
# the happy path.

fake_sim <- function(n_groups = 4L, cards = c("01", "02", "03"),
                     session = NULL) {
  grid <- expand.grid(
    card = cards,
    sim_group = sprintf("sim_%02d", seq_len(n_groups)),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  out <- tibble::tibble(
    sim_group = grid$sim_group,
    card = grid$card,
    efast = rep(c(TRUE, FALSE), length.out = nrow(grid)),
    died = rep(c(FALSE, FALSE, TRUE), length.out = nrow(grid))
  )
  if (!is.null(session)) out$session_id <- session
  out
}

fake_metrics <- function(verdict = c("PASS", "PASS")) {
  tibble::tibble(
    statistic = c("efast", "died")[seq_along(verdict)],
    observed = c(0.12, 0.03)[seq_along(verdict)],
    simulated = c(0.13, 0.03)[seq_along(verdict)],
    ratio = c(0.95, 1.02)[seq_along(verdict)],
    ci_lower = c(0.8, 0.85)[seq_along(verdict)],
    ci_upper = c(1.1, 1.3)[seq_along(verdict)],
    verdict = verdict,
    control = c(FALSE, TRUE)[seq_along(verdict)]
  )
}

read_member <- function(zipfile, pattern, envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = envir)
  utils::unzip(zipfile, exdir = dir)
  path <- list.files(dir, pattern, recursive = TRUE, full.names = TRUE)
  expect_length(path, 1L)
  path[[1L]]
}

test_that("a bundle is one archive holding the tables, README and manifest", {
  dir <- withr::local_tempdir()
  zipfile <- file.path(dir, "bundle.zip")
  out <- sdt_export_simulation(
    fake_sim(), zipfile, metrics = fake_metrics(), verbose = FALSE
  )
  expect_identical(out[[1L]], zipfile)
  expect_true(file.exists(zipfile))
  expect_setequal(
    attr(out, "members"),
    c("simulated-groups.csv", "fidelity-metrics.csv", "deck-cards.csv",
      "README.md", "manifest.json")
  )
  names <- basename(utils::unzip(zipfile, list = TRUE)$Name)
  expect_true(all(attr(out, "members") %in% names))
})

test_that("the archive lands on the path a downloadHandler hands over", {
  # Shiny supplies an extension-less temporary file, and Info-ZIP would
  # otherwise append ".zip" and leave the handler with nothing to serve.
  dir <- withr::local_tempdir()
  bare <- file.path(dir, "downloaded-file")
  out <- sdt_export_simulation(fake_sim(), bare, verbose = FALSE)
  expect_identical(out[[1L]], bare)
  expect_true(file.exists(bare))
  expect_false(file.exists(paste0(bare, ".zip")))
  expect_gt(nrow(utils::unzip(bare, list = TRUE)), 0L)
})

test_that("the archive path comes back spelled as the caller passed it", {
  # A canonicalised path names the same file but compares unequal to the one
  # handed over: macOS resolves its temporary directory through /private, and
  # Windows swaps the separators and expands short names. A "." segment and a
  # symlinked directory show the same thing on Linux.
  dir <- withr::local_tempdir()
  dotted <- file.path(dir, ".", "bundle.zip")
  out <- sdt_export_simulation(fake_sim(), dotted, verbose = FALSE)
  expect_identical(out[[1L]], dotted)
  expect_true(file.exists(dotted))

  skip_on_os("windows")
  real <- file.path(dir, "real")
  dir.create(real)
  link <- file.path(dir, "link")
  skip_if_not(file.symlink(real, link), "could not create a symlink")
  linked <- file.path(link, "bundle.zip")
  out <- sdt_export_simulation(fake_sim(), linked, verbose = FALSE)
  expect_identical(out[[1L]], linked)
  expect_true(file.exists(file.path(real, "bundle.zip")))
})

test_that("an existing bundle is never overwritten by accident", {
  dir <- withr::local_tempdir()
  zipfile <- file.path(dir, "bundle.zip")
  sdt_export_simulation(fake_sim(), zipfile, verbose = FALSE)
  expect_error(
    sdt_export_simulation(fake_sim(), zipfile, verbose = FALSE),
    class = "sandtablR_error_export_exists"
  )
  expect_no_error(
    sdt_export_simulation(fake_sim(), zipfile, overwrite = TRUE,
                          verbose = FALSE)
  )
})

test_that("a git working tree is refused unless the caller insists", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, ".git"))
  zipfile <- file.path(dir, "sub", "bundle.zip")
  expect_error(
    sdt_export_simulation(fake_sim(), zipfile, verbose = FALSE),
    class = "sandtablR_error_export_git"
  )
  expect_no_error(
    sdt_export_simulation(fake_sim(), zipfile, allow_git = TRUE,
                          verbose = FALSE)
  )
})

test_that("course identifiers are pseudonymised by default", {
  dir <- withr::local_tempdir()
  raw <- "session-BGLU2-2026-09-01T19-28-02-931.json"
  zipfile <- file.path(dir, "bundle.zip")
  key <- file.path(dir, "key.csv")
  sdt_export_simulation(
    fake_sim(session = raw), zipfile, crosswalk = key, verbose = FALSE
  )
  csv <- utils::read.csv(read_member(zipfile, "simulated-groups.csv"))
  expect_identical(unique(csv$session_id), "S01")
  expect_false(any(grepl(raw, readLines(
    read_member(zipfile, "README.md"), warn = FALSE
  ), fixed = TRUE)))
  cross <- utils::read.csv(key)
  expect_true(raw %in% cross$original)
  expect_identical(cross$pseudonym[cross$original == raw], "S01")
})

test_that("writing identifiers takes an explicit opt-in and is declared", {
  dir <- withr::local_tempdir()
  raw <- "session-BGLU2-2026-09-01T19-28-02-931.json"
  zipfile <- file.path(dir, "bundle.zip")
  sdt_export_simulation(
    fake_sim(session = raw), zipfile, pseudonymise = FALSE, verbose = FALSE
  )
  csv <- utils::read.csv(read_member(zipfile, "simulated-groups.csv"))
  expect_identical(unique(csv$session_id), raw)
  readme <- readLines(read_member(zipfile, "README.md"), warn = FALSE)
  expect_true(any(grepl("Pseudonymisation was switched off", readme)))
  expect_error(
    sdt_export_simulation(
      fake_sim(), file.path(dir, "other.zip"), pseudonymise = FALSE,
      crosswalk = file.path(dir, "key.csv"), verbose = FALSE
    ),
    class = "sandtablR_error_export"
  )
})

test_that("the crosswalk may not travel inside the bundle", {
  dir <- withr::local_tempdir()
  bundle <- file.path(dir, "bundle")
  expect_error(
    sdt_export_simulation(
      fake_sim(session = "session-X-2026-01-01T00-00-00-000.json"),
      bundle, zip = FALSE, crosswalk = file.path(bundle, "key.csv"),
      verbose = FALSE
    ),
    class = "sandtablR_error_export"
  )
})

test_that("the README states what the bundle is not", {
  dir <- withr::local_tempdir()
  zipfile <- file.path(dir, "bundle.zip")
  sdt_export_simulation(fake_sim(), zipfile, metrics = fake_metrics(),
                        verbose = FALSE)
  readme <- readLines(read_member(zipfile, "README.md"), warn = FALSE)
  expect_true(any(grepl("SYNTHETIC", readme)))
  expect_true(any(grepl("NOT THE PUBLISHED EXERCISE", readme)))
  expect_true(any(grepl("NO GROUND-TRUTH TRIAGE", readme)))
  expect_true(any(grepl("NOT THE DISCRETE-EVENT ENGINE", readme)))
})

test_that("a failing fidelity table is reported as failing", {
  dir <- withr::local_tempdir()
  zipfile <- file.path(dir, "bundle.zip")
  sdt_export_simulation(
    fake_sim(), zipfile, metrics = fake_metrics(c("FAIL", "PASS")),
    verbose = FALSE
  )
  readme <- readLines(read_member(zipfile, "README.md"), warn = FALSE)
  expect_true(any(grepl("1 of 2 statistics PASS and 1 FAIL", readme)))
  expect_true(any(grepl("FAILING: efast", readme)))
})

test_that("a bundle without metrics says it makes no fidelity claim", {
  dir <- withr::local_tempdir()
  zipfile <- file.path(dir, "bundle.zip")
  out <- sdt_export_simulation(fake_sim(), zipfile, verbose = FALSE)
  expect_false("fidelity-metrics.csv" %in% attr(out, "members"))
  readme <- readLines(read_member(zipfile, "README.md"), warn = FALSE)
  expect_true(any(grepl("No fidelity metrics were supplied", readme)))
})

test_that("nothing that came attached to the input is written", {
  # The real object carries the fitted model, and the model carries the real
  # course data. Writing it would ship the corpus inside a synthetic bundle.
  dir <- withr::local_tempdir()
  sim <- fake_sim()
  attr(sim, "model") <- list(
    data = data.frame(
      session_id = "session-SECRET-2026-01-01T00-00-00-000.json",
      group_id = "session-SECRET-2026-01-01T00-00-00-000.json/A",
      card = "01", stringsAsFactors = FALSE
    ),
    terms = list(group = "group_id", block = "session_id", card = "card")
  )
  zipfile <- file.path(dir, "bundle.zip")
  sdt_export_simulation(sim, zipfile, format = "rds", verbose = FALSE)
  back <- readRDS(read_member(zipfile, "simulated-groups.rds"))
  expect_null(attr(back, "model"))
  expect_setequal(names(back), names(sim))
})

test_that("the manifest checksums match the files that were written", {
  skip_if_not_installed("digest")
  skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  zipfile <- file.path(dir, "bundle.zip")
  sdt_export_simulation(fake_sim(), zipfile, metrics = fake_metrics(),
                        verbose = FALSE)
  manifest <- read_member(zipfile, "manifest.json")
  json <- jsonlite::fromJSON(paste(
    readLines(manifest, warn = FALSE), collapse = "\n"
  ))
  files <- json$checksums$files
  expect_false("manifest.json" %in% files$file)
  paths <- file.path(dirname(manifest), files$file)
  expect_true(all(file.exists(paths)))
  expect_identical(
    files$sha256,
    vapply(paths, digest::digest, character(1), algo = "sha256", file = TRUE,
           USE.NAMES = FALSE)
  )
  expect_identical(files$bytes, as.integer(file.info(paths)$size))
  expect_true(json$bundle$contents_are == "synthetic")
})

test_that("every format reads back what was handed over", {
  dir <- withr::local_tempdir()
  sim <- fake_sim()
  rds <- file.path(dir, "rds")
  sdt_export_simulation(sim, rds, format = "rds", zip = FALSE,
                        verbose = FALSE)
  back <- readRDS(file.path(rds, "simulated-groups.rds"))
  expect_equal(back, tibble::as_tibble(sim), ignore_attr = TRUE)

  csv <- file.path(dir, "csv")
  sdt_export_simulation(sim, csv, zip = FALSE, verbose = FALSE)
  text <- utils::read.csv(file.path(csv, "simulated-groups.csv"),
                          colClasses = "character")
  expect_identical(
    text$card, as.character(sim$card)
  )
  expect_identical(text$efast, as.character(sim$efast))

  skip_if_not_installed("nanoparquet")
  pq <- file.path(dir, "parquet")
  sdt_export_simulation(sim, pq, format = "parquet", zip = FALSE,
                        verbose = FALSE)
  expect_equal(
    as.data.frame(nanoparquet::read_parquet(
      file.path(pq, "simulated-groups.parquet")
    )),
    as.data.frame(sim)
  )
})

test_that("directory mode returns the member paths", {
  dir <- withr::local_tempdir()
  bundle <- file.path(dir, "bundle")
  out <- sdt_export_simulation(fake_sim(), bundle, zip = FALSE,
                               verbose = FALSE)
  expect_true(all(file.exists(out)))
  expect_true("README.md" %in% basename(out))
  expect_false(any(grepl("\\.zip$", out)))
})

test_that("a table that is not a simulation is refused", {
  dir <- withr::local_tempdir()
  expect_error(
    sdt_export_simulation(tibble::tibble(card = "01"),
                          file.path(dir, "b.zip"), verbose = FALSE),
    class = "sandtablR_error_export"
  )
  expect_error(
    sdt_export_simulation(fake_sim()[0, ], file.path(dir, "b.zip"),
                          verbose = FALSE),
    class = "sandtablR_error_export"
  )
  expect_error(
    sdt_export_simulation(fake_sim(), file.path(dir, "b.zip"),
                          metrics = data.frame(x = 1), verbose = FALSE),
    class = "sandtablR_error_export"
  )
})

test_that("the manifest is valid JSON and records the run", {
  skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  bundle <- file.path(dir, "bundle")
  sdt_export_simulation(fake_sim(n_groups = 5L), bundle, zip = FALSE,
                        metrics = fake_metrics(), verbose = FALSE)
  json <- jsonlite::fromJSON(paste(
    readLines(file.path(bundle, "manifest.json"), warn = FALSE),
    collapse = "\n"
  ))
  expect_identical(json$simulation$n_groups, 5L)
  expect_identical(json$simulation$n_cards, 3L)
  expect_true(json$privacy$pseudonymised)
  expect_false(json$privacy$crosswalk_written)
  expect_identical(json$fidelity$n_pass, 2L)
  expect_true(any(grepl("SYNTHETIC", json$statements)))
  expect_identical(
    json$tables$file,
    c("simulated-groups.csv", "fidelity-metrics.csv", "deck-cards.csv")
  )
})

test_that("an unrecognised column holding an identifier is caught", {
  # The column-name rules cannot know every spelling, so the written bytes
  # are searched for the identifier shape as well.
  dir <- withr::local_tempdir()
  sim <- fake_sim()
  sim$course <- "session-BGLU2-2026-09-01T19-28-02-931.json"
  expect_error(
    sdt_export_simulation(sim, file.path(dir, "b.zip"), verbose = FALSE),
    class = "sandtablR_error_export_identity"
  )
  expect_no_error(
    sdt_export_simulation(sim, file.path(dir, "c.zip"), pseudonymise = FALSE,
                          verbose = FALSE)
  )
})
