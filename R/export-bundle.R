# Download bundles ------------------------------------------------------------
#
# What leaves the building is a single `.zip`, because the thing on the other
# end is usually a `shiny::downloadHandler()`: it hands the writer exactly one
# file path and expects the file to exist when the function returns. A
# directory cannot be served that way, so the archive is the primary artefact
# and `zip = FALSE` is the escape hatch for scripted use, where an unpacked
# directory is easier to read.
#
# Three rules are enforced rather than merely documented, because each guards a
# mistake that is silent at the time and expensive later:
#
# * synthetic rows must never be committed by accident, so a destination inside
#   a git working tree is refused unless the caller says otherwise;
# * course identifiers carry hospital, venue, city and the exact course date,
#   so they are pseudonymised by default and the crosswalk never travels inside
#   the bundle;
# * the bundle has to state what it is *not* -- not the published exercise, not
#   ground truth, not real observations -- in both the README and the manifest,
#   so the caveats survive the file being forwarded to someone who never saw
#   this session.

#' Export a simulation run as a self-describing bundle
#'
#' Writes a simulated cohort, the fidelity metrics that judge it, the deck it
#' was drawn over, a README and a `manifest.json` into one archive that can be
#' handed to someone who was not in the room. The archive is the return value,
#' so the whole function is a one-liner inside a Shiny `downloadHandler()`.
#'
#' # What the bundle contains
#'
#' * `simulated-groups.*` — one row per simulated group and card.
#' * `fidelity-metrics.*` — the output of [sdt_fidelity()], when supplied.
#' * `deck-cards.*` — the cards the simulation ranged over.
#' * `README.md` — generated at write time; carries the caveats.
#' * `manifest.json` — versions, seed, frame, model terms, random-effect SDs,
#'   skipped outcomes, the privacy scheme, and a SHA-256 of every other file.
#'
#' # Refusals
#'
#' The writer aborts rather than doing something quietly regrettable:
#'
#' * an existing destination is never overwritten unless `overwrite = TRUE`;
#' * a destination inside a git working tree is refused unless
#'   `allow_git = TRUE`, because synthetic rows that get committed are read as
#'   real data by the next person;
#' * with `pseudonymise = TRUE` the written bytes are searched for the original
#'   identifiers before the file is closed, and a survivor is an error.
#'
#' # Pseudonymisation
#'
#' Session identifiers and scenario names in this domain embed hospital, venue
#' and city names and the exact course date. They are therefore mapped to
#' `S01`…`Snn`, groups to `G001`…`Gnnn` and scenarios to `SC01`…`SCnn` by
#' default. The crosswalk is written only if `crosswalk` names a path, and that
#' path must lie outside the bundle; otherwise the mapping is discarded when
#' the function returns and the bundle cannot be re-identified from itself.
#'
#' @param x A `sdt_simulated_groups` table from [sdt_simulate_groups()], or any
#'   data frame with `sim_group` and `card` columns. Model provenance is read
#'   from `attr(x, "model")` or `attr(x, "meta")` when present.
#' @param path Destination. With `zip = TRUE` the archive is written at exactly
#'   this path — pass a `.zip` name in a script, or the `file` argument of a
#'   `downloadHandler()` unchanged. With `zip = FALSE` it is the directory to
#'   fill.
#' @param format Storage format for the tables: `"csv"` (widest
#'   compatibility), `"parquet"` (via \pkg{nanoparquet}) or `"rds"`
#'   (lossless R). The README and manifest are always plain text.
#' @param metrics A fidelity table from [sdt_fidelity()]. Defaults to
#'   `attr(x, "fidelity")` when that is present. A bundle without metrics is
#'   written, and says in as many words that it makes no fidelity claim.
#' @param overwrite Overwrite an existing destination?
#' @param pseudonymise Replace course identifiers with stable codes? Defaults
#'   to `TRUE`; see the section above before turning it off.
#' @param crosswalk Optional path, outside the bundle, for the pseudonym
#'   crosswalk as CSV. `NULL` discards the mapping.
#' @param allow_git Permit a destination inside a git working tree?
#' @param zip Write a single archive (`TRUE`) or a directory (`FALSE`)?
#' @param verbose If `TRUE`, report what was written via \pkg{cli}.
#'
#' @return Invisibly, the written paths: the archive when `zip = TRUE`, the
#'   member files otherwise. The archive path is always the first element, and
#'   carries the member names in `attr(., "members")`.
#' @seealso [sdt_fidelity()] for the metrics, [sdt_simulate_groups()] for the
#'   input.
#' @export
#' @examples
#' sim <- tibble::tibble(
#'   sim_group = rep(c("sim_01", "sim_02"), each = 2L),
#'   card = rep(c("01", "02"), 2L),
#'   red = c(TRUE, FALSE, TRUE, TRUE)
#' )
#' out <- sdt_export_simulation(
#'   sim,
#'   tempfile("demo-bundle-", fileext = ".zip"),
#'   verbose = FALSE
#' )
#' utils::unzip(out, list = TRUE)$Name
#'
#' # In Shiny, the handler's `file` is the archive path:
#' # downloadHandler(
#' #   filename = function() "simulation-bundle.zip",
#' #   content = function(file) {
#' #     sdt_export_simulation(sim, file, metrics = fid, verbose = FALSE)
#' #   }
#' # )
sdt_export_simulation <- function(x, path,
                                  format = c("csv", "parquet", "rds"),
                                  metrics = NULL, overwrite = FALSE,
                                  pseudonymise = TRUE, crosswalk = NULL,
                                  allow_git = FALSE, zip = TRUE,
                                  verbose = TRUE) {
  format <- rlang::arg_match(format)
  .check_string(path)
  .check_flag(overwrite)
  .check_flag(pseudonymise)
  .check_flag(allow_git)
  .check_flag(zip)
  .check_flag(verbose)
  if (!is.null(crosswalk)) {
    .check_string(crosswalk)
    if (!pseudonymise) {
      cli::cli_abort(
        c(
          "{.arg crosswalk} needs {.code pseudonymise = TRUE}.",
          "i" = "Without pseudonyms there is nothing to cross-reference."
        ),
        class = "sandtablR_error_export"
      )
    }
  }
  if (identical(format, "parquet")) .require_pkgs("nanoparquet")

  sim <- .bundle_simulation(x)
  model <- .bundle_model(x)
  met <- .bundle_metrics(metrics, x)
  deck <- .bundle_deck(sim, model)
  tables <- list(
    "simulated-groups" = sim,
    "fidelity-metrics" = met,
    "deck-cards" = deck
  )
  tables <- tables[!vapply(tables, is.null, logical(1))]
  tables <- lapply(tables, .plain_table)

  target <- .export_target(path, zip)
  members <- c(paste0(names(tables), ".", format), "README.md",
               "manifest.json")
  .check_export_git(target$anchor, allow_git)
  .check_export_free(target, members, overwrite)

  meta <- .bundle_meta(sim, met, model, format, pseudonymise, crosswalk)
  map <- character()
  if (pseudonymise) {
    map <- .bundle_id_map(tables, model)
    tables <- lapply(tables, .pseudonymise_table, map = map)
    meta <- .map_recursive(meta, map[nchar(names(map)) >= 4L])
  }
  meta$privacy$n_identifiers_mapped <- length(map)
  meta$tables <- .table_schema(tables, format)

  created <- .export_prepare(target)
  ok <- FALSE
  on.exit(.export_cleanup(target, created, ok), add = TRUE)

  readme <- .bundle_readme(meta, tables)
  .check_no_identifiers(
    c(unlist(lapply(tables, .table_strings), use.names = FALSE), readme),
    names(map), pseudonymise
  )
  files <- vapply(
    names(tables),
    function(nm) .write_table(tables[[nm]], nm, target$dir, format),
    character(1), USE.NAMES = FALSE
  )
  files <- c(files, .write_text(readme, file.path(target$dir, "README.md")))
  manifest <- .bundle_manifest(meta, files)
  .check_no_identifiers(manifest, names(map), pseudonymise)
  files <- c(
    files,
    .write_text(manifest, file.path(target$dir, "manifest.json"))
  )
  cw <- .write_crosswalk(map, crosswalk, target, overwrite)

  out <- if (zip) .zip_bundle(target) else files
  attr(out, "members") <- basename(files)
  ok <- TRUE
  if (verbose) .export_report(out, files, cw, meta)
  invisible(out)
}

# Destination ------------------------------------------------------------------

# With `zip = TRUE` the caller's path is honoured literally, because Shiny
# hands over an extension-less temporary file and the archive has to land on
# exactly that path. The staging directory is named after the archive so the
# zip unpacks into one folder rather than scattering five files.
.export_target <- function(path, zip) {
  path <- path.expand(path)
  if (!zip) {
    return(list(
      zip = NA_character_, dir = path, staging = NA_character_,
      staged = FALSE, anchor = path
    ))
  }
  stem <- sub("\\.zip$", "", basename(path), ignore.case = TRUE)
  stem <- gsub("[^A-Za-z0-9._-]+", "-", stem)
  # A Shiny handler passes `tempfile()`, whose basename is "fileXXXXX". The
  # person who unzips the download should not be met by that.
  if (!nzchar(stem) || grepl("^file[0-9a-f]{6,}$", stem)) {
    stem <- "sandtablR-simulation"
  }
  staging <- tempfile("sdt-bundle-")
  list(
    zip = path, dir = file.path(staging, stem), staging = staging,
    staged = TRUE, anchor = dirname(path)
  )
}

.export_prepare <- function(target) {
  parent <- if (isTRUE(target$staged)) dirname(target$zip) else target$dir
  if (!dir.exists(parent)) {
    dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  }
  created <- !dir.exists(target$dir)
  if (created) dir.create(target$dir, recursive = TRUE)
  created
}

.export_cleanup <- function(target, created, ok) {
  if (isTRUE(target$staged)) {
    unlink(target$staging, recursive = TRUE, force = TRUE)
  } else if (!ok && isTRUE(created)) {
    unlink(target$dir, recursive = TRUE, force = TRUE)
  }
  invisible(NULL)
}

.check_export_free <- function(target, members, overwrite,
                               call = rlang::caller_env()) {
  clash <- if (isTRUE(target$staged)) {
    target$zip[file.exists(target$zip)]
  } else {
    file.path(target$dir, members)[
      file.exists(file.path(target$dir, members))
    ]
  }
  if (!length(clash) || isTRUE(overwrite)) {
    return(invisible(TRUE))
  }
  n_clash <- length(clash)
  shown <- basename(utils::head(clash, 5L))
  cli::cli_abort(
    c(
      "Refusing to overwrite an existing bundle.",
      "x" = "{n_clash} file{?s} already there: {.file {shown}}.",
      "i" = "Pass {.code overwrite = TRUE} to replace them."
    ),
    class = "sandtablR_error_export_exists",
    call = call
  )
}

# Synthetic rows that get committed are read as real data by the next person to
# open the repository, so a git working tree is refused by default. The walk is
# at most a few dozen `file.exists()` calls and stops at the filesystem root.
.check_export_git <- function(anchor, allow_git,
                              call = rlang::caller_env()) {
  if (isTRUE(allow_git)) {
    return(invisible(FALSE))
  }
  root <- .find_git_root(.existing_dir(anchor))
  if (is.null(root)) {
    return(invisible(FALSE))
  }
  cli::cli_abort(
    c(
      "Refusing to write synthetic data into a git working tree.",
      "x" = "{.path {root}} is a git repository.",
      "i" = "Simulated rows committed by accident read as real data later.",
      "i" = "Write outside the repository, or pass {.code allow_git = TRUE}."
    ),
    class = "sandtablR_error_export_git",
    call = call
  )
}

.find_git_root <- function(dir, max_up = 60L) {
  for (i in seq_len(max_up)) {
    if (file.exists(file.path(dir, ".git"))) {
      return(dir)
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) break
    dir <- parent
  }
  NULL
}

.existing_dir <- function(p) {
  p <- normalizePath(p, winslash = "/", mustWork = FALSE)
  while (!dir.exists(p)) {
    parent <- dirname(p)
    if (identical(parent, p)) break
    p <- parent
  }
  p
}

# The archive is built under a temporary `.zip` name and only then moved onto
# the caller's path, because Info-ZIP appends `.zip` to an extension-less
# archive name -- and an extension-less path is exactly what a Shiny
# `downloadHandler()` hands over.
.zip_bundle <- function(target, call = rlang::caller_env()) {
  dest <- file.path(
    normalizePath(dirname(target$zip), winslash = "/", mustWork = FALSE),
    basename(target$zip)
  )
  tmp <- tempfile("sdt-archive-", fileext = ".zip")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  member <- basename(target$dir)
  cmd <- Sys.getenv("R_ZIPCMD", "zip")
  if (nzchar(Sys.which(cmd))) {
    withr::with_dir(
      target$staging,
      suppressWarnings(utils::zip(tmp, member, flags = "-r9Xq"))
    )
  } else if (requireNamespace("zip", quietly = TRUE)) {
    withr::with_dir(target$staging, zip::zipr(tmp, member))
  }
  if (!file.exists(tmp)) {
    cli::cli_abort(
      c(
        "Could not create the archive.",
        "x" = "No usable zip program was found.",
        "i" = "Install the {.pkg zip} package, set {.envvar R_ZIPCMD},",
        "i" = "or call with {.code zip = FALSE} for a plain directory."
      ),
      class = "sandtablR_error_export_zip",
      call = call
    )
  }
  if (file.exists(dest)) unlink(dest, force = TRUE)
  moved <- suppressWarnings(file.rename(tmp, dest))
  if (!moved) moved <- file.copy(tmp, dest, overwrite = TRUE)
  if (!moved) {
    cli::cli_abort(
      c(
        "Could not move the archive into place.",
        "x" = "{.path {dest}} could not be written."
      ),
      class = "sandtablR_error_export_zip",
      call = call
    )
  }
  # `dest` is the same file, but normalizePath() spells it differently on
  # macOS (/private/var) and Windows (separators, long names), and the
  # documented return value is the caller's path.
  target$zip
}

.export_report <- function(out, files, cw, meta) {
  size <- .fmt_bytes(sum(file.info(out)$size, na.rm = TRUE))
  n_files <- length(files)
  target <- out[[1L]]
  cli::cli_inform(c(
    "v" = "Wrote {.file {target}} ({size}, {n_files} member{?s}).",
    "!" = "Rows are synthetic; never pool them with real observations."
  ))
  if (!is.na(cw)) {
    cli::cli_inform(c("i" = "Crosswalk written to {.file {cw}}."))
  } else if (isTRUE(meta$privacy$pseudonymised)) {
    cli::cli_inform(c(
      "i" = "Pseudonym crosswalk discarded; the bundle cannot be re-linked."
    ))
  }
  invisible(NULL)
}

.fmt_bytes <- function(n) {
  if (!is.finite(n)) {
    return("unknown size")
  }
  if (n >= 1e6) {
    return(paste0(formatC(n / 1e6, digits = 1L, format = "f"), " MB"))
  }
  paste0(formatC(n / 1e3, digits = 1L, format = "f"), " kB")
}

# Inputs -----------------------------------------------------------------------
#
# Everything below reads the simulation object defensively. The bundle writer
# is downstream of the model and simulator, and a bundle that refuses to be
# written because an upstream object grew a field is worse than a bundle that
# records "not supplied" in the manifest. Nothing here is invented: a field
# that is absent is reported as absent.

.bundle_simulation <- function(x, call = rlang::caller_env()) {
  if (is.list(x) && !is.data.frame(x) && is.data.frame(x$groups)) {
    x <- x$groups
  }
  if (!is.data.frame(x)) {
    cli::cli_abort(
      c(
        "{.arg x} must be a simulated-groups table.",
        "x" = "Got {.cls {class(x)[[1]]}} instead."
      ),
      class = "sandtablR_error_export",
      call = call
    )
  }
  miss <- setdiff(c("sim_group", "card"), names(x))
  if (length(miss)) {
    n_miss <- length(miss)
    cli::cli_abort(
      c(
        "{.arg x} must be a simulated-groups table.",
        "x" = "Missing {n_miss} column{?s}: {.field {miss}}.",
        "i" = "{.fun sdt_simulate_groups} returns them."
      ),
      class = "sandtablR_error_export",
      call = call
    )
  }
  if (!nrow(x)) {
    cli::cli_abort(
      c("{.arg x} has no rows.", "i" = "There is nothing to export."),
      class = "sandtablR_error_export",
      call = call
    )
  }
  tibble::as_tibble(x)
}

.bundle_model <- function(x) {
  cand <- list(
    attr(x, "model", exact = TRUE),
    attr(x, "behaviour_model", exact = TRUE)
  )
  meta <- attr(x, "meta", exact = TRUE)
  if (is.list(meta)) cand <- c(cand, list(meta$model))
  if (is.list(x) && !is.data.frame(x)) cand <- c(cand, list(x$model))
  for (v in cand) {
    if (is.list(v) && length(v)) {
      return(v)
    }
  }
  NULL
}

.bundle_metrics <- function(metrics, x, call = rlang::caller_env()) {
  if (is.null(metrics)) metrics <- attr(x, "fidelity", exact = TRUE)
  if (is.null(metrics)) {
    return(NULL)
  }
  if (!is.data.frame(metrics) || !nrow(metrics)) {
    cli::cli_abort(
      c(
        "{.arg metrics} must be a fidelity table with at least one row.",
        "i" = "{.fun sdt_fidelity} returns one."
      ),
      class = "sandtablR_error_export",
      call = call
    )
  }
  if (!"statistic" %in% names(metrics)) {
    cli::cli_abort(
      c(
        "{.arg metrics} must have a {.field statistic} column.",
        "x" = "Got: {.field {names(metrics)}}."
      ),
      class = "sandtablR_error_export",
      call = call
    )
  }
  tibble::as_tibble(metrics)
}

# The deck table exists so a reader can see what the simulation ranged over
# without opening the model. Observed coverage is joined in when the model
# carried its fitting frame, and left out when it did not.
.bundle_deck <- function(sim, model) {
  cards <- as.character(sim$card)
  tab <- table(cards)
  out <- tibble::tibble(
    card = names(tab),
    n_sim_rows = as.integer(tab)
  )
  out$n_sim_groups <- vapply(
    out$card,
    function(cd) length(unique(sim$sim_group[cards == cd])),
    integer(1), USE.NAMES = FALSE
  )
  dat <- model$data
  cols <- .model_cols(model, dat)
  if (is.data.frame(dat) && !is.null(cols$card) && !is.null(cols$group)) {
    obs <- as.character(dat[[cols$card]])
    grp <- as.character(dat[[cols$group]])
    out$n_observed_groups <- vapply(
      out$card,
      function(cd) length(unique(grp[obs == cd])),
      integer(1), USE.NAMES = FALSE
    )
    out$in_model_frame <- out$card %in% unique(obs)
  }
  out[order(out$card), ]
}

# Resolve the column each role lives in. The contract names them, but the
# object may carry them under `terms` or `meta`, so both are consulted and the
# answer is checked against the data actually present.
.model_cols <- function(model, dat = model$data) {
  fallback <- list(
    group = c("group_id", "gid", "group", "team_id", "team"),
    block = c("session_id", "sid", "session", "block_id", "block", "file"),
    card = c("card", "card_id")
  )
  named <- model$terms
  if (!is.list(named)) named <- list()
  meta <- model$meta
  if (!is.list(meta)) meta <- list()
  out <- lapply(names(fallback), function(role) {
    given <- named[[role]] %||% meta[[role]]
    cand <- c(
      if (is.character(given)) given else character(),
      fallback[[role]]
    )
    if (!is.data.frame(dat)) {
      return(if (length(cand)) cand[[1L]] else NULL)
    }
    hit <- cand[cand %in% names(dat)]
    if (length(hit)) hit[[1L]] else NULL
  })
  stats::setNames(out, names(fallback))
}

.model_re_sds <- function(model) {
  pre <- model$meta$sds %||% model$meta$re_sd %||% model$re_sd
  if (is.data.frame(pre) && nrow(pre)) {
    return(tibble::as_tibble(pre))
  }
  fits <- model$fits
  if (!is.list(fits) || !length(fits)) {
    return(NULL)
  }
  if (!requireNamespace("lme4", quietly = TRUE)) {
    return(NULL)
  }
  rows <- lapply(names(fits), function(nm) {
    fit <- fits[[nm]]
    if (!inherits(fit, "merMod")) {
      return(NULL)
    }
    vc <- tryCatch(
      as.data.frame(lme4::VarCorr(fit)),
      error = function(e) NULL
    )
    if (is.null(vc) || !nrow(vc)) {
      return(NULL)
    }
    vc <- vc[is.na(vc$var2) & vc$grp != "Residual", , drop = FALSE]
    tibble::tibble(
      outcome = nm, grouping = as.character(vc$grp),
      sd = as.numeric(vc$sdcor)
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) {
    return(NULL)
  }
  dplyr::bind_rows(rows)
}

# Outcomes the caller asked for that are not in the bundle, with the reason if
# the model recorded one. Silence here would be the dishonest option: an
# outcome that vanished between the request and the file is exactly what a
# reader needs to be told about.
.bundle_skipped <- function(model, sim) {
  pre <- model$meta$skipped %||% model$skipped
  out <- NULL
  if (is.data.frame(pre) && nrow(pre)) {
    out <- tibble::as_tibble(pre)
  } else if (is.character(pre) && length(pre)) {
    nms <- names(pre)
    out <- if (is.null(nms)) {
      tibble::tibble(outcome = pre, reason = "recorded without a reason")
    } else {
      tibble::tibble(outcome = nms, reason = unname(pre))
    }
  }
  asked <- model$outcomes
  fitted <- names(model$fits)
  if (is.character(asked) && is.character(fitted)) {
    gone <- setdiff(asked, fitted)
    if (length(gone)) {
      out <- dplyr::bind_rows(
        out,
        tibble::tibble(outcome = gone, reason = "requested but never fitted")
      )
    }
  }
  if (is.character(fitted)) {
    dropped <- setdiff(fitted, names(sim))
    if (length(dropped)) {
      out <- dplyr::bind_rows(
        out,
        tibble::tibble(
          outcome = dropped,
          reason = "fitted but absent from the simulated groups"
        )
      )
    }
  }
  if (is.null(out) || !nrow(out)) {
    return(NULL)
  }
  out[!duplicated(out$outcome), ]
}

# Column honesty labels, if the upstream object carried them. Accepted either
# as a data frame with `column` and `status`, or as a named character vector.
.bundle_provenance <- function(x, model) {
  cand <- list(
    attr(x, "provenance", exact = TRUE),
    model$meta$provenance,
    model$provenance,
    attr(model$data, "provenance", exact = TRUE)
  )
  for (v in cand) {
    if (is.data.frame(v) && nrow(v) && "column" %in% names(v)) {
      return(tibble::as_tibble(v))
    }
    if (is.character(v) && length(v) && !is.null(names(v))) {
      return(tibble::tibble(column = names(v), status = unname(v)))
    }
  }
  NULL
}

.outcome_columns <- function(sim, model) {
  structural <- c(
    "sim_group", "card", "sim", ".sim", "sim_id", "nsim", "rep",
    "replicate", ".replicate", "n", "row_id"
  )
  cand <- setdiff(names(sim), structural)
  known <- model$outcomes
  if (is.character(known) && length(known)) {
    hit <- intersect(known, cand)
    if (length(hit)) {
      return(hit)
    }
  }
  cand
}

.sim_attr <- function(x, name) {
  v <- attr(x, name, exact = TRUE)
  if (!is.null(v)) {
    return(v)
  }
  meta <- attr(x, "meta", exact = TRUE)
  if (is.list(meta)) meta[[name]] else NULL
}

.scalar_or_na <- function(v) {
  if (is.null(v) || !length(v)) {
    return(NA)
  }
  if (length(v) > 1L) {
    return(paste(as.character(v), collapse = ", "))
  }
  v[[1L]]
}

.n_unique <- function(dat, col) {
  if (!is.data.frame(dat) || is.null(col) || !col %in% names(dat)) {
    return(NULL)
  }
  length(unique(dat[[col]]))
}

.bundle_meta <- function(sim, met, model, fmt, pseudonymise, crosswalk) {
  version <- tryCatch(
    as.character(utils::packageVersion("sandtablR")),
    error = function(e) NA_character_
  )
  outcomes <- .outcome_columns(sim, model)
  frame <- model$meta$frame
  if (!is.list(frame)) frame <- model$frame
  frame_meta <- list(
    definition = .scalar_or_na(
      (if (is.list(frame)) frame$type else frame) %||% model$frame
    ),
    min_coverage = .scalar_or_na(
      (if (is.list(frame)) frame$min_coverage else NULL) %||%
        model$meta$min_coverage
    ),
    n_cards = .scalar_or_na(
      model$meta$n_cards %||% .n_unique(model$data, .model_cols(model)$card)
    ),
    n_groups = .scalar_or_na(
      model$meta$n_groups %||% .n_unique(model$data, .model_cols(model)$group)
    ),
    n_blocks = .scalar_or_na(
      model$meta$n_blocks %||% .n_unique(model$data, .model_cols(model)$block)
    ),
    n_rows = .scalar_or_na(model$meta$n_rows %||% nrow(model$data)),
    why_it_matters = paste(
      "Between-group dispersion must be computed on a composition-matched",
      "frame. On the full deck each group sees a different card set, and",
      "that composition variance swamps the team effect."
    )
  )
  list(
    bundle = list(
      tool = "sandtablR",
      sandtablR_version = version,
      r_version = R.version.string,
      platform = R.version$platform,
      created = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      format = fmt,
      contents_are = "synthetic"
    ),
    simulation = list(
      n_groups = length(unique(sim$sim_group)),
      n_cards = length(unique(as.character(sim$card))),
      n_rows = nrow(sim),
      outcomes = .json_arr(outcomes),
      seed = .scalar_or_na(.sim_attr(sim, "seed")),
      new_teams = .scalar_or_na(.sim_attr(sim, "new_teams")),
      n_replicates = .scalar_or_na(
        .sim_attr(sim, "nsim") %||% .n_unique(sim, ".replicate")
      ),
      note = paste(
        "sim_group is unique across replicates, so grouping by it never",
        "merges two simulated teams."
      )
    ),
    model = list(
      available = !is.null(model),
      engine = .scalar_or_na(model$meta$engine %||% model$engine),
      terms = .scalar_or_na(.model_formula(model)),
      columns = lapply(.model_cols(model), .scalar_or_na),
      frame = frame_meta,
      outcomes_modelled = .json_arr(as.character(model$outcomes)),
      outcomes_skipped = .bundle_skipped(model, sim) %||% .json_arr(list()),
      random_effect_sd = .model_re_sds(model) %||% .json_arr(list())
    ),
    fidelity = .fidelity_meta(met),
    privacy = list(
      pseudonymised = pseudonymise,
      scheme = if (pseudonymise) {
        "sessions S01+, groups G001+, scenarios SC01+"
      } else {
        "none: original identifiers were written on request"
      },
      crosswalk_written = !is.null(crosswalk),
      crosswalk_path = .scalar_or_na(crosswalk),
      n_identifiers_mapped = NA_integer_
    ),
    provenance = list(
      available = !is.null(.bundle_provenance(sim, model)),
      columns = .bundle_provenance(sim, model) %||% .json_arr(list())
    ),
    statements = .json_arr(.bundle_statements(met))
  )
}

.model_formula <- function(model) {
  f <- model$terms
  if (is.list(f)) f <- f$formula %||% f$terms
  if (inherits(f, "formula")) {
    return(paste(deparse(f), collapse = " "))
  }
  if (is.character(f) && length(f)) {
    return(paste(f, collapse = " "))
  }
  fits <- model$fits
  if (is.list(fits) && length(fits)) {
    ff <- tryCatch(stats::formula(fits[[1L]]), error = function(e) NULL)
    if (inherits(ff, "formula")) {
      return(paste(deparse(ff), collapse = " "))
    }
  }
  NULL
}

# The manifest records each column's R type, because CSV does not carry one:
# a card labelled "08" comes back as the number 8 from any reader that guesses.
.table_schema <- function(tables, fmt) {
  out <- lapply(names(tables), function(nm) {
    tb <- tables[[nm]]
    list(
      name = nm,
      file = paste0(nm, ".", fmt),
      n_rows = nrow(tb),
      columns = .json_arr(lapply(names(tb), function(cn) {
        list(name = cn, type = class(tb[[cn]])[[1L]])
      }))
    )
  })
  .json_arr(out)
}

# A simulated-groups object carries the fitted model, the call and the draws
# as attributes, and the model carries the frame it was fitted on -- real
# session identifiers and all. `saveRDS()` on the object as handed over would
# therefore ship the entire course corpus inside a bundle whose README says it
# is synthetic. Every table is reduced to its columns first, so what is written
# is the data and nothing that came attached to it.
.plain_table <- function(tbl) {
  cols <- stats::setNames(lapply(seq_along(tbl), function(i) tbl[[i]]),
                          names(tbl))
  tibble::as_tibble(cols)
}

# Fidelity summary -------------------------------------------------------------
#
# A bundle whose metrics say the simulation is poor has to say so in the first
# paragraph of its README. The counts below drive both the README and the
# manifest, so the two can never disagree.

.fidelity_counts <- function(met) {
  out <- list(
    n = if (is.null(met)) 0L else nrow(met),
    has_verdict = !is.null(met) && "verdict" %in% names(met),
    n_pass = NA_integer_, n_fail = NA_integer_, n_control = NA_integer_,
    failing = character()
  )
  if (is.null(met)) {
    return(out)
  }
  if ("control" %in% names(met)) {
    out$n_control <- sum(as.logical(met$control), na.rm = TRUE)
  }
  if (out$has_verdict) {
    v <- toupper(as.character(met$verdict))
    out$n_pass <- sum(v == "PASS", na.rm = TRUE)
    out$n_fail <- sum(v == "FAIL", na.rm = TRUE)
    out$failing <- as.character(met$statistic)[which(v == "FAIL")]
  }
  out
}

.verdict_text <- function(met) {
  cnt <- .fidelity_counts(met)
  if (!cnt$n) {
    return(c(paste(
      "**No fidelity metrics were supplied**, so this bundle makes no claim",
      "that the simulated groups resemble real ones. Run `sdt_fidelity()` on",
      "the simulation and export again before anyone leans on these rows."
    )))
  }
  if (!cnt$has_verdict) {
    return(c(paste(
      "The fidelity table carried no `verdict` column, so this bundle cannot",
      "say whether the simulation passed. Treat it as unjudged."
    )))
  }
  head <- sprintf(
    paste(
      "**Dispersion verdict: %d of %d statistics PASS and %d FAIL.**",
      "Ratios are observed divided by simulated. A ratio near 1 means the",
      "simulator reproduces the real spread between groups; a ratio well",
      "above 1 means it makes groups that are far too alike."
    ),
    cnt$n_pass, cnt$n, cnt$n_fail
  )
  out <- head
  if (cnt$n_fail > 0L) {
    out <- c(out, sprintf(
      paste(
        "FAILING: %s. There the simulated groups do not reproduce the",
        "observed spread, and any number computed from them is wrong in a",
        "way this bundle has measured. Do not use them."
      ),
      paste(cnt$failing, collapse = ", ")
    ))
  } else {
    out <- c(out, paste(
      "Nothing failed. That is evidence about the statistics listed and",
      "about nothing else."
    ))
  }
  if (!is.na(cnt$n_control) && cnt$n_control > 0L) {
    out <- c(out, sprintf(
      paste(
        "%d of them %s a negative control, expected to pass by",
        "construction. It carries no evidence about the team effect and",
        "must not be quoted as if it did."
      ),
      cnt$n_control, if (cnt$n_control == 1L) "is" else "are"
    ))
  }
  out
}

.bundle_statements <- function(met) {
  c(
    paste(
      "SYNTHETIC. Every row of the simulated groups file was drawn from a",
      "fitted model. Not one of them is an observation of a real team.",
      "Never pool, append or join them with real course data, and never",
      "report a count from this bundle as a count of something that",
      "happened."
    ),
    paste(
      "NOT THE PUBLISHED EXERCISE. The corpus behind the model is the",
      "46-card, roughly 10-round High Volume variant of the TDSC tabletop.",
      "The published evaluation (Achatz et al. 2023) describes a 20-card,",
      "4-round, 90-minute exercise. Marginals from this bundle are not",
      "comparable with the published ones."
    ),
    paste(
      "NO GROUND-TRUTH TRIAGE. The corpus records what a team called, never",
      "what was correct. A consensus triage label is the modal call across",
      "teams and is therefore circular: scoring against it measures",
      "agreement between teams, never accuracy. The clinical content of the",
      "cards is absent from the export entirely."
    ),
    paste(
      "NOT THE DISCRETE-EVENT ENGINE. These rows come from a fitted",
      "behaviour model, not from sdt_simulate(). Measured on the reference",
      "corpus, the engine's spread between replications is far narrower",
      "than the spread between real teams, so the two are not",
      "interchangeable and their outputs must not be mixed."
    ),
    paste(.verdict_text(met), collapse = " ")
  )
}

.fidelity_meta <- function(met) {
  cnt <- .fidelity_counts(met)
  list(
    available = cnt$n > 0L,
    n_statistics = cnt$n,
    n_pass = cnt$n_pass,
    n_fail = cnt$n_fail,
    n_control = cnt$n_control,
    failing = .json_arr(cnt$failing),
    verdict_in_words = .json_arr(.verdict_text(met))
  )
}

# Pseudonymisation -------------------------------------------------------------
#
# Session identifiers and scenario names in this corpus embed hospital, venue
# and city names and the exact course date, so they are mapped by default and
# the crosswalk is written outside the bundle or not at all.

.id_columns <- function(nms) {
  low <- tolower(nms)
  list(
    session = nms[low %in% c(
      "session", "session_id", "sid", "block", "block_id", "file"
    )],
    group = nms[low %in% c(
      "group", "group_id", "gid", "team", "team_id"
    )],
    scenario = nms[low %in% c("scenario", "scenario_id", "scenario_name")]
  )
}

.bundle_id_map <- function(tables, model) {
  vals <- list(session = character(), group = character(),
               scenario = character())
  for (tb in tables) {
    cols <- .id_columns(names(tb))
    for (dom in names(vals)) {
      for (cn in cols[[dom]]) {
        vals[[dom]] <- c(vals[[dom]], as.character(tb[[cn]]))
      }
    }
  }
  # The model's own frame contributes only the two columns it declared as
  # identifiers. Guessing by column name there would sweep up a within-session
  # team letter and give it a pseudonym of its own.
  dat <- model$data
  if (is.data.frame(dat)) {
    cols <- .model_cols(model, dat)
    for (dom in c("session", "group")) {
      cn <- if (identical(dom, "session")) cols$block else cols$group
      if (!is.null(cn) && cn %in% names(dat)) {
        vals[[dom]] <- c(vals[[dom]], as.character(dat[[cn]]))
      }
    }
  }
  prefix <- c(session = "S", group = "G", scenario = "SC")
  width <- c(session = 2L, group = 3L, scenario = 2L)
  map <- character()
  for (dom in names(vals)) {
    v <- sort(unique(vals[[dom]][!is.na(vals[[dom]])]))
    if (!length(v)) next
    w <- max(width[[dom]], nchar(as.character(length(v))))
    new <- sprintf(paste0(prefix[[dom]], "%0", w, "d"), seq_along(v))
    map <- c(map, stats::setNames(new, v))
  }
  map[!duplicated(names(map))]
}

.pseudonymise_table <- function(tb, map) {
  if (!length(map)) {
    return(tb)
  }
  cols <- unlist(.id_columns(names(tb)), use.names = FALSE)
  for (cn in cols) {
    tb[[cn]] <- unname(map[as.character(tb[[cn]])])
  }
  tb
}

.map_recursive <- function(x, map) {
  if (!length(map)) {
    return(x)
  }
  if (is.character(x)) {
    hit <- x %in% names(map)
    x[hit] <- unname(map[x[hit]])
    return(x)
  }
  if (is.data.frame(x)) {
    for (cn in names(x)) x[[cn]] <- .map_recursive(x[[cn]], map)
    return(x)
  }
  if (is.list(x)) {
    out <- lapply(x, .map_recursive, map = map)
    attributes(out) <- attributes(x)
    return(out)
  }
  x
}

# The last line of defence: the bytes about to be written are searched for the
# identifiers the mapping was meant to remove. Short codes are skipped because
# a one-character group label matches everything; the session-id prefix is
# checked separately so a stray raw identifier cannot slip through. The probe
# takes the hyphen form only -- `session_id` is a column name, not a course.
.check_no_identifiers <- function(txt, originals, active = TRUE,
                                  call = rlang::caller_env()) {
  txt <- txt[!is.na(txt)]
  if (!isTRUE(active) || !length(txt)) {
    return(invisible(TRUE))
  }
  blob <- paste(txt, collapse = "\n")
  probes <- unique(originals[nchar(originals) >= 4L])
  hit <- probes[vapply(
    probes, function(o) grepl(o, blob, fixed = TRUE), logical(1)
  )]
  if (grepl("session-[A-Za-z0-9]", blob)) hit <- c(hit, "session-*")
  if (!length(hit)) {
    return(invisible(TRUE))
  }
  n_hit <- length(hit)
  shown <- utils::head(hit, 5L)
  cli::cli_abort(
    c(
      "Refusing to write a course identifier into a pseudonymised bundle.",
      "x" = "{n_hit} identifier{?s} survived the mapping: {.val {shown}}.",
      "i" = "A column holding identifiers may not have been recognised;",
      "i" = "name it {.field session_id} or {.field group_id} to map it.",
      "i" = "Passing {.code pseudonymise = FALSE} writes them deliberately."
    ),
    class = "sandtablR_error_export_identity",
    call = call
  )
}

.table_strings <- function(tb) {
  vals <- lapply(tb, function(col) {
    if (is.character(col)) {
      col
    } else if (is.factor(col)) {
      c(as.character(col), levels(col))
    } else {
      NULL
    }
  })
  unique(c(names(tb), unlist(vals, use.names = FALSE)))
}

# Writers ----------------------------------------------------------------------

.write_table <- function(tbl, stem, dir, format) {
  path <- file.path(dir, paste0(stem, ".", format))
  switch(
    format,
    csv = utils::write.csv(
      as.data.frame(tbl), path, row.names = FALSE, na = "",
      fileEncoding = "UTF-8"
    ),
    parquet = nanoparquet::write_parquet(as.data.frame(tbl), path),
    rds = saveRDS(tibble::as_tibble(tbl), path)
  )
  path
}

.write_text <- function(lines, path) {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(as.character(lines)), con, useBytes = TRUE)
  path
}

.write_crosswalk <- function(map, crosswalk, target, overwrite,
                             call = rlang::caller_env()) {
  if (is.null(crosswalk) || !length(map)) {
    return(NA_character_)
  }
  path <- path.expand(crosswalk)
  inside <- .existing_dir(dirname(path))
  bundle <- .existing_dir(target$dir)
  if (identical(inside, bundle) || startsWith(inside, paste0(bundle, "/"))) {
    cli::cli_abort(
      c(
        "The crosswalk must not live inside the bundle.",
        "x" = "{.path {path}} is inside the bundle directory.",
        "i" = "A bundle that carries its own key is not pseudonymised."
      ),
      class = "sandtablR_error_export",
      call = call
    )
  }
  # The crosswalk is the one file that re-identifies a course, so it needs the
  # git guard at least as much as the bundle does -- more, in fact, since the
  # bundle is pseudonymous and this is the key to it. `allow_git` deliberately
  # does not reach here: an explicit path outside the repository is the only
  # way to write a key, because there is no legitimate reason to commit one.
  root <- .find_git_root(.existing_dir(dirname(path)))
  if (!is.null(root)) {
    cli::cli_abort(
      c(
        "Refusing to write the pseudonymisation key into a git working tree.",
        "x" = "{.path {root}} is a git repository.",
        "i" = "The key re-identifies courses, sites and dates.",
        "i" = "Write it outside the repository, to a private location."
      ),
      class = "sandtablR_error_export_git",
      call = call
    )
  }
  if (file.exists(path) && !isTRUE(overwrite)) {
    cli::cli_abort(
      c(
        "Refusing to overwrite an existing crosswalk.",
        "x" = "{.file {path}} already exists.",
        "i" = "Pass {.code overwrite = TRUE} to replace it."
      ),
      class = "sandtablR_error_export_exists",
      call = call
    )
  }
  code <- unname(map)
  domain <- ifelse(
    startsWith(code, "SC"), "scenario",
    ifelse(startsWith(code, "S"), "session", "group")
  )
  cw <- data.frame(
    domain = domain, original = names(map), pseudonym = code,
    stringsAsFactors = FALSE
  )
  utils::write.csv(cw, path, row.names = FALSE, fileEncoding = "UTF-8")
  path
}

.sha256_file <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    return(NA_character_)
  }
  tryCatch(
    digest::digest(path, algo = "sha256", file = TRUE),
    error = function(e) NA_character_
  )
}

# README -----------------------------------------------------------------------
#
# Generated at write time rather than shipped as a template, so that what it
# says about the run is what the run actually did. The caveats come first,
# because this is the file that travels to someone who was never told any of
# this in person.

.bundle_readme <- function(meta, tables) {
  b <- meta$bundle
  out <- c(
    "# sandtablR simulation bundle",
    "",
    sprintf(
      "Written %s by %s %s on %s (%s).",
      b$created, b$tool, b$sandtablR_version, b$r_version, b$platform
    ),
    "",
    "## Read this first",
    ""
  )
  st <- as.character(meta$statements)
  for (i in seq_along(st)) {
    out <- c(out, sprintf("%d. %s", i, st[[i]]), "")
  }
  c(
    out,
    .readme_files(meta, tables),
    .readme_model(meta),
    .readme_fidelity(meta, tables),
    .readme_provenance(meta),
    .readme_privacy(meta),
    .readme_checksums(meta)
  )
}

.readme_files <- function(meta, tables) {
  what <- c(
    "simulated-groups" = "one row per simulated group and card",
    "fidelity-metrics" = "how faithful the simulation is, per statistic",
    "deck-cards" = "the cards the simulation ranged over"
  )
  fmt <- meta$bundle$format
  df <- data.frame(
    file = paste0(names(tables), ".", fmt),
    rows = vapply(tables, nrow, integer(1), USE.NAMES = FALSE),
    columns = vapply(tables, ncol, integer(1), USE.NAMES = FALSE),
    contents = unname(what[names(tables)]),
    stringsAsFactors = FALSE
  )
  df <- rbind(df, data.frame(
    file = c("README.md", "manifest.json"),
    rows = NA_integer_, columns = NA_integer_,
    contents = c("this file", "machine-readable record of the run"),
    stringsAsFactors = FALSE
  ))
  c(
    "## What is in here", "", .md_table(df), "",
    paste(
      "`manifest.json` lists every column and its R type. CSV does not",
      "carry types, so a card labelled `08` comes back as the number 8",
      "from any reader that guesses; `parquet` and `rds` keep the types."
    ),
    "",
    paste(
      "Each table holds its columns and nothing else. The R object a",
      "simulation arrives in carries the fitted model as an attribute, and",
      "that model carries the real course data it was fitted on, so the",
      "attributes are dropped before anything is written."
    ),
    ""
  )
}

.readme_model <- function(meta) {
  m <- meta$model
  if (!isTRUE(m$available)) {
    return(c(
      "## How the simulated groups were made",
      "",
      paste(
        "The simulation object carried no model, so this bundle cannot say",
        "what produced these rows. That is a gap in the export, not a",
        "guarantee about the data: treat the rows as unattributed."
      ),
      ""
    ))
  }
  fr <- m$frame
  out <- c(
    "## How the simulated groups were made",
    "",
    sprintf("* engine: `%s`", .as_text(m$engine)),
    sprintf("* model terms: `%s`", .as_text(m$terms)),
    sprintf("* frame: %s", .as_text(fr$definition)),
    sprintf(
      "* frame size: %s cards x %s groups = %s rows, in %s sessions",
      .as_text(fr$n_cards), .as_text(fr$n_groups), .as_text(fr$n_rows),
      .as_text(fr$n_blocks)
    ),
    sprintf("* minimum card coverage: %s", .as_text(fr$min_coverage)),
    sprintf("* seed: %s", .as_text(meta$simulation$seed)),
    sprintf(
      "* simulated: %s groups over %s cards, %s rows",
      .as_text(meta$simulation$n_groups),
      .as_text(meta$simulation$n_cards),
      .as_text(meta$simulation$n_rows)
    ),
    "",
    paste("Why the frame matters:", fr$why_it_matters),
    ""
  )
  sds <- m$random_effect_sd
  if (is.data.frame(sds) && nrow(sds)) {
    out <- c(
      out, "### Random-effect standard deviations", "",
      paste(
        "On the log-odds scale. The group standard deviation is the team",
        "effect: the reason simulated groups differ from one another at",
        "all. A zero there means the fit found no team signal, and every",
        "simulated team is identical for that outcome."
      ),
      "", .md_table(sds), ""
    )
  }
  sk <- m$outcomes_skipped
  if (is.data.frame(sk) && nrow(sk)) {
    out <- c(
      out, "### Outcomes that are not in this bundle", "",
      .md_table(sk), ""
    )
  }
  out
}

.readme_fidelity <- function(meta, tables) {
  met <- tables[["fidelity-metrics"]]
  if (is.null(met)) {
    return(c(
      "## Fidelity",
      "",
      paste(
        "No metrics were supplied with this run, so nothing here has been",
        "checked against reality."
      ),
      ""
    ))
  }
  core <- c(
    "statistic", "observed", "simulated", "ratio", "ci_lower", "ci_upper",
    "verdict", "control"
  )
  keep <- intersect(core, names(met))
  shown <- utils::head(met, 40L)
  extra <- setdiff(names(met), keep)
  out <- c(
    "## Fidelity", "",
    .md_table(if (length(keep)) shown[, keep, drop = FALSE] else shown)
  )
  if (nrow(met) > nrow(shown)) {
    out <- c(out, "", sprintf(
      "...and %d further statistics in the file.", nrow(met) - nrow(shown)
    ))
  }
  if (length(keep) && length(extra)) {
    out <- c(out, "", sprintf(
      "The file carries %d further columns: %s.",
      length(extra), paste(extra, collapse = ", ")
    ))
  }
  c(out, "")
}

.readme_provenance <- function(meta) {
  head <- c("## Which columns are real", "")
  cols <- meta$provenance$columns
  if (!isTRUE(meta$provenance$available) || !is.data.frame(cols)) {
    return(c(head, paste(
      "The input carried no column provenance labels, so this bundle cannot",
      "tell you which columns are real, inferred, surrogate or placeholder.",
      "Two labels hold regardless: any triage column is a participant's",
      "call, never ground truth, and a consensus triage column is circular."
    ), ""))
  }
  c(head, .md_table(cols), "", paste(
    "Labels are the upstream object's own. Read `surrogate` and",
    "`placeholder` as: this column exists so the machinery runs, and it",
    "carries no evidence."
  ), "")
}

.readme_privacy <- function(meta) {
  p <- meta$privacy
  if (!isTRUE(p$pseudonymised)) {
    return(c("## Identifiers", "", paste(
      "Pseudonymisation was switched off for this bundle, so original",
      "session identifiers, group labels and scenario names are present.",
      "Those strings carry hospital, venue and city names and the exact",
      "course date. Handle this bundle accordingly."
    ), ""))
  }
  where <- if (isTRUE(p$crosswalk_written)) {
    sprintf(
      "The crosswalk was written outside this bundle, to `%s`.",
      .as_text(p$crosswalk_path)
    )
  } else {
    paste(
      "No crosswalk was kept, so this bundle cannot be re-identified from",
      "itself."
    )
  }
  c("## Identifiers", "", paste(
    sprintf(
      "Identifiers were replaced: %s. %s identifiers were mapped.",
      .as_text(p$scheme), .as_text(p$n_identifiers_mapped)
    ),
    where
  ), "")
}

.readme_checksums <- function(meta) {
  c("## Checksums", "", paste(
    "`manifest.json` records the size and SHA-256 of every other file in",
    "this bundle. It cannot list itself. Verify a file with",
    "`sha256sum <file>` on a shell, or with",
    "`digest::digest(<file>, algo = \"sha256\", file = TRUE)` in R."
  ), "")
}

.as_text <- function(x) {
  if (is.null(x) || !length(x)) {
    return("not recorded")
  }
  x <- x[[1L]]
  if (is.na(x)) {
    return("not recorded")
  }
  if (is.logical(x)) {
    return(if (x) "yes" else "no")
  }
  as.character(x)
}

.md_table <- function(df) {
  df <- as.data.frame(df)
  cells <- lapply(df, .md_cell)
  head <- paste0("| ", paste(names(df), collapse = " | "), " |")
  rule <- paste0(
    "| ", paste(rep("---", ncol(df)), collapse = " | "), " |"
  )
  body <- vapply(seq_len(nrow(df)), function(i) {
    paste0(
      "| ",
      paste(vapply(cells, function(cc) cc[[i]], character(1)),
            collapse = " | "),
      " |"
    )
  }, character(1))
  c(head, rule, body)
}

.md_cell <- function(col) {
  out <- if (is.numeric(col) && !is.integer(col)) {
    formatC(col, digits = 3L, format = "f")
  } else if (is.list(col)) {
    vapply(col, function(v) paste(as.character(v), collapse = " "),
           character(1))
  } else {
    as.character(col)
  }
  out[is.na(col)] <- "-"
  gsub("|", "/", out, fixed = TRUE)
}

# Manifest ---------------------------------------------------------------------

.bundle_manifest <- function(meta, files) {
  info <- file.info(files)
  fl <- data.frame(
    file = basename(files),
    bytes = as.integer(info$size),
    sha256 = vapply(files, .sha256_file, character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
  have <- !anyNA(fl$sha256)
  meta$checksums <- list(
    algorithm = "sha256",
    available = have,
    note = if (have) {
      paste(
        "manifest.json is not listed because it carries the digests and",
        "cannot contain its own."
      )
    } else {
      paste(
        "Digests could not be computed: the digest package is not",
        "installed. Install it and export again if the bundle needs to be",
        "verifiable."
      )
    },
    files = fl
  )
  strsplit(.json_document(meta), "\n", fixed = TRUE)[[1L]]
}

# A small JSON writer, so that a manifest -- which every bundle has -- does not
# put a package in Imports. It covers exactly what the manifest holds: nested
# named lists, data frames as arrays of objects, atomic vectors, and NA as
# null. Arrays that could be mistaken for scalars are marked explicitly.

.json_arr <- function(x) {
  class(x) <- c("sdt_json_array", setdiff(class(x), "sdt_json_array"))
  x
}

.json_document <- function(x) .json_value(x, 0L)

.json_value <- function(x, indent) {
  if (is.null(x)) {
    return("null")
  }
  if (is.data.frame(x)) x <- .json_rows(x)
  arr <- inherits(x, "sdt_json_array")
  if (is.factor(x)) x <- as.character(x)
  if (is.list(x)) {
    if (!length(x)) {
      return(if (arr || is.null(names(x))) "[]" else "{}")
    }
    if (!is.null(names(x)) && !arr) {
      return(.json_object(x, indent))
    }
    return(.json_array(unclass(x), indent))
  }
  if (arr || length(x) != 1L) {
    return(.json_array(as.list(unclass(x)), indent))
  }
  .json_scalar(x)
}

.json_object <- function(x, indent) {
  pad <- strrep(" ", indent + 2L)
  nms <- names(x)
  parts <- vapply(seq_along(x), function(i) {
    paste0(
      pad, "\"", .json_escape(nms[[i]]), "\": ",
      .json_value(x[[i]], indent + 2L)
    )
  }, character(1))
  paste0(
    "{\n", paste(parts, collapse = ",\n"), "\n", strrep(" ", indent), "}"
  )
}

.json_array <- function(x, indent) {
  if (!length(x)) {
    return("[]")
  }
  pad <- strrep(" ", indent + 2L)
  parts <- vapply(x, function(el) {
    paste0(pad, .json_value(el, indent + 2L))
  }, character(1))
  paste0(
    "[\n", paste(parts, collapse = ",\n"), "\n", strrep(" ", indent), "]"
  )
}

.json_rows <- function(df) {
  if (!nrow(df)) {
    return(.json_arr(list()))
  }
  rows <- lapply(seq_len(nrow(df)), function(i) {
    lapply(df, function(col) {
      v <- col[[i]]
      if (is.factor(v)) as.character(v) else v
    })
  })
  .json_arr(rows)
}

.json_scalar <- function(x) {
  if (is.na(x)) {
    return("null")
  }
  if (is.logical(x)) {
    return(if (x) "true" else "false")
  }
  if (is.numeric(x)) {
    if (!is.finite(x)) {
      return("null")
    }
    return(format(x, digits = 15L, scientific = FALSE, trim = TRUE))
  }
  paste0("\"", .json_escape(as.character(x)), "\"")
}

.json_escape <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  gsub("[\001-\037]", " ", x)
}
