# Internal validation helpers -------------------------------------------------
# All emit cli errors that point at the *caller* via `call`, so a user sees the
# function they invoked rather than the helper that noticed the problem.

.check_string <- function(x,
                          arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be a single non-NA string.", call = call)
  }
  invisible(x)
}

.check_count <- function(x,
                         arg = rlang::caller_arg(x),
                         call = rlang::caller_env()) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) &&
    x >= 1 && x == round(x)
  if (!ok) {
    cli::cli_abort(
      "{.arg {arg}} must be a single positive whole number.",
      call = call
    )
  }
  invisible(as.integer(x))
}

.check_flag <- function(x,
                        arg = rlang::caller_arg(x),
                        call = rlang::caller_env()) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be {.code TRUE} or {.code FALSE}.",
      call = call
    )
  }
  invisible(x)
}

.check_class <- function(x, cls,
                         arg = rlang::caller_arg(x),
                         call = rlang::caller_env()) {
  if (!inherits(x, cls)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a {.cls {cls}} object.",
        "x" = "Got {.cls {class(x)[[1]]}} instead."
      ),
      call = call
    )
  }
  invisible(x)
}

# Guard an optional dependency. Every model engine, explainer and sequence
# backend lives in Suggests and is checked at the point of use, so the package
# installs and checks cleanly without any of them.
.require_pkgs <- function(pkgs, call = rlang::caller_env()) {
  have <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  miss <- pkgs[!have]
  if (!length(miss)) {
    return(invisible(TRUE))
  }
  n_miss <- length(miss)
  quoted <- paste0("\"", miss, "\"", collapse = ", ")
  install <- if (n_miss > 1L) {
    paste0("install.packages(c(", quoted, "))")
  } else {
    paste0("install.packages(", quoted, ")")
  }
  cli::cli_abort(
    c(
      "{n_miss} required package{?s} not installed: {.pkg {miss}}.",
      "i" = "Install with {.code {install}}."
    ),
    call = call
  )
}

# Render one failed check as a bullet, for the validators that report every
# offending row at once rather than aborting on the first. cli reads a `{`
# followed by a dot as a style name, so anything computed here is bound to a
# plain local before it reaches a format string.
.field_problem <- function(check) {
  rows <- check$rows
  if (!length(rows)) {
    return(NULL)
  }
  field <- check$field
  detail <- check$detail
  shown <- utils::head(rows, 5L)
  hidden <- length(rows) - length(shown)
  more <- if (hidden > 0L) cli::format_inline(" (+{hidden} more)") else ""
  cli::format_inline("{.field {field}}: {detail} in row{?s} {shown}{more}.")
}

# Refuse an area outside the board vocabulary.
#
# This exists because the failure it prevents is silent rather than loud.
# `.path_strings()` encodes areas by position, and an unrecognised area used to
# become `NA`, which `paste()` then rendered as the two-character literal
# "NA" -- so one state silently became two characters, every unknown state
# collided on the same token, and the normalised edit distance was computed on
# strings of the wrong length. `summary.game_run()` failed differently and
# worse: its `factor()` dropped the row entirely, shrinking the denominator so
# that the remaining shares silently added to one.
#
# Called from the two choke points every consumer passes through, so paths,
# consistency, sequences and the end-state summary all behave the same way.
.check_areas <- function(x, arg = "area",
                         areas = sdt_board_areas(),
                         call = rlang::caller_env()) {
  bad <- setdiff(unique(x[!is.na(x)]), areas)
  if (length(bad)) {
    n_bad <- length(bad)
    shown <- utils::head(bad, 5L)
    cli::cli_abort(
      c(
        "{.arg {arg}} must name board areas.",
        "x" = "Unknown: {.val {shown}}.",
        "i" = "{n_bad} unrecognised value{?s}; allowed: {.val {areas}}."
      ),
      class = "sandtablR_error_area",
      call = call
    )
  }
  if (anyNA(x)) {
    cli::cli_abort(
      c("{.arg {arg}} must not be missing.",
        "x" = "Found {sum(is.na(x))} missing value{?s}."),
      class = "sandtablR_error_area",
      call = call
    )
  }
  invisible(x)
}

# Parallel backend -------------------------------------------------------------
# Every entry point that can parallelise takes `parallel = FALSE`. The default
# is off on purpose: a package that silently claims every core is a bad guest
# on a shared machine, and a sequential run is reproducible without having to
# think about RNG streams.
#
# Turning it on has to actually do something. future.apply and tune both
# dispatch through the future framework, but they run sequentially unless a
# plan is set -- so a `parallel = TRUE` that only calls future_lapply() is a
# no-op, which is exactly what it was here. This establishes a plan for the
# duration of the CALLING function and restores whatever was there before, so
# the setting never leaks into the caller's session.

# Workers to use when nothing is specified. Leaves a core free, and caps well
# below the machine size: past a handful of workers these fits are bound by
# memory bandwidth and process startup, not by cores, and a 72-worker plan on
# a shared box is antisocial.
.sdt_workers <- function(workers = NULL) {
  if (!is.null(workers)) {
    return(.check_count(workers))
  }
  n <- if (requireNamespace("parallelly", quietly = TRUE)) {
    parallelly::availableCores()
  } else {
    parallel::detectCores()
  }
  n <- suppressWarnings(as.integer(n))
  if (!length(n) || is.na(n)) n <- 1L
  max(1L, min(8L, n - 1L))
}

.sdt_parallel_plan <- function(parallel, workers = NULL,
                               env = rlang::caller_env()) {
  if (!isTRUE(parallel)) {
    return(invisible(FALSE))
  }
  missing_pkg <- !requireNamespace("future", quietly = TRUE)
  if (missing_pkg) {
    cli::cli_warn(c(
      "!" = "Package {.pkg future} is not installed; running serially.",
      "i" = "Install it with {.code install.packages(\"future\")}."
    ))
    return(invisible(FALSE))
  }
  n <- .sdt_workers(workers)
  if (n < 2L) {
    return(invisible(FALSE))
  }
  # Fork where the platform allows it, separate sessions where it does not.
  # Forking costs nothing to start and inherits the loaded namespace, so it
  # works under pkgload::load_all() and does not pay a worker-startup tax that
  # can make a short job slower than running it serially. parallelly reports
  # FALSE inside RStudio and on Windows, where forking is unsafe, and the
  # fallback is a normal multisession cluster.
  forkable <- requireNamespace("parallelly", quietly = TRUE) &&
    isTRUE(parallelly::supportsMulticore())
  strategy <- if (forkable) future::multicore else future::multisession
  old <- future::plan(strategy, workers = n)
  withr::defer(future::plan(old), envir = env)
  invisible(TRUE)
}
