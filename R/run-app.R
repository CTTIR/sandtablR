# App launcher ----------------------------------------------------------------

#' Launch the sandtablR workbench
#'
#' Starts the interactive \pkg{shiny} workbench: build a scenario, run the
#' board, annotate decisions, train and compare models, explain them, and
#' analyse decision paths.
#'
#' The app is assembled by [sdt_app()] rather than loaded from a directory, so
#' the modules are ordinary package functions: checked, linted, covered and
#' testable like everything else.
#'
#' @param ... Passed to [shiny::runApp()].
#'
#' @return Invisible `NULL`. Launches a Shiny application.
#' @seealso [sdt_app_ui()], [sdt_app_server()].
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   sdt_run_app()
#' }
#' }
sdt_run_app <- function(...) {
  .require_pkgs(c("shiny", "bslib", "htmltools"))
  missing <- .sdt_optional_app_pkgs()
  if (length(missing)) {
    cli::cli_inform(c(
      "i" = "Some panels will fall back to simpler output.",
      "*" = "Not installed: {.pkg {missing}}."
    ))
  }
  shiny::runApp(sdt_app(), ...)
  invisible(NULL)
}

# Optional packages the workbench uses when present. None is required: each
# panel degrades to a static plot or a plain table.
.sdt_optional_app_pkgs <- function() {
  pkgs <- c("DT", "reactable", "plotly", "ggiraph", "rmarkdown",
            "DALEX", "DALEXtra", "TraMineR", "WeightedCluster")
  pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
}
