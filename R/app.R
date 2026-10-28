# The workbench --------------------------------------------------------------
# UI and server are exported so the app can be embedded in a larger Shiny
# application, mounted by a deployment directory, or driven headlessly by
# shiny::testServer() in the test suite.

#' The sandtablR workbench as a Shiny app object
#'
#' Returns a fully wired [shiny::shinyApp()] object: use it to deploy the
#' workbench, to mount it behind a router, or to run it yourself.
#'
#' Prefer this to assembling [sdt_app_ui()] and [sdt_app_server()] by hand.
#' The package serves its own logo from an `addResourcePath()` mount, and that
#' registration is process-local: an app object built in one process and
#' served from another registers nothing in the process that answers the
#' request. This constructor mounts the assets from `onStart`, which runs in
#' the serving process before it takes any request.
#'
#' @return A `shiny.appobj`.
#' @seealso [sdt_run_app()] to launch it, [sdt_app_ui()] and
#'   [sdt_app_server()] for the pieces.
#' @export
#' @examples
#' if (requireNamespace("shiny", quietly = TRUE) &&
#'     requireNamespace("bslib", quietly = TRUE)) {
#'   app <- sdt_app()
#'   class(app)
#' }
sdt_app <- function() {
  .require_pkgs(c("shiny", "bslib", "htmltools"))
  shiny::shinyApp(
    ui = function(req) sdt_app_ui(),
    server = sdt_app_server,
    onStart = function() .sdt_mount_assets()
  )
}

#' User interface for the sandtablR workbench
#'
#' @return A \pkg{shiny} UI definition.
#' @seealso [sdt_run_app()], [sdt_app_server()].
#' @export
#' @examples
#' if (requireNamespace("shiny", quietly = TRUE) &&
#'     requireNamespace("bslib", quietly = TRUE)) {
#'   ui <- sdt_app_ui()
#'   class(ui)
#' }
sdt_app_ui <- function() {
  .require_pkgs(c("shiny", "bslib", "htmltools"))
  .sdt_mount_assets()
  bslib::page_navbar(
    window_title = "sandtablR",
    title = .sdt_brand(),
    theme = .sdt_app_theme(),
    header = htmltools::tags$head(
      htmltools::tags$style(htmltools::HTML(.sdt_app_css()))
    ),
    bslib::nav_panel("Overview", mod_overview_ui("overview")),
    bslib::nav_panel("Scenario", mod_scenario_ui("scenario")),
    bslib::nav_panel("Simulate", mod_simulate_ui("simulate")),
    bslib::nav_panel("Annotate", mod_annotate_ui("annotate")),
    bslib::nav_panel("Model lab", mod_model_ui("model")),
    bslib::nav_panel("Explain", mod_explain_ui("explain")),
    bslib::nav_panel("Sequences", mod_sequence_ui("sequence")),
    bslib::nav_panel("Generate", mod_generate_ui("generate")),
    bslib::nav_spacer(),
    bslib::nav_item(shiny::downloadButton(
      "report", "Report", class = "btn-sm btn-outline-secondary"
    )),
    # The toggle carries an id so the server can read it. Plots are rendered
    # server-side as images, so the one thing on the page that CSS cannot
    # recolour is the one thing that has to be told which mode it is in.
    bslib::nav_item(bslib::input_dark_mode(id = "colour_mode"))
  )
}

#' Server for the sandtablR workbench
#'
#' All six modules share one [shiny::reactiveValues()] holding the scenario,
#' the game run, the fitted models and the leaderboard. Nothing is passed
#' between modules by any other route, so there is one place to look when the
#' app and the console disagree.
#'
#' @param input,output,session Supplied by \pkg{shiny}.
#' @return Invisible `NULL`, called for its side effects.
#' @seealso [sdt_run_app()], [sdt_app_ui()].
#' @export
#' @examples
#' # Driven headlessly in the package's own tests:
#' # shiny::testServer(sdt_app_server, { session$setInputs() })
#' sdt_app_ui
sdt_app_server <- function(input, output, session) {
  .sdt_mount_assets()

  state <- shiny::reactiveValues(
    scenario = NULL,
    run = NULL,
    models = NULL,
    bench = NULL
  )

  mod_overview_server("overview", state)
  mod_scenario_server("scenario", state)
  mod_simulate_server("simulate", state)
  mod_annotate_server("annotate", state)
  mod_model_server("model", state)
  mod_explain_server("explain", state)
  mod_sequence_server("sequence", state)
  mod_generate_server("generate", state)

  output$report <- shiny::downloadHandler(
    filename = function() "sandtablR-report.html",
    content = function(file) {
      shiny::withProgress(message = "Rendering report", value = 0.3, {
        .sdt_render_report(state, file)
      })
    }
  )

  invisible(NULL)
}

# Render the session's state to a self-contained HTML report.
#
# Rendering happens in a temporary directory with the state passed through
# params, never through the global environment: knitr evaluates in its own
# session, and a report that silently picked up whatever happened to be in
# the caller's workspace would be reproducible only by accident.
.sdt_render_report <- function(state, file, call = rlang::caller_env()) {
  .require_pkgs("rmarkdown", call = call)
  template <- system.file("rmarkdown", "report.Rmd", package = "sandtablR")
  if (!nzchar(template)) {
    cli::cli_abort("Could not find the bundled report template.", call = call)
  }

  work <- withr::local_tempdir()
  local_template <- file.path(work, "report.Rmd")
  file.copy(template, local_template, overwrite = TRUE)

  rmarkdown::render(
    local_template,
    output_file = "report.html",
    output_dir = work,
    quiet = TRUE,
    envir = new.env(parent = globalenv()),
    params = list(
      scenario = state$scenario,
      run = state$run,
      models = state$models,
      bench = state$bench
    )
  )
  file.copy(file.path(work, "report.html"), file, overwrite = TRUE)
  invisible(file)
}
