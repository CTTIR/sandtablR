# The workbench, driven through a real browser.
#
# Everything else drives the module servers with shiny::testServer(), which
# never renders a page. An output that throws while Shiny is turning it into
# HTML, a nav panel whose contents never become visible, a JavaScript binding
# that fails on load: none of those are reachable from testServer(), and all
# of them break the app for the person in front of it. This is the one test
# that starts the app in its own R process, points a headless Chromium at it
# and reads back what the browser is actually holding.
#
# It is also the one test with a prerequisite it cannot install for itself, so
# the absence of a browser is a skip rather than a failure. A machine with no
# browser has nothing to say about the app, which is different from having
# something bad to say.

# The browser chromote should drive, or NULL when there is not one.
#
# chromote's own discovery comes first, so this needs no help wherever Chrome
# or Chromium is on the PATH. The fallback is the Chromium that Playwright
# caches, which chromote does not know to look for. It is globbed rather than
# spelled out: a literal path is a permanent skip on every machine except the
# one it was written on, which is worse than having no test at all.
sdt_driver_chrome <- function() {
  found <- suppressMessages(try(chromote::find_chrome(), silent = TRUE))
  usable <- !inherits(found, "try-error") && length(found) == 1L &&
    !is.na(found) && nzchar(found)
  if (usable) {
    return(as.character(found))
  }
  cached <- Sys.glob(file.path(
    "~", ".cache", "ms-playwright", "chromium*", "chrome-linux64", "chrome"
  ))
  cached <- cached[file.exists(cached)]
  if (length(cached)) cached[[1L]] else NULL
}

# The app.R for the app's own R process, or NULL if it cannot reach sandtablR.
#
# shinytest2 starts the app in a fresh R process, and that process reaches the
# package through .libPaths(), not through the namespace this one is holding.
# Handed a shiny.appobj it would have to carry a UI and a server that both
# close over namespace:sandtablR across that boundary, and under
# pkgload::load_all() nothing on the other side can restore it -- so the whole
# test skipped on every machine where the package was not also installed,
# which is every machine in the middle of developing it. An app.R instead lets
# the app process reach the package the way this one did: out of the library
# when there is an installed copy, off the source tree when there is not.
#
# .libPaths() is passed to find.package() explicitly, because given no lib.loc
# it answers from the loaded namespaces and reports a source tree under
# load_all() as though it were installed -- so the obvious test would pass
# here and the app would still fail to start. The load_all() call is built as
# text rather than written as code: it runs in the other process, where
# pkgload is present by construction because load_all() is what put us on this
# branch, and spelling it as a call here would make pkgload look like a
# dependency of this package's tests to R CMD check.
sdt_driver_app_code <- function() {
  in_library <- length(find.package("sandtablR", lib.loc = .libPaths(),
                                    quiet = TRUE)) > 0L
  if (in_library) {
    return("sandtablR::sdt_app()")
  }
  src <- tryCatch(getNamespaceInfo("sandtablR", "path"),
                  error = function(e) NULL)
  from_source <- !is.null(src) && dir.exists(file.path(src, "R")) &&
    file.exists(file.path(src, "DESCRIPTION"))
  have_pkgload <- length(find.package("pkgload", lib.loc = .libPaths(),
                                      quiet = TRUE)) > 0L
  if (!from_source || !have_pkgload) {
    return(NULL)
  }
  c(
    sprintf("pkgload::load_all(%s, quiet = TRUE, helpers = FALSE)",
            deparse1(src)),
    "sandtablR::sdt_app()"
  )
}


test_that("the workbench builds, runs and switches tabs in a browser", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("simmer")
  app_code <- sdt_driver_app_code()
  skip_if(is.null(app_code), "no copy of sandtablR the app process can load")
  chrome <- sdt_driver_chrome()
  skip_if(is.null(chrome), "no Chromium-based browser for chromote to drive")

  app_dir <- withr::local_tempdir("sdt-app")
  writeLines(app_code, file.path(app_dir, "app.R"))

  # Told nothing, Chromium makes a throwaway profile called org.chromium.*
  # straight in TMPDIR and, killed rather than shut down, leaves it there.
  # R CMD check then reports detritus in the temp directory: a NOTE with no
  # other symptom, appearing only under check and only because of this test.
  # Handing the browser a TMPDIR of its own inside this session's tempdir()
  # puts the profile where check does not look -- it ignores the RtmpXXXXXX
  # directories -- and lets the clean-up name a directory this test created
  # rather than glob for whatever else in /tmp happens to match. The name is
  # short on purpose: the profile holds a Unix domain socket, those are capped
  # near 100 characters, and a longer path aborts the browser on startup.
  # The defer is registered before the driver's, so it runs after it.
  chrome_tmp <- file.path(tempdir(), "sdt-chrome")
  dir.create(chrome_tmp, showWarnings = FALSE)
  withr::defer(unlink(chrome_tmp, recursive = TRUE, force = TRUE))

  # AppDriver$new() calls skip_on_cran() itself unless this variable says not
  # to, and there is deliberately no skip_on_cran() of our own either. NOT_CRAN
  # is unset in a plain Rscript run just as it is on CRAN, so leaving both in
  # place makes the one test that opens a browser also the one test that hardly
  # ever runs: it would skip in silence for everyone except devtools::check().
  # The honest gate on a browser test is whether there is a browser, and that
  # is the skip above.
  withr::local_envvar(
    CHROMOTE_CHROME = chrome,
    SHINYTEST2_APP_DRIVER_TEST_ON_CRAN = "true",
    TMPDIR = chrome_tmp, TMP = chrome_tmp, TEMP = chrome_tmp
  )

  # Chromium's sandbox is unavailable inside most containers and CI images,
  # and /dev/shm there is far smaller than it assumes. Either one makes the
  # browser exit before it ever speaks the DevTools protocol, which surfaces
  # as an opaque connection timeout rather than as anything about sandboxes.
  # The previous arguments are restored so this does not leak into whatever
  # else in the session happens to use chromote.
  previous_args <- chromote::get_chrome_args()
  withr::defer(chromote::set_chrome_args(previous_args))
  chromote::set_chrome_args(c(
    "--no-sandbox", "--disable-gpu", "--disable-dev-shm-usage",
    "--headless=new"
  ))

  app <- shinytest2::AppDriver$new(
    app_dir, name = "workbench", height = 1100, width = 1600,
    load_timeout = 60000
  )
  withr::defer(app$stop())

  # The Overview panel's demo button reaches this same state in one click, but
  # it also trains the whole model bench, which costs around twenty seconds --
  # a third of the entire suite for one assertion. Building a small cohort on
  # the Scenario tab and running it on the Simulate tab gets there in about
  # five, and drives two more modules through the browser on the way.
  app$set_inputs(`scenario-n` = 20, wait_ = FALSE)
  app$click("scenario-build", wait_ = FALSE)
  app$wait_for_idle(timeout = 60000)
  app$set_inputs(`simulate-reps` = 2, wait_ = FALSE)
  app$click("simulate-run", wait_ = FALSE)
  app$wait_for_idle(timeout = 60000)

  # Output values, not screenshots. A value is a claim about what the server
  # sent, and it survives a font substitution, a Bootstrap point release and a
  # different rendering machine; a screenshot survives none of the three and
  # fails for reasons that have nothing to do with the package.
  #
  # A renderUI() output arrives as a list of HTML and its dependencies, so the
  # markup has to be pulled out before anything can be matched against it.
  box_html <- function(id) as.character(app$get_value(output = id)$html)

  scenario_box <- box_html("overview-vb_scenario")
  expect_match(scenario_box, "sdt-metric\">20<")
  expect_match(scenario_box, "casualties on", fixed = TRUE)

  run_box <- box_html("overview-vb_run")
  expect_match(run_box, "sdt-metric\">2<")
  expect_match(run_box, "replications over [0-9]+ min")

  # How many engines are installed varies by machine, so the assertion is on
  # the shape of the count rather than on a number: an empty or half-rendered
  # box fails it, a leaner library does not.
  engines_box <- box_html("overview-vb_engines")
  expect_match(engines_box, "sdt-metric\">[0-9]+ of [0-9]+<")
  expect_match(engines_box, "model engines installed", fixed = TRUE)

  nav_links <- "document.querySelectorAll('.navbar a.nav-link')"
  labels <- unlist(app$get_js(sprintf("Array.from(%s, a => a.text)",
                                      nav_links)))
  expect_setequal(labels, c("Overview", "Scenario", "Simulate", "Annotate",
                            "Model lab", "Explain", "Sequences", "Generate"))

  # What the page holds once a panel has been opened. Shiny suspends the
  # outputs of a hidden nav panel, so a panel nobody opens is a panel nobody
  # renders, and visiting each in turn is the only thing that makes a broken
  # one visible.
  #
  # Four claims, because "it did not error" is true of a panel that never
  # opened and of one that opened empty: the navbar has to agree about which
  # panel is current, the pane has to be laid out rather than merely present,
  # Shiny has to have bound outputs inside it, and none of those outputs may
  # carry shiny-output-error -- the class Shiny puts on an output whose render
  # expression threw, which nothing in the app's own markup or CSS uses.
  # Nested tabsets keep an active pane of their own inside every hidden panel,
  # so the pane is reached through the current link's target rather than by
  # asking for the first active one on the page.
  panel_state <- "(() => {
    const link = document.querySelector('.navbar a.nav-link.active');
    const pane = document.querySelector(
      link.getAttribute('data-bs-target') || link.getAttribute('href'));
    return {
      label: link.text,
      visible: pane.offsetHeight > 0,
      outputs: pane.querySelectorAll('.shiny-bound-output').length,
      errors: Array.from(
        document.querySelectorAll('.shiny-output-error'), e => e.id)
    };
  })()"

  seen <- lapply(seq_along(labels), function(i) {
    app$run_js(sprintf("%s[%d].click()", nav_links, i - 1L))
    app$wait_for_idle(timeout = 60000)
    app$get_js(panel_state)
  })

  # Named rather than counted throughout, so a failure says which panel.
  expect_identical(vapply(seen, function(x) x$label, character(1)), labels)
  hidden <- labels[!vapply(seen, function(x) isTRUE(x$visible), logical(1))]
  expect_identical(hidden, character())
  bare <- labels[vapply(seen, function(x) x$outputs, numeric(1)) == 0]
  expect_identical(bare, character())
  broken <- unlist(lapply(seen, function(x) unlist(x$errors)))
  expect_identical(as.character(broken), character())

  # A binding that throws leaves the page looking finished while half of it
  # has quietly stopped responding, and only the browser console says so.
  # chromote files console calls under their own type and uncaught exceptions
  # under "throw"; the messages are compared rather than counted so that a
  # failure names the script that broke.
  logs <- app$get_logs()
  js <- logs[logs$location == "chromote" &
               logs$level %in% c("error", "throw"), , drop = FALSE]
  expect_identical(js$message, character())
})
