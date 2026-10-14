# Deployment launcher ----------------------------------------------------------
# The workbench itself lives in the package's R/ directory, where R CMD check,
# lintr and covr can see it and the test suite can drive it headlessly. This
# file exists so the app can still be deployed as a directory, to shinyapps.io
# or a Shiny Server.

sandtablR::sdt_app()
