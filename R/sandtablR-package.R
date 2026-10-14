#' @keywords internal
"_PACKAGE"

#' @importFrom rlang .data %||%
#' @importFrom stats predict rexp runif
NULL

# Quieten R CMD check for tidy-eval column references used across the package.
utils::globalVariables(c("."))

# parsnip keeps its model registry in a package environment, so a custom model
# has to be declared at load time. .sdt_register_transformer() is idempotent
# because pkgload::load_all() re-runs this and parsnip errors on a duplicate.
.onLoad <- function(libname, pkgname) {
  try(.sdt_register_transformer(), silent = TRUE)
  invisible()
}
