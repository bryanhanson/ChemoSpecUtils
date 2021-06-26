#'
#' @noRd
#'
.onAttach <- function(libname, pkgname) {
  # needed for testing CSU when CS is not loaded
  if (is.null(getOptions("ChemoSpecGraphics"))) options(ChemoSpecGraphics = "base")
}