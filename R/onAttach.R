#'
#' @noRd
#' @template authors-BH
#'
.onAttach <- function(libname, pkgname) {
  # needed for testing CSU when CS is not loaded
  if (is.null(getOption("ChemoSpecGraphics"))) options(ChemoSpecGraphics = "base")
}