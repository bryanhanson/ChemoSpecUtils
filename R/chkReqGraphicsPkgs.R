#'
#' Verify Required Graphics Packages are Available.
#'
#' @param pkgs Character. Vector of package names.  Each is checked for availability.
#'
#' @return \code{NULL}, invisibly.
#'
#' @author Bryan A. Hanson (DePauw University), Tejasvi Gupta.
#'
#' @keywords utilities
#' @noRd
#' @export
#'
.chkReqGraphicsPkgs <- function(pkgs) {

  if ("ggplot2" %in% pkgs) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("You need to install package ggplot2 to use this function")
    }
  }

  if ("plotly" %in% pkgs) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("You need to install package ggplot2 to use this function")
    }
    if (!requireNamespace("plotly", quietly = TRUE)) {
      stop("You need to install package plotly to use this function")
    }
  }

  if ("patchwork" %in% pkgs) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("You need to install package ggplot2 to use this function")
    }
    if (!requireNamespace("patchwork", quietly = TRUE)) {
      stop("You need to install package patchwork to use this function")
    }
  }

  if ("lattice" %in% pkgs) {
    if (!requireNamespace("lattice", quietly = TRUE)) {
      stop("You need to install package lattice to use this function")
    }
  }
  
  invisible(NULL)
}
