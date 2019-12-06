#'
#' plotScores.Spectra2D
#'
#' @export
#' @noRd
#'
plotScores.Spectra2D <- function(spectra, so,
                                 pcs = c(1, 2), ellipse = "none", tol = "none",
                                 use.sym = FALSE, leg.loc = "topright", ...) {

  # This function will handle score plots from MIA, PARAFAC or POP analyses

  .chkArgs(mode = 22L)
  chkSpectra(spectra)
  if (length(pcs) != 2L) stop("Please supply two scores to plot (argument pcs)")

  # See stackoverflow.com/a/46289614/633251 for the concepts re: argument handling

  # Use a sensible xlab and ylab if none provided
  args <- as.list(match.call()[-1])
  if (!("xlab" %in% names(args))) {
    if (!inherits(so, "pfac")) {
      variance <- .getVarExplained(so)
      xlab <- paste("Component ", pcs[1], " (", round(variance[ pcs[1] ], 2), "%)", sep = "")
    }
    if (inherits(so, "pfac")) xlab <- paste("Component", pcs[1], sep = " ")
    args <- c(args, list(xlab = xlab))
  }
  if (!("ylab" %in% names(args))) {
    if (!inherits(so, "pfac")) {
      variance <- .getVarExplained(so)
      ylab <- paste("Component ", pcs[2], " (", round(variance[ pcs[2] ], 2), "%)", sep = "")
    }
    if (inherits(so, "pfac")) ylab <- paste("Component", pcs[2], sep = " ")
    args <- c(args, list(ylab = ylab))
  }

  # Update & clean the argument list

  args <- c(args, list(use.sym = FALSE))
  do.call(.scorePlot, args)
}
