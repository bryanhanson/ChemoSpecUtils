#'
#' distPlot
#'
#' @export
#' @noRd
#' @importFrom grDevices rainbow
#'
.distPlot <- function(spectra, M, method, ...) {
  if (!requireNamespace("lattice", quietly = TRUE)) {
    stop("You need to install package lattice to plot")
  }

  fixedScale <- FALSE
  if ((method == "correlation") | (method == "pearson") | (method == "cosine")) fixedScale <- TRUE # [-1...1]

  myc <- rev(rainbow(20, start = 0.0, end = 0.66))

  M <- as.matrix(M) # M is class dist, need true matrix to plot, but diag is missing

  if (fixedScale) {
    diag(M) <- 1.0
    p <- lattice::levelplot(M,
      col.regions = myc, xlab = "", ylab = "",
      at = seq(-1.0, 1.0, by = 0.1),
      scales = list(labels = spectra$names, cex = 0.75, rot = 45), ...
    )
    print(p)
  }

  if (!fixedScale) {
    diag(M) <- NA
    p <- lattice::levelplot(M,
      col.regions = myc, xlab = "", ylab = "",
      scales = list(labels = spectra$names, cex = 0.75, rot = 45), ...
    )
    print(p)
  }
}
