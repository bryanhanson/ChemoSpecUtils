#'
#' sampleDist.Spectra2D
#'
#' @author `r .writeDoc_Authors("BH")`
#' @export
#' @noRd
#'
sampleDist.Spectra2D <- function(spectra, method = "pearson", plot = TRUE, ...) {
  .chkArgs(mode = 21L)
  chkSpectra(spectra)

  # Helper function, repeated here to avoid an awkward export conflict
  # Original is in ChemoSpec2D
  
  .unstack <- function(spectra) {
    vectorizeByRow <- function(IN) { # Helper function from HandyStuff
      OUT <- rep(NA_real_, length(IN))
      nc <- ncol(IN)
      nr <- nrow(IN)
      a <- seq(1, length(IN), nc)
      b <- a + nc - 1
      for (n in 1:length(a)) {
        OUT[a[n]:b[n]] <- IN[n, ]
      }
      OUT
    }

    # Unstack the data into a new matrix

    no.samples <- length(spectra$names)
    no.F1 <- length(spectra$F1)
    no.F2 <- length(spectra$F2)
    no.pts <- no.F2 * no.F1
    M <- matrix(NA_real_, nrow = no.samples, ncol = no.pts)

    for (i in 1:no.samples) M[i, ] <- vectorizeByRow(spectra$data[[i]])

    M
  }

  M1 <- .unstack(spectra)
  M <- rowDist(M1, method)
  M <- as.matrix(M)
  dimnames(M) <- list(spectra$names, spectra$names)

  if (plot) .distPlot(spectra, M, method, ...)

  M
}
