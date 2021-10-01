#'
#' distPlot
#'
#' @template authors-BH
#' @export
#' @noRd
#' @importFrom grDevices rainbow
#'
.distPlot <- function(spectra, M, method, ...) {
  
  .chkReqGraphicsPkgs("lattice")

  # The bounded distances in rowDist get a fixed reference scale
  unbounded <- TRUE
  bounded1 <- FALSE
  bounded2 <- FALSE
  b1 <- "abspearson" # [0...1]
  b2 <- c("correlation", "cosine", "pearson") # [0...2]
  if (method %in% b1) bounded1 <- TRUE
  if (method %in% b2) bounded2 <- TRUE
  if (bounded1 | bounded2) unbounded <- FALSE
 
  myc <- rev(rainbow(20, start = 0.0, end = 0.66))

  M <- as.matrix(M) # incoming M is class dist, need true matrix to plot, but diag is missing

  if (bounded1) {
    diag(M) <- 0.0
    p <- lattice::levelplot(M,
      col.regions = myc, xlab = "", ylab = "",
      at = seq(0.0, 1.0, by = 0.1),
      scales = list(labels = spectra$names, cex = 0.75, rot = 45), ...
    )
    print(p)
  }

  if (bounded2) {
    diag(M) <- 0.0
    p <- lattice::levelplot(M,
      col.regions = myc, xlab = "", ylab = "",
      at = seq(0.0, 2.0, by = 0.2),
      scales = list(labels = spectra$names, cex = 0.75, rot = 45), ...
    )
    print(p)
  }

  if (unbounded) {
    diag(M) <- 0.0
    p <- lattice::levelplot(M,
      col.regions = myc, xlab = "", ylab = "",
      scales = list(labels = spectra$names, cex = 0.75, rot = 45), ...
    )
    print(p)
  }
}
