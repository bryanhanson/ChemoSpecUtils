#'
#' hcaScores.Spectra
#'
#' @template authors-BH
#' @export
#' @noRd
#' @importFrom stats hclust
#'
hcaScores.Spectra <- function(spectra, so, scores = c(1:5),
                              c.method = "complete", d.method = "euclidean",
                              use.sym = FALSE, leg.loc = "topright", ...) {
  .chkArgs(mode = 12L)
  chkSpectra(spectra)

  if (use.sym) spectra$names <- paste(spectra$alt.sym, spectra$names, sep = " ")
  sub.title <- paste("clustering method: ", c.method, "      distance method: ", d.method, sep = "")

  distance <- rowDist(as.data.frame(so$x[, scores], row.names = spectra$names), method = d.method)
  hclst <- hclust(distance, method = c.method)

  d <- .plotHCA(
    spectra = spectra, hclst = hclst, sub.title = sub.title,
    use.sym = use.sym, leg.loc = leg.loc, ...
  )
  L <- list(hclst = hclst, dend = d)
  return(L)
}
