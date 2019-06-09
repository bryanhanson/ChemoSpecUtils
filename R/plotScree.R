#'
#' Scree Plots from PCA or MIA Analysis of a Spectra or Spectra2D Object
#' 
#' Functions that draw a traditional scree plot, or an alternative style that is perhaps
#' more informative.  These plots illustrate the variance explained by each component
#' in a PCA or MIA analysis.
#'
#' @param pca Either:
#' \itemize{
#'   \item An object of class \code{\link{prcomp}}, modified to include a
#' list element called \code{$method}, a character string describing the
#' pre-processing carried out and the type of PCA performed (it appears on the
#' plot).  This is automatically provided if \code{ChemoSpec} functions
#' \code{\link[ChemoSpec]{c_pcaSpectra}} or \code{\link[ChemoSpec]{r_pcaSpectra}}
#' were used to create \code{pca}.
#'  \item An object of class \code{mia} produced by
#'  function \code{miaSpectra2D}.
#' }
#'
#' @param style Character.  One of \code{c("trad", "alt")} giving the style of
#' plot desired (traditional or alternative).  \code{"trad"} is not supported
#' for \code{mia} objects.
#'
#' @param \dots Additional parameters to be passed to plotting functions.
#'
#' @return None.  Side effect is a plot.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @references The idea for the alternative style plot came from the NIR-Quimiometria
#'  blog by jrcuesta, at \url{https://nir-quimiometria.blogspot.com/2012/02/pca-for-nir-spectrapart-004-projections.html}
#' 
#' @keywords multivariate hplot
#' @export
#' @examples
#' \dontrun{
#' if (checkForPackageWithVersion("ChemoSpec", "5.1")) {
#'   library("ChemoSpec")
#'   data(metMUD1)
#'
#'   pca <- c_pcaSpectra(metMUD1)
#'   plotScree(pca, style = "trad")
#'   plotScree(pca, style = "alt")
#' }
#' 
#' if (checkForPackageWithVersion("ChemoSpec2D", "0.3")) {
#'   library("ChemoSpec2D")
#'   data(MUD1)
#'
#'   mia <- miaSpectra2D(MUD1)
#'   plotScree(mia, style = "alt")
#' }
#' }
#'
plotScree <- function(pca,  style = "alt", ...) {
	UseMethod("plotScree")
}
