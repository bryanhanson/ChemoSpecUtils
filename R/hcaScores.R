#'
#'
#' HCA on PCA/MIA/PARAFAC scores from a Spectra or Spectra2D Object
#' 
#' A wrapper which performs HCA on the scores from a PCA of a
#' \code{\link[ChemoSpec]{Spectra}} object or MIA/PARAFAC of a \code{Spectra2D} object.
#' Many methods for computing the clusters and distances are
#' available.
#'
#' @param spectra An object of S3 class \code{\link[ChemoSpec]{Spectra}} or \code{Spectra2D} object.
#' 
#' @param so ("score object") Either:
#' \itemize{
#'   \item An object of class \code{\link{prcomp}}, modified to include a
#' list element called \code{$method}, a character string describing the
#' pre-processing carried out and the type of PCA performed (it appears on the
#' plot).  This is automatically provided if \code{ChemoSpec} functions
#' \code{\link[ChemoSpec]{c_pcaSpectra}} or \code{\link[ChemoSpec]{r_pcaSpectra}}
#' were used to create \code{pca}.
#'  \item An object of class \code{mia} produced by
#'  function \code{miaSpectra2D}.
#'  \item An object of class \code{parafac} produced by
#'  function \code{pfacSpectra2D}.
#' }
#'
#' @param scores A vector of integers specifying the components (scores) to plot.
#' 
#' @param c.method A character string describing the clustering method; must be
#' acceptable to \code{\link{hclust}}.
#' 
#' @param d.method A character string describing the distance calculation
#' method; must be acceptable as a method in \code{\link{rowDist}}.
#' 
#' @param use.sym A logical; if true, use no color and use lower-case letters
#' to indicate group membership.  Applies only to \code{Spectra} objects.
#' 
#' @param leg.loc Character; if \code{"none"} no legend will be drawn.
#' Otherwise, any string acceptable to \code{\link{legend}}.
#' 
#' @param \dots Additional parameters to be passed to the plotting functions.
#' 
#' @return A list, containing an object of class \code{\link{hclust}} and an
#' object of class \code{\link{dendrogram}}.  The side effect is a plot.
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @seealso \code{\link{hclust}} for the underlying function. See
#' \code{\link[ChemoSpec]{hcaSpectra}} for HCA of the entire data set stored in the
#' \code{\link[ChemoSpec]{Spectra}} object.
#' 
#' @keywords multivariate cluster
#' @export
#'
#' @examples
#'
#' if (requireNamespace("ChemoSpec", quietly = TRUE)) {
#'   library("ChemoSpec")
#'   data(metMUD1)
#'
#'   pca <- c_pcaSpectra(metMUD1)
#'   hca <- hcaScores(metMUD1, pca, main = "metMUD1 NMR Data")
#' }
#' 
#' if (requireNamespace("ChemoSpec2D", quietly = TRUE)) {
#'   library("ChemoSpec2D")
#'   data(MUD1)
#'
#'   mia <- miaSpectra2D(MUD1)
#'   hca <- hcaScores(MUD1, mia, scores = 1:2, main = "MIA Scores")
#'
#'   set.seed(123)
#'   pfac <- pfacSpectra2D(MUD1, parallel = FALSE, nfac = 2)
#'   hca <- hcaScores(MUD1, pfac, scores = 1:2, main = "PARAFAC Score Plot")
#' }
#'
hcaScores <- function(spectra, so, scores = c(1:5),
	c.method = "complete", d.method = "euclidean",
	use.sym = FALSE, leg.loc = "topright",  ...) {

	UseMethod("hcaScores")
}

