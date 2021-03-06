#'
#' Plot Scores from PCA, MIA or PARAFAC Analysis of a Spectra or Spectra2D Object
#'
#' Plots the requested scores using the color scheme derived from the
#' \code{\link[ChemoSpec]{Spectra}} or \code{\link[ChemoSpec2D]{Spectra2D}} object.
#' Options are provided to add confidence ellipses for each group in the object.
#' The ellipses may be robust or classical.  Option to label the extreme points provided.
#'
#' @param spectra An object of S3 class \code{\link[ChemoSpec]{Spectra}}
#' or \code{\link[ChemoSpec2D]{Spectra2D}} object.
#'
#' @param so "Score Object" One of the following:
#' \itemize{
#'   \item An object of class \code{\link{prcomp}}, created by \code{ChemoSpec} functions
#'   \code{\link[ChemoSpec]{c_pcaSpectra}}, \code{\link[ChemoSpec]{r_pcaSpectra}},
#'   \code{\link[ChemoSpec]{irlba_pcaSpectra}} or \code{\link[ChemoSpec]{s_pcaSpectra}}.
#' \item An object of class \code{mia} produced by
#'  function \code{\link[ChemoSpec2D]{miaSpectra2D}}.
#' \item An object of class \code{parafac} produced by
#'  function \code{\link[ChemoSpec2D]{pfacSpectra2D}}.
#' \item An object of class \code{pop} produced by
#'  function \code{\link[ChemoSpec2D]{popSpectra2D}}.
#' }
#' Any of the above score objects will have been modified to include a
#' list element called \code{$method}, a character string describing the
#' pre-processing carried out and the type of PCA performed (used to annotate the
#' plot).
#'
#' @param pcs A vector of two integers specifying the components (scores) to plot.
#'
#' @param ellipse A character vector specifying the type of ellipses to be
#' plotted.  One of \code{c("both"}, \code{"none"}, \code{"cls"}, \code{"rob")}.  \code{cls}
#' specifies classical confidence ellipses, \code{rob} specifies robust
#' confidence ellipses.  An ellipse is drawn for each group unless there
#' are three or fewer samples in the group.
#'
#' @param tol A number describing the fraction of points to be labeled.
#' \code{tol = 1.0} labels all the points; \code{tol = 0.05} labels the most
#' extreme 5 percent.  Set to \code{"none"} to completely suppress labels.
#'
#' @param use.sym A logical; if TRUE, the color scheme is set to black and the
#' points plotted with symbols.  Applies only to \code{\link[ChemoSpec]{Spectra}} objects.
#'
#' @param leg.loc Character; if \code{"none"} no legend will be drawn.
#' Otherwise, any string acceptable to \code{\link{legend}}.
#'
#' @param \dots Additional parameters to be passed to the plotting functions.
#'
#' @return None.  Side effect is a plot.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords multivariate robust hplot
#'
#' @export
#'
#' @examples
#' if (checkForPackageWithVersion("ChemoSpec", "5.1")) {
#'   library("ChemoSpec")
#'   data(metMUD1)
#'
#'   pca <- c_pcaSpectra(metMUD1)
#'   plotScores(metMUD1, pca,
#'     main = "metMUD1 NMR Data",
#'     pcs = c(1, 2), ellipse = "cls", tol = 0.05
#'   )
#' }
#'
#' if (checkForPackageWithVersion("ChemoSpec2D", "0.3")) {
#'   library("ChemoSpec2D")
#'   data(MUD1)
#'
#'   res <- miaSpectra2D(MUD1)
#'   plotScores(MUD1, res, main = "MIA Scores", tol = 0.1, ellipse = "cls")
#'
#'   set.seed(123)
#'   res <- pfacSpectra2D(MUD1, parallel = FALSE, nfac = 2)
#'   plotScores(MUD1, res, tol = 0.1, leg.loc = "bottomright", main = "PARAFAC Score Plot")
#' }
plotScores <- function(spectra, so,
                       pcs = c(1, 2), ellipse = "none", tol = "none",
                       use.sym = FALSE, leg.loc = "topright", ...) {
  UseMethod("plotScores")
}
