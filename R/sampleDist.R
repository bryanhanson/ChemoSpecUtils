#'
#' Compute the Distances Between Samples in a Spectra or Spectra2D Object
#'
#' Compute the distances between samples in a \code{\link[ChemoSpec]{Spectra}} or
#' \code{\link[ChemoSpec2D]{Spectra2D}} object. This is a means
#' to quantify the similarity between samples. A heat map style plot is an
#' option.
#'
#' @param spectra An object of S3 class \code{\link[ChemoSpec]{Spectra}} or
#' \code{\link[ChemoSpec2D]{Spectra2D}}.
#'
#' @param method Character.  A string giving the distance method.  See
#' \code{\link{rowDist}} for options.
#'
#' @param plot Logical.  Shall a level plot (heat map) be made?
#'
#' @param \dots Arguments to be passed to the plotting function.
#'
#' @return A numeric matrix giving the distances between the samples.
#'
#' @template authors-BH
#'
#' @seealso For \code{\link[ChemoSpec]{Spectra}} objects, see \code{\link[ChemoSpec]{plotSpectraDist}}
#' which compares all spectra to a single reference spectrum.
#'
#' @keywords hplot
#' @export
#' @examples
#'
#' if (checkForPackageWithVersion("ChemoSpec", 6.0)) {
#'   library("ChemoSpec")
#'   library("lattice")
#'   data(SrE.IR)
#'
#'   SrE.dmatrix <- sampleDist(SrE.IR, # cosine distance bounded on [0...2]
#'     method = "cosine",
#'     main = "SrE.IR Cosine Distance Between Samples"
#'   )
#'   SrE.dmatrix <- sampleDist(SrE.IR, # abspearson distance bounded on [0...1]
#'     method = "abspearson",
#'     main = "SrE.IR Absolute Pearson Distance Between Samples"
#'   )
#'   SrE.dmatrix <- sampleDist(SrE.IR, # euclidean distance unbounded
#'     method = "euclidean",
#'     main = "SrE.IR Euclidean Distance Between Samples"
#'   )
#' }
#'
#' if (checkForPackageWithVersion("ChemoSpec2D", 0.5)) {
#'   library("ChemoSpec2D")
#'   library("lattice")
#'   data(MUD1)
#'
#'   MUD1.dmatrix <- sampleDist(MUD1,
#'     method = "cosine",
#'     main = "MUD1 Cosine Distance Between Samples"
#'   )
#' }
sampleDist <- function(spectra, method = "pearson", plot = TRUE, ...) {
  UseMethod("sampleDist")
}
