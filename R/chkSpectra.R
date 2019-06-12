#'
#' Verify the Integrity of a Spectra or Spectra2D Object
#' 
#' Utility function to verify that the structure of a \code{\link[ChemoSpec]{Spectra}}
#' or \code{\link[ChemoSpec2D]{Spectra2D}} object is internally consistent.
#' This function should be used after manual editing of these objects.
#' However, in most cases rather than
#' directly editing these objects, one should modify them via:
#' \itemize{
#'   \item {\code{\link{removeFreq}}}
#'   \item {\code{\link{removeSample}}}
#'   \item {\code{\link{removeGroup}}}
#' }
#'
#' @param spectra An object of S3 class \code{\link[ChemoSpec]{Spectra}} or 
#' \code{\link[ChemoSpec2D]{Spectra2D}}.
#' 
#' @param confirm Logical indicating whether or not to write the results to the
#' console, as would be desirable for interactive use.
#' 
#' @return None. When used at the console, and the object is OK, no message is
#' written unless \code{confirm = TRUE}.  At the console, if there is a
#' problem, messages are issued regardless of the value of \code{confirm}.
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @keywords classes utilities
#' 
#' @export chkSpectra
#' @importFrom utils str 
#'
#' @examples
#' if (checkForPackageWithVersion("ChemoSpec", "5.1")) {
#'   library("ChemoSpec")
#'   data(SrE.IR)
#'   chkSpectra(SrE.IR)
#' }
#' 
#' if (checkForPackageWithVersion("ChemoSpec2D", "0.3")) {
#'   library("ChemoSpec2D")
#'   data(MUD1)
#'   chkSpectra(MUD1)
#' }
#'
 
chkSpectra <- function(spectra, confirm = FALSE) {
	UseMethod("chkSpectra")
}

