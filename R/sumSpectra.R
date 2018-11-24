#'
#' Summarize a Spectra or Spectra2D Object
#' 
#' Provides a summary of a \code{\link[ChemoSpec]{Spectra}}
#' or \code{Spectra2D} object,
#' essentially a more spectroscopist-friendly version of \code{str()}.
#' 
#' Prior to summarizing, \code{\link{chkSpectra}} is run with confirm = FALSE.
#' If there are problems, warnings are issued to the console and the summary is
#' not done.
#' The \code{\link[ChemoSpec]{Spectra}} or \code{Spectra2D}
#' object is checked to see if it contains data elements
#' beyond what is required.  If so, these extra elements are reported to the
#' console.
#' 
#' @param spectra An object of S3 class \code{\link[ChemoSpec]{Spectra}} or 
#' \code{Spectra2D} whose group membership information is desired.
#'
#' @param ...  Arguments to be passed downstream. Currently not used.
#'
#' @return None.  Results printed at console.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords utilities
#'
#' @export
#' 
#' @examples
#' if (requireNamespace("ChemoSpec", quietly = TRUE)) {
#'   library("ChemoSpec")
#'   data(SrE.IR)
#'   sumSpectra(SrE.IR)
#' }
#' 
#' if (requireNamespace("ChemoSpec2D", quietly = TRUE)) {
#'   library("ChemoSpec2D")
#'   data(MUD1)
#'   sumSpectra(MUD1)
#' }
#' 
 
sumSpectra <- function(spectra, ...) {
	UseMethod("sumSpectra")
}

