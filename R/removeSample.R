#'
#' Remove Samples from a Spectra or Spectra2D Object
#'
#' Removes specified samples from a \code{\link[ChemoSpec]{Spectra}} or
#' \code{\link[ChemoSpec2D]{Spectra2D}} object.
#'
#' This function will report if extra data elements are found.  These will
#' probably need to be edited manually.  The indices reported to the console
#' can be helpful in this regard.
#'
#' If \code{rem.sam} is a character vector, the sample
#' names are grepped for the corresponding values.  Remember that the
#' grepping process is greedy, i.e. grepping for "XY" find not only "XY" but
#' also "XYZ".
#'
#' @param spectra `r .writeDoc_Spectra3()`
#'
#' @param rem.sam Either an integer vector specifying the samples to be
#'   removed, or a character vector (handled as a regex) giving the sample names to be removed.
#'
#' @return `r .writeDoc_Spectra3()`
#'
#' @author `r .writeDoc_Authors("BH")`
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' if (checkForPackageWithVersion("ChemoSpec", 6.0)) {
#'   library("ChemoSpec")
#'   data(SrE.IR)
#'
#'   # Remove the 9th spectrum/sample:
#'   SrE.IR$names
#'   SrE.IRa <- removeSample(SrE.IR, rem.sam = 9)
#'   SrE.IRa$names
#'
#'   # Removes a spectrum/sample with this exact name:
#'   SrE.IRb <- removeSample(SrE.IR, rem.sam = "NW_adSrE")
#'   SrE.IRb$names
#' }
#'
#' if (checkForPackageWithVersion("ChemoSpec2D", 0.5)) {
#'   library("ChemoSpec2D")
#'   data(MUD1)
#'
#'   # Removes the 5th spectrum:
#'   MUD1$names
#'   MUD1a <- removeSample(MUD1, rem.sam = 5)
#'   MUD1a$names
#'
#'   # Removes a spectrum/sample with this exact name:
#'   MUD1$names
#'   MUD1b <- removeSample(MUD1, rem.sam = "Ether_3")
#'   MUD1b$names
#' }
removeSample <- function(spectra, rem.sam) {
  UseMethod("removeSample")
}
