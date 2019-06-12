#'
#' Remove a Group from a Spectra or Spectra2D Object
#' 
#' Removes specified groups from a \code{\link[ChemoSpec]{Spectra}} or
#' \code{\link[ChemoSpec2D]{Spectra2D}} object.
#' 
#' This function will report if extra data elements are found.  These will
#' probably need to be edited manually.  The indices reported to the console
#' can be helpful in this regard.
#'
#' If \code{rem.group} is a character vector, the sample
#' names are grepped for the corresponding values.  Remember that the
#' grepping process is greedy, i.e. grepping for "XY" find not only "XY" but
#' also "XYZ".
#'
#' Unused levels in \code{$groups} are dropped.
#'
#' @param spectra An object of S3 class \code{\link[ChemoSpec]{Spectra}} or 
#' \code{\link[ChemoSpec2D]{Spectra2D}}.
#'
#' @param rem.group A character vector (handled as a regex) giving the groups to be removed.
#'
#' @return An object of S3 class \code{\link[ChemoSpec]{Spectra}} or \code{\link[ChemoSpec2D]{Spectra2D}}.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' if (checkForPackageWithVersion("ChemoSpec", "5.1")) {
#'   library("ChemoSpec")
#'   data(SrE.IR)
#'
#'   sumGroups(SrE.IR)
#'   SrE.IRa <- removeGroup(SrE.IR, rem.group = "pSrE")
#'   sumGroups(SrE.IRa)
#' }
#' 
#' if (checkForPackageWithVersion("ChemoSpec2D", "0.3")) {
#'   library("ChemoSpec2D")
#'   data(MUD1)
#'
#'   sumGroups(MUD1)
#'   MUD1a <- removeGroup(MUD1, rem.group = "Ether")
#'   sumGroups(MUD1a)
#' }
#'
removeGroup <- function(spectra, rem.group) {
	UseMethod("removeGroup")
}
