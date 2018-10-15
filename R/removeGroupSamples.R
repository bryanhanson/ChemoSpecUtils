#'
#' Remove Groups or Samples from a Spectra or Spectra2D Object
#' 
#' Removes specified groups or samples from a \code{\link[ChemoSpec]{Spectra}} or
#' \code{Spectra2D} object.
#' 
#' Both functions will report if extra data elements are found.  These will
#' probably need to be edited manually.  The indices reported to the console
#' can be helpful in this regard.
#'
#' If \code{rem.sam} or \code{rem.group} is a character vector, the sample
#' names are grepped for the corresponding values.  Remember that the
#' grepping process is greedy, i.e. grepping for "XY" find not only "XY" but
#' also "XYZ".
#'
#' Unused levels in \code{$groups} are dropped.
#'
#' @param spectra An object of S3 class \code{\link[ChemoSpec]{Spectra}} or 
#' \code{Spectra2D}.
#'
#' @param rem.group A character vector (possibly a regex) giving the groups to be removed.
#'
#' @param rem.sam Either an integer vector specifying the samples to be
#'   removed, or a character vector (possibly a regex) giving the sample names to be removed.
#' 
#' @return An object of S3 class \code{\link[ChemoSpec]{Spectra}} or \code{Spectra2D}.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso \code{\link{removeFreq}} to remove selected frequencies.
#'
#' @keywords utilities
#'
#' @describeIn removeGroup Remove groups from a \code{Spectra} or \code{Spectra2D} object
#' @export
#'
#' @examples
#' if (requireNamespace("ChemoSpec", quietly = TRUE)) {
#'   library("ChemoSpec")
#'   data(SrE.IR)
#'
#'   # Remove a group
#'   sumGroups(SrE.IR)
#'   SrE.IRa <- removeGroup(SrE.IR, rem.group = "pSrE")
#'   sumGroups(SrE.IRa)
#'   
#'   # Remove the 9th spectrum/sample:
#'   SrE.IR$names
#'   SrE.IRb <- removeSample(SrE.IR, rem.sam = 9)
#'   SrE.IRb$names
#'
#'   # Removes a spectrum/sample with this exact name:
#'   SrE.IRc <- removeSample(SrE.IR, rem.sam = "NW_adSrE")
#'   SrE.IRc$names
#' }
#' 
#' if (requireNamespace("ChemoSpec2D", quietly = TRUE)) {
#'   library("ChemoSpec2D")
#'   data(MUD1)
#'   sumGroups(MUD1)
#'   MUD1a <- removeGroup(MUD1, rem.group = "GroupA")
#'   sumGroups(MUD1a)
#' }
#' 
removeGroup <- function(spectra, rem.group) {

	if (missing(spectra)) stop("No spectral data provided")
	if (missing(rem.group)) stop("Nothing to remove")
	chkSpectra(spectra)
	OneD <- TwoD <- FALSE
	if (class(spectra) == "Spectra") OneD <- TRUE
	if (class(spectra) == "Spectra2D") TwoD <- TRUE
	
	# Get indices to drop
	if (is.character(rem.group)) {
		drop <- NA_integer_
		for (n in 1:length(rem.group)) {
			more <- grep(rem.group[n], spectra$groups)
			drop <- c(drop, more)
			}
		rem.group <- drop[-1]
	}

	if (max(rem.group) > length(spectra$groups)) stop("Groups to remove are out of range")
	if (length(rem.group) == 0L) stop("No matching groups found to remove")
	
	# Modify the original objects
	if (OneD) {
		spectra$data <- spectra$data[-rem.group,]
		spectra$sym <- spectra$sym[-rem.group]
		spectra$alt.sym <- spectra$alt.sym[-rem.group]
	}
	
	if (TwoD) spectra$data <- spectra$data[-rem.group]
	
	spectra$names <- spectra$names[-rem.group]
	spectra$groups <- spectra$groups[-rem.group, drop = TRUE]
	spectra$colors <- spectra$colors[-rem.group]

	# Wrap up
	if (length(spectra$groups) == 0) warning("You have removed all your samples!")
	jnk <-  .extraData(spectra)	
	chkSpectra(spectra)
	return(spectra)
}

##### The code below is nearly identical to the above except rem.group -> rem.sam
##### Also, name of function, messages differ

#' @describeIn removeGroup Remove samples from a \code{Spectra} or \code{Spectra2D} object
#' @export

removeSample <- function(spectra, rem.sam) {

	if (missing(spectra)) stop("No spectral data provided")
	if (missing(rem.sam)) stop("Nothing to remove")
	chkSpectra(spectra)
	OneD <- TwoD <- FALSE
	if (class(spectra) == "Spectra") OneD <- TRUE
	if (class(spectra) == "Spectra2D") TwoD <- TRUE
	
	# Get indices to drop
	# If rem.sam is already an integer this is skipped
	if (is.character(rem.sam)) {
		drop <- NA_integer_
		for (n in 1:length(rem.sam)) {
			more <- grep(rem.sam[n], spectra$names)
			drop <- c(drop, more)
			}
		rem.sam <- drop[-1]
	}

	if (max(rem.sam) > length(spectra$groups)) stop("Samples to remove are out of range")
	if (length(rem.sam) == 0L) stop("No matching samples found to remove")
	
	# Modify the original objects
	if (OneD) {
		spectra$data <- spectra$data[-rem.sam,, drop = FALSE]
		spectra$sym <- spectra$sym[-rem.sam]
		spectra$alt.sym <- spectra$alt.sym[-rem.sam]
	}
	
	if (TwoD) spectra$data <- spectra$data[-rem.sam]
	
	spectra$names <- spectra$names[-rem.sam]
	spectra$groups <- spectra$groups[-rem.sam, drop = TRUE]
	spectra$colors <- spectra$colors[-rem.sam]

	# Wrap up
	if (length(spectra$groups) == 0) warning("You have removed all your samples!")
	jnk <-  .extraData(spectra)	
	chkSpectra(spectra)
	return(spectra)
}
