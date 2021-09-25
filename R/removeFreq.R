#'
#' Remove Frequencies from a Spectra or Spectra2D Object
#'
#' This function removes specified frequencies from a \code{\link[ChemoSpec]{Spectra}}
#' or \code{\link[ChemoSpec2D]{Spectra2D}} object.
#' For instance, one might want to remove regions lacking any useful
#' information (to reduce the data size), remove regions with large
#' interfering peaks (e.g. the water peak in 1H NMR) or simply focus
#' on a region of interest.
#'
#' @param spectra An object of S3 class \code{\link[ChemoSpec]{Spectra}} or
#' \code{\link[ChemoSpec2D]{Spectra2D}} from which to remove frequencies.
#'
#' @param rem.freq For a \code{Spectra} object, a vector of logicals.
#' \code{rem.freq} can be any valid \code{R} statement that leads to a vector of
#' logicals (must be of \code{length(Spectra$freq)}).  This vector should be \code{TRUE}
#' for frequencies you want to be removed
#' and \code{FALSE} for those frequencies which will be kept.
#' In the examples, the | and & operators may seem backward in
#' a sense, but R evaluates them one at a time and then combines them to give
#' the desired result.  You may wish to look at \code{\link{Comparison}} and
#' \code{\link{Logic}}. See the examples.  \emph{In addition, since January 2020 \code{rem.freq}
#' may be a formula as described below.}
#'
#' @param remF2 Applies to \code{\link[ChemoSpec2D]{Spectra2D}} objects. A formula giving the range of
#' frequencies to be extracted.  May include
#' "low" or "high" representing the extremes of the spectra.  Values outside the range of
#' F2 are tolerated without notice and are handled as \code{min} or \code{max}. See the examples.
#'
#' @param remF1 As for \code{remF2}.
#'
#' @section Modifying Spectra2D Objects:
#' Regarding \code{\link[ChemoSpec2D]{Spectra2D}} objects, one cannot remove frequencies from the interior of
#' a 2D NMR data set and expect
#' to get a meaningful contour plot, because doing so puts unrelated peaks adjacent
#' in the data set. This would lead to contours being drawn that don't exist in the
#' original data set. However, one can remove data from the interior and run a PARAFAC
#' analysis on the result, using the spectrum as an abstract object (that is, the
#' spectrum may not plottable, but the resulting scores are still meaningful).
#'
#' @return An object of S3 class \code{\link[ChemoSpec]{Spectra}} or
#' \code{\link[ChemoSpec2D]{Spectra2D}}.
#'
#' @seealso \code{\link[ChemoSpec2D]{removeFreq}} for another way to remove data.
#'
#' @template authors-BH
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' if (checkForPackageWithVersion("ChemoSpec", 6.0)) {
#'   library("ChemoSpec")
#'   data(SrE.IR)
#'   sumSpectra(SrE.IR)
#'
#'   # Examples where rem.freq is a logical vector
#' 
#'   # Remove frequencies from one end:
#'   newIR <- removeFreq(SrE.IR, rem.freq = SrE.IR$freq > 3500)
#'
#'   # Remove frequencies from both ends at once:
#'   newIR <- removeFreq(SrE.IR, rem.freq = SrE.IR$freq > 3500
#'   | SrE.IR$freq < 800)
#'
#'   # Remove frequencies from the middle:
#'   newIR <- removeFreq(SrE.IR, rem.freq = SrE.IR$freq > 800
#'   & SrE.IR$freq < 1000)
#'
#'   # The logic of this last one is as follows.  Any values
#'   # that are TRUE will be removed.
#'   values <- 1:7
#'   values > 2
#'   values < 6
#'   values > 2 & values < 6
#'
#'   # Examples where rem.freq is a formula
#'
#'   # Remove frequencies from one end:
#'   newIR <- removeFreq(SrE.IR, rem.freq = 3500 ~ high)
#'
#'   # Remove frequencies from both ends is a two step process with formulas:
#'   newIR <- removeFreq(SrE.IR, rem.freq = 3500 ~ high)
#'   newIR <- removeFreq(newIR, rem.freq = low ~ 800)
#'
#'   # Remove frequencies from the middle:
#'   newIR <- removeFreq(SrE.IR, rem.freq = 800 ~ 1000)
#'
#'   # After any of these, inspect the results:
#'   sumSpectra(newIR)
#' }
#'
#' if (checkForPackageWithVersion("ChemoSpec2D", 0.5)) {
#'   library("ChemoSpec2D")
#' # Note we will set contours a bit low to better
#' # show what is going on.
#'
#'   data(MUD1)
#'
#'   plotSpectra2D(MUD1, which = 7, lvls = 0.1, cols = "black",
#'     main = "MUD1 Sample 7: Complete Data Set")
#'
#'   MUD1a <- removeFreq(MUD1, remF2 = 2.5 ~ 4)
#'   sumSpectra(MUD1a) # don't plot, removing peaks from interior is misleading
#'
#'   MUD1b <- removeFreq(MUD1, remF2 = low ~ 2)
#'   sumSpectra(MUD1b)
#'   plotSpectra2D(MUD1b, which = 7, lvls = 0.1, cols = "black",
#'     main = "MUD1 Sample 7\nRemoved Peaks: F2 low ~ 2")
#'
#'   MUD1c <- removeFreq(MUD1, remF1 = high ~ 23)
#'   sumSpectra(MUD1c)
#'   plotSpectra2D(MUD1c, which = 7, lvls = 0.1, cols = "black",
#'     main = "MUD1 Sample 7\nRemoved Peaks: F1 high ~ 23")
#'
#'   MUD1d <- removeFreq(MUD1, remF2 = 2.5 ~ 4, remF1 = 45 ~ 55)
#'   sumSpectra(MUD1d)  # don't plot, removing peaks from interior is misleading
#'
#' }
removeFreq <- function(spectra, rem.freq = NULL, remF2 = NULL, remF1 = NULL) {
  UseMethod("removeFreq")
}
