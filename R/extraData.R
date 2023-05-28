#'
#' Check for Extra Data Appended to a Spectra or Spectra2D Object
#'
#' *Internal function*.
#'
#' @param spectra `r .writeDoc_Spectra3()`
#' @return.  Logical. `TRUE` if a mismatch was found for the data lengths.  Message are emited as well.
#' 
#' @author `r .writeDoc_Authors("BH")`
#' @export
#' @keywords internal
#'
.extraData <- function(spectra) {
  .chkArgs(mode = 0L)

  trouble <- FALSE
  spec.names <- names(spectra)
  if (inherits(spectra, "Spectra")) {
    reqd.names <- c("freq", "data", "names", "groups", "colors", "sym", "alt.sym", "unit", "desc")
  }
  if (inherits(spectra, "Spectra2D")) {
    reqd.names <- c("F2", "F1", "data", "names", "groups", "colors", "units", "desc")
  }
  extra <- setdiff(spec.names, reqd.names)

  if (length(extra) > 0) {
    # Give the extra data names & check their lengths
    ns <- length(spectra$names)
    for (i in 1:length(extra)) {
      msg <- paste("\tAdditional data was found:", extra[i], sep = " ")
      message(msg)
      if (length(spectra[[extra[i]]]) != ns) {
        msg <- paste("\tThe length of *", extra[i],
          "* did not match the number of samples.\n",
          sep = ""
        )
        message(msg)
        trouble <- TRUE
      }
    }
  }

  return(trouble)
}
