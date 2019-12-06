#'
#' sumSpectra.Spectra2D
#'
#' @export
#' @importFrom stats median
#' @noRd
#'
sumSpectra.Spectra2D <- function(spectra, ...) {
  args2 <- as.list(match.call())[-1]
  .chkArgs(mode = 21L)
  chkSpectra(spectra)

  gF1 <- check4Gaps(spectra$F1, silent = TRUE)
  gF2 <- check4Gaps(spectra$F2, silent = TRUE)

  # if (!("tolF1" %in% args2)) gF1 <- check4Gaps(spectra$F1, silent = TRUE)
  # if (!("tolF2" %in% args2)) gF2 <- check4Gaps(spectra$F2, silent = TRUE)

  # if ("tolF1" %in% args2) gF1 <- check4Gaps(spectra$F1, tol = tolF1, silent = TRUE)
  # if ("tolF2" %in% args2) gF2 <- check4Gaps(spectra$F2, tol = tolF2, silent = TRUE)

  # if ("tolF1" %in% args2) {
  # new.args <- list(x = spectra$F1, silent = TRUE, ... = ...)
  # gF1 <- do.call(check4Gaps, new.args)
  # }

  # if ("tolF2" %in% args2) {
  # new.args <- list(x = spectra$F2, silent = TRUE, ... = ...)
  # gF2 <- do.call(check4Gaps, new.args)
  # }

  # Check for NAs in the matrices

  NAindx <- .findNA(spectra, retFreq = FALSE)
  foundNA <- FALSE
  if ((length(NAindx[[1]]) > 0) | (length(NAindx[[2]]) > 0)) foundNA <- TRUE

  # Now print main summary to console

  cat("\n", spectra$desc, "\n\n")

  cat("\tThere are ", length(spectra$names), " spectra in this set.\n\n", sep = "")

  cat("\tThe F2 dimension runs from ", spectra$F2[1], " to ",
    spectra$F2[length(spectra$F2)], " ", as.character(spectra$unit[1]),
    "\n\tand there are ", length(spectra$F2), " data points.\n",
    sep = ""
  )

  if (nrow(gF2) > 1) {
    cat("\n\tThe F2 dimension has gaps. Here are the data chunks:\n\n")
    print(gF2)
  }
  cat("\n")

  cat("\tThe F1 dimension runs from ", spectra$F1[1], " to ",
    spectra$F1[length(spectra$F1)], " ", as.character(spectra$unit[2]),
    "\n\tand there are ", length(spectra$F1), " slices.\n",
    sep = ""
  )

  if (nrow(gF1) > 1) {
    cat("\n\tThe F1 dimension has gaps. Here are the data chunks:\n\n")
    print(gF1)
  }
  cat("\n")

  if (foundNA) cat("\tNAs were found in the data matrices.  To see where, use plotSpectra2D.\n\n")

  cat("\tThe spectra are divided into", length(levels(spectra$groups)), "groups:", "\n\n")
  sg <- sumGroups(spectra)
  print(sg)

  # Check for extra data and report if found
  cat("\n")
  jnk <- .extraData(spectra)

  cat("*** Note: this is an S3 object\nof class 'Spectra2D'\n")
}
