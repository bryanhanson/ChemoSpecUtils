#'
#' chkSpectra.Spectra2D
#'
#' @template authors-BH
#' @export
#' @noRd
#'
#' @tests tinytest
#' ### Unit tests for chkSpectra.Spectra2D
#' load("tiny2D.RData")
#'
#' tiny2D_NAc <- tiny2D
#' tiny2D_NAc$data[[1]][,4] <- NA # single spectrum with col of NAs
#'
#' # chkSpectra detects mismatched col NAs
#' expect_error(chkSpectra(tiny2D_NAc), "NAs are present in the data but differ between samples")
#' tiny2D_NAr <- tiny2D
#' tiny2D_NAr$data[[1]][3,] <- NA # single spectrum with row of NAs
#'
#' # chkSpectra detects mismatched row NAs
#' expect_error(chkSpectra(tiny2D_NAr), "NAs are present in the data but differ between samples")
#' tiny2D_NAc$data[[2]][,4] <- NA 
#' tiny2D_NAc$data[[3]][,4] <- NA 
#'
#' # .findNA reports col NAs correctly
#' expect_equal(.findNA(tiny2D_NAc)$colNA, 4)
#' tiny2D_NAr$data[[2]][3,] <- NA 
#' tiny2D_NAr$data[[3]][3,] <- NA 
#'
#' # .findNA reports row NAs correctly"
#' expect_equal(.findNA(tiny2D_NAr)$rowNA, 3)
#' 
#' tiny2D_NAmr <- tiny2D
#' M <- tiny2D_NAmr$data[[1]]
#' M <- M[-1,]
#' tiny2D_NAmr$data[[1]] <- M
#' 
#' # chkSpectra detects matrices with differing no. rows
#' expect_error(chkSpectra(tiny2D_NAmr), "Data matrices do not have the same dimensions")
#' 
#' tiny2D_NAmc <- tiny2D
#' M <- tiny2D_NAmc$data[[1]]
#' M <- M[,-1]
#' tiny2D_NAmc$data[[1]] <- M
#' 
#' # chkSpectra detects matrices with differing no. cols
#' expect_error(chkSpectra(tiny2D_NAmc), "Data matrices do not have the same dimensions")
#' 
#' tiny2D_NAmm <- tiny2D
#' tiny2D_NAmm$data[[1]][,6] <- NA
#' tiny2D_NAmm$data[[2]][,5] <- NA
#' 
#' # chkSpectra detects matrices with NAs in different positions
#' expect_error(chkSpectra(tiny2D_NAmm), "NAs are present in the data but differ between samples")
#' 

chkSpectra.Spectra2D <- function(spectra, confirm = FALSE) {

  # Check classes/types of each element

  trouble <- FALSE
  extra <- FALSE
  if (!inherits(spectra, "Spectra2D")) {
    warning("The object provided was not a Spectra2D object")
    trouble <- TRUE
  }
  if (!inherits(spectra$F2, "numeric")) {
    warning("The F2 frequency data are not class numeric")
    trouble <- TRUE
  }
  if (!inherits(spectra$F1, "numeric")) {
    warning("The F1 frequency data are not class numeric")
    trouble <- TRUE
  }
  if (!inherits(spectra$data, "list")) {
    warning("The data entry is not class list")
    trouble <- TRUE
  }
  if (!inherits(spectra$names, "character")) {
    warning("The sample names are not class character")
    trouble <- TRUE
  }
  if (!((inherits(spectra$unit, "character")) | (is.expression(spectra$unit)))) {
    warning("The units are not class character or expression")
    trouble <- TRUE
  }
  if (!inherits(spectra$desc, "character")) {
    warning("The description is not class character")
    trouble <- TRUE
  }
  if (!inherits(spectra$groups, "factor")) {
    warning("The assigned groups are not class factor")
    trouble <- TRUE
  }
  if (!inherits(spectra$colors, "character")) {
    warning("The assigned colors are not class character")
    trouble <- TRUE
  }

  # Check that F2 and F1 are sorted ascending

  if (is.unsorted(spectra$F2)) {
    warning("F2 frequency data are not sorted ascending")
    trouble <- TRUE
  }
  if (is.unsorted(spectra$F1)) {
    warning("F1 frequency data are not sorted ascending")
    trouble <- TRUE
  }

  # Check to make sure that every data matrix has the same dim & proper type

  ns <- length(spectra$names)
  dims <- matrix(NA_integer_, ncol = 2, nrow = ns)
  rownames(dims) <- spectra$names
  colnames(dims) <- c("F2", "F1")
  for (i in 1:ns) {
    if (!is.matrix(spectra$data[[i]])) stop("spectra$data entries should be matrices")
    if (!is.numeric(spectra$data[[i]])) stop("spectra$data entries should be numeric matrices")
    dims[i, ] <- c(ncol(spectra$data[[i]]), nrow(spectra$data[[i]]))
  }

  Ucol1 <- length(unique(dims[, 1])) == 1L # returns TRUE/FALSE
  Ucol2 <- length(unique(dims[, 2])) == 1L

  if (!Ucol1 | !Ucol2) {
    print(dims)
    stop("Data matrices do not have the same dimensions.")
  }

  # Check that the data matrices have no attributes except dim (messes with the next check if present)

  M <- spectra$data
  for (i in 1:ns) {
    at <- attributes(M[[i]])
    if (length(setdiff(names(at), "dim") > 0)) stop("Please remove extra attributes in the data matrices")
  }

  # Check that data matrices have NAs in the same positions, if they have them at all
  # Only applies if there is more than one sample.

  if (ns >= 2) {
    M <- spectra$data # list of numeric matrices
    for (i in 1:ns) {
      M[[i]] <- is.na(spectra$data[[i]]) # now a list of logical matrices
    }
    for (i in 2:ns) {
      if (!identical(M[[i]], M[[i - 1]])) stop("NAs are present in the data but differ between samples")
    }
  }

  # Check that the relationships between each element are correct

  F2 <- length(spectra$F2)
  F1 <- length(spectra$F1)
  dd <- dim(spectra$data[[1]])
  g <- length(spectra$groups)
  nc <- length(spectra$colors)
  # note: ns was defined earlier as length(spectra$names)

  if (!identical(F1, dd[1])) {
    warning("Length(F1) != nrow(data)")
    trouble <- TRUE
  }
  if (!identical(F2, dd[2])) {
    warning("Length(F2) != ncol(data)")
    trouble <- TRUE
  }
  if (!identical(ns, g)) {
    warning("The dimensions don't make sense (names, group)")
    trouble <- TRUE
  }
  if (!identical(ns, nc)) {
    warning("The dimensions don't make sense (names, colors)")
    trouble <- TRUE
  }

  # Check for duplicates sample names

  dup <- anyDuplicated(spectra$names)
  if (dup != 0L) {
    message("Duplicate sample names found, please inspect/repair")
    trouble <- TRUE
  }

  # Check for extra list elements and report

  extra <- .extraData(spectra)

  # Wrap up

  if ((!trouble) && (!extra) && (confirm)) message(">>> Everything looks good!")
  if (extra) message("\n\t>>>  Please check the extra data entries.")
  if (trouble) stop("\n>>>  Bummer: There seem to be one or more problems with this data set!")
}
