#'
#' Check spectra and pca Arguments of Functions That Require Them
#'
#' @template authors-BH
#' @export
#' @noRd
#'
.chkArgs <- function(mode = 11L) {

  # The following is from stackoverflow.com/a/53137483/633251
  fargs <- function(n) {
    mget(names(formals(sys.function(n))), sys.frame(n), inherits = TRUE)
  }
  args <- fargs(-2) # 2 because the helper function fargs is yet another level down

  # print(data.frame(cls = unlist(lapply(args, class)))) # SAVE for debugging

  if (mode == 0L) {
    specOK <- FALSE
    specOK <- any(inherits(args$spectra, "Spectra"), inherits(args$spectra, "Spectra2D"))
    if (!specOK) stop("Argument 'spectra' was not found or did not have class Spectra or Spectra2D")
  }

  if (mode == 11L) {
    if (!inherits(args$spectra, "Spectra")) {
      stop("Argument 'spectra' was not found or not a Spectra object")
    }
  }

  if (mode == 21L) {
    if (!inherits(args$spectra, "Spectra2D")) {
      stop("Argument 'spectra' was not found or not a Spectra2D object")
    }
  }

  if (mode == 12L) {
    if (!inherits(args$spectra, "Spectra")) {
      stop("Argument 'spectra' was not found or not a Spectra object")
    }
    # PCA methods for Spectra objects all have prcomp as the return class
    pcaOK <- FALSE
    pcaOK <- any(inherits(args$pca, "prcomp"), inherits(args$so, "prcomp"))
    # pcaOK <- any("prcomp" %in% class(args$pca), "prcomp" %in% class(args$so))
    if (!pcaOK) stop("Argument 'pca' was not found or did not have class prcomp")
  }

  if (mode == 22L) {
    if (!inherits(args$spectra, "Spectra2D")) {
      stop("Argument 'spectra' was not found or not a Spectra2D object")
    }
    # PCA methods for Spectra2D objects have varying classes for return value
    pcaOK <- FALSE
    # pcaOK <- any("mia" %in% class(args$so),
    # "pfac" %in% class(args$so),
    # "pop" %in% class(args$so),
    # "mia" %in% class(args$pca), # last 3 needed for unit tests
    # "pfac" %in% class(args$pca),
    # "pop" %in% class(args$pca))
    pcaOK <- any(
      inherits(args$so, "mia"),
      inherits(args$so, "pfac"),
      inherits(args$so, "pop"),
      inherits(args$pca, "mia"), # last 3 needed for unit tests
      inherits(args$pca, "pfac"),
      inherits(args$pca, "pop")
    )
    if (!pcaOK) stop("Argument 'so' was not found or did not have class mia/pfac/pop")
  }
} # end of chkArgs
