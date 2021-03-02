#'
#' Assign Group Membership and Colors for a Spectra or Spectra2D Object
#'
#' A utility function which looks for \code{gr.crit} in the file names of .csv
#' files and assigns group membership.  Also assigns a color,
#' and for \code{Spectra} objects, a symbol and an
#' alternate symbol to each group.  Warnings are given if there are file names
#' that don't match entries in \code{gr.crit} or there are entries in
#' \code{gr.crit} that don't match any file names.  An internal function, not
#' generally called by the user.
#'
#' @param spectra An object of S3 class \code{\link[ChemoSpec]{Spectra}} or
#' \code{\link[ChemoSpec2D]{Spectra2D}}.
#'
#' @param gr.crit As per \code{\link{files2SpectraObject}}.
#'
#' @param gr.cols As per \code{\link{files2SpectraObject}}.
#'
#' @return A \emph{complete} object of S3 class \code{Spectra} or \code{\link[ChemoSpec2D]{Spectra2D}}.  This
#' function is the last internal step in creating these objects.
#' Until this function has done its job, these objects
#' will not pass checks as the assembly is not complete
#' (see \code{\link[ChemoSpecUtils]{chkSpectra}}).
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso \code{\link[ChemoSpec]{files2SpectraObject}} or
#' \code{files2Spectra2DObject}for details;
#' \code{\link[ChemoSpecUtils]{sumGroups}} to see the outcome.
#'
#' @keywords utilities color
#'
#' @export
#'
#' @importFrom utils data
#'
#' @noRd
#'
.groupNcolor <- function(spectra, gr.crit = NULL, gr.cols = "auto", mode = "1D") {
  msg1 <- "At least one file name did not correspond any entry in gr.crit and its group is thus NA"
  msg2 <- "More groups than colors, some colors will be NA.\n  Redefine groups or specify colors another way."
  msg3 <- "Too many groups to use the preferred symbols; setting all symbols to 1\n  and alt.sym to 'a'. Assign symbols manually."

  builtInColors <- c("auto", "Col7", "Col8", "Col12")
  builtIn <- FALSE
  if (gr.cols[1] %in% builtInColors) builtIn <- TRUE # flags selection of a builtIn color
  colorsAssigned <- FALSE
  symbolsAssigned <- FALSE
  ng <- length(gr.crit) # no. of groups
  ns <- length(spectra$names) # no. of spectra

  # Use the group criteria (gr.crit) to classify the samples

  spectra$groups <- rep(NA_character_, ns)

  for (i in 1:ng) {
    which <- grep(gr.crit[i], spectra$names)
    if (length(which) == 0) warning("There was no match for gr.crit value ", gr.crit[i], " among the file names.")
    spectra$groups[which] <- gr.crit[i]
  }

  spectra$groups <- as.factor(spectra$groups)
  if (any(is.na(spectra$groups))) warning(msg1)

  # Assign each group a color for plotting

  spectra$colors <- rep(NA_character_, ns)

  if (gr.cols[1] == "auto") {
    if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
      stop("You need to install package RColorBrewer or supply the colors yourself")
    }

    if (ng > 8) warning(msg2)
    cscols <- RColorBrewer::brewer.pal(8, "Set1") # 9 colors in Set1, only using 8 so as to match symbol restrictions
    gr.cols <- cscols[1:ng]

    for (i in 1:ng) {
      which <- grep(gr.crit[i], spectra$names)
      spectra$colors[which] <- gr.cols[i]
    }

    colorsAssigned <- TRUE
  }

  if (gr.cols[1] == "Col7") {
    if (ng > 7) warning(msg2)
    gr.cols <- ChemoSpecUtils::Col7[1:ng]

    for (i in 1:ng) {
      which <- grep(gr.crit[i], spectra$names)
      spectra$colors[which] <- gr.cols[i]
    }

    colorsAssigned <- TRUE
  }

  if (gr.cols[1] == "Col8") {
    if (ng > 8) warning(msg2)
    gr.cols <- ChemoSpecUtils::Col8[1:ng]

    for (i in 1:ng) {
      which <- grep(gr.crit[i], spectra$names)
      spectra$colors[which] <- gr.cols[i]
    }

    colorsAssigned <- TRUE
  }

  if (gr.cols[1] == "Col12") {
    if (ng > 12) warning(msg2)
    gr.cols <- ChemoSpecUtils::Col12[1:ng]

    for (i in 1:ng) {
      which <- grep(gr.crit[i], spectra$names)
      spectra$colors[which] <- gr.cols[i]
    }

    colorsAssigned <- TRUE
  }

  if (!builtIn) { # User is providing a vector of colors
    if (length(gr.cols) != ng) stop("Length of gr.cols and gr.crit did not match")
    for (i in 1:ng) {
      which <- grep(gr.crit[i], spectra$names)
      spectra$colors[which] <- gr.cols[i]
    }
    colorsAssigned <- TRUE
  }

  if (!colorsAssigned) stop("Did not encounter a valid color specification!")

  # Fix symbols for Spectra objects / mode = 1D
  # Associate symbols and alt.sym with each gr.crit; Trying to keep original behavior as well as give new options

  if (mode == "1D") {
    if (ng <= 8) {
      sym1 <- ChemoSpecUtils::Sym8[1:ng]
      sym2 <- letters[1:ng]

      for (i in 1:ng) {
        which <- grep(gr.crit[i], spectra$names)
        spectra$sym[which] <- sym1[i]
        spectra$alt.sym[which] <- sym2[i]
      }
      symbolsAssigned <- TRUE
    }

    if ((ng >= 9) & (ng <= 12)) {
      sym1 <- ChemoSpecUtils::Sym12[1:ng]
      sym2 <- letters[1:ng]
      for (i in 1:ng) {
        which <- grep(gr.crit[i], spectra$names)
        spectra$sym[which] <- sym1[i]
        spectra$alt.sym[which] <- sym2[i]
      }
      symbolsAssigned <- TRUE
    }

    if (ng > 12) {
      spectra$sym <- rep(1L, ns)
      spectra$alt.sym <- rep("a", ns)
      warning(msg3)
      symbolsAssigned <- TRUE
    }

    if (!symbolsAssigned) stop("Was not able to assign symbols!")
    class(spectra) <- "Spectra"
  } # end of mode = 1D

  if (mode == "2D") {
    class(spectra) <- "Spectra2D"
  }

  spectra
}
