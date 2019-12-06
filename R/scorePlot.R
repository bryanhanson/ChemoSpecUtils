#'
#' scorePlot
#'
#' @keywords multivariate robust hplot
#' @noRd
#' @export
#' @importFrom plyr dlply llply m_ply
#'
.scorePlot <- function(spectra, so,
                       pcs = c(1, 2), ellipse = "none", tol = "none",
                       use.sym = FALSE, leg.loc = "topright", ...) {

  # Handle user-provided xlim and/or ylim

  args <- as.list(match.call())[-1] # a COPY of the args for use with do.call

  # Step 0. Check the inputs

  if (length(pcs) != 2) stop("You must choose exactly two PC's to plot")

  case <- NULL # set up flags for the different classes of score results, args already checked
  if (inherits(spectra, "Spectra")) case <- "PCA"
  if (inherits(spectra, "Spectra2D")) case <- "MIA"
  if (inherits(so, "pop")) case <- "PCA" # pop returns prcomp so even though 2D treat like it's not
  if (is.null(case)) stop("Could not reconcile data object and scores object.")
  if ((case == "MIA") && (use.sym)) stop("ChemoSpec2D does not support use.sym")

  chkSpectra(spectra)

  # Prep the data

  if (case == "PCA") DF <- data.frame(so$x[, pcs], group = spectra$groups)
  if (case == "MIA") DF <- data.frame(so$C[, pcs], group = spectra$groups)
  GRPS <- dlply(DF, "group", subset, select = c(1, 2))

  # Step 1.  Compute overall plot limits, incl. ellipses if requested

  if ((ellipse == "cls") || (ellipse == "rob") || (ellipse == "both")) {
    # Compute ellipses and get overall plot limits.
    # Keep in mind the ellipses may be quite flattened and hence large.
    # At the same time, the ellipses might be quite round and
    # the scores well outside them, if there is an outlier.
    # Must check all cases!

    # There must be at least 3 data points per level to make a classic ellipse.
    # Possibly more to make a robust ellipse, as at least one point may be dropped.

    gr <- sumGroups(spectra)

    for (n in 1:length(gr$group)) {
      if (gr$no.[n] == 1) message("Group ", gr$group[n], "\n\thas only 1 member (no ellipse possible)")
      if (gr$no.[n] == 2) message("Group ", gr$group[n], "\n\thas only 2 members (no ellipse possible)")
      if (gr$no.[n] == 3) message("Group ", gr$group[n], "\n\thas only 3 members (ellipse not drawn)")
    }

    idx <- which(gr$no. > 3) # Index for those groups that will get ellipses
    gr <- gr[idx, ]
    ELL <- llply(GRPS[idx], .computeEllipses) # these are the ellipses we'll need later

    x.scores <- range(llply(GRPS, subset, select = 1))
    y.scores <- range(llply(GRPS, subset, select = 2))
    x.ell <- range(llply(ELL, function(x) {
      range(x[1])
    }))
    y.ell <- range(llply(ELL, function(x) {
      range(x[2])
    }))
    x.ell.r <- range(llply(ELL, function(x) {
      range(x[4])
    }))
    y.ell.r <- range(llply(ELL, function(x) {
      range(x[5])
    }))
    # extend.limits: stackoverflow.com/a/29647893/633251
    x.all <- range(x.scores, x.ell, x.ell.r)
    x.all <- x.all + diff(x.all) * 0.05 * c(-1.0, 1.15) # expand slightly for labels on right of points
    y.all <- range(y.scores, y.ell, y.ell.r)
    y.all <- y.all + diff(x.all) * 0.05 * c(-1.0, 1.15) # leave room for annotations at top of plot
  }

  if (ellipse == "none") {
    x.scores <- range(llply(GRPS, subset, select = 1))
    y.scores <- range(llply(GRPS, subset, select = 2))
    # x.all <- range(x.scores)*c(1.0, 1.15) # expand slightly for labels
    # y.all <- range(y.scores)*c(1.0, 1.15) # leave room for annotations at top of plot
    x.all <- range(x.scores) + diff(range(x.scores)) * 0.05 * c(-1.0, 1.15) # expand slightly for labels
    y.all <- range(y.scores) + diff(range(y.scores)) * 0.05 * c(-1.0, 1.15) # leave room for annotations at top of plot
  }

  # Step 2.  Draw the scores.

  dPargs <- list(PCs = DF[, 1:2], spectra = spectra, case = case, use.sym = use.sym, ... = ...)

  # Allow user to give xlim, ylim but provide good defaults as well
  if (!"xlim" %in% names(args)) dPargs <- c(dPargs, list(xlim = x.all))
  if (!"ylim" %in% names(args)) dPargs <- c(dPargs, list(ylim = y.all))

  do.call(.drawPoints, dPargs)

  # Step 3.  Draw the ellipses if requested.

  if ((ellipse == "cls") | (ellipse == "rob") | (ellipse == "both")) .drawEllipses(ELL, gr, ellipse, use.sym, ...)

  # Step 4.  Decorations

  if (case == "PCA") {
    .addMethod(so)
    if (leg.loc != "none") .addLegend(spectra, leg.loc, use.sym, bty = "n")
    .addEllipseInfo(ellipse)
  }

  if (case == "MIA") {
    if (leg.loc != "none") .addLegend(spectra, leg.loc, use.sym = FALSE, bty = "n")
    .addEllipseInfo(ellipse)
  }

  # Step 5.  Label extremes if requested

  if (tol != "none") .labelExtremes(DF[, 1:2], spectra$names, tol)
} # End of plotScores
