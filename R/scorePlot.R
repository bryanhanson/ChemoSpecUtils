#'
#' scorePlot
#'
#' @keywords multivariate robust hplot
#' @noRd
#' @export
#' @importFrom plyr dlply llply
#' @importFrom ggplot2 aes_string annotation_custom geom_path scale_color_manual lims
#' @importFrom grid grobTree textGrob gpar
#' @importFrom ggrepel geom_text_repel
#'
.scorePlot <- function(spectra, so,
                       pcs = c(1, 2), ellipse = "none", tol = "none",
                       use.sym = FALSE, leg.loc = "topright", ...) {

  # Save call for possible user-provided xlim and/or ylim (base graphics)

  args <- as.list(match.call())[-1]

  # Step 0. Check the inputs

  if (length(pcs) != 2) stop("You must choose exactly two PC's to plot")

  case <- NULL # set up flags for the different classes of score results, args already checked
  if (inherits(spectra, "Spectra")) case <- "PCA"
  if (inherits(spectra, "Spectra2D")) case <- "MIA"
  if (inherits(so, "pop")) case <- "PCA" # pop returns prcomp so even though 2D treat like it's not
  if (is.null(case)) stop("Could not reconcile data object and scores object.")
  if ((case == "MIA") && (use.sym)) stop("ChemoSpec2D does not support use.sym")

  chkSpectra(spectra)

  # Step 1. Prep the data

  # For base graphics, we need to compute the ellipses *and* use them to set overall plot
  #   limits because base graphics doesn't understand how to set limits when plotting a bunch
  #   of different lines, points etc
  #
  # For ggplot2 graphics, we need to compute the ellipses but don't need to figure out any
  #   plot limits because ggplot2 understands setting the limits for a "whole" plot.
  #   Note that ggplot2 does not have a geom that does robust ellipses, so we have to calculate
  #   our own ellipse data.

  if (case == "PCA") DF <- data.frame(so$x[, pcs], group = spectra$groups)
  if (case == "MIA") DF <- data.frame(so$C[, pcs], group = spectra$groups)
  GRPS <- dlply(DF, "group", subset, select = c(1, 2))

  # Step 1.  Compute the ellipses if requested

  if ((ellipse == "cls") || (ellipse == "rob") || (ellipse == "both")) {
    # Compute ellipses.
    # There must be at least 3 data points per level to make a classic ellipse,
    # more to make a robust ellipse, as at least one (outlying) point may be dropped.

    gr <- sumGroups(spectra)

    for (n in 1:length(gr$group)) {
      if (gr$no.[n] == 1) message("Group ", gr$group[n], "\n\thas only 1 member (no ellipse possible)")
      if (gr$no.[n] == 2) message("Group ", gr$group[n], "\n\thas only 2 members (no ellipse possible)")
      if (gr$no.[n] == 3) message("Group ", gr$group[n], "\n\thas only 3 members (ellipse not drawn)")
    }

    idx <- which(gr$no. > 3) # Index for those groups that will get ellipses
    gr <- gr[idx, ]
    ELL <- llply(GRPS[idx], .computeEllipses) # These are the ellipses we'll need later
  }

  # Step 2.  Branch for each graphics mode.

  go <- chkGraphicsOpt()

  if (go == "base") {
    if ((ellipse == "cls") || (ellipse == "rob") || (ellipse == "both")) {

      # Get limits all possible pieces of the data
      #
      # Keep in mind the ellipses may be quite flattened and hence large.
      # At the same time, the ellipses might be quite round and
      # the scores well outside them, if there is an outlier.
      # Must check all cases!

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
      x.all <- range(x.scores) + diff(range(x.scores)) * 0.05 * c(-1.0, 1.15) # expand slightly for labels
      y.all <- range(y.scores) + diff(range(y.scores)) * 0.05 * c(-1.0, 1.15) # leave room for annotations at top of plot
    }

    # Now we have our limits, plot the scores after accounting for user provided xlim, ylim

    dPargs <- list(PCs = DF[, 1:2], spectra = spectra, case = case, use.sym = use.sym, ... = ...)

    # Allow user to give xlim, ylim but provide good defaults as well
    if (!"xlim" %in% names(args)) dPargs <- c(dPargs, list(xlim = x.all))
    if (!"ylim" %in% names(args)) dPargs <- c(dPargs, list(ylim = y.all))

    do.call(.drawPoints, dPargs)

    # Draw the ellipses if requested.

    if ((ellipse == "cls") | (ellipse == "rob") | (ellipse == "both")) .drawEllipses(ELL, gr, ellipse, use.sym, ...)

    # Do the decorations

    if (case == "PCA") {
      .addMethod(so)
      if (leg.loc != "none") .addLegend(spectra, leg.loc, use.sym, bty = "n")
      .addEllipseInfo(ellipse)
    }

    if (case == "MIA") {
      if (leg.loc != "none") .addLegend(spectra, leg.loc, use.sym = FALSE, bty = "n")
      .addEllipseInfo(ellipse)
    }

    # Label extremes if requested

    if (tol != "none") .labelExtremes(DF[, 1:2], spectra$names, tol)
  } # end of go == "base"

  if (go == "ggplot2") {
    label <- NULL

    x <- y <- name <- NULL # satisfy CRAN check engine
    if (case == "PCA") {
      if (!use.sym) {
        p <- ggplot(DF) +
          geom_point(aes_string(x = colnames(DF)[1], y = colnames(DF)[2]), color = spectra$colors, shape = 20, size = 3) # plotting the points
      }

      if (use.sym) {
        p <- ggplot(DF) +
          geom_point(aes_string(x = colnames(DF)[1], y = colnames(DF)[2]), color = "black", shape = spectra$sym)
      }


      method <- grobTree(textGrob(so$method,
        x = 0.05, y = 0.98, hjust = 0,
        gp = gpar(col = "black", fontsize = 10)
      ))
      p <- p + annotation_custom(method) # Adding the method name
    }

    if (case == "MIA") {
      p <- ggplot(DF) +
        geom_point(aes_string(x = colnames(DF)[1], y = colnames(DF)[2]), color = spectra$colors, shape = 20, size = 3)
    }

    # Changing theme to theme_bw() and removing grids
    p <- p + theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


    if (ellipse == "cls") {
      cls.coords <- llply(ELL, function(x) {
        x[1:2]
      })
      cls.coords <- llply(cls.coords, function(x) {
        do.call(cbind, x)
      })
      df.cls <- .getEllipseCoords(cls.coords)

      # cls.coords.df<-as.data.frame(cls.coords)
      if (!use.sym) {
        p <- p + geom_path(data = df.cls, aes(x = x, y = y, color = name), linetype = 2) +
          scale_color_manual(values = gr$color)
      }

      if (use.sym) {
        color.black <- rep("black", length(gr$color))
        p <- p + geom_path(data = df.cls, aes(x = x, y = y, color = name), linetype = 2) +
          scale_color_manual(values = color.black)
      }

      ell <- grobTree(textGrob(" ' ' ' ' classical ellipse by group",
        x = 0.05, y = 0.95, hjust = 0,
        gp = gpar(col = "black", fontsize = 10)
      ))
      p <- p + annotation_custom(ell)
    }

    if (ellipse == "rob") {
      rob.coords <- llply(ELL, function(x) {
        x[4:5]
      })
      rob.coords <- llply(rob.coords, function(x) {
        do.call(cbind, x)
      })
      df.rob <- .getEllipseCoords(rob.coords)


      if (!use.sym) {
        p <- p + geom_path(data = df.rob, aes(x = x, y = y, color = name)) +
          scale_color_manual(values = gr$color)
      }

      if (use.sym) {
        color.black <- rep("black", length(gr$color))
        p <- p + geom_path(data = df.rob, aes(x = x, y = y, color = name)) +
          scale_color_manual(values = color.black)
      }

      ell <- grobTree(textGrob(" ----- robust ellipses by group",
        x = 0.05, y = 0.95, hjust = 0,
        gp = gpar(col = "black", fontsize = 10)
      ))
      p <- p + annotation_custom(ell)
    }

    if (ellipse == "both") {
      cls.coords <- llply(ELL, function(x) {
        x[1:2]
      })
      cls.coords <- llply(cls.coords, function(x) {
        do.call(cbind, x)
      })
      df.cls <- .getEllipseCoords(cls.coords) # Data frame with cls.coords values

      rob.coords <- llply(ELL, function(x) {
        x[4:5]
      })
      rob.coords <- llply(rob.coords, function(x) {
        do.call(cbind, x)
      })
      df.rob <- .getEllipseCoords(rob.coords) # Data frame with rob.coords values


      if (!use.sym) {
        lines <- rep(2, length(gr$color))
        p <- p + geom_path(data = df.cls, aes(x = x, y = y, color = name), linetype = 2)

        p <- p + geom_path(data = df.rob, aes(x = x, y = y, color = name)) +
          scale_color_manual(values = gr$color)
      }

      if (use.sym) {
        color.black <- rep("black", length(gr$color))
        p <- p + geom_path(data = df.cls, aes(x = x, y = y, color = name))

        p <- p + geom_path(data = df.rob, aes(x = x, y = y, color = name), linetype = 2) +
          scale_color_manual(values = color.black)
      }

      # putting type of ellipse data on the plot
      ell.cls <- grobTree(textGrob(" ' ' ' ' classic ellipse by group",
        x = 0.05, y = 0.95, hjust = 0,
        gp = gpar(col = "black", fontsize = 10)
      ))
      ell.rob <- grobTree(textGrob(" ----- robust ellipses by group",
        x = 0.05, y = 0.92, hjust = 0,
        gp = gpar(col = "black", fontsize = 10)
      ))
      p <- p + annotation_custom(ell.rob) + annotation_custom(ell.cls)
    }

    # label extremes
    if (tol != "none") {
      CoordList <- .getExtremeCoords(DF[, 1:2], spectra$names, tol)
      df <- data.frame(x = CoordList$x, y = CoordList$y, label = CoordList$l)
      p <- p + geom_text_repel(data = df, aes(x = x, y = y, label = label), box.padding = 0.5, max.overlaps = Inf)
    }

    # removing the ggplot legend
    p <- p + theme(legend.position = "none")

    group <- c(NA_real_)
    color <- c(NA_real_)
    for (i in spectra$groups) {
      if (!(i %in% group)) {
        group <- c(group, i)
      }
    }

    for (i in spectra$colors) {
      if (!(i %in% color)) {
        color <- c(color, i)
      }
    }
    group <- group[-1]
    color <- color[-1]

    # If use.sym then color of the legend should be black
    if (use.sym) {
      color <- rep("black", length(group))
    }

    if (leg.loc == "topright") {
      lab.x <- 0.9
      lab.y <- 0.9
    }
    if (leg.loc == "topleft") {
      lab.x <- 0.01
      lab.y <- 0.9
    }
    if (leg.loc == "bottomright") {
      lab.x <- 0.9
      lab.y <- 0.1
    }
    if (leg.loc == "bottomleft") {
      lab.x <- 0.01
      lab.y <- 0.1
    }
    if (leg.loc == "bottom") {
      lab.x <- 0.5
      lab.y <- 0.1
    }

    if (leg.loc == "top") {
      lab.x <- 0.5
      lab.y <- 0.9
    }

    if (leg.loc == "left") {
      lab.x <- 0.01
      lab.y <- 0.5
    }

    if (leg.loc == "right") {
      lab.x <- 0.9
      lab.y <- 0.5
    }


    if (leg.loc != "none") {
      keys <- grobTree(textGrob("Key",
        x = lab.x, y = lab.y + 0.04, hjust = 0,
        gp = gpar(col = "black", fontsize = 10)
      ))

      for (i in 1:length(group)) {
        grob <- grid::grobTree(textGrob(group[i],
          x = lab.x, y = lab.y, hjust = 0,
          gp = gpar(col = color[i], fontsize = 10)
        ))
        lab.y <- lab.y - 0.04
        p <- p + annotation_custom(grob) + annotation_custom(keys)
      }
    }

    return(p)
  } # end of go == "ggplot2"
} # End of plotScores
