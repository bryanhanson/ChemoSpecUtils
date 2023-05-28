#'
#' scorePlot
#'
#' *Internal function*.  Parameters etc are documented in [plotScores()].
#'
#' @author `r .writeDoc_Authors(c("BH", "TG"))`
#' @export
#' @importFrom ggplot2 aes_string geom_path scale_color_manual
#' @importFrom plotly add_annotations layout
#' @importFrom magrittr %>%
#' @importFrom data.table as.data.table
#' @keywords internal
#'
.scorePlot <- function(spectra, so,
                       pcs = c(1, 2), ellipse = "none", tol = "none",
                       use.sym = FALSE, leg.loc = "topright", ...) {

  # Save call for possible user-provided xlim and/or ylim (base graphics)

  args <- as.list(match.call())[-1]

  # Step 00. Check the inputs

  if (length(pcs) != 2) stop("You must choose exactly two PC's to plot")

  case <- NULL # set up flags for the different classes of score results, args already checked
  if (inherits(spectra, "Spectra")) case <- "PCA"
  if (inherits(spectra, "Spectra2D")) case <- "MIA"
  if (inherits(so, "pop")) case <- "PCA" # pop returns prcomp so even though 2D treat like it's not
  if (is.null(case)) stop("Could not reconcile data object and scores object.")
  if ((case == "MIA") && (use.sym)) stop("ChemoSpec2D does not support use.sym")

  chkSpectra(spectra)

  # Step 0. Prep the data

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

  # Use a data.table for the ellipse calculations (also for base limit calculations)
  DT <- as.data.table(DF)
  GRPS <- split(DT, by = "group", keep.by = FALSE)

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

    nm <- unlist(lapply(GRPS, nrow)) # keep only groups with > 4 members
    ELL <- lapply(GRPS[nm >= 4], .computeEllipses) # These are the ellipses we'll need later
  }

  # Step 2.  Branch for each graphics mode.

  go <- chkGraphicsOpt()

  # Step 2-base

  if (go == "base") {
    if ((ellipse == "cls") || (ellipse == "rob") || (ellipse == "both")) {

      # Get plot limits all possible pieces of the data
      #
      # Keep in mind the ellipses may be quite flattened and hence large.
      # At the same time, the ellipses might be quite round and
      # the scores well outside them, if there is an outlier.
      # Must check all cases!

      x.scores <- range(lapply(GRPS, subset, select = 1))
      y.scores <- range(lapply(GRPS, subset, select = 2))
      x.ell <- range(lapply(ELL, function(x) {
        range(x[1])
      }))
      y.ell <- range(lapply(ELL, function(x) {
        range(x[2])
      }))
      x.ell.r <- range(lapply(ELL, function(x) {
        range(x[4])
      }))
      y.ell.r <- range(lapply(ELL, function(x) {
        range(x[5])
      }))
      # extend.limits: stackoverflow.com/a/29647893/633251
      x.all <- range(x.scores, x.ell, x.ell.r)
      x.all <- x.all + diff(x.all) * 0.05 * c(-1.0, 1.15) # expand slightly for labels on right of points
      y.all <- range(y.scores, y.ell, y.ell.r)
      y.all <- y.all + diff(x.all) * 0.05 * c(-1.0, 1.15) # leave room for annotations at top of plot
    }

    if (ellipse == "none") {
      x.scores <- range(lapply(GRPS, subset, select = 1))
      y.scores <- range(lapply(GRPS, subset, select = 2))
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
      if (all(leg.loc != "none")) {
        leg.loc <- .prepLegendCoords(go, leg.loc, x.all[1], x.all[2], y.all[1], y.all[2])
        .addLegend(spectra, leg.loc, use.sym, bty = "n")
      }
      .addEllipseInfo(ellipse)
    }

    if (case == "MIA") {
      if (all(leg.loc != "none")) {
        leg.loc <- .prepLegendCoords(go, leg.loc, x.all[1], x.all[2], y.all[1], y.all[2])
        .addLegend(spectra, leg.loc, use.sym = FALSE, bty = "n")
      }
      .addEllipseInfo(ellipse)
    }

    # Label extremes if requested

    if (tol != "none") .labelExtremes(DF[, 1:2], spectra$names, tol)
  } # end of go == "base"

  # Step 2-ggplot2

  if ((go == "ggplot2") || (go == "plotly")) {
    args <- as.list(match.call()[-1]) # Capturing xlabel and ylabel from plotScore call
    xlab <- eval(args$xlab)
    ylab <- eval(args$ylab)
    .chkReqGraphicsPkgs("ggplot2")

    x <- y <- name <- label <- NULL # satisfy CRAN check engine

    ## Prepare the main plot

    if (case == "PCA") {
      if (!use.sym) {
        p <- ggplot(DF) +
          geom_point(aes_string(x = colnames(DF)[1], y = colnames(DF)[2]), color = spectra$colors, shape = 20, size = 3) +
          labs(x = xlab, y = ylab)
      }

      if (use.sym) {
        p <- ggplot(DF) +
          geom_point(aes_string(x = colnames(DF)[1], y = colnames(DF)[2]), color = "black", shape = spectra$sym) +
          labs(x = xlab, y = ylab)
      }

      if (go == "ggplot2") {
        p <- p + .ggAnnotate(so$method, x = 0.05, y = 0.98, just = "left", gp = gpar(fontsize = 8))
      }
    } # end of case == "PCA"

    if (case == "MIA") {
      p <- ggplot(DF) +
        geom_point(aes_string(x = colnames(DF)[1], y = colnames(DF)[2]), color = spectra$colors, shape = 20, size = 3)
    }

    # Change theme to theme_bw() and remove grids
    p <- p + theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

    ## Take care of the ellipse options -- classical

    # Bring in helper function if needed
    if ((ellipse == "cls") | (ellipse == "rob") | (ellipse == "both")) {
      .ggPrepEllipseCoords <- function(data) {
        df <- data.frame(x = numeric(), y = numeric(), name = character())
        for (i in 1:length(data)) {
          x <- c()
          y <- c()
          name <- c()
          total <- length(data[[i]]) / 2
          for (j in 1:total) {
            x <- c(x, data[[i]][j, 1])
            y <- c(y, data[[i]][j, 2])
          }
          name <- rep(names(data)[i], total)
          temp <- data.frame(x, y, name)
          df <- rbind(df, temp)
        }
        return(df)
      }
    }

    if (ellipse == "cls") {
      cls.coords <- lapply(ELL, function(x) {
        x[1:2]
      })
      cls.coords <- lapply(cls.coords, function(x) {
        do.call(cbind, x)
      })
      df.cls <- .ggPrepEllipseCoords(cls.coords)

      if (!use.sym) {
        p <- p + geom_path(data = df.cls, aes(x = x, y = y, color = name), linetype = 2) +
          scale_color_manual(values = gr$color)
      }

      if (use.sym) {
        color.black <- rep("black", length(gr$color))
        p <- p + geom_path(data = df.cls, aes(x = x, y = y, color = name), linetype = 2) +
          scale_color_manual(values = color.black)
      }

      if (go == "ggplot2") {
        p <- p + .ggAnnotate("cls")
      }
    }

    ## Take care of the ellipse options -- robust

    if (ellipse == "rob") {
      rob.coords <- lapply(ELL, function(x) {
        x[4:5]
      })
      rob.coords <- lapply(rob.coords, function(x) {
        do.call(cbind, x)
      })
      df.rob <- .ggPrepEllipseCoords(rob.coords)


      if (!use.sym) {
        p <- p + geom_path(data = df.rob, aes(x = x, y = y, color = name)) +
          scale_color_manual(values = gr$color)
      }

      if (use.sym) {
        color.black <- rep("black", length(gr$color))
        p <- p + geom_path(data = df.rob, aes(x = x, y = y, color = name)) +
          scale_color_manual(values = color.black)
      }

      if (go == "ggplot2") {
        p <- p + .ggAnnotate("rob")
      }
    }

    ## Take care of the ellipse options -- both classical and robust

    if (ellipse == "both") {
      cls.coords <- lapply(ELL, function(x) {
        x[1:2]
      })
      cls.coords <- lapply(cls.coords, function(x) {
        do.call(cbind, x)
      })
      df.cls <- .ggPrepEllipseCoords(cls.coords) # Data frame with cls.coords values

      rob.coords <- lapply(ELL, function(x) {
        x[4:5]
      })
      rob.coords <- lapply(rob.coords, function(x) {
        do.call(cbind, x)
      })
      df.rob <- .ggPrepEllipseCoords(rob.coords) # Data frame with rob.coords values


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


      if (go == "ggplot2") {
        p <- p + .ggAnnotate("both")
      }
    }

    ## Final touches

    if (go == "ggplot2") {
      # label extremes
      if (tol != "none") {
        CoordList <- .getExtremeCoords(DF[, 1:2], spectra$names, tol)
        df <- data.frame(x = CoordList$x, y = CoordList$y, label = CoordList$l)
        p <- p + .ggRepel(df)
      }

      # removing the ggplot legend & adding our own
      p <- p + theme(legend.position = "none")
      if (all(leg.loc != "none")) {
        p <- p + .ggAddLegend(spectra, use.sym, leg.loc)
      }

      return(p)
    } else {
      .chkReqGraphicsPkgs("plotly")
      p <- ggplotly(p, tooltip = c(colnames(DF[1]), colnames(DF[2]), "name"))
      if (tol != "none") {
        CoordList <- .getExtremeCoords(DF[, 1:2], spectra$names, tol)
        df <- data.frame(x = CoordList$x, y = CoordList$y, label = CoordList$l)
        p <- p %>% add_annotations(
          x = df$x, y = df$y, text = df$label, xref = "x",
          yref = "y",
          showarrow = TRUE,
          arrowhead = 4,
          arrowsize = .5,
          ax = 40,
          ay = -25,
          font = list(
            size = 12
          )
        )
      }
      p <- p %>% layout(showlegend = FALSE)
      return(p)
    }
  } # end of go == "ggplot2" and go == "plotly"
} # End of plotScores
