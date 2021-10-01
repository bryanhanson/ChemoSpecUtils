#'
#' plotScree.default
#'
#' @author Bryan A. Hanson (DePauw University), Tejasvi Gupta.
#' @export
#' @noRd
#' @importFrom graphics points abline legend axis
#' @importFrom ggplot2 aes labs ylim theme_bw geom_hline scale_x_continuous geom_point
#' @importFrom ggplot2 theme element_blank xlim annotate scale_y_continuous ggplot geom_line
#' @importFrom plotly ggplotly
#'
plotScree.default <- function(pca, style = "alt", ...) {
  variance <- .getVarExplained(pca)
  cumvariance <- cumsum(variance)
  ncp <- length(variance)
  if (ncp > 10) ncp <- 10

  go <- chkGraphicsOpt()

  if (go == "base") {
    if (style == "trad") {
      plot(c(1:ncp), variance[1:ncp], type = "l", col = "red", xlab = "factor", ylab = "percent", ylim = c(0, 100), ...)
      axis(1, at = c(1:ncp), labels = TRUE)
      points(c(1:ncp), cumvariance[1:ncp], type = "l", col = "blue")

      abline(v = c(1:ncp), h = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), col = "lightgray")
      abline(h = 95, lty = "dashed")

      legend("bottomleft", y = NULL, pca$method, bty = "n", cex = 0.75)
      legend("topright", y = NULL, "cumulative percent", lty = 1, bty = "n", inset = c(0, 0.40), col = "blue", cex = 0.75)
      legend("topright", y = NULL, " individual percent", lty = 1, bty = "n", inset = c(0, 0.50), col = "red", cex = 0.75)
    }

    if (style == "alt") {

      # Handle class specific stuff here, better than separate dispatch
      if (inherits(pca, "prcomp")) {
        plot(rep(1:ncp, each = nrow(pca$x)), as.vector(pca$x[, 1:ncp]),
          type = "p",
          col = "red", xlab = "component", ylab = "scores",
          xlim = c(1, ncp + 0.5), cex = 0.5, xaxt = "n", ...)
        y.pos <- apply(pca$x[, 1:ncp], MARGIN = 2, FUN = range) # used in a moment
      }

      if (inherits(pca, "mia")) {
        plot(rep(1:ncp, each = nrow(pca$C)), as.vector(pca$C[, 1:ncp]),
          type = "p",
          col = "red", xlab = "component", ylab = "scores",
          xlim = c(1, ncp + 0.5), cex = 0.5, xaxt = "n", ...
        )
        y.pos <- apply(pca$C[, 1:ncp], MARGIN = 2, FUN = range) # used in a moment
      }

      axis(1, at = c(1:ncp), labels = TRUE)

      # label with cumulative variance
      lab.txt <- paste(round(cumvariance[1:ncp], 0), "%", sep = "")
      y.pos <- y.pos[2, ]
      y.max <- max(y.pos)
      off <- 0.1 * y.max
      text(c(1:ncp) + 0.35, off, labels = lab.txt, cex = 0.75)
      abline(h = 0, lty = "dashed", col = "gray")

      legend("bottomright", y = NULL, pca$method, bty = "n", cex = 0.75)
      legend("topright", y = NULL, "cumulative percent variance shown to right of PC", bty = "n", cex = 0.75)
    }
  }

  if ((go == "ggplot2") || (go == "plotly")) {

    .chkReqGraphicsPkgs("ggplot2")

    if (style == "trad") {
      df <- data.frame(ncp = c(1:ncp), variance = variance[1:ncp], cumvariance = cumvariance[1:ncp])
      p <- ggplot(df) +
        geom_line(aes(x = ncp, y = variance), color = "red") +
        geom_line(aes(x = ncp, y = cumvariance), color = "blue") +
        labs(x = "factor", y = "percent") +
        ylim(c(0, 100)) +
        theme_bw() +
        geom_hline(yintercept = 95, linetype = "dashed") + # horizontal dashed line
        scale_x_continuous(breaks = 1:ncp) # scaling x axis ticks to whole numbers

      p <- p + .ggAnnotate(pca$method, x = 0.05, y = 0.05, just = "left", gp = gpar(fontsize = 8))
      p <- p + .ggAnnotate("cumulative percent",
        x = 0.98, y = 0.52, just = "right",
        gp = gpar(col = "blue", fontsize = 8))
      p <- p + .ggAnnotate("individual percent",
        x = 0.98, y = 0.48, just = "right",
        gp = gpar(col = "red", fontsize = 8))

      if (go == "ggplot2") {
        return(p)
      } else {
        .chkReqGraphicsPkgs("plotly")
        p <- ggplotly(p, tooltip = c("variance", "cumvariance"))
        return(p)
      }
    }

    if (style == "alt") {
      if (inherits(pca, "prcomp")) {
        x <- rep(1:ncp, each = nrow(pca$x))
        y <- as.vector(pca$x[, 1:ncp])
        df.temp <- data.frame(pca$x)
        sample <- names(df.temp)
        sample <- sample[1:ncp]
        sample <- rep(sample, each = nrow(pca$x))
        df <- data.frame(x = x, y = y, sample = sample)
        p <- ggplot(df, aes(x = x, y = y, label = sample, text = paste("Sample :", sample, "<br>", "Score", y))) +
          geom_point(color = "red", alpha = 0.7, shape = 1) +
          labs(x = "component", y = "scores") +
          theme_bw() +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

        y.pos <- apply(pca$x[, 1:ncp], MARGIN = 2, FUN = range) # used in a moment
      }

      if (inherits(pca, "mia")) {
        x <- rep(1:ncp, each = nrow(pca$C))
        y <- as.vector(pca$C[, 1:ncp])
        df.temp <- data.frame(pca$C)
        sample <- names(df.temp)
        sample <- sample[1:ncp]
        sample <- rep(sample, each = nrow(pca$C))
        df <- data.frame(x = x, y = y, sample = sample)
        p <- ggplot(df, aes(x = x, y = y, label = sample, text = paste("Sample :", sample, "<br>", "Score", y))) +
          geom_point(color = "red", alpha = 0.7, shape = 1) +
          labs(x = "component", y = "scores") +
          xlim(c(1, ncp + 0.5)) +
          theme_bw() +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

        y.pos <- apply(pca$C[, 1:ncp], MARGIN = 2, FUN = range) # used in a moment
      }
      lab.txt <- paste(round(cumvariance[1:ncp], 0), "%", sep = "")
      y.pos <- y.pos[2, ]
      y.max <- max(y.pos)
      off <- 0.1 * y.max
      p <- p + annotate(geom = "text", x = c(1:ncp) + 0.4, y = off, label = lab.txt, size = 3)
      p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "gray")
      p <- p + scale_x_continuous(breaks = 1:ncp)
      p <- p + scale_y_continuous(breaks = c(-0.5, 0.0, 0.5))
      p <- p + .ggAnnotate(pca$method, x = 0.97, y = 0.03, just = "right", gp = gpar(fontsize = 8))
      p <- p + .ggAnnotate("cumulative percent variance shown to the right of PC",
        x = 0.97, y = 0.97,
        just = "right", gp = gpar(fontsize = 8)
      )

      if (go == "ggplot2") {
        return(p)
      } else {
        .chkReqGraphicsPkgs("plotly")
        p <- ggplotly(p, tooltip = "text")
        return(p)
      }
    }
  }
} # end of plotScree.default
