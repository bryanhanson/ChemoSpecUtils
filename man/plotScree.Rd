% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotScree.R
\name{plotScree}
\alias{plotScree}
\title{Scree Plots from PCA or MIA Analysis of a Spectra or Spectra2D Object}
\usage{
plotScree(pca, style = "alt", ...)
}
\arguments{
\item{pca}{Either:
\itemize{
\item An object of class \code{\link{prcomp}}, modified to include a
list element called \code{$method}, a character string describing the
pre-processing carried out and the type of PCA performed (it appears on the
plot).  This is automatically provided if \code{ChemoSpec} functions
\code{\link[ChemoSpec]{c_pcaSpectra}} or \code{\link[ChemoSpec]{r_pcaSpectra}}
were used to create \code{pca}.
\item An object of class \code{mia} produced by
function \code{miaSpectra2D}.
}}

\item{style}{Character.  One of \code{c("trad", "alt")} giving the style of
plot desired (traditional or alternative).  \code{"trad"} is not supported
for \code{mia} objects.}

\item{\dots}{Parameters to be passed to the plotting routines. \emph{Applies to base graphics only}.}
}
\value{
The returned value depends on the graphics option selected (see \code{\link[=GraphicsOptions]{GraphicsOptions()}}).
\itemize{
\item \code{base}: None.  Side effect is a plot.
\item \code{ggplot2}: The plot is displayed, and a \code{ggplot2} object is returned if the value is assigned.  The plot can be modified in the usual \code{ggplot2} manner.
}
}
\description{
Functions that draw a traditional scree plot, or an alternative style that is perhaps
more informative.  These plots illustrate the variance explained by each component
in a PCA or MIA analysis.
}
\examples{
if (checkForPackageWithVersion("ChemoSpec", 6.0)) {
  library("ChemoSpec")
  data(metMUD1)

  pca <- c_pcaSpectra(metMUD1)
  p1 <- plotScree(pca, style = "trad")
  p1
  p2 <- plotScree(pca, style = "alt")
  p2
}

if (checkForPackageWithVersion("ChemoSpec2D", 0.5)) {
  library("ChemoSpec2D")
  data(MUD1)

  mia <- miaSpectra2D(MUD1)
  plotScree(mia, style = "alt")
}
}
\references{
The idea for the alternative style plot came from the NIR-Quimiometria
blog by jrcuesta, at \url{https://nir-quimiometria.blogspot.com/2012/02/pca-for-nir-spectrapart-004-projections.html}
}
\author{
Bryan A. Hanson (DePauw University), Tejasvi Gupta.
}
\keyword{hplot}
\keyword{multivariate}
