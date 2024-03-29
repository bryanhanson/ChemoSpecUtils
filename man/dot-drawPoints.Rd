% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drawPoints.R
\name{.drawPoints}
\alias{.drawPoints}
\title{Add Points to a Score Plot}
\usage{
.drawPoints(PCs, spectra, case, use.sym, ...)
}
\arguments{
\item{PCs}{Integer.  The PCs to plot.}

\item{spectra}{An object of S3 class \code{\link[ChemoSpec:Spectra]{ChemoSpec::Spectra()}} or \code{\link[ChemoSpec2D:Spectra2D]{ChemoSpec2D::Spectra2D()}}.}

\item{case}{String. The type of data reduction that was done.}

\item{use.sym}{Logical.  Should symbols be used?}

\item{\ldots}{Parameters to be passed to the plotting routines. \emph{Applies to base graphics only}.}
}
\value{
None.  Side effect is a modifed plot.
}
\description{
\emph{Internal function.}
}
\author{
Bryan A. Hanson (DePauw University).
}
\keyword{internal}
