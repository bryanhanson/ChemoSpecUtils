#'
#' Compute Distance Between Rows of a Matrix
#'
#' This function computes the distance between rows of a matrix using a number of methods.
#' It is primarily a wrapper for \code{\link[amap]{Dist}} which provides many options.
#' However, cosine distance is calculated locally.
#' See the reference for an excellent summary of distances and similarities.
#' Keep in mind that distances are always positive by definition.  Further, in the literature one
#' can find the same distance defined different ways.  For instance, the definition of the 
#' \code{"pearson"} and \code{"correlation"} distances differs slightly between the reference below
#' and \code{\link[amap]{Dist}}.  So please study the definitions carefully to get the one you want.
#' The example illustrates the behavior of some common distance definitions.  Notice that \code{"pearson"}
#' and \code{"cosine"} are mathemtically identical for the particular definition of \code{"pearson"}
#' used by \code{\link[amap]{Dist}}.
#' 
#' @param x A matrix whose rows will be used for the distance calculation.
#'
#' @param method Character; one of \code{"cosine"}, \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"},
#'        \code{"canberra"},  \code{"binary"}, \code{"pearson"}, \code{"correlation"}, \code{"spearman"},
#'        \code{"kendall"}, \code{"abspearson"}, \code{"abscorrelation"}.
#'
#' @return An object of class \code{dist}.
#'
#' @template authors-BH
#'
#' @references R. Todeschini, D. Ballabio, V. Consonni
#' "Distances and Similarity Measures in Chemometrics and Chemoinformatics"
#' in \emph{Encyclopedia of Analytical Chemistry} Wiley and Sons, 2020 
#' \doi{10.1002/9780470027318.a9438.pub2}
#' 
#' @tests tinytest
#' M1 <- matrix(c(0.0, 1.0, 0.0, 1.0), nrow = 2, byrow = TRUE) # parallel/colinear vectors
#' M2 <- matrix(c(0.0, 1.0, 0.0, -1.0), nrow = 2, byrow = TRUE) # anti-parallel vectors
#' M3 <- matrix(c(0.0, 1.0, 1.0, 0.0), nrow = 2, byrow = TRUE) # orthogonal vectors
#' 
#' possMeth <- c("cosine", "pearson", "abspearson", "correlation", "abscorrelation", "spearman", "kendall",
#'                 "euclidean", "maximum", "manhattan", "canberra", "binary")
#' 
#' boundMeth1 <- c("abspearson", "abscorrelation")
#' boundMeth2 <- c("pearson", "correlation", "cosine")
#' 
#' # Check that all distances by any method are positive
#' for (i in 1:length(possMeth)) {
#'   expect_true(all(rowDist(M1, possMeth[i]) >= 0.0))
#'   expect_true(all(rowDist(M2, possMeth[i]) >= 0.0))
#'   expect_true(all(rowDist(M3, possMeth[i]) >= 0.0))
#' }
#' 
#' # Check that distances bounded on [0...1] are actually bounded on [0...1]
#' for (i in 1:length(boundMeth1)) {
#'   expect_true((all(rowDist(M1, boundMeth1[i]) >= 0.0)) & (all(rowDist(M1, boundMeth1[i]) <= 1.0)))
#'   expect_true((all(rowDist(M2, boundMeth1[i]) >= 0.0)) & (all(rowDist(M2, boundMeth1[i]) <= 1.0)))
#'   expect_true((all(rowDist(M3, boundMeth1[i]) >= 0.0)) & (all(rowDist(M3, boundMeth1[i]) <= 1.0)))
#' }
#' 
#' # Check that distances bounded on [0...2] are actually bounded on [0...2]
#' for (i in 1:length(boundMeth2)) {
#'   expect_true((all(rowDist(M1, boundMeth2[i]) >= 0.0)) & (all(rowDist(M1, boundMeth2[i]) <= 2.0)))
#'   expect_true((all(rowDist(M2, boundMeth2[i]) >= 0.0)) & (all(rowDist(M2, boundMeth2[i]) <= 2.0)))
#'   expect_true((all(rowDist(M3, boundMeth2[i]) >= 0.0)) & (all(rowDist(M3, boundMeth2[i]) <= 2.0)))
#' }
#' 
#' @keywords utilities
#'
#' @export
#'
#' @importFrom stats dist as.dist
#'
#' @examples
#' # This examples imagines spectra as a series of vectors
#' # on a half unit circle.
#' # 1. Compute half of a unit circle
#' theta <- seq(0, pi, length = 100) 
#' x = cos(theta)
#' y = sin(theta)
#' 
#' # 2. Compute some illustrative vectors
#' # Get tail/origin & tip/head coordinates
#' lt <- length(theta)
#' set.seed(6)
#' tips <- theta[c(1, sample(2:100, 5))]
#' x0 <- y0 <- rep(0.0, lt) # tail/origin at 0,0
#' x1 <- cos(tips) # tips/heads
#' y1 <- sin(tips)
#' 
#' # 3. Compute the distance functions
#' # Bounded distances
#' RDcor <- rep(NA_real_, lt) # correlation distance
#' RDpea <- rep(NA_real_, lt) # pearson distance
#' RDabp <- rep(NA_real_, lt) # abspearson distance
#' RDcos <- rep(NA_real_, lt) # cosine distance
#' 
#' # Unbounded distances
#' RDeuc <- rep(NA_real_, lt) # Euclidean distance
#' RDman <- rep(NA_real_, lt) # manhattan distance
#' 
#' # Compute all
#' np <- 5
#' refVec <- c(seq(0.0, x[1], length.out = np), seq(0.0, y[1], length.out = np))
#' for (i in 1:lt) {
#'   Vec <- c(seq(0.0, x[i], length.out = np), seq(0.0, y[i], length.out = np))
#'   M <- matrix(c(refVec, Vec), nrow = 2, byrow = TRUE)
#'   RDman[i] <- rowDist(M, method = "manhattan")
#'   RDeuc[i] <- rowDist(M, method = "euclidean")
#'   RDcos[i] <- rowDist(M, method = "cosine")
#'   RDcor[i] <- rowDist(M, method = "correlation")
#'   RDpea[i] <- rowDist(M, method = "pearson")
#'   RDabp[i] <- rowDist(M, method = "abspearson")
#' }
#' 
#' # 4. Plots
#' # a. Unit circle w/representative vectors/spectra
#' plot.new()
#' plot.window(xlim = c(-1, 1), ylim = c(0, 1), asp = 1)
#' title(main = "Representative 'Spectral' Vectors on a Unit Half Circle\nReference Vector in Red",
#'   sub = "Each 'spectrum' is represented by a series of x, y points") 
#' lines(x, y, col = "gray") # draw half circle
#' lines(x = x[c(1,100)], y = y[c(1,100)], col = "gray") # line across bottom
#' arrows(x0, y0, x1, y1, angle = 5) # add arrows & a red reference vector
#' arrows(x0[1], y0[1], x1[1], y1[1], col = "red", angle = 5, lwd = 2)
#' 
#' # b. Distances
#' degrees <- theta*180/pi
#' plot(degrees, RDman, type = "l",
#'   xlab = "Angle Between Spectral Vectors and Reference Vector in Degrees",
#'   ylab = "Distance",
#'   main = "Spectral Distance Comparisons\nUsing ChemoSpecUtils::rowDist")
#' abline(h = c(1.0, 2.0), col = "gray")
#' lines(degrees, RDeuc, col = "blue")
#' lines(degrees, RDcos, col = "green", lwd = 4)
#' lines(degrees, RDcor, col = "red")
#' lines(degrees, RDabp, col = "black", lty = 2)
#' lines(degrees, RDpea, col = "black", lty = 3)
#' leg.txt <- c("manhattan", "euclidean", "correlation", "cosine", "pearson", "abspearson")
#' leg.col <- c("black", "blue", "red", "green", "black", "black")
#' leg.lwd <- c(1, 1, 1, 4, 1, 1)
#' leg.lty <- c(1, 1, 1, 1, 3, 2)
#' legend("topleft", legend = leg.txt, col = leg.col, lwd = leg.lwd, lty = leg.lty)
#' 
#'
rowDist <- function(x, method) {
  MethOpts <- c("cosine", "euclidean", "maximum", "manhattan", "canberra",  "binary",
                "pearson", "correlation", "spearman", "kendall", "abspearson", "abscorrelation")
  amapMeth <- MethOpts[-1]
  method <- match.arg(method, MethOpts)

  if (method %in% amapMeth) {
    if (!requireNamespace("amap", quietly = TRUE)) {
      stop("You need to install package amap to use this distance option")
    }
    distance <- amap::Dist(x, method = method) 
  }

  if (method == "cosine") { # stackoverflow.com/a/19550737/633251
    distance <- as.dist(1 - (x %*% t(x) / (sqrt(rowSums(x^2) %*% t(rowSums(x^2))))))
  }

  distance
}
