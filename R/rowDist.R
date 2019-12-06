#'
#' Compute Distance Between Rows of a Matrix
#'
#' This function is a wrapper to compute the distance between rows of a matrix
#' using a number of methods.  Some of these are available in package
#' \code{\link{stats}} and some in \code{\link[amap]{Dist}} from package \code{amap}.
#' This function determines which method is requested and then the
#' distance calculation is done by the appropriate method. The exception is the
#' cosine distance which is calculated locally.
#'
#' Methods \code{c("euclidean"}, \code{"maximum"}, \code{"manhattan"}, \code{"canberra"},\code{"binary"},
#' \code{"minkowski")} are sent to function \code{\link{dist}} in package
#' \code{\link{stats}} while methods \code{c("pearson"}, \code{"correlation"},
#' \code{"spearman"}, \code{"kendall")} are handled by \code{Dist} in package \code{amap}.
#' See the respective help pages for details. \code{"cosine"} is handled
#' locally.
#'
#' @param x A matrix whose rows will be used for the distance calculation.
#'
#' @param method A character; one of \code{c("pearson"}, \code{"correlation"},
#' \code{"spearman"}, \code{"kendall"}, \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"},
#' \code{"canberra"},\code{"binary"}, \code{"minkowski"}, \code{"cosine")}.
#'
#' @return An object of class \code{dist}.
#'
#' @author Bryan A. Hanson, DePauw University.
#' Suggested by and original code written by Roberto Canteri.
#'
#' @keywords utilities
#'
#' @export
#'
#' @importFrom stats dist as.dist
#'
rowDist <- function(x, method) {

  # Some code suggested by Roberto Canteri, and used with extremely minor modifications

  method <- match.arg(method, c(
    "pearson", "correlation", "spearman", "kendall",
    "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "cosine"
  ))

  if (method %in% c("pearson", "correlation", "spearman", "kendall")) {
    if (!requireNamespace("amap", quietly = TRUE)) {
      stop("You need to install package amap to use this function/option")
    }
    # Note amap::Dist returns some values different, see documentation
    if ((method == "correlation") | (method == "pearson")) distance <- 1 - amap::Dist(x, method = method)
    if ((method == "spearman") | (method == "kendall")) distance <- amap::Dist(x, method = method)
  }

  if (method %in% c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")) {
    distance <- dist(x, method = method)
  }

  if (method == "cosine") { # stackoverflow.com/a/19550737/633251 but note we need the distance
    distance <- as.dist(x %*% t(x) / (sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
  }

  distance
}
