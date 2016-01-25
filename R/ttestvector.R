#' T-test vector
#'
#' Runs \code{t.test} for each column in the two input sets
#'
#' @param x First group. Samples in rows
#' @param y Second group. Samples in rows
#' @param adjust Adjust for multiple testing? Boolean indicating whether the
#'  FDR should be controlled by Benjamini-Hochberg correction using \code{p.adjust}.
#' @param ... Additional arguments passed to \code{t.test}
#'
#' @return an object of class "ttestvector" that contains the following elements:
#' \item{tests}{a list that contains the \code{htest} objects corresponding to
#'  each call to \code{t.test}}
#' \item{pvalues}{an array of the p-values corresponding to each test. If
#'  \code{adjust} was \code{TRUE}, these will be adjusted to control FDR}
#' \item{adjusted}{logical indicating whether p-values were adjusted}
#'
#' @seealso \code{\link{t.test}}, \code{\link{p.adjust}}
#' @export
# TODO: add @example
ttestvector <- function(x, y, adjust=TRUE, ...) {
  if (ncol(x) != ncol(y)) {
    stop("Number of columns must match for the two groups")
  }
  ntests <- ncol(x)
  tests <- plyr::alply(1:ntests, 1, function(i) { t.test(x[, i], y[, i], ...) })
  pvals <- plyr::laply(tests, function(x) { x$p.value })

  if (adjust) {
    pvals <- p.adjust(pvals, method="BH")
  }

  ttv <- list(tests=tests, pvalues=pvals, adjusted=adjust)
  class(ttv) <- "ttestvector"

  return(ttv)
}