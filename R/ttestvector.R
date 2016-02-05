#' T-test vector
#'
#' Runs \code{t.test} for each column in the two input sets
#'
#' @param x First group. Samples in rows
#' @param y Second group. Samples in rows. If \code{y=NULL} (default), a
#'  one-sample test is performed exactly as in \code{t.test}
#' @param adjust Adjust for multiple testing? Boolean indicating whether the
#'  FDR should be controlled by Benjamini-Hochberg correction using \code{p.adjust}.
#' @param ... Additional arguments passed to \code{t.test}
#'
#' @return an object of class "ttestvector" that contains the following elements:
#' \item{tests}{a list that contains the \code{htest} objects corresponding to
#'  each call to \code{t.test}}
#' \item{pvalues}{an array of the adjusted p-values corresponding to each test. If
#'  \code{adjust} was \code{FALSE}, these will be the same as \code{rawpvalues}}
#' \item{rawpvalues}{an array of the unadjusted p-values corresponding to each test.}
#' \item{adjusted}{logical indicating whether p-values were adjusted}
#'
#' @seealso \code{\link{t.test}}, \code{\link{p.adjust}}
#' @export
# TODO: add @example
ttestvector <- function(x, y=NULL, adjust=TRUE, ...) {
  if (!is.null(y) && ncol(x) != ncol(y)) {
    stop("Number of columns must match for the two groups")
  }
  ntests <- ncol(x)
  if(is.null(y)) {
    tests <- plyr::alply(1:ntests, 1, function(i) { t.test(x[, i], y, ...) })
  } else {
    tests <- plyr::alply(1:ntests, 1, function(i) { t.test(x[, i], y[, i], ...) })
  }

  rawpvals <- plyr::laply(tests, function(x) { x$p.value })
  if (adjust) {
    pvals <- p.adjust(rawpvals, method="BH")
  } else {
    pvals <- rawpvals
  }

  ttv <- list(tests=tests, pvalues=pvals, rawpvalues=rawpvals, adjusted=adjust)
  class(ttv) <- "ttestvector"

  return(ttv)
}

#' @export
summary.ttestvector <- function(object) {
  pvalues <- object$pvalues
  intervals_lower <- plyr::laply(object$tests, function(x) { x$conf.int[1] })
  intervals_upper <- plyr::laply(object$tests, function(x) { x$conf.int[2] })

  first_test <- object$tests[[1]]
  if (length(first_test$estimate) > 1) {
    # 2-sample
    estimates <- plyr::laply(object$tests, function(x) { x$estimate[1] - x$estimate[2] })
  } else {
    # 1-sample
    estimates <- plyr::laply(object$tests, function(x) { x$estimate })
  }

  sm <- list(pvalue=pvalues, ci_lower=intervals_lower, ci_upper=intervals_upper,
             estimate=estimates, test=first_test)
  class(sm) <- "summary.ttestvector"
  sm
}

#' @export
as.data.frame.summary.ttestvector <- function(x) {
  x$test <- NULL
  class(x) <- "list"
  data.frame(x, stringsAsFactors=F)
}