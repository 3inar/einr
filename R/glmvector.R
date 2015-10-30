#' GLM vector
#'
#' Fits univariate GLMs for each of the predictors in your data set. Assumes
#' that each column is a predictor.
#'
#' @param x A predictor matrix with samples in the rows.
#' @param y A response vector.
#' @param family A description of the error distribution/link function for the
#' glm. For more info see the documentation for \code{glm}.
#' @param swapxy A logical value indicating that instead of regressing \code{y}
#'  on each column in \code{x}, you would like to regress the columns in \code{x}
#'  on \code{y}.
#'
#' @return an object of class "glmvector" with the following components:
#' \item{glms}{a list of univariate glms corresponding to the predictors in
#'    \code{x}.}
#' \item{pvalues}{the p-values from the univariate regressions.}
#' \item{slopes}{the estimated slopes from the regressions.}
#' \item{names}{predictor names. Basically \code{colnames(x)}}
#'
#' @seealso \code{\link{glm}}
#' @export
# TODO: add @example
glmvector <- function(x, y, family=gaussian, swapxy=F) {
  if (swapxy) {
    glms <- apply(x, 2, function(x) { glm(x~y, family=family) } )
  } else {
    glms <- apply(x, 2, function(x) { glm(y~x, family=family) } )
  }

  pvalues <- sapply(glms, function(x) { coef(summary(x))[2, 4] } )
  slopes <- sapply(glms, function(x) { coef(summary(x))[2, "Estimate"] } )
  pnames <- colnames(x)

  vec <- list(glms=glms, pvalues=pvalues, slopes=slopes, names=pnames)
  class(vec) <- "glmvector"

  vec
}

#' @export
coef.glmvector <- function(object, ...) {
  c <- cbind(object$slopes, object$pvalues)
  colnames(c) <- c("Slope", "p-value")
  rownames(c) <- object$names
  c
}

#' Plot for glmvector objects
#'
#' Creates a "volcano plot" for the glm vector: plots slope as a function of
#' negative log p-value for each \code{glm} in the vector
#'
#' @param x a \code{glmvector} object.
#' @param ... other parameters passed on to \code{plot()}
#' @export
plot.glmvector <- function(x, ...) {
  coefs <- coef(x)
  coefs[,2] <- -log(coefs[,2])
  colnames(coefs)[2] <- "-log p-value"
  plot(coefs, ...)
}