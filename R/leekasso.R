#' Leekasso regression
#'
#' Jeff Leek's leekasso for p-value-based feature selection. Performs univariate
#' regressions of the response on each of the predictors and selects the ten
#' predictors with the lowest p-values. Then regressses the response on these
#' ten predictors.
#'
#' @param x A predictor matrix with samples in the rows.
#' @param y A response vector.
#' @param family A description of the error distribution/link function for the
#' glm. For more info see the documentation for \code{glm}.
#'
#' @return \code{leekasso} reurns an object of class "leekasso" with the
#' following components:
#' \item{glm}{a glm object fit with the best ten predictors.}
#' \item{pvalues}{the p-values from the univariate regressions.}
#' \item{top_p}{the indexes of the best predictors.}
#' @export
# TODO: add @example
leekasso <- function(x, y, family=gaussian) {
  pvals <- glm_rank(x, y, family=family)
  top <- order(pvals)[1:10]

  lglm <- list()
  lglm$top_p <- glm(y~x[, top], family=family)
  lglm$pvalues <- pvals
  class(lglm) <- "leekasso"

  return(lglm)
}

# @describeIn leekasso
#testman <-function(x) { 5 }