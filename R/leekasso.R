#' Leekasso feature selection
#'
#' Jeff Leek's leekasso for p-value-based feature selection. Performs univariate
#' regressions of the response on each of the predictors and selects the ten
#' predictors with the lowest p-values.
#'
#' @param x A predictor matrix.
#' @param y A response vector.
#' @return Vector containing the indexes of the top ten predictors.
#' @export
# TODO: add @example
leekasso <- function(x, y) {
  lms <- apply(x, 1, function(x) { lm(x~negfollowup) } )
  pvals <- sapply(lms, function(x) { summary(x)$coefficients[2, "Pr(>|t|)"] } )

  return(order(pvals))
}

# @describeIn leekasso
#testman <-function(x) { 5 }