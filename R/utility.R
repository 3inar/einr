# Utility functions

#' Rank features by univariate glm
#'
#' Fits a glm in each predictor and returns the p-value for \eqn{\beta_1} in
#' each glm.
#'
#' @param x A predictor matrix with samples in the rows.
#' @param y A response vector.
#' @param family A description of the error distribution/link function for the
#' glms. For more info see the documentation for \code{glm}.
#' @return Vector of p-values from the glms.
#' @export
glm_rank <- function(x, y, family=gaussian) {
  lms <- apply(x, 2, function(x) { glm(y~x, family=family) } )
  pvals <- sapply(lms, function(x) { summary(x)$coefficients[2, 4] } )

  return(pvals)
}