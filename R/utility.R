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
  lms <- glmvector(x, y, family)
  pvals <- coef(lms)[, "p-value"]

  return(pvals)
}

#' Draw bootstrap samples
#'
#' Draws n samples in the range \code{1:n} with replacement. Provides the
#' out-of-bag samples for testing.
#'
#' @param n The size of the data set to draw samples from
#' @param b Number of bootstrap resamples to draw
#' @return Returns an object of class "bootsample" with the following components
#' \item{samples}{a \code{b} by \code{n} matrix containing bootstrap samples}
#' \item{oob}{a list of b vectors that provide the samples that arent in the
#'  respective bootstrap samples}
#' @export
#' @examples
#' data("iris")
#' nsamples <- nrow(iris)
#' bs <- bootsample(nsamples, 1)  # draw a single bootstrap sample
#'
#' ## split into training and test sets based on the bootstrap resampling
#' train <- iris[bs$sample[1, ], ]
#' test <- iris[bs$oob[[1]], ]
bootsample <- function(n, b=1000) {
  samples <- t(replicate(b, sample(1:n, n, replace=TRUE)))
  oob <- plyr::alply(samples, 1, function(x) { which(!(1:n %in% x)) })

  bs <- list(samples=samples, oob=oob)
  class(bs) <- "bootsample"

  bs
}