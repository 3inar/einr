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

#' Standard colors for two-class plots
#'
#' Returns two colors to use in plotting.
#'
#' @return Vector of two colors
#' @export
twocolors <- function() {
  c("deepskyblue", "goldenrod")
}

#' Draw bootstrap samples
#'
#' Draws n samples in the range \code{1:n} with replacement. Provides the
#' out-of-bag samples for testing.
#'
#' @param n The size of the data set to draw samples from
#' @param b Number of bootstrap resamples to draw
#' @return Returns an object of class "bootsample" which is a list of \code{b} lists
#' with the following two attributes:
#' \item{sample}{a \code{n}-vector containting a bootstrap sample.}
#' \item{oob}{a vector that contains the data points not in \code{sample}. Can
#' for eg be used as test set.}
#' @export
#' @examples
#' data("iris")
#' nsamples <- nrow(iris)
#' bs <- bootsample(nsamples, 1)  # draw a single bootstrap sample
#'
#' ## split into training and test sets based on the bootstrap resampling
#' train_idx <- bs[[1]]$sample
#' test_idx <- bs[[1]]$oob
#' train <- iris[train_idx, ]
#' test <- iris[test_idx, ]
bootsample <- function(n, b=1000) {
  samples <- plyr::rlply(b, sample(1:n, n, replace=TRUE))
  oobs <- plyr::llply(samples, function(x) { which(!(1:n %in% x)) })

  bs <- plyr::alply(1:b, 1, function(x) { list(sample=samples[[x]], oob=oobs[[x]]) })
  class(bs) <- "bootsample"
  bs
}