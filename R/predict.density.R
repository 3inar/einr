#' Predict method for density estimates
#'
#' Predicted values based on density object
#'
#' @param obj an object of class "density"
#' @param oldx the data from which the density was estimated. this is needed
#'   because the \code{density} method does not keep the original data
#' @param newx the values at which you want to compute the density
#'
#' @return a vector of density values at \code{newx}
#'
#' @seealso \code{\link{density}}
#' @export
predict.density <- function(obj, oldx, newx) {
  x <- oldx
  h <- obj$bw

  plyr::aaply(newx, 1, function(nx) {
    mean(dnorm(nx, mean=x, sd=h))
  })
}