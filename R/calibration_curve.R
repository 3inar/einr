#' Calibration curve plot
#'
#' Shows the calibration curve of a set of predictions. This is the ratio of the
#' smoothed histogram of predicted probabilities for the "ones" to the smoothed
#' histogram of all predicted probabilities. This estimates the proportion of class
#'  "one" that receives a given predicted probability. Ideally these should be
#'  equal.
#'
#' @param predictions vector of predicted probabilities
#' @param responses vector of zeroes and ones denoting the truth
#'
#' @export
calibration_curve <- function(predictions, responses) {
  if (any(!responses %in% c(0, 1))) {
    stop("responses must be a zero-one vector")
  }
  d_a <- density(predictions)
  d_t <- density(predictions[responses==1], bw=d_a$bw)

  xgrid <- seq(0,1, 0.01)
  prd <- predict(d_t, oldx=predictions[responses==1], newx=xgrid)/predict(d_a, oldx=predictions, newx=xgrid)

  n_true <- sum(responses)
  n_tot <- length(responses)
  prd <- prd*(n_true/n_tot)

  plot(xgrid, prd, main="Calibration curve", xlab="Predicted probability", ylab="True proportion", type = "l")
  abline(0,1, col="grey")

  invisible(list(x=xgrid, y=prd))
}
