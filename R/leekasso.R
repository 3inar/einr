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
#' \item{model}{a glm object fit with the best ten predictors.}
#' \item{pvals}{the p-values from the univariate regressions.}
#' \item{predictors}{the indexes of the best predictors.}
#' \item{x}{the data frame used to fit the model.}
#' \item{y}{the responses used to fit the model.}
#'
#' @seealso \code{\link{glm}}
#' @export
# TODO: add @example
leekasso <- function(x, y, family=gaussian) {
  pvals <- glm_rank(x, y, family=family)
  top <- order(pvals)[1:10]

  lglm <- list()
  lglm$model <- glm(y~x[, top], family=family)
  lglm$predictors <- top
  lglm$pvals <- pvals

  # not 100% sure about keeping the data, let's do for now...
  lglm$x <- x
  lglm$y <- y

  class(lglm) <- "leekasso"
  return(lglm)
}

#' Preddict Method for Leekasso Fits
#'
#' Obtains predictions from the leekasso-fit glm in a leekasso object.
#'
#' @param object A gitted object of "leekasso" class
#' @param newdata An optional data frame of new data to predict the response of.
#'    If omitted, the data used to fit the model is used.
#' @param ... Arguments passed on to \code{predict.glm}
#'
#' @return In general a vector or matrix of predictions, but see also the
#' documentation for \code{predict.glm}.
#'
#' @seealso \code{\link{predict.glm}}
#' @export
# TODO: add @example
predict.leekasso <- function(object, newdata=NULL, ...) {
  if (is.null(newdata)) {
    prediction <- predict(object$model, ...)
  } else {
    prediction <- predict(object$model, newdata[, object$predictors], ...)
  }

  return(prediction)
}