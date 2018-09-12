#' AIC from glmnet object
#'
#' Calculates AIC modified by effective degrees of freedom from glmnet lasso
#' models
#'
#' @param glm_fit object of type glmnet
#'
#' @export
AIC.glmnet <- function(glm_fit) {
  chisqLR <- glm_fit$nulldev - deviance(glm_fit)

  chisqLR - 2*glm_fit$df
}


