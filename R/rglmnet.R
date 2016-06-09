#' @export
rglmnet <- function(x, y, nfolds=5) {
  # TODO hardcoded B
  bsamples <- bootsample(nrow(x), b=100)

  betas <- plyr::laply(bsamples, function(bs) {
    xb <- x[bs$sample, ]
    yb <- y[bs$sample]
    # TODO hardcoded lasso, binomial
    glmnetfit <- glmnet::cv.glmnet(xb, yb, alpha=1, family="binomial", nfolds=nfolds)

    # TODO hardcoded lambda
    as.numeric(coef(glmnetfit, s="lambda.1se"))
  })

  class(betas) <- "rglmnet"
  betas
}

#' @export
predict.rglmnet <- function(obj, newx) {
  b <- colMeans((obj))
  newx <- cbind(rep(1, nrow(newx)), newx)
  pred <- plyr::aaply(newx, 1, function(x) {
    g <- sum(x*b)
    exp(g)/(1+exp(g))
  })

  pred
}

