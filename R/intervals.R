#' Plots an inner and outer quantile interval for a list or matrix of measurements.
#'
#' @param x list of vectors or matrix to plot
#' @param .names names for use on the y-axis
#' @param .inner width of the inner quantile interval. Defaults to 0.75
#' @param .outer width of the outer quantile interval. Defaults to 0.95
#' @param .main main title of the plot. Passed directly to \code{plot(main=.main)}
#' @param .width width of the interval strips
#' @export
intervals <- function(x, .names=NULL, .inner=0.75, .outer=0.95, .main="",
                      .width=0.5) {
  .inner <- 1 - .inner
  .outer <- 1 - .outer
  .probs <- c(.outer/2, .inner/2, 0.5, 1-.inner/2, 1-.outer/2)
  if (class(x) == "list") {
    .quantiles <- plyr::laply(x, quantile, probs=.probs)
  } else if(class(x) == "matrix") {
    .quantiles <- plyr::aaply(x, 1, quantile, probs=.probs)
  } else {
    stop("x must be list or matrix")
  }

  if (!is.null(.names)) {
    op <- par(mar = c(4,10,2,2) + 0.1)
  } else {
    op <- par(mar = c(4,2,2,2) + 0.1)
  }
  n_c <- nrow(.quantiles)

  plot(range(.quantiles), c(0, n_c+0.5), type="n",
       yaxt="n", bty="n", ylab="", xlab="", main=.main)
  if (! is.null(.names)) {
    axis(2, at=1:n_c, labels=.names, las=1, tick=F)
  }

  plyr::a_ply(1:n_c, 1, function(i) {
    rect(.quantiles[i,1], i-.width/2, .quantiles[i,5], i+.width/2, border=NA, col="gray87")
    rect(.quantiles[i,2], i-.width/2, .quantiles[i,4], i+.width/2, border=NA, col="gray25")
    lines(x=rep(.quantiles[i,3], 2), y=c(i-.width/2, i+.width/2), col=par("bg"))
  })
}