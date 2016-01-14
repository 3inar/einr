#' Hyperboxplot: for when you have too many dimensions for regular boxplots
#'
#' This is a very naive reimplementation of Karl Broman's \code{manyboxplot},
#' which see \url{https://kbroman.wordpress.com/2012/04/25/microarrays-suck/}.
#'
#' @param x  A matrix from which the plot is produced. Each column is a separate
#'    group or dimension.
#' @param medianorder Logical argument indicating whether the box plots should be
#'    ordered by median, which usually gives you a cleaner plot. Defaults to \code{F}.
#' @param ... Addidional arguments passed to \code{plot()}
#' @return None.
#' @export
hyperboxplot <- function(x, medianorder=F, ...) {
  p <- c(2.5, 5, 25, 50, 75, 95, 97.5)/100
  quant <- apply(x, 2, quantile, probs=p)

  # from colorbrewer
  color_palette <- c(
    "#e7e1ef",  # lighter red
    "#c994c7",
    "#dd1c77"   # deeper red
  )

  if (medianorder) {
    quant <- quant[, order(quant["50%", ])]
  }

  ymax <- max(quant)
  ymin <- min(quant)
  if (!"ylim" %in% names(list(...))) {
    plot(quant["50%", ], type="n", ylim=c(ymin, ymax), ...)
  } else {
    plot(quant["50%", ], type="n", ...)
  }

  lines(quant["2.5%", ], col=color_palette[1])
  lines(quant["97.5%", ], col=color_palette[1])
  lines(quant["5%", ], col=color_palette[2])
  lines(quant["95%", ], col=color_palette[2])
  lines(quant["25%", ], col=color_palette[3])
  lines(quant["75%", ], col=color_palette[3])
  lines(quant["50%", ], col="black")

  # not returning anything for now but might later
  invisible()
}