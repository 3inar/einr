# EXPERIMENTAL, DOCUMENT THIS SOON
#' @export
nearest_centroids <- function(x, y) {
  centroids <- plyr::aaply(levels(y), 1, function(clss) {
    colMeans(x[y==clss, ])
  })

  rownames(centroids) <- levels(y)
  obj <- list(centroids=centroids)
  obj$y = y
  class(obj) <- "nearest_centroids"
  obj
}

# EXPERIMENTAL, DOCUMENT THIS SOON
#' @export
predict.nearest_centroids <- function(obj, newdata, type="response") {
  centroids <- obj$centroids
  classes <- levels(obj$y)

  distance <- plyr::aaply(as.matrix(newdata), 1, function(x) {
    ds <- rep(-1, length(classes))
    names(ds) <- classes
     for (cls in 1:length(classes)) {
       diff <- as.vector(x - centroids[cls, ])
       ds[cls] <- sqrt(diff %*% diff)
    }

    ds
  })

  if (is.null(dim(distance))) {
    distance<- matrix(distance, nrow=1)
    dimnames(distance) <- list(dimnames(newdata)[[1]], classes)
  }

  if (type=="response") {
    ret <- plyr::aaply(distance, 1, function(x) {
      which(x==min(x), arr.ind = T)
    })
    ret <- classes[ret]
    ret <- factor(ret, levels=classes)
  } else if (type=="raw") {
    ret <- distance
  } else if (type=="probability") {
    ret <- NULL
    dsums <- plyr::aaply(distance, 1, sum)
    ret <- plyr::aaply(distance, 2, function(x) {
      1 - x/dsums
    })
    ret <- plyr::aaply(t(ret), 1, function(x) { x/sum(x) })
    ret
  } else stop("unrecognized type argument")

  ret
}