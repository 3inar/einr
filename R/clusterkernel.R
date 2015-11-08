#' Bagged cluster kernel
#'
#' Implements the bagged cluster kernel desctibed by Weston et al. Calculates a
#' distance metric between samples based on co-occurence in k-means clustering.
#' The algorithm clusters the data using a range of different values for
#' \code{k} in k-means and then uses the fraction of times two data points occur
#' in  the same cluster as their similarity.
#'
#' @param x A matrix of data points, samples in the rows.
#' @param ncluster Maximum number of clusters to use (maximum \code{k} for
#'  kmeans).
#' @param ninit Number of times to repeat the clusterings. As k-means has some
#'  randomness to it, it might be a good idea to run it several times.
#'
#' @return  A \code{nrow(x)} by \code{nrow(x)} matrix of pairwise similarities in
#' the range [0, 1]
#'
#' @references
#'  Weston, J., Leslie, C., Ie, E., Zhou, D., Elisseeff, A., & Noble, W. S. (2005).
#'  Semi-supervised protein classification using cluster kernels.
#'  Bioinformatics, 21(15), 3241-3247.
#' @export
clusterkernel <- function(x, ncluster, ninit=100) {
  clusters <- plyr::rlply(ninit, function() {
    plyr::alply(2:ncluster, 1, function(g) {
      kmeans(x, centers=g)$cluster
    })
  })
  clusters <- unlist(clusters, recursive=F)

  kc <- plyr::llply(clusters, function(cluster) {
    plyr::aaply(1:nrow(x), 1, function(i) {
      cl <- cluster[i]
      ifelse(cluster == cl, 1, 0)
    })
  })

  Reduce('+', kc)/length(kc)
}
