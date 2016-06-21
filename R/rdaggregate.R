#' Aggregate the \code{.rda} files in a given directory
#'
#' Read all the files in a directory ending in ".rda" and combine their contents
#' by either \code{cbind} or \code{rbind}. Assumes each \code{.rda} file contains
#' a single object. Assumes array inputs, produces array outputs. Combines by row.
#'
#' @param directory path to the directory to search.
#'
#' @return a matrix containing the contents of the \code{.rda} files
#'  in \code{directory}.
#'
#' @export
aaggr <- function(directory) {
  histobj <- NULL
  files <- list.files(path = directory, pattern = "*.rda")
  for (f in files) {
    objectname <- load(file.path(directory, f))
    histobj <- rbind(histobj, get(objectname))
  }

  histobj
}

#' Aggregate the \code{.rda} files in a given directory
#'
#' Read all the files in a directory ending in ".rda" and combine their contents
#' by either \code{cbind} or \code{rbind}. Assumes each \code{.rda} file contains
#' a single object. Assumes list inputs, produces list outputs. Combines by row.
#'
#' @param directory path to the directory to search.
#'
#' @return list containing the contents of the \code{.rda} files
#'  in \code{directory}.
#'
#' @export
llggr <- function(directory) {
    histobj <- NULL
  files <- list.files(path = directory, pattern = "*.rda")
  for (f in files) {
    objectname <- load(file.path(directory, f))
    histobj <- c(histobj, get(objectname))
  }

  histobj
}