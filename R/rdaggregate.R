#' Aggregate the \code{.rda} files in a given directory
#'
#' Read all the files in a directory ending in ".rda" and combine their contents
#' by either \code{cbind} or \code{rbind}. Assumes each \code{.rda} file contains
#' a single object.
#'
#' @param directory path to the directory to search.
#' @param byrow combine by row or by column. Defaults to \code{TRUE}.
#'
#' @return a matrix or data frame containing the contents of the \code{.rda} files
#'  in \code{directory}.
#'
#' @export
# TODO: add @example
rdaggregate <- function(directory, byrow=T) {
  histobj <- NULL
  files <- list.files(path = directory, pattern = "*.rda")
  for (f in files) {
    objectname <- load(file.path(directory, f))

    if (byrow) {
      histobj <- rbind(histobj, get(objectname))
    } else {
      histobj <- cbind(histobj, get(objectname))
    }
  }

  histobj
}