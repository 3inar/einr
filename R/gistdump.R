# function for addin that dumps current doc to gist.github.com
#' @export
gistdump <- function() {
  tempf <- file.path(tempdir(), paste0("untitled-", Sys.Date(), ".R"))
  context <- rstudioapi::getSourceEditorContext()
  cat(context$contents, file=tempf, sep='\n')
  desc <- paste0(Sys.Date(), " untitled from rstudio")
  gst <- gistr::gist_create(tempf, description = desc, browse = F)
  file.remove(tempf)

  gst
}
