#' @title source_dir
#' @description sources all R scriptd (*.R) from sepcified directory
#' 
#' @importFrom assertthat not_empty
#' @param path character; path to directory
#'
#' @return NULL
#' @export
#'
source_dir <- function(path) {
  
  if (!is.character(path))
    stop("'path' is not a character")
  
  if (!file.exists(path))
    stop(sprintf("Path '%s' does not exist", path ))
  
  file.sources <- list.files( path, pattern = ".*\\.R$", full.names = TRUE, ignore.case = TRUE)
  
  if (not_empty(file.sources) == F)
    stop(sprintf("Directory '%s' is empty.", path))
  
  out <- sapply(file.sources, source, .GlobalEnv)
  message("Sourced files:")
  print(colnames(out))
}

