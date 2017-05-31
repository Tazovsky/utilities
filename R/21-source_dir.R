#' @title source_dir
#' @description source wszystkich skryptów (*.R) ze wskazanego katalogu
#' 
#' @importFrom assertthat not_empty
#' 
#' @param path character; ścieżka do katalagu
#'
#' @return NULL
#' @export
#'
source_dir <- function(path) {
  
  file.sources <- list.files( path, pattern = ".*\\.R$", full.names = TRUE, ignore.case = TRUE)
  
  dirs.list <- list.dirs(path)
  
  
  if (not_empty(file.sources) == F)
    stop(sprintf("Directory '%s' is empty.", path))
  
  out <- sapply(file.sources, source, .GlobalEnv)
  message("Sourced files:")
  print(colnames(out))
}

