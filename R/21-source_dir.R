#' @title source_dir
#' @description source wszystkich skryptów (*.R) ze wskazanego katalogu
#' 
#' @importFrom assertthat not_empty
#' @param path character; ścieżka do katalagu
#'
#' @return NULL
#' @export
#'
source_dir <- function(path) {
  
  if (!is.character(path))
    stop("Agrument 'path' musi byc klasy 'character'")
  
  if (!file.exists(path))
    stop(sprintf("Sciezka '%s' nie istnieje.", path ))
  
  file.sources <- list.files( path, pattern = ".*\\.R$", full.names = TRUE, ignore.case = TRUE)
  
  if (not_empty(file.sources) == F)
    stop(sprintf("Directory '%s' is empty.", path))
  
  out <- sapply(file.sources, source, .GlobalEnv)
  message("Sourced files:")
  print(colnames(out))
}

