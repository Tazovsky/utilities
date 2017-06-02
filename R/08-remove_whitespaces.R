#' @title remove_whitespaces
#' @description Funkcja usuwa białe znaki takie jak spacja, tabulator, nowa linia, non-breaking spacja. Funkcja jest zwektoryzowana. Jeśli beginning i end są FALSE to usuwa wszyskie białem znaki ze stringa
#' @param string string; 
#' @param beginning logical; jeśli TRUE to usuwa białe znaki z początku stringa
#' @param end logical; jeśli TRUE to usuwa białe znaki z końca stringa
#'
#' @return string bez bialych znaków
#' @export
#'
#' @examples remove_whitespaces("  ad  asd as  ", beginning = FALSE, end = TRUE)
remove_whitespaces <- Vectorize(function(string,  beginning = FALSE, end = FALSE ){
  
  if (is.null(string) || nchar(string) == 0)
    stop("Argument 'string' jest pusty.")

  if (!is.character(string))
    stop("Argument 'string' musi byc klasy 'character'")
  
  
  if (all(is.logical(c(beginning, end))) == F)
    stop("Argumenty 'beginning', 'end' nie sa wartosciami logicznymi.")
  
  
  if (beginning == TRUE ) s1 <- gsub("^\\s+|^\\t+|^\\n+|^\\r+","", string )
  if (end == TRUE ) s1 <- gsub("\\s+$|\\t+$|\\n+$|\\r+$","", string )
  if (beginning == TRUE & end == TRUE) s1 <- gsub("^\\s+|^\\t+|^\\n+|^\\r+|\\s+$|\\t+$|\\n+$|\\r+$","", string )
  if (beginning == FALSE & end == FALSE) s1 <- gsub("\\s|\\t|\\n|\\r","", string )
  
  gsub(intToUtf8(160),"", s1)
})


