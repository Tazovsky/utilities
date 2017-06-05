#' @title remove_whitespaces
#' @description removes whitespaves such as space, tab, nee line, non-breaking space; is vectorized; if beginnig and end are TRUE, then removes ALL whitespaces from string
#' @param string string; 
#' @param beginning logical; if TRUE, then removes whitespaces from the beginnig of string
#' @param end logical; jeśli if FALSE, then removes whitespaces from the end of string
#'
#' @importFrom assertthat is.string  
#'
#' @return string bez bialych znaków
#' @export
#'
#' @examples remove_whitespaces("  ad  asd as  ", beginning = FALSE, end = TRUE)
remove_whitespaces <- Vectorize(function(string,  beginning = FALSE, end = FALSE ){
  
  if (is.string(string) == F)
    stop("'string' argument is not a string")
  
  if (is.null(string) || nchar(string) == 0)
    stop("'string' argument is empty")

  if (all(is.logical(c(beginning, end))) == F)
    stop("'beginning' and/or 'end' are not a logical values")
  
  
  if (beginning == TRUE ) s1 <- gsub("^\\s+|^\\t+|^\\n+|^\\r+","", string )
  if (end == TRUE ) s1 <- gsub("\\s+$|\\t+$|\\n+$|\\r+$","", string )
  if (beginning == TRUE & end == TRUE) s1 <- gsub("^\\s+|^\\t+|^\\n+|^\\r+|\\s+$|\\t+$|\\n+$|\\r+$","", string )
  if (beginning == FALSE & end == FALSE) s1 <- gsub("\\s|\\t|\\n|\\r","", string )
  
  gsub(intToUtf8(160),"", s1)
})
