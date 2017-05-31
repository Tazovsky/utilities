#' @title unifyCaseType
#' @description Funkcja w wybrancych kolumanch zmienia wartości na UPPERCASE/LOWERCASE
#' 
#' @import data.table
#' 
#' @param DT_frame data.table
#' @param cols2lower character; wektor nazw kolumn, które mają byc zamienione na LOWER CASE
#' @param cols2upper character; wektor nazw kolumn, które mają byc zamienione na UPPER CASE
#'
#' @return data.table
#' @export
#' @examples 
#' data <- data.table(a = c("X", "y", "Z"), b = c("a", "a", "Aa"), c = c("q", "w", "e"))
#' data2 <- unify_case_type(data, cols2lower = c("a", "b"), cols2upper = "c")
unify_case_type <- function(DT_frame,  cols2lower = c("ac", "cid", "products"), cols2upper = c("country") ) {
  
  stopifnot(is.data.table(DT_frame))
  
  DT_frame <- copy(DT_frame)  # zeby uniknac modyfikacji tabeli przez referencje pamieci
  
  if (!is.null(cols2lower) && length(cols2lower) > 0 ) {
    
    if (all(cols2lower %in% names(DT_frame) ) == F)
      stop(sprintf("W 'DT_frame' nie ma kolumn o nazwach: %s", 
                   paste0(cols2lower[!cols2lower %in% names(DT_frame)], collapse = ", ") ))
    
    for (i in 1:length(cols2lower) ) {
      DT_frame[, eval( copy( cols2lower[i] ) ) := tolower( get(eval(cols2lower[i]) ) ) ]
    }
  }
  
  if (!is.null(cols2upper) && length(cols2upper) > 0 ) {  
    
    if (all(cols2upper %in% names(DT_frame) ) == F)
      stop(sprintf("W 'DT_frame' nie ma kolumn o nazwach: %s", 
                   paste0(cols2upper[!cols2upper %in% names(DT_frame)], collapse = ", ") ))
    
    for (i in 1:length(cols2upper) ) {
      DT_frame[, eval( copy( cols2upper[i] ) ) := toupper( get(eval(cols2upper[i]) ) )]
    }
  }
  return(DT_frame)
}
