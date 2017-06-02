#' @title unify_case_type
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
unify_case_type <- function(DT_frame,  cols2lower, cols2upper ) {
  
  assert_that(is.data.table(DT_frame))
  assert_that(nrow(DT_frame) > 0 )
  
  if (!is.null(cols2lower) && !is.vector(cols2lower))
    stop("'cols2lower' nie jest wektorem")
  
  if (!is.null(cols2upper) && !is.vector(cols2upper))
    stop("'cols2upper' nie jest wektorem")
  
  if (length(intersect(cols2lower, cols2upper) ) > 0 )
    stop("Nie możesz jednocześnie zmieniać tych samych kolumn na UPPERCASE i LOWERCASE")
    
  # oba argumenty nie moga byc puste bo wtedy stosowanie funkcji nie ma sensu
  if ( (is.null(cols2lower) | length(cols2lower) == 0) &  (is.null(cols2upper) | length(cols2upper) == 0) )
    stop("Oba argumenty 'cols2upper' i 'cols2lower' nie mogą być puste")
  
  if (all(cols2lower %in% names(DT_frame) ) == F)
    stop(sprintf("W 'DT_frame' nie ma kolumn o nazwach: %s", paste0(cols2lower[!cols2lower %in% names(DT_frame)], collapse = ", ") ))
    
  if (all(cols2upper %in% names(DT_frame) ) == F)
    stop(sprintf("W 'DT_frame' nie ma kolumn o nazwach: %s", paste0(cols2upper[!cols2upper %in% names(DT_frame)], collapse = ", ") ))
  
  if (any(sapply(DT_frame[, c(cols2lower, cols2upper), with = F], is.list)))
      stop("Wybrane kolumny w argumentach 'cols2lowe', 'cols2upper' są klasy 'list', której funkcja nie obsługuje.")
  
  
    DT_frame <- copy(DT_frame)  # zeby uniknac modyfikacji tabeli przez referencje pamieci
  
  if (!is.null(cols2lower) && length(cols2lower) > 0 ) {
    
    for (i in 1:length(cols2lower) ) {
      DT_frame[, eval( copy( cols2lower[i] ) ) := tolower( get(eval(cols2lower[i]) ) ) ]
    }
  }
  
  if (!is.null(cols2upper) && length(cols2upper) > 0 ) {  
    
    for (i in 1:length(cols2upper) ) {
      DT_frame[, eval( copy( cols2upper[i] ) ) := toupper( get(eval(cols2upper[i]) ) )]
    }
  }
  return(DT_frame)
}
