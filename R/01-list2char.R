#' @title list2char
#' @description Funkcja zamieniająca klasę list na string w kolumnach data.table
#' @import data.table
#' @param data data.table; dane w postaci data.table 
#' @param except character vector; nazwy kolumn podawane w wektorze, których nie chcemy zamieniać na character
#' @param sep character; separator zamienianych kolumn w data.table
#'
#' @return data.table 
#' @export
#' @examples 
#' data <- data.table(a = c(1,2,3), b = list(c("a", "b"), "c", c("d", "e")))
#' data_no_list <- list2char(data)
list2char <- function(data, except = NULL, sep = NULL){
  
  if (any(sapply(data, is.list)) == F ) {
    message("W danych nie ma zadnej kolumny klasy 'list'. Zwracam niezmieniony argument 'data'.")
    return(data)
  }
  
  stopifnot(is.data.table(data))
  
  if ( length(except) > 0 && is.vector(except) == F )
    stop("Argument 'except' nie jest wektorem.")
  
  cols_not_in <- except[!except %in% names(data)]
  
  if (length(cols_not_in) > 0 )
    stop(sprintf("Niepoprawne nazwy kolumn w argumencie 'except': %s.", paste0(cols_not_in, collapse = ", ")))
  
  # nie chcemy zeby zmienila sie struktura danych wejsciowych poprzez referencje do pamieci stad robimy jej kopie w pamieci
  data <- copy(data)
  
  ## wybiera kolumny ktore są listą
  do_zamiany <- colnames(data)[sapply(data, is.list) & !(colnames(data) %in% except)] 
  
  if ( length(do_zamiany) > 0  ) {
    # zamienia na character
    data[, (do_zamiany) := lapply(.SD, function(x)  as.character(x)  )  , .SDcols = do_zamiany]
    data[, (do_zamiany) := lapply(.SD, function(x)  gsub('^c\\(|\\\\|\\)|\\"', "", x)  )  , .SDcols = do_zamiany]
    
    # tutaj wstawiam separator
    if (is.null(sep) == F)  data[, (do_zamiany) := lapply(.SD, function(x)  gsub(", ", sep, x)  )  , .SDcols = do_zamiany]
    
  }
  return(data)
}
