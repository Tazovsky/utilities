#' @title list2char
#' @description Changes 'list' class to 'character' in data.table column
#' @import data.table
#' @param data data.table; 
#' @param except character vector; column names we don't want to change to character
#' @param sep character; to separate values; if NULL then value is set to ", "; 
#'
#' @return data.table 
#' @export
#' @examples 
#' data <- data.table(a = c(1,2,3), b = list(c("a", "b"), "c", c("d", "e")))
#' data_no_list <- list2char(data)
list2char <- function(data, except = NULL, sep = NULL){
  
  stopifnot(is.data.table(data))
  
  if (any(sapply(data, is.list)) == F ) {
    warning("None of columns is 'list' class. Returns unchanged 'data'.")
    return(data)
  }
  
  if ( is.character(except) == F & length(except) > 0  )
    stop("'except' is not a character vector.")
  
  cols_not_in <- except[!except %in% names(data)]
  
  if (length(cols_not_in) > 0 )
    stop(sprintf("There are no columns like: %s.", paste0(cols_not_in, collapse = ", ")))
  
  if (!is.null(sep) && !is.character(sep))
    stop("'sep' is not a character class")
  
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
