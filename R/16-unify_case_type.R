#' @title unify_case_type
#' @description converts selected columns to UPPERCASE or LOWERCASE
#' 
#' @import data.table
#' @importFrom assertthat assert_that
#' @param DT_frame data.table
#' @param cols2lower character vector; column names to convert to LOWERCASE
#' @param cols2upper character vector; column names to convert toUPPERCASE
#'
#' @return data.table
#' @export
#' @examples 
#' data <- data.table(a = c("X", "y", "Z"), b = c("a", "a", "Aa"), c = c("q", "w", "e"))
#' data2 <- unify_case_type(data, cols2lower = c("a", "b"), cols2upper = "c")
unify_case_type <- function(DT_frame,  cols2lower, cols2upper ) {
  
  stopifnot(is.data.table(DT_frame))
  
  assert_that(nrow(DT_frame) > 0 )
  
  if ((!is.null(cols2lower) && !is.vector(cols2lower) ) | is.list(cols2lower))
    stop("'cols2lower' is not a vector")
  
  if ( (!is.null(cols2upper) && !is.vector(cols2upper)) | is.list(cols2upper))
    stop("'cols2upper'is not a vector")
  
  if (length(intersect(cols2lower, cols2upper) ) > 0 )
    stop("Can not convert same columns to UPPERCASE and LOWERCASE")
    
  # oba argumenty nie moga byc puste bo wtedy stosowanie funkcji nie ma sensu
  if ( (is.null(cols2lower) | length(cols2lower) == 0) &  (is.null(cols2upper) | length(cols2upper) == 0) )
    stop("Both 'cols2upper' and 'cols2lower' are empty")
  
  if (all(cols2lower %in% names(DT_frame) ) == F)
    stop(sprintf("Column(s): (%s) are missing in 'DT_frame'", paste0(cols2lower[!cols2lower %in% names(DT_frame)], collapse = ", ") ))
    
  if (all(cols2upper %in% names(DT_frame) ) == F)
    stop(sprintf("Column(s): (%s) are missing in 'DT_frame'", paste0(cols2upper[!cols2upper %in% names(DT_frame)], collapse = ", ") ))
  
  if (any(sapply(DT_frame[, c(cols2lower, cols2upper), with = F], is.list)))
      stop("Some columns from 'cols2lower' and/or 'cols2upper' are 'list' classes in 'DT_frame'. Function does not support lists.")
  
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
