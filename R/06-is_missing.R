#' @title is_missing
#' @description Sprawdzenie, czy elementy wektora nie są puste (NA, NULL, NaN, "" lub " "; opcjonalnie: +/-Inf)
#' checks if any vector element is empty (NA, NULL, NaN, "" or " "; optional: +/-Inf) and returns logical vector
#' 
#' @param vec vector
#' @param check_if_finite logical; If TRUE, then `Inf` tis considered as missing too
#'
#' @return logical vector
#' @export
#' @examples is_missing(vec = c(1,3, NA, NaN, Inf), check_if_finite = FALSE)
is_missing <- function(vec, check_if_finite = TRUE) {
  
  if ( is.logical(check_if_finite) == FALSE)
    stop("'check_if_finite' is not a logical value")
  
  if ( not_empty(vec) == FALSE )
    stop("'vec' is empty")
  
  if ( is.vector(vec) == FALSE | is.list(vec)  == T)
    stop("'vec' is not a vector")
  
  check_character <- is.character(vec) 
  ifelse( is.na(vec) | is.null(vec) | (check_character & (vec == "" | vec == " ")) | (check_if_finite & is.infinite(vec)), TRUE, FALSE) 
  
}


