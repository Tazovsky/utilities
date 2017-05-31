#' @title is_missing
#' @description Sprawdzenie, czy elementy wektora nie są puste (NA, NULL, NaN, "" lub " "; opcjonalnie: +/-Inf)
#' 
#' @param vec vector. Wektor z wartościami, w których spodziewamy się brakujących wartości.
#' @param check_if_finite logical. Jeśli flaga jest TRUE, wartość `Inf` też będzie uznawana za brakującą.
#'
#' @return logical vector
#' @export
#' @examples is_missing(vec = c(1,3, NA, NaN, Inf), check_if_finite = FALSE)
is_missing <- function(vec, check_if_finite = TRUE) {
  
  if ( not_empty(vec) == FALSE )
    stop("Argument 'vec' jest pusty.")
  
  if ( is.vector(vec) == FALSE )
    stop("Argument 'vec' nie jest vectorem.")
  
  check_character <- is.character(vec) 
  ifelse( is.na(vec) | is.null(vec) | (check_character & (vec == "" | vec == " ")) | (check_if_finite & is.infinite(vec)), TRUE, FALSE) 
  
}


