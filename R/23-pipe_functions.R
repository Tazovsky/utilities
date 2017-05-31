#' @title \%l0>\%
#' @description Jeśli \code{length(lhs) == 0}, to zamienia na rhs
#' 
#' @param lhs arg to check
#' @param rhs arg to return if lhs length is 0
#'
#' @export
#'
#' @examples list(1, integer(0), character(0), numeric(0), NA,24) %l0>% 666
`%l0>%` <- function(lhs, rhs) {
  if (is.list(lhs)) {
    return(
      lapply(lhs, function(x) if (length(x) == 0) {rhs} else {x} )
    )
  } else {
    
    if (  length(lhs) == 0 )
      return(rhs)
    
    lhs    
  }
}