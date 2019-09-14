#' Euclidean division
#'
#' @param a A number.
#' @param b A number.
#' @return The Greatest Common Divisor of \code{a} and \code{b}.
#' @examples
#' euclidean(1,1)
#' euclidean(2,1)
euclidean <- function(a,b){
  stopifnot(exprs = {is.numeric(a); is.numeric(b); 
    is.atomic(a); is.atomic(b); length(a)==1; length(b)==1})
  
  while (b!=0){
    t <- b
    b <- a%%b
    a <- t
  }
  return(a)
}