#' Euclidean division
#'
#' @param a A number.
#' @param b A number.
#' @return A number, the Greatest Common Divisor of \code{a} and \code{b}.
#' @description Euclidean alorithm finds the Greatest Common Divisor of two integers
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' @examples
#' euclidean(123612,13892347912)
#' euclidean(100,1000)
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