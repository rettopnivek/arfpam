#' Logistic Function
#'
#' This function applies the logistic function to a set of values.
#'
#' @param x A set of values (\code{-Inf} &#8804; x &#8804; \code{Inf}).
#'
#' @return Returns a set of probabilities.
#'
#' @encoding UTF-8
#'
#' @examples
#' logistic( c(0,-2.197225,2.1972255,-Inf,Inf) )
#'
#' @export

logistic = function(x) {
  return( 1/(1+exp(-x)) )
}
