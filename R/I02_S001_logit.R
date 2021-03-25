#' Logit Function
#'
#' This function calculates the logit (log of the odds) of a set of
#' probabilities.
#'
#' @param p A set of probabilities (0 &#8804; p &#8804; 1).
#'
#' @return Returns a set of values that now lie between -Inf and Inf.
#'
#' @encoding UTF-8
#'
#' @examples
#' logit( c(.5,.1,.9,0,1) )
#'
#' @export

logit = function(p) {
  p[p<0 | p>1] = NA
  return( log(p/(1-p)) )
}
