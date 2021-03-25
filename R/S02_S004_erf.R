#' Error Function
#'
#' Calculates the error function.
#'
#' @param x A vector of values on the real number line.
#'
#' @return A vector of transformed values based on the error function.
#'
#' @examples
#' plot( c(-1,1), c(-3,3), type='n', xlab='x', ylab='erf(x)' )
#' x <- seq(-3,3,length=100)
#' lines(x,erf(x))
#'
#' @export

erf = function(x) {
  return( 2*pnorm( x*sqrt(2), 0, 1 ) - 1 )
}
