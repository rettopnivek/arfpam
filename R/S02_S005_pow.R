#' Raise a Value to a Power
#'
#' This function raises a value x to a power a. It
#' is useful for improving the readability of code,
#' clearly separating the base and exponent from
#' other aspects of an equation.
#'
#' @param x A numeric vector, the base values.
#' @param a A numeric vector, the exponents.
#'
#' @return Returns the result of raising x to
#'   the power a.
#'
#' @examples
#' 2^4
#' pow( 2, 4 )
#'
#' # Sometimes it can be hard to determine
#' # what is being raised to a power
#' x <- 2 * 13 ^ 4 / 8
#' # This function makes it easier to tell
#' x <- 2 * pow( 13, 4 ) / 8
#'
#' @export

pow = function( x, a ) {

  out = x^a

  return( out )
}
