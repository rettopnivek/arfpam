#' Softmax Function
#'
#' A generalization of the logistic function that takes a
#' K-dimensional vector of arbitrary values and converts
#' it to a K-dimensional vector of real values in the range
#' (0,1) that sum to 1. The function is also known as the
#' normalized exponential.
#'
#' The function can take either a vector or a matrix of values.
#' If a matrix is passed in, the function is applied to each row
#' of the matrix.
#'
#' @param x A vector of values from \code{-Inf} to \code{Inf}.
#'
#' @return A vector of values from 0 to 1 that sum to 1.
#'
#' @examples
#' set.seed(3902)
#' ex <- softmax( rnorm(5) )
#' sum( ex ) # Should equal 1
#' mat <- matrix( rnorm(9), 3, 3 )
#' ex <- softmax( mat )
#' rowSums( ex ) # Each row should sum to 1
#'
#' @export

softmax = function(x) {

  # Vector case
  if ( is.vector( x ) ) {
    out = exp(x)/sum( exp(x) )
  }
  # Matrix case
  if ( is.matrix(x) ) {
    out = t( apply( x, 1, function(x) exp(x)/sum( exp(x) ) ) )
  }

  return( out )
}
