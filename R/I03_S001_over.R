#' Return Range over an Iterator
#'
#' A function to adjust a range of index values
#' based on an iterator variable in a loop.
#'
#' @param x A range.
#' @param iter ...
#' @param per ...
#' @param adj ...
#'
#' @return A vector of values.
#'
#' @examples
#' # Pull 3 values per iteration
#' y <- 1:9
#' for ( i in 1:3 ) {
#'   print( y[ over( 1:3, i ) ] )
#' }
#'
#' # Pull first 2 of each 3 values per iteration
#' # using the 'per' argument
#' for ( i in 1:3 ) {
#'   print( y[ over( 1:2, i, per = 3 ) ] )
#' }
#'
#' # Pull last of 3 values per iteration
#' # using the 'per' argument
#' for ( i in 1:3 ) {
#'   print( y[ over( 3, i, per = 3 ) ] )
#' }
#'
#' # Pull 2 values for final 2 sets using the
#' # 'adj' argument
#' y <- 1:8
#' for( i in 1:2 ) {
#'   print( y[ over( 1:2, i, adj = 1 ) ] )
#' }
#'
#' @export

over <- function( x, iter,
                  per = NULL,
                  adj = -1 ) {

  # Ensure inputs are integers
  x <- as.integer( round( x ) )

  if ( is.null( per ) ) {
    per = max( x )
  }

  return( x + per * (iter + adj) )
}
