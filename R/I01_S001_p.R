#' Operator to Concatenate Two Strings
#'
#' The operator \code{%p%} combines character vectors.
#'
#' @param x,y \strong{R} objects that can be converted
#'   to character vectors.
#'
#' @return A character vector of the concatenated values.
#'
#' @details The call \code{ x %p% y } is equivalent to the
#' call \code{ paste0(x,y)}; see \code{\link[base]{paste}}
#' for more details.
#'
#' @examples
#' # Combine strings via operator
#' 'Hello' %p% ' ' %p% 'world'
#'
#' # Vectorized
#' x <- 'I like '
#' y <- c( 'cats', 'dogs', 'fish' )
#' x %p% y
#'
#' @export

`%p%` <- function( x, y ) {
  return( paste0( x, y ) )
}
