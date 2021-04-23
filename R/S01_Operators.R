# Operators
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-04-22

# Table of contents
# 1) `%p%`
# 2) Assignment
#   2.1) `%+=%`
#   2.2) `%-=%`
# 3) `%w%`

###
### 1)
###

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
#' "Hello" %p% " " %p% "world"
#'
#' # Vectorized
#' x <- "I like "
#' y <- c("cats", "dogs", "fish")
#' x %p% y
#' @export

`%p%` <- function(x, y) {
  return(paste0(x, y))
}

###
### 2)
###

#' Addition/Subtraction Assignment Operators
#'
#' The operator \code{%+=%} adds the right operand
#' to a variable and assigns the resulting value to
#' the variable. The operator \code{%-=%} subtracts
#' the right operand from a variable and assigns the
#' resulting value to the variable.
#'
#' @param x,y Numeric vectors.
#'
#' @return A numeric vector.
#'
#' @details Care must be taken with order of operations.
#' Because \code{%+=%} and \code{%-=%} are functions,
#' they and their arguments will be evaluated first,
#' followed by subsequent operations. Hence a call like
#' \code{ x %+=% 1/2 } will not result in an increment of
#' 0.5 to x. Instead, x will be first be
#' incremented by 1. Then the call \code{ x/2 } will be
#' run with no assignment of values.
#'
#' @references
#' https://stackoverflow.com/questions/5738831/r-plus-equals-and-plus-plus-equivalent-from-c-c-java-etc
#'
#' @name Assignment
#'
#' @examples
#' # Simple assignment
#' x <- 1
#' x %+=% 1
#' x
#' y <- 1
#' y %-=% 2
#' y
#'
#' # Order of operations can be tricky
#' x <- 1
#' y <- 1
#' invisible(x %+=% y / 2)
#' x
#' # Above is equivalent to (x %+=% y)/2
#'
#' # Therefore embed multiple operations in parentheses
#' x <- 1
#' y <- 1
#' x %+=% (y / 2)
#' x
#'
#' # Vectorized
#' x <- 1:3
#' x %+=% 3
#' x
#' x <- 3:1
#' x %-=% 2:0
#' x
NULL

# 2.1)

#' @rdname Assignment
#' @export

`%+=%` <- function(x, y) {
  eval.parent(substitute(x <- x + y))
}

# 2.2)

#' @rdname Assignment
#' @export

`%-=%` <- function(x, y) {
  eval.parent(substitute(x <- x - y))
}

###
### 3)
###

#' Operator to Check if String is Part of Another String
#'
#' The operator \code{%w%} checks if the string
#' on the left-hand side is contained within the
#' string on the right-hand side.
#'
#' @param x A character string.
#' @param y A character vector.
#'
#' @return A logical vector, \code{TRUE} if \code{x} is
#' contained in a given element of \code{y}.
#'
#' @details
#'
#' @examples
#' # Check if a string is part of
#' # another string
#' "A" %w% "ABC"
#' "D" %w% "ABC"
#'
#' # Vectorized for y
#' "A" %w% c( "ABC", "DEF", "GHI" )
#'
#' @export

`%w%` <- function(x, y) {

  if ( length(x) > 1 ) {
    stop( 'x must be a single character string', call. = FALSE )
  }

  out <- sapply( y, function(s) grepl( x, s, fixed = T) )
  names( out ) <- NULL

  return( out )
}

