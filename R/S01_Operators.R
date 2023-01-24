# Operators
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2022-11-03

# Table of contents
# 1) `%p%`
# 2) Assignment
#   2.1) `%+=%`
#   2.2) `%-=%`
# 3) `%w%`
# 4) `%rows%`
# 5) `%btw%`

#### 1) `%p%` ####
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

#### 2) `%+=%` ####
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

#### 2.1) `%+=%` ####

#' @rdname Assignment
#' @export

`%+=%` <- function(x, y) {
  eval.parent(substitute(x <- x + y))
}

#### 2.2) `%-=%` ####

#' @rdname Assignment
#' @export

`%-=%` <- function(x, y) {
  eval.parent(substitute(x <- x - y))
}

#### 3) `%w%` ####
#' Operator to Check if String is Part of Another String
#'
#' The operator \code{%w%} checks if the string
#' on the left-hand side is contained within the
#' string on the right-hand side.
#'
#' @param x A character string.
#' @param y A character vector.
#'
#' @returns A logical vector, \code{TRUE} if \code{x} is
#' contained in a given element of \code{y}.
#'
#' @examples
#' # Check if a string is part of
#' # another string
#' "A" %w% "ABC"
#' "D" %w% "ABC"
#'
#' # Vectorized for y
#' "A" %w% c("ABC", "DEF", "GHI")
#' @export

`%w%` <- function(x, y) {
  if (length(x) > 1) {
    stop("x must be a single character string", call. = FALSE)
  }

  out <- sapply(y, function(s) grepl(x, s, fixed = T))
  names(out) <- NULL

  return(out)
}

#### 4) `%rows%` ####
#' Operator to Subset Rows While Preserving Attributes
#'
#' The binary operator %rows% returns the subset of
#' specified rows for a data frame without removing
#' column attributes.
#'
#' @param x A data frame
#' @param y An integer vector, logical vector, or
#'   character vector specifying the rows in x to keep.
#'
#' @returns A data frame.
#'
#' @examples
#' dtf <- data.frame(
#'   X1 = 1:4,
#'   X2 = LETTERS[1:4],
#'   X3 = c( TRUE, TRUE, FALSE, FALSE )
#' )
#' attributes( dtf$X1 ) <- list( Example_attr = "Integer" )
#' attributes( dtf$X2 ) <- list( Example_attr = "Character" )
#' attributes( dtf$X3 ) <- list( Example_attr = "Logical" )
#'
#' # Each column has an attribute
#' str( dtf )
#'
#' # Normal indexing removes attributes
#' str( dtf[1:2,] )
#'
#' # Can use operator to avoid losing attributes
#' str( dtf %rows% 1:2 )
#'
#' @export

`%rows%` <- function( x, y ) {

  out <- x[y,]

  K <- ncol( x )

  for ( k in 1:K ) {
    attributes( out[[k]] ) <- attributes( x[[k]] )
  }

  return( out )
}

#### 5) `%btw%` ####
#' Operator to Determine Range Between Which a Value Falls
#'
#' The binary operator %btw% returns a logical vector
#' indicating all values in a vector that fall within
#' a specified lower and upper limit.
#'
#' @param x A numeric vector.
#' @param y Either a numeric vector specifying a lower and upper
#'   limit, or a character string in the form \code{'(x,y)'},
#'   \code{'[x,y]'}, \code{'(x,y]'}, or \code{'[x,y)'}.
#'   Here \code{x} and \code{y} denote the lower and upper
#'   limits, while square brackets indicate the end point
#'   is included, while round parentheses indicate the end
#'   point is excluded.
#'
#' @returns A logical vector.
#'
#' @examples
#' x <- 1:10
#'
#' # Limits specified as numeric vector
#' x[ x %btw% c( 3, 6 ) ]
#'
#' # Limits specified as character string
#' x[ x %btw% '(3,6)' ]
#' x[ x %btw% '[3,6]' ]
#' x[ x %btw% '(3,6]' ]
#' x[ x %btw% '[3,6)' ]
#'
#' @export

`%btw%` <- function( x, y ) {

  if ( is.character(y) ) {

    values <- replace_string( y, c( '(', '[', ')', ']' ) )
    values <- strsplit( values, split = ',', fixed = TRUE )[[1]]
    values <- as.numeric( values )

    if ( '(' %w% y & ')' %w% y ) {
      out <- x > values[1] & x < values[2]
    }
    if ( '(' %w% y & ']' %w% y ) {
      out <- x > values[1] & x <= values[2]
    }
    if ( '[' %w% y & ')' %w% y ) {
      out <- x >= values[1] & x < values[2]
    }
    if ( '[' %w% y & ']' %w% y ) {
      out <- x >= values[1] & x <= values[2]
    }

  } else {

    out <- x > y[1] & x <= y[2]

  }

  return( out )
}

