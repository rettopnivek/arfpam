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
#' x <- 1; x %+=% 1; x
#' y <- 1; y %-=% 2; y
#'
#' # Order of operations can be tricky
#' x <- 1; y <- 1
#' x %+=% y/2
#' # Above is equivalent to (x %+=% y)/2
#'
#' # Therefore embed multiple operations in parentheses
#' x <- 1; y <- 1
#' x %+=% (y/2)
#'
#' # Vectorized
#' x <- 1:3; x %+=% 3; x
#' x <- 1:3; x %+=% 2:0; x
#'
NULL

#' @rdname Assignment
#' @export

`%+=%` <- function(x,y) {
  eval.parent(substitute(x <- x + y))
}

#' @rdname Assignment
#' @export

`%-=%` <- function(x,y) {
  eval.parent(substitute(x <- x - y))
}
