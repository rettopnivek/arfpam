# Math
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2023-01-03

# Table of contents
# 1) Log-odds and the logistic function
#   1.1) logit
#   1.2) logistic
# 2) softmax
# 3) erf
# 4) pow
# 5) log_of_sum_of_exp_x

#### 1) Log-odds and the logistic function ####

#### 1.1) logit ####
#' Logit Function
#'
#' This function calculates the logit (log of the
#' odds) of a set of probabilities.
#'
#' @param p A set of probabilities (0 &#8804; p &#8804; 1).
#' @param na.rm Logical; if \code{TRUE}, removes \code{NA}
#'   values first.
#'
#' @return Returns a set of values that now lie
#' between -&#8734; and &#8734;.
#'
#' @encoding UTF-8
#'
#' @examples
#' round(logit(c(.5, .1, .9, 0, 1)), 6)
#' @export

logit <- function(p, na.rm = TRUE) {

  # Check for values outside support [0,1]
  values_outside_support <- p < 0 | p > 1

  #< Warn user if any values found
  if (any(values_outside_support)) {
    warning(
      "Values lower than 0 and higher than 1 found - setting to NA",
      call. = FALSE
    )

    # Set values to NA
    p[values_outside_support] <- NA

    #> Close conditional 'Warn user if any values found'
  }

  #< Remove missing values
  if (na.rm) {
    p <- p[!is.na(p)]

    #> Close conditional 'Remove missing values'
  }

  return(log(p / (1 - p)))
}

#### 1.2) logistic ####
#' Logistic Function
#'
#' This function applies the logistic function to
#' a set of values.
#'
#' @param x A set of values (\code{-Inf} &#8804;
#'   x &#8804; \code{Inf}).
#' @param na.rm Logical; if \code{TRUE}, removes \code{NA}
#'   values first.
#'
#' @return Returns a set of values between 0 and 1.
#'
#' @encoding UTF-8
#'
#' @examples
#' round(logistic(c(0, -2.197225, 2.1972255, -Inf, Inf)), 6)
#' @export

logistic <- function(x, na.rm = TRUE) {

  #< Remove missing values
  if (na.rm) {
    x <- x[!is.na(x)]

    #> Close conditional 'Remove missing values'
  }

  return(1 / (1 + exp(-x)))
}

#### 2) softmax ####
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
#' ex <- softmax(rnorm(5))
#' sum(ex) # Should equal 1
#' mat <- matrix(rnorm(9), 3, 3)
#' ex <- softmax(mat)
#' rowSums(ex) # Each row should sum to 1
#' @export

softmax <- function(x) {

  # Vector case
  if (is.vector(x)) {
    out <- exp(x) / sum(exp(x))
  }
  # Matrix case
  if (is.matrix(x)) {
    out <- t(apply(x, 1, function(x) exp(x) / sum(exp(x))))
  }

  return(out)
}

#### 3) erf ####
#' Error Function
#'
#' Calculates the error function.
#'
#' @param x A vector of values on the real number line.
#'
#' @return A vector of transformed values based on the error function.
#'
#' @examples
#' plot(c(-2, 2), c(-1, 1), type = "n", xlab = "x", ylab = "erf(x)")
#' x <- seq(-2, 2, length = 100)
#' abline(h = c(-1, 1), col = "grey")
#' lines(x, erf(x))
#' @export

erf <- function(x) {
  return(2 * pnorm(x * sqrt(2), 0, 1) - 1)
}

#### 4) pow ####
#' Raise a Value to a Power
#'
#' This function raises a value \code{x} to a power
#' \code{a}. It is useful for improving the readability
#' of code, clearly separating the base and exponent
#' from other aspects of an equation.
#'
#' @param x A numeric vector, the base values.
#' @param a A numeric vector, the exponents.
#'
#' @return Returns the result of raising \code{x} to
#'   the power \code{a}.
#'
#' @examples
#' 2^4
#' pow(2, 4)
#'
#' # Sometimes it can be hard to determine
#' # what is being raised to a power
#' x <- 2 * 13^4 / 8
#' # This function makes it easier to tell
#' x <- 2 * pow(13, 4) / 8
#' @export

pow <- function(x, a) {
  out <- x^a

  return(out)
}

#### 5) log_of_sum_of_exp_x ####
#' Compute the Log of the Sum of the Exponent of Log-Values
#'
#' Function to compute the log of the sum of exponent of
#' a set of log-values.
#'
#' @param x A numeric vector of log-values.
#' @param na.rm Logical; if \code{TRUE} removes \code{NA} values.
#'
#' @details It is often useful to take the log of very small
#' or large values (e.g., likelihoods). Working with log-values
#' makes exponentiation, multiplication, and division easier.
#' However, addition and subtraction become harder.
#'
#' Assume \code{ a = log(a.) } and \code{ b = log(b.) }. The identity
#' \code{log(a. + b.) = a + log(1 + exp(b - a))} allows us to compute
#' the log of the sum of the exponentiated values of a and b. This can
#' be further extended to a series of log-values
#' \code{x = {x[1], x[2], ..., x[n]}} as
#' \code{x[1] + log( 1 + exp(x[2] - x[1]) + ... + exp(x[n] - x[1]) )}.
#'
#' @references
#' http://bayesfactor.blogspot.com/2016/05/numerical-pitfalls-in-computing-variance.html
#'
#' https://stackoverflow.com/questions/65233445/how-to-calculate-sums-in-log-space-without-underflow
#'
#' @returns The log of the sum of the exponentiated values of \code{x}.
#'
#' @examples
#' exp_x <- 1:4
#' x <- log( exp_x )
#' log( sum( exp_x) )
#' log_of_sum_of_exp_x( x )
#'
#' @export

log_of_sum_of_exp_x <- function(x, na.rm = FALSE) {

  # Remove NA values
  if ( na.rm ) {

    x <- x[ !is.na(x) ]

    # Close 'Remove NA values'
  } else {

    # If any NA values
    if ( any( is.na(x) ) ) {

      return( NA )

      # Close 'If any NA values'
    }

    # Close else for 'Remove NA values'
  }

  # Number of elements
  n <- length( x )

  # Largest log-value
  max_x <- max( x )
  # Number of duplicate max log-values
  matches_max <- x == max_x
  n_max <- sum( matches_max )

  # Remove max log-value
  x_no_max <- x[ !matches_max ]

  # Compute cumulative sum of exp( x[i] - max(x) )
  exp_x_minus_max <- exp( x_no_max - max_x )
  sum_exp <- sum( exp_x_minus_max )

  # Adjust if duplicate cases of max log-value
  if ( n_max > 1 ) {
    sum_exp <- sum_exp + 1*(n_max - 1)
  }

  # Compute log of sum of exp(x)
  out <- max_x + log1p( sum_exp )

  return( out )
}

