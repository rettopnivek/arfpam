# Math
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-05-20

# Table of contents
# 1) Log-odds and the logistic function
#   1.1) logit
#   1.2) logistic
# 2) softmax
# 3) erf
# 4) pow

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
