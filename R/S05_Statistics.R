# Statistics
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-04-24

# Table of contents
# 1) sem
# 2) statistic
# 3) boxcox_transform

# TO DO
# - Add unit tests for 'sem', 'statistic', 'boxcox_transform'

###
### 1)
###

#' Standard Error of the Mean
#'
#' This function calculates the standard error
#' of the mean for a set of numeric values.
#'
#' @param x A numeric vector.
#' @param na.rm Logical; if \code{TRUE}, removes \code{NA}
#'   values first.
#'
#' @details Given the standard deviation \eqn{s_x} and sample size
#' \eqn{n} of a sample \eqn{x}, the standard error of the mean is:
#'
#' \deqn{ \frac{s_x}{\sqrt{n}}. }
#'
#' @return The standard error of the mean.
#'
#' @examples
#' # Simulate 100 values from a normal distribution
#' set.seed(2)
#' x <- rnorm(100)
#' # Standard error of the mean should be ~0.1
#' sem(x)
#' @export

sem <- function(x, na.rm = TRUE) {

  #< Remove missing values
  if (na.rm) {
    x <- x[!is.na(x)]

    # > Close conditional
  }

  return(sd(x) / sqrt(length(x)))
}

###
### 2)
###

#' Compute a Statistic
#'
#' This function robustly computes a statistic
#' over a vector of values. The user can
#' specify what to return if the vector
#' is empty, values to include or exclude,
#' and how to treat missing values. Useful in
#' combination of functions like
#' \code{\link[stats]{aggregate}} or
#' \code{\link[dplyr:summarise]{dplyr's summarise}}.
#'
#' @param x A vector.
#' @param f A function that takes the vector
#'   \code{x} as its first argument.
#' @param include A logical vector matching
#'   length to the vector \code{x}.
#' @param exclude A vector of unique cases
#'   to match and exclude.
#' @param na.rm Logical; if \code{TRUE}
#'   removes \code{NA} values from \code{x}.
#' @param default The default value to
#'   return if \code{x} is empty.
#' @param ... Additional arguments to
#'  pass to the function \code{f}.
#'
#' @return A computed statistic, output from
#' the user-supplied function.
#'
#' @examples
#' # Examples using the 'iris' data set
#' data( "iris" )
#'
#' # Default
#' statistic( iris$Sepal.Length )
#' # User-specified statistic
#' statistic( iris$Sepal.Length, f = mean )
#' # Include subset of cases
#' statistic( iris$Sepal.Length, f = mean,
#'            include = iris$Species == 'setosa' )
#' # Custom function
#' statistic( iris$Species, f = function(x) mean( x %in% 'setosa' ) )
#' # Exclude unique cases
#' statistic( iris$Species, f = function(x) mean( x %in% 'setosa' ),
#'            exclude = 'virginica' )
#' # If empty vector supplied, user-specified default value returned
#' statistic( iris$wrong_name, default = 0 )
#'
#' @export

statistic <- function(x,
                      f = length,
                      include = NULL,
                      exclude = NULL,
                      na.rm = T,
                      default = NA,
                      ...) {

  # Initialize output
  out <- default

  #< If there is any data
  if (length(x) > 0) {

    #<| If no logical vector is provided
    if (is.null(include)) {
      include <- rep(T, length(x))
      #|> Close conditional
    }

    #|< If categories to exclude are included
    if (!is.null(exclude)) {
      include <- include & !(x %in% exclude)
      #|> Close conditional
    }

    #|< If NA values should be removed
    if (na.rm) {
      include <- include & !is.na(x)
      #|> Close conditional
    } else {
      include <- include | is.na(x)
      #|> Close conditional
    }

    #<| If any data remains, apply function
    if (any(include)) {
      out <- f(x[include], ...)
      #|> Close conditional
    }

    # > Close conditional
  }

  return(out)
}

###
### 3)
###

#' Box-Cox Transformations for Linear Model
#'
#' Wrapper function to the \code{\link[MASS]{boxcox}}
#' function; estimates the optimal parameter
#' for the Box-Cox power transformation and
#' then applies the appropriate power transformation
#' to the outcome variable.
#'
#' @param x Either
#'   \itemize{
#'     \item A numeric vector of positive values;
#'     \item A data frame with an outcome variable and
#'     set of predictors for a linear regression.
#'   }
#' @param outcome An optional character string with
#'   the column name for the outcome variable in \code{x}.
#'   Otherwise, the first column in \code{x} is assumed
#'   to be the outcome variable.
#' @param parameter_grid Vector specifying the grid of
#'   parameters to explore for maximum likelihood estimation.
#' @param output Logical; if \code{TRUE}, returns the
#'   outcome variable following the power transformation.
#'
#' @details The Box-Cox power transformation for a vector of
#' positive values \eqn{x} and parameter \eqn{\lambda} is:
#'
#' \deqn{ f(x) = \frac{x^{\lambda} - 1}{\lambda}. }
#'
#' For \eqn{\lambda = 0}, one simply uses the log transform.
#'
#' @return Either the maximum likelihood estimate for
#' the power transformation parameter or a vector
#' for the outcome variable following the transformation.
#'
#' @examples
#' # Simulate 100 values from the normal distribution
#' set.seed( 3 ); z <- rnorm( 100 )
#' # Transform the simulated values
#' x <- (z*.5 + 1)^(1/.5)
#' # Maximum likelihood estimate for
#' # transformation parameter
#' boxcox_transform( x, output = FALSE )
#'
#' # Histogram of x, transform of x, and
#' # original simulated values
#' layout( rbind( 1, 2, 3) )
#' hist( x )
#' hist( boxcox_transform( x ) )
#' hist( z )
#'
#' @export

boxcox_transform <- function( x,
                              outcome = NULL,
                              parameter_grid = seq(-3, 3, .01),
                              output = TRUE ) {

  #< If data frame with predictors is provided
  if (is.data.frame(x)) {

    # Prep output and formula for 'lm' call
    if (is.null(outcome)) {
      frm <- paste0(
        colnames(x)[1], " ~ ."
      )
      transformed_x <- x[[colnames(x)[1]]]
    } else {
      frm <- paste0(outcome, "~ .")
      transformed_x <- x[[outcome]]
    }

    # Maximum likelihood estimation via grid search
    # of the Box-Cox power transformation parameter
    xy <- MASS::boxcox(as.formula(frm),
      data = x,
      plotit = FALSE,
      lambda = parameter_grid
    )

    # Extract maximum likelihood estimate
    param <- xy$x[which.max(xy$y)]

    # > Close conditional
  } else {

    # Convert to data frame to avoid scoping issues
    dtf <- data.frame(
      outcome = x
    )
    transformed_x <- x

    # Maximum likelihood estimation via grid search
    # of the Box-Cox power transformation parameter
    xy <- MASS::boxcox(outcome ~ 1,
      data = dtf,
      plotit = FALSE,
      lambda = parameter_grid
    )

    # Extract maximum likelihood estimate
    param <- xy$x[which.max(xy$y)]

    # > Close conditional
  }


  #< ...
  if (output) {

    #<|
    if (param != 0) {

      # Power transformation
      transformed_x <- (transformed_x^param - 1) / param

      #|> Close conditional
    } else {

      # Log-transform
      transformed_x <- log(transformed_x)

      #|> Close conditional
    }

    attributes(transformed_x) <- list(
      boxcox_parameter = param
    )

    out <- transformed_x

    # > Close conditional
  } else {

    # Return maximum likelihood estimate
    out <- param

    # > Close conditional
  }

  return(out)
}
