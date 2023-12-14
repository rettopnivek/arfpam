# Statistics
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2023-12-14

# Table of contents
# 1) sem
# 2) statistic
# 3) boxcox_transform
# 4) pvalues
# 5) bootstrap
# 6) summa
# 7) bounds
# 8) standardize
# 9) density_points
# 10) yeo_johnson
# 11) yeo_johnson_transform

# TO DO
# - Add unit tests for 'sem', 'statistic', 'boxcox_transform',
#   'pvalues', 'bootstrap', and 'summa'

#### 1) sem ####
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

sem <- function( x,
                 na.rm = TRUE) {

  # Remove missing values
  if (na.rm) {
    x <- x[!is.na(x)]

    # Close 'Remove missing values'
  }

  return(sd(x) / sqrt(length(x)))
}

#### 2) statistic ####
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
#' data("iris")
#'
#' # Default
#' statistic(iris$Sepal.Length)
#' # User-specified statistic
#' statistic(iris$Sepal.Length, f = mean)
#' # Include subset of cases
#' statistic(iris$Sepal.Length,
#'   f = mean,
#'   include = iris$Species == "setosa"
#' )
#' # Custom function
#' statistic(iris$Species, f = function(x) mean(x %in% "setosa"))
#' # Exclude unique cases
#' statistic(iris$Species,
#'   f = function(x) mean(x %in% "setosa"),
#'   exclude = "virginica"
#' )
#' # If empty vector supplied, user-specified default value returned
#' statistic(iris$wrong_name, default = 0)
#' @export

statistic <- function( x,
                       f = length,
                       include = NULL,
                       exclude = NULL,
                       na.rm = T,
                       default = NA,
                       ... ) {

  # Initialize output
  out <- default

  # If there is any data
  if (length(x) > 0) {

    # If no logical vector is provided
    if (is.null(include)) {

      include <- rep(T, length(x))

      # Close 'If no logical vector is provided'
    }

    # If categories to exclude are included
    if (!is.null(exclude)) {

      include <- include & !(x %in% exclude)

      # Close 'If categories to exclude are included'
    }

    # If NA values should be removed
    if (na.rm) {

      include <- include & !is.na(x)

      # Close 'If NA values should be removed'
    } else {

      include <- include | is.na(x)

      # Close 'If NA values should be removed'
    }

    # If any data remains apply function
    if (any(include)) {

      out <- f(x[include], ...)

      # Close 'If any data remains apply function'
    }

    # Close 'If there is any data'
  }

  return(out)
}

#### 3) boxcox_transform ####
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
#' set.seed(3)
#' z <- rnorm(100)
#' # Transform the simulated values
#' x <- (z * .5 + 1)^(1 / .5)
#' # Maximum likelihood estimate for
#' # transformation parameter
#' boxcox_transform(x, output = FALSE)
#'
#' # Histogram of x, transform of x, and
#' # original simulated values
#' layout(rbind(1, 2, 3))
#' hist(x)
#' hist(boxcox_transform(x))
#' hist(z)
#' @export

boxcox_transform <- function( x,
                              outcome = NULL,
                              parameter_grid = seq(-3, 3, .01),
                              output = TRUE ) {

  # If data frame with predictors is provided
  if (is.data.frame(x)) {

    # Prep output and formula for lm call
    if (is.null(outcome)) {

      frm <- paste0(
        colnames(x)[1], " ~ ."
      )
      transformed_x <- x[[colnames(x)[1]]]

      # Close 'Prep output and formula for lm call'
    } else {

      frm <- paste0(outcome, "~ .")
      transformed_x <- x[[outcome]]

      # Close else for 'Prep output and formula for lm call'
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

    # Close 'If data frame with predictors is provided'
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

    # Close else for 'If data frame with predictors is provided'
  }


  # If returning transformed values
  if (output) {

    # If power transformation
    if (param != 0) {

      # Power transformation
      transformed_x <- (transformed_x^param - 1) / param

      # Close 'If power transformation'
    } else {

      # Log-transform
      transformed_x <- log(transformed_x)

      # Close else for 'If power transformation'
    }

    # Add maximum likelihood estimate as attribute
    attributes(transformed_x) <- list(
      boxcox_parameter = param
    )

    out <- transformed_x

    # Close 'If returning transformed values'
  } else {

    # Return maximum likelihood estimate
    out <- param

    # Close else for 'If returning transformed values'
  }

  return(out)
}

#### 4) pvalues ####
#' Compute and Format P-values
#'
#' Given a set of Monte Carlo
#' samples, estimates a p-value from
#' the proportion of values that fall
#' above or below a comparison point.
#' If \code{string} is \code{TRUE},
#' takes a numeric p-value and converts
#' it into a formatted character
#' string, either 'p = ...' or
#' 'p < ...'.
#'
#' @param x Either a) a vector of numeric
#'   values (Monte Carlo samples) or b)
#'   a single p-value.
#' @param comparison The comparison value;
#'   the p-value is computed from the
#'   proportion of Monte Carlo samples
#'   above or below this cut-off.
#' @param alternative A character string
#'   indicating the type of alternative
#'   hypothesis to test, either a) \code{'two-sided'},
#'   b) \code{'less'}, or c) \code{'greater'}.
#'   If \code{'two-side'}, uses the alternative
#'   hypothesis that produces the small p-value,
#'   and then multiplies by two to adjust for
#'   the two-sided comparison.
#' @param digits Number of digits to round to
#'   when formatting the p-value.
#' @param string Logical; if \code{TRUE},
#'   returns output as a nicely formatted
#'   character string. Automatically set
#'   to \code{TRUE} if length of \code{x}
#'   equals 1.
#' @param pad Logical; if \code{TRUE},
#'   pads number of values to right
#'   of decimal to always equal \code{digits}.
#'
#' @return Either a numeric p-value or a character
#'   string, a nicely formatted version of the
#'   p-value.
#'
#' @examples
#' # Example based on two-sample t-test
#' set.seed(40)
#' x <- data.frame(
#'   y = c(rnorm(50), rnorm(50, mean = .3)),
#'   group = rep(1:2, each = 50)
#' )
#'
#' # Two-sample t-test
#' tt <- t.test(y ~ group, data = x)
#' print(pvalues(tt$p.value))
#' print(pvalues(tt$p.value, digits = 2))
#'
#' # For very small p-values, automatically
#' # converts to 'p < cut-off' format
#' print(pvalues(1e-6))
#'
#' # Computing p-values from
#' # Monte Carlo samples
#'
#' # Simulate data from standard normal;
#' # on average 50% of sample falls
#' # below zero
#' set.seed(50)
#' x <- rnorm(1000)
#'
#' # Default is two-sided
#' pvalues(x)
#' # Can specify less than or greater than
#' pvalues(x, alternative = "less")
#' pvalues(x, alternative = "greater")
#' # Against different comparison point
#' pvalues(x, alternative = "less", comparison = .68)
#'
#' # Simulate data from normal distribution
#' # with mean of 0.68, on average
#' # approximately 75% of sample falls
#' # below zero
#' set.seed(60)
#' x <- rnorm(1000, mean = .68)
#' pvalues(x)
#' pvalues(x, alternative = "less")
#' pvalues(x, alternative = "greater")
#' @export

pvalues <- function( x,
                     comparison = 0,
                     alternative = "two-sided",
                     digits = 3,
                     string = FALSE,
                     pad = FALSE ) {

  # Initialize output
  out <- NULL

  # Assume if size of x equals 1 that
  # it is a numeric p-value

  # Check length
  if (length(x) == 1) {

    # Check if valid p-value
    if (x >= 0 &
      x <= 1) {

      # Force argument 'string' to be TRUE
      string <- TRUE

      # Close 'Check if valid p-value'
    } else {

      stop("p-values must be between 0 and 1",
        call. = FALSE
      )

      # Close else for 'Check if valid p-value'
    }

    # Close 'Check length'
  }

  # Estimate p-value from Monte Carlo samples
  if (!string) {

    check <- FALSE

    # Two-sided test
    if (alternative == "two-sided") {

      check <- TRUE

      # Lower tail
      if (median(x) > comparison) {

        out <- mean(x < comparison)

        # Close 'Lower tail'
      } else {

        out <- mean(x > comparison)

        # Close 'Lower tail'
      }

      out <- out * 2

      # Close 'Two-sided test'
    }

    # Test if greater than comparison
    if (alternative == "greater") {

      check <- TRUE

      out <- mean(x < comparison)

      # Close 'Test if greater than comparison'
    }

    # Test if less than comparison
    if (alternative == "less") {

      check <- TRUE

      out <- mean(x > comparison)

      # Close 'Test if less than comparison'
    }

    # If alternative argument misspecified
    if (!check) {

      err_msg <- paste(
        "Please specify 'alternative' as",
        "either 'two-sided', 'greater', or",
        "'less'."
      )

      stop(err_msg)

      # Close 'If alternative argument misspecified'
    }

    # Close 'Estimate p-value from Monte Carlo samples'
  }

  # Convert to formatted string
  if (string) {

    # Pad end
    if (pad) {

      p <- format(round(x, digits = digits), nsmall = digits)

      # Close 'Pad end'
    } else {

      p <- format(round(x, digits = digits))

      # Close else for 'Pad end'
    }

    # If rounds to zero
    if ( round(x, digits) == 0 ) {

      # Specify as less than next smallest value
      nd <- digits - 1
      nd <- paste(
        "0.",
        paste(rep(0, nd), collapse = ""),
        "1",
        sep = ""
      )
      out <- paste("p < ", nd, sep = "")

      # Close 'If rounds to zero'
    } else {

      out <- paste0("p = ", p)

      # Close else for 'If rounds to zero'
    }

    # Close 'Convert to formatted string'
  }

  return(out)
}

#### 5) bootstrap ####
#' Non-Parametric Bootstrap
#'
#' Computes a test statistic over multiple
#' replicates from a set of data. Replicates
#' are created drawing a sample of the same
#' size and with replacement from the original
#' set of data.
#'
#' @param x A vector of values.
#' @param t_x A function to compute a test statistic;
#'   the first argument must be for \code{x}.
#' @param N The number of samples with replacement
#'   to draw from \code{X}.
#' @param summary An optional function to apply
#'   to the test statistics after resampling.
#' @param ... Additional parameters for the \code{t_x}
#'   function.
#'
#' @return A list consisting of...
#' \itemize{
#'   \item observed = the value of the test statistic
#'     for the original data set.
#'   \item replicates = the values of the test statistic
#'     for each of the replicates produced via resampling.
#'   \item summary = the output of summary function
#'     applied over the replicates; \code{NULL} if
#'     no summary function was specified.
#' }
#'
#' @examples
#' # Simulate from normal distribution
#' set.seed(200)
#' x <- rnorm(50, mean = 100, sd = 15)
#'
#' # Use bootstrap method to estimate
#' # sampling distribution for the mean
#' btstrp <- bootstrap(x)
#'
#' hist(btstrp$replicates,
#'   main = "",
#'   xlab = "Sampling distribution - mean"
#' )
#' # True mean
#' abline(v = 100, lwd = 1)
#'
#' # Estimate of standard error
#' print(round(sem(x), 1))
#'
#' # Estimate of standard error
#' # computed from bootstrapped samples
#' btstrp <- bootstrap(x, summary = sd)
#' print(round(btstrp$summary, 1))
#'
#' # 95% confidence interval around the mean
#' # using bootstrapped samples
#' f <- function(y) {
#'   paste(round(quantile(y, c(.025, .975)), 1),
#'     collapse = " to "
#'   )
#' }
#' btstrp <- bootstrap(x, summary = f)
#' print(btstrp$summary)
#'
#' # Use bootstrap method to estimate
#' # sampling distribution for the median
#' # (which has no close-formed solution)
#' btstrp <- bootstrap(x, t_x = median)
#' hist(btstrp$replicates,
#'   main = "",
#'   xlab = "Sampling distribution - median"
#' )
#' @export

bootstrap <- function(x, t_x = mean, N = 1000,
                      summary = NULL, ...) {
  n <- length(x)

  indices <- lapply(1:N, function(i) {
    sample(1:n, size = n, replace = T)
  })

  monte_carlo_samples <- sapply(
    indices, function(index) {
      t_x(x[index], ...)
    }
  )

  out <- list(
    observed = t_x(x, ...),
    replicates = monte_carlo_samples
  )

  if (!is.null(summary)) {
    out$summary <-
      summary(out$replicates)
  }

  return(out)
}

#### 6) summa ####
#' Flexible Formatted Summary Statistics
#'
#' A function that allows users to create
#' a nicely formatted character string
#' with summary statistics based on
#' user-supplied identifiers via a
#' simple, intuitive syntax.
#'
#' @param x A vector of values.
#' @param syntax A character string with
#'   identifiers in the form \code{[[.]]}
#'   (where \code{.} can be a variety of
#'   letter sets for different summary statistics) -
#'   the function then substitutes the appropriate
#'   computed value for the corresponding
#'   identifier (see details for more information).
#' @param categories An optional vector of
#'   elements to match in \code{x} when
#'   computing frequencies, proportions,
#'   or percentages.
#' @param digits Number of digits to round
#'   summary statistics.
#' @param na.rm Logical; if \code{TRUE}
#'   removes \code{NA} values from \code{x}.
#' @param pad Logical; if \code{TRUE} pads
#'   values with 0 to all have a matching
#'   number of decimal places.
#' @param f An optional user-defined function
#'   that takes \code{x} as a first argument
#'   and returns a vector of values.
#'   The i-th outputted value will then
#'   be substituted for the corresponding
#'   identifier \code{[[i]]} (see examples).
#' @param ... Additional arguments for the
#'   user-defined function \code{f}.
#'
#' @details This function provides some simple syntax
#' to allow users to write out a custom phrase for
#' reporting summary statistics.
#' The function then searches the input for
#' identifiers - once found, the function computes
#' the appropriate summary statistic and
#' substitutes the numeric result in place of
#' the given identifier.
#'
#' For example, a user can provide the phrase:
#'
#' \code{'Mean = [[M]]'},
#'
#' and the function will then substitute the sample
#' mean of the vector \code{x} for the identifier
#' \code{[[M]]}.
#'
#' Pre-defined identifiers are:
#' \itemize{
#'   \item \code{[[N]]} = Sample size;
#'   \item \code{[[M]]} = Mean;
#'   \item \code{[[SD]]} = Standard deviation;
#'   \item \code{[[SE]]} = Standard error of the mean;
#'   \item \code{[[Mn]]} = Minimum;
#'   \item \code{[[Q1]]} = 1st quartile;
#'   \item \code{[[Md]]} = Median;
#'   \item \code{[[Q3]]} = 2nd quartile;
#'   \item \code{[[Mx]]} = Maximum;
#'   \item \code{[[IQR]]} = Inter-quartile range;
#'   \item \code{[[C]]} = Counts/frequencies;
#'   \item \code{[[P]]} = Percent;
#'   \item \code{[[Pr]]} = Proportion.
#' }
#'
#' Users can also pass in a custom function \code{f}
#' that takes \code{x} as a first argument and
#' returns a vector of values. Then element \code{i}
#' from the outputted vector is substituted for
#' the identifier \code{[[i]]}.
#'
#' @return A character string.
#'
#' @examples
#' # Example using 'iris' data set
#' data("iris")
#' # Continuous variable - sepal length
#' x <- iris$Sepal.Length
#'
#' # Mean and standard deviation
#' summa(x)
#' # Median and IQR
#' summa(x, "[[M]] ([[IQR]])")
#' # Pad to 2 decimal places
#' summa(x, "[[M]] ([[IQR]])", pad = TRUE)
#' # Mean (SD); N [min and max]
#' summa(x, "[[N]]; [[M]] ([[SD]]); " %p%
#'   "[[[Mn]], [[Q1]], [[Md]], [[Q3]], [[Mx]]]",
#' digits = 1
#' )
#'
#' # Custom measures via user-defined function
#' # (e.g., bootstrapped confidence interval)
#' fnc <- function(x) {
#'   btstrp <- bootstrap(
#'     x,
#'     summary = function(y) quantile(y, c(.025, .975))
#'   )
#'   return(btstrp$summary)
#' }
#' summa(x, "[[M]] ([[SE]]) [[[1]] to [[2]]]",
#'   f = fnc
#' )
#'
#' # Example using 'mtcars' data set
#' # Categorical variable - # of forward gears
#' data("mtcars")
#' x <- mtcars$gear
#'
#' # Percent and counts for 3 forward gears
#' summa(x == 3, "[[P]]% ([[C]] out of [[N]])")
#' # Percent and counts for 4 or 5 forward gears
#' summa(x, "[[P]]% ([[C]] out of [[N]])",
#'   categories = c(4, 5)
#' )
#' @export

summa <- function(x, syntax = "[[M]] ([[SD]])",
                  categories = NULL,
                  digits = NULL, na.rm = TRUE,
                  pad = FALSE,
                  f = NULL,
                  ... ) {

  # Initialize output
  out <- syntax

  # Sample size
  if (grepl("[[N]]", syntax, fixed = TRUE)) {
    n <- length(x)
    if (na.rm) length(x[!is.na(x)])

    out <- gsub("[[N]]", n, out, fixed = TRUE)

    # Close 'Sample size'
  }

  # Mean
  if (grepl("[[M]]", syntax, fixed = TRUE)) {

    # Default number of digits to round to
    if (is.null(digits)) {
      dgt <- 2
    } else {
      dgt <- digits
    }

    m <- round(mean(x, na.rm = na.rm), dgt)
    if (pad) {
      m <- format(m, nsmall = dgt)
    }

    out <- gsub("[[M]]", m, out, fixed = TRUE)

    # Close 'Mean'
  }

  # Standard deviation
  if (grepl("[[SD]]", syntax, fixed = TRUE)) {

    # Default number of digits to round to
    if (is.null(digits)) {
      dgt <- 2
    } else {
      dgt <- digits
    }

    s <- round(sd(x, na.rm = na.rm), dgt)
    if (pad) {
      s <- format(s, nsmall = dgt)
    }

    out <- gsub("[[SD]]", s, out, fixed = TRUE)

    # Close 'Standard deviation'
  }


  # Standard error
  if (grepl("[[SE]]", syntax, fixed = TRUE)) {

    # Default number of digits to round to
    if (is.null(digits)) {
      dgt <- 2
    } else {
      dgt <- digits
    }

    n <- length(x)
    if (na.rm) n <- length(x[!is.na(x)])
    se <- round(sd(x, na.rm = na.rm) / sqrt(n), dgt)
    if (pad) {
      se <- format(se, nsmall = dgt)
    }

    out <- gsub("[[SE]]", se, out, fixed = TRUE)

    # Close 'Standard error'
  }

  # Minimum
  if (grepl("[[Mn]]", syntax, fixed = TRUE)) {

    # Default number of digits to round to
    if (is.null(digits)) {
      dgt <- 2
    } else {
      dgt <- digits
    }

    mn <- round(min(x, na.rm = na.rm), dgt)
    if (pad) {
      mn <- format(mn, nsmall = dgt)
    }

    out <- gsub("[[Mn]]", mn, out, fixed = TRUE)

    # Close 'Minimum'
  }


  # 1st quartile
  if ( grepl("[[Q1]]", syntax, fixed = TRUE) ) {

    # Default number of digits to round to
    if (is.null(digits)) {
      dgt <- 2
    } else {
      dgt <- digits
    }

    q1 <- round(quantile(x, prob = .25, na.rm = na.rm), dgt)
    if (pad) {
      q1 <- format(q1, nsmall = dgt)
    }

    out <- gsub("[[Q1]]", q1, out, fixed = TRUE)

    # Close '1st quartile'
  }

  # Median
  if (grepl("[[Md]]", syntax, fixed = TRUE)) {

    # Default number of digits to round to
    if (is.null(digits)) {
      dgt <- 2
    } else {
      dgt <- digits
    }

    md <- round(median(x, na.rm = na.rm), dgt)
    if (pad) {
      md <- format(md, nsmall = dgt)
    }

    out <- gsub("[[Md]]", md, out, fixed = TRUE)

    # Close 'Median'
  }

  # 3rd quartile
  if (grepl("[[Q3]]", syntax, fixed = TRUE)) {

    # Default number of digits to round to
    if (is.null(digits)) {
      dgt <- 2
    } else {
      dgt <- digits
    }

    q3 <- round(quantile(x, prob = .75, na.rm = na.rm), dgt)
    if (pad) {
      q3 <- format(q3, nsmall = dgt)
    }

    out <- gsub("[[Q3]]", q3, out, fixed = TRUE)

    # Close '3rd quartile'
  }

  # Maximum
  if (grepl("[[Mx]]", syntax, fixed = TRUE)) {

    # Default number of digits to round to
    if (is.null(digits)) {
      dgt <- 2
    } else {
      dgt <- digits
    }

    mx <- round(max(x, na.rm = na.rm), dgt)
    if (pad) {
      mx <- format(mx, nsmall = dgt)
    }

    out <- gsub("[[Mx]]", mx, out, fixed = TRUE)

    # Close 'Maximum'
  }

  # Inter-quartile range
  if (grepl("[[IQR]]", syntax, fixed = TRUE)) {

    # Default number of digits to round to
    if (is.null(digits)) {
      dgt <- 2
    } else {
      dgt <- digits
    }

    q <- quantile(x, prob = c(.25, .75), na.rm = na.rm)
    iqr <- round(diff(q), dgt)

    if (pad) {
      iqr <- format(iqr, nsmall = dgt)
    }

    out <- gsub("[[IQR]]", iqr, out, fixed = TRUE)

    # Close 'Inter-quartile range'
  }

  #<Counts/frequencies
  if (grepl("[[C]]", syntax, fixed = TRUE)) {
    if (is.null(categories)) {
      categories <- TRUE
    }

    cnt <- sum(x %in% categories, na.rm = na.rm)

    out <- gsub("[[C]]", cnt, out, fixed = TRUE)

    # Close 'Counts/frequencies'
  }

  # Percent
  if (grepl("[[P]]", syntax, fixed = TRUE)) {

    # Default number of digits to round to
    if (is.null(digits)) {
      dgt <- 1
    } else {
      dgt <- digits
    }

    if (is.null(categories)) {
      categories <- TRUE
    }

    per <- 100 * mean(x %in% categories, na.rm = na.rm)
    per <- round(per, dgt)
    if (pad) {
      per <- format(per, nsmall = dgt)
    }

    out <- gsub("[[P]]", per, out, fixed = TRUE)

    # Close 'Percent'
  }

  # Proportion
  if (grepl("[[Pr]]", syntax, fixed = TRUE)) {

    # Default number of digits to round to
    if (is.null(digits)) {
      dgt <- 2
    } else {
      dgt <- digits
    }

    if (is.null(categories)) {
      categories <- TRUE
    }

    prp <- round(mean(x %in% categories, na.rm = na.rm), dgt)
    if (pad) {
      prp <- format(prp, nsmall = dgt)
    }

    out <- gsub("[[Pr]]", prp, out, fixed = TRUE)

    # Close 'Proportion'
  }

  # Custom function
  if (!is.null(f)) {
    val <- f(x, ...)

    for (i in 1:length(val)) {
      if (is.null(digits)) {
        dgt <- 2
      } else {
        dgt <- digits
      }

      val[i] <- round(val[i], dgt)
      if (pad) {
        val[i] <- format(val[i], nsmall = dgt)
      }

      out <- gsub(paste0("[[", i, "]]"),
        val[i], out,
        fixed = TRUE
      )
    }

    # Close conditional 'Custom function'
  }

  return(out)
}

#### 7) bounds ####
#' Lower and Upper Limits for Confidence or Credible Intervals
#'
#' Given a desired width, computes the lower and upper limit
#' for a confidence or credible interval. Can be combined
#' with functions like \code{\link[stat]{quantile}} or
#' the quantile functions for assorted probability
#' distributions( e.g., \code{\link[stats:Normal]{qnorm}},
#' \code{\link[stats:Beta]{qbeta}}, etc.).
#'
#' @param width The width of the interval, a numeric value
#'   between 0 and 1.
#'
#' @return A vector with the lower and upper limits.
#'
#' @examples
#' # Lower and upper limits of 95% interval
#' bounds( .95 )
#' # Example data
#' x <- rnorm( 100, mean = 100, sd = 15 )
#' # 95% confidence interval around mean
#' mean(x) + qnorm( bounds( .95 ) ) * sem( x )
#' # Predicted
#' qnorm( bounds( .95 ), mean = 100, sd = 15/sqrt(100) )
#' # The 1st and 3rd quartiles
#' quantile( x, bounds( .5 ) )
#'
#' @export

bounds <- function( width ) {

  return( .5 + c( -.5, .5 )*width )

}

#### 8) standardize ####
#' Standardize Columns in a Data Frame or Matrix
#'
#' Function to standardize (mean-center and scale
#' by standard deviation resulting in a mean of 0
#' and standard deviation of 1) columns in a
#' matrix or data frame.
#'
#' @param x A data frame or matrix of numeric values.
#' @param y A data frame or matrix of numeric values
#'   (must have the same column names in same order
#'   as \code{x}).
#' @param mean_sd A list of two numeric vectors equal in
#'   length to the number of columns with the means and
#'   standard deviations, respectively, to use for scaling.
#' @param raw Logical; if \code{TRUE}, uses the means
#'   and standard deviations given in \code{mean_sd}
#'   to return the original raw values of \code{x}.
#' @param as_list Logical; if \code{TRUE} returns
#'   a named list with the scaled values of \code{x}
#'   (and \code{y} if provided) along with the means and
#'   standard deviations used for scaling. Automatically
#'   set to \code{TRUE} when \code{y} is provided.
#' @param labels A character vector with the labels for
#'   the \code{x} and \code{y} data sets if returning a
#'   list.
#'
#' @returns Either a scaled data frame or matrix or a list
#' with the scaled values and the means and standard deviations
#' used for scaling.
#'
#' @examples
#' # Create data frame
#' x_raw <- round( matrix( rnorm( 9, 100, 15 ), 3, 3 ) )
#' colnames(x_raw) <- paste0( 'X', 1:3 )
#' print(x_raw)
#'
#' # Standardize columns
#' x <- standardize( x_raw )
#' print(x)
#'
#' # Create second data frame with same
#' # variables but new values
#' y_raw <- round( matrix( rnorm( 9, 50, 15 ), 3, 3 ) )
#' colnames(y_raw) <- paste0( 'X', 1:3 )
#' print(y_raw)
#'
#' # Scale columns of y_raw based on means and
#' # standard deviations from x_raw
#' lst <- standardize( x_raw, y_raw, labels = c('x', 'y') )
#' y <- lst$Data$y
#' print( y )
#'
#' # Undo scaling
#' standardize( y, mean_sd = lst$Scaling, raw = TRUE )
#'
#' @export

standardize <- function( x,
                         y = NULL,
                         mean_sd = NULL,
                         raw = FALSE,
                         as_list = FALSE,
                         labels = c( 'X', 'Y' ) ) {

  # If scaling not provided
  if ( is.null( mean_sd ) ) {

    M <- apply(
      x, 2, mean, na.rm = T
    )
    SD <- apply(
      x, 2, sd, na.rm = T
    )

    # Close 'If scaling not provided'
  } else {

    M <- mean_sd[[1]]
    SD <- mean_sd[[2]]

    # Close else for 'If scaling not provided'
  }

  # If scaling should be undone
  if ( raw ) {

    # If scaling provided
    if ( !is.null( mean_sd ) ) {

      x.raw <- x

      # Loop over columns
      for ( k in 1:ncol(x) ) {

        x.raw[, k] <- x[, k]*SD[k] + M[k]

        # Close 'Loop over columns'
      }

      # Close 'If scaling provided'
    } else {

      stop(
        paste0(
          "Must provide list with means and standard deviations ",
          "in order to compute original raw values"
        )
      )

      # Close else for 'If scaling provided'
    }

    return( x.raw )

    # Close 'If scaling should be undone'
  }

  # Initialize scaled data sets
  z.x <- x
  z.y <- NULL

  # If second data set provided
  if ( !is.null(y) ) {

    # Check if same number of columns
    if ( ncol(y) != ncol(x) ) {

      stop( "Data set 'y' must have same number of columns as 'x'" )

      # Close 'Check if same number of columns'
    }

    # Check if same column names
    if ( any( colnames(y) != colnames(x) ) ) {

      stop( "Data set 'y' should have same column names as 'x'" )

      # Close 'Check if same column names'
    }

    z.y <- y

    # Close 'If second data set provided'
  }

  # Loop over columns
  for ( k in 1:ncol(x) ) {

    z.x[, k] <- ( x[, k] - M[k] )/SD[k]

    # If second data set provided
    if ( !is.null(y) ) {

      z.y[, k] <- ( y[, k] - M[k] )/SD[k]

      # Close 'If second data set provided'
    }

    # Close 'Loop over columns'
  }

  # Return list given two data sets
  if ( !is.null(y) ) as_list <- TRUE

  # Return list of outputs
  if ( as_list ) {

    lst_output <- list(
      Data = list(
        X = z.x,
        Y = z.y
      ),
      Scaling = list(
        Mean = M,
        SD = SD
      )
    )

    names( lst_output$Data ) <- labels

    return( lst_output )

    # Close 'Return list of outputs'
  }

  return( z.x )
}

#### 9) density_points ####
#' Estimate Densities for Individual Observations
#'
#' Given a vector of values, computes the empirical
#' density for each observation.
#'
#' @param x A numeric vector.
#' @param ... Additional arguments to be passed
#'   to the density function.
#'
#' @return A list with...
#' \itemize{
#'   \item x = the sorted values for the original input;
#'   \item y = the associated empirical densities.
#' }
#'
#' @examples
#' plot(c(-4,4),c(0,.5),type='n',ylab='Density',xlab='z-scores')
#' x = rnorm( 100 )
#' dp = density_points( x )
#' points( dp$x, dp$y, pch = 19 )
#'
#' @export

density_points <- function (x, ...) {

  ed = density(x, ...)
  af = approxfun(ed)
  y = af(sort(x))

  return( list(x = sort(x), y = y) )
}

#### 10) yeo_johnson ####
#' Yeo-Johnson Transform
#'
#' Transforms a numeric vector of values using
#' the Yeo-Johnson (2000) power transformation
#' family.
#'
#' @param y A numeric vector (values can be
#'   positive, negative, or zero).
#' @param lambda A numeric value specifying
#'   the transformation to apply.
#'
#' @references
#' Yeo, I. K., & Johnson, R. A. (2000). A new family of power
#' transformations to improve normality or symmetry. Biometrika,
#' 87 (4), 954-959. https://doi.org/10.1093/biomet/87.4.954
#'
#' @return A numeric vector.
#'
#' @examples
#' # Example from page 958 of Yeo & Johnson (2000)
#' y <- c(
#'   6.1, -8.4, 1.0, 2.0, 0.7, 2.9, 3.5,
#'   5.1, 1.8, 3.6, 7.0, 3.0, 9.3, 7.5, -6.0
#' )
#' shapiro.test( y ) # Test of normality
#' y_transformed <- yeo_johnson(y, 1.305)
#' shapiro.test( y_transformed ) # Test of normality shows improvement
#'
#' @export

yeo_johnson <- function(y, lambda) {

  if ( !is.numeric(y) ) {
    stop(
      "Argument 'y' must be a numeric vector"
    )
  }

  if ( !is.numeric(lambda) | length( lambda ) != 1 ) {
    stop(
      "Argument 'lambda' must be a numeric value"
    )
  }

  y_transformed <- y

  # Remove missing values
  lgc_no_NA <- !is.na(y)

  # Identify negative values
  lgc_negative <- lgc_no_NA & y < 0

  # Apply transformation
  if ( lambda != 1 ) {

    lgc_subset <-
      lgc_no_NA & !lgc_negative

    if ( lambda != 0 ) {

      y_transformed[lgc_subset] <-
        ( (y[lgc_subset] + 1)^lambda - 1 )/lambda

    }

    if ( lambda == 0 ) {

      y_transformed[no_NA] <-
        log( y[no_NA] + 1)

    }

    lgc_subset <-
      lgc_no_NA & lgc_negative

    if ( lambda != 2 ) {

      y_transformed[lgc_subset] <-
        -( (-y[lgc_subset] + 1)^(2 - lambda) - 1 )/(2 - lambda)

    }

    if ( lambda == 2 ) {

      y_transformed[lgc_subset] <-
        log( -y[lgc_subset] + 1)

    }

    # Close 'Apply transformation'
  }

  return(y_transformed)
}

#### 11) yeo_johnson_transform ####
#' Determine Best Parameter for Yeo-Johnson Transform
#'
#' Estimates and applies best-fitting Yeo-Johnson
#' transformation parameter via maximum likelihood
#' for a vector of numeric values.
#'
#' @param x A numeric vector (values can be
#'   positive, negative, or zero).
#' @param lower The smallest value for the transformation
#'   parameter to consider.
#' @param upper The highest value for the transformation
#'   parameter to consider.
#'
#' @details
#' The transformation parameter to use is estimated via
#' maximum likelihood using the [base:optimize] and
#' [stats:dnorm] functions.
#'
#' @references
#' Yeo, I. K., & Johnson, R. A. (2000). A new family of power
#' transformations to improve normality or symmetry. Biometrika,
#' 87 (4), 954-959. https://doi.org/10.1093/biomet/87.4.954
#'
#' @return A numeric vector, the transformed values of x.
#'
#' @examples
#' # Example from page 958 of Yeo & Johnson (2000)
#' x <- c(
#'   6.1, -8.4, 1.0, 2.0, 0.7, 2.9, 3.5,
#'   5.1, 1.8, 3.6, 7.0, 3.0, 9.3, 7.5, -6.0
#' )
#' shapiro.test( x ) # Test of normality
#' x_transformed <- yeo_johnson_transform(x)
#' # Extract results of maximum likelihood estimation
#' attributes(x_transformed)$mle_for_yeo_johnson
#' shapiro.test( x_transformed ) # Test of normality shows improvement
#'
#' @export

yeo_johnson_transform <- function(
    x,
    lower = -100,
    upper = 100 ) {

  y <- x[ !is.na(x) ]

  fun_sum_of_log_likelihood <- function(lambda, y) {

    n <- length(y)
    yt <- yeo_johnson(y, lambda)
    mu <- mean(yt)
    sigma_sq <- var(yt)*( length(yt)-1 )/length(yt)

    sll_part_1 <- -(n/2)*log(2*pi)
    sll_part_2 <- -(n/2)*log(sigma_sq)
    sll_part_3 <- ( -1/(2*sigma_sq) )*sum( (yt - mu)^2 )
    sll_part_4 <- (lambda-1) * sum( sign(y) * log( abs(y) + 1) )

    sum_of_log_likelihood <-
      sll_part_1 + sll_part_2 + sll_part_3 + sll_part_4

    return(sum_of_log_likelihood)
  }

  mle = optimize(
    fun_sum_of_log_likelihood,
    lower = lower, upper = upper,
    maximum = TRUE,
    y = y
  )

  x_transformed <- yeo_johnson(
    x, mle$maximum
  )
  lst_attr <- attributes(x_transformed)
  if ( is.null( lst_attr ) ) {
    lst_attr <- list(
      mle_for_yeo_johnson = mle
    )
  } else {
    lst_attr$mle_for_yeo_johnson <- mle
  }
  attributes( x_transformed ) <- lst_attr

  return( x_transformed )
}

#### 12) principal_components_analysis ####
#' Principal Components Analysis
#'
#' Function for running a principal components
#' analysis (PCA) with training (and test) data.
#' Expands on the [stats::prcomp] function.
#'
#' @param train A data frame or matrix. Assumes all
#'   columns should be included in the PCA.
#' @param test An optional data frame or matrix.
#'   Must have the same number of columns and
#'   same column names as \code{train}.
#'
#' @returns A list with the standardized data, the
#' results of the call to [stats::prcomp], the rotation
#' matrix (eigenvectors), the eigenvalues, the
#' correlations between raw scores and component scores,
#' and the root-mean square error for the training and
#' test sets when using a smaller number of components.
#'
#' @examples
#' # Simulate training and test data
#' train <- MASS::mvrnorm( 100, rep( 0, 8 ), diag(8) )
#' test <- MASS::mvrnorm( 100, rep( 0, 8 ), diag(8) )
#'
#' PCA <- principal_components_analysis(
#'   train = train, test = test
#' )
#'
#' # Loading matrix
#' lambda <- cbind(
#'   c( runif( 4, .3, .9 ), rep( 0, 4 ) ),
#'   c( rep( 0, 4 ), runif( 4, .3, .9 ) )
#' )
#' # Communalities
#' D_tau <- diag( runif( 8, .5, 1.5 ) )
#'
#' cov_mat <- lambda %*% t( lambda ) + D_tau
#' cor_mat <- cov2cor( cov_mat )
#'
#' set.seed( 341 ) # For reproducibility
#' x <- MASS::mvrnorm( n = 200, mu = rep( 0, 8 ), Sigma = cor_mat )
#' colnames(x) <- paste0( 'C', 1:8 )
#'
#' PCA <- principal_components_analysis(
#'   train = x
#' )
#'
#' @export

principal_components_analysis <- function( train,
                                           test = NULL ) {

  is_na <- apply(
    train, 1, function(x) any( is.na(x) )
  )

  # If any missing training data
  if ( any(is_na) ) {

    train <- train[ !is_na, ]

    warning(
      paste0( "Removed ", sum(is_na),
              " rows with missing values for 'train' argument" )
    )

    # Close 'If any missing training data'
  }

  # If test data provided
  if ( !is.null(test) ) {

    is_na <- apply(
      train, 1, function(x) any( is.na(x) )
    )

    # If any missing test data
    if ( any(is_na) ) {

      train <- train[ !is_na, ]

      warning(
        paste0( "Removed ", sum(is_na), " rows with missing values" )
      )

      # Close 'If any missing test data'
    }

    # Close 'If test data provided'
  }

  # Initialize output
  lst_output <- list(
    Data = list(
      Train = NULL,
      Test = NULL
    ),
    Scaling = list(
      Mean = NA,
      SD = NA
    ),
    Fit = NA,
    Rotation = NA,
    Eigenvalues = NA,
    Proportion = NA,
    Scores = list(
      Train = NULL,
      Test = NULL
    ),
    Correlations = list(
      Train = NULL,
      Test = NULL
    ),
    RMSE = list(
      Train = NA,
      Test = NA
    )
  )

  # Standardize inputs
  lst_scaled <- arfpam::standardize(
    x = train,
    y = test,
    as_list = TRUE
  )

  # Save data
  lst_output$Data$Train <- lst_scaled$Data$X
  lst_output$Scaling$Mean <- lst_scaled$Scaling$Mean
  lst_output$Scaling$SD <- lst_scaled$Scaling$SD

  # Run PCA using training data
  lst_PCA <- prcomp(
    lst_scaled$Data$X,
    retx = TRUE,
    center = FALSE, scale. = FALSE
  )
  # Extract eigenvalues and proportion accounted for
  importance <- summary( lst_PCA )$importance

  # Save results
  lst_output$Fit <- lst_PCA
  lst_output$Rotation <- lst_PCA$rotation
  lst_output$Eigenvalues <- importance[1, ]^2
  lst_output$Scores$Train <- lst_PCA$x
  lst_output$Proportion <- importance[2, ]

  # Compute correlations between components and raw variables
  lst_output$Correlations$Train <- sapply(
    1:ncol(train), function(p) {
      sapply(
        1:ncol( train ), function(v) {
          cor( lst_scaled$Data$X[, v], lst_PCA$x[, p] )
        }
      )
    }
  )

  rmse <- rep( NA, ncol(train) )
  iR <- solve( lst_output$Rotation )

  # Loop over components
  for ( p in 1:ncol(train) ) {

    # Reconstruct raw scores from component scores

    # If one component
    if ( p == 1 ) {

      # Ensure matrix algebra works
      train.hat <-
        cbind( lst_output$Scores$Train[, 1] ) %*% rbind( iR[1, ] )

      # Close 'If one component'
    } else {

      train.hat <-
        lst_output$Scores$Train[, 1:p] %*% iR[1:p, ]

      # Close else for 'If one component'
    }

    rmse[p] <-
      sqrt( mean( (train.hat - lst_scaled$Data$X)^2 ) )

    # Close 'Loop over components'
  }

  lst_output$RMSE$Train <- rmse

  # If test data provided
  if ( !is.null(test) ) {

    # Save data
    lst_output$Data$Test <- lst_scaled$Data$Y

    # Compute component scores
    lst_output$Scores$Test <-
      as.matrix( lst_scaled$Data$Y ) %*% lst_PCA$rotation

    # Compute correlations between components and raw variables
    lst_output$Correlations$Test <- sapply(
      1:ncol(train), function(p) {
        sapply(
          1:ncol( train ), function(v) {
            cor( lst_scaled$Data$Y[, v], lst_output$Scores$Test )
          }
        )
      }
    )

    rmse <- rep( NA, ncol(train) )

    # Loop over components
    for ( p in 1:ncol(train) ) {

      # If one component
      if ( p == 1 ) {

        # Ensure matrix algebra works
        test.hat <-
          cbind( lst_output$Scores$Test[, 1] ) %*% rbind( iR[1, ] )

        # Close 'If one component'
      } else {

        test.hat <-
          lst_output$Scores$Test[, 1:p] %*% iR[1:p, ]

        # Close else for 'If one component'
      }

      rmse[p] <-
        sqrt( mean( (test.hat - lst_scaled$Data$Y)^2 ) )

      # Close 'Loop over components'
    }

    lst_output$RMSE$Test <- rmse

    # Close 'If test data provided'
  }

  return( lst_output )
}


