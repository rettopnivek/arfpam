# Statistics
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-07-19

# Table of contents
# 1) sem
# 2) statistic
# 3) boxcox_transform
# 4) pvalues
# 5) bootstrap
# 6) summa
# 7) bounds
# 8) standardize

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

sem <- function(x, na.rm = TRUE) {

  #< Remove missing values
  if (na.rm) {
    x <- x[!is.na(x)]

    # > Close conditional
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

boxcox_transform <- function(x,
                             outcome = NULL,
                             parameter_grid = seq(-3, 3, .01),
                             output = TRUE) {

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

pvalues <- function(x,
                    comparison = 0,
                    alternative = "two-sided",
                    digits = 3,
                    string = FALSE,
                    pad = FALSE) {

  # Initialize output
  out <- NULL

  # Assume if size of x equals 1 that
  # it is a numeric p-value

  #< Check length
  if (length(x) == 1) {

    #<| Check if valid p-value
    if (x >= 0 &
      x <= 1) {

      # Force argument 'string' to be TRUE
      string <- TRUE

      #|> Close conditional 'Check if valid p-value'
    } else {
      stop("p-values must be between 0 and 1",
        call. = FALSE
      )

      #|> Close else for 'Check if valid p-value'
    }

    # > Close conditional 'Check length'
  }

  #< Estimate p-value from Monte Carlo samples
  if (!string) {
    check <- FALSE

    # Two-sided test
    if (alternative == "two-sided") {
      check <- TRUE

      if (median(x) > comparison) {
        out <- mean(x < comparison)
      } else {
        out <- mean(x > comparison)
      }
      out <- out * 2
    }

    # Test if greater than comparison
    if (alternative == "greater") {
      check <- TRUE

      out <- mean(x < comparison)
    }

    # Test if less than comparison
    if (alternative == "less") {
      check <- TRUE

      out <- mean(x > comparison)
    }

    # Informative error message if
    # 'alternative' mis-specified
    if (!check) {
      err_msg <- paste(
        "Please specify 'alternative' as",
        "either 'two-sided', 'greater', or",
        "'less'."
      )
      stop(err_msg)
    }

    # > Close conditional 'Estimate p-value from Monte Carlo samples'
  }

  #< Convert to formatted string
  if (string) {
    if (pad) {
      p <- format(round(x, digits = digits), nsmall = digits)
    } else {
      p <- format(round(x, digits = digits))
    }

    if (round(x, digits) == 0) {
      nd <- digits - 1
      nd <- paste(
        "0.",
        paste(rep(0, nd), collapse = ""),
        "1",
        sep = ""
      )
      out <- paste("p < ", nd, sep = "")
    } else {
      out <- paste0("p = ", p)
    }

    # > Close conditional 'Convert to formatted string'
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

  #< Sample size
  if (grepl("[[N]]", syntax, fixed = TRUE)) {
    n <- length(x)
    if (na.rm) length(x[!is.na(x)])

    out <- gsub("[[N]]", n, out, fixed = TRUE)

    #> Close conditional 'Sample size'
  }

  #< Mean
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

    #> Close conditional 'Mean'
  }

  #< Standard deviation
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

    #> Close conditional 'Standard deviation'
  }


  #< Standard error
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

    #> Close conditional 'Standard error'
  }

  #< Minimum
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

    #> Close conditional 'Minimum'
  }

  #< 1st quartile
  if (grepl("[[Q1]]", syntax, fixed = TRUE)) {

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

    #> Close conditional '1st quartile'
  }

  #< Median
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

    #> Close conditional 'Median'
  }

  #< 3rd quartile
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

    #> Close conditional '3rd quartile'
  }

  #< Maximum
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

    #> Close conditional 'Maximum'
  }


  #< Inter-quartile range
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

    #> Close conditional 'Inter-quartile range'
  }

  #< Counts/frequencies
  if (grepl("[[C]]", syntax, fixed = TRUE)) {
    if (is.null(categories)) {
      categories <- TRUE
    }

    cnt <- sum(x %in% categories, na.rm = na.rm)

    out <- gsub("[[C]]", cnt, out, fixed = TRUE)

    #> Close conditional 'Counts/frequencies'
  }

  #< Percent
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

    #> Close conditional 'Percent'
  }

  #< Proportion
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

    #> Close conditional 'Proportion'
  }

  #< Custom function
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

    #> Close conditional 'Custom function'
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
#'   (must have the same column names as \code{x}).
#' @param columns A character vector of column names,
#'   the subset of columns in \code{x} to standardize.
#' @param raw Logical; if \code{TRUE}, uses the means and
#'   standard deviations stored in each column's
#'   attributes to restore scaled values to their
#'   original raw values.
#' @param scaling Logical; if \code{TRUE} returns
#'   the means and standard deviations for columns
#'   stored in their attributes.
#'
#' @return A data frame or matrix with standardized
#' columns.
#'
#' @examples
#' # Create data frame
#' x_raw <- matrix( rnorm( 1000, 100, 15 ), 100, 10 )
#' colnames(x_raw) <- paste0( 'X', 1:10 )
#' x_raw <- data.frame( x_raw )
#'
#' # Standardize columns
#' x <- standardize( x_raw )
#'
#' # Create second data frame with same
#' # variables but new values
#' y <- matrix( rnorm( 50*10, 50, 15 ), 50, 10 )
#' colnames(y) <- paste0( 'X', 1:10 )
#' y <- data.frame( y )
#'
#' # Scale columns of y based on means and
#' # standard deviations from x
#' y <- standardize( x, y )
#'
#' # Undo scaling
#' all( round( x_raw, 4 ) == round( standardize( x, raw = TRUE ), 4 ) )
#'
#' @export

standardize <- function( x, y = NULL,
                         columns = NULL,
                         raw = FALSE,
                         scaling = FALSE ) {

  # If no column names are given
  if ( is.null( columns ) ) {
    columns <- colnames( x )
  }

  # Number of columns
  K <- length( columns )

  # Vectors for mean and standard deviation
  m <- rep( NA, K )
  s <- rep( NA, K )

  # Identify rows with any missing data
  is_na <- apply( x[,columns], 1, function(x) any( is.na(x) ) )

  #< Check if data frame
  if ( is.data.frame( x ) ) {

    #<| Loop over columns
    for ( k in 1:K ) {

      # Extract attributes
      cur_attr <- attributes( x[[ columns[k] ]] )

      if ( !is.null( cur_attr ) ) {

        if ( 'standardize' %in% names( cur_attr ) ) {
          m[k] <- cur_attr$standardize$mean
          s[k] <- cur_attr$standardize$sd
        }

      }

      #|> Close 'Loop over columns'
    }

    #<| Convert back to original scale
    if ( raw ) {

      #<|< Loop over columns
      for ( k in 1:K ) {

        x[, columns[k]] <-
          x[, columns[k]]*s[k] + m[k]

        #>|> Close 'Loop over columns'
      }

      return( x )

      #|> Close 'Convert back to original scale'
    }

    if ( scaling ) {

      return( lapply(
        columns,
        function(i) attributes( x[[ i ]] )$standardize
      ) )

    }

    #> Close 'Check if data frame'
  }

  #< Compute means and standard deviations
  if ( any( is.na( m ) ) ) {

    #<| Loop over columns
    for ( k in 1:K ) {

      m[k] <- mean( x[ !is_na, columns[k] ] )
      s[k] <- sd( x[ !is_na, columns[k] ] )

      # Standardize current column
      x[, columns[k]] <-
        as.numeric( ( x[, columns[k]] - m[k] )/ s[k] )

      #<|< If x is a data frame
      if ( is.data.frame( x ) ) {

        cur_attr <- attributes( x[[ columns[k] ]] )
        if ( is.null( cur_attr ) ) {
          cur_attr <- list(
            standardize = list( mean = m[k], sd = s[k] )
          )
        } else {
          cur_attr$standardize = list( mean = m[k], sd = s[k] )
        }

        attributes( x[[ columns[k] ]] ) <- cur_attr

        #>|> Close 'If x is a data frame'
      }

      #|> Close 'Loop over columns'
    }

    #< Close 'Compute means and standard deviations'
  }

  #< If second matrix or data frame is provided
  if ( !is.null( y ) ) {

    #<| Loop over columns
    for ( k in 1:K ) {

      y[, columns[k]] <-
        as.numeric( ( y[, columns[k]] - m[k] ) / s[k] )

      #<|< If y is a data frame
      if ( is.data.frame( y ) ) {

        cur_attr <- attributes( y[[ columns[k] ]] )
        if ( is.null( cur_attr ) ) {
          cur_attr <- list(
            standardize = list( mean = m[k], sd = s[k] )
          )
        } else {
          cur_attr$standardize = list( mean = m[k], sd = s[k] )
        }

        attributes( y[[ columns[k] ]] ) <- cur_attr

        #>|> Close 'If y is a data frame'
      }

      #|> Close 'Loop over columns'
    }

    return( y )

    #> Close 'If second matrix or data frame is provided'
  }

  return( x )
}
