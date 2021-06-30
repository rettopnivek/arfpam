# Modeling
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-06-30

# Table of contents
# 1) binary_SDT
# 2) EZ_diffusion

# TO DO
# - Add details and examples for documentation of
#   'binary_SDT'
# - Update equations and implementation for
#   'EZ_diffusion'
# - Add documentation for 'EZ_diffusion'
# - Add unit tests for functions

#### 1) binary_SDT ####
#' Transform Hit/False Alarm Rates into SDT Parameters
#'
#' Calculates d' and c parameter estimates for the Gaussian
#' equal-variance Signal Detection Theory (SDT) model
#' for binary data using the algebraic method. Also
#' computes alternative metrics, including A', B, and B”
#' (Stanislaw & Todorov, 1993).
#'
#' @param x Either...
#'   \itemize{
#'     \item A vector of four values, the frequencies for hits
#'     and false alarms followed by the total number of
#'     observations for the associated signal and noise
#'     items, respectively;
#'     \item A vector of two values, the proportion of hits
#'     and false alarms.
#'   }
#' @param centered Logical; if \code{TRUE} uses the
#'   parameterization in which the signal and noise distributions
#'   are equidistant from zero.
#' @param correct Type of correction to apply, either...
#'   \itemize{
#'     \item '0', '', 'None', 'none', 'No', or 'no' = No
#'     correction applied;
#'     \item '1', 'log-linear', 'H', or 'hautus' = The log-linear
#'     approach, adds \code{0.5} to the hits and false alarm
#'     frequencies, then adds \code{1} to the total number of
#'     observations for the signal and noise items respectively
#'     (Hautus, 1995);
#'     \item '2', 'conditional', 'MK', or 'macmillan' =
#'     The conditional approach, where proportions equal
#'     to 0 or 1 are adjusted by \code{0.5/N} or \code{(N-0.5)/N}
#'     respectively, with \code{N} referring the associated number
#'     of total observations for the given proportion
#'     (Macmillan & Kaplan, 1985).
#'   }
#'
#' @details Forthcoming
#'
#' @return A named vector with five values:
#' \enumerate{
#'   \item \code{d'} - The estimate of separation between the noise
#'   and signal distributions;
#'   \item \code{c} - The estimate of response bias (the cut-off
#'   determining whether a response is 'signal' versus 'noise');
#'   \item \code{A'} - A non-parametric estimate of discrimination
#'   (however see Pastore, Crawley, Berens, & Skelly, 2003);
#'   \item \code{B} - The ratio of whether people favor responding
#'   'signal' over whether they favor responding 'noise';
#'   \item \code{B”} - A non-parametric estimate of B (however see
#'   Pastore et al., 2003).
#' }
#'
#' @references
#'
#' Hautus, M. J. (1995). Corrections for extreme proportions and
#' their biasing effects on estimated values of d'. Behavior Research
#' Methods Instruments, & Computers, 27(1), 46 - 51.
#' DOI: 10.3758/BF03203619.
#'
#' Macmillan, N. A. & Kaplan, H. L. (1985). Detection theory analysis
#' of group data: Estimating sensitivity from average hit and
#' false-alarm rates. Psychological Bulletin, 98(1), 185 - 199.
#' DOI: 10.1037/0033-2909.98.1.185
#'
#' Pastore, R. E., Crawley, E. J., Berens, M. S., & Skelly, M. A.
#' (2003). "Nonparametric" A' and other modern misconceptions
#' about signal detection theory. Psychonomic Bulletin &
#' Review, 10(3), 556-569.
#' DOI: 10.3758/BF03196517
#'
#' Stanislaw, H. & Todorov, N. (1993). Calculation of signal
#' detection theory measures. Behavior Research Methods,
#' Instruments, & Computers, 31, 137 - 149.
#' DOI: 10.3758/BF03207704
#'
#' @examples
#' # Forthcoming
#'
#' @export

binary_SDT <- function(x,
                       centered = T,
                       correct = 'none') {

  # Initialize values
  out <- c("d'" = NA, "c" = NA, "A'" = NA, "B" = NA, "B''" = NA)

  H <- NULL
  FA <- NULL

  # Extract data

  #< Counts and # of observations
  if (length(x) == 4) {

    # Extract frequencies
    S <- x[1]
    N <- x[2]
    # Extract total number of trials
    Ns <- x[3]
    Nn <- x[4]

    # Determine hits and false-alarm rates
    H <- S / Ns
    FA <- N / Nn

    # Apply corrections if indicated

    #<| Log-linear
    if (correct %in% c('1', 'log-linear', 'H', 'hautus')) {

      H <- (S + .5) / (Ns + 1)
      FA <- (N + .5) / (Nn + 1)

      #|> Close conditional 'Log-linear'
    }

    #<| Conditional
    if (correct %in% c( '2', 'conditional', 'MK', 'macmillan')) {

      if (H == 0) H <- .5 / Ns
      if (FA == 0) FA <- .5 / Nn
      if (H == 1) H <- (Ns - .5) / Ns
      if (FA == 1) FA <- (Nn - .5) / Nn

      #|> Close conditional 'Conditional'
    }

    #> Close conditional 'Counts and # of observations'
  }

  #< Proportions
  if (length(x) == 2) {

    # Extract hits and false-alarm rates
    H <- x[1]
    FA <- x[2]

    if (!correct %in% c('0','none','','None','No','no')) {
      string <- paste(
        "Correction cannot be applied",
        "using proportions; frequencies",
        "and total number of trials are",
        "required."
      )
      warning(string, call. = F)
    }

    #> Close conditional 'Proportions'
  }

  #< Compute measures
  if (!is.null(H) & !is.null(FA)) {

    #<| Non-centered method
    if (!centered) {

      # Obtain estimate of d'
      dp_est <- qnorm(H) - qnorm(FA)

      # Obtain estimate of criterion
      k_est <- qnorm(1 - FA)

      #|> Close conditional 'Non-centered method'
    } else {
      # Centered method

      # Obtain estimate of criterion
      k_est <- -.5 * (qnorm(H) + qnorm(FA))

      # Obtain estimate of d'
      dp_est <- 2 * (qnorm(H) + k_est)

      #|> Close conditional 'Centered method'
    }

    # Compute additional values
    num <- pow(H - FA, 2) + (H - FA)
    denom <- 4 * max(H, FA) - 4 * H * FA
    Ap_est <- sign(H - FA) * (num / denom)

    log_beta_est <- .5 * (pow(qnorm(FA), 2) - pow(qnorm(H), 2))

    num <- H * (1 - H) - FA * (1 - FA)
    denom <- H * (1 - H) + FA * (1 - FA)
    beta_pp <- sign(H - FA) * (num / denom)

    out[1] <- dp_est
    out[2] <- k_est
    out[3] <- Ap_est
    out[4] <- exp(log_beta_est)
    out[5] <- beta_pp

    #> Close conditional 'Compute measures'
  }

  return(out)
}

#### 2) EZ_diffusion ####

#' ...
#'
#' ...
#'
#' @param x ...
#'
#' @return ...
#'
#' @examples
#' # Forthcoming
#'
#' @export

EZ_diffusion <- function(dat, s = 1) {

  # Initialize output
  out <- c(
    drift_rate = NA,
    bias = NA,
    boundary_sep = NA,
    non_decision_time = NA
  )

  # Extract data
  Trials <- dat[1]
  Correct <- dat[2]
  MRT <- dat[3]
  VRT <- dat[4]

  # Check for invalid inputs
  invalid_inputs <- c(
    # Missing data
    any(is.na(dat)),
    # Non-positive mean response time
    MRT <= 0,
    # Non-positive variance
    VRT <= 0,
    # Number of trials is not an integer
    Trials %% 1 != 0,
    # Number of correct responses is not an integer
    Correct %% 1 != 0,
    # Number of correct responses exceeds number of trials
    Correct > Trials
  )

  invalid_inputs_warning <- c(
    "NA values",
    "Non-positive mean response time",
    "Non-positive variance for response time",
    "Non-integer value for trials",
    "Non-interger value for frequency correct",
    "Frequency correct exceeds number of trials"
  )

  # Return NA for invalid inputs
  if (any(invalid_inputs)) {
    string <- paste(
      "Invalid inputs:\n",
      paste(invalid_inputs_warning[invalid_inputs], collapse = "\n"),
      sep = ""
    )
    warning(string, call. = F)
    return(out)
  }

  # Proportion of correct responses
  Pc <- Correct / Trials

  # Scaling parameter
  s2 <- pow(s, 2)

  # No information if P( Correct ) = 0, 0.5, or 1
  if (Pc %in% c(0, .5, 1)) {
    warning(paste(
      "Insufficient information for estimation",
      " - an edge correction will be applied"
    ),
    call. = FALSE
    )

    # For the edge correction, adjust results
    # by one half of an error:
    if (Pc == 1) {
      Pc <- 1 - 1 / (2 * Trials)
    }
    if (Pc == 0) {
      Pc <- 1 + 1 / (2 * Trials)
    }
    if (Pc == .5) {
      Pc <- .5 + 1 / (2 * Trials)
    }
  }

  # Compute the log odds for P( Correct )
  L <- qlogis(Pc)
  x <- L * (L * pow(Pc, 2) - L * Pc + Pc - .5) / VRT

  # Estimate drift and boundary separation
  # from P( Correct ) and the variance of correct RT
  drift_rate <- sign(Pc - .5) * s * pow(x, 1 / 4)
  boundary_sep <- s2 * L / drift_rate

  # Estimate the non-decision time
  y <- -drift_rate * boundary_sep / s2
  MDT <- (boundary_sep / (2 * drift_rate)) *
    (1 - exp(y)) / (1 + exp(y))
  non_decision_time <- MRT - MDT

  # Return estimates of parameters
  # if ( Correct / Trials == .5 ) drift_rate = 0
  out[1] <- drift_rate
  out[2] <- 0.5
  out[3] <- boundary_sep
  out[4] <- non_decision_time

  return(out)
}
