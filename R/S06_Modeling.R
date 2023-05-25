# Modeling
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2023-05-24

# Table of contents
# 1) binary_SDT
# 2) EZ_diffusion

# TO DO
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
#'     \item{'none':}{ No correction applied.}
#'     \item{'log-linear':}{ The log-linear
#'     approach, adds \code{0.5} to the hits and false alarm
#'     frequencies, then adds \code{1} to the total number of
#'     observations for the signal and noise items respectively
#'     (Hautus, 1995).}
#'     \item{'conditional': }{ The conditional approach, where
#'     proportions equal to 0 or 1 are adjusted by \code{0.5/N}
#'     or \code{(N-0.5)/N} respectively, with \code{N} referring
#'     the associated number of total observations for the given
#'     proportion (Macmillan & Kaplan, 1985).}
#'   }
#'
#' @details The basic binary signal detection model assumes responses
#'   (correct identification of a signal during 'signal' trials and
#'   incorrect identification of a signal - a false alarm - during
#'   'noise' trials) arise by classifying a latent continuous value
#'   sampled either from a 'signal' or 'noise' distribution. If the
#'   value is above a threshold, participants indicates there is
#'   a 'signal', otherwise they indicate it is 'noise'. Because the
#'   'signal' and 'noise' distribution overlap, sometimes a
#'   value sampled from the 'noise' distribution will fall above the
#'   threshold and classified as a 'signal', and sometime a value
#'   sampled from the 'signal' distribution will fall below the
#'   threshold and be classified as 'noise'.
#'
#'   Assuming the distributions of evidence for 'signal' and 'noise'
#'   are Gaussian with their variances fixed to 1, their means
#'   fixed to 0.5(&#948;) and -0.5(&#948;) respectively,
#'   and a threshold parameter &#954;, one can solve for the
#'   parameters &#948; and &#954; given the proportion of
#'   hits H and false alarms FA:
#'
#'   &#954; = -0.5( &#934;&#8315;&#185;(H) +
#'   &#934;&#8315;&#185;(FA) ) and
#'   &#948; = 2( &#934;&#8315;&#185;(H) + &#954;), where
#'   &#934;&#8315;&#185; is the inverse of the
#'   standard normal cumulative distribution function
#'   (see [stats::qnorm]).
#'
#'   Other parameterizations exist, but the benefit of this one
#'   is easier interpretation of &#954;, as a value of 0
#'   indicates no bias in responding, values above zero
#'   indicate bias against responding 'noise' and
#'   values below zero indicate bias against responding 'signal'.
#'
#'
#' @return A named vector with five values:
#' \enumerate{
#'   \item{d': }{The estimate of separation between the noise
#'   and signal distributions.}
#'   \item{c: }{The estimate of response bias (the cut-off
#'   determining whether a response is 'signal' versus 'noise').}
#'   \item{A': }{A non-parametric estimate of discrimination
#'   (however see Pastore, Crawley, Berens, & Skelly, 2003).}
#'   \item{B: }{The ratio of whether people favor responding
#'   'signal' over whether they favor responding 'noise'}.
#'   \item{B'': }{A non-parametric estimate of B (however see
#'   Pastore et al., 2003).}
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
#' @encoding UTF-8
#'
#' @examples
#' # Proportion of hits and false alarms
#' x <- c(.8, .2)
#' round( binary_SDT( x ), 3 )
#' # Frequency of hits, false alarms, signal trials, noise trials
#' x <- c(15, 2, 20, 20)
#' round( binary_SDT( x ), 3 )
#' # Cannot compute d' if 100% or 0% for hits or false alarms
#' x <- c(20, 0, 20, 20)
#' binary_SDT( x )
#' # Corrections allow computation
#' round( binary_SDT( x, correct = 'log-linear' ), 3 )
#' round( binary_SDT( x, correct = 'conditional' ), 3 )
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
#' Estimate Parameters for EZ-Diffusion Model
#'
#' Function that estimates the parameters of the EZ-diffusion
#' model (Wagenmakers, van der Mass, Grasman, 2007) using
#' the mean and variance of all response times and the
#' proportion correct.
#'
#' @param x A numeric vector with 1) the mean for the response times
#'   (over both correct and incorrect trials), 2) the variance for the
#'   response times (over both correct and incorrect trials), and
#'   3) the proportion of correct responses.
#' @param number_of_trials An optional integer value given the
#'   total number of trials. When supplied, allows an edge
#'   correction to be implemented if the proportion correct
#'   is 0, 0.5, or 1 (parameters cannot be estimated in those cases
#'   without an edge correction).
#' @param within_trial_variability A numeric value governing the
#'   within-trial variability for the evidence accumulation
#'   process. If set to 0.1, assumes the mean and variance for
#'   response times are in seconds.
#' @param suppress_warnings Logical; if \code{TRUE} suppresses
#'   warnings of when an edge correction was necessary.
#'
#' @return A vector with the estimates for the drift rate,
#'   bias (assumed fixed to 0.5), boundary separation, and
#'   non-decision time.
#'
#' @references
#' Wagenmakers, E.-J., van der Mass, H. L. J., & Grasman,
#' R. P. P. P. (2007). An EZ-diffusion model for response
#' time and accuracy. Psychonomic Bulletin & Review, 14 (1),
#' 3 - 22.
#' DOI: 10.3758/BF03194023
#'
#' @examples
#' # Vector with mean RT, RT variance, P(Correct)
#' # using values from web app
#' x <- c( 0.723, 0.112, 0.802 )
#' round( EZ_diffusion( x ), 3 )
#'
#' # Edge correction for 100% accuracy
#' x <- c( 0.723, 0.112, 1.00 )
#' round( EZ_diffusion( x, number_of_trials = 100 ), 3 )
#'
#' @export

EZ_diffusion <- function(
    x,
    number_of_trials = NA,
    within_trial_variability = 0.1,
    suppress_warnings = FALSE) {

  # Initialize output
  output <- c(
    drift_rate = NA,
    bias = NA,
    boundary_sep = NA,
    non_decision_time = NA
  )

  # Extract data
  MRT <- x[1]
  VRT <- x[2]
  PC <- x[3]


  invalid_inputs <- c(
    # Missing data
    any( is.na(x) ),
    # Non-positive mean
    MRT <= 0,
    # Non-positive variance
    VRT <= 0,
    # Not proportion
    PC < 0 | PC > 1
  )

  invalid_input_warnings <- c(
    "  NA values",
    "  Non-positive mean response time",
    "  Non-positive variance for response time",
    "  Total correct not given as a proportion"
  )

  # Return NA for invalid inputs
  if (any(invalid_inputs)) {

    chr_error <- paste0(
      "Invalid inputs:\n",
      paste(invalid_inputs_warning[invalid_inputs], collapse = "\n")
    )
    warning(chr_error)

    return(output)

    # Close 'Return NA for invalid inputs'
  }

  # No information if P(Correct) = {0, 0.5, 1}
  if ( PC %in% c( 0, 0.5, 1.0) ) {

    # If no trials provided
    if ( is.na(number_of_trials) ) {

      chr_error <- paste0(
        "P(Correct) = ", round(PC, 1),
        "; parameters cannot be estimated ",
        "without an edge correction - provide ",
        "argument 'number_of_trials' to do so"
      )

      stop( chr_error )

      # Close 'If no trials provided'
    }

    # Warn if not enough information
    if (!suppress_warnings) {

      chr_warning <- paste0(
        "Insufficient information for estimation",
        " - an edge correction will be applied"
      )

      # Close 'Warn if not enough information'
    }

    if ( PC == 1) {
      PC <- (number_of_trials + .5) / (number_of_trials + 1)
    }
    if ( PC == 0) {
      PC <- .5 / (number_of_trials + 1)
    }
    if ( PC == 0.5 ) {
      (number_of_trials/2 + .5) / (number_of_trials + 1)
    }

    # Close 'No information if P(Correct) = {0, 0.5, 1}'
  }

  # Scaling
  s <- within_trial_variability
  s2 <- s^2

  # Compute intermediary terms
  lC <- qlogis(PC)
  PC2 <- PC^2
  intrm = lC*(lC*PC2 - lC*PC + PC - .5)/VRT

  # Estimate drift and boundary separation
  # from P(Correct) and the variance for response times
  drift_rate <- sign(PC - 0.5)*s*pow(intrm, 1/4)
  boundary_sep <- s2 * lC / drift_rate

  # Estimate the non-decision time
  y <- -drift_rate * boundary_sep / s2
  MDT <- (boundary_sep / (2 * drift_rate)) *
    (1 - exp(y)) / (1 + exp(y))
  non_decision_time <- MRT - MDT

  # Return estimates of parameters
  # if ( Correct / Trials == .5 ) drift_rate = 0
  output[1] <- drift_rate
  output[2] <- 0.5
  output[3] <- boundary_sep
  output[4] <- non_decision_time

  return(output)
}
