% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S06_Modeling.R
\name{EZ_diffusion}
\alias{EZ_diffusion}
\title{Estimate Parameters for EZ-Diffusion Model}
\usage{
EZ_diffusion(
  x,
  number_of_trials = NA,
  within_trial_variability = 0.1,
  suppress_warnings = FALSE
)
}
\arguments{
\item{x}{A numeric vector with 1) the mean for the response times
(over both correct and incorrect trials), 2) the variance for the
response times (over both correct and incorrect trials), and
3) the proportion of correct responses.}

\item{number_of_trials}{An optional integer value given the
total number of trials. When supplied, allows an edge
correction to be implemented if the proportion correct
is 0, 0.5, or 1 (parameters cannot be estimated in those cases
without an edge correction).}

\item{within_trial_variability}{A numeric value governing the
within-trial variability for the evidence accumulation
process. If set to 0.1, assumes the mean and variance for
response times are in seconds.}

\item{suppress_warnings}{Logical; if \code{TRUE} suppresses
warnings of when an edge correction was necessary.}
}
\value{
A vector with the estimates for the drift rate,
bias (assumed fixed to 0.5), boundary separation, and
non-decision time.
}
\description{
Function that estimates the parameters of the EZ-diffusion
model (Wagenmakers, van der Mass, Grasman, 2007) using
the mean and variance of all response times and the
proportion correct.
}
\examples{
# Vector with mean RT, RT variance, P(Correct)
# using values from web app
x <- c( 0.723, 0.112, 0.802 )
round( EZ_diffusion( x ), 3 )

# Edge correction for 100\% accuracy
x <- c( 0.723, 0.112, 1.00 )
round( EZ_diffusion( x, number_of_trials = 100 ), 3 )

}
\references{
Wagenmakers, E.-J., van der Mass, H. L. J., & Grasman,
R. P. P. P. (2007). An EZ-diffusion model for response
time and accuracy. Psychonomic Bulletin & Review, 14 (1),
3 - 22.
DOI: 10.3758/BF03194023
}
