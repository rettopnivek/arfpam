% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S06_Modeling.R
\name{binary_SDT}
\alias{binary_SDT}
\title{Transform Hit/False Alarm Rates into SDT Parameters}
\usage{
binary_SDT(x, centered = T, correct = "none")
}
\arguments{
\item{x}{Either...
\itemize{
\item A vector of four values, the frequencies for hits
and false alarms followed by the total number of
observations for the associated signal and noise
items, respectively;
\item A vector of two values, the proportion of hits
and false alarms.
}}

\item{centered}{Logical; if \code{TRUE} uses the
parameterization in which the signal and noise distributions
are equidistant from zero.}

\item{correct}{Type of correction to apply, either...
\itemize{
\item '0', '', 'None', 'none', 'No', or 'no' = No
correction applied;
\item '1', 'log-linear', 'H', or 'hautus' = The log-linear
approach, adds \code{0.5} to the hits and false alarm
frequencies, then adds \code{1} to the total number of
observations for the signal and noise items respectively
(Hautus, 1995);
\item '2', 'conditional', 'MK', or 'macmillan' =
The conditional approach, where proportions equal
to 0 or 1 are adjusted by \code{0.5/N} or \code{(N-0.5)/N}
respectively, with \code{N} referring the associated number
of total observations for the given proportion
(Macmillan & Kaplan, 1985).
}}
}
\value{
A named vector with five values:
\enumerate{
\item \code{d'} - The estimate of separation between the noise
and signal distributions;
\item \code{c} - The estimate of response bias (the cut-off
determining whether a response is 'signal' versus 'noise');
\item \code{A'} - A non-parametric estimate of discrimination
(however see Pastore, Crawley, Berens, & Skelly, 2003);
\item \code{B} - The ratio of whether people favor responding
'signal' over whether they favor responding 'noise';
\item \code{B”} - A non-parametric estimate of B (however see
Pastore et al., 2003).
}
}
\description{
Calculates d' and c parameter estimates for the Gaussian
equal-variance Signal Detection Theory (SDT) model
for binary data using the algebraic method. Also
computes alternative metrics, including A', B, and B”
(Stanislaw & Todorov, 1993).
}
\details{
Forthcoming
}
\examples{
# Forthcoming

}
\references{
Hautus, M. J. (1995). Corrections for extreme proportions and
their biasing effects on estimated values of d'. Behavior Research
Methods Instruments, & Computers, 27(1), 46 - 51.
DOI: 10.3758/BF03203619.

Macmillan, N. A. & Kaplan, H. L. (1985). Detection theory analysis
of group data: Estimating sensitivity from average hit and
false-alarm rates. Psychological Bulletin, 98(1), 185 - 199.
DOI: 10.1037/0033-2909.98.1.185

Pastore, R. E., Crawley, E. J., Berens, M. S., & Skelly, M. A.
(2003). "Nonparametric" A' and other modern misconceptions
about signal detection theory. Psychonomic Bulletin &
Review, 10(3), 556-569.
DOI: 10.3758/BF03196517

Stanislaw, H. & Todorov, N. (1993). Calculation of signal
detection theory measures. Behavior Research Methods,
Instruments, & Computers, 31, 137 - 149.
DOI: 10.3758/BF03207704
}