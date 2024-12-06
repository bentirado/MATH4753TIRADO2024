% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ntickets.R
\name{ntickets}
\alias{ntickets}
\title{The ntickets Function}
\usage{
ntickets(N = 200, gamma = 0.02, p = 0.95)
}
\arguments{
\item{N}{an integer stating the amount of "seats" in the flight}

\item{gamma}{an integer between 0 <= gamma <= 1 which is the probability that the plane will be overbooked}

\item{p}{an integer between 0<= p <= 1 which is the probability that a seat "shows"}
}
\value{
two graphs discrete and continuous, which display the n value of the distribution.
}
\description{
The ntickets Function
}
\examples{
ntickets(N=200, gamma=0.02, p=0.95)
}
