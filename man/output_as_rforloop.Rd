% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output_as_rforloop.R
\name{output_as_rforloop}
\alias{output_as_rforloop}
\title{Output a simplan into an R script.}
\usage{
output_as_rforloop(file, plan, add_nsims = TRUE)
}
\arguments{
\item{file}{Output file path.}

\item{plan}{Simplan plan.}

\item{add_nsims}{Add the n-sims directly.}
}
\description{
Output a simplan into an R script.
}
\examples{
plan <- yaml::read_yaml('~/git-projects/simplan/inst/test.yaml')
\dontrun{output_as_rforloop(plan,file = 'results.R',add_nsims = TRUE)}
}
