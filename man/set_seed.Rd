% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_set_seed.R
\name{set_seed}
\alias{set_seed}
\title{Set seed before random selection}
\usage{
set_seed(seed = NULL)
}
\arguments{
\item{seed}{Seed for the pseudo-random selection. By default (\code{NULL}), seed will be selected based on \code{Sys.time()}}
}
\value{
Random seed set
}
\description{
Do once only!
}
\examples{
set_seed()
}
