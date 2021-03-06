% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dirichlet.R
\name{fun_rdirichlet_hyperparameter}
\alias{fun_rdirichlet_hyperparameter}
\title{Sample the Dirichlet hyperparameter}
\usage{
fun_rdirichlet_hyperparameter(p)
}
\arguments{
\item{p}{number of variables}
}
\value{
a tibble with named colums
}
\description{
Assume it comes from a Uniform distribution on the $(p-1)$-simplex.
}
\details{
Formally, we sample from a Dirichlet with constant base measure, and concentration parameter = p.
The resulting Dirichlet parameter is constant = 1.
}
\seealso{
Other RNG functions: 
\code{\link{fun_rdirichlet}()}
}
\concept{RNG functions}
