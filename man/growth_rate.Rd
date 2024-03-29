% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output_analysis.R
\name{growth_rate}
\alias{growth_rate}
\title{Calculate multiplicative growth rate of population}
\usage{
growth_rate(out)
}
\arguments{
\item{out}{Model output data frame}
}
\value{
Numeric vector of length one representing daily growth rate.
}
\description{
Calculate multiplicative growth rate of population
}
\examples{
out <- run(config_ex_1)
growth_rate(out)

}
