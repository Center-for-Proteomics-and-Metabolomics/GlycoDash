% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_derived_traits.R
\name{calculate_fucosylation}
\alias{calculate_fucosylation}
\title{Calculate fucosylation}
\usage{
calculate_fucosylation(.data)
}
\arguments{
\item{.data}{A data frame / tibble. Can be passed to the function via a pipe
(\code{\%>\%}) (see Examples below).}
}
\value{
A tibble with the columns \code{sample_name}, \code{cluster},
  \code{group}, \code{Fucosylation} and \code{fuc_formula}.
}
\description{
This function calculates the derived trait fucosylation based on the relative
abundances of glycans. This function is used in the function
\code{\link{calculate_derived_traits}}. It considers every analyte with "F"
followed by a single digit in its name to be a fucosylated analyte.
}
\examples{
See usage in the function calculate_derived_traits.
}
