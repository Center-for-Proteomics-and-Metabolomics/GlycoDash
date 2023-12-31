% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_read_lacytools.R
\name{get_block}
\alias{get_block}
\title{Create a subset containing one block from a LaCyTools summary.}
\usage{
get_block(data, variable)
}
\arguments{
\item{data}{A dataframe with the LaCyTools summary (the result of
\code{\link{read_non_rectangular}}).}

\item{variable}{The name of a LaCyTools output format.}
}
\value{
A dataframe that is a subset of the input dataframe.
}
\description{
Create a subset containing one block from a LaCyTools summary.
}
\examples{
data(LaCyTools_summary)
get_block(LaCyTools_summary, 
          variable = "Absolute Intensity (Background Subtracted, 2+)")
          
}
