% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_read_lacytools.R
\name{get_analytes_info_from_list}
\alias{get_analytes_info_from_list}
\title{Get the analytes info from a LaCyTools summary using a list of output formats}
\usage{
get_analytes_info_from_list(data, list_of_variables)
}
\arguments{
\item{data}{A dataframe with a LaCyTools summary.}

\item{list_of_variables}{A list/vector with the name of LaCyTools output
formats.}
}
\value{
A dataframe with three columns (analyte, exact mass and fraction) and
  one row per analyte and charge combination.
}
\description{
This function uses \code{\link{get_analytes_info}} to get the exact mass of
the most abundant isotopologue and the fraction for each analyte in a
LaCyTools summary, for each charge state of those analytes. The reason that
the info is retrieved for each charge state is that the fraction of an
analyte can be different in different charge states. This can happen when
there are contaminants that need to be excluded near one charge state of an
analyte, but not near another charge state of that same analyte.
}
\examples{
data("LaCyTools_summary")

outputs <- list("Absolute Intensity (Background Subtracted, 2+)",
                "Absolute Intensity (Background Subtracted, 3+)", 
                "Mass Accuracy [ppm] (2+)", 
                "Mass Accuracy [ppm] (3+)",
                "Isotopic Pattern Quality (2+)",
                "Isotopic Pattern Quality (3+)",
                "S/N (2+)",
                "S/N (3+)")

get_analytes_info_from_list(data = LaCyTools_summary, list_of_variables = outputs)
}
