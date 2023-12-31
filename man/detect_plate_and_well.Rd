% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_add_sample_ids.R
\name{detect_plate_and_well}
\alias{detect_plate_and_well}
\title{Detect the plate and well of a sample from the sample name.}
\usage{
detect_plate_and_well(data)
}
\arguments{
\item{data}{A dataframe. Should include a column named "sample_name".}
}
\value{
The input dataframe with an added column named "plate_well" that
  indicates on which plate and in which well a sample was analysed.
}
\description{
This function detects the plate and well position of a sample, based on it's
sample name. The sample name should be in a column named "sample_name" and
should contain either "plate" or "pl" (not case-sensitive) followed by the
plate number or a capital letter. Then the well position should be indicated
by a single capital letter between A and H followed directly by a number
between 1 and 12 (numbers smaller than 10 may be preceded by a zero, e.g. A01
and A1 will both be recognized). The plate number has to precede the well position,
but they can be separated by other characters.
}
\details{
If there are no plate numbers in the sample names, this function will assume
that there was only one plate.
}
\examples{
example <- data.frame(sample_name = c("s_0216_Specific_Plate4_A10",
                                      "s_568_Total_pl5_H4",
                                      "plate23_B6.s_8759",
                                      "sample3857_Pl8_D05.568"))

detect_plate_and_well(example)

example_no_plate_numbers <- data.frame(sample_name = c("s_0216_Specific_A10",
                                                       "s_568_Total_H4",
                                                       "specific_B6.s_8759",
                                                       "sample3857_D05.568"))

detect_plate_and_well(example_no_plate_numbers)
}
