% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_analyte_curation.R
\name{plot_analyte_curation}
\alias{plot_analyte_curation}
\title{Create a visual summary of the analyte curation process}
\usage{
plot_analyte_curation(
  curated_analytes,
  cut_off_percentage,
  selected_cluster,
  bio_groups_colname = ""
)
}
\arguments{
\item{curated_analytes}{The result of the \code{\link{curate_analytes}}
function: A dataframe with all analytes and charge combinations in the
data, their percentage of passing spectra and whether or not they passed
analyte curation.}

\item{cut_off_percentage}{The minimum percentage of spectra in which an
analyte needs to fulfill the quality criteria in order for that analyte to
pass curation.}

\item{selected_cluster}{The cluster for which the analyte curation results
should be plotted.}

\item{bio_groups_colname}{The name of the column that contains the biological groups,
as a character string. This parameter is only required when you want to perform
analyte curation per biological group. When this parameter is not specified, it is 
set to NULL (default) and analyte curation will not be performed per group.}
}
\value{
A bar plot with all analytes in the selected cluster on the x-axis,
  and the \code{passing_percentage} on the y-axis. The
  \code{cut_off_percentage} is indicated with a red horizontal dashed line.
  Analytes that passed curation are shown in blue, while analytes that didn't
  pass curation are shown in red.
}
\description{
Create a plot showing the results of analyte curation for a single cluster.
}
\examples{
data("example_data")
example_data <- curate_spectra(data = example_data,
                            min_ppm_deviation = -20,
                            max_ppm_deviation = 20,
                            max_ipq = 0.2,
                            min_sn = 9,
                            clusters_regex = "IgGI1",
                            cut_off_basis = c("Spike PBS", "Total PBS"))

curated_spectra <- example_data$curated_data \%>\%
   dplyr::filter(has_passed_spectra_curation == TRUE) \%>\% 
   dplyr::select(-has_passed_spectra_curation)

curated_analytes <- curate_analytes(
                data = curated_spectra,
                group_to_ignore = "Total",
                sample_types_to_ignore = c("Visucon",
                                           "PBS"),
                cut_off_percentage = 25)

plot_analyte_curation(curated_analytes = curated_analytes,
                      cut_off_percentage = 25,
                      selected_cluster = "IgGI1")
}
