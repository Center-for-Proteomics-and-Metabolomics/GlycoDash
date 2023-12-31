% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_spectra_curation.R
\name{plot_spectra_curation_results}
\alias{plot_spectra_curation_results}
\title{Visualize the results of the spectra curation}
\usage{
plot_spectra_curation_results(curated_data, total_and_specific)
}
\arguments{
\item{curated_data}{The return value from the function
\code{\link{curate_spectra}}.}

\item{total_and_specific}{Character string, "Yes" or "No". This argument
indicates whether there are total and specific Ig samples in the data.}
}
\value{
A ggplot object.
}
\description{
This function can be used to visualize how many spectra per sample type passed
spectra curation and how many failed and why. It will create a bar plot with 
sample types on the x-axis and the percentage of spectra on the y-axis. Colors 
represent if and why the spectrum failed curation. If the data contains total 
and specific samples, the plot is faceted by group (total or specific). If the 
data contains 4 or less clusters the plot is also faceted by cluster.
}
\details{
The ggplot has a text aesthetic that can be used to show the number of spectra 
and the percentage of spectra as hover info if the ggplot object is converted 
to a ggplotly object (see example).
}
\examples{
data("example_data")

checked_data <- check_analyte_quality_criteria(my_data = example_data,
                                               min_ppm_deviation = -20,
                                               max_ppm_deviation = 20,
                                               max_ipq = 0.2,
                                               min_sn = 9,
                                               criteria_to_consider = c("Mass accuracy",
                                                                        "S/N",
                                                                        "IPQ"))

summarized_checks <- summarize_spectra_checks(checked_data = checked_data)

cut_offs <- calculate_cut_offs(summarized_checks = summarized_checks,
                               control_sample_types = NULL,
                               exclude_sample_types = "PBS",
                               group_keyword = NULL,
                               percentile = 2,
                               use_mean_SD = FALSE,
                               SD_factor = NULL,
                               uncalibrated_as_NA = TRUE)

curated_data <- curate_spectra(checked_data = checked_data,
                               summarized_checks = summarized_checks,
                               cut_offs = cut_offs)
                               
plot <- plot_spectra_curation_results(curated_data = curated_data,
                                      total_and_specific = "Yes")
                                      
# The plot can be made interactive with plotly. Use the "text" aesthetic to
# show hover info:
plotly::ggplotly(plot,
                 tooltip = "text")

}
