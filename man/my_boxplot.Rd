% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_data_exploration.R
\name{my_boxplot}
\alias{my_boxplot}
\title{Create a box plot}
\usage{
my_boxplot(data, xvar, yvar, color = NULL, facets = NULL)
}
\arguments{
\item{data}{The normalized data in a wide format (every analyte has its own
column).}

\item{xvar}{The variable that should be shown on the x-axis.}

\item{yvar}{The variable that should be shown on the y-axis.}

\item{color}{The variable that should correspond to the colors in the plot.
If \code{color} is NULL no variable will be linked to the colors (default).}

\item{facets}{The variable that should be used to facet the plot. If
\code{facets} is NULL the plot will not be faceted (default).}
}
\value{
A ggplot object.
}
\description{
This function creates a box plot with points overlaid on top.
}
\examples{
# First spectra curation has to be performed:
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

cut_offs_total <- calculate_cut_offs(summarized_checks = summarized_checks,
                                     control_sample_types = "PBS",
                                     exclude_sample_types = NULL,
                                     group_keyword = "Total",
                                     percentile = 97,
                                     use_mean_SD = FALSE,
                                     SD_factor = NULL,
                                     uncalibrated_as_NA = TRUE)

cut_offs_specific <- calculate_cut_offs(summarized_checks = summarized_checks,
                                        control_sample_types = "PBS",
                                        exclude_sample_types = NULL,
                                        group_keyword = "Spike",
                                        percentile = 97,
                                        use_mean_SD = FALSE,
                                        SD_factor = NULL,
                                        uncalibrated_as_NA = TRUE)

cut_offs <- dplyr::full_join(cut_offs_total,
                             cut_offs_specific)

curated_spectra <- curate_spectra(checked_data = checked_data,
                                  summarized_checks = summarized_checks,
                                  cut_offs = cut_offs)

passing_spectra <- kick_out_spectra(curated_spectra = curated_spectra)

for_analyte_curation <- remove_unneeded_columns(passing_spectra = passing_spectra)

# Then analyte curation is performed:
without_samples_to_ignore <- throw_out_samples(
   passing_spectra = for_analyte_curation,
   samples_to_ignore = c("PBS", "Visucon", "IVIGg", "Total")
)

checked_analytes <- check_analyte_quality_criteria(my_data = without_samples_to_ignore,
                                                   min_ppm_deviation = -20,
                                                   max_ppm_deviation = 20,
                                                   max_ipq = 0.2,
                                                   min_sn = 9,
                                                   criteria_to_consider = c("Mass accuracy",
                                                                            "S/N",
                                                                            "IPQ"))

curated_analytes <- curate_analytes(checked_analytes = checked_analytes,
                                    cut_off_percentage = 25)

analyte_curated_data <- dplyr::full_join(curated_analytes,
                                         for_analyte_curation) \%>\%
   dplyr::filter(has_passed_analyte_curation) \%>\%
   dplyr::select(-c(has_passed_analyte_curation, passing_percentage))

# Then we calculate the total intensities for each analyte:
total_intensities <- calculate_total_intensity(analyte_curated_data)

# And then we can perform total area normalization:
normalized_data <- normalize_data(total_intensities)

normalized_data_wide <- normalized_data \%>\% 
# Removing columns with values that differ between clusters: 
 dplyr::select(-tidyselect::any_of(c("passing_analyte_percentage",
                                     "cut_off_passing_analyte_percentage", 
                                     "cut_off_sum_intensity"))) \%>\% 
 tidyr::pivot_wider(names_from = c(cluster, analyte),
                    names_sep = "_",
                    values_from = relative_abundance)

my_boxplot(data = normalized_data_wide,
           xvar = "sample_type",
           yvar = "IgGI_IgGI1H3N4F1",
           color = "sample_type",
           facets = "group")
}
