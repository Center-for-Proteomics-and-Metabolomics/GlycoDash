# This file contains all functions that are used within the module
# mod_spectra_curation.R and within its sub-modules mod_curate_based_on_controls.R,
# mod_curate_based_on_percentiles.R, mod_tab_cut_offs.R and mod_tab_curated_spectra_plot.R.


#' Perform an analyte quality criteria check for every spectrum in the data.
#'
#' \code{check_analyte_quality_criteria()} performs an analyte quality criteria check for
#' every spectrum in the data.
#'
#' @param my_data A dataframe in long format (one row for each analyte + sample
#'   + charge combination).
#' @param min_ppm_deviation The lowest allowed value for the mass accuracy (in
#'   ppm).
#' @param max_ppm_deviation The highest allowed value for the mass accuracy (in
#'   ppm).
#' @param max_ipq The highest allowed value for the Isotopic Pattern Quality
#'   (IPQ). The IPQ indicates how much the isotopic pattern deviates from the
#'   theoretic isotopic pattern.
#' @param min_sn The lowest allowed value for the signal to noise ratio (S/N).
#' @param criteria_to_consider A character vector that indicates which quality
#'  criteria should be taken into account during spectra curation. The analyte 
#'  quality criteria that can be included are "Mass accuracy", "S/N" and "IPQ".
#'
#' @return The original dataframe given as the my_data argument, but with 3
#'   additional columns: \describe{\item{analyte_meets_criteria}{A Boolean:
#'   \code{TRUE} indicates that the analyte + charge combination in that sample fulfilled
#'   the quality criteria in criteria_to_consider, whereas \code{FALSE} indicates that 
#'   one or more criteria were not fulfilled.}\item{uncalibrated}{A Boolean:
#'   \code{TRUE} if the combination of cluster and sample failed calibration in LacyTools}
#'   \item{failed_criteria}{A character string describing which criteria in 
#'   criteria_to_consider were not fulfilled. If all criteria were fulfilled 
#'   failed_criteria is \code{NA}}}
#' @export
#'
#' @examples
#' data(example_data)
#' 
#' with_clusters <- define_clusters(data = example_data,
#'                                  cluster_keywords = "IgGI")
#' 
#' check_analyte_quality_criteria(my_data = with_clusters,
#'                                min_ppm_deviation = -20,
#'                                max_ppm_deviation = 20,
#'                                max_ipq = 0.2,
#'                                min_sn = 9,
#'                                criteria_to_consider = c("Mass accuracy",
#'                                                         "S/N",
#'                                                         "IPQ"))
check_analyte_quality_criteria <- function(my_data, 
                                           min_ppm_deviation, 
                                           max_ppm_deviation, 
                                           max_ipq, 
                                           min_sn,
                                           criteria_to_consider) {
  
  data_checked <- my_data %>% 
    dplyr::group_by(sample_name,  cluster) %>% 
    dplyr::mutate(uncalibrated = all(
      all(is.na(absolute_intensity_background_subtracted)),
      all(is.na(mass_accuracy_ppm)),
      all(is.na(isotopic_pattern_quality)),
      all(is.na(sn))
    )) %>% 
    dplyr::ungroup() %>% 
    check_each_criterium(., 
                         min_ppm_deviation,
                         max_ppm_deviation,
                         max_ipq,
                         min_sn) %>% 
    apply_chosen_criteria(.,
                          criteria_to_consider) %>%
    report_failed_criteria(.,
                           criteria_to_consider)
  
  return(data_checked)
}

#' Check all analyte quality criteria
#'
#' \code{check_each_criterium()} checks for each analyte quality criterium
#' separately if it is fulfilled, and is used in the function
#' \code{\link{check_analyte_quality_criteria}}.
#'
#' @inheritParams check_analyte_quality_criteria
#'
#' @return The original dataframe, but with an extra column for each quality
#'   criterium that is \code{TRUE} or \code{FALSE}.
#' @export
#'
#' @examples
#' data(example_data)
#' 
#' with_clusters <- define_clusters(data = example_data,
#'                                  cluster_keywords = "IgGI")
#' 
#' check_each_criterium(my_data = with_clusters,
#'                      min_ppm_deviation = -20,
#'                      max_ppm_deviation = 20,
#'                      max_ipq = 0.2,
#'                      min_sn = 9)
#' 
check_each_criterium <- function(my_data, 
                                 min_ppm_deviation,
                                 max_ppm_deviation,
                                 max_ipq,
                                 min_sn) {
  my_data %>% 
    dplyr::mutate(`Mass accuracy` = dplyr::between(mass_accuracy_ppm, 
                                                   min_ppm_deviation, 
                                                   max_ppm_deviation),
                  IPQ = isotopic_pattern_quality < max_ipq,
                  `S/N` = sn > min_sn,
                  dplyr::across(c(`Mass accuracy`, IPQ, `S/N`),
                                ~ tidyr::replace_na(.x, FALSE))
    )
}

#' Apply chosen analyte quality criteria
#'
#' \code{apply_chosen_criteria()} determines if the analyte quality criteria in
#' criteria to consider were all fulfilled and is used inside the function
#' \code{\link{check_analyte_quality_criteria}}.
#'
#' @param my_data The return value of the function
#'   \code{\link{check_each_criterium}}.
#' @inheritParams check_analyte_quality_criteria
#'
#' @return The original dataframe my_data with an additional column
#'   \code{analyte_meets_criteria} that is \code{TRUE} if the analyte fulfills
#'   all criteria in \code{criteria_to_consider} and otherwise is \code{FALSE}.
#'
#' @export
#'
#' @examples
#' data(example_data)
#' 
#' with_clusters <- define_clusters(data = example_data,
#'                                  cluster_keywords = "IgGI")
#' 
#' checked <- check_each_criterium(my_data = with_clusters,
#'                      min_ppm_deviation = -20,
#'                      max_ppm_deviation = 20,
#'                      max_ipq = 0.2,
#'                      min_sn = 9)
#' 
#' apply_chosen_criteria(my_data = checked,
#'                       criteria_to_consider = c("Mass accuracy",
#'                                                "S/N",
#'                                                "IPQ"))
#' 
apply_chosen_criteria <- function(my_data,
                                  criteria_to_consider) {
  to_return <- my_data %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(analyte_meets_criteria = all(
      dplyr::c_across(tidyselect::all_of(criteria_to_consider))
    ))
  
  return(to_return)
}

#' Report which analyte quality criteria were not met
#'
#' @param my_data The return value of the function
#'   \code{\link{apply_chosen_criteria}}.
#' @inheritParams check_analyte_quality_criteria
#'
#' @return The original dataframe my_data with an additional column
#'   \code{failed_criteria} that describes which criteria from criteria_to_consider
#'   were not fulfilled. If all criteria were met the value will be \code{NA}.
#' @export
#'
#' @examples
#' data(example_data)
#' 
#' with_clusters <- define_clusters(data = example_data,
#'                                  cluster_keywords = "IgGI")
#' 
#' checked <- check_each_criterium(my_data = with_clusters,
#'                      min_ppm_deviation = -20,
#'                      max_ppm_deviation = 20,
#'                      max_ipq = 0.2,
#'                      min_sn = 9)
#' 
#' applied <- apply_chosen_criteria(my_data = checked,
#'                                  criteria_to_consider = c("Mass accuracy",
#'                                                           "S/N",
#'                                                           "IPQ"))
#' 
#' report_failed_criteria(my_data = applied,
#'                        criteria_to_consider = c("Mass accuracy",
#'                                                "S/N",
#'                                                "IPQ"))
report_failed_criteria <- function(my_data,
                                   criteria_to_consider) {
  
  failed_criteria_dataframe <- my_data %>% 
    # Only criteria in criteria_to_consider will be reported as failed criteria:
    tidyr::pivot_longer(cols = tidyselect::all_of(criteria_to_consider),
                        names_to = "criterium",
                        values_to = "passed") %>% 
    dplyr::filter(!passed) %>% 
    dplyr::group_by(sample_name, analyte, charge) %>% 
    dplyr::summarize(failed_criteria = comma_and(criterium))
  
  dplyr::full_join(my_data, failed_criteria_dataframe) %>% 
    dplyr::select(-c(`Mass accuracy`, IPQ, `S/N`))
}

#' Summarize analyte quality criteria checks
#'
#' \code{summarize_spectra_checks()} calculates the percentage of passing
#' analytes per spectrum and the sum intensity of passing analytes per spectrum.
#' \code{summarize_spectra_checks()} should be used after
#' \code{\link{check_analyte_quality_criteria}} has been used to perform analyte quality
#' criteria checks for every spectrum and analyte combination in the data.
#'
#' @param checked_data The dataframe that is returned by
#'   \code{\link{check_analyte_quality_criteria}}.
#'
#' @return A dataframe that contains one row per spectrum for each cluster (
#'   Thus the number of rows is equal to the number of spectra times the number
#'   of clusters). The dataframe contains five columns: \describe{
#'   \item{sample_name}{The name of the sample for which this spectrum was
#'   recorded.} \item{sample_type}{The type of sample (e.g. negative control,
#'   blank etc.).} \item{group}{The group (Total or Specific) that this sample
#'   belongs to.} \item{cluster}{The cluster for which the metrics were calculated.}
#'   \item{passing_analyte_percentage}{The percentage of analytes that passed the
#'   criteria checks in this spectrum.} \item{sum_intensity}{The sum intensity
#'   of all passing analytes in this spectrum} }
#' @export
#'
#' @examples
#' data("example_data")
#'
#' example_data <- define_clusters(data = example_data,
#'                                 cluster_keywords = "IgGI")
#'
#' checked_data <- check_analyte_quality_criteria(my_data = example_data,
#'                                   min_ppm_deviation = -20,
#'                                   max_ppm_deviation = 20,
#'                                   max_ipq = 0.2,
#'                                   min_sn = 9,
#'                                   criteria_to_consider = c("Mass accuracy",
#'                                                           "S/N",
#'                                                           "IPQ"))
#'
#' summarize_spectra_checks(checked_data = checked_data)
summarize_spectra_checks <- function(checked_data) {
  
  # Alternative name?: calculate_sum_intensities_and_analyte_passing_percentage
  # or calculate_spectra_curation_metrics
  
  grouping_variables <- c("group", "sample_type", "cluster", "sample_name", "sample_id")
  
  summarized_checks <- checked_data %>% 
    dplyr::mutate(intensity_divided_by_fraction = absolute_intensity_background_subtracted / fraction) %>% 
    # I'm using across() and any_of() because if the data does not contain total
    # and specific samples, the column "group" doesn't exist:
    dplyr::group_by(dplyr::across(tidyselect::any_of(grouping_variables))) %>% 
    dplyr::summarise(passing_analyte_percentage = sum(analyte_meets_criteria)/dplyr::n() * 100, 
                     sum_intensity = sum(
                       intensity_divided_by_fraction[analyte_meets_criteria == TRUE],
                       na.rm = TRUE # needed for spectra that did calibrate but where 
                       # absolute_intensity still has one or more NAs 
                     ),
                     uncalibrated = unique(uncalibrated)) %>% 
    dplyr::ungroup(.)
  
  return(summarized_checks)
}

#' Calculate the population standard deviation.
#'
#' @param x A numeric vector or an R object but not a factor coercible to numeric by as.double(x).
#' @param na.rm Logical that indicates whether missing values should be removed.
#'
#' @return The population standard deviation (numeric).
#' @export
#'
#' @examples
#' numbers <- c(2, 5, 3, 9, 6)
#' sd_p(numbers)
#' 
#' numbers_na <- c(2, 5, 3, 9, 6, NA)
#' sd_p(numbers, na.rm = TRUE)
sd_p <- function(x, na.rm = FALSE) {
  sd(x, na.rm = na.rm) * sqrt((length(x)-1)/length(x))
}

mean_plus_SD <- function(x, SD_factor, na.rm) {
  mean_x <- mean(x, na.rm = na.rm)
  SD_x <- sd_p(x, na.rm = na.rm)
  mean_x + SD_factor * SD_x
}

#' Calculate cut-off values
#'
#' \code{calculate_cut_offs()} calculates the cut-off values for the sum
#' intensity and percentage of passing analytes during spectra curation.
#'
#' @param summarized_checks The return value of the function
#'   \code{\link{summarize_spectra_checks}}.
#' @param control_sample_types The sample type(s) that should be used as
#'   negative control samples to base the cut-offs on. This argument is only
#'   relevant if the method "Based on negative controls" is chosen, otherwise
#'   this argument should be NULL (default).
#' @param exclude_sample_types The sample type(s) that should not be taken into
#'   account in the calculation of the cut-off values. This argument is only
#'   relevant when the method "Based on percentiles" is chosen, otherwise this
#'   argument should be NULL (default).
#' @param group_keyword If the data contains total and specific Ig samples, the
#'   group_keyword indicates for which group (total or specific) cut-offs are
#'   being calculated. The cut-offs need to be calculated separately because the
#'   control_sample_types can differ between total and specific.
#' @param percentile When control_sample_types is not NULL, this is the
#'   percentile of the negative control samples' sum intensity and percentage of
#'   passing analytes that will be used as cut-off values. When
#'   control_sample_types is NULL, this is the percentile of all samples (except
#'   exclude_sample_types)' sum intensity and percentage of passing analytes
#'   that will be used as cut-off values.
#' @param use_mean_SD TRUE or FALSE, if TRUE then the mean and standard
#'   deviation (SD) instead of a percentile will be used to calculate the
#'   cut-off value for the sum intensity. The cut-off for the percentage of
#'   passing analytes will still be calculated with a percentile.
#' @param SD_factor Only relevant when use_mean_SD is TRUE. The cut-off value
#'   will be calculated as mean + SD_factor * SD.
#' @param uncalibrated_as_NA Should uncalibrated spectra be regarded as missing
#'   values (TRUE) or as zeroes (FALSE)?
#'
#' @return
#' @export
#'
#' @examples
calculate_cut_offs <- function(summarized_checks,
                               control_sample_types = NULL,
                               exclude_sample_types = NULL,
                               group_keyword = NULL,
                               percentile,
                               use_mean_SD = FALSE,
                               SD_factor = NULL,
                               uncalibrated_as_NA = FALSE) {
  
  cut_off_basis <- summarized_checks %>% 
    dplyr::filter(if (!is.null(group_keyword)) group == group_keyword else TRUE) %>% 
    dplyr::filter(if (!is.null(control_sample_types)) sample_type %in% control_sample_types else TRUE) %>% 
    dplyr::filter(if (!is.null(exclude_sample_types)) !(sample_type %in% exclude_sample_types) else TRUE) %>% 
    dplyr::filter(if (uncalibrated_as_NA) !uncalibrated else TRUE)
  
  grouping_variables <- c("group", "cluster")
  
  cut_offs <- cut_off_basis %>%  
    dplyr::group_by(dplyr::across(tidyselect::any_of(grouping_variables))) %>% 
    dplyr::summarise(
      cut_off_sum_intensity = if (use_mean_SD) { 
        mean_plus_SD(sum_intensity, SD_factor, uncalibrated_as_NA) } else { 
          quantile(sum_intensity, 
                   probs = percentile / 100,
                   names = FALSE,
                   na.rm = uncalibrated_as_NA) },
      cut_off_passing_analyte_percentage = quantile(passing_analyte_percentage, 
                              probs = percentile / 100,
                              names = FALSE,
                              na.rm = uncalibrated_as_NA),
      sample_type = unique(sample_type),
      curation_method = ifelse(is.null(control_sample_types), 
                               "based_on_percentiles",
                               "based_on_negative_controls")) %>% 
    tidyr::nest(., "sample_type_list" = sample_type)
    
  return(cut_offs)
}

#' Perform spectra curation
#'
#' \code{curate_spectra()} performs spectra curation on mass spectrometry data.
#' For each spectrum, analytes are curated based on the quality criteria (mass
#' accuracy, isotopic pattern quality (IPQ) and signal to noise ratio (S/N)).
#' Then the proportion of passing analytes and the sum intensity of passing
#' analytes is calculated for each spectrum (using the function
#' \code{\link{summarize_spectra_checks}}). Based on the average proportion and
#' sum intensity in a chosen group of samples that should not pass curation
#' (e.g. Specific Ig negative control samples), cut-off values for spectra
#' curation are defined (using the function \code{\link{calculate_cut_offs}}).
#' All spectra with values above those cut-off values pass the spectra curation.
#'
#' @inheritParams check_analyte_quality_criteria
#' @inheritParams calculate_cut_offs
#' @inheritParams define_clusters
#'
#' @return The function returns the original dataframe given as the data
#'   argument, but with two additional columns. One column is named
#'   "has_passed_spectra_curation"; This logical column is \code{TRUE} for spectra that have
#'   passed curation and \code{FALSE} for spectra that did not pass curation.
#'   The other column is named "analyte_meets_criteria" and is \code{TRUE} for analyte +
#'   sample combinations that passed all three quality criteria checks, whereas
#'   \code{FALSE} indicates that one or more criteria were not fulfilled.
#' @export
#'
#' @examples
#' data("example_data")
#' curate_spectra(data = example_data,
#'                clusters_regex = "IgGI1",
#'                min_ppm_deviation = -20,
#'                max_ppm_deviation = 20,
#'                max_ipq = 0.2,
#'                min_sn = 9,
#'                cut_off_basis = c("Spike PBS", "Total PBS"))
curate_spectra <- function(checked_data, summarized_checks, cut_offs) {
  
  summarized_checks_with_cut_offs <- dplyr::left_join(summarized_checks, 
                                                      cut_offs) %>% 
    dplyr::ungroup(.)
  
  curated_spectra <- summarized_checks_with_cut_offs %>% 
    # Can't use all() instead of & because all() is not vectorized
    dplyr::mutate(has_passed_spectra_curation = passing_analyte_percentage > cut_off_passing_analyte_percentage &
                    sum_intensity > cut_off_sum_intensity,
                  has_passed_spectra_curation = ifelse(uncalibrated, 
                                                       FALSE,
                                                       has_passed_spectra_curation)) %>% 
    determine_reason_for_failure()
  
  curated_data <- dplyr::full_join(curated_spectra, 
                                   checked_data) %>% 
    dplyr::relocate(c(has_passed_spectra_curation, reason_for_failure), 
                    .after = sample_name) %>% 
    dplyr::relocate(c(analyte_meets_criteria,
                      failed_criteria),
                    .after = charge) %>% 
    # any_of() because if sample_list was used to add sample ID's the plate_well
    # column doesn't exist:
    dplyr::relocate(tidyselect::any_of(c("sample_id", "plate_well")),
                    .after = sample_name)
  
  without_uncalibrated <- curated_data %>% 
    dplyr::filter(!uncalibrated)
  
  if (all(!without_uncalibrated$has_passed_spectra_curation)) {
    rlang::warn("None of the spectra passed curation.")
  } else if (all(without_uncalibrated$has_passed_spectra_curation)) {
    rlang::warn("All spectra passed curation.")
  }
  
  return(curated_data)
}

determine_reason_for_failure <- function(data) {
  with_reasons <- data %>% 
    dplyr::mutate(reason_for_failure = dplyr::case_when(
      uncalibrated ~ "Calibration failed.",
      passing_analyte_percentage <= cut_off_passing_analyte_percentage & sum_intensity <= cut_off_sum_intensity ~ "Percentage of passing analytes and sum intensity below cut-offs.",
      passing_analyte_percentage <= cut_off_passing_analyte_percentage ~ "Percentage of passing analytes below cut-off.",
      sum_intensity <= cut_off_sum_intensity ~ "Sum intensity below cut-off.",
      TRUE ~ as.character(NA) # as.character(), because case_When requires that 
      # all possible values are of the same data type
    ))
  
  return(with_reasons)
}

#' Create a plot to show the cut_offs for spectra curation
#'
#' @param spectra_check The dataframe returned by \code{\link{curate_spectra}}.
#'   \code{curate_spectra} returns two dataframes in a list. The dataframe that
#'   should be passed as the \code{spectra_check} argument to
#'   \code{create_cut_off_plot} is named "spectra_check".
#'
#' @return This function returns a ggplot object.
#' @export
#'
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#' data("example_data")
#' spectra_curation <- curate_spectra(data = example_data,
#'                                    clusters_regex = "IgGI1",
#'                                    min_ppm_deviation = -20,
#'                                    max_ppm_deviation = 20,
#'                                    max_ipq = 0.2,
#'                                    min_sn = 9,
#'                                    cut_off_basis = c("Spike PBS", "Total PBS"))
#' 
#' create_cut_off_plot(spectra_check = spectra_curation$spectra_check,
#'                     cut_off_basis = c("Spike PBS", "Total PBS"))
create_cut_off_plot <- function(summarized_checks) {
  
  for_plot <- summarized_checks %>% 
    # in case there are NAs when uncalibrated_as_NA is TRUE:
    tidyr::replace_na(replace = list(sum_intensity = 0,
                                     passing_analyte_percentage = 0))
  
  n_colors <- length(unique(for_plot$sample_type))
  my_palette <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_colors)
  
  p <- for_plot %>% 
    ggplot2::ggplot() +
    ggplot2::geom_jitter(data = for_plot[!for_plot$uncalibrated, ],
                         ggplot2::aes(color = sample_type,
                                      x = passing_analyte_percentage,
                                      y = sum_intensity,
                                      text = paste0("Sample name: ", 
                                                    sample_name,
                                                    "\n",
                                                    "Sample ID: ",
                                                    sample_id,
                                                    "\n",
                                                    "Passing analyte percentage: ",
                                                    passing_analyte_percentage,
                                                    "\n",
                                                    "Sum intensity: ",
                                                    sum_intensity,
                                                    "\nUncalibrated: ",
                                                    uncalibrated)),
                         size = 1,
                         alpha = 0.7) +
    ggplot2::geom_jitter(data = for_plot[for_plot$uncalibrated, ],
                         ggplot2::aes(color = sample_type,
                                      x = passing_analyte_percentage,
                                      y = sum_intensity,
                                      text = paste0("Sample name: ", 
                                                    sample_name,
                                                    "\n",
                                                    "Sample ID: ",
                                                    sample_id,
                                                    "\n",
                                                    "Passing analyte percentage: ",
                                                    passing_analyte_percentage,
                                                    "\n",
                                                    "Sum intensity: ",
                                                    sum_intensity,
                                                    "\nUncalibrated: ",
                                                    uncalibrated)),
                         shape = 15,
                         size = 1,
                         alpha = 0.7,
                         width = 0.01) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5),
                   strip.background = ggplot2::element_rect(fill = "#F6F6F8")) +
    ggplot2::scale_color_manual(values = my_palette,
                                name = "Sample type") +
    ggplot2::labs(y = "Sum intensity of passing analytes") +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x * 100, "%"), 
                                name = "Percentage of passing analytes")
  
  if ("group" %in% colnames(for_plot)) {
    p <- p +
      ggplot2::facet_wrap(cluster ~ group)
  } else {
    p <- p +
      ggplot2::facet_wrap(~ cluster)
  }
  
  return(p)
}


#' Title
#'
#' @param curated_data 
#' @param total_and_specific 
#'
#' @return
#' @export
#'
#' @examples
plot_spectra_curation_results <- function(curated_data,
                                          total_and_specific) {
  
  n_colors <- length(unique(curated_data$reason_for_failure)) - 1
  my_palette <- c(colorRampPalette(RColorBrewer::brewer.pal(8, "OrRd")[5:8])(n_colors),
                  "#3498DB")
  
  my_data <- curated_data %>% 
    dplyr::distinct(dplyr::across(tidyselect::any_of(c("group", 
                                                       "sample_type", 
                                                       "cluster", 
                                                       "sample_name", 
                                                       "has_passed_spectra_curation",
                                                       "reason_for_failure")))) %>% 
    create_nicer_reason_labels() %>%
    calculate_number_and_percentage_per_reason()
  
  plot <- my_data %>% 
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = sample_type,
                                   fill = `Passed curation?`,
                                   text = paste(
                                     "Number of spectra:",
                                     number,
                                     "\nPercentage of spectra:",
                                     percentage
                                   )), 
                      position = "fill") +
    ggplot2::xlab("Sample type") +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x * 100, "%"), 
                                name = "Percentage of spectra") +
    ggplot2::scale_fill_manual(values = my_palette) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   strip.background = ggplot2::element_rect(fill = "#F6F6F8"),
                   panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5))
  
  more_than_4_clusters <- length(unique(curated_data$cluster)) > 4
  
  if (total_and_specific == "Yes" & !more_than_4_clusters) {
    plot <- plot +
      ggplot2::facet_wrap(cluster ~ group)
  } else if (total_and_specific == "Yes" & more_than_4_clusters) {
    plot <- plot +
      ggplot2::facet_wrap(~ group)
  } else if (total_and_specific == "No" & !more_than_4_clusters) {
    plot <- plot +
      ggplot2::facet_wrap(~ cluster)
  }
  
  return(plot)
}

create_nicer_reason_labels <- function(curated_data) {
  
  #TODO: this function is overcomplicated, hardcode the nicer labels instead.
  
  reasons <- unique(curated_data$reason_for_failure)
  reason_labels <- firstlower(reasons) %>% 
    tidyr::replace_na(., "Yes")
  
  reason_labels[reason_labels != "Yes"] <- paste("No,", reason_labels[reason_labels != "Yes"])
  
  # Insert a linebreak after 20 characters
  reason_labels <- purrr::map_chr(
    reason_labels,
    function(label) {
      if (nchar(label) > 20) {
        from_char_20 <- substr(label, 20, nchar(label))
        replacement <- stringr::str_replace(from_char_20, 
                                            " ",
                                            "\n")
        substr(label, 20, nchar(label)) <- replacement
      }
      if (nchar(label) > 40) {
        from_char_40 <- substr(label, 40, nchar(label))
        replacement <- stringr::str_replace(from_char_40, 
                                            " ",
                                            "\n")
        substr(label, 40, nchar(label)) <- replacement
      }
      return(label)
    })
  
  # Set the names to the 'old' values in the data, so that this named vector can
  # be used to recode reason_for_failure with nicer labels:
  names(reason_labels) <- paste(reasons) # paste() converts NA to "NA"
  
  curated_data %>% 
    dplyr::mutate(
      `Passed curation?` = dplyr::recode(
        paste(reason_for_failure), # to convert NA to "NA"
        !!!reason_labels
      ) # recode() can't take a named vector as an argument, use !!! to 'unlist' it
    )
}

calculate_number_and_percentage_per_reason <- function(curated_data) {
  
  curated_data %>% 
    dplyr::group_by(dplyr::across(tidyselect::any_of(c("group",
                                                       "cluster",
                                                       "sample_type",
                                                       "reason_for_failure")))) %>% 
    dplyr::mutate(
      number_true = sum(has_passed_spectra_curation),
      number_false = sum(!has_passed_spectra_curation)) %>% 
    dplyr::ungroup(reason_for_failure) %>% # grouping by group, cluster and 
    # sample_type will remain, so that n() can be used later to calculate the
    # percentages
    dplyr::mutate(
      number = dplyr::case_when(
        has_passed_spectra_curation ~ number_true,
        !has_passed_spectra_curation ~ number_false,
        TRUE ~ as.integer(NA)
      ),
      percentage = scales::label_percent(accuracy = 0.01)(number / dplyr::n())
    )
  
}
