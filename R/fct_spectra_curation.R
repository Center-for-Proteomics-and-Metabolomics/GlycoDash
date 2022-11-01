

#' Perform an analyte quality criteria check for every spectrum in the data.
#'
#' \code{check_analyte_quality_criteria()} performs an analyte quality criteria check for
#' every spectrum in the data. This function is used within the function
#' \code{\link{summarize_spectra_checks}}.
#'
#' @param data A dataframe in long format (one row for each analyte + sample
#'   combination).
#' @param min_ppm_deviation The lowest allowed value for the mass accuracy (in
#'   ppm).
#' @param max_ppm_deviation The highest allowed value for the mass accuracy (in
#'   ppm).
#' @param max_ipq The highest allowed value for the Isotopic Pattern Quality
#'   (IPQ). The IPQ indicates how much the isotopic pattern deviates from the
#'   theoretic isotopic pattern.
#' @param min_sn The lowest allowed value for the signal to noise ratio (S/N).
#'
#' @return The original dataframe given as the data argument, but with an
#'   additional column named "analyte_meets_criteria". This column is a logical vector:
#'   TRUE indicates that the analyte + sample combination passed all three
#'   quality criteria checks, whereas FALSE indicates one or more criteria were
#'   not fulfilled.
#' @export
#'
#' @examples
#' data(example_data)
#' check_analyte_quality_criteria(data = example_data,
#'                   min_ppm_deviation = -20,
#'                   max_ppm_deviation = 20,
#'                   max_ipq = 0.2,
#'                   min_sn = 9)
check_analyte_quality_criteria <- function(my_data, 
                                           min_ppm_deviation, 
                                           max_ppm_deviation, 
                                           max_ipq, 
                                           min_sn,
                                           criteria_to_consider,
                                           uncalibrated_as_NA) {
  
  data_checked <- my_data %>% 
    check_each_criterium(., 
                         min_ppm_deviation,
                         max_ppm_deviation,
                         max_ipq,
                         min_sn) %>% 
    apply_chosen_criteria(.,
                          criteria_to_consider,
                          uncalibrated_as_NA) %>%
    report_failed_criteria(.,
                           criteria_to_consider)
  
  return(data_checked)
}

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
                  `S/N` = sn > min_sn)
}

apply_chosen_criteria <- function(my_data,
                                  criteria_to_consider,
                                  uncalibrated_as_NA) {
  to_return <- my_data %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(analyte_meets_criteria = all(
      dplyr::c_across(tidyselect::all_of(criteria_to_consider))
    ))
  
  if (!uncalibrated_as_NA) {
    to_return <- dplyr::mutate(
      to_return,
      analyte_meets_criteria = tidyr::replace_na(analyte_meets_criteria,
                                                 FALSE)
    )
  }
  return(to_return)
}

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
#' \code{summarize_spectra_checks()} calculates the proportion of passing
#' analytes per spectrum and the sum intensity of passing analytes per spectrum.
#' \code{summarize_spectra_checks()} should be used after
#' \code{\link{check_analyte_quality_criteria}} has been used to perform analyte quality
#' criteria checks for every spectrum and analyte combination in the data.
#'
#' @param data_checked The dataframe that is returned by
#'   \code{\link{check_analyte_quality_criteria}}.
#'
#' @return A dataframe that contains one row per spectrum for each cluster (
#'   Thus the number of rows is equal to the number of spectra times the number
#'   of clusters). The dataframe contains five columns: \describe{
#'   \item{sample_name}{The name of the sample for which this spectrum was
#'   recorded.} \item{sample_type}{The type of sample (e.g. negative control,
#'   blank etc.).} \item{group}{The group (Total or Specific) that this sample
#'   belongs to.} \item{cluster}{The cluster that the analyte belongs to.}
#'   \item{passing_proportion}{The proportion of analytes that passed the
#'   criteria checks in this spectrum.} \item{sum_intensity}{The sum intensity
#'   of all passing analytes in this spectrum} }
#' @export
#'
#' @examples
#' data("example_data")
#'
#' example_data <- define_clusters(data = example_data,
#'                                 clusters_regex = "IgGI1")
#'
#' checked_data <- check_analyte_quality_criteria(data = example_data,
#'                                   min_ppm_deviation = -20,
#'                                   max_ppm_deviation = 20,
#'                                   max_ipq = 0.2,
#'                                   min_sn = 9)
#'
#' summarize_spectra_checks(data_checked = checked_data)
summarize_spectra_checks <- function(data_checked) {
  
  # Alternative name?: calculate_sum_intensities_and_analyte_passing_percentage
  
  grouping_variables <- c("group", "sample_type", "cluster", "sample_name", "sample_id")
  
  summarized_checks <- data_checked %>% 
    dplyr::mutate(intensity_times_fraction = absolute_intensity_background_subtracted * fraction) %>% 
    # I'm using across() and any_of() because if the data is not Ig data, the
    # column "group" doesn't exist:
    dplyr::group_by(dplyr::across(tidyselect::any_of(grouping_variables))) %>% 
    dplyr::summarise(passing_proportion = sum(analyte_meets_criteria)/dplyr::n(), 
                     sum_intensity = sum(
                       intensity_times_fraction[analyte_meets_criteria == TRUE]
                     )) %>% 
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
    dplyr::filter(if (!is.null(exclude_sample_types)) !(sample_type %in% exclude_sample_types) else TRUE)
  
  grouping_variables <- c("group", "cluster")
  
  cut_offs <- cut_off_basis %>%  
    dplyr::group_by(dplyr::across(tidyselect::any_of(grouping_variables))) %>% 
    dplyr::summarise(
      cut_off_sum_int = if (use_mean_SD) { 
        mean_plus_SD(sum_intensity, SD_factor, uncalibrated_as_NA) } else { 
          quantile(sum_intensity, 
                   probs = percentile / 100,
                   names = FALSE,
                   na.rm = uncalibrated_as_NA) },
      cut_off_prop = quantile(passing_proportion, 
                              probs = percentile / 100,
                              names = FALSE,
                              na.rm = uncalibrated_as_NA),
      sample_type = unique(sample_type),
      curation_method = "based_on_negative_controls") %>% 
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
curate_spectra <- function(checked_data, summarized_checks, cut_offs,
                           uncalibrated_as_NA) {
  
  summarized_checks_with_cut_offs <- dplyr::left_join(summarized_checks, 
                                                      cut_offs) %>% 
    dplyr::ungroup(.)
  
  curated_spectra <- summarized_checks_with_cut_offs %>% 
    # Can't use all() instead of & because all() is not vectorized
    dplyr::mutate(has_passed_spectra_curation = passing_proportion > cut_off_prop &
                    sum_intensity > cut_off_sum_int) %>% 
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
  
  if (!uncalibrated_as_NA) {
    curated_data <- curated_data %>% 
      dplyr::mutate(reason_for_failure = ifelse(
        is.na(absolute_intensity_background_subtracted) &
          is.na(mass_accuracy_ppm) &
          is.na(isotopic_pattern_quality) &
          is.na(sn), 
        "Calibration failed.", 
        reason_for_failure
      ))
  }
  
  without_uncalibrated <- curated_data %>% 
    dplyr::filter(reason_for_failure != "Calibration failed." | is.na(reason_for_failure))
  
  if (all(!without_uncalibrated$has_passed_spectra_curation)) {
    rlang::warn("None of the spectra passed curation.")
  } else if (all(without_uncalibrated$has_passed_spectra_curation)) {
    rlang::warn("All spectra passed curation.")
  }
  
  return(curated_data)
}

determine_reason_for_failure <- function(data) {
  with_reasons <- data %>% 
    dplyr::mutate(reason_for_failure = dplyr::na_if(
      dplyr::case_when(
        passing_proportion <= cut_off_prop & sum_intensity <= cut_off_sum_int ~ "Proportion of passing analytes and sum intensity below cut-offs.",
        passing_proportion <= cut_off_prop ~ "Proportion of passing analytes below cut-off.",
        sum_intensity <= cut_off_sum_int ~ "Sum intensity below cut-off.",
        # When uncalibrated_as_NA is TRUE:
        is.na(has_passed_spectra_curation) ~ "Calibration failed.",
        TRUE ~ "" # Cannot be NA, because case_When requires that all possible 
        # values are of the same type (in this case string)
      ),
      "")) # Replace with NA outside of the case_When call
  
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
create_cut_off_plot <- function(spectra_check) {
  
  n_colors <- length(unique(spectra_check$sample_type))
  my_palette <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_colors)
  
  p <- spectra_check %>% 
    ggplot2::ggplot() +
    ggplot2::geom_jitter(ggplot2::aes(color = sample_type,
                                      x = passing_proportion,
                                      y = sum_intensity,
                                      text = paste0("Sample name: ", 
                                                    sample_name,
                                                    "\n",
                                                    "Sample ID: ",
                                                    sample_id,
                                                    "\n",
                                                    "Passing proportion: ",
                                                    passing_proportion,
                                                    "\n",
                                                    "Sum intensity: ",
                                                    sum_intensity)),
                         size = 1) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5),
                   #text = ggplot2::element_text(size = 16),
                   strip.background = ggplot2::element_rect(fill = "#F6F6F8")) +
    ggplot2::scale_color_manual(values = my_palette,
                                name = "Sample type") +
    ggplot2::labs(y = "Sum intensity of passing analytes") +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x * 100, "%"), 
                                name = "Percentage of passing analytes")
  
  if ("group" %in% colnames(spectra_check)) {
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
#' @param Ig_data 
#'
#' @return
#' @export
#'
#' @examples
plot_spectra_curation_results <- function(curated_data,
                                          Ig_data) {
  
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
                                name = "Proportion of spectra (%)") +
    ggplot2::scale_fill_manual(values = my_palette) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   strip.background = ggplot2::element_rect(fill = "#F6F6F8"),
                   panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5))
  
  if (Ig_data == "Yes") {
    plot <- plot +
      ggplot2::facet_wrap(cluster ~ group)
  } else {
    plot <- plot +
      ggplot2::facet_wrap(~ cluster)
  }
  
  return(plot)
}

create_nicer_reason_labels <- function(curated_data) {
  
  reasons <- unique(curated_data$reason_for_failure)
  reason_labels <- firstlower(reasons) %>% 
    tidyr::replace_na(., "Yes")
  
  reason_labels[reason_labels != "Yes"] <- paste("No,", reason_labels[reason_labels != "Yes"])
  
  # Set the names to the 'old' values in the data, so that this named vector can
  # be used to recode reason_for_failure with nice:
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
    dplyr::ungroup(reason_for_failure) %>% # so that n() can be used later
    dplyr::mutate(
      number = dplyr::case_when(
        has_passed_spectra_curation ~ number_true,
        !has_passed_spectra_curation ~ number_false,
        TRUE ~ as.integer(NA)
      ),
      percentage = scales::label_percent(accuracy = 0.01)(number / dplyr::n())
    )
  
}
