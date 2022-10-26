

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
    dplyr::mutate(to_return,
                  analyte_meets_criteria = tidyr::replace_na(analyte_meets_criteria,
                                                             FALSE))
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
  
  # Remove because not relevant in dashboard?
  
  # required_columns <- list("sample_type",
  #                          "sample_name",
  #                          "cluster",
  #                          "analyte_meets_criteria")
  # 
  # if(any(!(required_columns %in% colnames(data_checked)))) {
  #   missing_columns <- required_columns[!(required_columns %in% colnames(data_checked))]
  #   stop(paste0("The data doesn't contain the required column(s) ",
  #               paste0(missing_columns,
  #                      collapse = " and "),
  #               "."))
  # }
  
  grouping_variables <- c("group", "sample_type", "cluster", "sample_name", "sample_id")
  
  summarized_checks <- data_checked %>% 
    # I'm using across() and any_of() because if the data is not Ig data, the
    # column "group" doesn't exist:
    dplyr::group_by(dplyr::across(tidyselect::any_of(grouping_variables))) %>% 
    dplyr::summarise(passing_proportion = sum(analyte_meets_criteria)/dplyr::n(), 
                     sum_intensity = sum(
                       absolute_intensity_background_subtracted[analyte_meets_criteria == TRUE]
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

#' Calculate cut-off values for spectra curation
#'
#' Calculate the cutoffs for the proportion of passing analytes per spectrum and
#' for the sum intensity of passing analytes per spectrum. This function is used
#' within the function \code{\link{curate_spectra}}.
#'
#' @param spectra_check The dataframe returned by
#'   \code{\link{summarize_spectra_checks}}.
#' @param cut_off_basis A character vector or a single character string
#'   specifying which sample types (and optionally which group (Specific or
#'   Total Ig)) the spectra curation cut-offs should be based on. For each
#'   sample type or sample type-group combination that you want to base the
#'   cut-offs on you should add a character string. For example. if you want to
#'   base the cut-offs on all PBS samples and on the Specific negative_control
#'   samples, \code{cut_off_basis} should be \code{c("PBS", "Specific
#'   negative_control")}
#'
#' @return
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
#' spectra_check <- summarize_spectra_checks(data_checked = checked_data)
#'
#' calculate_cut_offs(spectra_check = spectra_check,
#'                    cut_off_basis = c("Spike PBS", "Total PBS"))
#'                    
calculate_cut_offs_with_mean_SD <- function(summarized_checks, 
                                            negative_controls, 
                                            percentile,
                                            SD_factor) {
  
  negative_control_samples <- filter_cut_off_basis(cut_off_basis = negative_controls,
                                                data = summarized_checks)
  
  cut_offs <- negative_control_samples %>%  
    dplyr::group_by(cluster) %>% 
    dplyr::summarise(cut_off_prop = quantile(passing_proportion, 
                                             probs = percentile / 100,
                                             names = FALSE),
                     av_sum_int = mean(sum_intensity, na.rm = FALSE),
                     sd_sum_int = sd_p(sum_intensity, na.rm = FALSE),
                     cut_off_sum_int = av_sum_int + (SD_factor * sd_sum_int),
                     across(tidyselect::any_of(c("group", "sample_type")))) %>% 
    dplyr::mutate(type = "based_on_negative_controls") %>% 
    dplyr::distinct() %>% 
    dplyr::select(tidyselect::any_of(c("cluster",
                                     "sample_type",
                                     "group",
                                     "cut_off_prop",
                                     "cut_off_sum_int",
                                     "type"))) %>% 
    tidyr::nest(., "sample_type_list" = sample_type)
  
  return(cut_offs)
}

# Combine these two cut-off calculations?

calculate_cut_offs_with_percentile <- function(summarized_checks,
                                               negative_control_sample_types = NULL,
                                               percentile,
                                               na.rm) {
  
  if (!is.null(negative_control_sample_types)) {
    negative_controls <- filter_cut_off_basis(
      cut_off_basis = negative_control_sample_types,
      data = summarized_checks
    )
    cut_off_basis <- negative_controls
  } else {
    cut_off_basis <- summarized_checks
  }
  
  grouping_variables <- c("group", "cluster")
  
  cut_offs <- cut_off_basis %>%  
    dplyr::group_by(dplyr::across(tidyselect::any_of(grouping_variables))) %>% 
    dplyr::summarise(cut_off_sum_int = quantile(sum_intensity, 
                                                probs = percentile / 100,
                                                names = FALSE,
                                                na.rm = na.rm),
                     cut_off_prop = quantile(passing_proportion, 
                                             probs = percentile / 100,
                                             names = FALSE,
                                             na.rm = na.rm),
                     dplyr::across(sample_type)) %>% 
    dplyr::mutate(type = "based_on_negative_controls") %>% 
    dplyr::distinct() %>% 
    dplyr::select(tidyselect::any_of(c("cluster",
                                       "sample_type",
                                       "group",
                                       "cut_off_prop",
                                       "cut_off_sum_int",
                                       "type"))) %>% 
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
#'   "passed_spectra_curation"; This logical column is \code{TRUE} for spectra that have
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
  
  # if ("cluster" %in% colnames(cut_offs)) {
  #   summarized_checks <- dplyr::left_join(summarized_checks, 
  #                                         cut_offs) %>% 
  #     dplyr::ungroup(.)
  # } else {
  #   summarized_checks <- summarized_checks %>% 
  #     dplyr::mutate(cut_off_sum_int = cut_offs$cut_off_sum_int,
  #                   cut_off_prop = cut_offs$cut_off_prop)
  # }
  
  summarized_checks <- dplyr::left_join(summarized_checks, 
                                        cut_offs) %>% 
    dplyr::ungroup(.)
  
  passing_spectra <- summarized_checks %>% 
    dplyr::mutate(passed_spectra_curation = ifelse((passing_proportion > cut_off_prop) &
                                                     (sum_intensity > cut_off_sum_int),
                                                   TRUE,
                                                   FALSE),
                  reason_for_failure = dplyr::case_when(
                    passing_proportion < cut_off_prop & sum_intensity < cut_off_sum_int ~ "Proportion of passing analytes and sum intensity below cut-offs",
                    passing_proportion < cut_off_prop ~ "Proportion of passing analytes below cut-off",
                    sum_intensity < cut_off_sum_int ~ "Sum intensity below cut-off",
                    TRUE ~ ""
                  )) %>%
    dplyr::select(-tidyselect::any_of(c("type", 
                                        "sample_type_list")))
  
  curated_data <- dplyr::full_join(passing_spectra, 
                                   checked_data) %>% 
    dplyr::mutate(reason_for_failure = dplyr::case_when(
      is.na(absolute_intensity_background_subtracted) &
        is.na(mass_accuracy_ppm) &
        is.na(isotopic_pattern_quality) &
        is.na(sn) ~ "Empty line in LacyTools summary file",
      TRUE ~ reason_for_failure
    )) %>% 
    dplyr::relocate(c(passed_spectra_curation, reason_for_failure), 
                    .after = sample_name) %>% 
    dplyr::relocate(c(analyte_meets_criteria,
                      failed_criteria),
                    .after = charge) %>% 
    dplyr::relocate(tidyselect::any_of(c("sample_id", "plate_well")),
                    .after = sample_name)
  
  no_NAs <- curated_data %>% 
    dplyr::filter(dplyr::if_all(.cols = c(mass_accuracy_ppm,
                                          isotopic_pattern_quality,
                                          sn),
                                .fns = ~ !is.na(.x)))
  
  if (all(no_NAs$passed_spectra_curation == FALSE)) {
    warning("None of the spectra passed curation.")
  } else {
    if (all(no_NAs$passed_spectra_curation == TRUE)) {
      warning("All spectra passed curation.")
    }
  }
  
  return(curated_data)
}

# check_spectra <- function(data, min_ppm_deviation, max_ppm_deviation, 
#                             max_ipq, min_sn, qcs_to_consider) {
#   checked_data <- check_analyte_quality_criteria(data = data, 
#                                     min_ppm_deviation = min_ppm_deviation,
#                                     max_ppm_deviation = max_ppm_deviation,
#                                     max_ipq = max_ipq,
#                                     min_sn = min_sn,
#                                     qcs_to_consider = qcs_to_consider)
#   spectra_check <- summarize_spectra_checks(checked_data)
#     
#   return(spectra_check)
# }

# calculate_cut_offs_per_type <- function(checked_spectra) {
#   
#   grouping_variables <- c("group", "cluster", "sample_type")
#   
#   cut_offs <- checked_spectra %>%  
#     dplyr::group_by(dplyr::across(tidyselect::any_of(grouping_variables))) %>% 
#     dplyr::summarise(av_prop = mean(passing_proportion, na.rm = FALSE),
#                      sd_prop = sd_p(passing_proportion, na.rm = FALSE),
#                      cut_off_prop = av_prop + (3 * sd_prop),
#                      av_sum_int = mean(sum_intensity, na.rm = FALSE),
#                      sd_sum_int = sd_p(sum_intensity, na.rm = FALSE),
#                      cut_off_sum_int = av_sum_int + (3 * sd_sum_int))
#   return(cut_offs)
# }

filter_cut_off_basis <- function(cut_off_basis, data) {
  
  sample_types_to_filter <- stringr::str_extract(
    string = cut_off_basis,
    pattern = paste0(unique(data$sample_type),
                     collapse = "|")) %>% 
    na.omit(.)
  
  # if (any(!(sample_types_to_filter %in% data$sample_type))) {
  #   stop("One or more of the sample types in cut_off_basis are not present in the data.")
  # }
  
  if ("group" %in% colnames(data)) {
    
    groups_to_filter <- stringr::str_extract(
      string = cut_off_basis,
      pattern = paste0(unique(data$group),
                       collapse = "|")) %>% 
      na.omit(.)
    
    if (any(!(groups_to_filter %in% data$group))) {
      stop("One or more of the groups in cut_off_basis are not present in the data.")
    } 
    
    if (!rlang::is_empty(groups_to_filter)) {
      
      cut_off_basis_samples <- purrr::map2_dfr(
        groups_to_filter,
        sample_types_to_filter,
        function(group_to_filter, sample_type_to_filter) {
          data %>% 
            dplyr::filter(group == group_to_filter & sample_type == sample_type_to_filter)
        })
      
    } else {
      
      cut_off_basis_samples <- purrr::map_dfr(sample_types_to_filter,
                                              function(sample_type_to_filter) {
                                                data %>% 
                                                  dplyr::filter(sample_type == sample_type_to_filter)
                                              })
    }
  } else {
    
    cut_off_basis_samples <- purrr::map_dfr(sample_types_to_filter,
                                            function(sample_type_to_filter) {
                                              data %>% 
                                                dplyr::filter(sample_type == sample_type_to_filter)
                                            })
  }
  
  return(cut_off_basis_samples)
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
                                name = "Proportion of analytes that passed curation (%)")
  
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
plot_spectra_curation <- function(curated_data,
                                  Ig_data) {
  
  n_colors <- length(unique(curated_data$reason_for_failure)) - 1
  my_palette <- c(colorRampPalette(RColorBrewer::brewer.pal(8, "OrRd")[5:8])(n_colors),
                  "#3498DB")
  
  # Create nicer labels for reason_fo_failure to use in plot legend:
  reasons <- unique(curated_data$reason_for_failure)
  # Convert first letter to lower case:
  substr(reasons, 1, 1) <- tolower(substr(reasons, 1, 1))
  # Paste "No, " before the reason:
  reason_labels <- paste("No,", reasons) %>% 
    # Set the names to the 'old' values in the data, so that this named vector can
    # be used to recode reason_for_failure with nice labels that will appear in
    # the plot legend. 
    rlang::set_names(., nm = unique(
      # The empty value "" needs to be replaced by "none" because a name cannot be
      # empty:
      replace(curated_data$reason_for_failure,
              curated_data$reason_for_failure == "",
              "none")
    ))
  reason_labels[reason_labels == "No, "] <- "Yes"
  
  my_data <- curated_data %>% 
    dplyr::distinct(dplyr::across(tidyselect::any_of(c("group", 
                                                       "sample_type", 
                                                       "cluster", 
                                                       "sample_name", 
                                                       "passed_spectra_curation",
                                                       "reason_for_failure")))) %>% 
    dplyr::mutate(
      `Passed curation` = dplyr::case_when(
        passed_spectra_curation == "TRUE" ~ "Yes",
        passed_spectra_curation == "FALSE" ~ "No"
      ),
      # Replace the empty values "" in reason_for_failure with "none"  to match
      # the named vector reason_labels:
      reason_for_failure = replace(reason_for_failure,
                                   reason_for_failure == "",
                                   "none"),
      # Create a variable with nice name and labels to display in plot:
      `Passed curation?` = dplyr::recode(reason_for_failure, 
                                         # recode() cannot take named vectors as
                                         # an argument, so use !!! to 'unlist' it
                                         !!!reason_labels)
    ) %>% 
    dplyr::group_by(dplyr::across(tidyselect::any_of(c("group",
                                                       "cluster",
                                                       "sample_type",
                                                       "reason_for_failure")))) %>% 
    dplyr::mutate(
      number_true = length(passed_spectra_curation[passed_spectra_curation == "TRUE"]),
      number_false = length(passed_spectra_curation[passed_spectra_curation == "FALSE"])) %>% 
    dplyr::ungroup(reason_for_failure) %>% 
    dplyr::mutate(
      number = dplyr::case_when(
        passed_spectra_curation == "TRUE" ~ number_true,
        passed_spectra_curation == "FALSE" ~ number_false,
        TRUE ~ as.integer(NA)
      ),
      percentage = scales::label_percent(accuracy = 0.01)(number / dplyr::n())
    )
  
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
