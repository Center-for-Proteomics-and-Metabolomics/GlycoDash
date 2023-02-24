

#' Determine the choices for the repeatability sample menu
#'
#' This function finds the sample ID's or if applicable the combinations of
#' sample ID and group (total or specific) for which there are multiple
#' measurements in the data.
#'
#' @param normalized_data
#'
#' @return A list of character strings where each string contains "sample id: "
#'   followed by the sample ID and if applicable "group: " followed by the total
#'   or specific keyword. This list can be given as the \code{choices} argument
#'   to the function \code{\link{selectInput}}.
#' @export
#'
#' @examples
#' # First spectra curation has to be performed:
#' data("example_data")
#'
#' checked_data <- check_analyte_quality_criteria(my_data = example_data,
#'                                                min_ppm_deviation = -20,
#'                                                max_ppm_deviation = 20,
#'                                                max_ipq = 0.2,
#'                                                min_sn = 9,
#'                                                criteria_to_consider = c("Mass accuracy",
#'                                                                         "S/N",
#'                                                                         "IPQ"))
#'
#' summarized_checks <- summarize_spectra_checks(checked_data = checked_data)
#'
#' cut_offs_total <- calculate_cut_offs(summarized_checks = summarized_checks,
#'                                      control_sample_types = "PBS",
#'                                      exclude_sample_types = NULL,
#'                                      group_keyword = "Total",
#'                                      percentile = 97,
#'                                      use_mean_SD = FALSE,
#'                                      SD_factor = NULL,
#'                                      uncalibrated_as_NA = TRUE)
#'
#' cut_offs_specific <- calculate_cut_offs(summarized_checks = summarized_checks,
#'                                         control_sample_types = "PBS",
#'                                         exclude_sample_types = NULL,
#'                                         group_keyword = "Spike",
#'                                         percentile = 97,
#'                                         use_mean_SD = FALSE,
#'                                         SD_factor = NULL,
#'                                         uncalibrated_as_NA = TRUE)
#'
#' cut_offs <- dplyr::full_join(cut_offs_total,
#'                              cut_offs_specific)
#'
#' curated_spectra <- curate_spectra(checked_data = checked_data,
#'                                   summarized_checks = summarized_checks,
#'                                   cut_offs = cut_offs)
#'
#' passing_spectra <- kick_out_spectra(curated_spectra = curated_spectra)
#'
#' for_analyte_curation <- remove_unneeded_columns(passing_spectra = passing_spectra)
#'
#' # Then analyte curation is performed:
#' without_samples_to_ignore <- throw_out_samples(
#'    passing_spectra = for_analyte_curation,
#'    samples_to_ignore = c("PBS", "Visucon", "IVIGg", "Total")
#' )
#'
#' checked_analytes <- check_analyte_quality_criteria(my_data = without_samples_to_ignore,
#'                                                    min_ppm_deviation = -20,
#'                                                    max_ppm_deviation = 20,
#'                                                    max_ipq = 0.2,
#'                                                    min_sn = 9,
#'                                                    criteria_to_consider = c("Mass accuracy",
#'                                                                             "S/N",
#'                                                                             "IPQ"))
#'
#' curated_analytes <- curate_analytes(checked_analytes = checked_analytes,
#'                                     cut_off_percentage = 25)
#'
#' analyte_curated_data <- dplyr::full_join(curated_analytes,
#'                                          for_analyte_curation) %>%
#'    dplyr::filter(has_passed_analyte_curation) %>%
#'    dplyr::select(-c(has_passed_analyte_curation, passing_percentage))
#'
#' # Then we calculate the total intensities for each analyte:
#' total_intensities <- calculate_total_intensity(analyte_curated_data)
#'
#' # And then we can perform total area normalization:
#' normalized_data <- normalize_data(total_intensities)
#'
#' find_choices_for_repeatability_menu(normalized_data)
find_choices_for_repeatability_menu <- function(normalized_data) {
  
  menu_df <- normalized_data %>% 
    dplyr::select(tidyselect::any_of(c("sample_name", "group", "sample_id"))) %>%
    dplyr::distinct() %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of("group"))) %>% 
    dplyr::add_count(sample_id, name = "number_of_replicates_after_curation") %>% 
    dplyr::mutate(number_of_replicates_after_curation = ifelse(
      sample_id == "empty cell in plate design",
      1,
      number_of_replicates_after_curation
    )) %>% 
    dplyr::filter(number_of_replicates_after_curation > 1) %>% 
    dplyr::distinct(dplyr::across(tidyselect::any_of(c("group", "sample_id")))) 
  
  if ("group" %in% colnames(menu_df)) {
    choices <- purrr::pmap(menu_df,
                           function(group, sample_id) {
                             paste("group:", group, "sample_id:", sample_id)
                           })
  } else {
    choices <- paste("sample_id:", menu_df$sample_id)
  }
  
  return(choices)
}

#' Calculate the RSD and mean of standards across a plate.
#'
#' @param data A dataframe with the curated (and normalized) data.
#' @param standard_sample_id The sample ID of the replicates that the RSD's
#'   should be calculated for.
#' @param standard_group The group (total or specific Ig) that the RSD's should
#'   be calculated for.
#'
#' @return A tibble with 6 columns: \describe{\item{plate}{The plate number.}
#'   \item{analyte}{The analyte.} \item{average_abundance}{The mean relative
#'   abundance of that analyte on that plate.} \item{RSD}{The relative standard
#'   deviation of the relative abundance of that analyte on that plate.}
#'   \item{n}{The number of samples that the mean and RSD were based on.}
#'   \item{cluster}{The cluster that the analyte belongs to.}}
#'
#' @export
#'
#' @examples
#' # First spectra curation has to be performed:
#' data("example_data")
#'
#' checked_data <- check_analyte_quality_criteria(my_data = example_data,
#'                                                min_ppm_deviation = -20,
#'                                                max_ppm_deviation = 20,
#'                                                max_ipq = 0.2,
#'                                                min_sn = 9,
#'                                                criteria_to_consider = c("Mass accuracy",
#'                                                                         "S/N",
#'                                                                         "IPQ"))
#'
#' summarized_checks <- summarize_spectra_checks(checked_data = checked_data)
#'
#' cut_offs_total <- calculate_cut_offs(summarized_checks = summarized_checks,
#'                                      control_sample_types = "PBS",
#'                                      exclude_sample_types = NULL,
#'                                      group_keyword = "Total",
#'                                      percentile = 97,
#'                                      use_mean_SD = FALSE,
#'                                      SD_factor = NULL,
#'                                      uncalibrated_as_NA = TRUE)
#'
#' cut_offs_specific <- calculate_cut_offs(summarized_checks = summarized_checks,
#'                                         control_sample_types = "PBS",
#'                                         exclude_sample_types = NULL,
#'                                         group_keyword = "Spike",
#'                                         percentile = 97,
#'                                         use_mean_SD = FALSE,
#'                                         SD_factor = NULL,
#'                                         uncalibrated_as_NA = TRUE)
#'
#' cut_offs <- dplyr::full_join(cut_offs_total,
#'                              cut_offs_specific)
#'
#' curated_spectra <- curate_spectra(checked_data = checked_data,
#'                                   summarized_checks = summarized_checks,
#'                                   cut_offs = cut_offs)
#'
#' passing_spectra <- kick_out_spectra(curated_spectra = curated_spectra)
#'
#' for_analyte_curation <- remove_unneeded_columns(passing_spectra = passing_spectra)
#'
#' # Then analyte curation is performed:
#' without_samples_to_ignore <- throw_out_samples(
#'    passing_spectra = for_analyte_curation,
#'    samples_to_ignore = c("PBS", "Visucon", "IVIGg", "Total")
#' )
#'
#' checked_analytes <- check_analyte_quality_criteria(my_data = without_samples_to_ignore,
#'                                                    min_ppm_deviation = -20,
#'                                                    max_ppm_deviation = 20,
#'                                                    max_ipq = 0.2,
#'                                                    min_sn = 9,
#'                                                    criteria_to_consider = c("Mass accuracy",
#'                                                                             "S/N",
#'                                                                             "IPQ"))
#'
#' curated_analytes <- curate_analytes(checked_analytes = checked_analytes,
#'                                     cut_off_percentage = 25)
#'
#' analyte_curated_data <- dplyr::full_join(curated_analytes,
#'                                          for_analyte_curation) %>%
#'    dplyr::filter(has_passed_analyte_curation) %>%
#'    dplyr::select(-c(has_passed_analyte_curation, passing_percentage))
#'
#' # Then we calculate the total intensities for each analyte:
#' total_intensities <- calculate_total_intensity(analyte_curated_data)
#'
#' # And then we can perform total area normalization:
#' normalized_data <- normalize_data(total_intensities)
#'
#' calculate_repeatability_stats(data = normalized_data,
#'                               standard_sample_id = "pool",
#'                               standard_group = "Spike")
calculate_repeatability_stats <- function(data,
                                          standard_sample_id,
                                          standard_group) {
  if (is.null(standard_group)) {
    repeatability <- data %>% 
      dplyr::filter(sample_id %in% standard_sample_id)
  } else {
    repeatability <- data %>% 
      dplyr::filter(sample_id %in% standard_sample_id & group == standard_group)
  }
  
  if (all(purrr::map_lgl(repeatability, rlang::is_empty))) {
    rlang::abort(class = "no_samples",
                 message = paste("There are no samples of this sample_id",
                                 ifelse(is.null(standard_group), "", "and group"),
                                 "that passed curation. Please choose a different", 
                                 "standard to assess."))
  }
  
  repeatability <- repeatability %>% 
    dplyr::mutate(plate = stringr::str_extract(plate_well, "^[A-Z]|\\d+")) %>% 
    dplyr::group_by(plate, analyte) %>% 
    dplyr::summarise(average_abundance = mean(relative_abundance),
                     RSD = sd(relative_abundance) / average_abundance,
                     n = dplyr::n(),
                     dplyr::across(cluster)) %>% 
    dplyr::ungroup(.)
  
  return(repeatability)
}

#' Create a plot showing the repeatability per plate
#'
#' This function creates a dodged barplot with a bar for each plate, the
#' relative abundance in percent on the y-axis and the analytes on the x-axis.
#' The plot is faceted by cluster. On top of each bar a point is shown that
#' correspond to the relative standard deviation (RSD) in percent.
#'
#' @param repeatability_data The return value of
#'   \code{\link{calculate_repeatability_stats}}.
#' @param selected_group A character string indicating if the repeatability of
#'   the total or of the specific samples should be shown. Default is NULL.
#' @param selected_sample_id A character string indicating what sample type
#'   should be shown in the plot.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' # First spectra curation has to be performed:
#' data("example_data")
#'
#' checked_data <- check_analyte_quality_criteria(my_data = example_data,
#'                                                min_ppm_deviation = -20,
#'                                                max_ppm_deviation = 20,
#'                                                max_ipq = 0.2,
#'                                                min_sn = 9,
#'                                                criteria_to_consider = c("Mass accuracy",
#'                                                                         "S/N",
#'                                                                         "IPQ"))
#'
#' summarized_checks <- summarize_spectra_checks(checked_data = checked_data)
#'
#' cut_offs_total <- calculate_cut_offs(summarized_checks = summarized_checks,
#'                                      control_sample_types = "PBS",
#'                                      exclude_sample_types = NULL,
#'                                      group_keyword = "Total",
#'                                      percentile = 97,
#'                                      use_mean_SD = FALSE,
#'                                      SD_factor = NULL,
#'                                      uncalibrated_as_NA = TRUE)
#'
#' cut_offs_specific <- calculate_cut_offs(summarized_checks = summarized_checks,
#'                                         control_sample_types = "PBS",
#'                                         exclude_sample_types = NULL,
#'                                         group_keyword = "Spike",
#'                                         percentile = 97,
#'                                         use_mean_SD = FALSE,
#'                                         SD_factor = NULL,
#'                                         uncalibrated_as_NA = TRUE)
#'
#' cut_offs <- dplyr::full_join(cut_offs_total,
#'                              cut_offs_specific)
#'
#' curated_spectra <- curate_spectra(checked_data = checked_data,
#'                                   summarized_checks = summarized_checks,
#'                                   cut_offs = cut_offs)
#'
#' passing_spectra <- kick_out_spectra(curated_spectra = curated_spectra)
#'
#' for_analyte_curation <- remove_unneeded_columns(passing_spectra = passing_spectra)
#'
#' # Then analyte curation is performed:
#' without_samples_to_ignore <- throw_out_samples(
#'    passing_spectra = for_analyte_curation,
#'    samples_to_ignore = c("PBS", "Visucon", "IVIGg", "Total")
#' )
#'
#' checked_analytes <- check_analyte_quality_criteria(my_data = without_samples_to_ignore,
#'                                                    min_ppm_deviation = -20,
#'                                                    max_ppm_deviation = 20,
#'                                                    max_ipq = 0.2,
#'                                                    min_sn = 9,
#'                                                    criteria_to_consider = c("Mass accuracy",
#'                                                                             "S/N",
#'                                                                             "IPQ"))
#'
#' curated_analytes <- curate_analytes(checked_analytes = checked_analytes,
#'                                     cut_off_percentage = 25)
#'
#' analyte_curated_data <- dplyr::full_join(curated_analytes,
#'                                          for_analyte_curation) %>%
#'    dplyr::filter(has_passed_analyte_curation) %>%
#'    dplyr::select(-c(has_passed_analyte_curation, passing_percentage))
#'
#' # Then we calculate the total intensities for each analyte:
#' total_intensities <- calculate_total_intensity(analyte_curated_data)
#'
#' # And then we can perform total area normalization:
#' normalized_data <- normalize_data(total_intensities)
#'
#' repeatability_stats <- calculate_repeatability_stats(data = normalized_data,
#'                               standard_sample_id = "pool",
#'                               standard_group = "Spike")
#' 
#' visualize_repeatability(repeatability_data = repeatability_stats,
#'                         selected_group = "Spike",
#'                         selected_sample_id = "pool")
visualize_repeatability <- function(repeatability_data,
                                    selected_group = NULL,
                                    selected_sample_id) {
  # # This code was needed when I made a second axis with the RSD, but because 
  # # plotly didn't work with a second axis it's no longer needed.
  # range_av_abundance <- max(repeatability_data$average_abundance,
  #                           na.rm = TRUE) - min(repeatability_data$average_abundance,
  #                                               na.rm = TRUE)
  # range_RSD <- max(repeatability_data$RSD,
  #                  na.rm = TRUE) - min(repeatability_data$RSD,
  #                                      na.rm = TRUE)
  # rescale_RSD_with <- range_av_abundance / range_RSD
  
  if (nrow(repeatability_data) == 0) {
    rlang::abort(
      class = "all_NAs",
      message = paste0("For all",
                       ifelse(!is.null(selected_group),
                              paste0(" ", selected_group),
                              ""),
                       " samples with sample ID ",
                       selected_sample_id,
                       " spectra curation or calibration for this cluster ",
                       "has failed, so there is no data to show.")
    )
  }
  
  n_colors <- length(unique(repeatability_data$plate))
  my_palette <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_colors)
  
  plot <- repeatability_data %>% 
    # # Code needed for second axis:
    #dplyr::mutate(RSD = RSD * rescale_RSD_with) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = analyte, 
                                   y = average_abundance, 
                                   fill = plate,
                                   text = paste("Analyte:",
                                                analyte,
                                                "\nPlate:",
                                                plate,
                                                "\nAverage relative abundance:",
                                                signif(average_abundance, 3),
                                                "%",
                                                "\nNumber of samples (n):",
                                                n)),
                      position = "dodge") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1),
                   panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5)) +
    ggplot2::geom_point(ggplot2::aes(x = analyte,
                                     y = RSD,
                                     group = plate,
                                     fill = plate,
                                     text = paste("Analyte:",
                                                  analyte,
                                                  "\nPlate:",
                                                  plate,
                                                  "\nRSD:",
                                                  signif(RSD, 3),
                                                  "%"
                                     )),
                      shape = 21,
                      stroke = 0.5,
                      position = ggplot2::position_dodge(width = .9),
                      ) +
  ggplot2::scale_y_continuous(name = "Relative abundance (%)",
                              labels = function(x) paste0(x, "%")#,
                              # # Code needed for second axis:
                              # sec.axis = ggplot2::sec_axis(~ . / rescale_RSD_with, 
                              #                              name = "RSD (%)",
                              #                              labels = function(x) paste0(x,
                              #                                                          "%"))
  ) +
    ggplot2::scale_x_discrete(name = "Analyte") +
    ggplot2::scale_fill_manual(values = my_palette, 
                               name = "Plate") +
    ggplot2::facet_wrap(~cluster, scales = "free_x")
  
  return(plot)
}

#' Create a plot showing the repeatability for all plates together
#'
#' This function creates a bar plot with the relative abundance in percent on
#' the y-axis and the analytes on the x-axis. On top of the bars points are
#' shown that correspond to the relative standard deviation (RSD). The plot is
#' faceted by cluster. Error bars show the standard deviations.
#'
#' @inheritParams visualize_repeatability
#' @param data A dataframe with the curated (and normalized) data.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' # First spectra curation has to be performed:
#' data("example_data")
#'
#' checked_data <- check_analyte_quality_criteria(my_data = example_data,
#'                                                min_ppm_deviation = -20,
#'                                                max_ppm_deviation = 20,
#'                                                max_ipq = 0.2,
#'                                                min_sn = 9,
#'                                                criteria_to_consider = c("Mass accuracy",
#'                                                                         "S/N",
#'                                                                         "IPQ"))
#'
#' summarized_checks <- summarize_spectra_checks(checked_data = checked_data)
#'
#' cut_offs_total <- calculate_cut_offs(summarized_checks = summarized_checks,
#'                                      control_sample_types = "PBS",
#'                                      exclude_sample_types = NULL,
#'                                      group_keyword = "Total",
#'                                      percentile = 97,
#'                                      use_mean_SD = FALSE,
#'                                      SD_factor = NULL,
#'                                      uncalibrated_as_NA = TRUE)
#'
#' cut_offs_specific <- calculate_cut_offs(summarized_checks = summarized_checks,
#'                                         control_sample_types = "PBS",
#'                                         exclude_sample_types = NULL,
#'                                         group_keyword = "Spike",
#'                                         percentile = 97,
#'                                         use_mean_SD = FALSE,
#'                                         SD_factor = NULL,
#'                                         uncalibrated_as_NA = TRUE)
#'
#' cut_offs <- dplyr::full_join(cut_offs_total,
#'                              cut_offs_specific)
#'
#' curated_spectra <- curate_spectra(checked_data = checked_data,
#'                                   summarized_checks = summarized_checks,
#'                                   cut_offs = cut_offs)
#'
#' passing_spectra <- kick_out_spectra(curated_spectra = curated_spectra)
#'
#' for_analyte_curation <- remove_unneeded_columns(passing_spectra = passing_spectra)
#'
#' # Then analyte curation is performed:
#' without_samples_to_ignore <- throw_out_samples(
#'    passing_spectra = for_analyte_curation,
#'    samples_to_ignore = c("PBS", "Visucon", "IVIGg", "Total")
#' )
#'
#' checked_analytes <- check_analyte_quality_criteria(my_data = without_samples_to_ignore,
#'                                                    min_ppm_deviation = -20,
#'                                                    max_ppm_deviation = 20,
#'                                                    max_ipq = 0.2,
#'                                                    min_sn = 9,
#'                                                    criteria_to_consider = c("Mass accuracy",
#'                                                                             "S/N",
#'                                                                             "IPQ"))
#'
#' curated_analytes <- curate_analytes(checked_analytes = checked_analytes,
#'                                     cut_off_percentage = 25)
#'
#' analyte_curated_data <- dplyr::full_join(curated_analytes,
#'                                          for_analyte_curation) %>%
#'    dplyr::filter(has_passed_analyte_curation) %>%
#'    dplyr::select(-c(has_passed_analyte_curation, passing_percentage))
#'
#' # Then we calculate the total intensities for each analyte:
#' total_intensities <- calculate_total_intensity(analyte_curated_data)
#'
#' # And then we can perform total area normalization:
#' normalized_data <- normalize_data(total_intensities)
#'
#' visualize_repeatability_mean_bars(data = normalized_data,
#'                                   selected_group = "Spike",
#'                                   selected_sample_id = "pool")
visualize_repeatability_mean_bars <- function(data,
                                              selected_sample_id,
                                              selected_group = NULL) {
  to_plot <- data %>% 
    dplyr::filter(sample_id == selected_sample_id)
  
  if (!is.null(selected_group)) {
    to_plot <- to_plot %>% 
      dplyr::filter(group == selected_group)
  }
  
  to_plot <- to_plot %>% 
    dplyr::group_by(analyte) %>% 
    na.omit(cols = "relative_abundance") %>% 
    dplyr::summarize(mean_rel_ab = mean(relative_abundance),
                     sd_rel_ab = sd(relative_abundance),
                     rsd = sd_rel_ab / mean_rel_ab,
                     n = dplyr::n(),
                     dplyr::across()) %>% 
    dplyr::ungroup()
  
  if (nrow(to_plot) == 0) {
    rlang::abort(
      class = "all_NAs",
      message = paste0("For all",
                       ifelse(!is.null(selected_group),
                              paste0(" ", selected_group),
                              ""),
                       " samples with sample ID ",
                       selected_sample_id,
                       " spectra curation or calibration has failed for this cluster, ",
                       "so there is no data to show.")
    )
  }
  
  n_colors <- length(unique(to_plot$analyte))
  my_palette <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_colors)
  
  mean_barplot <- ggplot2::ggplot(to_plot) +
    ggplot2::geom_col(ggplot2::aes(x = analyte, 
                                   y = mean_rel_ab,
                                   text = paste("Analyte:",
                                                analyte,
                                                "\nMean of relative abundance:",
                                                signif(mean_rel_ab, 3),
                                                "%",
                                                "\nNumber of samples (n):",
                                                n),
                                   fill = analyte),
                      position = "dodge",
                      color = "black") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = mean_rel_ab - sd_rel_ab,
                                        ymax = mean_rel_ab + sd_rel_ab,
                                        x = analyte,
                                        text = paste("Standard deviation:",
                                                     signif(sd_rel_ab, 3),
                                                     "%")),
                           width = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = analyte, 
                                     y = rsd,
                                     fill = analyte,
                                     text = paste("RSD:",
                                                  signif(rsd, 3),
                                                  "%")),
                        shape = 21,
                        stroke = 0.5) +
    ggplot2::scale_fill_manual(values = my_palette) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1),
                   panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5),
                   legend.position = "none") +
    ggplot2::scale_y_continuous(name = "Relative abundance (%)",
                                labels = function(x) paste0(x, "%")) +
    ggplot2::scale_x_discrete(name = "Analyte") +
    ggplot2::facet_wrap(~ cluster, scales = "free_x")
  
  return(mean_barplot)
  
}
