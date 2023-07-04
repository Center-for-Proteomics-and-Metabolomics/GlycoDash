#' Create a box plot
#'
#' This function creates a box plot with points overlaid on top.
#'
#' @param data The normalized data in a wide format (every analyte has its own
#'   column).
#' @param xvar The variable that should be shown on the x-axis.
#' @param yvar The variable that should be shown on the y-axis.
#' @param color The variable that should correspond to the colors in the plot.
#'   If \code{color} is NULL no variable will be linked to the colors (default).
#' @param facets The variable that should be used to facet the plot. If
#'   \code{facets} is NULL the plot will not be faceted (default).
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
#' normalized_data_wide <- normalized_data %>% 
#' # Removing columns with values that differ between clusters: 
#'  dplyr::select(-tidyselect::any_of(c("passing_analyte_percentage",
#'                                      "cut_off_passing_analyte_percentage", 
#'                                      "cut_off_sum_intensity"))) %>% 
#'  tidyr::pivot_wider(names_from = c(cluster, analyte),
#'                     names_sep = "_",
#'                     values_from = relative_abundance)
#' 
#' my_boxplot(data = normalized_data_wide,
#'            xvar = "sample_type",
#'            yvar = "IgGI_IgGI1H3N4F1",
#'            color = "sample_type",
#'            facets = "group")
my_boxplot <- function(data, xvar, yvar, color = NULL, facets = NULL) {
  
  plot <- data %>% 
    ggplot2::ggplot(
      ggplot2::aes(
        text = paste0(
          "\nSample name: ",
          sample_name,
          "\nSample type: ",
          sample_type,
          "\n",
          nicer_label(yvar),
          ": ",
          .data[[yvar]]
        )
      )
    ) +
    ggplot2::geom_boxplot(ggplot2::aes(x = .data[[xvar]],
                                       y = .data[[yvar]]),
                          outlier.shape = NA) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1)) +
    ggplot2::labs(x = nicer_label(xvar),
                  y = nicer_label(yvar))
  
  if (!is.null(color)) {
    n_colors <- length(unique(data[[color]]))
    # my_palette <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_colors)
    my_palette <- color_palette(n_colors)
    
    plot <- plot +
      ggplot2::geom_jitter(ggplot2::aes(x = .data[[xvar]],
                                        y = .data[[yvar]],
                                        color = .data[[color]]),
                           height = 0,
                           width = 0.25,
                           alpha = 0.6) +
      {
        # In case of continuous color variable
        if (is.numeric(data[[color]]) && !is.integer(data[[color]])) {
          ggplot2::scale_color_continuous(type = "viridis")
        } else {
          ggplot2::scale_color_manual(values = my_palette,
                                      name = nicer_label(color))
        }
      }
    
  } else {
    plot <- plot +
      ggplot2::geom_jitter(ggplot2::aes(x = .data[[xvar]],
                                        y = .data[[yvar]]),
                           height = 0,
                           width = 0.25,
                           color = "#1f77b4",
                           # color = RColorBrewer::brewer.pal(3, "Set2")[1],
                           alpha = 0.6)
  }
  
  if (!is.null(facets)) {
    plot <- plot +
      ggplot2::facet_wrap(facets)
  }
  return(plot)
}

#' Create a scatter plot 
#'
#' @inheritParams my_boxplot 
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
#' normalized_data_wide <- normalized_data %>% 
#' # Removing columns with values that differ between clusters: 
#'  dplyr::select(-tidyselect::any_of(c("passing_analyte_percentage",
#'                                      "cut_off_passing_analyte_percentage", 
#'                                      "cut_off_sum_intensity"))) %>% 
#'  tidyr::pivot_wider(names_from = c(cluster, analyte),
#'                     names_sep = "_",
#'                     values_from = relative_abundance)
#' 
#' my_scatter_plot(data = normalized_data_wide,
#'                 xvar = "IgGI_IgGI1H3N4",
#'                 yvar = "IgGI_IgGI1H3N4F1",
#'                 color = "sample_type",
#'                 facets = "group")
my_scatter_plot <- function(data, xvar, yvar, color = NULL, facets = NULL) {
  
  plot <- data %>% 
    ggplot2::ggplot(
      ggplot2::aes(
        text = paste0(
          "\nSample name: ",
          sample_name,
          "\nSample type: ",
          sample_type,
          "\n",
          nicer_label(yvar),
          ": ",
          .data[[yvar]]
        )
      )
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1)) +
    ggplot2::labs(x = nicer_label(xvar),
                  y = nicer_label(yvar))
  
  if (!is.null(color)) {
    n_colors <- length(unique(data[[color]]))
    # my_palette <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_colors)
    my_palette <- color_palette(n_colors)

    plot <- plot +
      ggplot2::geom_point(ggplot2::aes(x = as.numeric(.data[[xvar]]),
                                       y = as.numeric(.data[[yvar]]),
                                       color = .data[[color]])) +
      {
        if (is.numeric(data[[color]]) && !is.integer(data[[color]])) {
          ggplot2::scale_color_continuous(type = "viridis")
        } else {
          ggplot2::scale_color_manual(values = my_palette,
                                      name = nicer_label(color))
        }
      }
    
  } else {
    plot <- plot +
      ggplot2::geom_point(ggplot2::aes(x = as.numeric(.data[[xvar]]),
                                       y = as.numeric(.data[[yvar]])),
                          color = "#1f77b4")
                          # color = RColorBrewer::brewer.pal(3, "Set2")[1])
  }
  
  if (!is.null(facets)) {
    plot <- plot +
      ggplot2::facet_wrap(facets)
  }
  
  return(plot)
  
}

#' Create a histogram
#'
#' @inheritParams my_boxplot
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
#' normalized_data_wide <- normalized_data %>% 
#' # Removing columns with values that differ between clusters: 
#'  dplyr::select(-tidyselect::any_of(c("passing_analyte_percentage",
#'                                      "cut_off_passing_analyte_percentage", 
#'                                      "cut_off_sum_intensity"))) %>% 
#'  tidyr::pivot_wider(names_from = c(cluster, analyte),
#'                     names_sep = "_",
#'                     values_from = relative_abundance)
#' 
#' my_histogram(data = normalized_data_wide,
#'              xvar = "IgGI_IgGI1H3N4",
#'              color = "sample_type",
#'              facets = "group")
my_histogram <- function(data, xvar = NULL, color = NULL, facets = NULL) {
  
  plot <- data %>% 
    ggplot2::ggplot(
      ggplot2::aes(
        text = paste0(
          "Number of samples: ",
          ggplot2::after_stat(count),
          "\n",
          nicer_label(xvar),
          ": ",
          signif(xmin, 3),
          " to ",
          signif(xmax, 3)
        )
      )
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1)) +
      ggplot2::labs(x = nicer_label(xvar))
  
  if (!is.null(color)) {
    n_colors <- length(unique(data[[color]]))
    # my_palette <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_colors)
    my_palette <- color_palette(n_colors)
    
    plot <- plot +
      ggplot2::geom_histogram(ggplot2::aes(x = .data[[xvar]],
                                           fill = .data[[color]])) +
      {
        if (is.numeric(data[[color]]) && !is.integer(data[[color]])) {
          ggplot2::scale_fill_continuous(type = "viridis")
        } else {
          ggplot2::scale_fill_manual(values = my_palette,
                                     name = nicer_label(color))
        }
      }
    
  } else {
    plot <- plot +
      ggplot2::geom_histogram(ggplot2::aes(x = .data[[xvar]]),
                              fill = "#1f77b4")
                              # fill = RColorBrewer::brewer.pal(3, "Set2")[1])
  }
  
  if (!is.null(facets)) {
    plot <- plot +
      ggplot2::facet_wrap(facets)
  }
  
  return(plot)
  
}

# nicer_label is a helper function that is used in the plotting functions above:
nicer_label <- function(varname) {
  
  firstupper(stringr::str_replace_all(
    string = varname,
    # If the cluster prefix is the same as the start of the 
    # analyte name, remove it from the axis title:
    c("(.+)_\\1(.+)" = "\\1 \\2",
      # Replace any underscores with white spaces:
      "_" = " ")
  ))
  
}

