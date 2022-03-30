#' Create a plot that visualizes the repeatability
#'
#' @param repeatability_data The dataframe that is returned by the
#'   \code{\link{calculate_repeatability_stats}} function.
#' @param second_axis_rescale_with The factor used to rescale the RSD's.
#'
#' @return
#' @export
#'
#' @examples
visualize_repeatability <- function(repeatability_data) {
  
  range_av_abundance <- max(repeatability_data$average_abundance,
                            na.rm = TRUE) - min(repeatability_data$average_abundance,
                                                na.rm = TRUE)
  range_RSD <- max(repeatability_data$RSD,
                   na.rm = TRUE) - min(repeatability_data$RSD,
                                       na.rm = TRUE)
  rescale_RSD_with <- range_av_abundance / range_RSD
  
  plot <- repeatability_data %>% 
    dplyr::mutate(RSD = RSD * rescale_RSD_with) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = analyte, 
                                   y = average_abundance, 
                                   fill = plate),
                      position = "dodge") +
    ggplot2::theme_classic() +
    ggpubr::border(size = 0.5) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1),
                   text = ggplot2::element_text(size = 16)) +
    ggplot2::geom_point(ggplot2::aes(x = analyte, 
                                     y = RSD,
                                     group = plate,
                                     fill = plate),
                        shape = 21,
                        stroke = 1,
                        position = ggplot2::position_dodge(width = .9),
                        show.legend = FALSE) +
    ggplot2::scale_y_continuous(name = "Relative abundance (%)",
                                labels = function(x) paste0(x, "%"),
                                sec.axis = ggplot2::sec_axis(~ . / rescale_RSD_with, 
                                                             name = "RSD (%)",
                                                             labels = function(x) paste0(x,
                                                                                         "%"))) +
    ggplot2::scale_x_discrete(name = "Analyte") +
    ggplot2::scale_fill_brewer(palette = "Set2", 
                               name = "Plate")
  
  return(plot)
}

#' Calculate the RSD of standards across a plate.
#'
#' @param data A dataframe with the curated (and normalized) data.
#' @param standard_sample_type The sample type of the standards that the RSD's
#'   should be calculated for.
#' @param standard_group The group (total or specific Ig) that the RSD's should
#'   be calculated for.
#'
#' @return
#' @export
#'
#' @examples
calculate_repeatability_stats <- function(data,
                                          standard_sample_type,
                                          standard_group) {
  if (is.null(standard_group)) {
    repeatability <- data %>% 
      dplyr::filter(sample_type %in% standard_sample_type)
  } else {
    repeatability <- data %>% 
      dplyr::filter(sample_type %in% standard_sample_type & group == standard_group)
  }
  
  if (all(purrr::map_lgl(repeatability, rlang::is_empty))) {
    rlang::abort(class = "no_samples",
                 message = paste("There are no samples of this sample_type",
                                 ifelse(is.null(standard_group), "", "and group"),
                                 "that passed curation. Please choose a different", 
                                 "standard to assess."))
  }
  
  repeatability <- repeatability %>% 
    dplyr::mutate(plate = stringr::str_extract(plate_well, "^[A-Z]|\\d+")) %>% 
    dplyr::group_by(plate, analyte)
  
  repeatability <- repeatability %>% 
    dplyr::summarise(average_abundance = mean(relative_abundance * 100),
                     RSD = sd(relative_abundance * 100) / average_abundance * 100)
  
}
