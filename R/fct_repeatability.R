#' Create a plot that visualizes the repeatability
#'
#' @param repeatability_data The dataframe that is returned by the
#'   \code{\link{calculate_repeatability_stats}} function.
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
                                   fill = plate,
                                   text = paste("Average relative abundance:",
                                                signif(average_abundance, 3),
                                                "%")),
                      position = "dodge") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1),
                   #text = ggplot2::element_text(size = 16),
                   panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5)) +
    # ggplot2::geom_point(ggplot2::aes(x = analyte, 
    #                                  y = RSD,
    #                                  group = plate,
    #                                  fill = plate,
    #                                  text = paste("RSD:",
    #                                                signif(RSD, 3),
    #                                                "%")),
    #                     shape = 21,
    #                     stroke = 0.5,
    #                     position = ggplot2::position_dodge(width = .9),
    #                     #show.legend = FALSE
    #                     ) +
    ggplot2::scale_y_continuous(name = "Relative abundance (%)",
                                labels = function(x) paste0(x, "%")#,
                                # sec.axis = ggplot2::sec_axis(~ . / rescale_RSD_with, 
                                #                              name = "RSD (%)",
                                #                              labels = function(x) paste0(x,
                                #                                                          "%"))
                                ) +
    ggplot2::scale_x_discrete(name = "Analyte") +
    ggplot2::scale_fill_brewer(palette = "Set2", 
                               name = "Plate") +
    ggplot2::facet_wrap(~cluster)
  
  return(plot)
}

#' Calculate the RSD of standards across a plate.
#'
#' @param data A dataframe with the curated (and normalized) data.
#' @param standard_sample_id The sample type of the standards that the RSD's
#'   should be calculated for.
#' @param standard_group The group (total or specific Ig) that the RSD's should
#'   be calculated for.
#'
#' @return
#' @export
#'
#' @examples
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
    dplyr::summarise(average_abundance = mean(relative_abundance * 100),
                     RSD = sd(relative_abundance * 100) / average_abundance * 100,
                     n = dplyr::n(),
                     across(cluster)) %>% 
    dplyr::ungroup(.)
  
}

visualize_repeatability2 <- function(repeatability_data) {
  
  # range_av_abundance <- max(repeatability_data$average_abundance,
  #                           na.rm = TRUE) - min(repeatability_data$average_abundance,
  #                                               na.rm = TRUE)
  # range_RSD <- max(repeatability_data$RSD,
  #                  na.rm = TRUE) - min(repeatability_data$RSD,
  #                                      na.rm = TRUE)
  # rescale_RSD_with <- range_av_abundance / range_RSD
  
  plot <- repeatability_data %>% 
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
                   #text = ggplot2::element_text(size = 16),
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
                      #show.legend = FALSE
                      ) +
  ggplot2::scale_y_continuous(name = "Relative abundance (%)",
                              labels = function(x) paste0(x, "%")#,
                              # sec.axis = ggplot2::sec_axis(~ . / rescale_RSD_with, 
                              #                              name = "RSD (%)",
                              #                              labels = function(x) paste0(x,
                              #                                                          "%"))
  ) +
    ggplot2::scale_x_discrete(name = "Analyte") +
    ggplot2::scale_fill_brewer(palette = "Set2", 
                               name = "Plate") +
    ggplot2::facet_wrap(~cluster, scales = "free_x")
  
  return(plot)
}

visualize_repeatability_dogded_bars <- function(data,
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
    dplyr::summarize(mean_rel_ab = mean(relative_abundance),
                     sd_rel_ab = sd(relative_abundance),
                     rsd = sd_rel_ab / mean_rel_ab,
                     n = dplyr::n(),
                     dplyr::across()) %>% 
    dplyr::ungroup()
  
  n_colors <- length(unique(to_plot$sample_name))
  my_palette <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_colors)
  
  dodged_barplot <- ggplot2::ggplot(to_plot) +
    ggplot2::geom_col(ggplot2::aes(x = analyte, 
                                   y = relative_abundance,
                                   fill = sample_name,
                                   text = paste("Analyte:",
                                                analyte,
                                                "\nSample name:",
                                                sample_name,
                                                "\nRelative abundance:",
                                                signif(relative_abundance * 100, 3),
                                                "%"),),
                      position = "dodge") +
    ggplot2::geom_point(data = to_plot %>% dplyr::distinct(analyte, .keep_all = TRUE),
                        ggplot2::aes(x = analyte, 
                                     y = rsd,
                                     fill = sample_name,
                                     text = paste("RSD:",
                                                  signif(rsd * 100, 3),
                                                  "%")),
                        shape = 21,
                        stroke = 0.5)+
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1),
                   panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5),
                   legend.position = "none") +
    ggplot2::scale_y_continuous(name = "Relative abundance (%)",
                                labels = function(x) paste0(x * 100, "%")) +
    ggplot2::scale_x_discrete(name = "Analyte") +
    ggplot2::scale_fill_manual(values = my_palette,
                               name = "Sample name") +
    ggplot2::facet_wrap(~ cluster, scales = "free_x")
  
  return(dodged_barplot)
}

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
                       " calibration in LacyTools has failed, ",
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
                                                signif(mean_rel_ab * 100, 3),
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
                                                     signif(sd_rel_ab * 100, 3),
                                                     "%")),
                           width = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = analyte, 
                                     y = rsd,
                                     fill = analyte,
                                     text = paste("RSD:",
                                                  signif(rsd * 100, 3),
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
                                labels = function(x) paste0(x * 100, "%")) +
    ggplot2::scale_x_discrete(name = "Analyte") +
    ggplot2::facet_wrap(~ cluster, scales = "free_x")
  
  return(mean_barplot)
  
}
