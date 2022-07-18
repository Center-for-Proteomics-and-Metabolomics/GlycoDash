my_boxplot <- function(data, xvar, yvar, color = NULL, facets = NULL) {
  
  plot <- data %>% 
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = .data[[xvar]],
                                       y = .data[[yvar]]),
                          outlier.shape = NA) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1)) +
    ggplot2::labs(x = snakecase::to_sentence_case(stringr::str_replace_all(xvar,
                                                                           pattern = "_",
                                                                           replacement = " ")),
                  # If the cluster prefix is the same as the start of the 
                  # analyte name, remove it from the axis title:
                  y = stringr::str_replace_all(string = yvar,
                                               c("(.+)_\\1(.+)" = "\\1 \\2",
                                                 "_" = " ")))
  
  if (!is.null(color)) {
    n_colors <- length(unique(data[[color]]))
    my_palette <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_colors)
    
    plot <- plot +
      ggplot2::geom_jitter(ggplot2::aes(x = .data[[xvar]],
                                        y = .data[[yvar]],
                                        color = .data[[color]])) +
      ggplot2::scale_color_manual(values = my_palette)
    
  } else {
    plot <- plot +
      ggplot2::geom_jitter(ggplot2::aes(x = .data[[xvar]],
                                        y = .data[[yvar]]),
                           color = RColorBrewer::brewer.pal(3, "Set2")[1])
  }
  
  if (!is.null(facets)) {
    plot <- plot +
      ggplot2::facet_wrap(facets)
  }
  return(plot)
}
