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
    ggplot2::labs(x = nicer_label(xvar),
                  y = nicer_label(yvar))
  
  if (!is.null(color)) {
    n_colors <- length(unique(data[[color]]))
    my_palette <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_colors)
    
    plot <- plot +
      ggplot2::geom_jitter(ggplot2::aes(x = .data[[xvar]],
                                        y = .data[[yvar]],
                                        color = .data[[color]],
                                        text = paste0(
                                          "\nSample name: ",
                                          sample_name,
                                          "\nSample type: ",
                                          sample_type,
                                          "\n",
                                          nicer_label(yvar),
                                          ": ",
                                          .data[[yvar]]
                                        ))) +
      ggplot2::scale_color_manual(values = my_palette,
                                  name = nicer_label(color))
    
  } else {
    plot <- plot +
      ggplot2::geom_jitter(ggplot2::aes(x = .data[[xvar]],
                                        y = .data[[yvar]],
                                        text = paste0(
                                          "\nSample name: ",
                                          sample_name,
                                          "\nSample type: ",
                                          sample_type,
                                          "\n",
                                          nicer_label(yvar),
                                          ": ",
                                          .data[[yvar]]
                                        )),
                           color = RColorBrewer::brewer.pal(3, "Set2")[1])
  }
  
  if (!is.null(facets)) {
    plot <- plot +
      ggplot2::facet_wrap(facets)
  }
  return(plot)
}

my_scatter_plot <- function(data, xvar, yvar, color = NULL, facets = NULL) {
  
  plot <- data %>% 
    ggplot2::ggplot() +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1)) +
    ggplot2::labs(x = nicer_label(xvar),
                  y = nicer_label(yvar))
  
  if (!is.null(color)) {
    n_colors <- length(unique(data[[color]]))
    my_palette <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_colors)
    
    plot <- plot +
      ggplot2::geom_point(ggplot2::aes(x = as.numeric(.data[[xvar]]),
                                       y = as.numeric(.data[[yvar]]),
                                       color = .data[[color]],
                                       text = paste0(
                                         "\nSample name: ",
                                         sample_name,
                                         "\nSample type: ",
                                         sample_type,
                                         "\n",
                                         nicer_label(yvar),
                                         ": ",
                                         .data[[yvar]]
                                       ))) +
      ggplot2::scale_color_manual(values = my_palette,
                                  name = nicer_label(color))
    
  } else {
    plot <- plot +
      ggplot2::geom_point(ggplot2::aes(x = as.numeric(.data[[xvar]]),
                                       y = as.numeric(.data[[yvar]]),
                                       text = paste0(
                                         "\nSample name: ",
                                         sample_name,
                                         "\nSample type: ",
                                         sample_type,
                                         "\n",
                                         nicer_label(yvar),
                                         ": ",
                                         .data[[yvar]]
                                       )),
                          color = RColorBrewer::brewer.pal(3, "Set2")[1])
  }
  
  if (!is.null(facets)) {
    plot <- plot +
      ggplot2::facet_wrap(facets)
  }
  
  return(plot)
  
}

my_histogram <- function(data, xvar = NULL, color = NULL, facets = NULL) {
  
  plot <- data %>% 
    ggplot2::ggplot() +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1)) +
      ggplot2::labs(x = nicer_label(xvar))
  
  if (!is.null(color)) {
    n_colors <- length(unique(data[[color]]))
    my_palette <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_colors)
    
    plot <- plot +
      ggplot2::geom_histogram(ggplot2::aes(x = .data[[xvar]],
                                           fill = .data[[color]],
                                           text = paste0(
                                             "Number of samples: ",
                                             ggplot2::after_stat(count),
                                             "\n",
                                             nicer_label(xvar),
                                             ": ",
                                             signif(xmin, 3),
                                             " to ",
                                             signif(xmax, 3)
                                           ))) +
      ggplot2::scale_fill_manual(values = my_palette,
                                 name = nicer_label(color))
    
  } else {
    plot <- plot +
      ggplot2::geom_histogram(ggplot2::aes(x = .data[[xvar]],
                                           text = paste0(
                                             "Number of samples: ",
                                             ggplot2::after_stat(count),
                                             "\n",
                                             nicer_label(xvar),
                                             ": ",
                                             signif(xmin, 3),
                                             " to ",
                                             signif(xmax, 3)
                                           )),
                              fill = RColorBrewer::brewer.pal(3, "Set2")[1])
  }
  
  if (!is.null(facets)) {
    plot <- plot +
      ggplot2::facet_wrap(facets)
  }
  
  return(plot)
  
}

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

firstupper <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  return(string)
}

nfacets <- function(ggplot_object) {
  build <- ggplot2::ggplot_build(ggplot_object)
  length(levels(build$data[[1]]$PANEL))
}

hide_outliers <- function(plotly_object) {
  
  plotly_object[["x"]][["data"]] <- purrr::map(
    plotly_object[["x"]][["data"]], 
    function(x) {
      if (x$hoverinfo == 'y') {  
        x$marker = list(opacity = 0) 
      }  
      return(x) 
    })
  
  return(plotly_object)
}

