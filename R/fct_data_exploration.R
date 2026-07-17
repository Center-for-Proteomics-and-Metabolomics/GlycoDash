#' Create a box plot
#'
#' @description
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
my_boxplot <- function(
    data, 
    xvar, 
    yvar, 
    color = NULL, 
    facets = NULL  
  ) {
  
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
    ggplot2::geom_boxplot(
      ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]]), outlier.shape = NA
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.border = ggplot2::element_rect(
      colour = "black", fill = NA, linewidth = 0.5)
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::labs(x = nicer_label(xvar), y = nicer_label(yvar))
  
  if (!is.null(color)) {
    n_colors <- length(unique(data[[color]]))
    my_palette <- color_palette(n_colors)
    
    plot <- plot +
      ggplot2::geom_jitter(
        ggplot2::aes(
          x = .data[[xvar]], 
          y = .data[[yvar]], 
          color = .data[[color]]
        ),
          height = 0,
          width = 0.25,
          alpha = 0.6
      ) +
      {
        # In case of continuous color variable
        if (is.numeric(data[[color]]) && !is.integer(data[[color]])) {
          ggplot2::scale_color_continuous(type = "viridis")
        } else {
          ggplot2::scale_color_manual(
            values = my_palette, name = nicer_label(color)
          )
        }
      }
  } else {
    plot <- plot +
      ggplot2::geom_jitter(
        ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]]),
        height = 0,
        width = 0.25,
        color = "#1f77b4",
        alpha = 0.6
      )
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
my_scatter_plot <- function(
    data, 
    xvar, 
    yvar, 
    color = NULL, 
    facets = NULL  
  ) {
  
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
    ggplot2::theme(
      panel.border = ggplot2::element_rect(
        colour = "black", fill = NA, linewidth = 0.5
      )
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::labs(x = nicer_label(xvar), y = nicer_label(yvar))
  
  if (!is.null(color)) {
    n_colors <- length(unique(data[[color]]))
    my_palette <- color_palette(n_colors)

    plot <- plot +
      ggplot2::geom_point(
        ggplot2::aes(
          x = as.numeric(.data[[xvar]]),
          y = as.numeric(.data[[yvar]]),
          color = .data[[color]]
        )
      ) +
      {
        if (is.numeric(data[[color]]) && !is.integer(data[[color]])) {
          ggplot2::scale_color_continuous(type = "viridis")
        } else {
          ggplot2::scale_color_manual(
            values = my_palette,
            name = nicer_label(color)
          )
        }
      }
    
  } else {
    plot <- plot +
      ggplot2::geom_point(
        ggplot2::aes(
          x = as.numeric(.data[[xvar]]), 
          y = as.numeric(.data[[yvar]])
        ),
        color = "#1f77b4"
      )
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
my_histogram <- function(
    data, 
    xvar = NULL, 
    color = NULL, 
    facets = NULL  
  ) {
  
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
    ggplot2::theme(panel.border = ggplot2::element_rect(
      colour = "black", fill=NA, linewidth=0.5)
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
      ggplot2::labs(x = nicer_label(xvar))
  
  if (!is.null(color)) {
    n_colors <- length(unique(data[[color]]))
    my_palette <- color_palette(n_colors)
    
    plot <- plot +
      ggplot2::geom_histogram(
        ggplot2::aes(x = .data[[xvar]], fill = .data[[color]])
      ) +
      {
        if (is.numeric(data[[color]]) && !is.integer(data[[color]])) {
          ggplot2::scale_fill_continuous(type = "viridis")
        } else {
          ggplot2::scale_fill_manual(
            values = my_palette, name = nicer_label(color)
          )
        }
      }
    
  } else {
    plot <- plot +
      ggplot2::geom_histogram(
        ggplot2::aes(x = .data[[xvar]]), fill = "#1f77b4"
      )
  }
  
  if (!is.null(facets)) {
    plot <- plot +
      ggplot2::facet_wrap(facets)
  }
  
  return(plot)
}


# nicer_label is a helper function that is used in the plotting functions.
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

