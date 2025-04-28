# Functions used for protein quantitation


get_glycopeptide_intensities <- function(proteins_excel, normalized_data_wide) {
  
  data <- normalized_data_wide %>% 
    tidyr::pivot_longer(
      tidyselect::contains("sum_intensity"),
      names_to = "cluster", values_to = "sum_intensity"
    ) %>% 
    dplyr::mutate(cluster = gsub("_sum_intensity", "", cluster)) %>% 
    dplyr::filter(cluster %in% c(
      proteins_excel$natural, proteins_excel$labeled
    )) %>% 
    dplyr::select(
      sample_name, sample_type, sample_id, tidyselect::any_of("group"),
      cluster, sum_intensity
    ) %>% 
    dplyr::filter(!is.na(sum_intensity)) %>% 
    dplyr::distinct()
  
  return(data)
}



get_peptide_intensities <- function(proteins_excel, peptides_data) {
  
  data <- peptides_data %>%
    dplyr::filter(cluster %in% c(
      proteins_excel$natural, proteins_excel$labeled
    )) %>% 
    dplyr::mutate(intensity_by_fraction = 
                  absolute_intensity_background_subtracted / fraction) %>% 
    dplyr::group_by(sample_name, cluster) %>% 
    dplyr::mutate(sum_intensity = sum(intensity_by_fraction)) %>% 
    dplyr::select(
      sample_name, sample_type, sample_id, tidyselect::any_of("group"),
      cluster, sum_intensity
    ) %>% 
    dplyr::filter(!is.na(sum_intensity)) %>% 
    dplyr::distinct()
  
  return(data)
}



get_protein_quantities <- function(combined_intensities,
                                   proteins_excel) {
  
  # Go over each row in proteins_excel and calculate corresponding quantities
  protein_quantities <- purrr::map_dfr(1:nrow(proteins_excel), function(i) {
    # Extract data
    protein_name <- proteins_excel[i, ]$protein
    natural <- proteins_excel[i, ]$natural
    labeled <- proteins_excel[i, ]$labeled
    standard_quantity <- as.numeric(proteins_excel[i, ]$standard_quantity)
    # Get data for current protein peptide in wide format
    data <- combined_intensities %>% 
      dplyr::filter(cluster %in% c(natural, labeled)) %>% 
      tidyr::pivot_wider(names_from = cluster, values_from = sum_intensity) %>% 
      dplyr::mutate(
        protein = protein_name,
        peptide_pair = paste(natural, "/", labeled)
      )
    # Calculate quantity for each sample
    data$protein_quantity <- data[[natural]] / data[[labeled]] * standard_quantity
    # Get just the quantities
    quantities <- data %>% 
      dplyr::select(
        sample_name, sample_type, sample_id, tidyselect::any_of("group"),
        protein, peptide_pair, protein_quantity
      ) %>% 
      dplyr::mutate(standard_quantity = standard_quantity)
    
    return(quantities)
  })
  
  return(protein_quantities)
}



get_median_quantities <- function(protein_quantities) {
  
  data <- protein_quantities %>% 
    dplyr::filter(!is.na(protein_quantity)) %>% 
    dplyr::group_by(sample_name, sample_type, sample_id, protein) %>% 
    dplyr::mutate(quantity = median(protein_quantity)) %>% 
    dplyr::select(sample_name, sample_type, sample_id, tidyselect::any_of("group"),
                  protein, quantity) %>% 
    dplyr::distinct()
  
  return(data)
}



plot_protein_quantities <- function(quantities,
                                    log_scale) {
  
  protein_name <- unique(quantities$protein)
  
  sample_types <- unique(quantities$sample_type)
  colors <- color_palette(length(sample_types))
  color_palette <- setNames(colors, sample_types)
  
  plot <- ggplot2::ggplot(quantities, ggplot2::aes(
    x = sample_type, y = quantity,
    text = paste0(
      "Sample name: ", sample_name, "\n",
      "Sample ID: ", sample_id, "\n",
      "Protein quantity: ", format(round(quantity, digits = 2), nsmall = 2)
    )
  )) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(
      ggplot2::aes(color = sample_type),
      height = 0, width = 0.2, size = 1, alpha = 0.7
    ) + 
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 0.5),
      strip.background = ggplot2::element_rect(fill = "#F6F6F8")
    ) +
    ggplot2::scale_color_manual(values = color_palette, name = "Sample type") + 
    ggplot2::labs(x = "Sample type", y = paste(protein_name, "quantity"))
  
  # Check for total and specific
  if ("group" %in% colnames(quantities)) {
    plot <- plot + ggplot2::facet_wrap(~group)
  } 
  
  # Check if logarithmic scale should be applied
  if (log_scale) {
    plot <- plot + ggplot2::scale_y_log10()
  }
  
  return(plot)
}


# Plot protein quantity based on two peptide pairs
# "pair" is pair of two peptide pairs
quantity_correlation_plot <- function(df, pair, color_palette, log_scale,
                                      correlation = "pearson", 
                                      correlation_symbol = "R") {
  # Make plot
  plot <- ggplot2::ggplot(df, ggplot2::aes(
    x = !!rlang::sym(pair[1]), y = !!rlang::sym(pair[2])
  )) + 
    # Add line of identity (y = x)
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      color = "#3D3D3D",
      linetype = "dashed" 
    ) +
    ggplot2::geom_point(ggplot2::aes(
      color = sample_type, text = paste0(
        "Sample name: ", sample_name, "\n",
        "Sample ID: ", sample_id, "\n"
      )
    ), alpha = 0.7, size = 1.5) +
    ggplot2::labs(x = pair[1], y = pair[2]) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "#F6F6F8"),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 0.5),
      axis.title = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 11),
      axis.text = ggplot2::element_text(size = 10)
    ) + 
    ggplot2::scale_color_manual(values = color_palette, name = "Sample type") +
    # Add Pearson correlation
    ggpubr::stat_cor(
      method = correlation,
      cor.coef.name = correlation_symbol,
      ggplot2::aes(label = ggplot2::after_stat(r.label)),
      na.rm = TRUE,
      size = 4
    )
  
  # Check for total and specific
  if ("group" %in% colnames(df)) {
    plot <- plot + ggplot2::facet_wrap(~group)
  } 
  
  # Check if logarithmic scale should be applied
  if (log_scale) {
    plot <- plot + ggplot2::scale_y_log10()
  }
  
  return(plot)
}



plot_peptide_correlations <- function(protein_data, 
                                      log_scale, 
                                      correlation_method) {
  
  # Create color palette
  sample_types <- unique(protein_data$sample_type)
  colors <- color_palette(length(sample_types))
  color_palette <- setNames(colors, sample_types)
  
  # Get all possible pairs of peptide pairs
  pairs <- combn(unique(protein_data$peptide_pair), 2, simplify = FALSE)
  
  # Make plot for each pair
  plots <- purrr::map(pairs, function(pair) {
    # Reshape data to get two quantity columns
    df_pair <- protein_data %>% 
      dplyr::filter(peptide_pair %in% pair) %>% 
      tidyr::pivot_wider(names_from = peptide_pair,
                         values_from = protein_quantity)
    quantity_correlation_plot(
      df_pair, pair, color_palette, log_scale,
      correlation = dplyr::case_when(
        correlation_method == "Pearson (linear)" ~ "pearson",
        correlation_method == "Spearman (non-parametric)" ~ "spearman"
      ),
      correlation_symbol = dplyr::case_when(
        correlation_method == "Pearson (linear)" ~ "R",
        correlation_method == "Spearman (non-parametric)" ~ "rho"
      )
    )
  })
  
  # Combine plots into one
  combined <- patchwork::wrap_plots(plots) +
    patchwork::plot_layout(guides = "collect", ncol = 2)

  return(combined)
}
