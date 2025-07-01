
# Calculate total intensities of non-glycosylated peptides
calculate_peptides_intensities <- function(peptides_quality,
                                           exclude_peptides) {
  
  data <- peptides_quality %>% 
    dplyr::select(sample_name, sample_id, sample_type,
                  cluster, charge, tidyselect::any_of(c(
                    "group",
                    "absolute_intensity_background_subtracted",
                    "fraction",
                    "total_area"
                  ))) %>% 
    # Ignore ions when applicable
    dplyr::mutate(ion = paste0(cluster, ", ", charge), .after = charge) %>% 
    dplyr::filter(!ion %in% exclude_peptides)
  
  if (nrow(data) == 0) {
    return(NULL)
  }
  
  else if ("fraction" %in% colnames(data)) {
    data <- data %>% 
      dplyr::mutate(
        intensity_by_fraction = absolute_intensity_background_subtracted / fraction
      ) %>% 
      # Sum ion intensities for each peptide, per sample
      dplyr::group_by(sample_name, cluster) %>% 
      dplyr::mutate(total_intensity = sum(intensity_by_fraction)) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(sample_name, sample_id, sample_type, cluster,
                    tidyselect::any_of(c("group")), total_intensity) %>%
      dplyr::distinct() %>% 
      tidyr::pivot_wider(names_from = "cluster", values_from = "total_intensity")
    
    return(data)
  } 
  
  else {
    data <- data %>% 
      dplyr::group_by(sample_name, cluster) %>% 
      dplyr::mutate(total_intensity = sum(total_area)) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(sample_name, sample_id, sample_type, cluster,
                    tidyselect::any_of(c("group")), total_intensity) %>% 
      tidyr::pivot_wider(names_from = "cluster", values_from = "total_intensity")
    
    return(data)
  }
}



# Calculate site occupancies
calculate_site_occupancy <- function(peptides_intensities,
                                     normalized_data_wide,
                                     peptides_table) {
  
  peptides <- peptides_table$Peptide
  peptides <- peptides[peptides %in% colnames(peptides_intensities)]
  
  data <- normalized_data_wide %>% 
    dplyr::left_join(., peptides_intensities)
  
  for (peptide in peptides) {
    formula <- create_expr_ls(paste0(
      # Divide glycopeptides sum intensity by glycopeptide + peptide sum intensity
      peptide, "_site_occupancy = ", peptide, "_sum_intensity / ",
      "(", peptide, " + ", peptide, "_sum_intensity) * 100"
    ))
    data <- data %>% 
      dplyr::mutate(!!! formula, .after = tidyselect::contains("sum_intensity"))
  }
  
  data <- data %>% 
    dplyr::select(-peptides)
  
  return(data)
}



# Summarize QC criteria per ion, and for total/specific
summarize_peptides_quality <- function(peptides_quality,
                                       ipq,
                                       sn,
                                       idp,
                                       total_area,
                                       mass_accuracy) {

  if ("group" %in% colnames(peptides_quality)) {
    summary <- peptides_quality %>% 
      dplyr::group_by(group, cluster, charge)
  } else {
    summary <- peptides_quality %>% 
      dplyr::group_by(cluster, charge)
  }
  
  if (!is.null(ipq)) {
    # LaCyTools data
    summary <- summary %>% 
      dplyr::mutate(
        pass_ipq = isotopic_pattern_quality <= ipq,
        pass_sn = sn >= sn,
        # Option for different mass error because peptides are often not calibrated
        pass_mass_error = dplyr::between(
          mass_accuracy_ppm,
          left = mass_accuracy[[1]],
          right = mass_accuracy[[2]]
        ),
        pass = pass_ipq & pass_sn & pass_mass_error
      ) %>% 
      dplyr::filter(!is.na(pass)) %>% 
      dplyr::summarize(
        passing_percentage = sum(pass) / dplyr::n() * 100
      )
  } else {
    # Skyline data
    summary <- summary %>% 
      dplyr::mutate(
        pass_idp = isotope_dot_product >= idp,
        pass_total_area = total_area >= total_area,
        pass_mass_error = dplyr::between(
          mass_accuracy_ppm,
          left = mass_accuracy[[1]],
          right = mass_accuracy[[2]]
        ),
        pass = pass_idp & pass_total_area & pass_mass_error
      ) %>% 
      dplyr::summarize(
        passing_percentage = sum(pass) / dplyr::n() * 100
      ) 
  }
  
  return(summary)
}




# Quality plot for the non-glycosylated peptides
peptides_quality_plot <- function(peptides_quality_summary) {
  
  n_colors <- length(unique(peptides_quality_summary$cluster))
  
  plot <- ggplot2::ggplot(peptides_quality_summary, ggplot2::aes(
    x = charge, y = passing_percentage,
    text = paste0(
      "\nPeptide: ",
      cluster,
      "\nCharge: ",
      charge,
      "\nPassing percentage: ",
      paste0(signif(passing_percentage, 4), "%")
    )
  ))
  
  if ("group" %in% colnames(peptides_quality_summary)) {
    plot <- plot +
      ggplot2::facet_grid(cluster ~ group, scales = "free_x", ncol = 4)
  } else {
    plot <- plot + 
      ggplot2::facet_wrap(~cluster, scales = "free_x", ncol = 4)
  }
  
  plot <- plot + 
    ggplot2::geom_col(color = "black", ggplot2::aes(fill = cluster)) +
    ggplot2::theme_classic() + 
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5),
      strip.background = ggplot2::element_rect(fill = "#F6F6F8"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    ggplot2::labs(x = "", y = "Passing percentage")  + 
    ggplot2::scale_fill_manual(values = color_palette(n_colors)) + 
    ggplot2::scale_y_continuous(limits = c(0, 100))
  
  return(plot)
}




# Plot site occupancies boxplot
plot_site_occupancy <- function(site_occupancy) {
  
  n_colors <- length(unique(site_occupancy$sample_type))
  
  site_occupancy_long <- site_occupancy %>% 
    dplyr::select(sample_name, sample_type, sample_id,
                  tidyselect::contains("site_occupancy"),
                  tidyselect::any_of(c("group"))) %>% 
    tidyr::pivot_longer(tidyselect::contains("site_occupancy"),
                        names_to = "site", values_to = "occupancy") %>% 
    dplyr::mutate(site = gsub("_site_occupancy", "", site)) %>% 
    dplyr::filter(!is.na(occupancy))
  
  plot <- ggplot2::ggplot(site_occupancy_long, ggplot2::aes(
    x = site, y = occupancy, text = paste0(
      "Sample name: ", sample_name, "\n",
      "Sample ID: ", sample_id, "\n",
      "Site: ", site, "\n",
      "Site occupancy: ", format(round(occupancy, digits = 2), nsmall = 2), "%"
    )
  ))
  
  if ("group" %in% colnames(site_occupancy_long)) {
    plot <- plot + 
      ggplot2::facet_wrap(~group)
  }
  
  plot <- plot + 
    ggplot2::geom_boxplot(outlier.shape = NA) + 
    ggplot2::geom_jitter(ggplot2::aes(color = sample_type),
                         width = 0.2, height = 0, size = 1, alpha = 0.7) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5),
      strip.background = ggplot2::element_rect(fill = "#F6F6F8"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "right"
    ) + 
    ggplot2::scale_color_manual(values = color_palette(n_colors)) +
    ggplot2::labs(x = "Glycosylation site", y = "Site occupancy (%)",
                  color = "Sample type")
  
  return(plot)
}

