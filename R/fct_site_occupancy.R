
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



peptides_quality_plot <- function(peptides_quality_summary) {
  
  n_colors <- length(unique(peptides_quality_summary$cluster))
  
  plot <- ggplot2::ggplot(peptides_quality_summary, ggplot2::aes(
    x = cluster, y = passing_percentage,
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
      ggplot2::facet_grid(charge ~ group)
  } else {
    plot <- plot + 
      ggplot2::facet_wrap(~charge)
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
    ggplot2::scale_fill_manual(values = color_palette(n_colors))
  
  return(plot)
}
