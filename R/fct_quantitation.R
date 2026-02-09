#' Extract Glycopeptide Intensities
#'
#' Extracts glycopeptide intensities from normalized wide-format data for
#' specified proteins. The function filters the data to include only
#' glycopeptides listed in the proteins Excel file and returns their
#' sum intensities grouped by sample.
#'
#' @param proteins_excel A dataframe with protein specifications, including
#'   columns \code{natural} and \code{labeled} containing cluster names
#'   for natural and labeled glycopeptides.
#' @param normalized_data_wide A dataframe with normalized data in wide format,
#'   with sum intensities in columns containing "sum_intensity".
#'
#' @return A dataframe with columns: \code{sample_name}, \code{sample_type},
#'   \code{sample_id}, optionally \code{group}, \code{cluster}, and
#'   \code{sum_intensity}. Contains only distinct rows with non-NA intensities.
#'
#' @keywords internal
#' @noRd
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


#' Extract Peptide Intensities from Skyline
#'
#' Processes peptide data (typically from Skyline) to extract intensities for
#' specified proteins. The function calculates intensity per fraction,
#' aggregates by cluster, and excludes specified peptide ions and proteins
#' not in the protein specification file.
#'
#' @param proteins_excel A dataframe with protein specifications, including
#'   columns \code{natural} and \code{labeled} containing cluster names
#'   for natural and labeled peptides.
#' @param peptides_data A dataframe with peptide data from Skyline,
#'   including columns: \code{cluster}, \code{charge},
#'   \code{absolute_intensity_background_subtracted}, \code{fraction},
#'   \code{sample_name}, \code{sample_type}, and \code{sample_id}.
#' @param exclude_peptides A character vector of peptide ions to exclude
#'   from analysis (in format "cluster, charge").
#'
#' @return A dataframe with columns: \code{sample_name}, \code{sample_type},
#'   \code{sample_id}, optionally \code{group}, \code{cluster}, and
#'   \code{sum_intensity}. Contains only distinct rows with non-NA intensities.
#'
#' @keywords internal
#' @noRd
get_peptide_intensities <- function(proteins_excel, 
                                    peptides_data,
                                    exclude_peptides) {
  
  data <- peptides_data %>%
    dplyr::mutate(ion = paste0(cluster, ", ", charge)) %>% 
    dplyr::filter(
      !ion %in% exclude_peptides,
      cluster %in% c(proteins_excel$natural, proteins_excel$labeled)
    ) %>% 
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


#' Calculate Protein Quantities
#'
#' Calculates protein quantities (in ng/mL) for each sample based on the
#' quantitation formula using natural and labeled peptide pairs. The function
#' iterates through each protein in the proteins_excel file, uses the ratio of
#' natural to labeled intensities, and applies the standard and sample volume
#' to compute the final quantity.
#'
#' @param combined_intensities A dataframe with combined intensities from
#'   both glycopeptide and non-glycosylated peptides, containing columns:
#'   \code{sample_name}, \code{sample_type}, \code{sample_id}, optionally
#'   \code{group}, \code{cluster}, and \code{sum_intensity}.
#' @param proteins_excel A dataframe with protein specifications, including
#'   columns: \code{protein} (protein name), \code{natural} (natural cluster),
#'   \code{labeled} (labeled cluster), \code{standard_ng} (standard amount in ng),
#'   and \code{sample_ul} (sample volume in microliter).
#'
#' @return A dataframe with columns: \code{sample_name}, \code{sample_type},
#'   \code{sample_id}, optionally \code{group}, \code{protein},
#'   \code{peptide_pair}, and \code{protein_quantity} (in ng/mL).
#'
#' @keywords internal
#' @noRd
get_protein_quantities <- function(combined_intensities,
                                   proteins_excel) {
  
  # Go over each row in proteins_excel and calculate corresponding quantities
  protein_quantities <- purrr::map_dfr(1:nrow(proteins_excel), function(i) {
    # Extract data
    protein_name <- proteins_excel[i, ]$protein
    natural <- proteins_excel[i, ]$natural
    labeled <- proteins_excel[i, ]$labeled
    standard_ng <- as.numeric(proteins_excel[i, ]$standard_ng)
    sample_ul <- as.numeric(proteins_excel[i, ]$sample_ul)
    # Get data for current protein peptide in wide format
    data <- combined_intensities %>% 
      dplyr::filter(cluster %in% c(natural, labeled)) %>% 
      tidyr::pivot_wider(names_from = cluster, values_from = sum_intensity) %>% 
      dplyr::mutate(
        protein = protein_name,
        peptide_pair = paste(natural, "/", labeled)
      )
    # Calculate quantity for each sample in ng/mL
    ml <- sample_ul / 1000
    data$protein_quantity <- data[[natural]] / data[[labeled]] * standard_ng / ml
    
    # Get just the quantities
    quantities <- data %>% 
      dplyr::select(
        sample_name, sample_type, sample_id, tidyselect::any_of("group"),
        protein, peptide_pair, protein_quantity
      )
    
    return(quantities)
  })
  
  return(protein_quantities)
}


#' Calculate Median Protein Quantities
#'
#' Calculates the median protein quantity for each protein per sample.
#' This function is useful when multiple peptide pairs are used to quantify
#' the same protein, as it provides a single representative quantity value
#' per sample.
#'
#' @param protein_quantities A dataframe with protein quantities as returned
#'   by \code{get_protein_quantities()}, containing columns: \code{sample_name},
#'   \code{sample_type}, \code{sample_id}, optionally \code{group}, \code{protein},
#'   \code{peptide_pair}, and \code{protein_quantity}.
#'
#' @return A dataframe with columns: \code{sample_name}, \code{sample_type},
#'   \code{sample_id}, optionally \code{group}, \code{protein}, and \code{quantity}
#'   (the median quantity in ng/mL per sample and protein).
#'
#' @keywords internal
#' @noRd
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


#' Plot Protein Quantities by Sample Type
#'
#' Creates a boxplot visualization of protein quantities grouped by sample type.
#' Individual data points are displayed as jittered points overlaid on the
#' boxplot. If the data contains a \code{group} column, separate panels are
#' created for each group using faceting.
#'
#' @param quantities A dataframe with protein quantities, containing columns:
#'   \code{sample_name}, \code{sample_id}, \code{sample_type}, \code{quantity},
#'   and optionally \code{group}.
#' @param log_scale Logical, whether to apply a logarithmic scale to the y-axis.
#'   If \code{NULL} or \code{FALSE}, a linear scale is used.
#'
#' @return A ggplot object displaying the protein quantities as a boxplot with
#'   jittered points colored by sample type.
#'
#' @keywords internal
#' @noRd
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
      "Protein quantity: ", format(round(quantity, digits = 2), nsmall = 2), " ng/mL"
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
    ggplot2::labs(x = "Sample type", y = paste(protein_name, "quantity (ng/mL)"))
  
  # Check for total and specific
  if ("group" %in% colnames(quantities)) {
    plot <- plot + ggplot2::facet_wrap(~group)
  } 
  
  # Check if logarithmic scale should be applied
  if (!is.null(log_scale)) {
    if (log_scale == TRUE) {
      plot <- plot + ggplot2::scale_y_log10(guide = "axis_logticks")
    }
  }
  
  return(plot)
}


#' Plot Correlation Between Two Peptide Pair Quantities
#'
#' Creates a scatter plot showing the correlation between protein quantities
#' calculated from two different peptide pairs. A line of identity (y = x) is
#' included as a dashed reference line. If the data contains a \code{group}
#' column, separate panels are created for each group using faceting.
#'
#' @param df A dataframe with protein quantities for two peptide pairs.
#'   Must contain columns with names matching \code{pair[1]} and \code{pair[2]},
#'   as well as \code{sample_name}, \code{sample_id}, and \code{sample_type}.
#'   Optionally may contain a \code{group} column.
#' @param pair A character vector of length 2, containing the names of the two
#'   peptide pair columns to compare.
#' @param color_palette A named vector mapping sample types to colors.
#' @param log_scale Logical, whether to apply a logarithmic scale to both axes.
#'   If \code{NULL} or \code{FALSE}, linear scales are used.
#'
#' @return A ggplot object displaying the correlation between two peptide pair
#'   quantities as a scatter plot with a reference line of identity.
#'
#' @keywords internal
#' @noRd
quantity_correlation_plot <- function(df, pair, color_palette, log_scale) {
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
        "Sample ID: ", sample_id, "\n",
        "Sample type: ", sample_type, "\n",
        "Quantity based on ", pair[1], ": ", 
        format(round(!!rlang::sym(pair[1]), digits = 2), nsmall = 2), " ng/mL \n",
        "Quantity based on ", pair[2], ": ", 
        format(round(!!rlang::sym(pair[2]), digits = 2), nsmall = 2), " ng/mL \n"
      )
    ), alpha = 0.7, size = 1.2) +
    ggplot2::labs(x = pair[1], y = pair[2]) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "#F6F6F8"),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 0.5),
      legend.position = "none"
    ) + 
    ggplot2::scale_color_manual(values = color_palette, name = "Sample type")
  
  # Check for total and specific
  if ("group" %in% colnames(df)) {
    plot <- plot + ggplot2::facet_wrap(~group)
  } 
  
  # Check if logarithmic scale should be applied
  if (!is.null(log_scale)) {
    if (log_scale == TRUE) {
      plot <- plot + 
        ggplot2::scale_x_log10(guide = "axis_logticks") +
        ggplot2::scale_y_log10(guide = "axis_logticks")
    }
  }
  
  return(plot)
}


#' Plot Correlations Between All Peptide Pairs
#'
#' Creates a list of scatter plots showing the correlations between all
#' possible pairs of peptide pairs for a given protein. Each plot uses the
#' \code{quantity_correlation_plot()} function and includes a reference line
#' of identity. This function is useful for assessing the consistency of
#' protein quantitation across different peptide pairs.
#'
#' @param protein_data A dataframe with protein quantities for multiple peptide
#'   pairs, containing columns: \code{sample_name}, \code{sample_id},
#'   \code{sample_type}, \code{peptide_pair}, \code{protein_quantity}, and
#'   optionally \code{group}.
#' @param log_scale Logical, whether to apply a logarithmic scale to both axes.
#'   If \code{NULL} or \code{FALSE}, linear scales are used.
#'
#' @return A list of ggplot objects, each displaying a correlation plot between
#'   two different peptide pairs. Returns an empty list if only one peptide pair
#'   is present in the data.
#'
#' @keywords internal
#' @noRd
plot_peptide_correlations <- function(protein_data, log_scale) {
  
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
    
    quantity_correlation_plot(df_pair, pair, color_palette, log_scale)
  })
  
  # Return list of plots
  return(plots)
}
