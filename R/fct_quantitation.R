get_glycopeptide_intensities <- function(protein_peptides, normalized_data_wide) {
  
  data <- normalized_data_wide %>% 
    tidyr::pivot_longer(
      tidyselect::contains("sum_intensity"),
      names_to = "cluster", values_to = "sum_intensity"
    ) %>% 
    dplyr::mutate(cluster = gsub("_sum_intensity", "", cluster)) %>% 
    dplyr::filter(cluster %in% c(
      protein_peptides$natural, protein_peptides$labeled
    )) %>% 
    dplyr::select(
      sample_name, sample_type, sample_id, tidyselect::any_of("group"),
      cluster, sum_intensity
    ) %>% 
    dplyr::filter(!is.na(sum_intensity)) %>% 
    dplyr::distinct()
  
  return(data)
}



get_peptide_intensities <- function(protein_peptides, peptides_data) {
  
  data <- peptides_data %>%
    dplyr::filter(cluster %in% c(
      protein_peptides$natural, protein_peptides$labeled
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



get_protein_quantities <- function(glycopeptide_intensities, 
                                   peptide_intensities,
                                   protein_peptides) {
  
  # Go over each row in protein_peptides and calculate corresponding quantities
  protein_quantities <- purrr::map_dfr(1:nrow(protein_peptides), function(i) {
    # Extract data
    protein <- protein_peptides[i, ]$protein
    natural <- protein_peptides[i, ]$natural
    labeled <- protein_peptides[i, ]$labeled
    standard_quantity <- as.numeric(protein_peptides[i, ]$standard_quantity)
    # Get data for current protein peptide in wide format
    data <- dplyr::bind_rows(glycopeptide_intensities, peptide_intensities) %>% 
      dplyr::filter(cluster %in% c(natural, labeled)) %>% 
      tidyr::pivot_wider(names_from = cluster, values_from = sum_intensity) %>% 
      dplyr::mutate(protein = protein)
    # Calculate quantity for each sample
    data$protein_quantity <- data[[natural]] / data[[labeled]] * standard_quantity
    # Get just the quantities
    quantities <- data %>% 
      dplyr::select(
        sample_name, sample_type, sample_id, tidyselect::any_of("group"),
        protein, protein_quantity
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
                  protein, quantity)
  
  return(data)
}
