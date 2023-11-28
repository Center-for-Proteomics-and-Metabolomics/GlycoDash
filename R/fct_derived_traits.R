#' generate_formula
#'
#' Generates a formula for a glycan trait, based on a specified cluster,
#' trait reference file and a trait name that is present in the reference file as a column,
#' 
#' @param cluster Cluster name, e.g. "IgGI"
#' @param cluster_ref_df Reference file for traits, e.g. human_IgG_ref filtered with only glycans
# that passed the analyte curation.
#' @param target_trait   # Trait for which a formula should be created, e.g. "galactosylation"
#'
#' @return  A character string with a formula
generate_formula <- function(cluster, cluster_ref_df, target_trait) {
  
  # Get the glycans that should be used for calculating the trait
  df <- cluster_ref_df %>% 
    dplyr::select(glycan, tidyselect::all_of(target_trait)) %>%   # target_trait is just one trait
    dplyr::filter(!!rlang::sym(target_trait) != 0)

  # Create a string with the right hand side of the formula
  formula_string <- paste0(df[[target_trait]], " * ", paste0(cluster, "1", df$glycan), collapse = " + ")
  
  # Collect terms and coefficients
  terms <- strsplit(formula_string, " \\+ ")[[1]]
  coefficients <- sapply(strsplit(terms, " \\* "), `[`, 1)
  unique_coefficients <- unique(coefficients)
  
  # Group terms by coefficients
  grouped_terms <- lapply(unique_coefficients, function(coeff) {
    coeff_indices <- which(coefficients == coeff)
    terms_with_coeff <- terms[coeff_indices]
    if (length(terms_with_coeff) > 1) {
      paste0(
        coeff, 
        " * (", 
        paste(gsub(".* \\* ", "", terms_with_coeff), collapse = " + "),
        ")"
      )
    } else {
      terms_with_coeff
    }
  })
  
  # Reconstruct cleaned formula string with grouped terms
  clean_formula_string <- paste(unlist(grouped_terms), collapse = " + ")
  
  # Divide by the sum of all complex-type glycans if necessary
  if (target_trait %in% c("fucosylation", "bisection", "galactosylation", "sialylation", "mono_antennary")) {
    complex_types_df <- cluster_ref_df %>% 
      dplyr::select(glycan, complex) %>% 
      dplyr::filter(complex == 1)
    # String with sum of complex type glycans
    complex_sum <- paste0(cluster, "1", complex_types_df$glycan, collapse = " + ")
    # Adjust clean_formula_string to divide by complex_types
    clean_formula_string <- paste0("(", clean_formula_string, ") / (", complex_sum, ")")
  }
  
  # Add the left hand side of the formula
  final_formula_string <- ifelse(
    # If there are no glycans for the trait, than the formula is " * <cluster>1"
    # In that case the value should just be zero.
    clean_formula_string == paste0(" * ", cluster, "1"),
    paste0(cluster, "_", target_trait, " = ", "0"),
    paste0(cluster, "_", target_trait, " = ", clean_formula_string)
  )
  
  return(final_formula_string)
}




#' match_human_IgG_traits
#'
#' Matches human IgG traits descriptions from UI to column names in human_IgG_ref
#'
#' @param human_traits_ui_input Character vector from the UI human IgG traits input.
#'
#' @return A character vector with short names of IgG traits, which correspond to column names
# in the human_IgG_ref file.
#'
match_human_IgG_traits <- function(human_traits_ui_input) {
  traits <- c(
    "Fucosylation of complex-type glycans" = "fucosylation",
    "Bisection of complex-type glycans" = "bisection",
    "Galactosylation of complex-type glycans" = "galactosylation",
    "Sialylation of complex type-glycans" = "sialylation",
    "Percentage of monoantennary complex-type glycans" = "mono_antennary",
    "Percentage of hybrid-type glycans" = "hybrid",
    "Oligomannose-type glycans: average number of mannoses" = "high_mannose"
  )
  
  replaced_vector <- vector("character", length = length(human_traits_ui_input))
  
  for (i in seq(length(human_traits_ui_input))) {
    trait_desc <- human_traits_ui_input[i]
    replaced_vector[i] <- traits[[trait_desc]]
  }
  
  return(replaced_vector)
}





#' create_formula_list
#'
#' Creates a list of formulas for human IgG traits.
#'
#' @param normalized_data  normalized_data in long format
#' @param chosen_traits Character vector, e.g.  c("fucosylation", "sialylation")
#' @param chosen_clusters  Character vector, e.g. c("IgGI", "IgGII")
#' @param reference Reference file for traits, e.g. human_IgG_ref.
#' 
create_formula_list <- function(normalized_data, chosen_traits, chosen_clusters, reference) {
  # Initiate an empty list
  formula_list <- vector("character", length(chosen_clusters))
  # Loop over the chosen clusters
  for (i in seq(length(chosen_clusters))) {
    cluster_name <- chosen_clusters[[i]]
    # Create an empty list to store the traits formulas for the cluster
    cluster_trait_formulas <- vector("character", length(chosen_traits))
    # Get the normalized data for the cluster
    cluster_normalized_data <- normalized_data %>% 
      dplyr::filter(cluster == cluster_name)
    # Get all analytes/glycans in the cluster
    cluster_analytes <- unique(cluster_normalized_data$analyte)
    cluster_glycans <- stringr::str_remove(cluster_analytes, paste0(cluster_name, "1"))
    # Get a subset of the reference file with only the passing analytes
    cluster_ref <- reference %>% 
      dplyr::filter(glycan %in% cluster_glycans)
    # Loop over the chosen traits
    for (j in seq(length(chosen_traits))) {
      trait <- chosen_traits[[j]]
      trait_formula <- generate_formula(cluster_name, cluster_ref, trait)
      cluster_trait_formulas[j] <- trait_formula
    }
    # Add the formulas for this cluster to formula_list
    formula_list[i] <- list(cluster_trait_formulas)
  }
  return(formula_list)
}


#' Automatically calculate traits based on list of formulas
calculate_traits <- function(normalized_data_wide, trait_formulas) {
  # Initiate an empty vector for the trait names
  trait_names <- vector("character", length = length(trait_formulas))
  # Loop over the formulas and create a new column with traits
  normalized_data_wide_with_traits <- normalized_data_wide
  for (i in seq(length(trait_formulas))) {
    formula <- trait_formulas[[i]]
    expr_ls <- create_expr_ls(formula) 
    trait_names[[i]] <- names(expr_ls)
    normalized_data_wide_with_traits <- normalized_data_wide_with_traits %>% 
      dplyr::mutate(!!! expr_ls)
  }
  # Relocate the trait columns
  normalized_data_wide_with_traits <- normalized_data_wide_with_traits %>% 
    dplyr::relocate(tidyselect::all_of(trait_names), .after = replicates)
  # Return normalized data with the trait columns
  return(normalized_data_wide_with_traits)
}


#' create_expr_ls
#' 
#' Function to transform trait formula string into named list containing the 
#' expression of the right-hand side of the equation. The name is the left-hand
#' side of the equation. Used in the "calculate_custrom_trait" function.
#' Based on:
#' 
#' https://stackoverflow.com/questions/70821721/how-to-use-an-expression-in-dplyrmutate-in-r
#'
#'expr_rm extracts whatever comes before the "=" sign (spaces around the "=" sign do not matter)
#'
create_expr_ls <- function(str_expr) {
  expr_nm <- stringr::str_extract(str_expr, "^\\w+")
  expr_code <- stringr::str_replace_all(str_expr, "(^\\w+\\s?=\\s?)(.*)", "\\2")
  rlang::set_names(list(str2lang(expr_code)), expr_nm)
}




#' Calculate custom derived glycosylation traits
#' 
#' Calculate custom glycosylation traits of IgG based on formulas provided in an 
#' Excel file.
#' 
#'
#' @param normalized_data A dataframe that contains at least the columns
#' "analyte", "plate_well" and "relative_abundance". 
#' Entries in the column "analyte" must have the form <cluster>1<glycan composition>,
#' e.g.:
#' 
#' IgGII1H3N4F1.
#' @param custom_traits_formulas An Excel file loaded with the "load_excel" function
#' from the loadxl package. The first column in the Excel file contains the cluster.
#' The second column contains a trait to calculate for that cluster.
#' Each custom trait must be places on a new row. 
#' A formula must have the name of the trait on the left-hand side, 
#' and an expression on the right-hand side. E.g.:
#' 
#' my_trait = (0.5 * H3N4 + H4N4) / (H3N4F1 + H4N4F1)
#'
#' @return
#' A tibble with the following columns: sample_name, cluster, group, custom traits
#' and formulas used to calculate custom traits.
#' 
#' @export
#'
#' @examples fill this in...
calculate_custom_traits <- function(normalized_data, custom_traits_formulas){
  
  calculated_custom_traits <- normalized_data %>% 
    # Separate analyte into cluster and glycan
    dplyr::select(-cluster, -sum_intensity) %>% 
    tidyr::separate(analyte, sep = "1", into = c("cluster", "glycan"), 
                    extra = "merge", remove = TRUE) %>% 
    # Create column for each glycan with relative abundance as value
    tidyr::pivot_wider(names_from = "glycan", values_from = relative_abundance) %>% 
    # Replace NA relative abundances by zero
    dplyr::mutate_at(dplyr::vars(replicates:last_col()), ~ifelse(is.na(.), 0, .)) # relative abundances come after replicates column
  
  
  # Create two empty vectors: one for trait column names, and one for trait formulas
  trait_colnames <- vector("character", length = nrow(custom_traits_formulas))
  formula_colnames <- vector("character", length = nrow(custom_traits_formulas))
  
  # Loop the traits.
  # TODO: replace this by vectorized operations to make it faster.
  for (i in seq(1:nrow(custom_traits_formulas))){
    # Get cluster for which to calculate the trait
    cluster_specified <- as.character(custom_traits_formulas[i, 1])
    
    # Get formula as string
    formula_string <- as.character(custom_traits_formulas[i, 2])  
    
    # Convert to expression that can be used in dplyr mutate function
    formula_expr_ls <- create_expr_ls(formula_string)
    
    # Check that glycans in the formula actually exist in the data frame. 
    # If not, stop the for-loop (return NA) and show a warning message.
    cols_to_check <- all.vars(formula_expr_ls[[1]])
    if (!all(cols_to_check %in% names(calculated_custom_traits))) {
      shinyalert::shinyalert(
        text = "Your formulas for glycosylation traits contain one or more
               glycans that are not present in the data after analyte curation.
               Please check your formulas and try again.",
        type = "warning"
      )
      return(NA)
    }
    
    # Get name of custom trait including cluster: <cluster>_<trait name>
    trait_name <- paste(cluster_specified, names(formula_expr_ls)[1], sep = "_")
    
    # Add trait names and formulas to the vectors
    trait_colnames[i] <- trait_name
    formula_colnames[i] <- paste(trait_name, "formula", sep = "_")
    
    # Calculate trait per sample, cluster has to match specified cluster
    # Gives a tibble with 3 columns: cluster, plate_well, <custom trait>, and the used formula.
    calculated_trait_cluster <- calculated_custom_traits %>%
      dplyr::filter(cluster == cluster_specified) %>%
      dplyr::mutate(!!!formula_expr_ls) %>%
      dplyr::select(sample_name:replicates, names(formula_expr_ls)[1]) %>%
      # Change name of column <custom trait> to <cluster_specified>_<custom trait>
      dplyr::rename(!!trait_name := names(formula_expr_ls)[1]) %>%
      # Add a column with the formula that was used to calculate the trait
      dplyr::mutate(!!paste(trait_name, "formula", sep = "_") := formula_string)
    
    # Add to "calculated traits" data frame
    calculated_custom_traits <- calculated_custom_traits %>%
      dplyr::left_join(., calculated_trait_cluster)
    
  }
  
  # Get "calculated_custom_traits" in correct format
  calculated_custom_traits <- calculated_custom_traits %>%
    dplyr::select(sample_name:replicates, tidyselect::any_of(c(trait_colnames, formula_colnames))) %>% 
    dplyr::select(-cluster) %>% 
    dplyr::group_by(sample_name) %>% 
    tidyr::fill(tidyr::everything(), .direction = "downup") %>% 
    dplyr::ungroup() %>% 
    dplyr::distinct() %>% 
    dplyr::relocate(all_of(trait_colnames), .after = replicates)
  
  # Return calculated custom traits tibble
  return(calculated_custom_traits)
}

