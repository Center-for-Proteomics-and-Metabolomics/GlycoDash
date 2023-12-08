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
  # Or by the sum of all oligomannose glycans
  if (target_trait %in% c("fucosylation", "bisection", "galactosylation", "sialylation", 
                          "mono_antennary", "alpha_galactosylation")) {
    complex_types_df <- cluster_ref_df %>% 
      dplyr::select(glycan, complex) %>% 
      dplyr::filter(complex == 1)
    # String with sum of complex type glycans
    complex_sum <- paste0(cluster, "1", complex_types_df$glycan, collapse = " + ")
    # Adjust clean_formula_string to divide by complex_types
    clean_formula_string <- paste0("(", clean_formula_string, ") / (", complex_sum, ")")
  } else if (target_trait == "oligomannose_average") {
    oligomannose_df <- cluster_ref_df %>%
      dplyr::select(glycan, oligomannose_average) %>%
      dplyr::filter(oligomannose_average != 0)
    oligomannose_sum <- paste0(cluster, "1", oligomannose_df$glycan, collapse = " + ")
    clean_formula_string <- paste0("(", clean_formula_string, ") / (", oligomannose_sum, ")")
  }
  
  # Add the left hand side of the formula
  final_formula_string <- ifelse(
    # If there are no glycans for the trait, then the calculation is equal to 
    # " * <cluster>1" or starts with "( * <cluster>1)"
    # In that case the value should just be zero.
    any(
      clean_formula_string == paste0(" * ", cluster, "1"),
      stringr::str_starts(clean_formula_string, paste0("\\( \\* ", cluster, "1\\)"))  # need escapement characters
    ),
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
    "Percentage of oligomannose-type glycans" = "oligomannose_relative",
    "Oligomannose-type glycans: average number of mannoses" = "oligomannose_average"
  )
  
  replaced_vector <- vector("character", length = length(human_traits_ui_input))
  
  for (i in seq(length(human_traits_ui_input))) {
    trait_desc <- human_traits_ui_input[i]
    replaced_vector[i] <- traits[[trait_desc]]
  }
  
  return(replaced_vector)
}



#' match_mouse_IgG_traits
#'
#' Matches mouse IgG traits descriptions from UI to column names in mouse_IgG_ref
#'
#' @param mouse_traits_ui_input Character vector from the UI mouse IgG traits input.
#'
#' @return A character vector with short names of mouse IgG traits, which correspond to column names
# in the mouse_IgG_ref file.
#'
match_mouse_IgG_traits <- function(mouse_traits_ui_input) {
  traits <- c(
    "Fucosylation of complex-type glycans" = "fucosylation",
    "Bisection of complex-type glycans" = "bisection",
    "Galactosylation of complex-type glycans" = "galactosylation",
    "Sialylation of complex-type glycans" = "sialylation",
    "\u03B1-1,3-galactosylation of complex-type glycans" = "alpha_galactosylation",
    "Percentage of monoantennary complex-type glycans" = "mono_antennary",
    "Percentage of hybrid-type glycans" = "hybrid",
    "Percentage of oligomannose-type glycans" = "oligomannose_average",
    "Oligomannose-type glycans: average number of mannoses" = "oligomannose_relative"
  )
  
  replaced_vector <- vector("character", length = length(mouse_traits_ui_input))
  
  for (i in seq(length(mouse_traits_ui_input))) {
    trait_desc <- mouse_traits_ui_input[i]
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
  # Create an empty vector to store possible analytes with unknown glycan compositions
  unknown_glycans <- c()
  # Initiate an empty list
  formula_list <- vector("character", length(chosen_clusters))
  # Loop over the chosen clusters
  for (i in seq(length(chosen_clusters))) {
    # If the cluster name has "1" included at the end, remove it
    chosen_cluster <- chosen_clusters[[i]]
    cluster_name <- ifelse(
      stringr::str_ends(chosen_cluster, "1"),
      substr(chosen_cluster, 1, nchar(chosen_cluster) - 1),
      chosen_cluster
    )
    # Create an empty list to store the traits formulas for the cluster
    cluster_trait_formulas <- vector("character", length(chosen_traits))
    # Get the normalized data for the cluster
    cluster_normalized_data <- normalized_data %>% 
      dplyr::filter(cluster == chosen_cluster)
    # Get all analytes/glycans in the cluster
    cluster_analytes <- unique(cluster_normalized_data$analyte)
    cluster_glycans <- stringr::str_remove(cluster_analytes, paste0(cluster_name, "1"))
    # Check for unknown glycan compositions in the data
    cluster_unknown_glycans <- c()
    for (j in seq(length(cluster_glycans))) {
      if (!cluster_glycans[j] %in% reference$glycan) {
        cluster_unknown_glycans <- c(cluster_unknown_glycans, cluster_analytes[j])
      }
    }
    if (length(cluster_unknown_glycans) > 0) {
      unknown_glycans <- c(unknown_glycans, cluster_unknown_glycans)
    }
    # Get a subset of the reference file with only the passing analytes
    cluster_ref <- reference %>% 
      dplyr::filter(glycan %in% cluster_glycans)
    # Loop over the chosen traits
    for (k in seq(length(chosen_traits))) {
      trait <- chosen_traits[[k]]
      trait_formula <- generate_formula(cluster_name, cluster_ref, trait)
      cluster_trait_formulas[k] <- trait_formula
    }
    # Add the formulas for this cluster to formula_list
    formula_list[i] <- list(cluster_trait_formulas)
  }
  # Check if there are unknown glycan compositions
  if (length(unknown_glycans) > 0) {
    showNotification(
      paste0(
        "The following analytes in your data have unknown glycan compositions: ",
        paste0(unknown_glycans, collapse = ", "),
        ". Consider using the \"custom glycosylation traits\" option to include
        these analytes in your traits calculations."
      ),
      type = "warning", duration = NULL
    )
  }
  # Return the list with formulas for the traits
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



#' calculate_custom_traits
#' 
#' Calculate custom glycosylation traits based on an Excel file
#' provided by the user.
#'
#' @param traits_excel 
#' An Excel file with two columns: "trait" and "formula".
#' The "trait" column should contain the names of the traits.
#' The "formula" column should contain formulas that specify how to calculate the traits.
#' @param normalized_data_wide 
#' Data frame with normalized data in wide format.
#'
#' @return A wide dataframe with the normalized data + calculated custom traits.
#' 
calculate_custom_traits <- function(traits_excel, normalized_data_wide) {
  # Create vector with expressions for dplyr::mutate()
  expressions <- traits_excel %>% 
    dplyr::mutate(expression = paste0(trait, " = ", formula)) %>% 
    dplyr::pull(expression)
  # Loop over the expressions and create new columns
  data_with_custom_traits <- normalized_data_wide
  for (expr in expressions) {
    data_with_custom_traits <- data_with_custom_traits %>% 
      dplyr::mutate(!!!create_expr_ls(expr))
  }
  # Relocate the trait columns
  data_with_custom_traits <- data_with_custom_traits %>% 
    dplyr::relocate(tidyselect::all_of(traits_excel$trait), .after = replicates)
  
  return(data_with_custom_traits)
}

