# TODO: add roxygen skeleton
generate_formula <- function(cluster, cluster_ref_df, target_trait) {
  
  # Get the glycans that should be used for calculating the trait
  df <- cluster_ref_df %>% 
    dplyr::select(glycan, tidyselect::all_of(target_trait)) %>% 
    dplyr::filter(!!rlang::sym(target_trait) != 0)
  
  # Create a string with the right hand side of the formula
  formula_string <- paste0(df[[target_trait]], " * ", df$glycan, collapse = " + ")
  
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
  
  # Add the left hand side of the formula
  final_formula_string <- ifelse(
    # If there are no glycans for the trait, than the formula is " * "
    # In that case the value should just be zero.
    test = clean_formula_string == " * ",
    yes = paste0(cluster, "_", target_trait, " = ", "0"),
    no = paste0(cluster, "_", target_trait, " = ", clean_formula_string)
  )
  
  return(final_formula_string)
}