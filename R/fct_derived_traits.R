#' generate_formula
#'
#' Generates a formula for a glycan trait, based on a specified cluster,
#' trait reference file and a trait name that is present in the reference file as a column,
#' 
#' @param cluster Cluster name, e.g. "IgGI"
#' @param cluster_ref_df Reference file for traits, e.g. human_IgG_N_ref filtered with only glycans
# that passed the analyte curation.
#' @param target_trait   # Trait for which a formula should be created, e.g. "galactosylation"
#'
#' @return  A character string with a formula
generate_formula <- function(cluster, cluster_ref_df, target_trait) {
  
  # Get the glycans that should be used for calculating the trait
  df <- cluster_ref_df %>% 
    dplyr::select(glycan, tidyselect::all_of(target_trait)) %>%   # target_trait is just one trait
    dplyr::filter(!!rlang::sym(target_trait) != 0)
  
  # Check the number of glycans used for calculating the trait.
  if (nrow(df) == 0) { 
    return(paste0(cluster, "_", target_trait, " = Not reported: zero for all samples"))
  } 
  # If there is only one glycan: do not report if trait is average number of mannoses
  else if (nrow(df) == 1 & target_trait == "oligomannose_average") {
    return(paste0(cluster, "_", target_trait, " = Not reported: only one relevant glycan ", df$glycan))
  }
  # Check if "complex" is a column in cluster_ref_df
  else if ("complex" %in% colnames(cluster_ref_df)) {
    # Some traits are always 100 when all glycans are used
    if (nrow(cluster_ref_df %>% dplyr::filter(complex == 1)) == nrow(df)) {
      if (target_trait %in% c(
        "fucosylation", "core_fucosylation", "antennary_fucosylation",
        "bisection", "mono_antennary", "tri_antennary"
      )) {
        return(paste0(cluster, "_", target_trait, " = Not reported: 100 for all samples"))
      }
    }
    else if (nrow(cluster_ref_df) == nrow(df)) {
      if (target_trait %in% c(
        "hybrid", "hybrid_fucosylation", "hybrid_bisection", "oligomannose"
      )) {
        return(paste0(cluster, "_", target_trait, " = Not reported: 100 for all samples"))
      }
    }
  }
  
  # TODO: check for use of all glycans minus 1 --> not always a problem?

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
  if (target_trait %in% c("fucosylation", "bisection", "galactosylation", "sialylation",
                          "mono_antennary", "tri_antennary", "antennarity", 
                          "antennary_fucosylation", "alpha_galactosylation")) {
    complex_types_df <- cluster_ref_df %>% 
      dplyr::filter(complex == 1)
    # Check if all passing glycans were already complex-type, if not adjust formula
    if (nrow(complex_types_df) != nrow(cluster_ref_df)) {
      # String with sum of complex type glycans
      complex_sum <- paste0(cluster, "1", complex_types_df$glycan, collapse = " + ")
      # Adjust clean_formula_string to divide by complex_types
      # Multiple by 100 if trait is not antennarity
      clean_formula_string <- paste0("(", clean_formula_string, ") / (", complex_sum, ")")
      if (target_trait != "antennarity") {
        clean_formula_string <- paste0(clean_formula_string, " * 100")
      }
    }
  }
  # Divide by the sum of all oligomannose type glycans
  else if (target_trait == "oligomannose_average") {
    oligomannose_df <- cluster_ref_df %>%
      dplyr::filter(oligomannose_average != 0)
    oligomannose_sum <- paste0(cluster, "1", oligomannose_df$glycan, collapse = " + ")
    clean_formula_string <- paste0("(", clean_formula_string, ") / (", oligomannose_sum, ")") 
  }
  # Divide by sum of hybrids when calculating hybrid fucosylation or bisection
  else if (target_trait %in% c("hybrid_fucosylation", "hybrid_bisection")) {
    hybrid_df <- cluster_ref_df %>% 
      dplyr::filter(hybrid == 1)
    hybrid_sum <- paste0(cluster, "1", hybrid_df$glycan, collapse = " + ")
    clean_formula_string <- paste0("(", clean_formula_string, ") / (", hybrid_sum, ") * 100")
  }
  # Divide some O-glycan traits by 100
  else if (target_trait %in% c("sialic_acids", "galactoses", "galnacs",
                               "Tn_antigens", "T_antigens", "sT_antigens")) {
    clean_formula_string <- paste0("(", clean_formula_string, ") / 100")
  }
  
  # Change name of target trait if there is only one glycan used
  if (nrow(df) == 1) {
    target_trait <- paste0(target_trait, "_", df$glycan)
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




#' Matches traits descriptions from UI to column names in traits reference files.
#'
#' @param traits_ui_input  Character vector from UI traits input
#'
#' @return  Character vector with column names of traits that should be calculated,
#'          matching those in the traits reference files. 
match_traits <- function(traits_ui_input) {
  traits <- c(
    # Trait names to replace the descriptions with
    "Fucosylation of complex-type glycans" = "fucosylation",
    "Core fucosylation of complex-type glycans" = "fucosylation",
    "Antennary fucosylation of complex-type glycans" = "antennary_fucosylation",
    "Bisection of complex-type glycans" = "bisection",
    "Galactosylation per antenna of complex-type glycans" = "galactosylation",
    "Sialylation per antenna of complex-type glycans" = "sialylation",
    "Antennarity of complex-type glycans" = "antennarity",
    "Percentage of monoantennary complex-type glycans" = "mono_antennary",
    "Percentage of triantennary complex-type glycans" = "tri_antennary",
    "Percentage of hybrid-type glycans" = "hybrid",
    "Fucosylation of hybrid-type glycans" = "hybrid_fucosylation",
    "Bisection of hybrid-type glycans" = "hybrid_bisection",
    "Percentage of oligomannose-type glycans" = "oligomannose",
    "Oligomannose-type glycans: average number of mannoses" = "oligomannose_average",
    "Sialic acids" = "sialic_acids",
    "Galactoses" = "galactoses",
    "GalNAcs" = "galnacs",
    "Tn antigens" = "Tn_antigens",
    "T antigens" = "T_antigens",
    "Sialyl-T (sT) antigens" = "sT_antigens",
    "Disialylated O-antigens" = "disialylated_O_antigens",
    "\u03B1-1,3-galactosylation of complex-type glycans" = "alpha_galactosylation",
    "Sialylation (N-glycolylneuraminic acid) per antenna of complex-type glycans" = "sialylation"
  )
  matched_traits <- traits_ui_input
  for (description in names(traits)) {
    matched_traits[matched_traits == description] <- traits[[description]]
  }
  # Remove traits that are calculated based on other traits
  to_remove <- c(
    "Sialylation per galactose of complex-type glycans",
    "Sialic acids per galactose",
    "Galactoses per GalNAc"
  )
  matched_traits_clean <- matched_traits[!matched_traits %in% to_remove]
  
  return(matched_traits_clean)
}



#' create_formula_list
#'
#' Creates a list of formulas for automatically calculating derived traits
#'
#' @param normalized_data  normalized_data in long format
#' @param chosen_traits Character vector, e.g.  c("fucosylation", "sialylation")
#' @param chosen_clusters  Character vector, e.g. c("IgGI", "IgGII")
#' @param reference Reference file for traits, e.g. human_IgG_N_ref.
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
    # Create an empty list to store the traits formulas for the cluster.
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
    # Loop over the chosen traits to automatically generate formulas based on reference list.
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
  colnames <- colnames(normalized_data_wide_with_traits)
  sum_int_colnames <- colnames[grepl("_sum_intensity", colnames)]
  last_sum_int_colname <- tail(sum_int_colnames, 1)
  normalized_data_wide_with_traits <- normalized_data_wide_with_traits %>% 
    dplyr::relocate(tidyselect::all_of(trait_names), .after = last_sum_int_colname)
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
  tryCatch(
    return(rlang::set_names(list(str2lang(expr_code)), expr_nm)),
    error = function(e) {
      return(rlang::set_names(list(expr_code), expr_nm))
    }
  )
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
  colnames <- colnames(data_with_custom_traits)
  sum_int_colnames <- colnames[grepl("_sum_intensity", colnames)]
  last_sum_int_colname <- tail(sum_int_colnames, 1)
  data_with_custom_traits <- data_with_custom_traits %>% 
    dplyr::relocate(tidyselect::all_of(traits_excel$trait), .after = last_sum_int_colname)
  
  return(data_with_custom_traits)
}




traits_vs_intensity_plot <- function(data_to_plot, cluster) {
  
  data_to_plot <- data_to_plot %>% 
    dplyr::filter(!is.na(relative_abundance))
  
  my_palette <- color_palette(length(unique(data_to_plot$sample_type)))
  
  xvar <- paste0(cluster, "_sum_intensity")
  
  p <- ggplot2::ggplot(data_to_plot, ggplot2::aes(
    x = .data[[xvar]], y = .data[["relative_abundance"]],
    text = paste0(
      "Sample name: ", sample_name, "\n",
      "Sample ID: ", sample_id, "\n",
      "Relative abundance: ", format(round(relative_abundance, digits = 2), nsmall = 2), "\n",
      paste(cluster, "sum intensity: "), round(.data[[xvar]], digits = 0)
    )
  )) +
    ggplot2::geom_point(ggplot2::aes(color = sample_type), size = 1, alpha = 0.7) +
    ggplot2::labs(x = paste(cluster, "sum intensity"), y = "Relative abundance") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(color = "black", fill = NA, size = 0.5),
      strip.background = ggplot2::element_rect(fill = "#F6F6F8")
    ) +
    ggplot2::scale_color_manual(values = my_palette, name = "Sample type")
  
  if ("group" %in% colnames(data_to_plot)) {
    p <- p + ggplot2::facet_grid(trait ~ group, scales = "free")
  } else {
    p <- p + ggplot2::facet_wrap(~trait, scales = "free", ncol = 3)
  }
  
  return(p)
}




clean_traits <- function(trait_formulas, normalized_data_wide) {
  
  # Create a named list with all the trait formulas
  formulas <- trait_formulas 
  traits_names <- gsub(" = .*", "", formulas) 
  names(formulas) <- traits_names
  
  # Sialylation per galactose traits: check if sialylation is reported.
  # If not: don't report sialylation per galactose
  sial_per_gal_traits <- traits_names[endsWith(traits_names, "_sialylation_per_galactose")]
  other_traits <- traits_names[!endsWith(traits_names, "_sialylation_per_galactose")]
  for (trait in sial_per_gal_traits) {
    sial_trait <- gsub("_per_galactose", "", trait)  # <cluster>_sialylation
    sial_trait <- other_traits[grep(sial_trait, other_traits)]  # Sial trait name can have suffix _<glycan>
    if (grepl(" = Not reported", formulas[[sial_trait]])) {
      formulas[[trait]] <- paste0(trait, " = Not reported: sialylation not reported")
    }
  }
  
  # Get the formulas that should be used for calculating traits.
  # Then calculate the traits.
  formulas_calculate <- unname(formulas[!grepl(" = Not reported", formulas)])
  if (length(formulas_calculate) > 0) {
    
    data <- calculate_traits(normalized_data_wide, formulas_calculate)
    
    # Galactosylation traits: check if all values are the same
    gal_traits <- intersect(
      other_traits[grep("_galactosylation", other_traits)],
      colnames(data)
    )
    for (trait in gal_traits) {
      # Get all unique values (need rounding)
      unique <- unique(round(data[[trait]][!is.na(data[[trait]])], digits = 3))
      if (length(unique) == 1) {
        # Remove trait from data
        data[[trait]] <- NULL
        # Change trait formula
        formulas[[trait]] <- paste0(
          trait, " = Not reported: ", unique, " for all samples"
        )
        # Remove sialylation per galactose if it exists
        cluster <- sub("^(.*?)_.*", "\\1", trait)
        sial_per_gal <- paste0(cluster, "_sialylation_per_galactose")
        if (sial_per_gal %in% traits_names) {
          data[[sial_per_gal]] <- NULL
          formulas[[sial_per_gal]] <- paste0(
            sial_per_gal, " = Not reported: galactosylation is not reported"
          )
        }
      }
    }
    
    # Sialylation traits: check if all values are the same
    sial_traits <- intersect(
      other_traits[grep("_sialylation", other_traits)],
      colnames(data)
    )
    for (trait in sial_traits) {
      # Get all unique values (need rounding)
      unique <- unique(round(data[[trait]][!is.na(data[[trait]])], digits = 3))
      if (length(unique) == 1) {
        # Remove trait from data
        data[[trait]] <- NULL
        # Change trait formula
        formulas[[trait]] <- paste0(
          trait, " = Not reported: ", unique, " for all samples"
        )
        # Remove sialylation per galactose if it exists
        cluster <- sub("^(.*?)_.*", "\\1", trait)
        sial_per_gal <- paste0(cluster, "_sialylation_per_galactose")
        if (sial_per_gal %in% traits_names) {
          data[[sial_per_gal]] <- NULL
          formulas[[sial_per_gal]] <- paste0(
            sial_per_gal, " = Not reported: sialylation is not reported"
          )
        }
      }
    }
    
    # Antennarity traits: check if all values are the same
    antennarity_traits <- intersect(
      traits_names[endsWith(traits_names, "_antennarity")],
      colnames(data)
    )
    for (trait in antennarity_traits) {
      unique <- unique(round(data[[trait]][!is.na(data[[trait]])], digits = 3))
      if (length(unique) == 1) {
        data[[trait]] <- NULL
        formulas[[trait]] <- paste0(
          trait, " = Not reported: ", unique, " for all samples"
        )
      }
    }
    
  } else {
    # No formulas to calculate
    data <- normalized_data_wide
  }
  
  return(list(
    "formulas_toshow" = unname(formulas),
    "data" = data
  ))
  
}

  
