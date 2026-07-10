#' Read a Skyline CSV file
#'
#' @description 
#' Reads a Skyline-exported CSV file, automatically detecting whether the
#' delimiter is a comma or semicolon by inspecting the first line.
#'
#' @param path_to_file Path to Skyline CSV file.
#'
#' @return A dataframe with the raw data read from the CSV.
read_skyline_csv <- function(path_to_file) {
  L <- readLines(path_to_file, n = 1)
  if (grepl(";", L)) {
    raw_data <- read.csv(path_to_file, header = TRUE, sep = ";")
  } 
  else {
    raw_data <- read.csv(path_to_file, header = TRUE, sep = ",")
  }
  
  return(raw_data)
}



#' Check the structure of a Skyline CSV file
#'
#' @description 
#' Verifies that a Skyline CSV dataframe contains all required per-sample
#' variable columns (\code{Total.Area.MS1}, \code{Isotope.Dot.Product}, and
#' \code{Average.Mass.Error.PPM}). Shows an informative error message and
#' returns NULL if data is missing. Returns the data otherwise.
#'
#' @param raw_skyline_data A dataframe of raw Skyline data, as returned by
#'   \code{\link{read_skyline_csv}}.
#'
#' @return NULL if required columns are missing, otherwise returns the data.
check_skyline_data <- function(raw_skyline_data) {
  
  required_vars <- c("Total.Area.MS1", "Isotope.Dot.Product", "Average.Mass.Error.PPM") 
  vars_check <- sapply(required_vars, function(x) any(grepl(x, colnames(raw_skyline_data))))
  missing_vars <- required_vars[!vars_check]
  
  if (length(missing_vars) > 0) {
    message <- paste0(
      "The following variables are missing from your data: ",
      paste0(gsub("\\.", " ", missing_vars), collapse = ", ")
    )
    showNotification(message, type = "error", duration = NULL)
    
    return(NULL)
  }
  
  return(raw_skyline_data)
}



#' Reformat a wide Skyline dataframe that uses a single analyte column
#'
#' @description 
#' When Skyline data is exported with a single column for the full glycopeptide
#' analyte (rather than separate cluster and glycan columns), this function
#' parses that column to extract the peptide sequence, glycan composition,
#' methionine oxidation count, and glycosylation site abbreviation. The
#' resulting dataframe uses the same \code{cluster}/\code{glycan} column
#' structure expected by \code{\link{transform_skyline_data}}.
#'
#' @param raw_skyline_data A dataframe of raw Skyline data in wide format,
#'   as returned by \code{\link{read_skyline_csv}}.
#' @param protein_colname Name of the column containing protein identifiers.
#' @param analyte_colname Name of the column containing full glycopeptide
#'   analyte identifiers (including modification annotations).
#' @param charge_colname Name of the column containing charge states.
#' @param notes_colname Name of an optional column containing per-analyte notes.
#'   Pass \code{NULL} if not present.
#' @param molecular_formula_colname Name of an optional column containing
#'   molecular formulas. Pass \code{NULL} if not present.
#'
#' @return A dataframe with columns \code{protein}, \code{peptide_sequence},
#'   \code{cluster}, \code{glycan}, \code{methionine_oxidation}, \code{charge},
#'   and optionally \code{note} and \code{molecular_formula}, followed by the
#'   per-sample measurement columns (\code{Total.Area.MS1},
#'   \code{Isotope.Dot.Product}, \code{Average.Mass.Error.PPM},
#'   \code{Best.Retention.Time}, \code{Min.Start.Time}, \code{Max.End.Time}).
reformat_skyline_analyte_column <- function(
      raw_skyline_data, 
      protein_colname,
      analyte_colname, 
      charge_colname,
      notes_colname,
      molecular_formula_colname
  ) {
  
  # Rename columns
  data_renamed_cols <- raw_skyline_data %>% 
    dplyr::rename(
      protein = tidyselect::all_of(protein_colname),
      glycopeptide = tidyselect::all_of(analyte_colname),
      charge = tidyselect::all_of(charge_colname)
    )
  
  # Conditionally add the note and molecular formula columns
  if (!is.null(notes_colname)) {
    data_renamed_cols <- data_renamed_cols %>% 
      dplyr::rename(note = tidyselect::all_of(notes_colname))
  }
  
  if (!is.null(molecular_formula_colname)) {
    data_renamed_cols <- data_renamed_cols %>% 
      dplyr::rename(molecular_formula = tidyselect::all_of(molecular_formula_colname))
  }
  
  # Select required data.
  raw_data_required <- data_renamed_cols %>% 
    dplyr::select(
      tidyselect::any_of(c("note", "molecular_formula")),
      protein, glycopeptide, charge, 
      tidyselect::contains(c(
        "Total.Area.MS1", "Isotope.Dot.Product", "Average.Mass.Error.PPM",
        "Best.Retention.Time", "Min.Start.Time", "Max.End.Time"
      ))
    )
  
  # Reformat and annotate data
  raw_data_modifications <- raw_data_required %>%
    dplyr::mutate(
      # Count number of oxidized methionines
      oxidation = stringr::str_count(
        glycopeptide, "\\[Oxidation \\(M\\)\\]|\\[Oxi\\]"
      ),
      # Remove CAM modifications
      glycopeptide_cam_removed = stringr::str_remove_all(
        glycopeptide, "\\[Carbamidomethyl \\(C\\)\\]|\\[CAM\\]"
      ),
      # Remove oxidation to extract unmodified peptide
      glycopeptide_oxi_removed = stringr::str_remove_all(
        glycopeptide_cam_removed, "\\[Oxidation \\(M\\)\\]|\\[Oxi\\]"
      ),
      # Extract glycan and peptide sequence
      glycan = stringr::str_extract(glycopeptide_oxi_removed, "(?<=\\[).+?(?=\\])"),
      peptide = stringr::str_replace_all(glycopeptide_oxi_removed, "\\[.+?\\]", ""),
      
      .after = charge
    )
  
  # Generate abbreviations for glycosylation sites
  glycosites <- abbreviate_glycosites(
    protein_peptide_df = data.frame(
      protein = raw_data_modifications$protein,
      peptide = raw_data_modifications$peptide
    )
  )
  
  # Final processing
  data_reformatted <- raw_data_modifications %>% 
    dplyr::left_join(glycosites) %>% 
    dplyr::relocate(abbreviation, .after = peptide) %>% 
    dplyr::rename(cluster = abbreviation) %>% 
    dplyr::mutate(
      cluster = dplyr::case_when(
        oxidation > 0 ~ paste0(cluster, strrep("Ox", oxidation)),
        TRUE ~ cluster
      )
    ) %>% 
    # Move some columns to front
    dplyr::select(
      tidyselect::any_of(c("note", "molecular_formula")),
      protein, peptide, cluster, glycan, oxidation, charge,
      tidyselect::contains(c(
        "Total.Area.MS1", "Isotope.Dot.Product", "Average.Mass.Error.PPM",
        "Best.Retention.Time", "Min.Start.Time", "Max.End.Time"
      ))
    ) %>% 
    dplyr::mutate(oxidation = as.character(oxidation)) %>% 
    dplyr::rename(peptide_sequence = peptide) %>% 
    dplyr::rename(methionine_oxidation = oxidation)
  
  return(data_reformatted)
}



#' Reformat a wide Skyline dataframe with separate cluster and glycan columns
#'
#' @description 
#' Renames the selected Skyline columns to the standardized column names used
#' by downstream import steps and keeps the per-sample measurement columns.
#' This function is used for Skyline exports where glycosylation site or
#' cluster information and glycan composition are already present in separate
#' columns.
#'
#' @param raw_skyline_data A dataframe of raw Skyline data in wide format,
#'   as returned by \code{\link{read_skyline_csv}}.
#' @param cluster_colname Name of the column containing glycosylation site or
#'   cluster identifiers.
#' @param glycan_colname Name of the column containing glycan compositions.
#' @param charge_colname Name of the column containing charge states.
#' @param notes_colname Name of an optional column containing per-analyte notes.
#'   Pass \code{NULL} if not present.
#' @param molecular_formula_colname Name of an optional column containing
#'   molecular formulas. Pass \code{NULL} if not present.
#'
#' @return A dataframe with columns \code{cluster}, \code{glycan}, \code{charge},
#'   and optionally \code{note} and \code{molecular_formula}, followed by the
#'   per-sample measurement columns (\code{Total.Area.MS1},
#'   \code{Isotope.Dot.Product}, \code{Average.Mass.Error.PPM},
#'   \code{Best.Retention.Time}, \code{Min.Start.Time}, \code{Max.End.Time}).
reformat_skyline_data <- function(
    raw_skyline_data,  
    cluster_colname,
    glycan_colname,
    charge_colname,
    notes_colname,
    molecular_formula_colname
) {
  # Rename columns
  data_renamed_cols <- raw_skyline_data %>% 
    dplyr::rename(
      cluster = tidyselect::all_of(cluster_colname),
      glycan = tidyselect::all_of(glycan_colname),
      charge = tidyselect::all_of(charge_colname)
    )
  
  # Conditionally add the note and molecular formula columns
  if (!is.null(notes_colname)) {
    data_renamed_cols <- data_renamed_cols %>% 
      dplyr::rename(note = tidyselect::all_of(notes_colname))
  }
  
  if (!is.null(molecular_formula_colname)) {
    data_renamed_cols <- data_renamed_cols %>% 
      dplyr::rename(molecular_formula = tidyselect::all_of(molecular_formula_colname))
  }
  
  # Select required columns.
  data_reformatted <- data_renamed_cols %>% 
    dplyr::select(
      tidyselect::any_of(c("note", "molecular_formula")),
      cluster, glycan, charge,
      tidyselect::contains(c(
        "Total.Area.MS1", "Isotope.Dot.Product", "Average.Mass.Error.PPM",
        "Best.Retention.Time", "Min.Start.Time", "Max.End.Time"
      ))
    )
  
  return(data_reformatted)
}



#' Rename isomeric glycan compositions in Skyline data
#' 
#' @description 
#' Detects the presence of isomers in a Skyline CSV file. When an analyte is
#' present twice in a given charge state, the two duplicate analytes are assumed
#' to be isomers with the same glycan composition. The glycan compositions are
#' renamed to distinguish them by appending suffixes \code{"_a"}, \code{"_b"},
#' etc. A Shiny notification is shown if isomers are detected.
#' 
#' @param data_renamed_cols A dataframe of imported Skyline CSV data (from
#'   \code{\link{read_skyline_csv}}) with columns renamed to \code{"cluster"},
#'   \code{"glycan"} and \code{"charge"}.
#'
#' @return A dataframe with the same structure as \code{data_renamed_cols}, with
#'   glycan compositions of isomers renamed using \code{"_a"}, \code{"_b"},
#'   etc.
rename_skyline_isomers <- function(data_renamed_cols) {
  
  # Look for isomers in the glycan compositions, per peptide.
  data <- data_renamed_cols %>% 
    dplyr::group_by(
      dplyr::across(tidyselect::any_of(c("protein"))), 
      cluster, glycan, charge
    ) %>% 
    dplyr::mutate(n = dplyr::n()) %>% 
    dplyr::ungroup()
  
  # n == 1 implies unique glycan composition
  data_unique <- data %>% 
    dplyr::filter(n == 1)
  
  # n > 1 implies presence of isomers
  data_isomers <- data %>% 
    dplyr::filter(n > 1) %>% 
    dplyr::group_by(
      dplyr::across(tidyselect::any_of(c("protein"))), 
      cluster, glycan, charge
    ) %>% 
    dplyr::mutate(glycan_unique = make.unique(glycan)) %>% 
    dplyr::ungroup() %>% 
    # Instead of ".1", ".2", etc at the end of duplicates, add "_a","_b", to 
    # the ends of all isomers, including the first one.
    dplyr::mutate(
      glycan = dplyr::case_when(
        endsWith(glycan_unique, ".1") ~ paste0(glycan, "_b"),
        endsWith(glycan_unique, ".2") ~ paste0(glycan, "_c"),
        endsWith(glycan_unique, ".3") ~ paste0(glycan, "_d"),
        endsWith(glycan_unique, ".4") ~ paste0(glycan, "_e"),
        endsWith(glycan_unique, ".5") ~ paste0(glycan, "_f"),
        endsWith(glycan_unique, ".6") ~ paste0(glycan, "_g"),
        endsWith(glycan_unique, ".7") ~ paste0(glycan, "_h"),
        endsWith(glycan_unique, ".8") ~ paste0(glycan, "_i"),
        endsWith(glycan_unique, ".9") ~ paste0(glycan, "_j"),
        .default = paste0(glycan, "_a")
      )
    ) %>% 
    dplyr::select(-glycan_unique)
  
  # Combine the data again
  data_renamed <- dplyr::bind_rows(data_unique, data_isomers) %>% 
    dplyr::select(-n)
  
  # Show a notification if isomers were detected
  if (nrow(data_isomers) > 0) {
    # Get vector with the compositions for which isomers were detected
    isomeric <- data %>% 
      dplyr::filter(n > 1) %>% 
      dplyr::select(cluster, glycan) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(analyte = paste0(cluster, "1", glycan)) %>% 
      dplyr::pull(analyte)
    # Show notification with message
    message <- paste0(
      "The following ", length(isomeric), 
      " analytes with isomeric glycan compositions were detected and renamed: ",
      paste0(isomeric , collapse = ", ")
    )
    showNotification(message, type = "warning", duration = 30)
  }
  
  return(data_renamed)
}



#' Transform wide Skyline data into a tidy analyte-sample format
#'
#' @description
#' Reshapes a wide-format Skyline data frame (one row per analyte, one column
#' group per sample-variable combination) into a tidy format with one row per
#' analyte-sample combination and one column per measurement variable.
#'
#' Sample-variable columns are assumed to start immediately after
#' `Precursor.Charge` and follow the naming convention `<SampleID>.<Variable>`,
#' where `<Variable>` is one of `Best.Retention.Time`, `Total.Area.MS1`,
#' `Isotope.Dot.Product`, `Average.Mass.Error.PPM`, `Normalized.Area`,
#' `Min.Start.Time`, or `Max.End.Time`.
#'
#' @param data_renamed A data frame as returned by [rename_skyline_isomers()].
#'
#' @return A data frame with one row per analyte-sample combination and
#'   individual columns for each Skyline measurement variable.
reshape_skyline_data <- function(data_renamed) {
  # Assume variable columns for samples start after `charge`.
  # Everything after that point belongs to one sample-variable combination such
  # as "L20252001084c Best Retention Time" or "L20252001084c Min Start Time".
  # These column names will be split into:
  #   sample   = L20252001084c
  #   variable = Best.Retention.Time / Min.Start.Time / ...
  start_idx = which(colnames(data_renamed) == "charge") + 1
  variable_cols = colnames(data_renamed)[
    start_idx:length(colnames(data_renamed))
  ]
  
  data_long <- data_renamed %>%
    dplyr::mutate(
      # Convert Skyline measurement columns to numeric.
      # Notes:
      # - "#N/A" should become NA
      # - some Skyline exports prefix scientific notation with "*"
      #   (for example "*2.4246E+7"), which must be removed before conversion
      dplyr::across(
        .cols = tidyselect::all_of(variable_cols),
        .fns = ~ .x %>%
          # Force to character
          as.character() %>%
          # Turn "#N/A" into `NA`
          dplyr::na_if("#N/A") %>%
          # Skyline can prefix scientific notation with "*"
          stringr::str_remove("^\\*") %>%
          # Back to numeric
          as.numeric()
      )
    ) %>%
    tidyr::pivot_longer(
      tidyr::all_of(variable_cols), names_to = "sample_variable"
    ) %>%
    tidyr::extract(
      # Split original Skyline column name into sample ID and measurement type
      col   = sample_variable,
      into  = c("sample", "variable"),
      regex = paste0(
        "^(.+)\\.(",
        "Best\\.Retention\\.Time|",
        "Total\\.Area\\.MS1|",
        "Isotope\\.Dot\\.Product|",
        "Average\\.Mass\\.Error\\.PPM|",
        "Normalized\\.Area|",
        "Min\\.Start\\.Time|",
        "Max\\.End\\.Time",
        ")$"
      ),
      remove = TRUE
    )
  
  # Reshape back to wide format, but now with one row per analyte-sample
  # combination and one column per measurement variable.
  data_wide <- data_long %>%
    tidyr::pivot_wider(names_from = "variable", values_from = "value")
  
  return(data_wide)
}

