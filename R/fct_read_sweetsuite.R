#' Read one or more SweetSuite output Excel files
#' 
#' @description 
#' Reads the \code{"Data"} sheet from one or more SweetSuite output Excel
#' files, checks for required columns, renames columns for consistency with
#' LaCyTools data, and returns all files combined into a single dataframe.
#' 
#' @param datapaths A character vector of file paths to the SweetSuite output Excel files.
#' 
#' @return A dataframe combining the \code{"Data"} sheets from all input Excel
#'   files, with columns renamed to match the LaCyTools data format.
read_sweetsuite_data <- function(datapaths) {
  
  result <- lapply(seq_along(datapaths), function(i) {
    datapath <- datapaths[i]
    # Check if "Data" sheet exists
    sheets <- readxl::excel_sheets(datapath)
    if (!"Data" %in% sheets) {
      rlang::abort(
        class = "missing_sheet",
        message = paste0("The 'Data' sheet is missing from file number: ", i)
      )
    }
    
    # Read the data
    ncols <- ncol(readxl::read_excel(datapath, sheet = "Data", n_max = 0))
    data <- readxl::read_excel(
      datapath, sheet = "Data",
      # Explicitly specify col_types because columns from isotopic_fraction 
      # onward may have no values in the first 1000 rows (readxl's default guess_max). 
      # Without this, readxl guesses those columns as logical instead of numeric.
      col_types = c(
        "text", "text", # `file` and `analyte`,
        rep("numeric", ncols - 2)  # Remaining columns
      )
    )
    
    # Check for required columns
    required_cols <- c(
      "file", "analyte", "charge", "mz_exact", "isotopic_fraction",
      "total_area_background_subtracted", "mass_error_ppm",
      "isotopic_pattern_quality", "signal_to_noise"
    )
    missing_cols <- setdiff(required_cols, colnames(data))
    if (length(missing_cols) > 0) {
      rlang::abort(
        class = "missing_columns",
        message = paste0(
          "The following required columns are missing from the 'Data' sheet in file number ",
          i, ": ", paste(missing_cols, collapse = ", ")
        )
      )
    }
    
    # Rename columns for consistency with LaCyTools data
    data <- data %>%
      dplyr::rename(
        sample_name = file,
        fraction = isotopic_fraction,
        absolute_intensity_background_subtracted = total_area_background_subtracted,
        mass_accuracy_ppm = mass_error_ppm,
        sn = signal_to_noise
      ) %>%
      dplyr::mutate(charge = as.integer(charge))
    
    return(data)
  })
  
  return(dplyr::bind_rows(result))
}

