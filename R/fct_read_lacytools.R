# This file contains all functions used in the module mod_read_lacytools.R.

# The list 'outputs' with LaCyTools outputs is used in the function
# 'read_lacytools'.
max_positive_charge <- 20
output_types <- list(
  "Absolute Intensity (Background Subtracted, ",
  "Mass Accuracy [ppm] (",
  "Isotopic Pattern Quality (",
  "S/N ("
)
outputs <- as.list(unlist(lapply(output_types, 
                                 paste0,
                                 seq_len(max_positive_charge),
                                 "+)")))

#' Read in non-rectangular delimited files
#' 
#' \code{read_non_rectangular()} can read flat files where the number of fields 
#' per line is not constant (non-rectangular data). Blank lines are not skipped 
#' and empty fields "" and 0's are interpreted as \code{NA}. This function was 
#' created to read LaCyTools summary files (tab-delimited non-rectangular .txt 
#' files).
#'
#' @param path A path to a file with non-rectangular data.
#' @param delim The field separator used in the file. For example, use "\\t" for 
#' files with tab-separated values and "," or ";" for files with comma-separated 
#' values (.csv).
#'
#' @return A data frame (\code{\link[base]{data.frame}}) containing the data in 
#' the flat file.
#' @export
#' 
#' @importFrom utils read.table
#'
#' @examples 
#' data_file <- system.file("extdata", 
#'                          "LaCyTools_summary_example.txt", 
#'                          package = "GlycoDash")
#' read_non_rectangular(path = data_file, delim = "\t")
#' 
read_non_rectangular <- function(path, delim = "\t") {
  
  max_n_columns <- find_widest_row(path = path,
                                   delim = delim)
  
  column_names <- vector()
  for (i in 1:max_n_columns) {
    column_names[i] <- paste("col", i, sep = "_")
  }
  
  data <- read.table(path, 
                     fill = TRUE, 
                     header = FALSE, 
                     col.names = column_names, 
                     sep = delim, 
                     blank.lines.skip = FALSE, 
                     na.strings = c("", "0"))
  return(data)
}

#' Find the widest row in a non-rectangular data file
#' 
#' \code{find_widest_row()} identifies the row/line in a non-rectangular data file
#' that contains the highest number of columns/fields. It is used in \code{\link{read_non_rectangular}}
#' to determine how many columns need to be read in.
#'
#' @inheritParams read_non_rectangular
#'
#' @return The number of fields/columns in the widest line of the non-rectangular data file (an integer).
#' @export
#'
#' @examples
#' data_file <- system.file("extdata", 
#'                          "LaCyTools_summary_example.txt", 
#'                          package = "GlycoDash")
#' find_widest_row(path = data_file, delim = "\t")
find_widest_row <- function(path, delim) {
  
  lines <- tryCatch(
    expr = { 
      readLines(path)
    },
    error = function(e) {
      rlang::abort(
        class = "wrong_path",
        message = "No such file or directory exists.")
    },
    warning = function(w) {
      rlang::abort(
        class = "embedded_null",
        message = w$message
      )  
    })
  
  if (rlang::is_empty(lines)) {
    rlang::abort(
      class = "empty_file",
      message = "One or more of the uploaded files are empty."
    )
  }
  
  columns <- stringr::str_split(lines, pattern = delim)
  max_n_columns <- max(purrr::map_int(columns, 
                                      ~ length(.x)))
  
  if (max_n_columns == 1) {
    rlang::warn(class = "wrong_delim",
                message = 
                  "One or more files seem to consist of a single column.
                  Please make sure that you chose the correct delimiter for your files.")
  }
  
  return(max_n_columns)
}

#'Convert a LaCyTools summary to a 'tidy' dataframe
#'
#'The function \code{convert_lacytools_summary()} can convert a dataframe 
#'containing a LaCyTools summary to a 'tidy' dataframe in long format. The long 
#'format means that for each sample there is one row per analyte per charge state.
#'
#'@param data A dataframe containing a LaCyTools summary returned by
#' \code{\link{read_non_rectangular}}.
#'
#'@return This function returns a dataframe with the following columns:
#'  \describe{ \item{sample_name}{The (unchanged) sample names.}
#'  \item{analyte}{The analyte. For each sample, there is one row per analyte
#'  per charge state of that analyte.} \item{charge}{The charge state of the
#'  analyte.} \item{LaCyTools output formats}{The dataframe will contain one
#'  column for each LaCyTools output format that was present in the LaCyTools
#'  summary file.} \item{fraction}{The fraction of the isotopic pattern that was
#'  included for the analyte.} \item{exact_mass}{The exact mass of the most
#'  abundant isotopologue of the analyte.}}
#'@export
#'
#' @examples
#' data("LaCyTools_summary")
#'
#' convert_lacytools_summary(data = LaCyTools_summary)
convert_lacytools_summary <- function(data) {
  
  all_blocks <- purrr::map(outputs, # outputs is a list created at the top of fct_read_lacytools.R
                           function(output) {
                             tryCatch(expr = {
                               suppressWarnings(
                                 get_block(data = data, 
                                           variable = output)
                               )
                             },
                             error = function(e) { })
                           })
  
  all_blocks <- all_blocks[!sapply(all_blocks, is.null)]
  
  if (rlang::is_empty(all_blocks)) {
    rlang::abort(class = "no_outputs_present",
                 message = paste(", none of the LaCyTools output variables are present.", 
                                 "Did you choose the correct file?"))
  }
  
  long_data_list <- purrr::map(all_blocks, lengthen_block)
  charges <- as.factor(purrr::map_chr(long_data_list, function(x) unique(x$charge)))
  charge_sep_list <- split(long_data_list, charges)
  
  analytes_info <- get_analytes_info_from_list(data, outputs)
  
  long_data <- purrr::map(charge_sep_list, function(x) purrr::reduce(x, dplyr::left_join)) %>%
    purrr::reduce(dplyr::full_join) %>% 
    dplyr::left_join(analytes_info, by = c("analyte", "charge"))
  
  return(long_data)
}

#' Create a subset containing one block from a LaCyTools summary.
#'
#' @inheritParams find_block
#'
#' @return A dataframe that is a subset of the input dataframe.
#' @export
#'
#' @examples
#' data(LaCyTools_summary)
#' get_block(LaCyTools_summary, 
#'           variable = "Absolute Intensity (Background Subtracted, 2+)")
#'           
get_block <- function(data, variable) {
  row_indices <- find_block(data, variable)
  block <- data[row_indices, ]
  # The first row of the block contains the column names for the block:
  colnames(block) <- unlist(block[1, ])
  # The first column should be named "sample_name":
  colnames(block)[1] <- "sample_name"
  
  # In case there are duplicated analyte names in the LaCyTools summary, apply
  # .name_repair (and issue a warning message)
  if (any(duplicated(colnames(block)))) {
    duplicated_analytes <- unique(colnames(block)[duplicated(colnames(block))])
    rlang::warn(class = "duplicated_analytes",
                message = paste0(# "In your LaCyTools summary file, ",
                                "the following analytes are present more than once: ",
                                paste(duplicated_analytes, collapse = ", "),
                                ". The names of the duplicates analytes are given",
                                "a suffix ('..columnnumber') to differentiate between them."))
    
    block <- suppressMessages(tibble::tibble(block,
                                             .name_repair = "universal"))
    
  }
  
  better_name_output <- stringr::str_remove_all(stringr::str_replace_all(tolower(variable), " ", "_"),
                                                "[\\(\\)\\,\\/\\[\\]]")
  
  # The first three rows of each block contain the column names, the fraction and the exact mass.
  # These rows should be removed:
  block <- block[-c(1, 2, 3), ] %>% 
    dplyr::mutate(lacytools_output = better_name_output) %>% 
    dplyr::mutate(dplyr::across(-c(sample_name, lacytools_output), as.numeric)) %>% 
    # Remove columns where all values are NAs:
    dplyr::select(-tidyselect::vars_select_helpers$where(function(x) all(is.na(x))))
  
  return(block)
}

#' Find a block in a LaCyTools summary file
#'
#' @inheritParams find_next_na
#' @param variable The name of a LaCyTools output format.
#'
#' @return The row indices of the block.
#' @export
#'
#' @examples
#'  data("LaCyTools_summary")
#'  find_block(data = LaCyTools_summary, variable = "S/N (2+)")
find_block <- function(data, variable) {
  first_row <- which(data[ , 1] == variable)
  if (rlang::is_empty(first_row)) {
    rlang::abort(
      class = "lacytools_output_not_found",
      message = paste("Error: LaCyTools output format",
                      variable, "is not present in the first column of the input file."))
  } else {
    next_na <- find_next_na(data, first_row)
    if (rlang::is_empty(next_na)) { 
      rows <- first_row:nrow(data) 
    } else {
      rows <- first_row:(next_na - 1)
    }
  }
  return(rows)
}

#'Find the next empty line from a given line in a LaCyTools summary file.
#'
#'@param data A dataframe with the LaCyTools summary (the result of
#'  \code{\link{read_non_rectangular}}).
#'@param row The row used as a starting point from which to search for the next
#'  blank line (blank meaning consisting of only \code{NA}'s).
#'
#'@return The row index (integer) for the next line with \code{NA}'s. If there
#'  are no next lines with \code{NA}'s the function will return an empty integer vector.
#'@export
#'
#'@examples
#' df <- data.frame(c("John", "Lisa", NA, "Pete", NA, "Paul"),
#'                  c(12, 15, NA, 14, NA, 23),
#'                  c("apple", "pear", NA, "pear", NA, "orange"))
#'                  
#' find_next_na(data = df,
#'              row = 2)
#'
#' find_next_na(data = df,
#'              row = 5)
#' 
find_next_na <- function(data, row) {
  # Find which rows in the first column contain NA's:
  na_index <- which(is.na(data[ , 1]))
  # Select only the rows that are below the row used as a starting point:
  later_nas <- na_index[which(purrr::map_lgl(na_index, ~ .x > row))]
  # From those, select the row that is closest to the starting row:
  next_na <- later_nas[which.min(purrr::map_int(later_nas, ~ .x - as.integer(row)))]
  return(next_na)
}



#' Transform a LaCyTools summary block to a long format.
#'
#' \code{lengthen_block()} transforms a LaCyTools summary block from a wide
#' format (each analyte has its own column) to a long format. A column named
#' "analyte" and a column named "charge" have been added and each combination 
#' of sample, analyte and charge state has its own row.
#'
#' @param block A dataframe containing a block from a LaCyTools summary file
#'   (the result of \code{\link{get_block}}).
#' @param metadata A dataframe containing metadata in case the metadata has
#'   already been added to the data. Defaults too \code{NULL}.
#'
#' @return A dataframe containing the LaCyTools summary block in long format.
#' @export
#'
#' @examples
#' data("LaCyTools_summary")
#' block <- get_block(LaCyTools_summary, 
#'                    variable = "Absolute Intensity (Background Subtracted, 2+)")
#' lengthen_block(block = block)
lengthen_block <- function(block, metadata = NULL) {
  charge_value <- stringr::str_extract(block$lacytools_output[1], "\\d+[+\\-]")
  # The charge needs to be removed from the analyte name:
  new_output_name <- stringr::str_remove(block$lacytools_output[1], "_\\d+[+\\-]")
  cols_not_to_pivot <- c("sample_name", "group", "plate_well", colnames(metadata))
  
  long_block <- block %>% 
    dplyr::select(-lacytools_output) %>%
    tidyr::pivot_longer(cols = -tidyselect::any_of(cols_not_to_pivot),
                        names_to = "analyte",
                        values_to = tidyselect::all_of(new_output_name)) %>% 
    dplyr::mutate(charge = charge_value) %>% 
    dplyr::relocate(charge, .before = all_of(new_output_name)) %>% 
    # Remove leading or trailing spaces from analyte
    dplyr::mutate(analyte = trimws(analyte))
  
  return(long_block)
}

#' Get the analytes info from a LaCyTools summary using a list of output formats
#'
#' This function uses \code{\link{get_analytes_info}} to get the exact mass of
#' the most abundant isotopologue and the fraction for each analyte in a
#' LaCyTools summary, for each charge state of those analytes. The reason that
#' the info is retrieved for each charge state is that the fraction of an
#' analyte can be different in different charge states. This can happen when
#' there are contaminants that need to be excluded near one charge state of an
#' analyte, but not near another charge state of that same analyte.
#'
#' @param data A dataframe with a LaCyTools summary.
#' @param list_of_variables A list/vector with the name of LaCyTools output
#'   formats.
#'
#' @return A dataframe with three columns (analyte, exact mass and fraction) and
#'   one row per analyte and charge combination.
#' @export
#'
#' @examples
#' data("LaCyTools_summary")
#' 
#' outputs <- list("Absolute Intensity (Background Subtracted, 2+)",
#'                 "Absolute Intensity (Background Subtracted, 3+)", 
#'                 "Mass Accuracy [ppm] (2+)", 
#'                 "Mass Accuracy [ppm] (3+)",
#'                 "Isotopic Pattern Quality (2+)",
#'                 "Isotopic Pattern Quality (3+)",
#'                 "S/N (2+)",
#'                 "S/N (3+)")
#' 
#' get_analytes_info_from_list(data = LaCyTools_summary, list_of_variables = outputs)
get_analytes_info_from_list <- function(data, list_of_variables) {
  # Get the analytes_info for each variable and put them in a list:
  analytes_info_list <- purrr::map(list_of_variables,
                                   function(variable) {
                                     analytes_info <- tryCatch({
                                       get_analytes_info(data, variable) %>% 
                                         dplyr::mutate(
                                           charge = stringr::str_extract(variable, 
                                                                         # This doesn't work for charges higher/lower than 9!
                                                                         "\\d+[+\\-]"))
                                     },
                                     # Ignore list items that result in an error:
                                     error = function(e) { })
                                   }) 
  
  # Throw error if no matches are found:
  if (rlang::is_empty(analytes_info_list)) {
    stop("No output formats in the list are present in the input summary file")
  }
  
  # Remove NULL items from the analytes_info_list:
  analytes_info_list <- analytes_info_list[!sapply(analytes_info_list, is.null)]
  
  # Find what charges are present in the LaCyTools summary:
  charges <- as.factor(purrr::map_chr(analytes_info_list, 
                                      function(x) unique(x$charge)))
  # Divide the analytes_info_list into one list per charge:
  charge_sep_list <- split(analytes_info_list,
                           charges)
  # Take the first analytes_info dataframe from each list:
  analytes_info <- purrr::map(charge_sep_list,
                              function(x) x[[1]]) %>% 
    purrr::reduce(., dplyr::full_join)
  return(analytes_info)
}

#' Get analytes info from a LaCyTools summary for one output format
#'
#' This function gets the exact mass of the most abundant isotopologue and the
#' fraction for each analyte in a LaCyTools summary, for a single output format.
#' This function is used within \code{\link{get_analytes_info_from_list}}.
#'
#' @inheritParams find_block
#'
#' @return A dataframe with three columns named "analyte", "exact_mass" and
#'   "fraction". The number of rows will correspond to the number of analytes in
#'   the data.
#' @export
#'
#' @examples
#' data("LaCyTools_summary")
#' get_analytes_info(data = LaCyTools_summary, variable = "S/N (2+)")
get_analytes_info <- function(data, variable) {
  # The row that in the first column contains the name of the LaCyTools output
  # format, contains the analyte names in the remaining columns. Find the index
  # of that row:
  row_index_analyte_names <- which(data[ , 1] == variable)
  if (rlang::is_empty(row_index_analyte_names)){
    stop(paste("The LaCyTools output format", variable,
               "is not present in the input summary file"))
  }
  # The two rows below the row with analyte names, contain the exact mass and
  # the fraction for each analyte. Subset those rows:
  analytes_info <- data[row_index_analyte_names:(row_index_analyte_names + 2), ]
  # Use the first row with analyte names as column names and replace the name of
  # the first column:
  colnames(analytes_info) <- unlist(analytes_info[1, ])
  colnames(analytes_info)[1] <- "info_variables"
  
  # .name_repair is used in case of duplicate analytes
  analytes_info <- suppressMessages(tibble::as_tibble(analytes_info,  name_repair = "universal"))

  # Pivot the dataframe and do some formatting:
  analytes_info <- analytes_info %>%
    dplyr::slice(-1) %>% 
    tidyr::pivot_longer(cols = -info_variables,
                        names_to = "analyte", 
                        values_to = "value") %>%
    tidyr::pivot_wider(names_from = info_variables) %>% 
    # Remove leading or trailing spaces from the analyte column
    dplyr::mutate(analyte = trimws(analyte)) %>% 
    # I don't rename the columns directly using new_name = old_name, because in
    # different versions of LaCyTools these columns are named differently
    # ("Exact mass of most abundant isotopologue" in one version and
    # "Monoisotopic mass" in the other):
    dplyr::rename(fraction = tidyselect::contains("fraction"),
                  exact_mass = tidyselect::contains("mass")) %>% 
    dplyr::mutate(exact_mass = purrr::map_chr(exact_mass,
                                              function(x) stringr::str_remove_all(x, 
                                                                                  "[\\[\\]]"))) %>% 
    dplyr::mutate(dplyr::across(-analyte, ~ suppressWarnings(as.numeric(.x))))
  
  return(analytes_info)
}

#'Detect whether a sample is specific or total Ig based on the sample name.
#'
#'@param block A dataframe containing a LaCyTools summary with a column 
#'"sample_name".
#'@param keyword_specific The word(s)/characters within the sample name used to
#'  refer to Specific samples.
#'@param keyword_total The word(s)/characters within the sample name used to
#'  refer to Total samples.
#'
#'@return The dataframe containing a block from a LaCyTools summary file, with
#'  an additional column named "group" that indicates whether a sample is
#'  specific or total.
#'@export
#'
#'@examples
#'block_example <- data.frame(sample_name = c("s_0216_Specific", "s_568_Total","s_8759"),
#'                            values = c(13.56, 738.34, 4.56))
#'detect_group(block = block_example, keyword_specific = "Specific", keyword_total = "Total")
detect_group <- function(data, keyword_specific, keyword_total) {
  data <- data %>% 
    tidyr::extract(col = sample_name,
                   into = "group",
                   regex = paste0("(", keyword_specific, "|", 
                                  keyword_total, ")"),
                   remove = FALSE) %>% 
    dplyr::mutate(group = as.factor(group))
  
  if (!(keyword_specific %in% levels(data$group))) {
    rlang::abort(class = "unmatched_keyword_specific",
                 message = paste("This keyword for specific samples did not match", 
                                 "any sample names in your data. Please choose a different keyword."))
  }
  
  if (!(keyword_total %in% levels(data$group))) {
    rlang::abort(class = "unmatched_keyword_total",
                 message = paste("This keyword for total samples did not match", 
                                 "any sample names in your data. Please choose a different keyword."))
  }
  
  if (any(is.na(data$group))) {
    rlang::warn(class = "NAs",
                message = paste("Some sample names could not be classified as total or specific Ig.",
                                "Please reconsider your keywords."))
  }
  return(data)
}



# A function to generate ordinal suffixes
getOrdinalSuffix <- function(num) {
  if (num %% 10 == 1 && num %% 100 != 11) {
    return(paste0(num, "st"))
  } else if (num %% 10 == 2 && num %% 100 != 12) {
    return(paste0(num, "nd"))
  } else if (num %% 10 == 3 && num %% 100 != 13) {
    return(paste0(num, "rd"))
  } else {
    return(paste0(num, "th"))
  }
}



#' read_skyline_csv
#'
#' @param path_to_file Path to Skyline CSV file
#'
#' @return Dataframe with raw data read from CSV
read_skyline_csv <- function(path_to_file) {
  L <- readLines(path_to_file, n = 1)
  if (grepl(";", L)) {
    raw_data <- read.csv(path_to_file, header = TRUE, sep = ";")
  } else {
    raw_data <- read.csv(path_to_file, header = TRUE, sep = ",")
  }
  return(raw_data)
}



#' rename_skyline_isomers
#' 
#' This function detects the presence of isomers in a Skyline CSV file.
#' When an analyte is present twice in a given charge state, the two duplicate
#' analytes are assumed to be isomers with the same glycan composision. The glycan
#' compositions are renamed to distinguish them.
#' 
#' @param raw_skyline_data 
#' Imported skyline CSV data from the read_skyline_csv() function.
#' 
#' @param i Number of the Skyline CSV file that is being processed
#'
#' @return
#' Skyline CSV data with glycan compositions of isomers renamed using "_a", "_b", etc.
#' 
rename_skyline_isomers <- function(raw_skyline_data, i) {
  
  # Look for isomers in the glycan compositions, per peptide.
  data <- raw_skyline_data %>% 
    dplyr::group_by(Protein.Name, Peptide, Precursor.Charge) %>% 
    dplyr::mutate(n = dplyr::n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::relocate(n, .after = Precursor.Charge)
  
  # n == 1 implies unique glycan composition
  data_unique <- data %>% 
    dplyr::filter(n == 1)
  
  # n > 1 implies presence of isomers
  data_isomers <- data %>% 
    dplyr::filter(n > 1) %>% 
    dplyr::group_by(Protein.Name, Precursor.Charge, Peptide) %>% 
    dplyr::mutate(Peptide_unique = make.unique(Peptide)) %>% 
    dplyr::ungroup() %>% 
    # Instead of ".1", ".2", etc at the end of duplicates, add "_a","_b", to 
    # the ends of all isomers, including the first one.
    dplyr::mutate(
      Peptide = dplyr::case_when(
        endsWith(Peptide_unique, ".1") ~ paste0(Peptide, "_b"),
        endsWith(Peptide_unique, ".2") ~ paste0(Peptide, "_c"),
        endsWith(Peptide_unique, ".3") ~ paste0(Peptide, "_d"),
        endsWith(Peptide_unique, ".4") ~ paste0(Peptide, "_e"),
        endsWith(Peptide_unique, ".5") ~ paste0(Peptide, "_f"),
        endsWith(Peptide_unique, ".6") ~ paste0(Peptide, "_g"),
        endsWith(Peptide_unique, ".7") ~ paste0(Peptide, "_h"),
        endsWith(Peptide_unique, ".8") ~ paste0(Peptide, "_i"),
        endsWith(Peptide_unique, ".9") ~ paste0(Peptide, "_j"),
        .default = paste0(Peptide, "_a")
      )
    ) %>% 
    dplyr::select(-Peptide_unique)
  
  # Combine the data again
  data_renamed <- dplyr::bind_rows(data_unique, data_isomers)
  
  # Show a notification if isomers were detected
  if (length(data_isomers$Peptide > 0)) {
    # Get vector with the compositions for which isomers were detected
    isomeric <- data %>% 
      dplyr::filter(n > 1) %>% 
      dplyr::select(Protein.Name, Peptide) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(analyte = paste0(Protein.Name, "1", Peptide)) %>% 
      dplyr::pull(analyte)
    # Show notification with message
    message <- paste0(
      "In CSV file ", i, ", the following ", length(isomeric), 
      " analytes with isomeric glycan compositions were detected and renamed: ",
      paste0(isomeric , collapse = ", ")
    )
    showNotification(message, type = "warning", duration = 30)
  }
  
  return(data_renamed)
}



#' transform_skyline_data_wide
#'
#' @param raw_skyline_data_wide Raw skyline data in wide format, read into R with 
#' the read_skyline_csv() function above.
#' 
#' @param i Number of the Skyline CSV file that is being processed (1, 2, 3, etc)
#' Required for the rename_skyline_isomers() function to display notifications.
#'
#' @return A clean dataframe in a similar format as a transformed
#' LaCyTools summary. The dataframe contains the following columns:
#' - sample_name
#' - analyte
#' - charge
#' - absolute_intensity_background_subtracted
#' - mass_accuracy_ppm
#' - isotope_dot_product
#' 
transform_skyline_data_wide <- function(raw_skyline_data_wide, i) {
  # Check structure of skyline data
  check_skyline_data(raw_skyline_data_wide)
  # Check for isomers and rename them if they are present
  renamed_data <- rename_skyline_isomers(raw_skyline_data_wide, i)
  # Select required columns
  raw_data_required <- renamed_data %>% 
    dplyr::select(
      "Protein.Name", "Peptide", "Precursor.Charge",
      tidyselect::contains(c("Total.Area.MS1", "Isotope.Dot.Product", "Average.Mass.Error.PPM")), 
    )
  # Select notes
  # Will have 3 cols if notes exist, otherwise 2 cols
  notes <- renamed_data %>% 
    dplyr::select("Protein.Name", "Peptide", "Precursor.Charge", tidyselect::contains("Note")) %>% 
    dplyr::mutate(analyte = paste0(Protein.Name, "1", Peptide), .after = Peptide) %>% 
    dplyr::select(-Protein.Name, -Peptide) %>% 
    dplyr::rename(charge = Precursor.Charge) %>% 
    dplyr::mutate(charge = paste0(charge, "+")) %>% 
    dplyr::rename(note = tidyselect::contains("Note"))
  # Make columns numeric except for first three
  raw_data_required[raw_data_required == "#N/A"] <- NA
  raw_data_required <- dplyr::mutate_at(raw_data_required, dplyr::vars(-1, -2, -3), as.numeric)
  # Transform the data
  cols_to_pivot <- colnames(raw_data_required)[-(1:3)]
  raw_data_long <- raw_data_required %>% 
    tidyr::pivot_longer(tidyselect::all_of(cols_to_pivot))
  variables <- c("Total.Area.MS1", "Isotope.Dot.Product", "Average.Mass.Error.PPM")
  # Initiate empty list to store DFs
  var_dfs <- vector("list", length = length(variables))
  # Loop over the variables
  # Create separate dataframe for each variable
  # Create columns sample_name and variable
  for (i in seq(length(variables))) {
    var <- variables[i]
    var_data_long <- raw_data_long %>% 
      dplyr::filter(grepl(var, name)) %>% 
      tidyr::separate(
        name,
        sep = paste0(".", var),
        into = c("sample_name", "variable")
      ) %>% 
      dplyr::mutate(variable = var) %>% 
      # Combine Protein.Name and Peptide into analyte
      dplyr::mutate(analyte = paste0(Protein.Name, "1", Peptide)) %>% 
      dplyr::select(-Protein.Name, -Peptide)
    var_dfs[[i]] <- var_data_long
  }
  # Combine required data and turn into wide format
  data_clean <- dplyr::bind_rows(var_dfs) %>% 
    dplyr::group_by(variable) %>% 
    dplyr::mutate(row = dplyr::row_number()) %>% 
    tidyr::pivot_wider(names_from = variable, values_from = value) %>% 
    dplyr::select(-row) %>% 
    dplyr::ungroup() %>% 
    # Get into same format as processed LaCyTools summary
    dplyr::rename(
      charge = Precursor.Charge,
      total_area = Total.Area.MS1,
      isotope_dot_product = Isotope.Dot.Product,
      mass_accuracy_ppm = Average.Mass.Error.PPM
    ) %>% 
    dplyr::mutate(charge = paste0(charge, "+")) %>% 
    dplyr::relocate(charge, .after = analyte) %>% 
    dplyr::relocate(total_area, .after = charge) %>% 
    dplyr::relocate(mass_accuracy_ppm, .after = total_area)
  # Add notes if they exist
  if (ncol(notes) == 3) {
    data_clean <- dplyr::left_join(data_clean, notes) %>% 
      dplyr::relocate(note, .after = charge)
  }
  
  return(data_clean)
}



# Function to check the structure of Skyline CSV files.
check_skyline_data <- function(raw_skyline_data) {
  # Check if required columns are present in the file
  required_cols <- c("Protein.Name", "Peptide", "Precursor.Charge")
  cols_check <- required_cols %in% colnames(raw_skyline_data)
  missing_cols <- required_cols[!cols_check]
  if (length(missing_cols) > 0) {
    rlang::abort(
      class = "missing_columns",
      message = paste0(
        "the following columns are missing from your data: ",
        paste0(gsub("\\.", " ", missing_cols), collapse = ", ")
      )
    )
  }
  # Check if required variables per sample name are present in the file
  required_vars <- c("Total.Area.MS1", "Isotope.Dot.Product", "Average.Mass.Error.PPM") 
  vars_check <- sapply(required_vars, function(x) any(grepl(x, colnames(raw_skyline_data))))
  missing_vars <- required_vars[!vars_check]
  if (length(missing_vars) > 0) {
    rlang::abort(
      class = "missing_variables",
      message = paste0(
        "the following variables are missing from your data: ",
        paste0(gsub("\\.", " ", missing_vars), collapse = ", ")
      )
    )
  }
  # Check if Protein.Name contains spaces or letters
  contains_numbers_or_spaces <- grepl("[0-9 ]", raw_skyline_data$Protein.Name)
  if (any(contains_numbers_or_spaces)) {
    rlang::abort(
      class = "numbers_or_spaces",
      message = paste0(
        "please remove any numbers or spaces from all entries in the column \"Protein Name\"."
      )
    )
  }
}





