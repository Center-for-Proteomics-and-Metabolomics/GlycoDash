# This file contains all functions used in the module mod_read_lacytools.R.

# The list 'outputs' with LacyTools outputs is used in the function
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
#' created to read LacyTools summary files (tab-delimited non-rectangular .txt 
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
#'                          "LacyTools_summary_example.txt", 
#'                          package = "glycodash")
#' read_non_rectangular(path = data_file, delim = "\t")
#' 
read_non_rectangular <- function(path, delim = "\t") {
  
  lines <- tryCatch(
    expr = { 
      readLines(path)
    },
    error = function(e) {
      stop("No such file or directory exists.")
    })
  
  columns <- stringr::str_split(lines, pattern = delim)
  max_n_columns <- max(purrr::map_int(columns, ~ length(.x)))
  
  if (max_n_columns == 1) {
    rlang::warn(class = "wrong_delim",
                message = paste(
                  "The file seems to consist of a single column.",
                  "Are you sure that you chose the correct delimiter for your file?"
                ))
  }
  
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

#' Create a subset containing one block from a LacyTools summary.
#'
#' @inheritParams find_block
#'
#' @return A dataframe that is a subset of the input dataframe.
#' @export
#'
#' @examples
#' data(LacyTools_summary)
#' get_block(LacyTools_summary, 
#'           variable = "Absolute Intensity (Background Subtracted, 2+)")
#'           
get_block <- function(data, variable) {
  rows <- find_block(data, variable)
  block <- data[rows, ]
  # The first row of the block contains the column names for the block:
  colnames(block) <- unlist(block[1, ])
  # The first column should be named "sample_name":
  colnames(block)[1] <- "sample_name"
  better_name_output <- stringr::str_remove_all(stringr::str_replace_all(tolower(variable), " ", "_"),
                                                "[\\(\\)\\,\\/\\[\\]]")
  # The first three rows of each block contain the column names, the fraction and the exact mass.
  # These rows should be removed:
  
  # In case there are duplicated analyte names in the LacyTools summary, apply
  # .name_repair (and issue a warning message --> IMPLEMENT THIS)
  if (any(duplicated(colnames(block)))) {
    duplicated_analytes <- unique(colnames(block)[duplicated(colnames(block))])
    rlang::warn(class = "duplicated_analytes",
                message = paste("In your LacyTools summary file, the analytes",
                                paste(duplicated_analytes, collapse = " "),
                                "are present more than once. The names of the", 
                                "duplicated analytes are given a suffix", 
                                "('..columnnumber') to differentiate between them."))
  }
  
  block <- suppressMessages(tibble::tibble(block,
                                           .name_repair = "universal"))
  
  block <- block[-c(1, 2, 3), ] %>% 
    dplyr::mutate(lacytools_output = better_name_output) %>% 
    dplyr::mutate(dplyr::across(-c(sample_name, lacytools_output), as.numeric)) %>% 
    dplyr::select(-tidyselect::vars_select_helpers$where(function(x) all(is.na(x))))
  return(block)
}

#' Find a block in a LacyTools summary file
#'
#' @inheritParams find_next_na
#' @param variable The name of a LacyTools output format.
#'
#' @return The row indices of the block.
#' @export
#'
#' @examples
#'  data("LacyTools_summary")
#'  find_block(data = LacyTools_summary, variable = "S/N (2+)")
find_block <- function(data, variable) {
  first_row <- which(data[ , 1] == variable)
  if (rlang::is_empty(first_row)) {
    rlang::abort(
      class = "lacytools_output_not_found",
      message = paste("Error: LacyTools output format",
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

#'Find the next empty line from a given line in a LacyTools summary file.
#'
#'@param data A dataframe with the LacyTools summary (the result of
#'  \code{\link{read_non_rectangular}}).
#'@param row The row used as a starting point from which to search for the next
#'  blank line (consisting of only \code{NA}'s).
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

#'Convert a LacyTools summary to a 'tidy' dataframe
#'
#'The function \code{convert_lacytools_summary()} can convert a dataframe 
#'containing a LacyTools summary to a 'tidy' dataframe in long format. The long 
#'format means that for each sample there is one row per analyte per charge state.
#'
#'@param data A dataframe containing a LacyTools summary returned by
#' \code{\link{read_non_rectangular}}.
#'
#'@return This function returns a dataframe with the following columns:
#'  \describe{ \item{sample_name}{The (unchanged) sample names.}
#'  \item{analyte}{The analyte. For each sample, there is one row per analyte
#'  per charge state of that analyte.} \item{charge}{The charge state of the
#'  analyte.} \item{LacyTools output formats}{The dataframe will contain one
#'  column for each LacyTools output format that was present in the LacyTools
#'  summary file.} \item{fraction}{The fraction of the isotopic pattern that was
#'  included for the analyte.} \item{exact_mass}{The exact mass of the most
#'  abundant isotopologue of the analyte.}}
#'@export
#'
#' @examples
#' data("LacyTools_summary")
#'
#' convert_lacytools_summary(data = LacyTools_summary)
convert_lacytools_summary <- function(data) {

  all_blocks <- purrr::map(outputs,
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
                 message = paste("None of the LacyTools output variables are", 
                                 "present in the first column of the LacyTools",
                                 "summary file. Did you choose the correct file?"))
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

#' Transform a LacyTools summary block to a long format.
#'
#' \code{lengthen_block()} transforms a LacyTools summary block from a wide
#' format (each analyte has its own column) to a long format. A column named
#' "analyte" and a column named "charge" have been added and each combination 
#' of sample, analyte and charge state has its own row.
#'
#' @param block A dataframe containing a block from a LacyTools summary file
#'   (the result of \code{\link{get_block}}).
#' @param metadata A dataframe containing metadata in case the metadata has
#'   already been added to the data. Defaults too \code{NULL}.
#'
#' @return A dataframe containing the LacyTools summary block in long format.
#' @export
#'
#' @examples
#' data("LacyTools_summary")
#' block <- get_block(LacyTools_summary, 
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
    dplyr::relocate(charge, .before = all_of(new_output_name))
  
  return(long_block)
}

#' Get the analytes info from a LacyTools summary using a list of output formats
#'
#' This function uses \code{\link{get_analytes_info}} to get the exact mass of
#' the most abundant isotopologue and the fraction for each analyte in a
#' LacyTools summary, for each charge state of those analytes. The reason that
#' the info is retrieved for each charge state is that the fraction of an
#' analyte can be different in different charge states. This can happen when
#' there are contaminants that need to be excluded near one charge state of an
#' analyte, but not near another charge state of that same analyte.
#'
#' @param data A dataframe with a LacyTools summary.
#' @param list_of_variables A list/vector with the name of LacyTools output
#'   formats.
#'
#' @return A dataframe with three columns (analyte, exact mass and fraction) and
#'   one row per analyte and charge combination.
#' @export
#'
#' @examples
#' data("LacyTools_summary")
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
#' get_analytes_info_from_list(data = LacyTools_summary, list_of_variables = outputs)
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
  
  # Find what charges are present in the LacyTools summary:
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

#' Get analytes info from a LacyTools summary for one output format
#'
#' This function gets the exact mass of the most abundant isotopologue and the
#' fraction for each analyte in a LacyTools summary, for a single output format.
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
#' data("LacyTools_summary")
#' get_analytes_info(data = LacyTools_summary, variable = "S/N (2+)")
get_analytes_info <- function(data, variable) {
  # The row that in the first column contains the name of the LacyTools output
  # format, contains the analyte names in the remaining columns. Find the index
  # of that row:
  row_index_analyte_names <- which(data[ , 1] == variable)
  if (rlang::is_empty(row_index_analyte_names)){
    stop(paste("The LacyTools output format", variable,
               "is not present in the input summary file"))
  }
  # The two rows below the row with analyte names, contain the exact mass and
  # the fraction for each analyte. Subset those rows:
  analytes_info <- data[row_index_analyte_names:(row_index_analyte_names + 2), ]
  # Use the first row with analyte names as column names and replace the name of
  # the first column:
  colnames(analytes_info) <- unlist(analytes_info[1, ])
  colnames(analytes_info)[1] <- "info_variables"
  
  analytes_info <- suppressMessages(tibble::as_tibble(analytes_info,
                                                      .name_repair = "universal"))
  
  # Pivot the dataframe and do some formatting:
  analytes_info <- analytes_info %>%
    dplyr::slice(n = -1) %>% 
    tidyr::pivot_longer(cols = -info_variables,
                        names_to = "analyte", 
                        values_to = "value") %>%
    tidyr::pivot_wider(names_from = info_variables) %>% 
    # I don't rename the columns directly using new_name = old_name, because in
    # different versions of LacyTools these columns are named differently
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

#'Detect whether a sample is Specific or Total Ig based on the sample name.
#'
#'@param block A dataframe containing a LacyTools summary with a column 
#'"sample_name".
#'@param keyword_specific The word(s)/characters within the sample name used to
#'  refer to Specific samples.
#'@param keyword_total The word(s)/characters within the sample name used to
#'  refer to Total samples.
#'
#'@return The dataframe containing a block from a LacyTools summary file, with
#'  an additional column named "group" that indicates whether a sample is
#'  Specific or Total.
#'@export
#'
#'@examples
#'block_example <- data.frame(sample_name = c("s_0216_Specific", "s_568_Total","s_8759"),
#'                             values = c(13.56, 738.34, 4.56))
#'detect_group(block = block_example, keyword_specific = "Specific", keyword_total = "Total")
detect_group <- function(data, keyword_specific, keyword_total) {
  data <- data %>% 
    tidyr::extract(col = sample_name,
                   into = "group",
                   regex = paste0("(", keyword_specific, "|", 
                                  keyword_total, ")"),
                   remove = FALSE)
  
  if (all(!sapply(data$group, identical, keyword_specific))) {
    rlang::abort(class = "unmatched_keyword_specific",
                 message = paste("This keyword for specific samples did not match", 
                                 "any sample names in your data. Please choose a different keyword."))
  }
  
  if (all(!sapply(data$group, identical, keyword_total))) {
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
