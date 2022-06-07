outputs <- list("Absolute Intensity (Background Subtracted, 1+)",
                "Absolute Intensity (Background Subtracted, 2+)",
                "Absolute Intensity (Background Subtracted, 3+)", 
                "Absolute Intensity (Background Subtracted, 4+)",
                "Absolute Intensity (Background Subtracted, 5+)",
                "Absolute Intensity (Background Subtracted, 6+)",
                "Absolute Intensity (Background Subtracted, 7+)",
                "Absolute Intensity (Background Subtracted, 8+)",
                "Absolute Intensity (Background Subtracted, 9+)",
                "Absolute Intensity (Background Subtracted, 10+)",
                "Absolute Intensity (Background Subtracted, 11+)",
                "Absolute Intensity (Background Subtracted, 12+)",
                "Absolute Intensity (Background Subtracted, 13+)",
                "Mass Accuracy [ppm] (1+)",
                "Mass Accuracy [ppm] (2+)",
                "Mass Accuracy [ppm] (3+)",
                "Mass Accuracy [ppm] (4+)",
                "Mass Accuracy [ppm] (5+)",
                "Mass Accuracy [ppm] (6+)",
                "Mass Accuracy [ppm] (7+)",
                "Mass Accuracy [ppm] (8+)",
                "Mass Accuracy [ppm] (9+)",
                "Mass Accuracy [ppm] (10+)",
                "Mass Accuracy [ppm] (11+)",
                "Mass Accuracy [ppm] (12+)",
                "Mass Accuracy [ppm] (13+)",
                "Isotopic Pattern Quality (1+)",
                "Isotopic Pattern Quality (2+)",
                "Isotopic Pattern Quality (3+)",
                "Isotopic Pattern Quality (4+)",
                "Isotopic Pattern Quality (5+)",
                "Isotopic Pattern Quality (6+)",
                "Isotopic Pattern Quality (7+)",
                "Isotopic Pattern Quality (8+)",
                "Isotopic Pattern Quality (9+)",
                "Isotopic Pattern Quality (10+)",
                "Isotopic Pattern Quality (11+)",
                "Isotopic Pattern Quality (12+)",
                "Isotopic Pattern Quality (13+)",
                "S/N (1+)",
                "S/N (2+)",
                "S/N (3+)",
                "S/N (4+)",
                "S/N (5+)",
                "S/N (6+)",
                "S/N (7+)",
                "S/N (8+)",
                "S/N (9+)",
                "S/N (10+)",
                "S/N (11+)",
                "S/N (12+)",
                "S/N (13+)")

#' Read in non-rectangular delimited files
#' 
#' \code{read_non_rectangular()} can read flat files where the number of fields per line is 
#' not constant (non-rectangular data). Blank lines in the field are not skipped 
#' and empty fields "" are interpreted as \code{NA}. This function was created to read
#' LacyTools summary files (tab-delimited .txt files).
#'
#' @param path A path to a file with non-rectangular data.
#' @param delim The field separator used in the file. 
#'              For example, use "\\t" for files with tab-separated values 
#'              and "," or ";" for files with comma-separated values (.csv).
#'
#' @return A data frame (\code{\link[base]{data.frame}}) containing the data in the flat file.
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
  tryCatch(lines <- readLines(path, n = -1, ok = TRUE),
           error = function(e) stop("No such file or directory exists."))
  columns <- stringr::str_split(lines, pattern = delim)
  n_columns <- max(purrr::map_int(columns, ~ length(.x)))
  if (n_columns == 1) {
    rlang::warn(class = "wrong_delim",
                message = paste("The file seems to consist of a single column.",
                                "Are you sure that you chose the correct delimiter for your file?"))
  }
  column_names <- vector()
  for (i in 1:n_columns) {
    column_names[i] <- paste("col", i, sep = "_")
  }
  data <- read.table(path, fill = TRUE, header = FALSE, col.names = column_names, 
                     sep = delim, blank.lines.skip = FALSE, na.strings = c("", "0"))
  return(data)
}

#'Find the next empty line from a given line in a LacyTools summary file.
#'
#'@param data A dataframe with the LacyTools summary (the result of
#'  \code{\link{read_non_rectangular}}).
#'@param row The row used as a starting point from which to search for the next
#'  line with \code{NA}'s.
#'
#'@return The row index (integer) for the next line with \code{NA}'s. If there
#'  are no next lines with \code{NA}'s the function will return an empty integer vector.
#'@export
#'
#'@examples
#' df <- data.frame(c("John", "Lisa", "Paul", NA, "Pete", NA),
#'                  c(12, 15, 23, NA, 14, NA),
#'                  c("apple", "pear", "orange", NA, "pear", NA))
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

#'Detect whether a sample is Specific or Total Ig based on the sample name.
#'
#'@param block A dataframe containing a block from a LacyTools summary file.
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
                message = paste("Some sample_names could not be classified as total or specific.",
                                "Please reconsider your keywords (keyword_specific and keyword_total)."))
  }
  return(data)
}

#' Create a subset containing one block from a LacyTools summary.
#'
#' @inheritParams find_block
#' @inheritParams detect_group
#' @param Ig_data A character string. Is your data immunoglobulin data that
#'   contains both total Ig and specific Ig samples? If yes then \code{Ig_data}
#'   should be "Yes" if not \code{Ig_data} should be "No".
#'
#' @return A dataframe that is a subset of the input dataframe.
#' @export
#'
#' @examples
#' data(LacyTools_summary)
#' get_block(LacyTools_summary, 
#'           variable = "Absolute Intensity (Background Subtracted, 2+)", 
#'           Ig_data = "Yes", 
#'           keyword_specific = "Spike", 
#'           keyword_total = "Total")
#'           
get_block <- function(data, variable, Ig_data = NULL, keyword_specific = NULL, keyword_total= NULL) {
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
  #block <- detect_plate_and_well(block)
  # if (Ig_data == "Yes") {
  #   block <- detect_group(block, keyword_specific, keyword_total)
  # }
  return(block)
}

#' Detect the plate and well of a sample from the sample name.
#'
#' This function detects the plate and well position of a sample, based on it's
#' sample name. The sample name should be in a column named "sample_name" and
#' should contain either "P", "p", "Pl" or "PL" followed by the plate number or
#' a capital letter possibly separated by an underscore (_), a score (-) or a
#' dot (.). In addition, the sample name should contain a single capital letter
#' between A and H followed directly by a number between 1 and 12 (numbers
#' smaller than 10 may be preceded by a zero, e.g. A01 and A1 are both allowed).
#' This letter-number combination indicates the well.
#'
#' @param data A dataframe. Should include a column named "sample_name".
#'
#' @return The input dataframe with an added column named "plate_well" that
#'   indicates on which plate and in which well a sample was analysed.
#' @export
#'
#' @examples
#' block_example <- data.frame(sample_name = c("s_0216_Specific_pl_3.A10",
#'                                             "s_568_Total_P5_H4",
#'                                             "pl23_B6.s_8759"),
#'                             values = c(13.56, 738.34, 4.56))
#' detect_plate_and_well(block_example)
detect_plate_and_well <- function(data) {
  
  if (!("sample_name" %in% colnames(data))) {
    rlang::abort(class = "no_sample_name_column",
                 message = "The data doesn't contain the required column \"sample_name\".")
  }
  
  data <- data %>% 
    tidyr::extract(
      col = sample_name, 
      into = c("plate", "well"),
      # Plate number/letter does not have to be preceded by a P/p, but between
      # the plate number and well position only a _ - . or space are allowed:
      regex = "([Pp]?(?:[Ll]?|late)[_\\-.\\s]?(?:\\d+|[A-Z]))[_\\-.\\s]?([A-H][_\\-.\\s]?(?:0?\\d\\D|0?\\d$|1[012]))",
      remove = FALSE)
  
  if (any(anyNA(data$plate), anyNA(data$well))) {
    data <- data %>% 
      dplyr::select(-c(plate, well)) %>% 
      tidyr::extract(
        col = sample_name, 
        into = c("plate", "well"),
        # Plate number/letter has to be preceded by at least a P/p but there can
        # be all kinds of characters between the plate number and the well position:
        regex = "([Pp](?:[Ll]?|late)[_\\-.\\s]?(?:\\d+|[A-Z])).*([A-H][_\\-.\\s]?(?:0?\\d\\D|0?\\d$|1[012]))",
        remove = FALSE)
  }
  
  if (any(anyNA(data$plate), anyNA(data$well))) {
    data <- data %>% 
      dplyr::select(-c(plate, well)) %>% 
      tidyr::extract(
        col = sample_name, 
        into = c("well", "plate"),
        # The well position directly precedes the plate number (can only be
        # separated by a _ - . or space), but the plate number/letter does not
        # need to be preceded by a P/p:
        regex = "([A-H][_\\-.\\s]?(?:0?\\d\\D|0?\\d$|1[012]))[_\\-.\\s]?([Pp]?(?:[Ll]?|late)[_\\-.\\s]?(?:\\d+|[A-Z]))",
        remove = FALSE)
  }
  
  if (any(anyNA(data$plate), anyNA(data$well))) {
    NA_samples <- data$sample_name[is.na(data$plate) | is.na(data$well)]
    if (length(NA_samples) > 15) {
      rlang::abort(class = "plate_well NAs",
                   message = paste("For",
                                   length(NA_samples),
                                   "samples the plate and well could not be determined.",
                                   "Run `?detect_plate_and_well` and check if your sample names are in a suitable format."))
    } else {
      rlang::abort(class = "plate_well NAs",
                   message = paste("For the sample(s)",
                                   paste(NA_samples, collapse = " and "),
                                   "the plate and well could not be determined."))
    }
  }
  
  data <- data %>% 
    dplyr::mutate(plate = stringr::str_match(plate, "[Pp]?[Ll]?(?:ate)?[_\\-.\\s]?(\\d+|[A-Z])")[ , 2],
                  well = stringr::str_extract(well, "[A-H][_\\-.\\s]?\\d{1,2}"),
                  plate_well = paste(plate, well, sep = "_")) %>% 
    dplyr::select(-c(plate, well)) %>% 
    dplyr::relocate(plate_well, .after = sample_name)
  
  return(data)
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
                                     tryCatch({
                                       analytes_info <- get_analytes_info(data, variable) %>% 
                                         # add a column that indicates what the charge state is:
                                         # include this in get_analytes_info()? add an error?
                                         dplyr::mutate(
                                           charge = stringr::str_extract(variable, 
                                                                         # This doesn't work for charges higher/lower than 9!
                                                                         "\\d+[+\\-]"))
                                       return(analytes_info)
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

#' Transform the LacyTools summary data to a long format.
#'
#' \code{create_long_data()} transforms a LacyTools summary block from a wide
#' format (each analyte has it's own column) to a long format (an column named
#' "analyte" with the analytes has been added and each combination of sample and
#' analyte has it's own row). In addition, a column is added that contains
#' the charge of the LacyTools output format.
#'
#' @param block A dataframe containing a block from a LacyTools summary file
#'   (the result of \code{\link{get_block}}).
#' @param metadata A dataframe containing metadata in case the metadata has
#'   already been added to the data.Defaults too \code{NULL}.
#'
#' @return A dataframe containing the LacyTools summary block in long format.
#' @export
#'
#' @examples
#' data("LacyTools_summary")
#' block <- get_block(LacyTools_summary, 
#'                    variable = "Absolute Intensity (Background Subtracted, 2+)", 
#'                    Ig_data = "Yes", 
#'                    keyword_specific = "Spike", 
#'                    keyword_total = "Total")
#' create_long_data(block = block)
create_long_data <- function(block, metadata = NULL) {
  charge_value <- stringr::str_extract(block$lacytools_output[1], "\\d+[+\\-]")
  new_output_name <- stringr::str_remove(block$lacytools_output[1], "_\\d+[+\\-]")
  cols_not_to_pivot <- c("sample_name", "group", "plate_well", colnames(metadata))
  
  block <- block %>% 
    dplyr::select(-lacytools_output) %>%
    tidyr::pivot_longer(cols = -tidyselect::any_of(cols_not_to_pivot),
                 names_to = "analyte",
                 values_to = tidyselect::all_of(new_output_name)) %>% 
    dplyr::mutate(charge = charge_value) %>% 
    dplyr::relocate(charge, .before = all_of(new_output_name))
  
  return(block)
}

#'Read a LacyTools summary file and convert it to a 'tidy' dataframe
#'
#'The function \code{read_lacytools_summary()} can read in a LacyTools summary
#'.txt file and convert it to a 'tidy' dataframe in long format.
#'
#'@param summary_file The path to the LacyTools summary file. This file should
#'  be a tab-delimited .txt file. This function can process the output summary
#'  file of LacyTools without any changes needed.
#'  
#'@inheritParams get_block
#'
#'@return This function returns a dataframe with the following columns:
#'  \describe{ \item{sample_name}{The (unchanged) sample names.}
#'  \item{plate_well}{The plate and well that the sample was analyzed in. This
#'  information is deduced from the sample name using the function
#'  \code{\link{detect_plate_and_well}}} \item{group}{The group (total or
#'  specific Ig) that the sample belongs to. The groups are deduced from the
#'  sample name using the function \code{\link{detect_group}}}
#'  \item{analyte}{The analyte. For each sample, there is one row per analyte
#'  (per charge state of that analyte).} \item{charge}{The charge state of the
#'  analyte.} \item{LacyTools output formats}{The dataframe will contain one
#'  column for each LacyTools output format that was present in the LacyTools
#'  summary file.} \item{fraction}{The fraction of the isotopic pattern that was
#'  included for the analyte.} \item{exact_mass}{The exact mass of the most
#'  abundant isotopologue of the analyte.}}
#'@export
#'
#' @examples
#' path_to_file <- system.file("extdata",
#'                             "LacyTools_summary_example.txt",
#'                             package = "glycodash")
#'                             
#' read_lacytools_summary(summary_file = path_to_file,
#'                        Ig_data = "Yes",
#'                        keyword_specific = "Spike",
#'                        keyword_total = "Total")
read_lacytools_summary <- function(data, Ig_data, keyword_total = NULL, keyword_specific = NULL) {
  
  #data <- read_non_rectangular(summary_file)
  
  all_blocks <- purrr::map(outputs,
                           function(output) {
                             tryCatch(expr = {
                               suppressWarnings(
                                 get_block(data = data, 
                                           variable = output, 
                                           Ig_data = Ig_data,
                                           keyword_specific = keyword_specific,
                                           keyword_total = keyword_total)
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
  
  long_data_list <- purrr::map(all_blocks, create_long_data)
  charges <- as.factor(purrr::map_chr(long_data_list, function(x) unique(x$charge)))
  charge_sep_list <- split(long_data_list, charges)
  
  analytes_info <- get_analytes_info_from_list(data, outputs)
  
  long_data <- purrr::map(charge_sep_list, function(x) purrr::reduce(x, dplyr::left_join)) %>%
    purrr::reduce(dplyr::full_join) %>% 
    dplyr::left_join(analytes_info, by = c("analyte", "charge"))
  
  return(long_data)
}

#'Read in a plate design file
#'
#'The function \code{read_plate_design} reads in a plate design Excel
#'file and returns a dataframe. It uses the \code{\link[plater]{read_plate}}
#'function from the plater package.
#'
#'@param plate_design_file The path to the plate design Excel file. The file
#'  should be in the format described below.
#'
#'@return This function returns a dataframe with a column named "well" that
#'  indicates in which well a sample was analyzed. In addition, there is one
#'  column for each plate in the plate design file. Each plate column contains
#'  the sample ID's of the samples on that plate.
#'@export
#'
#'@section Plate design format: 
#'The top-left cell of the Excel sheet should contain the plate number (e.g. 
#'"Plate 1"). The cells to the right of the top-left cell need to be labelled 
#'1-12 (for a 96-well plate), while the cells below the top-left cell need to be
#'labelled A-H. The cells within the plate should contain the sample ID's.
#'
#'\preformatted{
#'Plate number  1             2            3             ...
#'A            sample_ID_A1  sample_ID_A2  sample_ID_A3  
#'B            sample_ID_B1  sample_ID_B2  sample_ID_B3
#'C            sample_ID_C1  sample_ID_C2  sample_ID_C3
#'...          ...           ...           ...           ...}
#'
#'At the bottom of the plate, leave one row blank and then add the next plate in
#'the same format.
#'
#'
#' @examples
#' path <- system.file("extdata",
#'                     "Plate_design_example.xlsx",
#'                     package = "glycodash")
#' read_plate_design(plate_design_file = path)
read_plate_design <- function(plate_design_file) {
  
  plate_design <- readxl::read_excel(plate_design_file,
                                     .name_repair = "minimal")
  path_to_platedesign_csv <- file.path(tempdir(), "glycodash_platedesign.csv")
  write.csv(plate_design,
            file = path_to_platedesign_csv, 
            row.names = FALSE,
            na = "",
            quote = FALSE)
  tryCatch(expr = {
    plate_design <- plater::read_plate(file = path_to_platedesign_csv, 
                                       well_ids_column = "well")
  },
  # Ignore error thrown by plater::read_plate():
  error = function(e) { 
    rlang::abort(class = "incorrect_formatting",
                 message = paste(
                   "Please check that your plate design file is formatted correctly.",
                   "Run `?read_and_process_plate_design` to find the required format."))
  },
  # Throw custom error when plater::read_plate() throws a warning:
  warning = function(w) { 
    rlang::abort(class = "incorrect_formatting",
                 message = paste(
                   "Please check that your plate design file is formatted correctly.",
                   "Run `?read_and_process_plate_design` to find the required format."))})
  
  return(plate_design)
}

#' Process the result of read_plate_design()
#'
#' This function takes the result from the \code{\link{read_plate_design}}
#' function and converts it to a different format. In addition it automatically
#' determines sample types based on the sample ID's.
#'
#' @param plate_design The dataframe that is returned by the
#'   \code{\link{read_plate_design}} function.
#'
#' @return A dataframe with three columns: \describe{\item{sample_id}{The sample
#'   ID's as given in the plate design file.} \item{sample_type}{Automatically
#'   determined sample types. The function takes the first string of letters
#'   within the sample ID as the sample type. This might not work for your
#'   sample ID's so you may need to alter the sample types manually after using
#'   this function.} \item{plate_well}{This column indicated the plate and well
#'   that a sample was analyzed in. The format is as follows: the plate number
#'   followed by the well ID, separated by an underscore (e.g. plate 1 well A1
#'   is 1_A01).}}
#' @export
#'
#' @examples
#' path <- system.file("extdata",
#'                     "Plate_design_example.xlsx",
#'                     package = "glycodash")
#'
#' plate_design <- read_plate_design(path)
#' process_plate_design(plate_design)
process_plate_design <- function (plate_design) {
  plate_design <- plate_design %>%
    tidyr::pivot_longer(cols = -well,
                        names_to = "plate",
                        values_to = "sample_id") %>%
    dplyr::mutate(plate = stringr::str_match(plate, "[Pp][Ll]?(?:ate)?[\\s_#]*(\\d+|[A-Z])")[ , 2],
                  well = stringr::str_extract(well, "[A-H]\\d+"))
  
  if (any(is.na(plate_design$plate))) {
    rlang::abort(class = "plate_numbers",
                 message = paste(
                   "The plate numbers could not be detected. Please check if",
                   "your plate design Excel file is in the correct format."))
  }
  
  plate_design <- plate_design %>% 
    dplyr::mutate(plate_well = paste(plate, well, sep = "_")) %>% 
    dplyr::arrange(plate_well) %>% 
    dplyr::select(-c(plate, well)) %>% 
    # tidyr::extract(col = sample_id, 
    #                into = c("sample_type"), 
    #                regex = "([[:alpha:]]+)",
    #                remove = FALSE) %>% 
    # Remove rows where sample_type is NA (these were empty cells
    # in the plate design Excel file)
    tidyr::replace_na(list(
      #sample_type = "unknown",
      sample_id = "unknown"
    ))
  # dplyr::filter(dplyr::if_all(.cols = c(sample_type),
  #                             .fns = ~ !is.na(.x)))
  
  return(plate_design)
}

#'Handle duplicate samples in a plate design file
#'
#'\code{handle_duplicates()} finds the samples on a plate design that are
#'specified as duplicate samples and finds the sample ID's and sample types of
#'these samples by copying the sample ID and type of the preceding sample. This
#'function assumes that duplicates are indicated in the plate design Excel file
#'as "duplicate" and that duplicate samples are always the duplicate of the
#'sample one well to the left, or, in case the duplicate is positioned in the
#'left-most column of the plate, the duplicate of the right-most well one row
#'up.
#'
#'@param plate_design The dataframe that is returned by the
#'  \code{\link{process_plate_design}} function.
#'
#'@return The original dataframe passed as the \code{plate_design} argument with
#'  an additional column named "duplicate". This column is \code{TRUE} for
#'  samples that were specified on the plate design as duplicate samples and
#'  \code{FALSE} for the remaining samples. In addition, the sample ID's in the
#'  sample_id column and the sample types in the sample_type column of duplicate
#'  samples are replaced with the sample ID's and sample types of the preceding
#'  sample.
#'@export
#'
#' @examples
#' path <- system.file("extdata",
#'                     "Plate_design_example.xlsx",
#'                     package = "glycodash")
#'
#' plate_design <- read_plate_design(path)
#' plate_design <- process_plate_design(plate_design)
#' handle_duplicates(plate_design)
handle_duplicates <- function(plate_design) {
  new_sample_ids <- vector()
  new_sample_types <- vector()
  duplicate <- vector()
  for (i in 1:length(plate_design$sample_id)) {
    if (i == 1 & plate_design$sample_id[i] == "duplicate") {
      rlang::abort(class = "first_sample_duplicate", 
                   message = "Error: first sample is a duplicate.")
    }
    if (plate_design$sample_id[i] == "duplicate") {
      new_sample_ids[i] <- plate_design$sample_id[i-1]
      new_sample_types[i] <- plate_design$sample_type[i-1]
      duplicate[i] <- TRUE
    } else {
      new_sample_ids[i] <- plate_design$sample_id[i]
      new_sample_types[i] <- plate_design$sample_type[i]
      duplicate[i] <- FALSE
    }
  }
  
  if (identical(new_sample_ids, plate_design$sample_id)) {
    rlang::warn(class = "no_duplicates",
                message = paste("No duplicates were found in your plate design file.",
                                "If there should be duplicate samples to be found,",
                                "please check if those samples are specified literally as \"duplicate\" in your plate design file."))
  }
  
  new_plate_design <- plate_design %>% 
    dplyr::mutate(sample_id = new_sample_ids,
                  sample_type = new_sample_types,
                  duplicate = duplicate)
  return(new_plate_design)
}

#'Read and process a plate design file
#'
#' The function \code{read_and_process_plate_design} reads in a plate design
#' Excel file and processes it. It automatically determines sample types based
#' on the sample ID's and finds the samples on a plate design that are specified
#' as duplicate samples. The sample ID's and sample types of these duplicate
#' samples are determined by copying the sample ID and type of the preceding
#' sample.
#' 
#'@inheritParams read_plate_design
#'
#'@section Plate design format: The top-left cell of the Excel sheet should
#'  contain the plate number (e.g. "Plate 1"). The cells to the right of the
#'  top-left cell need to be labelled 1-12 (for a 96-well plate), while the
#'  cells below the top-left cell need to be labelled A-H. The cells within the
#'  plate should contain the sample ID's.
#'
#'  \preformatted{ 
#'  Plate number  1             2            3             ... 
#'  A            sample_ID_A1  sample_ID_A2  sample_ID_A3 
#'  B            sample_ID_B1  sample_ID_B2  sample_ID_B3 
#'  C            sample_ID_C1  sample_ID_C2  sample_ID_C3  
#'  ...          ...           ...           ...           ...}
#'
#'  At the bottom of the plate, leave one row blank and then add the next plate
#'  in the same format.
#'
#'  Duplicate samples should be indicated in the plate design Excel file as
#'  "duplicate". \code{read_and_process_plate_design} assumes that duplicate 
#'  samples are always the duplicate of the preceding sample, that is the 
#'  sample one well to the left or, in case the duplicate is positioned in the 
#'  left-most column of the plate, the sample in the right-most well one row up.
#'
#'@return This function returns a dataframe with four columns:
#'  \describe{\item{sample_id}{The sample ID's as given in the plate design
#'  file.} \item{sample_type}{Automatically determined sample types. The
#'  function takes the first string of letters within the sample ID as the
#'  sample type. This might not work for your sample ID's so you may need to
#'  alter the sample types manually after using this function.}
#'  \item{plate_well}{This column indicated the plate and well that a sample was
#'  analyzed in. The format is as follows: the plate number followed by the well
#'  ID, separated by an underscore (e.g. plate 1 well A1 is 1_A01).}
#'  \item{duplicate}{This column is \code{TRUE} for samples that were specified
#'  on the plate design as duplicate samples and \code{FALSE} for the remaining
#'  samples.}}
#'
#'@export
#'
#' @examples
#' path <- system.file("extdata",
#'                     "Plate_design_example.xlsx",
#'                     package = "glycodash")
#' 
#' read_and_process_plate_design(plate_design_file = path)
read_and_process_plate_design <- function(plate_design_file) {
  # Pass along any errors (if applicable) from read_plate_design:
  tryCatch(expr = {
    plate_design <- read_plate_design(plate_design_file)
  },
  incorrect_formatting = function(cnd) {
    rlang::abort(message = cnd$message,
                 class = "incorrect_formatting")
  })
  
  plate_design <- process_plate_design(plate_design)
  
  # Pass along any errors (if applicable) from handle_duplicates. Warnings are
  # passed along automatically.
  # tryCatch(expr = {
  #   plate_design <- handle_duplicates(plate_design)
  # },
  # error = function(e) {
  #   rlang::abort(class = e$class,
  #                message = e$message)
  # })
  
  return(plate_design)
}

#' Read in a metadata file
#'
#' \code{read_metadata()} can be used to read in an Excel file containing
#' metadata. It reads in all columns as class character and interprets both "NA"
#' and empty cells as \code{NA}. The values in the first row are converted to
#' snake case (see: \code{\link[snakecase]{to_snake_case}}) and used as column
#' names. If the file contains multiple sheets, only the first sheet is read in.
#'
#' @param metadata_file The path to the metadata Excel file.
#'
#' @return This function returns a dataframe with the metadata.
#' @export
#'
#' @examples
#' path <- system.file("extdata",
#'                     "Metadata_example.xlsx",
#'                     package = "glycodash")
#'                     
#' read_metadata(path)
# read_metadata <- function (metadata_file) {
#   metadata <- readxl::read_excel(metadata_file,
#                                  col_types = "text", 
#                                  na = c("", "NA"))
#   metadata <- metadata %>% 
#     dplyr::rename_with(.cols = tidyselect::everything(), 
#                        .fn = snakecase::to_snake_case) 
#   return(metadata)
# }

#'Convert serial dates with comments to dates
#'
#'\code{date_with_text()} tries to convert a vector with serial dates (given as
#'character strings) to objects of the class "Date". In Excel dates are stored
#'as serial numbers. These serial dates are the number of days passed since a
#'set origin date. When Excel data is read into R the serial dates are usually
#'recognized as dates and converted to objects of class "Date". However, when in
#'Excel a cell contains both a date and a comment, the column containing this
#'cell is no longer recognized as class "Date" and is instead converted to class
#'"Character". \code{date_with_text()} attempts to convert serial dates (given
#'as character strings) to objects of class "Date". If some serial dates include
#'comments, the function instead converts the serial dates to character strings
#'with the dates formatted as "year-month-day". \strong{Important: This function
#'can only be used within the function \code{\link[dplyr]{across}} from the
#'dplyr package!}
#'
#'@param date_text_values A column in a dataframe or tibble with serial dates in
#'  the form of character strings.
#'@param origin The origin date used to calculate the serial dates. Defaults to
#'  1899-12-30 (year-month-day) which is the origin used in Excel on Windows for
#'  all dates from 1900-01-01 and on.
#'
#'@return
#'@export
#'
#' @section Credits:
#' This function was based on a solution found on StackOverflow.\cr
#' Question asked by Ryan C. Thompson:\cr
#' \url{https://stackoverflow.com/questions/63027147/how-to-read-in-a-date-column-from-an-excel-sheet-with-ambiguous-formats-into-r}\cr
#' Solution provided by Rui Barradas:\cr
#' \url{https://stackoverflow.com/users/8245406/rui-barradas}
#'
#' @examples
#' path <- system.file("extdata",
#'                     "for_tests",
#'                     "Dates_with_comment.xlsx",
#'                     package = "glycodash")
#'
#'# Dates with comments in Excel are usually interpreted
#'# as character strings in R:
#'
#' dates_with_comment <- readxl::read_excel(path)
#'
#' print(dates_with_comment)
#'
#'# # A tibble: 6 x 1
#'# date
#'# <chr>
#'# 1 44603
#'# 2 44604
#'# 3 13-02-2022 oh no a comment
#'# 4 44606
#'# 5 44607
#'# 6 44608
#'
#' dates_with_comment <- dates_with_comment %>%
#'    dplyr::mutate(dplyr::across(.cols = date,
#'                                .fns = date_with_text))
#'
#'# Because text is found within the dates
#'# column the following message is shown:
#'# "Some date entries in date contain text.
#'# Output will have class character."
#'
#'print(dates_with_comment)
#'
#' # # A tibble: 6 x 1
#' # date
#' # <chr>
#' # 1 2022-02-11
#' # 2 2022-02-12
#' # 3 13-02-2022 oh no a comment
#' # 4 2022-02-14
#' # 5 2022-02-15
#' # 6 2022-02-16
#'
#' path <- system.file("extdata",
#'                     "for_tests",
#'                     "Dates.xlsx",
#'                     package = "glycodash")
#'
#' # Forcing read_excel() to read the dates in as character strings 
#' # (even though there are no comments in this file) for 
#' # demonstration purposes:
#' dates <- readxl::read_excel(path,
#'                             col_types = "text")
#'
#' print(dates)
#'
#'# # A tibble: 6 x 1
#'# date
#'# <chr>
#'# 1 44603
#'# 2 44604
#'# 3 44605
#'# 4 44606
#'# 5 44607
#'# 6 44608
#'
#' dates <- dates %>%
#'    dplyr::mutate(dplyr::across(.cols = date,
#'                                .fns = date_with_text))
#'                                
#' # When no text is found within the dates column,
#' # the serial dates are converted to objects of class "date":
#' print(dates)
#'
#' # # A tibble: 6 x 1
#' # date
#' # <date>
#' # 1 2022-02-11
#' # 2 2022-02-12
#' # 3 2022-02-13
#' # 4 2022-02-14
#' # 5 2022-02-15
#' # 6 2022-02-16
#'           
date_with_text <- function(date_text_values, origin = "1899-12-30"){
  no_nas <- date_text_values[!is.na(date_text_values)]
  test <- suppressWarnings(as.numeric(no_nas))
  num <- suppressWarnings(as.numeric(date_text_values))
  dates <- as.Date(num, origin = origin)
  if(anyNA(test)){
    dates <- as.character(dates)
    dates[is.na(num)] <- as.character(date_text_values[is.na(num)])
    rlang::warn(class = "text_in_dates",
                message = paste("Some date entries in", 
                                tryCatch(dplyr::cur_column(),
                                         error = function(e) {}), 
                                "contain text. Output will have class character."))
  }
  return(dates)
}

#' Load R-objects and assign them a new name
#'
#' When you load an R-object with \code{\link[base]{load}} the object keeps the
#' name it had at the moment that it was saved. Sometimes it's more convenient
#' to assign a new name to the object when it is being loaded.
#' \code{load_and_assign()} allows you to do that.
#' 
#' @section Credits:
#' The code for this function was found on StackOverflow.\cr
#' Question asked by Ryan C. Thompson:\cr
#' \url{https://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file}\cr
#' Solution provided by ricardo:\cr
#' \url{https://stackoverflow.com/users/1453172/ricardo}
#' 
#' @param file_path The path to the R-object that you want to load.
#'
#' @return
#' @export
#'
#' @examples
#' old_name <- c("This is a random vector",
#'               "to serve as an example")
#'
#' # Creating a temporary directory:
#' file_path <- file.path(tempdir(),
#'                        "R_object.rds")
#'
#' # Saving the vector as an R-object to the temporary directory:
#' save(old_name,
#'      file = file_path)
#'
#' # Loading the R object and assigning a new name to it:
#' new_name <- load_and_assign(file_path)
#' 
load_and_assign <- function(file_path){
  tryCatch(
    load(file_path),
    error = function(e) {
      
    },
    warning = function(w) { 
      stop(paste("Check if the file_path was not misspelled.",
                 "Was the R-object you're trying to load saved with the function saveRDS()?",
                 "load_and_assign only works with objects saved with the function save()"))
      })
  get(ls()[ls() != "file_path"])
}

comma_and <- function(string_components) {
  comma_string <- paste(string_components, collapse = ", ")
  comma_and_string <- sub(pattern = ",\\s([^,]+)$", 
                          replacement = " and \\1",
                          x = comma_string)
  return(comma_and_string)
}

comma_or <- function(string_components) {
  comma_string <- paste(string_components, collapse = ", ")
  comma_and_string <- sub(pattern = ",\\s([^,]+)$", 
                          replacement = " or \\1",
                          x = comma_string)
  return(comma_and_string)
}

is_truthy <- function(x) {
  valid <- tryCatch(
    expr = {
      x
      TRUE
    },
    error = function(e) {
      FALSE
    })
  if (valid) {
    return(isTruthy(x))
  } else {
    return(FALSE)
  }
}
