outputs <- list("Absolute Intensity (Background Subtracted, 2+)",
                "Absolute Intensity (Background Subtracted, 3+)", 
                "Mass Accuracy [ppm] (2+)", 
                "Mass Accuracy [ppm] (3+)",
                "Isotopic Pattern Quality (2+)",
                "Isotopic Pattern Quality (3+)",
                "S/N (2+)",
                "S/N (3+)")

#' Read in non-rectangular delimited files
#' 
#' \code{read_non_rectangular()} can read flat files where the number of fields per line is 
#' not constant (non-rectangular data). Blank lines in the field are not skipped 
#' and empty fields "" are interpreted as \code{NA}. This function was created to read
#' LacyTools summary files (tab-delimited .txt file).
#'
#' @param path A path to a file with non-rectangular data.
#' @param delim The field separator used in the file. 
#'              For example, use "\t" for files with tab-separated values 
#'              and "," or ";" for files with comma-separated values (.csv).
#'
#' @return A data frame (\code{\link{[base]data.frame}}) containing the data in the flat file.
#' @export
#'
#' @examples
read_non_rectangular <- function(path, delim = "\t") {
  lines <- readLines(path, n = -1, ok = TRUE)
  columns <- stringr::str_split(lines, pattern = delim)
  n_columns <- max(purrr::map_int(columns, ~ length(.x)))
  column_names <- vector()
  for (i in 1:n_columns) {
    column_names[i] <- paste("col", i, sep = "_")
  }
  data <- read.table(path, fill = TRUE, header = FALSE, col.names = column_names, 
                     sep = delim, blank.lines.skip = FALSE, na.strings = "")
  return(data)
}

#' Find the next empty line from a given line in a LacyTools summary file.
#'
#' @param data A dataframe with the LacyTools summary.
#' @param row The row used as a starting point from which to search for the next line with NAs.
#'
#' @return The row index for the next line with NAs.
#' @export
#'
#' @examples
find_next_na <- function(data, row) {
  na_index <- which(is.na(data[ , 1]))
  later_nas <- na_index[which(purrr::map_lgl(na_index, ~ .x > row))]
  next_na <- later_nas[which.min(purrr::map_int(later_nas, ~ .x - row))]
  return(next_na)
}

#' Find a block in a LacyTools summary file
#'
#' @param data A dataframe with the LacyTools summary.
#' @param variable The name of a LacyTools output format.
#'
#' @return The row indices of the block.
#' @export
#'
#' @examples
find_block <- function(data, variable) {
  first_row <- which(data[ , 1] == variable)
  if (rlang::is_empty(first_row)) {
    print(paste("Error: LacyTools output format",
                variable, "is not present in the input file."))
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

#' Detect whether a sample is Specific or Total Ig based on the sample name.
#'
#' @param block A dataframe containing a block from a LacyTools summary file. 
#' @param name_specific The word(s) within the sample name used to refer to Specific samples.
#' @param name_total The word(s) within the sample name used to refer to Total samples.
#'
#' @return The dataframe containing a block from a LacyTools summary file, with an additional
#' column named "group" that indicates whether a sample is Specific or Total.
#' @export
#'
#' @examples
detect_group <- function(block, name_specific, name_total) {
  block %>% 
    tidyr::extract(col = sample_name,
            into = "group",
            regex = paste0("(", name_specific, "|", 
                           name_total, ")"),
            remove = FALSE)
  return(block)
}

#' Create a subset containing one block from a LacyTools summary. 
#'
#' @param data A dataframe with the LacyTools summary.
#' @param variable The name of a LacyTools output format.
#' @param name_specific The word(s) within the sample name used to refer to Specific samples.
#' @param name_total The word(s) within the sample name used to refer to Total samples.
#'
#' @return A dataframe that is a subset of the input dataframe.
#' @export
#'
#' @examples
#' data(LacyTools_summary)
#' get_block(LacyTools_summary, variable = "Absolute Intensity (Background Subtracted, 2+)")
get_block <- function(data, variable, name_specific = "Spike", name_total = "Total") {
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
  block <- block[-c(1, 2, 3), ] %>% 
    dplyr::mutate(lacytools_output = better_name_output) %>% 
    dplyr::mutate(dplyr::across(-c(sample_name, lacytools_output), as.numeric)) %>% 
    dplyr::select(-tidyselect::vars_select_helpers$where(function(x) all(is.na(x))))
  block <- detect_group(block, name_specific, name_total)
  return(block)
}

#' Detect plate and well
#'
#' @param data A dataframe. Should include a column named "sample_name".
#'
#' @return The input dataframe with an added column named "plate_well" that says on which plate
#' and in which well a sample was analysed. 
#' @export
#'
#' @examples
detect_plate_and_well <- function(data) {
  data %>% 
    tidyr::extract(col = sample_name, 
            into = c("plate", "well"),
            regex = "([-_][Pp]l\\d).+(_[A-H]\\d{1,2})", 
            remove = FALSE) %>% 
    dplyr::mutate(plate = stringr::str_extract(plate, "\\d+"),
           well = stringr::str_extract(well, "[A-H]\\d{1,2}"),
           plate_well = paste(plate, well, sep = "_")) %>% 
    dplyr::select(-c(plate, well)) %>% 
    dplyr::relocate(plate_well, .after = sample_name)
}

#' Merge a dataframe containing metadata to a dataframe containing the data.
#'
#' @param data A dataframe containing data. Should contain a column named "plate_well".
#' @param metadata A dataframe containing metadata. Should contain a column named "plate_well".
#'
#' @return
#' @export
#'
#' @examples
add_metadata <- function(data, metadata){
  data <- detect_plate_and_well(data)
  data <- dplyr::left_join(data, metadata, by = "plate_well")
  return (data)
}

get_analytes_info <- function(data, variable) {
  analyte_names <- which(data[ , 1] == variable)
  if (rlang::is_empty(analyte_names)){
    stop(paste("The LacyTools output format", variable,
               "is not present in the input summary file"))
  }
  analytes_info <- data[analyte_names:(analyte_names + 2), ]
  colnames(analytes_info) <- unlist(analytes_info[1, ])
  colnames(analytes_info)[1] <- "info_variables"
  analytes_info <- analytes_info %>%
    dplyr::slice(n = -1) %>% 
    tidyr::pivot_longer(cols = -info_variables,
                        names_to = "analyte", 
                        values_to = "value") %>%
    tidyr::pivot_wider(names_from = info_variables) %>% 
    dplyr::rename(exact_mass = `Exact mass of most abundant isotopologue`,
                  fraction = Fraction) %>% 
    dplyr::mutate(exact_mass = purrr::map_chr(exact_mass,
                                              function(x) stringr::str_remove_all(x, 
                                                                                  "[\\[\\]]"))) %>% 
    dplyr::mutate(dplyr::across(-analyte, as.numeric))
  return(analytes_info)
}

get_analytes_info_from_list <- function(data, list_of_variables) {
  for (variable in list_of_variables) {
    tryCatch({
      analytes_info <- get_analytes_info(data, variable)
      return(analytes_info)
    },
    # Ignore list items that result in an error:
    error = function(e) { })
  }
  # Throw error if no matches are found
  stop("No output formats in the list are present in the input summary file")
}

create_long_data <- function(block, metadata = NULL) {
  charge_value <- stringr::str_extract(block$lacytools_output[1], "\\d\\+")
  new_output_name <- stringr::str_remove(block$lacytools_output[1], "_\\d\\+")
  cols_not_to_pivot <- c("sample_name", "group", colnames(metadata))
  
  block <- block %>% 
    dplyr::select(-lacytools_output) %>%
    tidyr::pivot_longer(cols = -tidyselect::any_of(cols_not_to_pivot),
                 names_to = "analyte",
                 values_to = tidyselect::all_of(new_output_name)) %>% 
    dplyr::mutate(charge = charge_value) %>% 
    dplyr::relocate(charge, .before = all_of(new_output_name))
  
  return(block)
}

read_lacytools_summary <- function(summary_file) {
  
  data <- read_non_rectangular(summary_file)
  
  all_blocks <- purrr::map(outputs,
                           function(x) get_block(data, x))
  
  all_blocks <- all_blocks[which(purrr::map_lgl(all_blocks, is.data.frame))]
  
  long_data_list <- purrr::map(all_blocks, create_long_data)
  charges <- as.factor(purrr::map_chr(long_data_list, function(x) unique(x$charge)))
  charge_sep_list <- split(long_data_list, charges)
  
  analytes_info <- get_analytes_info_from_list(data, outputs)
  
  long_data <- purrr::map(charge_sep_list, function(x) purrr::reduce(x, dplyr::left_join)) %>%
    purrr::reduce(dplyr::full_join) %>% 
    dplyr::left_join(analytes_info, by = "analyte")
  
  return(long_data)
}

read_plate_design <- function(plate_design_file) {
  
  plate_design <- readxl::read_excel(plate_design_file)
  path_to_platedesign_csv <- file.path(tempdir(), "glycodash_platedesign.csv")
  readr::write_csv(plate_design, file = path_to_platedesign_csv)
  plate_design <- plater::read_plate(file = path_to_platedesign_csv, 
                                     well_ids_column = "well")
  return(plate_design)
}

process_plate_design <- function (plate_design) {
  plate_design <- plate_design %>%
    tidyr::pivot_longer(cols = -well,
                        names_to = "plate",
                        values_to = "sample_id") %>%
    dplyr::mutate(plate = stringr::str_extract(plate, "\\d+"),
                  well = stringr::str_extract(well, "[A-H]\\d{1,2}"),
                  plate_well = paste(plate, well, sep = "_")) %>% 
    dplyr::arrange(plate_well) %>% 
    dplyr::select(-c(plate, well)) %>% 
    tidyr::extract(col = sample_id, 
                   into = c("sample_type"), 
                   regex = "([[:alpha:]]+)",
                   remove = FALSE)
  
  plate_design <- handle_duplicates(plate_design)
  
  return(plate_design)
}

handle_duplicates <- function(plate_design) {
  new_sample_ids <- vector()
  new_sample_types <- vector()
  duplicate <- vector()
  for (i in 1:length(plate_design$sample_id)) {
    if (i == 1 & plate_design$sample_id[i] == "duplicate") {
      stop(print("Error: first sample is a duplicate."))
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
  new_plate_design <- plate_design %>% 
    dplyr::mutate(sample_id = new_sample_ids,
                  sample_type = new_sample_types,
                  duplicate = duplicate)
  return(new_plate_design)
}

read_and_process_plate_design <- function(plate_design_file) {
  plate_design <- read_plate_design(plate_design_file)
  plate_design <- process_plate_design(plate_design)
  return(plate_design)
}
