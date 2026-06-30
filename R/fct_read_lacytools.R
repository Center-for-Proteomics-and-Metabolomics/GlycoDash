# The list `OUTPUTS` with LaCyTools outputs is used in the function
# 'read_lacytools'.
OUTPUTS <- as.list(unlist(lapply(
  list(
    "Absolute Intensity (Background Subtracted, ",
    "Mass Accuracy [ppm] (",
    "Isotopic Pattern Quality (",
    "S/N ("
  ), 
  paste0,
  seq_len(20),  # Max. positive charge
  "+)"
)))



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
  
  max_n_columns <- find_widest_row(path = path, delim = delim)
  
  column_names <- vector()
  for (i in 1:max_n_columns) {
    column_names[i] <- paste("col", i, sep = "_")
  }
  
  data <- read.table(
    path, 
    fill = TRUE, 
    header = FALSE, 
    col.names = column_names, 
    sep = delim, 
    blank.lines.skip = FALSE, 
    na.strings = c("", "0")
  )
  
  return(data)
}



#' Find the widest row in a non-rectangular data file
#'
#' Reads a file line-by-line to determine the maximum number of fields in any
#' single line. This is used to pre-determine the column count needed by
#' \code{\link{read_non_rectangular}}.
#'
#' @param path File path to non-rectangular data.
#' @param delim The field separator used in the file.
#'
#' @return The number of fields/columns in the widest line (integer).
#' @export
find_widest_row <- function(path, delim) {
  # Error handling for file existence
  if (!file.exists(path)) {
    rlang::abort(
      class = "wrong_path",
      message = "No such file or directory exists."
    )
  }
  con <- file(path, "r")
  on.exit(close(con))
  max_cols <- 0L
  lines_read <- 0L
  repeat {
    line <- tryCatch(
      readLines(con, n = 1, warn = FALSE),
      warning = function(w) {
        # Embedded null warning
        rlang::abort(
          class = "embedded_null",
          message = w$message
        )
      },
      error = function(e) {
        rlang::abort(
          class = "read_error",
          message = e$message
        )
      }
    )
    if (length(line) == 0) {
      break
    }
    lines_read <- lines_read + 1L
    n_cols <- length(strsplit(line, delim, fixed = TRUE)[[1]])
    if (n_cols > max_cols) {
      max_cols <- n_cols
    }
  }
  if (lines_read == 0L) {
    rlang::abort(
      class = "empty_file",
      message = "One or more of the uploaded files are empty."
    )
  }
  if (max_cols == 1L) {
    rlang::warn(
      class = "wrong_delim",
      message = "One or more files seem to consist of a single column. Please make sure that you chose the correct delimiter for your files."
    )
  }
  
  return(max_cols)
}



#' Convert a LaCyTools summary to a tidy dataframe
#'
#' Transforms a LaCyTools summary dataframe (as returned by
#' \code{\link{read_non_rectangular}}) into a tidy long-format dataframe, with
#' one row per analyte per charge state per sample. The analyte exact mass and
#' fraction are joined from the summary header rows.
#'
#' @param data A dataframe containing a LaCyTools summary returned by
#'   \code{\link{read_non_rectangular}}.
#'
#' @return A dataframe in long format with one row per analyte per charge per
#'   sample.
#' @export
convert_lacytools_summary <- function(data) {
  # all_blocks: extract and tidy each output block, suppress warnings on get_block
  all_blocks <- lapply(OUTPUTS, function(output) {
    tryCatch(
      suppressWarnings(get_block(data = data, variable = output)),
      error = function(e) NULL
    )
  })
  all_blocks <- all_blocks[!vapply(all_blocks, is.null, logical(1))]
  if (rlang::is_empty(all_blocks)) {
    rlang::abort(
      class = "no_outputs_present",
      message = paste(
        ", none of the LaCyTools output variables are present.",
        "Did you choose the correct file?"
      )
    )
  }
  # lengthen_block: transform each block to long format (faster with lapply)
  long_data_list <- lapply(all_blocks, lengthen_block)
  # Ensure all elements have a charge field, and get factor levels quickly
  charges <- as.factor(vapply(long_data_list, function(x) unique(x$charge), character(1)))
  charge_sep_list <- split(long_data_list, charges)
  # Get analytes info (unchanged)
  analytes_info <- get_analytes_info_from_list(data, OUTPUTS)
  # Efficiently join blocks and charges
  # Use Reduce over each charge group, then Reduce over all charge groups
  joined_blocks <- lapply(charge_sep_list, function(blocks) Reduce(dplyr::left_join, blocks))
  long_data <- Reduce(dplyr::full_join, joined_blocks)
  long_data <- dplyr::left_join(long_data, analytes_info, by = c("analyte", "charge"))
  
  return(long_data)
}



#' Create a subset containing one block from a LaCyTools summary
#'
#' Extracts the rows that belong to a single named output block (e.g. a
#' specific charge state of one LaCyTools output type), sets proper column
#' names, removes the header and metadata rows, and adds a
#' \code{lacytools_output} column identifying the block.
#'
#' @inheritParams find_block
#'
#' @return A dataframe that is a subset of the input dataframe.
#' @export
get_block <- function(data, variable) {
  row_indices <- find_block(data, variable)
  block <- data[row_indices, , drop = FALSE]
  # The first row of the block contains the column names for the block:
  colnames(block) <- as.character(unlist(block[1, , drop = TRUE]))
  # The first column should be named "sample_name":
  colnames(block)[1] <- "sample_name"
  # In case there are duplicated analyte names in the LaCyTools summary, apply
  # .name_repair (and issue a warning message)
  if (any(duplicated(colnames(block)))) {
    duplicated_analytes <- unique(colnames(block)[duplicated(colnames(block))])
    rlang::warn(
      class = "duplicated_analytes",
      message = paste0(
        "the following analytes are present more than once: ",
        paste(duplicated_analytes, collapse = ", "),
        ". The names of the duplicates analytes are given",
        " a suffix ('..columnnumber') to differentiate between them."
      )
    )
    block <- suppressMessages(tibble::tibble(block, .name_repair = "universal"))
  }
  better_name_output <- stringr::str_remove_all(
    stringr::str_replace_all(tolower(variable), " ", "_"),
    "[\\(\\)\\,\\/\\[\\]]"
  )
  # Remove first row (column names)
  block <- block[-1, , drop = FALSE]
  # Remove columns where all values including fraction and exact mass are missing (NA)
  block <- block[, colSums(!is.na(block)) > 0, drop = FALSE]
  # Remove next two rows (fraction and exact mass rows)
  block <- block[-c(1, 2), , drop = FALSE]
  block <- dplyr::mutate(block, lacytools_output = better_name_output)
  # Convert all columns except sample_name and lacytools_output to numeric
  num_cols <- setdiff(colnames(block), c("sample_name", "lacytools_output"))
  block[num_cols] <- lapply(block[num_cols], function(x) suppressWarnings(as.numeric(x)))
  
  return(block)
}



#' Find a block in a LaCyTools summary file
#'
#' Locates the row indices of a named output block within a LaCyTools summary
#' dataframe. The block starts at the row whose first column matches
#' \code{variable} and ends just before the next all-NA row (or at the last row
#' if no such row exists).
#'
#' @inheritParams find_next_na
#' @param variable The name of a LaCyTools output format.
#'
#' @return The row indices of the block.
#' @export
find_block <- function(data, variable) {
  first_row <- which(data[, 1] == variable)
  if (rlang::is_empty(first_row)) {
    rlang::abort(
      class = "lacytools_output_not_found",
      message = paste(
        "Error: LaCyTools output format",
        variable, "is not present in the first column of the input file."
      ))
  } 
  else {
    next_na <- find_next_na(data, first_row)
    if (length(next_na) == 0) { 
      rows <- seq.int(first_row, nrow(data))
    } 
    else {
      rows <- seq.int(first_row, next_na - 1)
    }
  }
  
  return(rows)
}



#' Find the next empty line from a given line in a LaCyTools summary file
#'
#' Searches forward from \code{row} in the first column of \code{data} and
#' returns the index of the next row that contains only \code{NA}s. Used by
#' \code{\link{find_block}} to determine where a block ends.
#'
#' @param data A dataframe with the LaCyTools summary (the result of
#'   \code{\link{read_non_rectangular}}).
#' @param row The row used as a starting point from which to search for the next
#'   blank line (blank meaning consisting of only \code{NA}'s).
#'
#' @return The row index (integer) for the next line with \code{NA}'s. If there
#'   are no next lines with \code{NA}'s the function will return an empty integer vector.
#' @export
find_next_na <- function(data, row) {
  # Find rows in the first column containing NA's
  na_index <- which(is.na(data[, 1]))
  # Select only the rows after the starting row
  later_nas <- na_index[na_index > row]
  # Return the closest NA row after the starting point, or integer(0) if none
  if (length(later_nas) == 0) {
    return(integer(0))
  }
  later_nas[which.min(later_nas - row)]
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
#'   already been added to the data. Defaults to \code{NULL}.
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
    tidyr::pivot_longer(
      cols = -tidyselect::any_of(cols_not_to_pivot),
      names_to = "analyte",
      values_to = tidyselect::all_of(new_output_name)
    ) %>% 
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
  analytes_info_list <- purrr::map(list_of_variables, function(variable) {
    analytes_info <- tryCatch({
      get_analytes_info(data, variable) %>% 
        dplyr::mutate(charge = stringr::str_extract(variable, "\\d+[+\\-]"))
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
  analytes_info <- purrr::map(charge_sep_list, function(x) x[[1]]) %>% 
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
    stop(paste(
      "The LaCyTools output format", 
      variable,
      "is not present in the input summary file"
    ))
  }
  # The two rows below the row with analyte names, contain the exact mass and
  # the fraction for each analyte. Subset those rows:
  analytes_info <- data[row_index_analyte_names:(row_index_analyte_names + 2), ]
  # Use the first row with analyte names as column names and replace the name of
  # the first column:
  colnames(analytes_info) <- unlist(analytes_info[1, ])
  colnames(analytes_info)[1] <- "info_variables"
  
  # .name_repair is used in case of duplicate analytes
  analytes_info <- suppressMessages(tibble::as_tibble(
    analytes_info,  name_repair = "universal"
  ))
  
  # Pivot the dataframe and do some formatting:
  analytes_info <- analytes_info %>%
    dplyr::slice(-1) %>% 
    tidyr::pivot_longer(
      cols = -info_variables,
      names_to = "analyte", 
      values_to = "value"
    ) %>%
    tidyr::pivot_wider(names_from = info_variables) %>% 
    # Remove leading or trailing spaces from the analyte column
    dplyr::mutate(analyte = trimws(analyte)) %>% 
    # I don't rename the columns directly using new_name = old_name, because in
    # different versions of LaCyTools these columns are named differently
    # ("Exact mass of most abundant isotopologue" in one version and
    # "Monoisotopic mass" in the other):
    dplyr::rename(
      fraction = tidyselect::contains("fraction"),
      exact_mass = tidyselect::contains("mass")
    ) %>% 
    dplyr::mutate(exact_mass = purrr::map_chr(
      exact_mass, function(x) stringr::str_remove_all(x, "[\\[\\]]"))
    ) %>% 
    dplyr::mutate(dplyr::across(-analyte, ~ suppressWarnings(as.numeric(.x))))
  
  return(analytes_info)
}
