#' Detect plate and well
#'
#' @param data A dataframe. Should include a column named "sample_name".
#'
#' @return
#' @export
#'
#' @examples
detect_plate_and_well <- function(data) {
  data %>% 
    extract(col = sample_name, 
            into = c("plate", "well"),
            regex = "([-_][Pp]l\\d).+(_[A-H]\\d{1,2})", 
            remove = FALSE) %>% 
    mutate(plate = str_extract(plate, "\\d+"),
           well = str_extract(well, "[A-H]\\d{1,2}"),
           plate_well = paste(plate, well, sep = "_")) %>% 
    select(-c(plate, well)) %>% 
    relocate(plate_well, .after = sample_name)
}

#' Get block
#'
#' @param data 
#' @param variable 
#' @param name_specific 
#' @param name_total 
#'
#' @return
#' @export
#'
#' @examples
#' get_block(LacyTools_summary, variable = "Absolute Intensity (Background Subtracted, 2+)")
#' 
get_block <- function(data, variable, name_specific = "Spike", name_total = "Total") {
  rows <- find_block(data, variable)
  block <- data[rows, ]
  colnames(block) <- unlist(block[1, ])
  colnames(block)[1] <- "sample_name"
  better_name_output <- str_remove_all(str_replace_all(tolower(variable), " ", "_"),
                                       "[\\(\\)\\,\\/\\[\\]]")
  block <- block[-c(1, 2, 3), ] %>% 
    mutate(lacytools_output = better_name_output) %>% 
    mutate(across(-c(sample_name, lacytools_output), as.numeric)) %>% 
    select(-where(function(x) all(is.na(x))))
  block <- detect_group(block, name_specific, name_total)
  return(block)
}

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