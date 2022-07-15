
read_metadata <- function(filepaths, filenames) {
  
  metadata_list <- purrr::pmap(
    list(path = filepaths,
         name = filenames),
    function(path, name) {
      extension <- tools::file_ext(name)
      if (extension %in% c("xlsx", "xls")) {
        metadata <- readxl::read_excel(path,
                                       col_types = "text",
                                       na = c("", "NA"))
      } else {
        if (extension == "rds") {
          metadata <- load_and_assign(path)
        } else {
          rlang::abort(
            class = "wrong_extension",
            message = "Please upload a .xlsx, .xls or .rds file."
          )
        }
      }
      metadata <- metadata %>%
        dplyr::rename_with(.cols = tidyselect::everything(),
                           .fn = snakecase::to_snake_case)
      
      return(metadata)
    })
  
  names(metadata_list) <- filenames
  
  return(metadata_list)
}


prep_metadata <- function(input,
                          metadata_list,
                          sample_id_inputIds,
                          date_column_inputIds) {
  
  metadata_list_prepped <- purrr::pmap(
    list(metadata_list,
         sample_id_inputIds,
         date_column_inputIds),
    function(metadata, 
             sample_id_inputId,
             date_column_inputId) {
      # Check whether the metadata contains a column named "sample_id" that
      # is not chosen as the sample ID column. If this is the case, the
      # column needs to be renamed to prevent duplicated names (which would
      # cause the app to crash)
      conflict <- "sample_id" %in% colnames(metadata) & input[[sample_id_inputId]] != "sample_id"
      if (conflict) {
        metadata <- metadata %>% 
          dplyr::rename(sample_id_original = sample_id)
        
        rlang::warn(class = "sample_id_conflict",
                    message = paste("The column originally named \"sample_id\" was renamed",
                                    "as \"sample_id_original\" to avoid duplicate column names."))
      }
      
      prepped_metadata <- withCallingHandlers(
        expr = {
          metadata %>% 
            dplyr::mutate(dplyr::across(tidyselect::any_of(input[[date_column_inputId]]), 
                                        date_with_text)) %>% 
            dplyr::rename(sample_id = input[[sample_id_inputId]])
        },
        text_in_dates = function(c) {
          rlang::warn(class = "text_in_dates",
                      message = c$message)
        })
      
      return(prepped_metadata)
    })
  
  merged_metadata <- purrr::reduce(metadata_list_prepped,
                                   dplyr::full_join,
                                   by = "sample_id")
  return(merged_metadata)
}
