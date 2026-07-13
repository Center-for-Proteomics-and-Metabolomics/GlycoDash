testthat::test_that(
  desc = "BEAT LaCyTools data completes the standard pipeline", 
  code = {
    
    # TODO: Test actual pipeline

    # Read metadata file
    metadata_path <- extdata_path(
      "publication_data",
      "beat",
      "beat_metadata.xlsx"
    )
    
    metadata <- readxl::read_excel(
      metadata_path, na = c("", "NA"), col_types = "text"
    )
    
    # Example test: metadata should be a tibble
    testthat::expect_s3_class(metadata, "tbl_df")
  
  }
)
