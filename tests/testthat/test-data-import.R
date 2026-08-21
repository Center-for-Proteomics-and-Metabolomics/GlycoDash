testthat::test_that("BEAT LaCyTools summaries are imported correctly", {
  
  path1 <- extdata_path(
    "publication_data",
    "beat",
    "beat_summary_pl1-3_lacytools.txt"
  )
  path2 <- extdata_path(
    "publication_data",
    "beat",
    "beat_summary_pl4-5_lacytools.txt"
  )
  path3 <- extdata_path(
    "publication_data",
    "beat",
    "beat_summary_pl6-12_lacytools.txt"
  )
  
  raw1 <- read_non_rectangular(path1)
  raw2 <- read_non_rectangular(path2)
  raw3 <- read_non_rectangular(path3)
  
  conv1 <- convert_lacytools_summary(raw1)
  conv2 <- convert_lacytools_summary(raw2)
  conv3 <- convert_lacytools_summary(raw3)
  
  combined <- dplyr::bind_rows(list(conv1, conv2, conv3))
  
  testthat::expect_s3_class(combined, "data.frame")
  
  testthat::expect_true(
    all(
      c(
        "sample_name",
        "analyte",
        "charge",
        "absolute_intensity_background_subtracted",
        "mass_accuracy_ppm",
        "isotopic_pattern_quality",
        "sn",
        "fraction",
        "exact_mass"
      ) %in% colnames(combined)
    )
  )
  
  testthat::expect_true(
    nrow(combined) == 99216
  )

})

