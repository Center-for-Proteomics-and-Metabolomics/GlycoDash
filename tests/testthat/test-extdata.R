testthat::test_that("BEAT test files are installed", {
  beat_directory <- extdata_path(
    "publication_data",
    "beat"
  )
  
  expected_files <- c(
    "beat_metadata.xlsx",
    "beat_plate_layout.xlsx",
    "beat_sample_types.xlsx",
    "beat_summary_pl1-3_lacytools.txt",
    "beat_summary_pl4-5_lacytools.txt",
    "beat_summary_pl6-12_lacytools.txt"
  )
  
  testthat::expect_setequal(
    list.files(beat_directory),
    expected_files
  )
})


testthat::test_that("Keratinocytes test files are installed", {
  keratinocytes_directory <- extdata_path(
    "publication_data",
    "keratinocytes"
  )
  
  expected_files <- c(
    "skyline_keratinocytes_sample_ids.xlsx",
    "skyline_keratinocytes_sample_types.xlsx",
    "skyline_keratinocytes.csv"
  )
  
  testthat::expect_setequal(
    list.files(keratinocytes_directory),
    expected_files
  )
})


testthat::test_that("MAbs test files are installed", {
  mabs_directory <- extdata_path(
    "publication_data",
    "mabs"
  )
  
  expected_files <- c(
    "mabs_batch1_summary_lacytools.txt",
    "mabs_batch2_summary_lacytools.txt",
    "mabs_plate_layout.xlsx"
  )
  
  testthat::expect_setequal(
    list.files(mabs_directory),
    expected_files
  )
})


testthat::test_that("VisuCon test files are installed", {
  visucon_directory <- extdata_path(
    "sweetsuite",
    "visucon"
  )
  
  expected_files <- c(
    "sweetsuite_visucon_results.xlsx",
    "sweetsuite_visucon_plate_layout.xlsx"
  )
  
  testthat::expect_setequal(
    list.files(visucon_directory),
    expected_files
  )
})
