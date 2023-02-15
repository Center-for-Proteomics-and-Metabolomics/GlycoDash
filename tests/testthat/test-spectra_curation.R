library(glycodash)

test_that("analyte_meets_criteria is TRUE when the analyte meets all criteria", {
  
  test_df <- tibble::tibble(sample_name = c("sample01_pl1_A01"),
                            analyte = c("IgGI1H3N4"),
                            charge = c("2+"),
                            cluster = c("IgGI"),
                            absolute_intensity_background_subtracted = 72000,
                            mass_accuracy_ppm = c(10),
                            isotopic_pattern_quality = c(0.05),
                            sn = c(36))
  
  checked_data <- check_analyte_quality_criteria(my_data = test_df,
                                                 min_ppm_deviation = -20,
                                                 max_ppm_deviation = 20,
                                                 max_ipq = 0.2,
                                                 min_sn = 9,
                                                 criteria_to_consider = c("Mass accuracy",
                                                                          "S/N",
                                                                          "IPQ"))
  
  
  expect_true(checked_data$analyte_meets_criteria[1])
  
})

test_that("analyte_meets_criteria is FALSE when one or more criteria are not met", {
  
  test_df <- tibble::tibble(sample_name = c("sample01_pl1_A01", "sample01_pl1_A01"),
                            analyte = c("IgGI1H3N4", "IgGIH3N4F1"),
                            charge = c("2+", "2+"),
                            cluster = c("IgGI", "IgGI"),
                            absolute_intensity_background_subtracted = c(72000, 45000),
                            mass_accuracy_ppm = c(10, 10),
                            isotopic_pattern_quality = c(0.3, 0.05),
                            sn = c(4, 4))
  
  checked_data <- check_analyte_quality_criteria(my_data = test_df,
                                                 min_ppm_deviation = -20,
                                                 max_ppm_deviation = 20,
                                                 max_ipq = 0.2,
                                                 min_sn = 9,
                                                 criteria_to_consider = c("Mass accuracy",
                                                                          "S/N",
                                                                          "IPQ"))
  # In the first row S/N is too low and IPQ is too high
  expect_false(checked_data$analyte_meets_criteria[1])
  
  # In the 2nd row only the S/N is too low:
  expect_false(checked_data$analyte_meets_criteria[2])
})

test_that("check_analyte_quality_criteria() ignores criteria that are not in criteria_to_consider", {
  
  test_df <- tibble::tibble(sample_name = c("sample01_pl1_A01"),
                            analyte = c("IgGI1H3N4"),
                            charge = c("2+"),
                            cluster = c("IgGI"),
                            absolute_intensity_background_subtracted = 72000,
                            mass_accuracy_ppm = c(10),
                            isotopic_pattern_quality = c(0.3),
                            sn = c(36))
  
  checked_data <- check_analyte_quality_criteria(my_data = test_df,
                                                 min_ppm_deviation = -20,
                                                 max_ppm_deviation = 20,
                                                 max_ipq = 0.2,
                                                 min_sn = 9,
                                                 criteria_to_consider = c("Mass accuracy",
                                                                          "S/N"))
  
  # The IPQ in row 1 is too high, but IPQ is not in criteria_to_consider:
  expect_true(checked_data$analyte_meets_criteria[1])
  
})

test_that("analyte_meets_criteria is FALSE when one or more criteria are NA", {
  
  test_df <- tibble::tibble(sample_name = c("sample01_pl1_A01"),
                            analyte = c("IgGI1H3N4"),
                            charge = c("2+"),
                            cluster = c("IgGI"),
                            absolute_intensity_background_subtracted = 72000,
                            mass_accuracy_ppm = c(10),
                            isotopic_pattern_quality = c(NA),
                            sn = c(NA))
  
  checked_data <- check_analyte_quality_criteria(my_data = test_df,
                                                 min_ppm_deviation = -20,
                                                 max_ppm_deviation = 20,
                                                 max_ipq = 0.2,
                                                 min_sn = 9,
                                                 criteria_to_consider = c("Mass accuracy",
                                                                          "S/N",
                                                                          "IPQ"))
  
  # In the first row the mass accuracy criteria is met, but both the S/N and IPQ are NA:
  expect_false(checked_data$analyte_meets_criteria[1])
  
})

test_that("check_analyte_quality_criteria() correctly identifies uncalibrated clusters in spectra", {
  test_df <- tibble::tibble(sample_name = c("sample01_pl1_A01", 
                                            "sample01_pl1_A01",
                                            "sample01_pl1_A01", 
                                            "sample01_pl1_A01",
                                            "sample02_pl1_A02",
                                            "sample02_pl1_A02"),
                            analyte = c("IgGI1H3N4", 
                                        "IgGI1H3N4F1",
                                        "IgGIV1H3N4", 
                                        "IgGIV1H3N4F1",
                                        "IgGI1H3N4", 
                                        "IgGI1H3N4F1"),
                            charge = c("2+", 
                                       "2+",
                                       "2+",
                                       "2+",
                                       "2+",
                                       "2+"),
                            cluster = c("IgGI", 
                                        "IgGI",
                                        "IgGIV",
                                        "IgGIV",
                                        "IgGI",
                                        "IgGI"),
                            absolute_intensity_background_subtracted = c(NA,
                                                                         NA,
                                                                         72000, 
                                                                         45000,
                                                                         NA,
                                                                         8000),
                            mass_accuracy_ppm = c(NA,
                                                  NA,
                                                  10, 
                                                  8,
                                                  NA,
                                                  12),
                            isotopic_pattern_quality = c(NA,
                                                         NA,
                                                         0.05, 
                                                         0.05,
                                                         NA,
                                                         0.05),
                            sn = c(NA,
                                   NA,
                                   36, 
                                   36,
                                   NA,
                                   36))
  
  checked_data <- check_analyte_quality_criteria(my_data = test_df, 
                                                 min_ppm_deviation = -20,
                                                 max_ppm_deviation = 20,
                                                 max_ipq = 0.2,
                                                 min_sn = 9,
                                                 criteria_to_consider = c("Mass accuracy",
                                                                          "S/N",
                                                                          "IPQ"))
  
  expect_equal(checked_data$uncalibrated,
               expected = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  
})

test_that("summarize_spectra_checks() verifies the existence of required columns", {
  data("example_data")
  wrong_data <- example_data %>% 
    dplyr::select(-c(sample_name, group))
  expect_error(summarize_spectra_checks(data = wrong_data),
               regexp = "The data doesn't contain the required column\\(s\\) sample_name and cluster and criteria_check\\.")
  
})

test_that("summarize_spectra_checks() returns one row per cluster per spectrum", {
  data("example_data")
  example_data <- define_clusters(data = example_data,
                                  cluster_keywords = "IgGI1")
  to_replace <- sample(1:nrow(example_data), nrow(example_data)/2)
  example_data$cluster[to_replace] <- "IgGII1"
  checked_data <- check_analyte_quality_criteria(data = example_data,
                                    min_ppm_deviation = -20,
                                    max_ppm_deviation = 20,
                                    max_ipq = 0.2,
                                    min_sn = 9)
  expect_equal(nrow(summarize_spectra_checks(data_checked = checked_data)),
               length(unique(example_data$cluster)) * length(unique(example_data$sample_name)))
})

test_that("curate_spectra() issues warnings if all/no spectra pass curation.", {
  # If quality criteria are very lenient, even the spectra that cut-offs are
  # based on (spectra that should not pass) pass the quality criteria checks.
  # This leads to very high cut-offs, which causes all spectra to fail curation:
  expect_warning(curate_spectra(data = example_data,
                                cluster_keywords = "IgGI1",
                                min_ppm_deviation = 10000,
                                max_ppm_deviation = 10000,
                                max_ipq = 10000,
                                min_sn = 0,
                                cut_off_basis = "PBS"),
               "None of the spectra passed curation\\.")
  # On the other hand, when criteria are really strict, none of the spectra pass
  # either.
  expect_warning(curate_spectra(data = example_data,
                                cluster_keywords = "IgGI1",
                                min_ppm_deviation = 0,
                                max_ppm_deviation = 0,
                                max_ipq = 0,
                                min_sn = 10000,
                                cut_off_basis = "PBS"),
                 "None of the spectra passed curation\\.")
})

