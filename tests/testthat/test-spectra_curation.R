library(GlycoDash)

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

test_that("summarize_spectra_checks() calculates the sum intensity correctly", {
  
  test_df <- tibble::tibble(absolute_intensity_background_subtracted = c(18000,
                                                                         24000,
                                                                         9000,
                                                                         1400,
                                                                         36000,
                                                                         54000),
                            fraction = c(0.9,
                                         0.8,
                                         0.9,
                                         0.7,
                                         0.9,
                                         0.9),
                            sample_name = c("sample01_pl1_A01",
                                            "sample01_pl1_A01",
                                            "sample01_pl1_A01",
                                            "sample02_pl1_A02",
                                            "sample02_pl1_A02",
                                            "sample02_pl1_A02"),
                            cluster = c("IgGI",
                                        "IgGI",
                                        "IgGII",
                                        "IgGI",
                                        "IgGI",
                                        "IgGII"),
                            analyte_meets_criteria = c(TRUE,
                                                       TRUE,
                                                       TRUE,
                                                       TRUE,
                                                       FALSE,
                                                       TRUE),
                            uncalibrated = rep(TRUE, 6))
  
  expect_equal(object = summarize_spectra_checks(test_df)$sum_intensity,
               expected = c(50000, 10000, 2000, 60000))
  
})

test_that("summarize_spectra_checks() calculates the percentage of passing analytes correctly", {
  
  test_df <- tibble::tibble(absolute_intensity_background_subtracted = c(18000,
                                                                         24000,
                                                                         9000,
                                                                         1400,
                                                                         36000,
                                                                         54000),
                            fraction = c(0.9,
                                         0.8,
                                         0.9,
                                         0.7,
                                         0.9,
                                         0.9),
                            sample_name = c("sample01_pl1_A01",
                                            "sample01_pl1_A01",
                                            "sample01_pl1_A01",
                                            "sample02_pl1_A02",
                                            "sample02_pl1_A02",
                                            "sample02_pl1_A02"),
                            cluster = c("IgGI",
                                        "IgGI",
                                        "IgGII",
                                        "IgGI",
                                        "IgGI",
                                        "IgGII"),
                            analyte_meets_criteria = c(TRUE,
                                                       TRUE,
                                                       FALSE,
                                                       TRUE,
                                                       FALSE,
                                                       TRUE),
                            uncalibrated = rep(FALSE, 6))
  
  expect_equal(object = summarize_spectra_checks(test_df)$passing_analyte_percentage,
               expected = c(100, 0, 50, 100))
  
})

test_that("curate_spectra() lets the correct spectra pass and fail.", {
  
  test_checked_data <- tibble::tibble(absolute_intensity_background_subtracted = c(18000,
                                                                                   24000,
                                                                                   9000,
                                                                                   1400,
                                                                                   36000,
                                                                                   54000),
                                      fraction = c(0.9,
                                                   0.8,
                                                   0.9,
                                                   0.7,
                                                   0.9,
                                                   0.9),
                                      sample_name = c("sample01_pl1_A01",
                                                      "sample01_pl1_A01",
                                                      "sample01_pl1_A01",
                                                      "sample02_pl1_A02",
                                                      "sample02_pl1_A02",
                                                      "sample02_pl1_A02"),
                                      cluster = c("IgGI",
                                                  "IgGI",
                                                  "IgGII",
                                                  "IgGI",
                                                  "IgGI",
                                                  "IgGII"),
                                      charge = rep("2+", 6),
                                      analyte_meets_criteria = c(TRUE,
                                                                 FALSE,
                                                                 TRUE,
                                                                 TRUE,
                                                                 FALSE,
                                                                 TRUE),
                                      failed_criteria = c(NA, "Mass accuracy", NA, NA, "IPQ and S/N", NA),
                                      uncalibrated = rep(FALSE, 6))
  
  test_summarized_checks <- tibble::tibble(passing_analyte_percentage = c(50, 
                                                                          100, 
                                                                          50, 
                                                                          100),
                                           sum_intensity = c(20000,
                                                             10000, 
                                                             2000, 
                                                             60000),
                                           uncalibrated = c(rep(FALSE, 4)),
                                           sample_name = c("sample01_pl1_A01",
                                                           "sample01_pl1_A01",
                                                           "sample02_pl1_A02",
                                                           "sample02_pl1_A02"),
                                           cluster = c("IgGI", "IgGII", "IgGI", "IgGII"))
  
  test_cut_offs <- tibble::tibble(cut_off_sum_intensity = c(5000, 20000),
                                  cut_off_passing_analyte_percentage = c(30, 60),
                                  cluster = c("IgGI", "IgGII"))
  
  expect_equal(object = curate_spectra(checked_data = test_checked_data,
                                       summarized_checks = test_summarized_checks,
                                       cut_offs = test_cut_offs)$has_passed_spectra_curation,
               expected = c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE))
})

test_that("curate_spectra() reports the accurate reasons for failure.", {
  
  test_checked_data <- tibble::tibble(absolute_intensity_background_subtracted = c(18000,
                                                                                   24000,
                                                                                   9000,
                                                                                   1400,
                                                                                   36000,
                                                                                   54000),
                                      fraction = c(0.9,
                                                   0.8,
                                                   0.9,
                                                   0.7,
                                                   0.9,
                                                   0.9),
                                      sample_name = c("sample01_pl1_A01",
                                                      "sample01_pl1_A01",
                                                      "sample01_pl1_A01",
                                                      "sample02_pl1_A02",
                                                      "sample02_pl1_A02",
                                                      "sample02_pl1_A02"),
                                      cluster = c("IgGI",
                                                  "IgGI",
                                                  "IgGII",
                                                  "IgGI",
                                                  "IgGI",
                                                  "IgGII"),
                                      charge = rep("2+", 6),
                                      analyte_meets_criteria = c(TRUE,
                                                                 FALSE,
                                                                 TRUE,
                                                                 TRUE,
                                                                 TRUE,
                                                                 TRUE),
                                      failed_criteria = c(NA, "Mass accuracy", NA, NA, "IPQ and S/N", NA),
                                      uncalibrated = rep(FALSE, 6))
  
  test_summarized_checks <- tibble::tibble(passing_analyte_percentage = c(50, 
                                                                          100, 
                                                                          100, 
                                                                          100),
                                           sum_intensity = c(20000,
                                                             10000, 
                                                             42000, 
                                                             60000),
                                           uncalibrated = c(rep(FALSE, 4)),
                                           sample_name = c("sample01_pl1_A01",
                                                           "sample01_pl1_A01",
                                                           "sample02_pl1_A02",
                                                           "sample02_pl1_A02"),
                                           cluster = c("IgGI", "IgGII", "IgGI", "IgGII"))
  
  test_cut_offs <- tibble::tibble(cut_off_sum_intensity = c(30000, 20000),
                                  cut_off_passing_analyte_percentage = c(60, 30),
                                  cluster = c("IgGI", "IgGII"))
  
  expect_equal(object = curate_spectra(checked_data = test_checked_data,
                                       summarized_checks = test_summarized_checks,
                                       cut_offs = test_cut_offs)$reason_for_failure,
               expected = c("Percentage of passing analytes and sum intensity below cut-offs.", 
                            "Percentage of passing analytes and sum intensity below cut-offs.", 
                            "Sum intensity below cut-off.", 
                            NA, NA, NA))
})

