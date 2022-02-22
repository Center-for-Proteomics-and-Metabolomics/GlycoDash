test_that("curate_analytes() returns a dataframe with one row for each analyte + charge combination per cluster", {
  data("long_data")
  long_data <- curate_spectra(data = long_data,
                              min_ppm_deviation = -20,
                              max_ppm_deviation = 20,
                              max_ipq = 0.2,
                              min_sn = 9,
                              clusters_regex = "IgGI1",
                              group_to_filter = "Spike",
                              sample_type_to_filter = "CN")
  
  curated_spectra <- long_data %>%
    dplyr::filter(passed_spectra_curation == TRUE)
  
  curated_analytes <- curate_analytes(data = curated_spectra,
                                      group_to_ignore = "Total",
                                      sample_types_to_ignore = c("pool", 
                                                                 "IVIGg", 
                                                                 "CN", 
                                                                 "Visucon", 
                                                                 "PBS"),
                                      cut_off_percentage = 25)
  
  n_analyte_charge_per_cluster <- curated_spectra %>% 
    dplyr::distinct(., analyte, charge, cluster) %>% 
    dplyr::summarise(n = nrow(.)) %>% 
    as.numeric()
  
  expect_equal(nrow(curated_analytes),
               n_analyte_charge_per_cluster)
})
