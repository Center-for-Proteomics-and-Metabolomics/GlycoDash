# This file contains functions used for merging Skyline data with GlyCounter
# fragmentation data.



#' Calculate isotopic patterns for all analytes in a Skyline dataset
#'
#' @description
#' Extracts all unique molecular formula and charge state combinations from the
#' Skyline data and computes the nominal isotopic pattern (M, M+1, M+2, ...) for
#' each ion using [calculate_ion_fine_structure()] and
#' [collapse_to_nominal_pattern()].
#'
#' @param skyline_data A data frame containing at least the molecular formula
#'   and precursor charge columns.
#' @param charge_carrier A character string with the element symbol used as the
#'   charge carrier. Default is `"H"`.
#'
#' @return A nested named list: `patterns[[formula]][[charge]]` holds the
#'   nominal isotopic pattern (as returned by [collapse_to_nominal_pattern()])
#'   for the ion with that formula and charge state.
#'
#' @noRd
calculate_skyline_isotopic_patterns <- function(
    skyline_data,
    charge_carrier = "H"  # Fix to hydrogen for now
  ) {

  # Extract unique composition + charge combinations
  compositions <- skyline_data %>%
    dplyr::select(molecular_formula, charge) %>%
    dplyr::distinct() %>%
    # Ensure charge is integer (should already be the case).
    dplyr::mutate(charge = as.integer(charge))
  
  # Check against invalid charges or molecular formulas
  invalid <- (
    is.na(compositions$molecular_formula) | 
    compositions$molecular_formula == "" |
    is.na(compositions$charge) |
    compositions$charge == 0L
  )
  if (any(invalid)) {
    stop(paste0(
      "GlyCounter merge requires non-empty `molecular formula` ", 
      "and non-zero charge for all Skyline rows."
    ))
  }

  # Get nominal isotopic pattern(M, M+1, M+2, ...) for each ion
  patterns <- list()
  for (i in seq_len(nrow(compositions))) {
    formula <- compositions$molecular_formula[[i]]
    charge <- compositions$charge[[i]]
    charge_name <- as.character(charge)

    fine_structure_pattern <- calculate_ion_fine_structure(
      formula = formula, charge = charge, carrier = charge_carrier
    )

    nominal_pattern <- collapse_to_nominal_pattern(fine_structure_pattern)

    patterns[[formula]][[charge_name]] <- nominal_pattern
  }

  return(patterns)
}



#' Extract the top-n most probable isotope m/z values
#'
#' @description
#' For each unique molecular formula and charge state combination in the
#' isotopic pattern list, selects the `n_peaks` most probable nominal isotope
#' peaks and returns their m/z values in wide format with one column per rank.
#'
#' @param isotopic_patterns A nested named list as returned by
#'   [calculate_skyline_isotopic_patterns()].
#' @param n_peaks An integer giving the number of top peaks to extract per
#'   formula-charge combination. Default is `3`.
#'
#' @return A data frame with one row per formula-charge combination and columns
#'   `mz_prob1`, `mz_prob2`, ... for the m/z values of the top isotope peaks
#'   in descending probability order.
#'
#' @noRd
extract_top_isotopic_mz <- function(
    isotopic_patterns,
    n_peaks = 3
  ) {
  purrr::imap_dfr(
    isotopic_patterns, function(charge_list, formula) {
      purrr::imap_dfr(
        charge_list, function(peak_list, charge) {

          charge_int <- as.integer(charge)

          peak_df <- purrr::map_dfr(peak_list, tibble::as_tibble)

          peak_df %>%
            dplyr::arrange(dplyr::desc(prob)) %>%
            dplyr::slice_head(n = n_peaks) %>%
            dplyr::mutate(
              rank = dplyr::row_number(),
              # For negative charges, m/z is still reported as positive value
              mz = abs(mass / charge)
            ) %>%
            dplyr::select(molecular_formula, charge, rank, mz)
        }
      )
    }
  ) %>%
    tidyr::pivot_wider(
      names_from = rank, values_from = mz,
      names_prefix = "mz_prob"
    )
}



#' Extract isotope m/z candidates with optional probability filtering
#'
#' @description
#' Extracts nominal isotope peaks from the isotopic pattern list, optionally
#' filters by minimum relative probability, and returns the top `n_peaks` peaks
#' per formula-charge combination in long format (one row per peak).
#'
#' @param isotopic_patterns A nested named list as returned by
#'   [calculate_skyline_isotopic_patterns()].
#' @param n_peaks An integer giving the maximum number of candidate peaks to
#'   retain per formula-charge combination. Default is `3`.
#' @param min_relative_prob A numeric value between 0 and 1. Peaks with a
#'   relative probability below this threshold are excluded before applying
#'   `n_peaks`. Set to `NULL` (default) to skip filtering.
#'
#' @return A data frame with one row per formula-charge-isotope combination,
#'   containing columns `isotope_rank`, `isotope_group`, `extra_neutrons`,
#'   `isotope_mz`, `isotope_prob`, `isotope_prob_relative`, and
#'   `n_fine_structure_peaks`.
#'
#' @noRd
extract_isotopic_mz_candidates <- function(
    isotopic_patterns,
    n_peaks = 3,
    min_relative_prob = NULL
  ) {

  candidates <- purrr::imap_dfr(
    isotopic_patterns, function(charge_list, formula) {
      purrr::imap_dfr(
        charge_list, function(peak_list, charge_char) {

          charge_int <- as.integer(charge_char)

          peak_df <- purrr::map_dfr(peak_list, tibble::as_tibble)

          peak_df %>%
            dplyr::arrange(dplyr::desc(prob)) %>%
            dplyr::mutate(
              molecular_formula = formula,
              charge = charge_int,
              isotope_rank = dplyr::row_number(),
              isotope_mz = abs(mass / charge),
              isotope_prob = prob,
              isotope_prob_relative = prob_relative
            ) %>%
            dplyr::select(
              molecular_formula,
              charge,
              isotope_rank,
              isotope_group,
              extra_neutrons,
              isotope_mz,
              isotope_prob,
              isotope_prob_relative,
              n_fine_structure_peaks
            )
        }
      )
    }
  )

  if (!is.null(min_relative_prob)) {
    candidates <- candidates %>%
      dplyr::filter(isotope_prob_relative >= min_relative_prob)
  }

  slice <- candidates %>%
    dplyr::group_by(molecular_formula, charge) %>%
    dplyr::slice_head(n = n_peaks) %>%
    dplyr::ungroup()

  return(slice)
}



#' Join Skyline data with isotope m/z candidates and compute m/z windows
#'
#' @description
#' Performs a many-to-many left join between the prepared Skyline data and the
#' isotope m/z candidates on molecular formula and charge state, then computes
#' lower and upper m/z bounds for each candidate using the `ppm_tolerance`
#' column already present in `skyline_prepped`.
#'
#' @param skyline_prepped A data frame as returned by [prepare_skyline_data()],
#'   including a `ppm_tolerance` column.
#' @param isotope_mz_candidates A data frame as returned by
#'   [extract_isotopic_mz_candidates()].
#'
#' @return A data frame combining `skyline_prepped` and `isotope_mz_candidates`,
#'   with additional columns `isotope_mz_min` and `isotope_mz_max` representing
#'   the m/z search window for each candidate.
#'
#' @noRd
expand_skyline_isotope_candidates <- function(
    skyline_prepped,
    isotope_mz_candidates
) {
  skyline_prepped %>%
    dplyr::left_join(
      isotope_mz_candidates,
      by = c("molecular_formula", "charge"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      isotope_mz_min = isotope_mz * (1 - ppm_tolerance * 1e-6),
      isotope_mz_max = isotope_mz * (1 + ppm_tolerance * 1e-6)
    )
}



#' Load and process GlyCounter output files
#'
#' @description
#' Reads one or more GlyCounter output files, selects relevant columns, and
#' combines them into a single data frame. Sample names are recovered from the
#' original filenames by stripping the `_GlyCounter` suffix and everything that
#' follows.
#'
#' @param files A named list where names are the original filenames and values
#'   are the corresponding file paths in memory (e.g. as returned by a Shiny
#'   file input).
#'
#' @return A data frame with one row per MS2 scan across all input files,
#'   containing columns `sample`, `ScanNumber`, `RetentionTime`, `PrecursorMZ`,
#'   `LikelyGlycoSpectrum`, `DissociationType`, `fragment_sum`, and individual
#'   fragment ion intensity columns (named as `"<mz>, <annotation>"`).
#'
#' @noRd
load_glycounter_data <- function(files) {
  # Read and process all OxoSignal files
  purrr::imap_dfr(files, function(file, name) {
    read.delim(
      file = file,
      header = TRUE,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ) %>%
      dplyr::select(
        # Required output variables
        ScanNumber,
        RetentionTime,
        PrecursorMZ,
        LikelyGlycoSpectrum,
        DissociationType,
        # Fragment columns are named like "204.0867, HexNAc"
        tidyr::matches("^\\d+\\.\\d+,\\s")
      ) %>%
      dplyr::mutate(
        # Recover the sample name from the original filename
        sample = sub("_?GlyCounter.*$", "", basename(name)),
        # Convert "True"/"False" text values to logical values
        LikelyGlycoSpectrum = tolower(LikelyGlycoSpectrum) == "true",
        # Sum all fragment-ion intensities within each scan.
        # The individual fragment columns are retained as well.
        fragment_sum = rowSums(
          dplyr::pick(tidyr::matches("^\\d+\\.\\d+,\\s")),
          na.rm = TRUE
        )
      ) %>%
      dplyr::relocate(sample, .before = ScanNumber) %>%
      dplyr::relocate(fragment_sum, .after = DissociationType)
  })
}


#' Extract fragment ion column names from GlyCounter data
#'
#' @description
#' Identifies and returns the names of fragment ion intensity columns in a
#' GlyCounter data frame. These columns follow the naming pattern
#' `"<m/z>, <annotation>"` (e.g. `"204.0867, HexNAc"`).
#'
#' @param glycounter_data A data frame as returned by [load_glycounter_data()].
#'
#' @return A character vector of fragment ion column names.
#'
#' @noRd
extract_fragment_cols <- function(glycounter_data) {
  names <- colnames(glycounter_data)
  fragment_cols <- names[stringr::str_detect(names, "^\\d+\\.\\d+,\\s")]
  return(fragment_cols)
}


#' Prepare Skyline data for merging with GlyCounter data
#'
#' @description
#' Adds a stable row identifier (`skyline_row_id`) and a `ppm_tolerance` column.
#' These additions are required by downstream matching functions.
#'
#' @param skyline_data A data frame as returned by [load_skyline_data()].
#' @param ppm_tolerance A numeric value specifying the mass accuracy window in
#'   parts per million used for matching GlyCounter precursor m/z values to
#'   isotope candidates. Default is `10`.
#'   
#' @return A data frame identical to `skyline_data` with additional columns
#'   `skyline_row_id` (integer row index) and `ppm_tolerance`.
#'
#' @noRd
prepare_skyline_data <- function(
    skyline_data,
    ppm_tolerance = 10
  ) {
  skyline_data %>%
    dplyr::mutate(
      # Add a stable row identifier so GlyCounter summaries can be calculated
      # per Skyline row and safely joined back afterwards.
      skyline_row_id = dplyr::row_number(),
      # Add ppm tolerance.
      ppm_tolerance = ppm_tolerance
    )
}



#' Filter GlyCounter scans to isotope m/z and retention time windows
#'
#' @description
#' Joins the expanded Skyline isotope candidate table with the GlyCounter
#' scan-level data on sample, then filters to scans whose precursor m/z falls
#' within the isotope m/z window and whose retention time falls within the
#' Skyline peak boundaries. When multiple isotope candidates match the same
#' scan, only the best (lowest m/z error) match is retained.
#'
#' @param skyline_isotope_candidates A data frame as returned by
#'   [expand_skyline_isotope_candidates()].
#' @param glycounter_data A data frame as returned by [load_glycounter_data()].
#'
#' @return A filtered data frame with one row per unique
#'   `(skyline_row_id, sample, ScanNumber)` combination, containing both
#'   Skyline isotope candidate columns and GlyCounter scan-level columns, plus a
#'   `glycounter_mz_error_ppm` column.
#'
#' @noRd
extract_glycounter_candidates <- function(
    skyline_isotope_candidates,
    glycounter_data
) {
  skyline_isotope_candidates %>%
    dplyr::select(
      skyline_row_id,
      sample,
      `Best.Retention.Time`,
      Min.Start.Time,
      Max.End.Time,
      isotope_rank,
      isotope_group,
      extra_neutrons,
      isotope_mz,
      isotope_prob,
      isotope_prob_relative,
      isotope_mz_min,
      isotope_mz_max
    ) %>%
    dplyr::left_join(
      glycounter_data,
      by = "sample",
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(
      dplyr::between(PrecursorMZ, isotope_mz_min, isotope_mz_max),
      dplyr::between(RetentionTime, Min.Start.Time, Max.End.Time)
    ) %>%
    dplyr::mutate(
      glycounter_mz_error_ppm = (
        1e6 * abs(PrecursorMZ - isotope_mz) / isotope_mz
      )
    ) %>%
    dplyr::group_by(skyline_row_id, sample, ScanNumber) %>%
    dplyr::slice_min(
      order_by = glycounter_mz_error_ppm,
      n = 1,
      with_ties = FALSE
    ) %>%
    dplyr::ungroup()
}



#' Summarize GlyCounter scan data per Skyline row
#'
#' @description
#' Aggregates the scan-level GlyCounter candidate data to produce one summary
#' row per Skyline analyte-sample combination (`skyline_row_id`). Summaries
#' include scan counts, retention time statistics, precursor m/z statistics,
#' matched isotope metadata, and total fragment ion intensities. Optionally
#' converts fragment intensities to relative abundances (percentage of total
#' fragment signal per Skyline row).
#'
#' @param glycounter_candidates A data frame as returned by
#'   [extract_glycounter_candidates()].
#' @param fragment_cols A character vector of fragment ion column names as
#'   returned by [extract_fragment_cols()].
#' @param relative_abundances Logical. If `TRUE` (default), fragment ion
#'   intensities are converted to percentages of `fragment_sum` for each
#'   Skyline row.
#'
#' @return A data frame with one row per `skyline_row_id`, containing
#'   aggregated GlyCounter metrics and fragment ion intensities (or relative
#'   abundances when `relative_abundances = TRUE`).
#'
#' @noRd
summarize_glycounter_data <- function(
    glycounter_candidates,
    fragment_cols,
    relative_abundances = TRUE
) {
  # Aggregate the scan-level GlyCounter data to one summary per Skyline row.
  # This is where repeated fragmentation events are collapsed into one total
  # signal profile for each sample/analyte combination.
  #
  # Note: GlyCounter precursor m/z values may match M, M+1, M+2, etc. rather
  # than only the monoisotopic precursor m/z. Therefore, isotope-match metadata
  # is retained here for traceability.
  summary <- glycounter_candidates %>%
    dplyr::group_by(skyline_row_id) %>%
    dplyr::summarize(
      # Number of GlyCounter scans contributing to this Skyline row.
      glycounter_scan_count = dplyr::n(),

      # Keep contributing scan numbers for traceability.
      glycounter_scan_numbers = stringr::str_c(
        sort(unique(ScanNumber)), collapse = ";"
      ),

      # DissociationType can vary across contributing scans.
      glycounter_dissociation_types = stringr::str_c(
        sort(unique(DissociationType)), collapse = ";"
      ),

      # Keep track of which isotope-envelope peaks were matched.
      # These can be M, M+1, M+2, etc.
      glycounter_matched_isotope_groups = stringr::str_c(
        sort(unique(isotope_group)), collapse = ";"
      ),

      # Keep track of the probability ranks of the matched isotope peaks.
      # Rank 1 is the most probable isotope peak for that molecular formula
      # and charge state, not necessarily the monoisotopic peak.
      glycounter_matched_isotope_ranks = stringr::str_c(
        sort(unique(isotope_rank)), collapse = ";"
      ),

      # Count how many contributing scans were flagged as likely glyco spectra.
      glycounter_likely_glyco_count = sum(LikelyGlycoSpectrum, na.rm = TRUE),

      # Summaries of matched retention times.
      glycounter_retention_time_mean = mean(RetentionTime, na.rm = TRUE),
      glycounter_retention_time_min = min(RetentionTime, na.rm = TRUE),
      glycounter_retention_time_max = max(RetentionTime, na.rm = TRUE),

      # Summaries of observed GlyCounter precursor m/z values.
      glycounter_precursor_mz_mean = mean(PrecursorMZ, na.rm = TRUE),

      # Summaries of the theoretical isotope m/z values that were matched.
      # These are candidate isotope-envelope m/z values, not necessarily
      # monoisotopic m/z values.
      glycounter_matched_isotope_mz_mean = mean(isotope_mz, na.rm = TRUE),

      # Precursor m/z error is calculated relative to the matched isotope m/z,
      # not relative to the Skyline monoisotopic m/z.
      glycounter_mz_error_ppm_mean = mean(glycounter_mz_error_ppm, na.rm = TRUE),
      glycounter_mz_error_ppm_min = min(glycounter_mz_error_ppm, na.rm = TRUE),

      # Sum total fragment signal across all matched scans.
      fragment_sum = sum(fragment_sum, na.rm = TRUE),

      # Sum every individual fragment-ion column across matched scans so each
      # Skyline row ends up with one total intensity per fragment ion.
      dplyr::across(dplyr::all_of(fragment_cols), ~ sum(.x, na.rm = TRUE)),

      .groups = "drop"
    )

  # Optional: report relative abundances for fragments.
  # This converts each fragment-ion intensity to a percentage of the total
  # fragment signal for that Skyline row.
  if (relative_abundances) {
    summary <- summary %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(fragment_cols),
          ~ dplyr::if_else(fragment_sum > 0, .x / fragment_sum * 100, NA_real_)
        )
      )
  }

  return(summary)
}



#' Merge Skyline and GlyCounter summary data
#'
#' @description
#' Left-joins the GlyCounter summary onto the full prepared Skyline table,
#' matching on `skyline_row_id`. Because the join starts from the Skyline table,
#' the number of output rows is identical to the number of rows in
#' `skyline_prepped`. Helper columns added by [prepare_skyline_data()]
#' (`skyline_row_id`, `ppm_tolerance`) are removed from the output.
#'
#' @param skyline_prepped A data frame as returned by [prepare_skyline_data()].
#' @param glycounter_summary A data frame as returned by
#'   [summarize_glycounter_data()].
#'
#' @return A data frame with all Skyline analyte-sample columns plus the
#'   GlyCounter summary columns for matched rows (`NA` for unmatched rows).
#'
#' @noRd
merge_skyline_glycounter <- function(
    skyline_prepped,
    glycounter_summary
  ) {
  # Add the GlyCounter summaries back to the full Skyline table.
  # Because this is a left join starting from Skyline, the number of rows
  # remains identical to the original Skyline analyte-sample table.
  skyline_prepped %>%
    dplyr::left_join(glycounter_summary, by = "skyline_row_id") %>%
    # Remove helper columns only needed for matching.
    dplyr::select(-skyline_row_id, -ppm_tolerance)
}

