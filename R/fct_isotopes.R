# This file contains functions that are used for calculating isotopic fine
# structure based on a molecular formula (elemental composition), and for
# collapsing the fine structure into M, M+1, M+2, etc. peaks.

# It uses constants defined in `constants.R`
# (ISOTOPES, PROTON_MASS, ELECTRON_MASS, EXTRA_NEUTRON_LOOKUP)



#' Convert an element symbol to its full element name
#'
#' @param symbol A character string with the element symbol (e.g. `"C"`, `"H"`,
#'   `"Na"`).
#'
#' @return A character string with the full element name (e.g. `"carbon"`).
#'
#' @noRd
symbol_to_element <- function(symbol) {

  symbol_element_map <- list(
    C = "carbon",
    H = "hydrogen",
    O = "oxygen",
    N = "nitrogen",
    S = "sulfur",
    Na = "sodium",
    K = "potassium",
    Fe = "iron"
  )

  return(symbol_element_map[[symbol]])
}



#' Parse a molecular formula string into a named list
#'
#' @description
#' Converts a molecular formula string (e.g. `"C6H12O6"`) into a named list
#' with element symbols as names and integer atom counts as values.
#' Example: `"C6H12O6"` becomes `list(C = 6, H = 12, O = 6)`.
#'
#' @param formula A character string representing a molecular formula.
#'
#' @return A named list where each name is an element symbol and each value is
#'   an integer atom count.
#'
#' @noRd
parse_molecular_formula <- function(formula) {
  composition <- list()

  matches <- gregexpr("([A-Z][a-z]*)([0-9]*)", formula, perl = TRUE)
  matched_text <- regmatches(formula, matches)[[1]]

  for (match_text in matched_text) {
    symbol <- sub("^([A-Z][a-z]*)([0-9]*)$", "\\1", match_text, perl = TRUE)
    count_text <- sub("^([A-Z][a-z]*)([0-9]*)$", "\\2", match_text, perl = TRUE)

    if (count_text == "") {
      count <- 1
    }
    else {
      count <- as.integer(count_text)
    }

    if (is.null(composition[[symbol]])) {
      composition[[symbol]] <- count
    }
    else {
      composition[[symbol]] <- composition[[symbol]] + count
    }
  }

  return(composition)
}



#' Convert a named composition list to a molecular formula string
#'
#' @description
#' The inverse of [parse_molecular_formula()]. Converts a named list of element
#' symbols and atom counts (e.g. `list(C = 6, H = 12, O = 6)`) back into a
#' molecular formula string (e.g. `"C6H12O6"`). Elements with a count of 0 are
#' omitted; elements with a count of 1 are written without a number.
#'
#' @param composition A named list where each name is an element symbol and each
#'   value is a non-negative integer atom count.
#'
#' @return A character string containing the molecular formula.
#'
#' @noRd
composition_to_formula <- function(composition) {
  formula_parts <- character(0)

  for (symbol in names(composition)) {
    count <- composition[[symbol]]

    if (count < 0) {
      stop("Negative atom count for ", symbol, ": ", count)
    }

    if (count == 0) {
      next
    }

    if (count == 1) {
      formula_parts <- c(formula_parts, symbol)
    }
    else {
      formula_parts <- c(formula_parts, paste0(symbol, count))
    }
  }

  paste0(formula_parts, collapse = "")
}



#' Apply a charge carrier to a molecular formula
#'
#' @description
#' Modifies a molecular formula by adding or removing charge-carrier atoms to
#' represent the ionised form of a molecule. For a positive charge, `abs(charge)`
#' carrier atoms are added; for a negative charge, they are removed. Stops with
#' an error if the formula does not contain enough carrier atoms for removal.
#'
#' @param formula A character string with the neutral molecular formula.
#' @param charge An integer giving the charge state (positive for cations,
#'   negative for anions). Must be non-zero.
#' @param carrier A character string with the element symbol of the charge
#'   carrier. Default is `"H"` (proton adduct).
#'
#' @return A character string containing the modified molecular formula.
#'
#' @noRd
apply_charge_carrier <- function(
    formula,
    charge,
    carrier = "H"
) {
  if (charge == 0) {
    stop("Charge must be non-zero.")
  }

  composition <- parse_molecular_formula(formula)
  carrier_count <- abs(as.integer(charge))

  if (charge > 0) {
    if (is.null(composition[[carrier]])) {
      composition[[carrier]] <- carrier_count
    }
    else {
      composition[[carrier]] <- composition[[carrier]] + carrier_count
    }
  }
  else {
    if (is.null(composition[[carrier]])) {
      current_count <- 0L
    }
    else {
      current_count <- composition[[carrier]]
    }

    if (current_count < carrier_count) {
      stop(
        "Cannot remove ", carrier_count, " ", carrier,
        " atoms from formula ", formula,
        "; only ", current_count, " present."
      )
    }

    composition[[carrier]] <- current_count - carrier_count
  }

  composition_to_formula(composition)
}




#' Generate all isotope count combinations
#'
#' @description
#' Generates all non-negative integer vectors of length `k` that sum to `n`.
#' These represent every way to distribute `n` identical atoms over `k`
#' isotopes. For example, `n = 2, k = 3` (two oxygen atoms with 3 stable
#' isotopes) produces `(2,0,0)`, `(1,1,0)`, `(0,0,2)`, etc.
#'
#' @param n Integer. Total number of atoms to distribute.
#' @param k Integer. Number of isotopes.
#'
#' @return A list of integer vectors of length `k`, each summing to `n`.
#'
#' @noRd
isotope_count_combis <- function(n, k) {
  if (k == 1) {
    # Base case: only one isotope.
    return(list(c(n)))
  }

  # Store all combinations here.
  combinations <- list()

  # Try every possible count for the first isotope.
  for (first_count in 0:n) {
    # Distribute remaining atoms over the remaining isotopes.
    remaining_combinations <- isotope_count_combis(
      n = n - first_count,
      k = k - 1
    )

    # Add first_count in front of each remaining combination.
    for (remaining_counts in remaining_combinations) {
      new_combination <- c(first_count, remaining_counts)
      combinations[[length(combinations) + 1]] <- new_combination
    }
  }

  return(combinations)
}



#' Calculate the multinomial probability for a set of isotope counts
#'
#' @description
#' Computes the multinomial probability for a given set of isotope counts and
#' their natural abundances. For example, for carbon with counts `(5, 1)`:
#' `probability = 6! / (5! * 1!) * P(C12)^5 * P(C13)^1`.
#' Computation is performed in log-space for numerical stability with large
#' molecules.
#'
#' @param counts A numeric vector of isotope atom counts summing to the total
#'   number of atoms for the element.
#' @param probs A numeric vector of natural abundance probabilities for each
#'   isotope, in the same order as `counts`.
#'
#' @return A single numeric value: the multinomial probability.
#'
#' @noRd
multinomial_prob <- function(counts, probs) {
  total <- sum(counts)

  # The gamma function generalizes factorials.
  # `lgamma(n + 1)` is therefore log(n!)
  log_prob <- lgamma(total + 1)

  # Subtract the log-factorials of the isotope counts.
  log_prob <- log_prob - sum(lgamma(counts + 1))

  # Add the probability terms.
  # Only include counts > 0 to avoid problems such as 0 * log(0).
  positive <- counts > 0
  log_prob <- log_prob + sum(counts[positive] * log(probs[positive]))

  return(exp(log_prob))
}



#' Calculate the isotopic fine structure for a single element
#'
#' @description
#' Computes all isotope peaks for a given element and atom count by evaluating
#' every combination of isotopes. Peaks with probability below `min_prob` are
#' skipped to reduce computation.
#'
#' @param symbol A character string with the element symbol (e.g. `"C"`).
#' @param atom_count An integer giving the number of atoms of this element.
#' @param min_prob A numeric threshold. Peaks with probability below this
#'   value are excluded.
#'
#' @return A list of peaks. Each peak is a named list with:
#'   \describe{
#'     \item{mass}{The exact mass of the peak.}
#'     \item{prob}{The probability of the peak.}
#'     \item{isotope_counts}{A named integer vector of non-zero isotope counts.}
#'   }
#'
#' @noRd
element_fine_structure <- function(
    symbol,
    atom_count,
    min_prob = 1e-12
  ) {

  element <- symbol_to_element(symbol)
  isotope_data <- ISOTOPES[[element]]

  isotope_labels <- names(isotope_data)
  isotope_masses <- purrr::map_dbl(
    isotope_labels, function(label) isotope_data[[label]]$mass
  )
  isotope_probs <- purrr::map_dbl(
    isotope_labels, function(label) isotope_data[[label]]$abundance
  )

  pattern = list()

  for (counts in isotope_count_combis(atom_count, length(isotope_labels))) {
    prob = multinomial_prob(counts, isotope_probs)

    # Skip masses with very low probabilities, to reduce computation.
    if (prob < min_prob) {
      next
    }

    mass <- sum(counts * isotope_masses)

    isotope_counts <- counts[counts > 0]
    names(isotope_counts) <- isotope_labels[counts > 0]

    pattern[[length(pattern) + 1]] <- list(
      mass = mass, prob = prob, isotope_counts = isotope_counts
    )
  }

  return(pattern)
}



#' Convolve two isotope patterns
#'
#' @description
#' Combines two isotope peak lists by convolving them: every peak from
#' `pattern_a` is paired with every peak from `pattern_b`. Resulting peak
#' masses are summed and probabilities are multiplied. Pairs whose combined
#' probability falls below `min_prob` are discarded.
#'
#' @param pattern_a A list of isotope peaks (each a named list with `mass`,
#'   `prob`, and `isotope_counts`).
#' @param pattern_b A list of isotope peaks in the same format as `pattern_a`.
#' @param min_prob A numeric threshold. Combined peaks with probability below
#'   this value are excluded.
#'
#' @return A list of combined isotope peaks in the same format as the inputs.
#'
#' @noRd
convolve_patterns <- function(
    pattern_a,
    pattern_b,
    min_prob = 1e-12
  ) {
  combined_pattern <- list()
  peak_index <- 1

  for (peak_a in pattern_a) {
    for (peak_b in pattern_b) {

      # When two independent isotope patterns are combined,
      # their probabilities are multiplied.
      prob <- peak_a$prob * peak_b$prob

      # Skip very small peaks.
      if (prob < min_prob) {
        next
      }

      # Start with the isotope counts from peak_a.
      isotope_counts <- peak_a$isotope_counts

      # Add the isotope counts from peak_b.
      for (isotope_label in names(peak_b$isotope_counts)) {
        count <- peak_b$isotope_counts[[isotope_label]]

        if (is.null(isotope_counts[[isotope_label]])) {
          isotope_counts[[isotope_label]] <- count
        }
        else {
          isotope_counts[[isotope_label]] <- (
            isotope_counts[[isotope_label]] + count
          )
        }
      }

      # Store the combined peak.
      combined_pattern[[peak_index]] <- list(
        mass = peak_a$mass + peak_b$mass,
        prob = prob,
        isotope_counts = isotope_counts
      )

      peak_index <- peak_index + 1
    }
  }

  return(combined_pattern)
}



#' Calculate the isotopic fine structure for a molecular formula
#'
#' @description
#' Computes the full isotopic fine structure for a molecule by successively
#' convolving the elemental fine-structure patterns for every element in the
#' formula. Corrects for electron mass based on the charge state. The resulting
#' peaks are sorted by mass and annotated with relative probabilities.
#'
#' @param formula A character string with the molecular formula (e.g.
#'   `"C40H68N2O20"`).
#' @param charge An integer giving the charge state (positive for cations,
#'   negative for anions).
#' @param min_prob A numeric probability threshold. Peaks below this value are
#'   excluded at each convolution step.
#'
#' @return A list of fine-structure peaks sorted by mass. Each peak is a named
#'   list with `mass`, `prob`, `isotope_counts`, and `relative_prob`.
#'
#' @noRd
calculate_fine_structure <- function(
    formula,
    charge,
    min_prob = 1e-12
) {

  # Determine elemental composition
  composition <- parse_molecular_formula(formula)

  # Start with one empty mass. Each element is convolved into this pattern.
  molecular_pattern = list(
    list(mass = 0.0, prob = 1.0, isotope_counts = list())
  )

  # Loop over elements
  element_symbols <- names(composition)
  for (symbol in element_symbols) {
    atom_count <- composition[[symbol]]

    element_pattern <- element_fine_structure(symbol, atom_count, min_prob)

    molecular_pattern <- convolve_patterns(
      molecular_pattern, element_pattern, min_prob
    )

    if (length(molecular_pattern) == 0) {
      return(list())
    }
  }

  # Correct for electron masses based on charge state.
  mass_correction <- as.integer(charge) * ELECTRON_MASS

  for (i in seq_along(molecular_pattern)) {
    molecular_pattern[[i]]$mass <- molecular_pattern[[i]]$mass - mass_correction
  }

  # Sort final fine-structure peaks by mass.
  masses <- vapply(
    molecular_pattern,
    function(peak) peak$mass,
    numeric(1)
  )

  molecular_pattern <- molecular_pattern[order(masses)]

  # Add probabilities relative to most probable peak.
  probs <- vapply(
    molecular_pattern,
    function(peak) peak$prob,
    numeric(1)
  )

  max_prob <- max(probs)

  for (i in seq_along(molecular_pattern)) {
    molecular_pattern[[i]]$relative_prob <- (
      molecular_pattern[[i]]$prob / max_prob
    )
  }

  return(molecular_pattern)
}



#' Collapse fine structure to a nominal isotope pattern
#'
#' @description
#' Groups fine-structure peaks by the number of extra neutrons relative to the
#' monoisotopic peak (M, M+1, M+2, ...) and collapses each group into a single
#' nominal peak. The mass of each group is the probability-weighted average of
#' all contributing fine-structure peaks.
#'
#' @param fine_structure_pattern A list of fine-structure peaks as returned by
#'   [calculate_fine_structure()] or [calculate_ion_fine_structure()].
#'
#' @return A named list of nominal isotope peaks sorted by `extra_neutrons`.
#'   Each entry is a named list with:
#'   \describe{
#'     \item{isotope_group}{Group label, e.g. `"M"` or `"M+1"`.}
#'     \item{extra_neutrons}{Integer number of extra neutrons relative to M.}
#'     \item{prob}{Summed probability of all fine-structure peaks in the group.}
#'     \item{mass}{Probability-weighted average mass of the group.}
#'     \item{prob_relative}{Probability relative to the most probable group.}
#'     \item{n_fine_structure_peaks}{Number of fine-structure peaks in the group.}
#'   }
#'
#' @noRd
collapse_to_nominal_pattern <- function(fine_structure_pattern) {

  grouped_pattern <- list()

  for (peak in fine_structure_pattern) {
    # Determine how many extra neutrons this fine-structure peak has.
    extra_neutrons <- 0

    for (isotope_label in names(peak$isotope_counts)) {
      isotope_count <- peak$isotope_counts[[isotope_label]]
      isotope_extra_neutrons <- EXTRA_NEUTRON_LOOKUP[[isotope_label]]

      extra_neutrons <- extra_neutrons +
        isotope_count * isotope_extra_neutrons
    }

    group_name <- paste0("M+", extra_neutrons)

    if (extra_neutrons == 0) {
      group_name <- "M"
    }

    # If this M+n group does not exist yet, create it.
    if (is.null(grouped_pattern[[group_name]])) {
      grouped_pattern[[group_name]] <- list(
        isotope_group = group_name,
        extra_neutrons = extra_neutrons,
        prob = 0,
        weighted_mass_sum = 0,
        n_fine_structure_peaks = 0
      )
    }

    # Sum probabilities.
    grouped_pattern[[group_name]]$prob <- (
      grouped_pattern[[group_name]]$prob + peak$prob
    )

    # Store sum(mass * probability), so we can calculate the
    # probability-weighted average mass later.
    grouped_pattern[[group_name]]$weighted_mass_sum <- (
      grouped_pattern[[group_name]]$weighted_mass_sum + peak$mass * peak$prob
    )

    grouped_pattern[[group_name]]$n_fine_structure_peaks <- (
      grouped_pattern[[group_name]]$n_fine_structure_peaks + 1
    )
  }

  # Convert weighted mass sums into weighted average masses.
  for (group_name in names(grouped_pattern)) {
    grouped_pattern[[group_name]]$mass <- (
      grouped_pattern[[group_name]]$weighted_mass_sum /
        grouped_pattern[[group_name]]$prob
    )

    grouped_pattern[[group_name]]$weighted_mass_sum <- NULL
  }

  # Sort by M, M+1, M+2, ...
  extra_neutrons <- vapply(
    grouped_pattern,
    function(group) group$extra_neutrons,
    numeric(1)
  )

  grouped_pattern <- grouped_pattern[order(extra_neutrons)]

  # Add probabilities relative to most probable peak.
  probs <- vapply(
    grouped_pattern,
    function(group) group$prob,
    numeric(1)
  )

  max_prob <- max(probs)

  for (i in seq_along(grouped_pattern)) {
    grouped_pattern[[i]]$prob_relative <- grouped_pattern[[i]]$prob / max_prob
  }

  return(grouped_pattern)
}



#' Calculate the isotopic fine structure for an ion
#'
#' @description
#' Computes the isotopic fine structure for an ionised molecule by modifying
#' the neutral molecular formula according to the charge carrier before calling
#' [calculate_fine_structure()]. For positive charge, carrier atoms are added.
#' For negative charge, carrier atoms are removed. The `formula` argument is
#' assumed to represent the neutral molecule.
#'
#' @param formula A character string with the neutral molecular formula (e.g.
#'   `"C40H68N2O20"`).
#' @param charge An integer giving the charge state. May be positive or
#'   negative.
#' @param carrier A character string with the element symbol of the charge
#'   carrier. Default is `"H"` (proton adduct).
#'
#' @return A list of fine-structure peaks as returned by
#'   [calculate_fine_structure()].
#'
#' @noRd
calculate_ion_fine_structure <- function(
    formula,
    charge,
    carrier = "H",
    min_prob = 1e-12
) {
  calculate_fine_structure(
    formula = apply_charge_carrier(
      formula = formula,
      charge = charge,
      carrier = carrier
    ),
    charge = charge,
    min_prob = min_prob
  )
}

