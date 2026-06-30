# This file contains constant values, such as masses of isotopes.


# Nested list with isotopes data.
# Masses apply to neutral atoms.
# Taken from: https://physics.nist.gov/cgi-bin/Compositions/stand_alone.pl
ISOTOPES <- list(
  carbon = list(
    C12 = list(mass = 12.000000000, abundance = 0.9893),
    C13 = list(mass = 13.003354835, abundance = 0.0107)
  ),
  hydrogen = list(
    H1 = list(mass = 1.007825032, abundance = 0.999885),
    H2 = list(mass = 2.014101778, abundance = 0.000115)
  ),
  oxygen = list(
    O16 = list(mass = 15.994914620, abundance = 0.99757),
    O17 = list(mass = 16.999131757, abundance = 0.00038),
    O18 = list(mass = 17.999159613, abundance = 0.00205)
  ),
  nitrogen = list(
    N14 = list(mass = 14.003074004, abundance = 0.99636),
    N15 = list(mass = 15.000108899, abundance = 0.00364)
  ),
  sulfur = list(
    S32 = list(mass = 31.972071174, abundance = 0.9499),
    S33 = list(mass = 32.971458910, abundance = 0.0075),
    S34 = list(mass = 33.967867004, abundance = 0.0425),
    S36 = list(mass = 35.96708071, abundance = 0.0001)
  ),
  sodium = list(
    Na23 = list(mass = 22.989769282, abundance = 1)
  ),
  potassium = list(
    K39 = list(mass = 38.963706486, abundance = 0.93258),
    K40 = list(mass = 39.96399817, abundance = 0.00012),
    K41 = list(mass = 40.961825258, abundance = 0.06730)
  ),
  iron = list(
    Fe54 = list(mass = 53.9396090, abundance = 0.05845),
    Fe56 = list(mass = 55.9349363, abundance = 0.91754),
    Fe57 = list(mass = 56.9353928, abundance = 0.02119),
    Fe58 = list(mass = 57.9332744, abundance = 0.00282)
  )
)


# Masses of particles (Da)
PROTON_MASS <- 1.00727646658  # https://physics.nist.gov/cgi-bin/cuu/Value?mpu
ELECTRON_MASS <- 5.4857990904e-4  # https://physics.nist.gov/cgi-bin/cuu/Value?meu



# Create a list with the number of extra neutrons for each isotope compared
# to the lightest isotope.
build_extra_neutron_lookup <- function() {
  extra_neutrons <- list()

  for (element_name in names(ISOTOPES)) {
    isotope_labels <- names(ISOTOPES[[element_name]])

    # Extract mass numbers from isotope labels.
    # Example: "C12" -> 12, "O18" -> 18
    mass_numbers <- as.integer(
      sub("^[A-Z][a-z]*", "", isotope_labels)
    )

    lightest_mass_number <- min(mass_numbers)

    for (i in seq_along(isotope_labels)) {
      isotope_label <- isotope_labels[[i]]
      extra_neutrons[[isotope_label]] <-
        mass_numbers[[i]] - lightest_mass_number
    }
  }

  return(extra_neutrons)
}

EXTRA_NEUTRON_LOOKUP <- build_extra_neutron_lookup()

