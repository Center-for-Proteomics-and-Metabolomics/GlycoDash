# Architecture

GlycoDash is an R Shiny dashboard built with the [golem](https://thinkr-open.github.io/golem/) framework for processing glycomics data from LaCyTools, SweetSuite, and SkyLine. This document describes the layout and conventions of the `R/` folder.

---

## R Folder Overview

### Main App

| File | Description |
|------|-------------|
| `app_server.R` | Server-side logic of the dashboard |
| `app_ui.R` | User interface definition of the dashboard |

### Default Golem Files

| File | Description |
|------|-------------|
| `app_config.R` | Default golem configuration — do not modify |
| `run_app.R` | Entry point used to launch the app — do not modify |
| `golem_utils_server.R` | Default golem server utility functions (not actively used) |
| `golem_utils_ui.R` | Default golem UI utility functions (not actively used) |

### Other

| File | Description |
|------|-------------|
| `data.R` | Documentation for the example datasets included in the package |
| `utils_general.R` | Utility functions used throughout the app |
| `utils-pipe.R` | Enables use of the `%>%` pipe operator within the package |

---

## Module Conventions

The app is organized into Shiny modules. Three file-naming prefixes are used:

- **`mod_`** — Contains the module UI and server functions.
- **`mod_tab_`** — A module that defines the content of a `tabPanel`.
- **`fct_`** — Contains the functions used by the corresponding module. All non-trivial functions in `fct_` files are documented with [roxygen2](https://roxygen2.r-lib.org/). To view the documentation as a help page, run `golem::document_and_reload()`, then use `?function_name` as you would with standard R functions.

---

## Module Structure

The modules follow the data processing pipeline of the application:

```
mod_data_import.R
├── mod_read_data.R               / fct_read_data.R
├── mod_add_sample_ids.R          / fct_add_sample_ids.R
├── mod_process_plate_design.R
├── mod_process_sample_list.R
├── mod_add_sample_types.R        / fct_add_sample_types.R
│   └── mod_process_sample_type_file.R
├── mod_clusters.R
└── mod_add_metadata.R            / fct_add_metadata.R

mod_spectra_curation.R            / fct_spectra_curation.R
├── mod_curate_based_on_controls.R
├── mod_curate_based_on_percentiles.R
├── mod_tab_cut_offs.R
└── mod_tab_curated_spectra_plot.R

mod_analyte_curation.R            / fct_analyte_curation.R
└── mod_tab_curated_analytes.R

mod_normalization.R               / fct_normalization.R

mod_quantitation.R                / fct_quantitation.R
├── mod_tab_quantitation.R
└── mod_tab_quantitation_peptides.R

mod_derived_traits.R              / fct_derived_traits.R
└── mod_tab_intensities.R

mod_site_occupancy.R              / fct_site_occupancy.R

mod_repeatability.R               / fct_repeatability.R
├── mod_tab_repeatability.R
└── mod_tab_repeatability_plot.R

mod_data_exploration.R            / fct_data_exploration.R
└── mod_tab_data_exploration.R

mod_export.R
```

---
