# v1.5.0
## New features
* Visualization of normalized data with heatmaps.

## Adjustments
* Disable the “Generate report” button until normalization is performed.

## Bug fixes
* Minor bug fixes.


# v1.4.3
## Adjustments
* Detect and automatically rename isomers in Skyline data.


# v1.4.2
## Adjustments
* April 2024: Make GlycoDash compatible with R 4.3.3, and most recent package versions.
* Include sialylation per galactose as a trait for human IgG.
* Automatic derived traits are now calculated in percentages instead of fractions.


# v1.4.1
## Bug fixes
* Automatically detect comma or semicolon separation in Skyline CSV files. 
* Show Skyline quality criteria in the generated report.


# v1.4.0
## New features
* Option to upload Skyline data.
* Option to exclude quality criteria in analyte curation.

## Adjustments
* More info boxes in the user interface.


# v1.3.7
## Adjustments
* The GlycoDash version is now shown in the data processing report.
* Unnecessary buttons were removed from the “Data Import” tab.


# v1.3.6
## Bug fixes
* Minor bug fixes in analyte curation and quantitation.


# v1.3.6
## Adjustments
* Peptides/clusters in the data are now detected automatically.


# v1.3.5
## Adjustments
* Multiple LaCyTools summary files can now be uploaded at once, rather than one by one.
* In analyte curation, it is now possible to choose separate cut-offs for different clusters.


# v1.3.4
## Bug fixes
* The redesigned option to automatically calculate traits now works when running GlycoDash in a Docker container.


# v1.3.3
## New features
* The option to automatically calculate glycosylation traits has been redesigned. Calculations are now performed based on a 
  reference list containing known glycan compositions. Mouse IgG traits were added, as well as extra human IgG traits.

## Adjustments
* Changed required formatting of custom traits Excel files to be consistent with the automatic trait calculations.
* Normalization now happens automatically after analyte curation. 
* Added the option to export data in the “Normalized data” tab.

## Bug fixes
* GlycoDash can now handle plate designs containing plate numbers with two digits.


# v1.3.2
## Adjustments
* Exclude TTP peptide from the IgG1 quantitation.

## Bug fixes
* Minor bug fixes in spectra curation UI


# v1.3.1
## Adjustments
* Add a line of equality to the peptide correlation plots in the “IgG1 quantitation” tab.
* Round cut-off numbers in curated spectra scatter plot and table.
* Display overview of passing spectra in the “Spectra curation” tab, and make it possible to download tables of failed spectra.
* Show a spinner while processing LaCyTools summary files.

## Bug fixes
* Analyte curation can be performed multiple times.
* Prevent dashboard from crashing when de-selecting all QC criteria in spectra curation.
* Prevent dashboard from crashing during spectra curation when all negative control spectra are uncalibrated. 
  Show a warning message and disable button when all negative controls are uncalibrated for one or more clusters.
* Report generation works when repeatability tabs were deleted.


# v1.3.0
## New features
* IgG1 quantitation with SILuMAb

## Adjustments
* The “Export results” tab now displays the final data that can be downloaded.


# v1.2.9
## Adjustments
* Performing analyte curation is (for now) possible only once, because curating analytes multiple times does not always work properly.
* Info boxes were added to the analyte curation tab.
* Option to exclude sample types when curating analytes per biological group.


# v1.2.8
## Bug fixes
* NA values are now treated as zeros when calculating custom glycosylation traits.

## Adjustments
* In the Data Import tab, the “Define the clusters” button is disabled when not all keywords were found or filled in.
* Show correct analyte curation method in data report.
* Show a warning when formulas for custom traits contain glycans that are not present in the data after analyte curation.


# v1.2.7
## Bug fixes
* The file names of the uploaded LaCyTools summaries and metadata are now shown in the report.


# v1.2.6
## Adjustments
* In the spectra curation cut-off plots, the points are no longer jittered.
* Implemented a check against duplicate sample IDs in the sample type list and metadata Excel file.
* Implemented a check against missing sample IDs in the sample type list.
* The “Perform analyte curation” button is now disabled when spectra curation was not yet performed.

## Bug fixes
* Clicking each cluster tab in the analyte curation results is no longer required.


# v1.2.5
## Performance improvements
* Adding clusters and adding metadata in the “Data import” tab is now much faster.
* Analyte curation is performed much faster.


# v1.2.4
## Bug fixes
* The “Changelog” download button now works in Docker and on the cpmtools server (previously it only worked in RStudio).

## Adjustments
* Added an example metadata file, and an info box about metadata.


# v1.2.3
## Adjustments
* Added a link to the GlycoDash GitHub page in the top right corner of the dashboard.
* Added a download button for the changelog in the top right corner.
* Changed the title of the dashboard to “GlycoDash v<x.y.z>”


# v1.2.2
## Bug fixes
* Curation method (all data, per biological group or per sample) no longer shown when supplying an analyte list.



# v1.2.1
## Bug fixes
* Plots with spectra curation results are now shown in the processing report when less than 5 clusters were used.

## Adjustments
* Consistent coloring in the spectra curation results plots.



# v1.2.0
## New features
* Ability to upload multiple LaCyTools summary files.



# v1.1.3
## Adjustments
* Change color palette of the spectra curation scatter plot.

* Show reason for failing spectra curation in the hover box.



# v1.1.2
## Bug fixes
* “group is not a factor” when using separate plate designs for total and specific antibodies.



# v1.1.1
## Bug fixes
* Clusters no longer combined in derived traits when the cluster names overlap.



# v1.1.0
## New features
* Analyte curation per sample.



# v1.0.0
The first official release of the GlycoDash master branch on GitHub.


