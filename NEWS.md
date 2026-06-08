# v1.11.5
## Fixed
* Fixed a bug where non-glycosylated peptides for calculating site occupancies were not detected.


# v1.11.4
## Added
* Glycosylation traits: support automatically calculating sulfation of complex-type glycans for IgA N205 and IgA1/2 N340/N327.

## Changed
* Updated the user guide.
* Improved performance when importing large datasets.
* When skipping spectra curation, figures showing the percentage of uncalibrated spectra per sample type are now generated.
* O-glycan trait: `disialylated O-antigens` is now calculated as an average number per glycopeptide, instead of a percentage.

## Fixed
* Fix broken example sample list `xlsx` file.


# v1.11.3
## Changed
* Update instructions in `README.md` for running GlycoDash using Docker.

## Fixed
* Allow the user to de-select all analytes in analyte curation. Previously,
this caused all initial analytes to pass curation.
* Prevent GlycoDash from crashing when zero analytes pass curation.


# v1.11.2
## Changed
* In the HTML report, explicitly list analytes that were manually included
or excluded during analyte curation.


# v1.11.1
## Changed
* Data import: allow for non-sequential plate numbering.
* Protein quantitation: disable sum intensity correlation plots (introduced in v1.11.0) 
for now.

## Fixed
* Fix inverted logic in normalized data heatmaps: hide option for excluding
sample types when showing the heatmaps per biological group.
* Fix a bug where numeric columns in SweetSuite data were sometimes read
in as logical.
* Fix a bug where non-glycosylated peptides without corresponding glycosylation
data were listed in the "Site occupancy" tab.


# v1.11.0
## Added
* Option to curate analytes based on the average values of QC parameters.
* Protein quantitation: plot the intensities of labeled (glyco)peptides against
natural (glyco)peptides for quality control.

## Fixed
* Fixed a bug in Skyline data input, where the table containing detected
glycosylation sites was not rendered properly.


# v1.10.1
## Fixed
* Fixed the splitting by total and specific samples for SweetSuite data.


# v1.10.0
## Added
* Option to upload data from SweetSuite (https://github.com/stainawarijar/SweetSuite).
* Option to enter notes in the "Export results" section, which will be
added to the end of the HTML report.

## Fixed
* Fixed a bug that caused a crash when uploading a custom traits file.
* Prevent crash when accidentally excluding all samples in analyte curation.


# v1.9.2
## Fixed
* Bug fix in metadata import.


# v1.9.1
## Fixed
* Bug fix in LaCyTools data import.


# v1.9.0
## Added
* Allow for more flexibility in the formatting of Skyline data files.

## Changed
* Improved performance.
* Glycosylation sites are now ordered alphabetically throughout the dashboard
and in the output data.
* Make the download button for the user manual more obvious.
* When spectra curation is skipped, the scatter plots with sum intensities
and percentage of passing analytes will be shown in the HTML report.

## Fixed
* Minor bug fixes.


# v1.8.2
## Fixed
* Fix a bug in the data import.


# v1.8.1
## Fixed
* Fix a column name in the example Excel file for protein quantitation.


# v1.8.0
## Added
* Option to quantify proteins based on stable isotope labeled (glyco)peptides.


# v1.7.2
## Changed
* Include Joining Chain glycan H6N5S1 in automatic traits calculations.

## Fixed
* Fix a bug in the automatic calculation of IgA glycosylation traits.
* Make site occupancy calculations work when spectra curation is skipped.
* Fix site occupancy in HTML report.


# v1.7.1
## Fixed
* Fix a bug where HTML report would not generate.


# v1.7.0
## Added
* Option to calculate glycosylation site occupancies.

## Fixed
* Various minor bug fixes.


# v1.6.6
## Changed
* Data import: remove leading or trailing spaces from entries in character columns.

## Fixed
* List excluded sample types in the HTML report when curating analytes per 
biological group.
* Minor bug fixes in UI.


# v1.6.5
## Changed
* Sort glycan compositions in the heatmaps of the normalized data.
* Sort glycan compositions in the repeatability figures.

## Fixed
* Bug fix in spectra curation results figures.


# v1.6.4
## Changed
* When a trait is calculated using only one glycan, that trait is reported
with the glycan added at the end of the trait name.

## Fixed
* Bug fix in calculating traits for hybrids and oligomannose glycans.


# v1.6.3
## Fixed
* Prevent crash after curating analytes per biological group.


# v1.6.2
## Changed
* Data import: option to specify only glycopeptide or GPS clusters for IgG1 quantitation.
* In spectra curation, samples that are on the cut-off now also pass curation.
* Sort analytes by glycan composition in the analyte curation plots and tables.

## Fixed
* Various bug fixes.


# v1.6.1
## Changed
* Implement messages telling the user to re-perform processing steps in case
changes were made to the data by adjusting settings in earlier steps.
* Spectra curation: make the coloring of sample types consistent between
scatter plots for different glycosylation sites.
* Analyte curation: option to select all traits at once for each antibody glycans type.
* Make clearer that IgG1 quantitation is meant only for antigen-specific IgG1.
When data contains both total and specific immunoglobulin samples, perform the 
quantitation only for the specific samples.
* Show a loading spinner while generating HTML report.

## Fixed
* Prevent GlycoDash from crashing when Total and Specific samples are specified
after adding the sample types.
* Minor bug fixes in automatic traits calculations.


# v1.6.0
## Added
* Option to automatically calculate glycosylation traits for IgA and IgM,
including joining chain (JC).
* Show notifications when an automatic trait is zero or 100 for all samples,
or when a trait would be calculated using only one glycan.
* Plot automatically calculated traits against total spectrum intensities.

## Changed
* Added a download button for the GlycoDash manual in the top-right corner of 
the dashboard.
* Added a button that links to known issues (GitHub).
* Notes on analytes in Skyline data are now kept and displayed in a separate
tab when downloading normalized data as an Excel file.
* Plates in a plate design are now automatically numbered, irrespective of how
they are named in the Excel file.
* Add filter options to all tables, and round numbers when displaying data
(rounding is NOT applied to downloaded data).

## Fixed
* Leading zeros in plate numbers sometimes caused issues when trying to merge
data with a plate design. This has now been fixed.
* When curating total and specific spectra based on negative controls that were
missing for one or more clusters, the “Perform analyte curation” button remained
grey even after choosing manual cut-offs. This has now been fixed.
* Prevent users from excluding all sample types or biological groups during analyte curation,
which caused the dashboard to crash.


# v1.5.4
## Changed
* Created a more appealing user interface.
* When visualizing repeatability per plate, calculate intra-plate variations and 
inter-plate variation using median values instead of mean.


# v1.5.3
## Changed
* Make plots resizable.

## Fixed
* Repeatability: fix incorrect error message about no data being available.


# v1.5.2
## Changed
* Change the calculation of “sialylation per galactose”.
It is now calculated as “Sialylation per antenna” divided by “Galactosylation per antenna”,
multiplied by 100%.


# v1.5.1
## Changed
* Remove the human IgG trait “terminal galactosylation”, as it is redundant.


# v1.5.0
## Added
* Visualization of normalized data with heatmaps.
* Option to normalize charge states of analytes separately.

## Changed
* Show a simple warning message when required variables are missing from LaCyTools data.
* Add terminal galactosylation as an automatically calculated trait for human IgG.
* Remove the “Calculate glycosylation traits” button.
* Disable the “Generate report” button until normalization is performed.
* Show chosen percentiles from spectra curation in the report.

## Fixed
* Minor bug fixes.


# v1.4.3
## Changed
* Detect and automatically rename isomers in Skyline data.


# v1.4.2
## Changed
* April 2024: Make GlycoDash compatible with R 4.3.3, and most recent package versions.
* Include sialylation per galactose as a trait for human IgG.
* Automatic derived traits are now calculated in percentages instead of fractions.


# v1.4.1
## Fixed
* Automatically detect comma or semicolon separation in Skyline CSV files. 
* Show Skyline quality criteria in the generated report.


# v1.4.0
## Added
* Option to upload Skyline data.
* Option to exclude quality criteria in analyte curation.

## Changed
* More info boxes in the user interface.


# v1.3.7
## Changed
* The GlycoDash version is now shown in the data processing report.
* Unnecessary buttons were removed from the “Data Import” tab.


# v1.3.6
## Fixed
* Minor bug fixes in analyte curation and quantitation.


# v1.3.6
## Changed
* Peptides/clusters in the data are now detected automatically.


# v1.3.5
## Changed
* Multiple LaCyTools summary files can now be uploaded at once, rather than one by one.
* In analyte curation, it is now possible to choose separate cut-offs for different clusters.


# v1.3.4
## Fixed
* The redesigned option to automatically calculate traits now works when running GlycoDash in a Docker container.


# v1.3.3
## Added
* The option to automatically calculate glycosylation traits has been redesigned. Calculations are now performed based on a 
  reference list containing known glycan compositions. Mouse IgG traits were added, as well as extra human IgG traits.

## Changed
* Changed required formatting of custom traits Excel files to be consistent with the automatic trait calculations.
* Normalization now happens automatically after analyte curation. 
* Added the option to export data in the “Normalized data” tab.

## Fixed
* GlycoDash can now handle plate designs containing plate numbers with two digits.


# v1.3.2
## Changed
* Exclude TTP peptide from the IgG1 quantitation.

## Fixed
* Minor bug fixes in spectra curation UI


# v1.3.1
## Changed
* Add a line of equality to the peptide correlation plots in the “IgG1 quantitation” tab.
* Round cut-off numbers in curated spectra scatter plot and table.
* Display overview of passing spectra in the “Spectra curation” tab, and make it possible to download tables of failed spectra.
* Show a spinner while processing LaCyTools summary files.

## Fixed
* Analyte curation can be performed multiple times.
* Prevent dashboard from crashing when de-selecting all QC criteria in spectra curation.
* Prevent dashboard from crashing during spectra curation when all negative control spectra are uncalibrated. 
  Show a warning message and disable button when all negative controls are uncalibrated for one or more clusters.
* Report generation works when repeatability tabs were deleted.


# v1.3.0
## Added
* IgG1 quantitation with SILuMAb

## Changed
* The “Export results” tab now displays the final data that can be downloaded.


# v1.2.9
## Changed
* Performing analyte curation is (for now) possible only once, because curating analytes multiple times does not always work properly.
* Info boxes were added to the analyte curation tab.
* Option to exclude sample types when curating analytes per biological group.


# v1.2.8
## Fixed
* NA values are now treated as zeros when calculating custom glycosylation traits.

## Changed
* In the Data Import tab, the “Define the clusters” button is disabled when not all keywords were found or filled in.
* Show correct analyte curation method in data report.
* Show a warning when formulas for custom traits contain glycans that are not present in the data after analyte curation.


# v1.2.7
## Fixed
* The file names of the uploaded LaCyTools summaries and metadata are now shown in the report.


# v1.2.6
## Changed
* In the spectra curation cut-off plots, the points are no longer jittered.
* Implemented a check against duplicate sample IDs in the sample type list and metadata Excel file.
* Implemented a check against missing sample IDs in the sample type list.
* The “Perform analyte curation” button is now disabled when spectra curation was not yet performed.

## Fixed
* Clicking each cluster tab in the analyte curation results is no longer required.


# v1.2.5
## Performance improvements
* Adding clusters and adding metadata in the “Data import” tab is now much faster.
* Analyte curation is performed much faster.


# v1.2.4
## Fixed
* The “Changelog” download button now works in Docker and on the cpmtools server (previously it only worked in RStudio).

## Changed
* Added an example metadata file, and an info box about metadata.


# v1.2.3
## Changed
* Added a link to the GlycoDash GitHub page in the top right corner of the dashboard.
* Added a download button for the changelog in the top right corner.
* Changed the title of the dashboard to “GlycoDash v<x.y.z>”


# v1.2.2
## Fixed
* Curation method (all data, per biological group or per sample) no longer shown when supplying an analyte list.



# v1.2.1
## Fixed
* Plots with spectra curation results are now shown in the processing report when less than 5 clusters were used.

## Changed
* Consistent coloring in the spectra curation results plots.



# v1.2.0
## Added
* Ability to upload multiple LaCyTools summary files.



# v1.1.3
## Changed
* Change color palette of the spectra curation scatter plot.

* Show reason for failing spectra curation in the hover box.



# v1.1.2
## Fixed
* “group is not a factor” when using separate plate designs for total and specific antibodies.



# v1.1.1
## Fixed
* Clusters no longer combined in derived traits when the cluster names overlap.



# v1.1.0
## Added
* Analyte curation per sample.



# v1.0.0
The first official release of the GlycoDash master branch on GitHub.


