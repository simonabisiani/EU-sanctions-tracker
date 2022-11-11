## EU Sanctions Tracker

### Description

Merging of Consolidated List of Travel Bans and Consolidated List of Financial Sanctions. 
Visualizations for sanctions tracker for DG FISMA and data.europa.eu. 


### Instructions

**Main script**: 
- [Sanctions_tracker_travel_bans_financial_sanctions_merged.R](https://github.com/simonabisiani/EU-sanctions-tracker/blob/main/Sanctions_tracker_travel_bans_financial_sanctions_merged.R) contains my messy join of the two datasets (and the many approaches I tried to achieve this) and the code to create the charts that appear on the homepage (excluding the newly inserted ones at the top, which are in stored [here](https://github.com/simonabisiani/EU-sanctions-tracker/blob/main/other_scripts/top_charts_last_addition.R), and including some charts that we have since gotten rid of, which means there is some redundant code in there). 

What to bear in mind when joining:
- we want to keep columns from both datasets
- we ideally want one row for each designation
- the financial dataset has several rows for the same individual, and relevant info appears in different rows, so there is NO BEST ROW to pick if wanting to retain only one in the end (e.g., the first one). If you are able to, please try to minimise the loss of info if selecting one row over the others, or try pasting information from other rows into the one final row for a designation.
- remove any designations under Programme: UNLI

### Sources
- **Financial Sanctions**: head over to data.europa.eu [here](https://data.europa.eu/data/datasets/consolidated-list-of-persons-groups-and-entities-subject-to-eu-financial-sanctions?locale=en) and extract *the File 1.1*. Please note I used File 1.0 in my script before I was told that File 1.1 is a better choice. This means the names of the variables in my code differ from the names of the variables you will use, but the content should be the same, and I can give you more info on which columns to use if you need any clarification.

-**Travel Bans**: head over to data.europa.eu [here](https://data.europa.eu/data/datasets/consolidated-list-of-persons-subject-under-eu-sanctions-to-travel-restrictions?locale=en) and extract the XML file. I asked Massi to convert this for me to csv and that's what I worked with.

### Useful resources
- [Variables code sheet](https://github.com/simonabisiani/EU-sanctions-tracker/blob/main/FSF-CSV-attributes%5B1384%5D.pdf)
- [Seed list of program names](https://github.com/simonabisiani/EU-sanctions-tracker/blob/main/Abbreviation%20FSD%20extract%20program%20(1).xlsx)
