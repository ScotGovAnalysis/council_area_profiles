Council Area Profiles
================

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

Rmarkdown to generate demographic profiles for each Scottish Council
area.

![Screenshot of the council area profile for Aberdeen
City](https://github.com/DataScienceScotland/council_area_profiles/blob/master/screenshot.png)

## How it works

For each council area we create: data, plots, tables, and text. These
elements are then knitted into an HTML document by an RMarkdown file.
The whole process is controlled by a single script that produces reports in parallel for each
council area.

## How to update

1. Save a copy of the most recent files to a local drive (RMarkdown will knit faster in a local drive)
2. Update the data file (including data and last/next updates)
3. Check you’re using the latest dataset and only the updated datasets have changed
4. Check all the 'next update' dates are still in the future
5. Update links to source tables
6. Run the MAIN script that produces reports for each council area (this normally takes around 20 mins)
7. Sense check the HTML document
8. Send a sample of HTML documents to the relevant Statistician for checking
9. Send all HTML documents to the web team to upload to the website
10. Set a reminder for the next update
11. **After publication** upload changes to GitHub
12. Delete old local copies of the code

## Roles and responsibilities
Branches:
- Provide updated data up to 1 week after any relevant publications
- Let central team know when data has been updated
- Check all content in HTML files is correct (including: data, links, text, next update estimates)
- Correct any issues flagged by the QA scripts. This includes minor issues like "and" vs "&" to retain data provenance.

Statistical Promotion and Analysis:
- Run code and share with branches for QA within 1 week of branches updating any datasets
- Make any changes required by branches and provide web team with HTML files

Statistics Dissemination:
- Upload HTML files within 1 week of them being available

## Who owns each tab
Household Estimates and Projections:
- Household estimates
- Household projections
- Dwellings
- Dwellings by type
- Dwellings by council tax band

Vital Events:
- Births by sex
- Standardised birth rates
- Births by age of mother
- Fertility rates
- Deaths by sex
- Standardised death rates
- Deaths by sex by age
- Leading causes of death
- Life expectancy
- Marriages
- Civil partnerships

Population and Migration Statistics:
- Population estimates
- Population projections
- Nature of population change
- Migration
- Net Migration
- Met migration rates

## Room for improvement

In rough order of importance/urgency:

  - Validate data before preparing tables (e.g. is every council area included? Should Scotland be included? Are all combinations of all variables included? Are values sensible e.g. age < 120?)
  - Simplify the code. For example:
      - Replace deeply nested `ifelse` with `case_when` or `switch` statements
      - Use functions and apply
        [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself)
      - Use [existing
        utilities](https://scales.r-lib.org/reference/label_ordinal.html)
  - Automate horizontal axis labels so the final label is for the final datapoint (same for the first)
  - Improve/shorten variable names
  - Improve comments (e.g. **why** not **what**)
  - Fix Y axis on fertility plot
      - [Plot function](https://github.com/DataScienceScotland/council_area_profiles/blob/c6dcec1e1daf40bdbc1892cc12aaf29edaf29ee1/3-Plots.R#L455-L524)
      - [Fertility plot code](https://github.com/DataScienceScotland/council_area_profiles/blob/c6dcec1e1daf40bdbc1892cc12aaf29edaf29ee1/3-Plots.R#L1384-L1392)
      - [Fertility rate data](https://github.com/DataScienceScotland/council_area_profiles/blob/c6dcec1e1daf40bdbc1892cc12aaf29edaf29ee1/2-Data_preparation.R#L1064-L1070)
      
## Licence

This repository is available under the [Open Government Licence
v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).
