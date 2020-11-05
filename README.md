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
The whole process is controlled by a single script that loops over each
council area.

## How to update

1.  Make a copy of the most recent files
      - Recommend you save to a local drive so the RMarkdown knits faster
2.  Update the data file
3.  Update the code
      - Start and end years, last and next updates, etc
4.  Run the script that loops over each council area
      - At the moment this takes several hours. So it’s best to try one
        council area first.
5.  Sense check the HTML document
6.  Send a sample of HTML documents to the relevant Statistician for QA
7.  Send all HTML documents to the web team to upload to the website
      - Email 10 zipped HTML documents at a time
8.  Make a note of the next date for update
9.  **After publication** upload changes to GitHub

## Room for improvement

In rough order of importance/urgency:

  - Validate data before preparing tables (e.g. is every council area
    included? Should Scotland be included? Are values as expected e.g. age < 120?)
  - Simplify the code. For example:
      - Replace deeply nested `ifelse` with `case_when`
      - Use functions and apply
        [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself)
      - Use [existing
        utilities](https://scales.r-lib.org/reference/label_ordinal.html)
  - Reduce the time it takes to run
      - This may be because some datasets are very large
      - A solution could be to create aggregate versions of a single
        dataset
  - Improve/shorten variable names
  - Improve comments (e.g. **why** not **what**)
  - Nest/tidy datasets so the environment is more managable
  - Fix Y axis on fertility plot
      - [Plot function](https://github.com/DataScienceScotland/council_area_profiles/blob/c6dcec1e1daf40bdbc1892cc12aaf29edaf29ee1/3-Plots.R#L455-L524)
      - [Fertility plot code](https://github.com/DataScienceScotland/council_area_profiles/blob/c6dcec1e1daf40bdbc1892cc12aaf29edaf29ee1/3-Plots.R#L1384-L1392)
      - [Fertility rate data](https://github.com/DataScienceScotland/council_area_profiles/blob/c6dcec1e1daf40bdbc1892cc12aaf29edaf29ee1/2-Data_preparation.R#L1064-L1070)
      
## Licence

This repository is available under the [Open Government Licence
v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).
