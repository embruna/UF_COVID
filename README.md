# UF_COVID

## Analysis of UF Covid Data

To generate a pdf with the graphs through today's date, open and knit ```UF_Covid_Stats.Rmd```.  This will:
1. Scrape today's data from the UF Data dashboard, clean it, and save it as a .csv
2. Merge today's csv file with all previous days' csv files
3. Combine the scraped data with some data previously gathered by others (see below), organize it, and save it as a csv.
4. Calclulate daily values and generate the figures.

## The UF Covid data come from 2 sources: 

1. Data from prior to 8 September 2020: The data were being scraped by an annonymous person daily [into a google sheet](https://docs.google.com/spreadsheets/d/18AylEt8G7JF5LZ9A5QQJ3KjvYfr5ZBfZ8g7jFZ8eZ6A/edit#gid=0). This is ongoing.

2. Data from September 8 2020 forward: because it was sometimes difficult to pull the data from the google sheet into R (due to notations and mixed text/numbers in some cells), I wrote a script to scrape the web page each day, orgaize it in tidy format, and then save each day's file in a folder. Another script is used to read in these daily data pulls, merge them with pre-9/8/2020 data from the g.sheet, calculate each day's number of new tests, new positives, etc., and visualize the results.

## Important Caveats

1. Calculations of tests per day, positives per day, and positivity rate are based on the dates the data were *posted*. It is possible that data released on one day includes tests that were conducted over multiple days.

2. UF posts daily data on UF affiliated cases at http://www.ufl.edu/articles/2020/09/covid-data.html.

## DISCLAIMER

The information presented is intended for educational and informational purposes only. This website and the visualizations presented was done as an exercise in programming and data management and should **not** be interpreted as official UF health statistics. Do not rely on the information presented here to make health or educational decisions. I make no representation or warranty of any kind, express or implied, regarding the accuracy, adequacy, validity, reliability, availability or completeness of any information on this site. This site is not affliated with UF, UF Health, or my employer. *Caveat emptor*.





Here is a tutorial for [how to make a Hopkins-style Covid-dashboard](https://www.esri.com/arcgis-blog/products/ops-dashboard/health/arcgis-dashboards-training-videos-for-covid-19/), shouldf anyone have some spare time on their hands.
