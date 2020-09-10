# UF_COVID
Analysis of UF Covid Data

The UF Covid data come from 2 sources: 

Data from prior to 8 September 2020: The data were being scraped by an annonymous person daily [into a google sheet](https://docs.google.com/spreadsheets/d/18AylEt8G7JF5LZ9A5QQJ3KjvYfr5ZBfZ8g7jFZ8eZ6A/edit#gid=0). This is ongoing.

Data from September 8 2020 forward: because it was sometimes difficult to pull the data from the google sheet into R (due to notations and mixed text/numbers in some cells), I wrote a script to scrape the web page each day, orgaize it in tidy format, and then save each day's file in a folder. Another script is used to read in these daily data pulls, merge them with pre-9/8/2020 data from the g.sheet, calculate each day's number of new tests, new positives, etc., and visualize the results.



Here is a tutorial for [how to make a Hopkins-style Covid-dashboard](https://www.esri.com/arcgis-blog/products/ops-dashboard/health/arcgis-dashboards-training-videos-for-covid-19/), shouldf anyone have some spare time on their hands.
