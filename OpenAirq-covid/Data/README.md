This data repository contains data files of real-time PM2.5, AQI, and COVID data from December 2020. 



| No. | Dataset | Description | Status  |
|---|---|---|---|
| 1 | [PM2.5](PM25_Weekly/pm25.csv) |  This dataset is automatically generated using [the script](https://github.com/GeoDaCenter/OpenAirq-covid/tree/main/OpenAirq-covid) to process raw data from the [US EPA](https://www.epa.gov/outdoor-air-quality-data/download-daily-data) website. As EPA delays the update of API in scraping the latest 6-month data, direct download to update datasets in PM25_Raw is used. Each column is the 7-day average PM2.5 value calcuated by taking the mean of the past seven days, while each row is a point data for a specific EPA sensor.|  Updated to 2021-02-12 |
| 2 | [AQI](PM25_Weekly/aqi.csv) |  This dataset is automatically generated using [the script](https://github.com/GeoDaCenter/OpenAirq-covid/tree/main/OpenAirq-covid) to process raw data from the [US EPA](https://www.epa.gov/outdoor-air-quality-data/download-daily-data) website. As EPA delays the update of API in scraping the latest 6-month data, direct download to update datasets in PM25_Raw is used. Each column is the 7-day average AQI value calcuated by taking the mean of the past seven days, while each row is a point data for a specific EPA sensor. | Updated to 2021-02-12 |
| 3 | [COVID Cases](CovidWeekly.csv) | This dataset is automatically generated using [the script](https://github.com/GeoDaCenter/OpenAirq-covid/tree/main/OpenAirq-covid) to process raw data from the [Chicago Data Portal](https://data.cityofchicago.org/browse?tags=gis). Each column is the 7-day average weekly confirmed COVID cases per 100,000 residents, calcuated by taking the mean of the past seven days, while each row is a administrative region represented by zipcode.|  Updated to 2021-02-07 |
| 4 | [Asthma ED Visits](Asthma2017.csv) | This dataset is generated using raw data from the [Chicago Health Atlas](https://www.chicagohealthatlas.org/indicators). Each column contains the 2017 rate of ED visits per 10,000 people, separated by age group, while each row is a administrative region represented by zipcode.| Final |
| 5 | [21 Counties](LargeAreaCounties/LargeAreaCounties.shp)| This shapefile is used to map the 21 counties. |  Final  |
| 6 | [City of Chicago by Zipcode](ZipcodeBoundary/geo_export_04ad4464-ddf0-4603-a903-1c86f00e6bad.shp) | This shapefile is used along with COVID cases data to map the COVID cases in Chicago.  | Final  |
| 7 | [Description](Description.csv) | This description file includes descriptions of pm2.5 and AQI to be shown in the dashboard. | Final  |


