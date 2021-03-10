library(tidyverse)
library(httr)
library(stringr)
library(jsonlite)
library(lubridate)
library(knitr)
library(sf)
library(lubridate)
library(raster)
library(scales)
library(broom)
library(viridis)
library(RSocrata)

## Load shapefiles for 21 counties 
counties <- st_read("Data/LargeAreaCounties/LargeAreaCounties.shp")


## Import data from the folder 
import_pm <- function(state_file){
  read_csv(state_file)}

air <- dir("Data/PM25_Raw", pattern = "\\.csv$", full.names = TRUE)

full.data <- map_df(air, import_pm)

## Date Wrangling to Calculate Daily Average for PM2.5
county.pm25<- full.data%>%
  filter(COUNTY %in% counties$COUNTYNAME)%>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
  dplyr::select(Date, `Site ID`,`Daily Mean PM2.5 Concentration`, COUNTY,SITE_LATITUDE, SITE_LONGITUDE, CBSA_NAME)%>%
  group_by(Date, `Site ID`, COUNTY, CBSA_NAME)%>%
  summarise(pm2.5 = mean(`Daily Mean PM2.5 Concentration`, na.rm =TRUE),
            latitude = mean(SITE_LATITUDE), 
            longitude = mean(SITE_LONGITUDE) )%>%
  filter(Date >= '2020-12-01')%>%
  ungroup(Date)%>%
  arrange(desc(Date))%>%
  pivot_wider(names_from = 'Date', values_from = 'pm2.5')

sensor<- county.pm25[1:5]
pm25_raw<- county.pm25[6:ncol(county.pm25)]
colnames_pm25<- colnames(pm25_raw[1:(ncol(pm25_raw)-6)])
pm25 <- sapply(1:(ncol(pm25_raw)-6), function(z) {apply(pm25_raw[,z:(z+6)],1,mean, na.rm=T)})
pm25<-as.data.frame(pm25)
colnames(pm25)<- colnames_pm25 
colnames(pm25)<- gsub("-", "", colnames(pm25))
colnames(pm25)<- paste0('PM25_', colnames(pm25))
pm25<- cbind(sensor, pm25)

##Write the csv file of pm2.5
write.csv(pm25, "Data/PM25_Weekly/pm25.csv")

## Date Wrangling to Calculate Daily Average for AQI
county.aqi<- full.data%>%
  filter(COUNTY %in% counties$COUNTYNAME)%>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
  dplyr::select(Date, `Site ID`,DAILY_AQI_VALUE, COUNTY,SITE_LATITUDE, SITE_LONGITUDE, CBSA_NAME)%>%
  group_by(Date, `Site ID`, COUNTY, CBSA_NAME)%>%
  summarise(aqi = mean(DAILY_AQI_VALUE, na.rm =TRUE),
            latitude = mean(SITE_LATITUDE), 
            longitude = mean(SITE_LONGITUDE) )%>%
  filter(Date >= '2020-12-01')%>%
  ungroup(Date)%>%
  arrange(desc(Date))%>%
  pivot_wider(names_from = 'Date', values_from = 'aqi')

aqi_raw<- county.aqi[6:ncol(county.aqi)]
colnames_aqi<- colnames(aqi_raw[1:(ncol(aqi_raw)-6)])
aqi <- sapply(1:(ncol(aqi_raw)-6), function(z) {apply(aqi_raw[,z:(z+6)],1,mean, na.rm=T)})
aqi<-as.data.frame(aqi)
colnames(aqi)<- colnames_aqi 
colnames(aqi)<- gsub("-", "", colnames(aqi))
colnames(aqi)<- paste0('AQI_', colnames(aqi))
aqi<- cbind(sensor, aqi)

##Write the csv file of aqi
write.csv(aqi, "Data/PM25_Weekly/aqi.csv")


### Scrape COVID data from Chicago data Portal
covid_chicago <- read.socrata("https://data.cityofchicago.org/resource/yhhz-zm2v.json")

### Calculate 7-day average total case
### Can get hospitality rate as well
covid_raw<- covid_chicago%>%
  rename(Date = week_start)%>%
  filter(Date >= '2020-12-01')%>%
  arrange(desc(Date))%>%
  dplyr::select(zip_code, Date, case_rate_weekly)%>%
  pivot_wider(names_from = 'Date', values_from = 'case_rate_weekly')%>%
  rename(zip = zip_code)

zipcode<- covid_raw[1]
covid<- covid_raw[2:ncol(covid_raw)]
colnames(covid)<- gsub("-", "", colnames(covid))
colnames(covid)<- paste0('COVID_Week_', colnames(covid))
covid<- cbind(zipcode, covid)

write.csv(covid, "Data/CovidWeekly.csv")