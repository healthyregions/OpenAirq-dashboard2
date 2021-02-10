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
library(tmap)
library(forcats)
library(broom)
library(wbstats)
library(wordcloud)
library(tidytext)
library(viridis)


## Load shapefiles for 21 counties 
counties <- st_read("Data/LargeAreaCounties/LargeAreaCounties.shp")


## Import data from the folder 
import_pm <- function(state_file){
  read_csv(state_file)}

pm2.5 <- dir("Data/PM2.5_2020_12_2021_1", pattern = "\\.csv$", full.names = TRUE)

full.data <- map_df(pm2.5, import_pm)

## Date Wrangling to Calculate Daily Average for PM2.5
county.pm25<- full.data%>%
  filter(COUNTY %in% counties$COUNTYNAME)%>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
  dplyr::select(Date, `Site ID`,`Daily Mean PM2.5 Concentration`, COUNTY,SITE_LATITUDE, SITE_LONGITUDE )%>%
  group_by(Date, `Site ID`,COUNTY )%>%
  summarise(pm2.5 = mean(`Daily Mean PM2.5 Concentration`, na.rm =TRUE),
            latitude = mean(SITE_LATITUDE), 
            longitude = mean(SITE_LONGITUDE) )%>%
  filter(Date >= '2020-12-01')%>%
  ungroup(Date)%>%
  arrange(desc(Date))%>%
  pivot_wider(names_from = 'Date', values_from = 'pm2.5')



# Create a function that calculate the 7-day average from the latest to 2020-12-07 
byapply <- function(x, by, fun, ...)
{
  # Create index list
  if (length(by) == 1)
  {
    nc <- ncol(x)
    split.index <- rep(1:ceiling(nc / by), each = by, length.out = nc)
  } else # 'by' is a vector of groups
  {
    nc <- length(by)
    split.index <- by
  }
  index.list <- split(seq(from = 1, to = nc), split.index)
  
  # Pass index list to fun using sapply() and return object
  sapply(index.list, function(i)
  {
    do.call(fun, list(x[, i], ...))
  })
}


# Run function
pm25 <- as.data.frame(t(byapply(county.pm25[5:ncol(county.pm25)],7, rowMeans, na.rm=TRUE)))
colnames(pm25) = gsub("V", "Week_", colnames(pm25))

## Date Wrangling to Calculate Daily Average for PM2.5
county.aqi<- full.data%>%
  filter(COUNTY %in% counties$COUNTYNAME)%>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
  dplyr::select(Date, `Site ID`,DAILY_AQI_VALUE, COUNTY,SITE_LATITUDE, SITE_LONGITUDE )%>%
  group_by(Date, `Site ID`,COUNTY )%>%
  summarise(aqi = mean(DAILY_AQI_VALUE, na.rm =TRUE),
            latitude = mean(SITE_LATITUDE), 
            longitude = mean(SITE_LONGITUDE) )%>%
  filter(Date >= '2020-12-01')%>%
  ungroup(Date)%>%
  arrange(desc(Date))%>%
  pivot_wider(names_from = 'Date', values_from = 'aqi')
# Run function 
aqi<- as.data.frame(t(byapply(county.aqi[5:ncol(county.aqi)],7, rowMeans, na.rm=TRUE)))
colnames(aqi)<- gsub("V", "Week_", colnames(aqi))


