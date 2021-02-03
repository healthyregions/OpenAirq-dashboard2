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
library(tidyverse)


## Load shapefiles for 21 counties 

counties <- st_read("Data/LargeAreaCounties/LargeAreaCounties.shp")

## Import PM2.5 Data

import_pm <- function(state_file){
  read_csv(state_file)}

pm2.5 <- dir("Data/PM2.5_2020_12_2021_1", pattern = "\\.csv$", full.names = TRUE)

full.data <- map_df(pm2.5, import_pm)

county.pm25<- full.data%>%
  filter(COUNTY %in% counties$COUNTYNAME)%>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
  dplyr::select(Date, `Site ID`,`Daily Mean PM2.5 Concentration`, COUNTY,SITE_LATITUDE, SITE_LONGITUDE )%>%
  group_by(Date, `Site ID`,COUNTY )%>%
  summarise(pm2.5 = mean(`Daily Mean PM2.5 Concentration`, na.rm =TRUE),
            latitude = mean(SITE_LATITUDE), 
            longitude = mean(SITE_LONGITUDE) )%>%
  pivot_wider(names_from = Date, values_from = "pm2.5")

x<-county.pm25

# Create a function that calculate the means for every 7 days
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
y <- as.data.frame(t(byapply(x[5:ncol(x)], 7, rowMeans, na.rm=TRUE)))
colnames(y) = gsub("V", "Week_", colnames(y))

# Bind with Sensor Location 
