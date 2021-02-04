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



## Scraping Data from Airnow 
## It seems that scraping directly from airnow using R is not an option
## Python should be working 
## Should we change to python?

apikey <- '8C890D5C-A78C-41A9-A087-061A9A8F1B10'
base_url <-"https://www.airnowapi.org/aq/data/"
current <- Sys.Date()
start<- as.Date('2021-01-01')
bounding<- paste0('-92.888114,36.970298,-84.784579,47.080621', collapse=",")

a<- GET(base_url, query=list("BBOX" ="-92.888114,36.970298,-84.784579,47.080621",
                              "parameters" = 'pm25',
                              "startDate"= '2021-01-01',
                              "endDate"= current,
                              "dataType"= 'B',
                              'format'='text/csv',
                              'api_key'= apikey,
                              'verbose'=1,
                              'nowcastonly'=0,
                              'includerawconcentrations'=0))
                              
## Scraping from EPA website 
## Data of year 2021 isn't available scraping, but only available downloading, 
## It has the most complete data
key = "khakiram62"
base_url<- "https://aqs.epa.gov/data/api/dailyData/byState/"
IL1<- GET(base_url, query=list("email" = "shuaiyuan4@gmail.com",
                              "param"= 88101,
                              "key"= key,
                              "bdate"= 20200101,
                              "edate"= 20201231,
                              "state"= 17))

IL<- GET(base_url, query=list("email" = "shuaiyuan4@gmail.com",
                              "param"= 88101,
                              "key"= key,
                              "bdate"= 20210101,
                              "edate"= 20210202,
                              "state"= 17))


## Direcly from importing 
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

## Load shapefiles for 21 counties 

counties <- st_read("Data/LargeAreaCounties/LargeAreaCounties.shp")

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
