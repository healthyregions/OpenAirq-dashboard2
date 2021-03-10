library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(htmlwidgets)
library(leaflet)
library(sf)
library(lubridate)
library(tidyverse)
library(classInt)
library(plotly)


##### DATA LOADING START #####
source("Functions.R")

large.area <- st_read("Data/LargeAreaCounties")
large.area$COUNTYNAME <- as.character(large.area$COUNTYNAME)

descriptions <- read.csv("Data/Description.csv", stringsAsFactors = F)

pm25<- read.csv("Data/PM25_Weekly/pm25.csv")
pm25.means<- data.frame(PM25=apply(na.omit(pm25[, 7:ncol(pm25)]), 2, mean)) # may be worth doing externally
week.idx<- seq(from=nrow(pm25.means) - 5, to=1, by=-7)
pm25.trace<- pm25.means$PM25[week.idx] # may be worth doing externally
aqi<- read.csv("Data/PM25_Weekly/aqi.csv")
aqi.means<- data.frame(AQI=apply(na.omit(aqi[, 7:ncol(aqi)]), 2, mean)) # may be worth doing externally
aqi.trace<- aqi.means$AQI[week.idx] # may be worth doing externally
covid.raw<- read.csv("Data/CovidWeekly.csv")
# FIXME: +1 is a workaround used to truncate COVID data without respective sensor data
covid.means<- data.frame(COVID=apply(na.omit(covid.raw[, ncol(covid.raw):(3 + 1)]), 2, mean)) # may be worth doing externally
covid.trace<- as.numeric(covid.means$COVID)
asthma.raw<- read.csv("Data/Asthma2017.csv")
asthma.raw$zip<- as.character(asthma.raw$zip)
chi.community.map <- st_read("Data/Chicago")
chi.admin.map<- st_read("Data/ZipcodeBoundary")
# chi.boundary<- st_transform(chi.admin.map, 3035) %>% # azimuthal equal-area projection (resolves warning)
#   st_union() %>%
#   st_transform(4326)
# cook.wo.chi <- st_difference(c(chi.boundary, large.area$geometry[large.area$COUNTYNAME == "Cook"]))[2]
covid<- left_join(chi.admin.map, covid.raw, by = ("zip"))
asthma<- left_join(chi.admin.map, asthma.raw, by = ("zip"))

##### DATA LOADING END ##### 

##### VARIABLE START #####

mapheight = 500

##### SENSOR START #####

sensor.tabname <- "sensor"

pm25.tabname <- "pm25"
pm25.name <- "Particulate Matter < 2.5μm (PM2.5)"
pm25.description <- descriptions$Description[descriptions["Variable"] == "PM2.5"]
pm25.source <- descriptions$Source[descriptions["Variable"] == "PM2.5"]

##### SENSOR END #####
##### AQI START #####

# aqi.tabname <- "aqi"
# aqi.name <- "Air Quality Index (AQI)"
# aqi.description <- descriptions$Description[descriptions["Variable"] == "AQI"]
# aqi.source <- descriptions$Source[descriptions["Variable"] == "AQI"]

##### PM2.5 END #####

##### COVID START #####
covid.tabname <- "covid"
##### COVID END #####

##### ASTHMA START #####
asthma.tabname <- "asthma"
##### ASTHMA END #####

##### VARIABLE END #####

##### THEME START #####

chicago_blue <- "rgb(128, 206, 255)"
chicago_red <- "rgb(199, 20, 20)"

sidebar_select_gradient <- cssGradientThreeColors(
  direction = "right"
  ,colorStart = "rgb(255, 67, 67)"
  ,colorMiddle = "rgb(255, 120, 120)"
  ,colorEnd = "rgb(255,175,175)"
  ,colorStartPos = 0
  ,colorMiddlePos = 30
  ,colorEndPos = 100
)

# sidebar_hover_gradient <- cssGradientThreeColors(
#   direction = "right"
#   ,colorStart = chicago_red
#   ,colorMiddle = "rgba(199,80,80,1)"
#   ,colorEnd = "rgba(199,110,110, 1)"
#   ,colorStartPos = 0
#   ,colorMiddlePos = 30
#   ,colorEndPos = 100
# )
sidebar_hover_gradient <- sidebar_select_gradient

### creating custom theme object
theme_air_chicago <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(217,217,217)"
  
  ### header
  ,logoBackColor = chicago_blue
  
  ,headerButtonBackColor = chicago_blue
  ,headerButtonIconColor = "rgb(245,245,245)"
  ,headerButtonBackColorHover = chicago_blue
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = chicago_blue
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = chicago_blue
  ,sidebarPadding = 2
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = sidebar_select_gradient
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = sidebar_hover_gradient
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
)

##### THEME END #####


###Function ###

ui <- dashboardPage(
  
  ##### LOGO START #####
  dashboardHeader(title = shinyDashboardLogoDIY(boldText = "Open Air-COVID",
                                                mainText = "Chicago",
                                                textSize = 16,
                                                badgeText = "BETA",
                                                badgeTextColor = "white",
                                                badgeTextSize = 2,
                                                badgeBackColor = chicago_red,
                                                badgeBorderRadius = 3)
  ),
  ##### LOGO END #####
  
  dashboardSidebar(sidebarMenu(id = "sidebar",
                               menuItem("Home", tabName = "home", icon = icon("home")),
                               menuItem("About", tabName = "about", icon = icon("info")),
                               menuItem("EPA Sensor Data", tabName = "sensor", icon = icon("envira")),
                               menuItem("COVID Data", tabName = "covid", icon = icon("medkit")),
                               menuItem("Asthma Data", tabName = "asthma", icon = icon("lungs")),
                               menuItem("Downloads", icon = icon("download"), tabName = "downloads"))
  ),
  
  dashboardBody(
    
    theme_air_chicago,
    
    tabItems(
      
      ##### HOME START #####
      tabItem(tabName = "home",
              fluidRow(
                box(width = 12,
                    sliderInput(paste("home", "dt", sep = "_"), "Select week:",
                                min = strptime("2020/12/06","%Y/%m/%d"), 
                                max = strptime("2021/02/14","%Y/%m/%d"),
                                value = strptime("2021/02/14","%Y/%m/%d"),
                                timeFormat = "%Y/%m/%d",
                                step = as.difftime(7, units = "days"),
                                animate = animationOptions(interval = 2000)))
              ),
              fluidRow(box(width = 4,
                           leafletOutput("home_points", height = mapheight)),
                       box(width = 4,
                           selectInput("home_points_var", "Select Variable for Sensors",
                                          c("Air Quality Index" = "aqi",
                                            "PM2.5" = "pm25",
                                            "None" = "none"),
                                       selected = "pm25"),
                           selectInput("home_choropleth_var", "Select Variable for Regions",
                                       c("Asthma ED Visits (0-18)" = "asthma018",
                                         "Asthma ED Visits (65+)" = "asthma65",
                                         "COVID-19 Cases" = "covid",
                                         "None" = "none"),
                                       selected = "covid"),
                           leafletOutput("home_choropleth", height = mapheight)),
                       box(width = 4,
                           plotlyOutput("home_plot", height = mapheight))),
              fluidRow(box(width = 4,
                           radioGroupButtons(inputId = paste("home_points", "source", sep = "_"),
                                             "Select Data Source:", 
                                             c("AQI" = "aqi", 
                                               "PM2.5" = "pm25",
                                               "None" = "none"),
                                             selected = "pm25")),
                       box(width = 2,
                           radioGroupButtons(inputId = paste("home_choropleth", "source", sep = "_"),
                                             "Select Data Source:", 
                                             c("COVID" = "covid", 
                                               "Asthma" = "asthma"),
                                             selected = "covid")),
                       box(width = 2,
                           radioGroupButtons(inputId = paste("home_choropleth", "age", sep = "_"),
                                             "Select Age Group (for ED visits):", 
                                             c("0-18" = "018", 
                                               "65+" = "65"),
                                             selected = "65")))),
      ##### HOME END #####
      
      ##### ABOUT START #####
      tabItem(tabName = "about"),
      ##### ABOUT END #####
      
      ##### SENSOR START #####
      generateOneTimeTab(sensor.tabname, pm25.name, pm25.description, pm25.source),
      ##### SENSOR END #####
      
      ##### COVID START #####
      tabItem(tabName = covid.tabname,
              fluidRow(
                box(width = 12,
                    sliderInput(paste(covid.tabname, "dt", sep = "_"), "Select week:",
                                min = strptime("2020/12/06","%Y/%m/%d"), 
                                max = strptime("2021/02/14","%Y/%m/%d"),
                                value = strptime("2021/02/14","%Y/%m/%d"),
                                timeFormat = "%Y/%m/%d",
                                step = as.difftime(7, units = "days"),
                                animate = animationOptions(interval = 2000))),
              fluidRow(
                box(width = 12,
                    leafletOutput("covid_map", height = mapheight)))

              )),
      ##### COVID END #####
      
      ##### ASTHMA START #####
      tabItem(tabName = asthma.tabname,
              fluidRow(
                box(width = 12,
                    leafletOutput("asthma_map", height = mapheight)),
              ),
              fluidRow(
                box(width = 3,
                    radioGroupButtons(inputId = paste(asthma.tabname, "source", sep = "_"),
                                      "Select Age Group:", 
                                      c("0-18" = "018", 
                                        "65+" = "65"),
                                      selected = "65"))
              )),
      ##### ASTHMA END #####
      
      ##### DOWNLOADS START #####
      tabItem(tabName = "downloads")
      ##### DOWNLOADS END #####
      
    )))


## Specify Mapping Details

start_date<- strptime(names(covid)[ncol(covid) - 1], "COVID_Week_%Y%m%d")
end_date<- strptime(names(covid)[6 + 1], "COVID_Week_%Y%m%d") # +1 is a placeholder to align covid/sensors
daterange<- paste("From", end_date, "to", end_date + days(6), sep = " ")

# deprecated
# end_date<- Sys.Date()
# start_date<- end_date - 6

# home.view <- list(lat = "41.97736", lng = "-87.62255", zoom = 7)

### Consider rounding these breaks? ###

# pm25.bins <- classIntervals(na.omit(unlist(pm25[,7:ncol(pm25)])), 5, style="quantile")$brks # 5 quantile bins
pm25.bins <- classIntervals(na.omit(unlist(pm25[,7:ncol(pm25)])), 5, style="fisher")$brks # 5 natural bins
pm25palette <- colorBin(palette="YlOrRd" , bins=pm25.bins, na.color="dimgrey") # discrete
# pm25palette <- colorBin(palette="YlOrRd" , domain = unlist(pm25[,6:ncol(pm25)]), na.color="dimgrey") # continuous

aqi.bins<- c(0, 50, 100, 150, 200, 300, 500)
aqi.palette<- c('#00FF00','#FFFF00','#FFA500','#FF0000','#99004C','#800000')
aqi.legend.labels<- c("Good", "Moderate", "USG", 
                      "Unhealthy", "Very Unhealthy", "Harzardous")

aqipalette <- colorBin(palette= aqi.palette, bins = aqi.bins, na.color="dimgrey")

# covid.bins <- classIntervals(na.omit(c(sapply(6:15, function(z) covid[,z][[1]]))), 5, style="quantile")$brks # 5 quantile bins
covid.bins <- classIntervals(na.omit(c(sapply(6:15, function(z) covid[,z][[1]]))), 5, style="fisher")$brks # 5 natural bins
covidpalette <- colorBin(palette="YlOrRd" , bins=covid.bins, na.color="transparent") # discrete
# covidpalette <- colorBin(palette="YlOrRd" , domain = c(sapply(6:15, function(z) covid[,z][[1]])), na.color="transparent") # continuous

# asthma.bins <- classIntervals(na.omit(c(sapply(6:7, function(z) asthma[,z][[1]]))), 5, style="quantile")$brks # 5 quantile bins
asthma.bins <- classIntervals(na.omit(c(sapply(6:7, function(z) asthma[,z][[1]]))), 5, style="fisher")$brks # 5 natural bins
asthmapalette <- colorBin(palette="YlOrRd" , bins=asthma.bins, na.color="transparent") # discrete
# asthmapalette <- colorBin(palette="YlOrRd" , domain = na.omit(c(sapply(6:7, function(z) asthma[,z][[1]]))), na.color="transparent") # continuous

# Getting an error: Error in sprintf(covid$zip) : 'fmt' is not a character vector
labels_covid <- sprintf(
  as.character(covid$zip)
) %>% lapply(htmltools::HTML)


server <- function(input, output) {
  ##### HOME START #####
  all.fips <- reactiveValues(fips = c())
  all.sensors <- reactiveValues(sensors = c())
  
  output$home_points <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron")%>%
      # setView(lat = home.view$lat, lng = home.view$lng, zoom = home.view$zoom)%>%
      addPolygons(data = large.area, 
                  color = "darkslategray",
                  fillOpacity  = 0.00, 
                  stroke = TRUE,
                  opacity = 1,
                  layerId = large.area$FIPS,
                  weight = 1,
                  highlight = highlightOptions(
                    weight = 2, 
                    color = "gray", 
                    fillOpacity = 0.05))%>%
      addCircles(data = pm25$PM25_20210220,
                 lng = pm25$longitude, 
                 lat = pm25$latitude, 
                 color = pm25palette(pm25$PM25_20210220), 
                 fillOpacity = 0.5, 
                 radius= 5000, 
                 stroke=FALSE,
                 layerId = pm25$Site.ID,
                 label = getLabels(input$home_dt + days(6), pm25, "PM25"),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", 
                                padding = "3px 8px"),
                   textsize = "15px",
                   direction = "auto"))%>%
      addControl(paste0(daterange), position = "bottomleft")%>%
      addLegend("bottomright", pal = pm25palette, values = pm25$PM25_20210220,
                title = "PM2.5", opacity = 1)
    
  })
  output$home_choropleth <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron")%>%
      # setView(lat = home.view$lat, lng = home.view$lng, zoom = home.view$zoom)%>%
      addPolygons(data = covid, 
                  fillColor = covidpalette(covid$COVID_Week_20210214),
                  fillOpacity  = 0.7, 
                  color = "white",
                  stroke = FALSE,
                  weight = 2,
                  opacity = 1,
                  dashArray = "3",
                  layerId = covid$zip,
                  label = labels_covid,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", 
                                 padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))%>%
      addControl(paste0(daterange), position = "bottomleft")%>%
      addLegend("bottomright", pal = covidpalette, values = covid$COVID_Week_20210214,
                title = "COVID Cases / 100,000", opacity = 1)
    
  })
  output$home_plot <- renderPlotly({
    dates <- format(strptime(rownames(covid.means), "COVID_Week_%Y%m%d"), "%Y-%m-%d")
    blues <- c("#033682", "#0356a3", "#0083d9", "#66ccff", "#c9e8ff")
    reds <- c("#9c1500", "#f52302", "#ff6e57", "#ff9a8a", "#ffc8bf")
    greens <- c("#165422", "#0b9926", "#14ff41", "#91faa5", "#d6ffde")
    
    p <- plot_ly() %>% config(displayModeBar = F) %>%
      layout(legend = list(x = .5, y = 100, orientation = "h")) %>%
      add_trace(x = dates,
                y = covid.trace,
                type = "scatter",
                mode = "lines",
                opacity = 1,
                line = list(dash = "solid", color = blues[1]),
                name = paste("Average", "COVID cases / zip code", sep = " "),
                text = paste("Average", "COVID cases / zip code", sep = " ")) %>%
      add_trace(x = dates,
                y = aqi.trace,
                type = "scatter",
                mode = "lines",
                opacity = 1,
                line = list(dash = "solid", color = greens[1]),
                name = paste("Average", "AQI / sensor", sep = " "),
                text = paste("Average", "AQI / sensor", sep = " ")) %>%
      add_trace(x = dates,
                y = pm25.trace,
                type = "scatter",
                mode = "lines",
                opacity = 1,
                line = list(dash = "solid", color = reds[1]),
                name = paste("Average", "PM2.5 / sensor", sep = " "),
                text = paste("Average", "PM2.5 / sensor", sep = " "))
    
    if (length(all.fips$fips) > 0) {
      for (i in 1:length(all.fips$fips)) {
        p <- p %>%
          add_trace(x = dates,
                    # FIXME: +1 is a workaround used to truncate COVID data without respective sensor data
                    y = as.numeric(covid.raw[which(grepl(all.fips$fips[i], covid.raw$zip)), ncol(covid.raw):(3 + 1)]),
                    type = "scatter",
                    mode = "lines",
                    opacity = 0.8,
                    line = list(dash = "dot", color = blues[(i %% length(blues)) + 1]),
                    name = paste("COVID cases in", all.fips$fips[i], sep = " "),
                    text = paste("COVID cases in", all.fips$fips[i], sep = " "))
      }
    }
    if (length(all.sensors$sensors) > 0) {
      for (i in 1:length(all.sensors$sensors)) {
        p <- p %>%
          add_trace(x = dates,
                    # +6 offsets week.idx to match full data frame
                    y = as.numeric(aqi[which(grepl(all.sensors$sensors[i], aqi$Site.ID)), (week.idx + 6)]),
                    type = "scatter",
                    mode = "lines",
                    opacity = 0.8,
                    line = list(dash = "dot", color = greens[(i %% length(greens)) + 1]),
                    name = paste("AQI at", 
                                 aqi$name[which(grepl(all.sensors$sensors[i], aqi$Site.ID))], 
                                 "sensor", sep = " "),
                    text = paste("AQI at", 
                                 aqi$name[which(grepl(all.sensors$sensors[i], aqi$Site.ID))], 
                                 "sensor", sep = " "))
        p <- p %>%
          add_trace(x = dates,
                    # +6 offsets week.idx to match full data frame
                    y = as.numeric(pm25[which(grepl(all.sensors$sensors[i], pm25$Site.ID)), (week.idx + 6)]),
                    type = "scatter",
                    mode = "lines",
                    opacity = 0.8,
                    line = list(dash = "dot", color = reds[(i %% length(reds)) + 1]),
                    name = paste("PM2.5 at", 
                                 pm25$name[which(grepl(all.sensors$sensors[i], pm25$Site.ID))], 
                                 "sensor", sep = " "),
                    text = paste("PM2.5 at", 
                                 pm25$name[which(grepl(all.sensors$sensors[i], pm25$Site.ID))], 
                                 "sensor", sep = " "))
      }
    }
    
    p
  })
  observeEvent(input$home_choropleth_shape_click, { # update reactive values on click
    if(input$sidebar == "home") {
      if(typeof(input$home_choropleth_shape_click$id) == "character") { # zip code
        all.fips$fips <- unique(c(all.fips$fips, input$home_choropleth_shape_click$id))
      }
      else if(typeof(input$home_choropleth_shape_click$id) == "integer") { # sensor id
        all.sensors$sensors <- unique(c(all.sensors$sensors, input$home_choropleth_shape_click$id))
      }
    }
  })
  observe({
    if (input$sidebar == "home") {
      points.date <- input$home_dt + days(6) # week shifts forward for sensor data
      
      # sensor map
      if (input$home_points_source == "aqi") {
        points.col <- aqi[, which(colnames(aqi) == format(points.date, "AQI_%Y%m%d"))]
        points.render <- function(map) { # define function to render points across maps
          addCircles(map,
                     data = points.col,
                     lng = aqi$longitude, 
                     lat = aqi$latitude, 
                     color = aqipalette(points.col), 
                     fillOpacity = 0.5, 
                     radius = 5000, 
                     stroke = FALSE,
                     layerId = aqi$Site.ID,
                     label = getLabels(points.date, aqi, "AQI"),
                     labelOptions = labelOptions(
                       style = list("font-weight" = "normal", 
                                    padding = "3px 8px"),
                       textsize = "15px",
                       direction = "auto"))
        }
        points.legend <- function(map) {
          addLegend(map, "bottomright", pal = aqipalette, values = points.col,
                    labFormat = function(type, cuts, p) {
                      paste0(aqi.legend.labels)
                    },
                    title = "AQI", opacity = 1)
        }
        leafletProxy("home_points")%>%
          clearControls()%>%
          clearShapes()%>%
          addProviderTiles("CartoDB.Positron")%>%
          addPolygons(data = large.area, 
                      color = "darkslategray",
                      fillOpacity  = 0.00, 
                      stroke = TRUE,
                      opacity = 1,
                      layerId = large.area$FIPS,
                      weight = 1,
                      highlight = highlightOptions(
                        weight = 2, 
                        color = "gray", 
                        fillOpacity = 0.05))%>%
          points.render%>%
          addControl(paste0("From ", format(input$home_dt, "%Y-%m-%d"), " to ", format(input$home_dt + days(6), "%Y-%m-%d")), position = "bottomleft")%>%
          points.legend
      }
      else if (input$home_points_source == "pm25") {
        points.col <- pm25[, which(colnames(pm25) == format(points.date, "PM25_%Y%m%d"))]
        points.render <- function(map) { # define function to render points across maps
          addCircles(map,
                     data = points.col,
                     lng = pm25$longitude, 
                     lat = pm25$latitude, 
                     color = pm25palette(points.col), 
                     fillOpacity = 0.5, 
                     radius = 5000, 
                     stroke = FALSE,
                     layerId = pm25$Site.ID,
                     label = getLabels(points.date, pm25, "PM25"),
                     labelOptions = labelOptions(
                       style = list("font-weight" = "normal", 
                                    padding = "3px 8px"),
                       textsize = "15px",
                       direction = "auto"))
        }
        points.legend <- function(map) {
          addLegend(map, "bottomright", pal = pm25palette, values = points.col,
                    title = "PM2.5", opacity = 1)
        }
        leafletProxy("home_points")%>%
          clearControls()%>%
          clearShapes()%>%
          addProviderTiles("CartoDB.Positron")%>%
          addPolygons(data = large.area, 
                      color = "darkslategray",
                      fillOpacity  = 0.00, 
                      stroke = TRUE,
                      opacity = 1,
                      layerId = large.area$FIPS,
                      weight = 1,
                      highlight = highlightOptions(
                        weight = 2, 
                        color = "gray", 
                        fillOpacity = 0.05))%>%
          points.render()%>%
          addControl(paste0("From ", format(input$home_dt, "%Y-%m-%d"), " to ", format(input$home_dt + days(6), "%Y-%m-%d")), position = "bottomleft")%>%
          points.legend
      }
      else if (input$home_points_source == "none") {
        points.render <- function(map) { # define function to render points across maps
          map
        }
        points.legend <- function(map) {
          map
        }
      }
      # choropleth map
      if (input$home_choropleth_source == "covid") {
        in.col <- covid[, which(colnames(covid) == format(input$home_dt, "COVID_Week_%Y%m%d"))][[1]]
        leafletProxy("home_choropleth")%>%
          clearControls()%>%
          clearShapes()%>%
          addPolygons(data = covid, 
                      fillColor = covidpalette(in.col),
                      fillOpacity  = 0.7, 
                      color = "white",
                      stroke = FALSE,
                      weight = 2,
                      opacity = 1,
                      dashArray = "3",
                      layerId = covid$zip,
                      label = labels_covid,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", 
                                     padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"))%>%
          points.render%>%
          addControl(paste0("From ", format(input$home_dt, "%Y-%m-%d"), " to ", format(input$home_dt + days(6), "%Y-%m-%d")),
                     position = "bottomleft")%>%
          addLegend("bottomright", pal = covidpalette, values = in.col,
                    title = "COVID Cases / 100,000", opacity = 1)%>%
          points.legend
      }
      else if (input$home_choropleth_source == "asthma") {
        if (input$home_choropleth_age == "018") {
          leafletProxy("home_choropleth")%>%
            clearControls()%>%
            clearShapes()%>%
            addPolygons(data = asthma, 
                        fillColor = asthmapalette(asthma$rate0_18),
                        fillOpacity  = 0.7, 
                        color = "white",
                        stroke = FALSE,
                        weight = 2,
                        opacity = 1,
                        dashArray = "3",
                        layerId = asthma$zip,
                        label = labels_covid,
                        labelOptions = labelOptions(
                          style = list("font-weight" = "normal", 
                                       padding = "3px 8px"),
                          textsize = "15px",
                          direction = "auto"))%>%
            points.render%>%
            addControl(paste0("From 2017"), position = "bottomleft")%>%
            addLegend("bottomright", pal = asthmapalette, values = asthma$rate0_18,
                      title = "Asthma ED Visits / 10,000", opacity = 1)%>%
            points.legend
        }
        else if (input$home_choropleth_age == "65") {
          leafletProxy("home_choropleth")%>%
            clearControls()%>%
            clearShapes()%>%
            addPolygons(data = asthma, 
                        fillColor = asthmapalette(asthma$rate65),
                        fillOpacity  = 0.7, 
                        color = "white",
                        stroke = FALSE,
                        weight = 2,
                        opacity = 1,
                        dashArray = "3",
                        layerId = asthma$zip,
                        label = labels_covid,
                        labelOptions = labelOptions(
                          style = list("font-weight" = "normal", 
                                       padding = "3px 8px"),
                          textsize = "15px",
                          direction = "auto"))%>%
            points.render%>%
            addControl(paste0("From 2017"), position = "bottomleft")%>%
            addLegend("bottomright", pal = asthmapalette, values = asthma$rate65,
                      title = "Asthma ED Visits / 10,000", opacity = 1)%>%
            points.legend
        }
      }
    }
  })
  ##### HOME END #####
  
  ##### SENSOR START #####
  output$sensor_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron")%>%
      addPolygons(data = large.area, 
                  color = "darkslategray",
                  fillOpacity  = 0.00, 
                  stroke = TRUE,
                  opacity = 1,
                  layerId = large.area$FIPS,
                  weight = 1,
                  highlight = highlightOptions(
                    weight = 2, 
                    color = "gray", 
                    fillOpacity = 0.05))%>%
      addCircles(data = pm25$PM25_20210220,
                 lng = pm25$longitude, 
                 lat = pm25$latitude, 
                 color = pm25palette(pm25$PM25_20210220), 
                 fillOpacity = 0.5, 
                 radius= 5000, 
                 stroke=FALSE,
                 label = getLabels(input$sensor_dt + days(6), aqi, "AQI"),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", 
                                padding = "3px 8px"),
                   textsize = "15px",
                   direction = "auto"))%>%
      addControl(paste0(daterange), position = "bottomleft")%>%
      addLegend("bottomright", pal = pm25palette, values = pm25$PM25_20210220,
                title = "PM2.5", opacity = 1)
    
  })
  observe({
    if (input$sidebar == "sensor") { #Optimize Dashboard speed by not observing outside of tab
      in.date <- input$sensor_dt + days(6)
      if(input$sensor_source == "aqi") {
        in.col <- aqi[, which(colnames(aqi) == format(in.date, "AQI_%Y%m%d"))]
        leafletProxy("sensor_map")%>%
          clearControls()%>%
          clearShapes()%>%
          addProviderTiles("CartoDB.Positron")%>%
          addPolygons(data = large.area, 
                      color = "darkslategray",
                      fillOpacity  = 0.00, 
                      stroke = TRUE,
                      opacity = 1,
                      layerId = large.area$FIPS,
                      weight = 1,
                      highlight = highlightOptions(
                        weight = 2, 
                        color = "gray", 
                        fillOpacity = 0.05))%>%
          addCircles(data = in.col,
                     lng = aqi$longitude, 
                     lat = aqi$latitude, 
                     color = aqipalette(in.col), 
                     fillOpacity = 0.5, 
                     radius = 5000, 
                     stroke = FALSE,
                     label = getLabels(in.date, aqi, "AQI"),
                     labelOptions = labelOptions(
                       style = list("font-weight" = "normal", 
                                    padding = "3px 8px"),
                       textsize = "15px",
                       direction = "auto"))%>%
          addControl(paste0("From ", format(input$sensor_dt, "%Y-%m-%d"), " to ", format(input$sensor_dt + days(6), "%Y-%m-%d")), position = "bottomleft")%>%
          addLegend("bottomright", pal = aqipalette, values = in.col,
                    labFormat = function(type, cuts, p) {
                      paste0(aqi.legend.labels)
                    },
                    title = "AQI", opacity = 1)
      }
      else if(input$sensor_source == "pm25") {
        in.col <- pm25[, which(colnames(pm25) == format(in.date, "PM25_%Y%m%d"))]
        leafletProxy("sensor_map")%>%
          clearControls()%>%
          clearShapes()%>%
          addProviderTiles("CartoDB.Positron")%>%
          addPolygons(data = large.area, 
                      color = "darkslategray",
                      fillOpacity  = 0.00, 
                      stroke = TRUE,
                      opacity = 1,
                      layerId = large.area$FIPS,
                      weight = 1,
                      highlight = highlightOptions(
                        weight = 2, 
                        color = "gray", 
                        fillOpacity = 0.05))%>%
          addCircles(data = in.col,
                     lng = pm25$longitude, 
                     lat = pm25$latitude, 
                     color = pm25palette(in.col), 
                     fillOpacity = 0.5, 
                     radius = 5000, 
                     stroke = FALSE,
                     label = getLabels(in.date, pm25, "PM25"),
                     labelOptions = labelOptions(
                       style = list("font-weight" = "normal", 
                                    padding = "3px 8px"),
                       textsize = "15px",
                       direction = "auto"))%>%
          addControl(paste0("From ", format(input$sensor_dt, "%Y-%m-%d"), " to ", format(input$sensor_dt + days(6), "%Y-%m-%d")), position = "bottomleft")%>%
          addLegend("bottomright", pal = pm25palette, values = in.col,
                    title = "PM2.5", opacity = 1)
      }
    }
  })
  ##### SENSOR END #####
  
  ##### COVID START #####
  output$covid_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron")%>%
      addPolygons(data = covid, 
                  fillColor = covidpalette(covid$COVID_Week_20210214),
                  fillOpacity  = 0.7, 
                  color = "white",
                  stroke = FALSE,
                  weight = 2,
                  opacity = 1,
                  dashArray = "3",
                  label = labels_covid,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", 
                                 padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))%>%
      addControl(paste0(daterange), position = "bottomleft")%>%
      addLegend("bottomright", pal = covidpalette, values = covid$COVID_Week_20210214,
                title = "COVID Cases / 100,000", opacity = 1)
    
  })
  observeEvent(input$covid_dt, {
    if (input$sidebar == "covid") { #Optimize Dashboard speed by not observing outside of tab
      # can someone explain why integer indexing fetches the column and geometry??
      in.col <- covid[, which(colnames(covid) == format(input$covid_dt, "COVID_Week_%Y%m%d"))][[1]]
      leafletProxy("covid_map")%>%
        clearControls()%>%
        clearShapes()%>%
        addPolygons(data = covid, 
                    fillColor = covidpalette(in.col),
                    fillOpacity  = 0.7, 
                    color = "white",
                    stroke = FALSE,
                    weight = 2,
                    opacity = 1,
                    dashArray = "3",
                    label = labels_covid,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", 
                                   padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))%>%
        addControl(paste0("From ", format(input$covid_dt, "%Y-%m-%d"), " to ", format(input$covid_dt + days(6), "%Y-%m-%d")),
                   position = "bottomleft")%>%
        addLegend("bottomright", pal = covidpalette, values = in.col,
                  title = "COVID Cases / 100,000", opacity = 1)
    }
  })
  ##### COVID END #####
  
  ##### ASTHMA START #####
  output$asthma_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron")%>%
      addPolygons(data = asthma, 
                  fillColor = asthmapalette(asthma$rate65),
                  fillOpacity  = 0.7, 
                  color = "white",
                  stroke = FALSE,
                  weight = 2,
                  opacity = 1,
                  dashArray = "3",
                  label = labels_covid,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", 
                                 padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))%>%
      addControl(paste0("From 2017"), position = "bottomleft")%>%
      addLegend("bottomright", pal = asthmapalette, values = asthma$rate65,
                title = "Asthma ED Visits / 10,000", opacity = 1)
    
  })
  observeEvent(input$asthma_source, {
    if(input$sidebar == "asthma") { #Optimize Dashboard speed by not observing outside of tab
      if(input$asthma_source == "018") {
        leafletProxy("asthma_map")%>%
          clearShapes()%>%
          addPolygons(data = asthma, 
                      fillColor = asthmapalette(asthma$rate0_18),
                      fillOpacity  = 0.7, 
                      color = "white",
                      stroke = FALSE,
                      weight = 2,
                      opacity = 1,
                      dashArray = "3",
                      label = labels_covid,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", 
                                     padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"))
      }
      else if (input$asthma_source == "65") {
        leafletProxy("asthma_map")%>%
          clearShapes()%>%
          addPolygons(data = asthma, 
                      fillColor = asthmapalette(asthma$rate65),
                      fillOpacity  = 0.7, 
                      color = "white",
                      stroke = FALSE,
                      weight = 2,
                      opacity = 1,
                      dashArray = "3",
                      label = labels_covid,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", 
                                     padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"))
      }
    }
  })
  ##### ASTHMA END #####
  
  ##### DOWNLOADS #####
}

shinyApp(ui, server)

