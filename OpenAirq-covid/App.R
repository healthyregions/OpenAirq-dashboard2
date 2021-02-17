library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(htmlwidgets)
library(leaflet)
library(sf)
library(lubridate)
library(tidyverse)

##### DATA LOADING START #####

large.area <- st_read("Data/LargeAreaCounties")
large.area$COUNTYNAME <- as.character(large.area$COUNTYNAME)

descriptions <- read.csv("Data/Description.csv", stringsAsFactors = F)

pm25<- read.csv("Data/PM25_Weekly/pm25.csv")
aqi<- read.csv("Data/PM25_Weekly/aqi.csv")

chi.map <- st_read("Data/Chicago")

##### DATA LOADING END ##### 

##### VARIABLE START #####

mapheight = 500

##### PM2.5 START #####

pm25.tabname <- "pm25"
pm25.name <- "Particulate Matter < 2.5μm (PM2.5)"
pm25.description <- descriptions$Description[descriptions["Variable"] == "PM2.5"]
pm25.source <- descriptions$Source[descriptions["Variable"] == "PM2.5"]

##### PM2.5 END #####
##### AQI START #####

aqi.tabname <- "aqi"
aqi.name <- "Air Quality Index (AQI)"
aqi.description <- descriptions$Description[descriptions["Variable"] == "AQI"]
aqi.source <- descriptions$Source[descriptions["Variable"] == "AQI"]

##### PM2.5 END #####

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


## Visualization
tmap_mode("view")

tm_shape(counties) +
  tm_borders() +
  tm_shape(county.pm25) +
  tm_bubbles(col  = "PM 2.5 emissions",
             alpha = 0.3,
             size = "PM 2.5 emissions",
             style = "fisher")

##### DATA LOADING END #####

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
                               menuItem("EPA Sensor Data", icon = icon("envira"),
                                        menuSubItem("PM2.5", tabName = "pm25"),
                                        menuSubItem("AQI", tabName = "aqi")),
                               menuItem("Downloads", icon = icon("download"), tabName = "downloads"))
  ),
  
  dashboardBody(
    
    theme_air_chicago,
    
    tabItems(
      
      ##### HOME START #####
      tabItem(tabName = "home"),
      ##### HOME END #####
      
      ##### ABOUT START #####
      tabItem(tabName = "about"),
      ##### ABOUT END #####
      
      ##### PM2.5 START #####
    
      generateQuarterlyTab(pm25.tabname, pm25.name, pm25.description, pm25.source),
      
      generateQuarterlyTab(pm10.tabname, pm10.name, pm10.description, pm10.source),
  
      
      ##### DOWNLOADS START #####
      tabItem(tabName = "downloads")
      ##### DOWNLOADS END #####
      
    )))


mypalette <- colorBin( palette="YlOrBr", domain=quakes$mag, na.color="transparent")

server <- function(input, output) {
  
  
  ##### HOME START #####
  
 data_input<- reactive({county.pm25$`Daily Mean PM2.5 Concentration`})
 labels_input<- reactive({county.pm25$Date})

  output$homemap <- renderLeaflet({
    leaflet(counties) %>%
      addProviderTiles(provider = "OpenStreetMap.HOT") %>%
      setView(lat = "41.97736", lng = "-87.62255", zoom = 7) %>% 
      leaflet::addPolygons(weight = 1,
                           color = "gray",
                           layerId = counties$FIPS,
                           fillOpacity = 0.2,
                           label = counties$COUNTYNAME,
                           highlight = highlightOptions(
                             weight = 2,
                             color = "#666",
                             fillOpacity = 0.7,
                             bringToFront = TRUE))
  })
  
  ##### HOME END #####
  
  output$pm25_map <- renderLeaflet({
    leaflet(counties) %>%
      addProviderTiles(provider = "OpenStreetMap.HOT") %>%
      setView(lat = "41.97736", lng = "-87.62255", zoom = 7) %>% 
      addCircleMarkers(county.pm25$SITE_LONGITUDE,county.pm25$SITE_LATITUDE, 
                         fillColor = mypalette(data_input), fillOpacity = 0.7, color="white", radius=8, stroke=FALSE,
                         label = labels_input,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
        )

  })
}

shinyApp(ui, server) 

