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
source("Functions.R")

large.area <- st_read("Data/LargeAreaCounties")
large.area$COUNTYNAME <- as.character(large.area$COUNTYNAME)

descriptions <- read.csv("Data/Description.csv", stringsAsFactors = F)

pm25<- read.csv("Data/PM25_Weekly/pm25.csv")
aqi<- read.csv("Data/PM25_Weekly/aqi.csv")
covid.raw<- read.csv("Data/CovidWeekly.csv")
asthma.raw<- read.csv("Data/Asthma2017.csv")
asthma.raw$zip<- as.character(asthma.raw$zip)
chi.community.map <- st_read("Data/Chicago")
chi.admin.map<- st_read("Data/ZipcodeBoundary")
covid<- left_join(chi.admin.map, covid.raw, by = ("zip"))
asthma<- left_join(chi.admin.map, asthma.raw, by = ("zip"))

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
                               menuItem("EPA Sensor Data", icon = icon("envira"),
                                        menuSubItem("PM2.5", tabName = "pm25"),
                                        menuSubItem("AQI", tabName = "aqi")),
                               menuItem("COVID Data", tabName = "covid", icon = icon("medkit")),
                               menuItem("Asthma Data", tabName = "asthma", icon = icon("lungs")),
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
    
      generateOneTimeTab(pm25.tabname, pm25.name, pm25.description, pm25.source),
      
      generateOneTimeTab(aqi.tabname, aqi.name, aqi.description, aqi.source),
      
      tabItem(tabName = covid.tabname,
                fluidRow(
                  box(width = 12,
                      leafletOutput("covid_map", height = mapheight))
                )),

      tabItem(tabName = asthma.tabname,
              fluidRow(
                box(width = 12,
                    leafletOutput("asthma_map", height = mapheight)),
              ),
              fluidRow(
                box(width = 3,
                    radioGroupButtons(inputId = paste(asthma.tabname, "source", sep = "_"),
                                      "Set Age", 
                                      c("0-18" = "018", 
                                        "65+" = "65"),
                                      selected = "65"))
              )),

      ##### DOWNLOADS START #####
      tabItem(tabName = "downloads")
      ##### DOWNLOADS END #####
      
    )))


## Specify Mapping Details

end_date<- Sys.Date()
start_date<- end_date - 6
daterange<- paste("The week of", start_date, "to", end_date, sep = " ")



pm25palette <- colorBin(palette="YlOrRd" , domain = pm25$PM25_20210222, na.color="transparent")

aqi.bins<- c(0, 50, 100, 150, 200, 300, 500)
aqi.palette<- c('#00FF00','#FFFF00','#FFA500','#FF0000','#99004C','#800000')
aqi.legend.labels<- c("Good", "Moderate", "USG", 
                       "Unhealthy", "Very Unhealthy", "Harzardous")
aqipalette <- colorBin(palette= aqi.palette, bins = aqi.bins, na.color="transparent")

covidpalette <- colorBin(palette="YlOrRd" , domain = covid$COVID_Week_20210207, na.color="transparent")

asthmapalette <- colorBin(palette="YlOrRd" ,
                          domain = min(min(asthma$rate0_18, na.rm=T), min(asthma$rate65, na.rm=T)):max(max(asthma$rate0_18, na.rm=T), max(asthma$rate65, na.rm=T)),
                          na.color="transparent")


labels_covid <- sprintf(
  covid$zip
) %>% lapply(htmltools::HTML)

server <- function(input, output) {

  ##### HOME END #####
  
  output$pm25_map <- renderLeaflet({
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
      addCircles(data = pm25$PM25_20210222,
                 lng = pm25$longitude, 
                 lat = pm25$latitude, 
                 color = pm25palette(pm25$PM25_20210222), 
                 fillOpacity = 0.5, 
                 radius= 5000, 
                 stroke=FALSE)%>%
      addControl(paste0(daterange), position = "bottomleft")%>%
      addLegend("bottomright", pal = pm25palette, values = pm25$PM25_20210222,
                title = "PM2.5", opacity = 1)
      
  })
  output$aqi_map <- renderLeaflet({
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
      addCircles(data = aqi$AQI_20210222,
                 lng = aqi$longitude, 
                 lat = aqi$latitude, 
                 color = aqipalette(aqi$AQI_20210222), 
                 fillOpacity = 0.5, 
                 radius= 5000, 
                 stroke=FALSE)%>%
      addControl(paste0(daterange), position = "bottomleft")%>%
      addLegend("bottomright", pal = aqipalette, values = aqi$AQI_20210222,
                labFormat = function(type, cuts, p) {
                  paste0(aqi.legend.labels)
                },
                title = "AQI", opacity = 1)
    
  })
  output$covid_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron")%>%
      addPolygons(data = covid, 
                  fillColor = covidpalette(covid$COVID_Week_20210207),
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
      addControl(paste0("From 2021-02-07 to 2021-02-13"), position = "bottomleft")%>%
      addLegend("bottomright", pal = covidpalette, values = covid$COVID_Week_20210207,
                title = "COVID Cases", opacity = 1)
    
  })
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
                title = "Asthma ED Visits", opacity = 1)
    
  })
  observeEvent(input$asthma_source, {
    if(input$sidebar == "asthma") { #Optimize Dashboard speed by not observing outside of tab
      if(input$asthma_source == "018") {
        fillColor <- asthmapalette(asthma$rate0_18)
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
        fillColor <- asthmapalette(asthma$rate65)
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
  
}

shinyApp(ui, server)

