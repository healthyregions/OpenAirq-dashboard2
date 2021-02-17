


### Create Tab Page for Variables with One Time Data
generateOneTimeTab <- function(tabname, variablename, variabledescription, sourcedescription,
                               mapviewselected = "lac", mapheight = 500) {
  tabItem(tabName = tabname,
          fluidRow(
            box(width = 4,
                tabsetPanel(
                  tabPanel(title = "Description",
                           h3(variablename),
                           p(variabledescription)),
                  tabPanel(title = "Source",
                           h4("Data Source"),
                           p(sourcedescription))
                ),
                radioGroupButtons(inputId = paste(tabname, "chi_zoom", sep = "_"),
                                  "Set View", 
                                  c("21 Counties" = "lac", 
                                    "Chicago" = "chi"),
                                  selected = mapviewselected)
            ),
            box(width = 8,
                leafletOutput(paste(tabname, "map", sep = "_"), height = mapheight)
                
            )
          ))
}


##### Generate Leaflet Map 
dashMap <- function(layername, layerpal, raster, area, layerId, rasterOpacity = 0.8,
                    EPApoints = NULL, VarName = NULL) {
  
  dMap <- leaflet(layername) %>%
    addProviderTiles("OpenStreetMap.HOT") %>%
    addRasterImage(raster[[layername]], opacity = rasterOpacity, colors = layerpal) %>%
    leaflet::addLegend(pal = layerpal, values = values(raster[[layername]]), title = gsub("_.*","",layername)) %>%
    addPolygons(data = area, 
                color = "darkslategray",
                fillOpacity  = 0.00, 
                stroke = TRUE,
                opacity = 1,
                layerId = layerId,
                weight = 1,
                highlight = highlightOptions(
                  weight = 2, 
                  color = "gray", 
                  fillOpacity = 0.05))
  
  if (!is.null(EPApoints)) {
    dMap <- dMap %>%
      addCircles(lng = EPApoints$Longitude[EPApoints$Var == VarName],
                 lat = EPApoints$Latitude[EPApoints$Var == VarName],
                 radius = 2, color = "black", opacity = 0.9)
  }
  
  dMap
}






