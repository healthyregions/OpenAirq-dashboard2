


### Create Tab Page for Variables with One Time EPA Data
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


### Create Tab Page for Variables with One Time COVID Data
generateTab <- function(tabname, variablename, variabledescription, sourcedescription,
                                mapheight = 500) {
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
                ))
            ),
            box(width = 8,
                leafletOutput(paste(tabname, "map", sep = "_"), height = mapheight)
                
            )
          )
}


