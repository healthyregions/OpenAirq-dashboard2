


### Create Tab Page for Variables with One Time EPA Data
generateOneTimeTab <- function(tabname, variablename, variabledescription, sourcedescription,
                               mapviewselected = "lac", mapheight = 500) {
  tabItem(tabName = tabname,
          fluidRow(
            box(width = 12,
                sliderInput(inputId = paste(tabname, "dt", sep = "_"), "Select day:",
                            min = strptime("2020/12/07","%Y/%m/%d"), 
                            max = strptime("2021/02/22","%Y/%m/%d"),
                            value = strptime("2021/02/22","%Y/%m/%d"),
                            timeFormat = "%Y/%m/%d",
                            step = as.difftime(1, units = "days"),
                            animate = animationOptions(interval = 2000))) # if animation is desired here, this time is probably too long
          ),
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


