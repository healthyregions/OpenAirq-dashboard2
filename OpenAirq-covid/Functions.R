


### Create Tab Page for Variables with One Time EPA Data
generateOneTimeTab <- function(tabname, variablename, variabledescription, sourcedescription,
                               mapviewselected = "lac", mapheight = 500) {
  tabItem(tabName = tabname,
          fluidRow(
            box(width = 12,
                sliderInput(inputId = paste(tabname, "dt", sep = "_"), "Select day:",
                            min = strptime("2020/12/06","%Y/%m/%d"), 
                            max = strptime("2021/02/14","%Y/%m/%d"),
                            value = strptime("2021/02/07","%Y/%m/%d"),
                            timeFormat = "%Y/%m/%d",
                            step = as.difftime(7, units = "days"),
                            animate = animationOptions(interval = 2000)))
          ),
          fluidRow(
            box(width = 12,
                leafletOutput(paste(tabname, "map", sep = "_"), height = mapheight)
            )
          ),
          fluidRow(
            box(width = 3,
                radioGroupButtons(inputId = paste(tabname, "source", sep = "_"),
                                  "Choose Data:", 
                                  c("AQI" = "aqi", 
                                    "PM2.5" = "pm25"),
                                  selected = "pm25"))
          ))
}

### Converts column to HTML labels; maps NA values to their date of last update
getLabels <- function(date, dataframe, varname) {
  col.format <- paste(varname, "%Y%m%d", sep = "_")
  idx <- which(colnames(dataframe) == format(date, col.format))
  col <- dataframe[, idx]
  labels <- sprintf(as.character(round(col, 3)))
  na.idx <- which(is.na(col))
  search <- dataframe[na.idx, idx:ncol(dataframe)] # slice of dataframe to search for last update
  # week is shifted back here, to be consistent with the dataproc
  # note: max.col doesn't work if there is no last update available; this does not happen currently
  update <- strptime(names(search)[max.col(!is.na(search), "first")], col.format) - days(6) 
  update[update == date - days(6)] <- NA # if update == today, no last update exists
  repl <- paste("NA<br>Last Updated:", # Create HTML strings for last updates
                 update, 
                 sep = " ") 
  labels[na.idx] <- repl
  labels %>% lapply(htmltools::HTML)
}


