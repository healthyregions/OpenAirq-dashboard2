---
  title: 'Homework 8: The New York Times'' Most Popular Articles'
author: "Angel Aliseda Alonso"
date: "November 26th, 2019"
output:
  pdf_document: default
html_document: default
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
library(tidyverse)
library(httr)
library(stringr)
library(jsonlite)
library(lubridate)
library(knitr)
nyt_api_key <- getOption("nyt_api_key")
```

## Introduction

```{r functions_import, include=FALSE}
# Function to create the URL
# Key: API key from NYT Developers website
# Type: takes values "emailed", "shared" or "viewed"
# Period: takes values 1, 7, 30
nyt_popular_url <- function(key, type, period){
  baseurl <-  "https://api.nytimes.com/svc/mostpopular/v2/"
  
  if_else(type %in% c("emailed", "shared", "viewed"), 
          if_else(period %in% c(1, 7, 30),
                  return(str_c(baseurl, type, "/", period, ".json?api-key=", key)),
                  stop("Provide a valid period")),
          stop("Provide a valid type"))
}


# Function to Get the info and tidy the dataframe
nyt_popular_df <- function(url) {
  
  json_response <- GET(url) #Gets the query using the url previously obtained
  final_df <- json_response %>%   # Parses the json response into a tidy dataframe
    content(as ="parsed", type = "application/json") %>% 
    as_tibble() %>% 
    select(results) %>% 
    hoist(results, 
          section = "section",
          byline = "byline",
          type = "type",
          title = "title",
          published_date = "published_date",
          org_facet = "org_facet",
          source = "source",
          abstract = "abstract",
          per_facet = "per_facet",
          des_facet = "des_facet")
  
  return(final_df)
} 


```
