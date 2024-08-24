library(devtools)
install_github("mikeasilva/blsAPI")
install.packages("tidyjson")
library(blsAPI)
library(jsonlite)
library(tidyverse)
library(tidyjson)

worldbank %>% gather_object %>% json_types

response <- blsAPI('LAUCN040010000000005')
response %>% spread_all()

json <- jsonlite::fromJSON(response) 
json %>% purrr::pluck("Results")
json %>% str() 

## Multiple Series 
payload <- list('seriesid'=c('LAUCN040010000000005','LAUCN040010000000006')) 
response <- blsAPI(payload)