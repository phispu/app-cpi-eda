####
# Develop code to pull data from FRED API
####

#install packages and libraries 
install.packages(c('httr', 'jsonlite', 'dplyr'))

library('httr')
library('jsonlite')
library('dplyr')

#specify the metric, MSA, and table ID (from FRED) for the table to pull 
import_table_names = list(
  c(metric = 'CPI',     msa = 'SF',   lookup_id = 'CUURA422SA0'),
  c(metric = 'CPI',     msa = 'NY',   lookup_id = 'CUURA101SA0'),
  c(metric = 'Income',  msa = 'SF',   lookup_id = 'SANF806PCPI'), 
  c(metric = 'Income',  msa = 'NY',   lookup_id = 'NEWY636PCPI'))

#create an empty data frame 
build_df = data.frame(metric=character(), msa=character(), date=as.Date(character()), 
                      value=numeric())

#loop through all import tables and pull, clean, and stack data 
for (item in import_table_names) {
  
  #api call for the data 
  api_call = GET(paste0('https://api.stlouisfed.org/fred/series/observations?series_id=', item[[3]], 
                        '&api_key=', Sys.getenv("FRED_API_KEY"), '&file_type=json'))
  
  #separate the data from metadata in api call 
  pull_data = fromJSON(rawToChar(api_call$content))$observations 
  
  #structure the data for stacking 
  clean_data = pull_data |>
    mutate(metric = item[[1]],
           msa = item[[2]],
           value = as.numeric(value)) |>
    select(metric, msa, date, value) |> 
    filter(!is.na(value))

  #stack the data to the base dataframe 
  build_df = build_df |>
    rbind(clean_data) 

}

#save to data folder 
write.csv(build_df, '~/app-cpi-eda/data/fred_data_pull.csv')
