####
# Develop code to pull data from FRED API
####

library('httr')
library('jsonlite')
library('dplyr')
library(rio)
library(arrow)

data_file_path = here('app-cpi/app_data/')

#specify the metric, MSA, and table ID (from FRED) for the table to pull 
import_table_names = read_csv(paste0(data_file_path, '/fred_dataset_lookup.csv'), 
                              show_col_types = FALSE) |>
  filter(!is.na(lookup_id) & lookup_id != '') 

#create an empty data frame 
build_df = data.frame(metric=character(), msa=character(), date=as.Date(character()), 
                      value=numeric())

delay_seconds = 0.7

#loop through all import tables and pull, clean, and stack data

for (i in 1:nrow(import_table_names)) {
  
  curr_time = as.numeric(Sys.time())
  
  metric = pull(import_table_names[i, 1])
  msa = pull(import_table_names[i, 2])
  table_id = pull(import_table_names[i, 3])
  
  print(paste0('Loading: ', metric, ' for ', msa))
  
  #api call for the data 
  api_call = GET(paste0('https://api.stlouisfed.org/fred/series/observations?series_id=', table_id, 
                        '&api_key=', Sys.getenv("FRED_API_KEY"), '&file_type=json'))
  
  #separate the data from metadata in api call 
  pull_data = fromJSON(rawToChar(api_call$content))$observations 
  
  #structure the data for stacking 
  clean_data = pull_data |>
    filter(!is.na(value)) |>
    mutate(metric = metric,
           msa = msa,
           value = as.numeric(value)) |>
    select(metric, msa, date, value) |> 
    filter(!is.na(value))
  
  #stack the data to the base dataframe 
  build_df = build_df |>
    rbind(clean_data) 
  
  while((as.numeric(Sys.time()) - as.numeric(curr_time)) < delay_seconds){}
  
}

#save to data folder 
#write.csv(build_df, paste0(data_file_path, '/fred_data_pull.csv'))

arrow::write_parquet(build_df, paste0(data_file_path, "/fred_data_pull.parquet"))

