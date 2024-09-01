####
# Develop code to pull data from FRED API
####

#install packages and libraries 
install.packages(c('httr', 'jsonlite', 'dplyr', 'renv'))

library('httr')
library('jsonlite')
library('dplyr')

#specify the metric, MSA, and table ID (from FRED) for the table to pull 
import_table_names = list(
  c(metric = 'CPI',     msa = 'ATL',   lookup_id = 'CUURA319SA0'), 
  c(metric = 'CPI',     msa = 'BAL',   lookup_id = 'CUURA311SA0'),
  c(metric = 'CPI',     msa = 'BOS',   lookup_id = 'CUURA103SA0'), 
  #c(metric = 'CPI',     msa = 'CHA',   lookup_id = ''),
  c(metric = 'CPI',     msa = 'CHI',   lookup_id = 'CUURA207SA0'), 
  c(metric = 'CPI',     msa = 'DAL',   lookup_id = 'CUURA316SA0'), 
  c(metric = 'CPI',     msa = 'DEN',   lookup_id = 'CUUSA433SA0S'), 
  c(metric = 'CPI',     msa = 'DET',   lookup_id = 'CUURA208SA0'), 
  c(metric = 'CPI',     msa = 'HOU',   lookup_id = 'CUURA318SA0'), 
  c(metric = 'CPI',     msa = 'LOS',   lookup_id = ''),
  c(metric = 'CPI',     msa = 'MIA',   lookup_id = 'CUURA320SA0'), 
  c(metric = 'CPI',     msa = 'MIN',   lookup_id = 'CUUSA211SA0S'), 
  c(metric = 'CPI',     msa = 'NEW',   lookup_id = 'CUURA101SA0'), 
  #c(metric = 'CPI',     msa = 'ORL',   lookup_id = ''),
  c(metric = 'CPI',     msa = 'PHI',   lookup_id = 'CUURA102SA0'), 
  c(metric = 'CPI',     msa = 'PHO',   lookup_id = 'CUUSA429SA0S'), 
  c(metric = 'CPI',     msa = 'POR',   lookup_id = ''),
  c(metric = 'CPI',     msa = 'RIV',   lookup_id = ''),
  #c(metric = 'CPI',     msa = 'SAT',   lookup_id = ''),
  c(metric = 'CPI',     msa = 'SAN',   lookup_id = 'CUUSA424SA0S'), 
  c(metric = 'CPI',     msa = 'SEA',   lookup_id = 'CUURA423SA0'), 
  c(metric = 'CPI',     msa = 'SFO',   lookup_id = 'CUURA422SA0'), 
  c(metric = 'CPI',     msa = 'STL',   lookup_id = 'CUUSA209SA0'), 
  c(metric = 'CPI',     msa = 'TAM',   lookup_id = 'CUUSA321SA0S'), 
  c(metric = 'CPI',     msa = 'WDC',   lookup_id = 'CUURA311SA0'),
  
  # BAL and WDC are combined through 2017. No new data available 
  # RIV and LOS are combined through 2017. No new data available 
  
  c(metric = 'Income',     msa = 'ATL',   lookup_id = 'CUURA319SA0'), 
  c(metric = 'Income',     msa = 'BAL',   lookup_id = 'CUURA311SA0'),
  c(metric = 'Income',     msa = 'BOS',   lookup_id = 'CUURA103SA0'), 
  #c(metric = 'Income',     msa = 'CHA',   lookup_id = ''),
  c(metric = 'Income',     msa = 'CHI',   lookup_id = 'CUURA207SA0'), 
  c(metric = 'Income',     msa = 'DAL',   lookup_id = 'CUURA316SA0'), 
  c(metric = 'Income',     msa = 'DEN',   lookup_id = 'CUUSA433SA0S'), 
  c(metric = 'Income',     msa = 'DET',   lookup_id = 'CUURA208SA0'), 
  c(metric = 'Income',     msa = 'HOU',   lookup_id = 'CUURA318SA0'), 
  c(metric = 'Income',     msa = 'LOS',   lookup_id = ''),
  c(metric = 'Income',     msa = 'MIA',   lookup_id = 'CUURA320SA0'), 
  c(metric = 'Income',     msa = 'MIN',   lookup_id = 'CUUSA211SA0S'), 
  c(metric = 'Income',     msa = 'NEW',   lookup_id = 'CUURA101SA0'), 
  #c(metric = 'Income',     msa = 'ORL',   lookup_id = ''),
  c(metric = 'Income',     msa = 'PHI',   lookup_id = 'CUURA102SA0'), 
  c(metric = 'Income',     msa = 'PHO',   lookup_id = 'CUUSA429SA0S'), 
  c(metric = 'Income',     msa = 'POR',   lookup_id = ''),
  c(metric = 'Income',     msa = 'RIV',   lookup_id = ''),
  #c(metric = 'Income',     msa = 'SAT',   lookup_id = ''),
  c(metric = 'Income',     msa = 'SAN',   lookup_id = 'CUUSA424SA0S'), 
  c(metric = 'Income',     msa = 'SEA',   lookup_id = 'CUURA423SA0'), 
  c(metric = 'Income',     msa = 'SFO',   lookup_id = 'CUURA422SA0'), 
  c(metric = 'Income',     msa = 'STL',   lookup_id = 'CUUSA209SA0'), 
  c(metric = 'Income',     msa = 'TAM',   lookup_id = 'CUUSA321SA0S'), 
  c(metric = 'Income',     msa = 'WDC',   lookup_id = 'CUURA311SA0'),

  
  
  c(metric = 'Income',  msa = 'SFO',   lookup_id = 'SANF806PCPI'), 
  c(metric = 'Income',  msa = 'NEW',   lookup_id = 'NEWY636PCPI'))




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

