### Load Libraries ###############################################################
library(tidyverse)
library(lubridate)
library(tigris)
library(here)
library(sf)
library(arrow)
library(sfarrow)
# library(rmapshaper)

data_file_path = here('app-cpi/app_data/')

### Import Data ###############################################################
# Import fred data 
fred_prelim <- arrow::read_parquet(paste0(data_file_path, "/fred_data_pull.parquet"))

# Import state outline shape 
states_sf <- tigris::states(year = 2010, cb = TRUE) |> 
  tigris::shift_geometry() # Shift AK/HI to be closer to contiguous US map
  # sf::st_cast(states_sf, "MULTIPOLYGON") # w/o cb, need to convert all to same polygons for interactive map
  # rmapshaper::ms_simplify() # simplify maps so plots faster


sfarrow::st_write_parquet(states_sf, paste0(data_file_path, "/states_sf.parquet")) 

# Import CBSA lat/long (note: CBSAs include MSAs)
cbsa_sf_prelim <- tigris::core_based_statistical_areas(year = 2010) |> 
  st_transform(crs = "ESRI:102003")

sfarrow::st_write_parquet(cbsa_sf_prelim, paste0(data_file_path, "/cbsa_sf_prelim.parquet"))


### Clean Data ###############################################################
# CBSAs
cbsa_sf <- cbsa_sf_prelim |> 
  dplyr::mutate(msa = dplyr::case_when(
    NAMELSAD10 |> str_detect('Atlanta') ~ 'ATL',
    NAMELSAD10 |> str_detect('Baltimore') ~ 'BAL',
    NAMELSAD10 |> str_detect('Boston') ~ 'BOS',
    NAMELSAD10 |> str_detect('Chicago') ~ 'CHI',
    NAMELSAD10 |> str_detect('Dallas') ~ 'DAL',
    NAMELSAD10 |> str_detect('Denver') ~ 'DEN', 
    NAMELSAD10 |> str_detect('Detroit') ~ 'DET',
    NAMELSAD10 |> str_detect('Houston') ~ 'HOU',
    NAMELSAD10 |> str_detect('Los Angeles') ~ 'LOS', 
    NAMELSAD10 |> str_detect('Miami') ~ 'MIA',
    NAMELSAD10 |> str_detect('Minneapolis') ~ 'MIN', 
    NAMELSAD10 |> str_detect('New York') ~ 'NEW',
    NAMELSAD10 |> str_detect('Philadelphia') ~ 'PHI',
    NAMELSAD10 |> str_detect('Phoenix') ~ 'PHO',
    NAMELSAD10 |> str_detect('Portland') ~ 'POR',
    NAMELSAD10 |> str_detect('Riverside') ~ 'RIV',
    NAMELSAD10 |> str_detect('San Diego') ~ 'SAN',
    NAMELSAD10 |> str_detect('Seattle') ~ 'SEA',
    NAMELSAD10 |> str_detect('San Francisco') ~ 'SFO',
    NAMELSAD10 |> str_detect('St. Louis') ~ 'STL',
    NAMELSAD10 |> str_detect('Tampa') ~ 'TAM',
    NAMELSAD10 |> str_detect('Washington') ~ 'WDC')) |>
  # remove portland Maine 
  dplyr::filter(LSAD10 == 'M1' & !is.na(msa) & GEOID10 != '38860') |>
  sf::st_centroid() 

cbsa_coord <- cbsa_sf |> 
  st_coordinates() |> 
  as_tibble() |> 
  dplyr::rename(lon = X, lat = Y)

cbsa_df <- bind_cols(cbsa_sf, cbsa_coord) |>
  dplyr::select(msa, lat, lon)

metric_labels = read_csv(paste0(data_file_path, '/metric_labels.csv'), show_col_types = FALSE)  

cbsa_labels = read_csv(paste0(data_file_path, '/cbsa_labels.csv'), show_col_types = FALSE)

# fred
fred_base <- fred_prelim |> 
  dplyr::mutate(year = year(date)) |> 
  dplyr::group_by(msa, metric, year) |> 
  dplyr::summarize(value = median(value)) |> 
  dplyr::ungroup()

# calculate ratios for every metric with year and MSA
fred_ratio <- fred_base |>
  dplyr::left_join(fred_base, by = c('msa', 'year'), relationship = 'many-to-many') |>
  dplyr::filter(metric.x != metric.y) |>
  dplyr::mutate(metric = paste(metric.x, metric.y, sep = '_'),
                value = value.x / value.y) |>
  #will need to add in lat and long values here 
  dplyr::select(msa, metric, year, value) 

# stack ratios on top of main metric dataset to make long dataset with all data 
fred_stack <- fred_base |>
  mutate(ratio = 0) |>
  rbind(fred_ratio |>
          mutate(ratio = 1)) |>
  dplyr::left_join(cbsa_df, by = "msa") |>
  dplyr::left_join(metric_labels, by = 'metric') |>
  dplyr::left_join(cbsa_labels, by = 'msa') |>
  dplyr::select(-geometry)

### Export Files ###############################################################
arrow::write_parquet(fred_stack, paste0(data_file_path, "/fred_data_clean.parquet"))

