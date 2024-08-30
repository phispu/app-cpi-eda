# Load libraries
library(rio)
library(tidyverse)
library(lubridate)
library(tigris)

# Import data

file_path = '~/app-cpi-eda/'

# Import fred data:
fred_prelim <- import(paste0(file_path, "data/fred_data_pull.csv")) |>
  as_tibble()

# Import/prep spatial data:
states_sf <- st_read(paste0(file_path, "data/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")) |> 
  # Figure out code to add inset for these states
  dplyr::filter(!NAME %in% c("Alaska", "Hawaii","Puerto Rico")) 

cbsa_sf_prelim <- core_based_statistical_areas()

cbsa_sf <- cbsa_sf_prelim |> 
  dplyr::filter(str_detect(NAMELSAD, "(San Francisco)|(New York)")) |>
  st_centroid() 

cbsa_coord <- cbsa_sf |> st_coordinates() |> as_tibble() |> 
  dplyr::rename(lon = X, lat = Y)

cbsa_name <- cbsa_sf |> dplyr::pull(NAMELSAD) |> 
  as_tibble_col(column_name = "msa") |> 
  dplyr::mutate(msa = dplyr::case_when(
    msa |> str_detect("New York") ~ "NY",
    msa |> str_detect("San Francisco") ~ "SF"
  ))

cbsa_df <- bind_cols(cbsa_name, cbsa_coord) |> 
  dplyr::mutate(msa)

# Clean data
fred_base <- fred_prelim |> 
  dplyr::select(-V1) |> 
  dplyr::mutate(year = year(date)) |> 
  dplyr::group_by(msa, metric, year) |> 
  dplyr::summarize(value = median(value)) |> 
  dplyr::ungroup() |> 
  dplyr::left_join(cbsa_df, by = "msa")

# calculate ratios for every metric with year and MSA
fred_ratio = fred_base |>
  left_join(fred_base, by = c('msa', 'year'), relationship = 'many-to-many') |>
  filter(metric.x != metric.y) |>
  mutate(metric = paste(metric.x, metric.y, sep = '_'),
         value = value.x / value.y) |>
  #will need to add in lat and long values here 
  select(msa, metric, year, value) 

# stack ratios on top of main metric dataset to make long dataset with all data 
fred_stack = fred_base |>
  mutate(ratio = 0) |>
  rbind(fred_ratio |>
          mutate(ratio = 1))

write.csv(fred_stack, paste0(file_path, 'data/fred_data_clean.csv'))

