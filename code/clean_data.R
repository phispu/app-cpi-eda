### Load Libraries ###############################################################
library(tidyverse)
library(lubridate)
library(tigris)
library(here)
library(sf)
library(arrow)
library(sfarrow)
library(rmapshaper)

### Import Data ###############################################################
# fred data:
fred_prelim <- read_csv(here("data/fred_data_pull.csv")) |>
  tibble::as_tibble() |> 
  dplyr::select(-1) # First column only has row numbers

states_sf <- tigris::states(year = 2010) |> 
  tigris::shift_geometry() |> # Shift AK/HI to be closer to contiguous US map
  rmapshaper::ms_simplify() # simplify maps so plots faster


# Spatial: CBSAs (note: CBSAs include MSAs)
  # Previously downloaded: cbsa_sf_prelim <- tigris::core_based_statistical_areas(year = 2010) |> st_transform(crs = "ESRI:102003")
cbsa_sf_prelim <- sfarrow::st_read_parquet(here("data/cbsa_sf_prelim.parquet"))
### Clean Data ###############################################################
# CBSAs
cbsa_sf <- cbsa_sf_prelim |> 
  dplyr::filter(str_detect(NAMELSAD10, "(San Francisco)|(New York)")) |>
  sf::st_centroid() 
cbsa_coord <- cbsa_sf |> st_coordinates() |> as_tibble() |> 
  dplyr::rename(lon = X, lat = Y)
cbsa_name <- cbsa_sf |> dplyr::pull(NAMELSAD10) |> 
  as_tibble_col(column_name = "msa") |> 
  dplyr::mutate(msa = dplyr::case_when(
    msa |> str_detect("New York") ~ "NY",
    msa |> str_detect("San Francisco") ~ "SF"
  ))
cbsa_df <- bind_cols(cbsa_name, cbsa_coord) |> 
  dplyr::mutate(msa)
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
  dplyr::left_join(cbsa_df, by = "msa")

### Export Files ###############################################################
write_csv(fred_stack, here("data/fred_data_clean.csv"))
write_csv(cbsa_df, here("data/cbsa_df.csv"))
sfarrow::st_write_parquet(cbsa_sf_prelim, here("data/cbsa_sf_prelim.parquet"))
sfarrow::st_write_parquet(states_sf, here("data/states_sf.parquet")) # parquet writes/reads faster than RDS
