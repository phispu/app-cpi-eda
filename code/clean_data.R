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
  dplyr::filter(
    LSAD10 == 'M1' & !is.na(msa) 
    # remove portland Maine 
      & GEOID10 != '38860') |>
  sf::st_centroid() 

cbsa_coord <- cbsa_sf |> st_coordinates() |> as_tibble() |> 
  dplyr::rename(lon = X, lat = Y)

cbsa_df <- bind_cols(cbsa_sf, cbsa_coord) |>
  dplyr::select(msa, lat, lon)

metric_labels = data.frame(
  metric = c(
    'cpi_all', 'personal_income', 
    'electric_kwh', 'gasoline_unleaded', 
    'home_list_price', 'unemploy_rate', 'pop_resident', 
    'cpi_rent', 'min_wage_state', 
    'new_housing'
  ),
  metric_label_short = c(
    'CPI: All Items', 'Income',
    'Electricity', 'Gasoline', 
    'Home Price', 'Unemployment', 'Population',
    'CPI: Rent', 'Minimum Wage', 
    'New Housing'
  ),
  metric_label_long = c(
    'Consumer Price Index: All Items', 'Per Capita Personal Income', 
    'Electriciy, Average Price per kWh', 'Unleaded Gasoline, Average Price per Gallon', 
    'Average Home Listing Price', 'Unemployment Rate', 'Resident Population, Thousands of Persons', 
    'Consumer Price Index: Rent', 'State Minimum Wage, Dollars per Hour', 
    'Permits for New Private Housing Structures'
  )
)

msa_labels = data.frame(
  msa = c(
    'ATL', 'BAL', 
    'BOS', 'CHI', 
    'DAL', 'DEN', 'DET', 
    'HOU', 'LOS',
    'MIA', 'MIN', 
    'NEW', 'PHI', 
    'PHO', 'POR', 'RIV', 
    'SAN', 'SEA', 'SFO', 
    'STL', 'TAM', 
    'WDC'
  ),
  msa_label_short = c(
    'Atlanta, GA', 'Baltimore, MD', 
    'Boston, MA', 'Chicago, IL', 
    'Dallas, TX', 'Denver, CO', 'Detroit, MI',
    'Houston, TX', 'Los Angeles, CA', 
    'Miami, FL', 'Minneapolis, MN',
    'New York, NY', 'Philadelphia, PA',
    'Phoenix, AZ', 'Portland, OR', 'Riverside, CA', 
    'San Diego, CA', 'Seattle, WA', 'San Francisco, CA', 
    'St. Louis, MO', 'Tampa, FL', 
    'Washington, DC'
  ),
  msa_label_long = c(
    'Atlanta-Sandy Springs-Roswell, GA', 'Baltimore-Columbia-Towson, MD', 
    'Boston-Cambridge-Newton, MA-NH', 'Chicago-Naperville-Elgin, IL', 
    'Dallas-Fort Worth-Arlington, TX', 'Denver-Aurora-Lakewood, CO', 'Detroit-Warren-Dearborn, MI',
    'Houston-The Woodlands-Sugar Land, TX', 'Los Angeles-Long Beach-Anaheim, CA', 
    'Miami-Fort Lauderdale-West Palm Beach, FL', 'Minneapolis-St. Paul-Bloomington, MN-WI', 
    'New York-Newark-Jersey City, NY-NJ-PA', 'Philadelphia-Camden-Wilmington, PA-NJ-DE-MD', 
    'Phoenix-Mesa-Scottsdale, AZ', 'Portland-Salem, OR-WA', 'Riverside-San Bernardino-Ontario, CA',
    'San Diego-Carlsbad, CA', 'Seattle-Tacoma-Bellevue, WA', 'San Francisco-Oakland-Hayward, CA', 
    'St. Louis, MO-IL', 'Tampa-St. Petersburg-Clearwater, FL', 
    'Washington-Arlington-Alexandria, DC-VA-MD-WV'
  )
)


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
  dplyr::left_join(msa_labels, by = 'msa') |>
  dplyr::left_join(cbsa_df, by = "msa")

### Export Files ###############################################################
write_csv(fred_stack, here("data/fred_data_clean.csv"))
write_csv(cbsa_df, here("data/cbsa_df.csv"))
sfarrow::st_write_parquet(cbsa_sf_prelim, here("data/cbsa_sf_prelim.parquet"))
sfarrow::st_write_parquet(states_sf, here("data/states_sf.parquet")) # parquet writes/reads faster than RDS
