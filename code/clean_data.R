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

# Clean data
fred_base <- fred_prelim |> 
  dplyr::select(-V1) |> 
  dplyr::mutate(year = year(date)) |> 
  dplyr::group_by(msa, metric, year) |> 
  dplyr::summarize(value = median(value)) |> 
  dplyr::ungroup() |> 
  dplyr::left_join(metric_labels, by = 'metric') |>
  dplyr::left_join(msa_labels, by = 'msa') |>
  dplyr::left_join(cbsa_df, by = "msa")

# calculate ratios for every metric with year and MSA
fred_ratio = fred_base |>
  left_join(fred_base, by = c('msa', 'year'), relationship = 'many-to-many') |>
  filter(metric.x != metric.y) |>
  mutate(metric = paste(metric.x, metric.y, sep = '_'),
         value = value.x / value.y) |>
  #will need to add in lat and long values here 
  select(msa, metric, year, value) |>
  mutate(metric_label_short = '',
         metric_label_long = '',
         msa_label_short = '', 
         msa_label_long = '')

# stack ratios on top of main metric dataset to make long dataset with all data 
fred_stack = fred_base |>
  mutate(ratio = 0) |>
  rbind(fred_ratio |>
          mutate(ratio = 1))

write.csv(fred_stack, paste0(file_path, 'data/fred_data_clean.csv'))

