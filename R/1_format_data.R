# June 19, 2025

# From the README
# https://github.com/dempsey-CMAR/air2wateR

# The series of observed air temperature must be complete. It cannot have gaps
# or no data. 
# The series of observed water temperature can contain no-data (-999). 

# The series of data must start on the 1st of January. If data are
# available after that date, air temperature should be reconstructed and the
# value -999 assigned to water temperature. 

# Both series are always at daily time scale, as the equation of the model is
# solved with daily time step. The model automatically evaluates weekly,
# multi-weekly, or monthly averages (of water temperature) when using different
# time scales for model calibration.

# Data Sources
## Bedford Range air temperature:  Government of Canada
## https://collaboration.cmc.ec.gc.ca/cmc/climate/Get_More_Data_Plus_de_donnees/

## Pockwock lake water temperature: Surface Water Quality Monitoring Network 
## Continuous Water Quality Data] (https://open.canada.ca/data/en/dataset/6ac5555e-7df4-6201-8462-3a2d5c4f1721)
 
## RCP 8.5 air temperature


library(data.table)
library(dplyr)
library(ggplot2)
library(here)
library(lubridate)
library(purrr)
library(readr)
library(tidyr)

theme_set(theme_light())

# Bedford Range Meteorology Data ------------------------------------------

air_raw <- map_df(
  .x = list.files(here("data-raw/bedford_range"), full.names = TRUE), 
  .f = fread, fill = TRUE, data.table = FALSE
) 

air <- air_raw %>% 
  select(
    station = `Station Name`,
    timestamp_ast = `Date/Time (LST)`,
    year_ast = Year,
    month_ast = Month,
    day_ast = Day,
    air_temperature_degree_c = `Temp (°C)`,
    relative_humidity_percent = `Rel Hum (%)`,
    wind_speed_km_per_hour = `Wind Spd (km/h)`,
  ) %>% 
  mutate(
    timestamp_ast = if_else(
      nchar(timestamp_ast) == 16, paste0(timestamp_ast, ":00"), timestamp_ast),
    timestamp_ast = as_datetime(timestamp_ast)
  ) %>% 
  pivot_longer(
    cols = air_temperature_degree_c:wind_speed_km_per_hour, 
    values_to = "value", names_to = "variable"
  ) %>% 
  filter(!is.na(value)) 

# Pockwock Water Temperature data -----------------------------------------------------------
water_raw <- fread(
  here("data-raw/Surface_Water_Quality_Monitoring_Network_Continuous_Water_Quality_Data.csv"),
  data.table = FALSE
) 

water <- water_raw %>% 
  filter(Station_Number == "NS01EH0050") %>% 
  select(
    Date, Time,
    water_temperature_degree_c = contains("Temperature")
  ) %>% 
  mutate(
    station = "Pockwock Lake",
    Date = parse_date(Date, format = "%Y/%m/%d"),
    Time = if_else(nchar(Time) == 5, paste0(Time, ":00"), Time),
    timestamp_ast = as_datetime(paste(Date, Time)),
    year_ast = year(timestamp_ast),
    month_ast = month(timestamp_ast),
    day_ast = day(timestamp_ast)
  ) %>% 
  select(-c(Date, Time)) %>% 
  pivot_longer(
    cols = water_temperature_degree_c,
    values_to = "value", names_to = "variable"
  ) %>% 
  filter(!is.na(value)) 

# Merge & Calculate Daily values --------------------------------------------------------

# to identify gaps in air temperature data
air_ts <- data.frame(
  date_ast =  seq(
    min(as_date(air$timestamp_ast)), max(as_date(air$timestamp_ast)), 
    by = "1 day"
  ))

dat <- air %>%
  bind_rows(water) %>% 
  group_by(year_ast, month_ast, day_ast, variable) %>% 
  summarise(daily_mean = round(mean(value), digits = 1)) %>% 
  ungroup() %>% 
  pivot_wider(
    values_from = "daily_mean", names_from = "variable", 
    names_prefix = "daily_mean_"
  ) %>% 
  mutate(
    date_ast = as_date(paste(year_ast, month_ast, day_ast, sep = "-")),
  ) %>%
  select(date_ast, year_ast, month_ast, day_ast, everything()) %>% 
  right_join(air_ts, by = join_by(date_ast)) 

dat %>% 
  pivot_longer(
    cols = contains("daily_mean"), values_to = "value", names_to = "variable"
  ) %>% 
  ggplot(aes(date_ast, value)) +
  geom_point() +
  facet_wrap(~variable, ncol = 1, scales = "free")

# impute missing data -----------------------------------------------------
# only imputing missing 2018 data for now to get continuous 
# series from 2015 - 2024
# could fill other gaps for data from 2005 - 2024

# days without air temperature
air_na <- dat %>% 
  filter(is.na(daily_mean_air_temperature_degree_c)) %>% 
  select(date_ast) %>% 
  mutate(diff_day = as.numeric(
    difftime(lead(date_ast), date_ast, units = "day"))
  ) 

# missing data for Jan 24, 2018 - Feb 11, 2018
# dat %>%
#   filter(month_ast %in% c(1, 2), year_ast %in% c(2016:2020)) %>%
#   mutate(
#     fake_date = as_date(paste("2020", month_ast, day_ast, sep = "-"))
#   ) %>%
#   ggplot(
#     aes(fake_date, daily_mean_air_temperature_degree_c,
#         col = factor(year_ast))) +
#   geom_line(linewidth = 1.5)

# impute data for missing days using mean of previous 2 years 
# and following 2 years for 
imp_2018 <- dat %>% 
  filter(
    year_ast %in% c(2016, 2017, 2019, 2020), 
    (month_ast == 1 & day_ast %in% 24:31) |
      (month_ast == 2 & day_ast %in% 1:11),
  ) %>% 
  pivot_longer(
    cols = contains("daily_mean"), values_to = "value", names_to = "variable"
  ) %>% 
  group_by(month_ast, day_ast, variable) %>% 
  summarise(daily_mean = round(mean(value), digits = 1)) %>% 
  ungroup() %>% 
  mutate(
    year_ast = 2018,
    date_ast = as_date(paste(year_ast, month_ast, day_ast, sep = "-"))
  ) %>% 
  pivot_wider(values_from = "daily_mean", names_from = "variable") 
  

# format for air2wateR package --------------------------------------------

dat_out <- dat %>% 
  filter(
    date_ast >= as_date("2015-01-01"), # consistent data starts in 2015
    !is.na(daily_mean_air_temperature_degree_c)
  ) %>% 
  bind_rows(imp_2018) %>% 
  # na value required by package
  mutate(
    daily_mean_water_temperature_degree_c = if_else(
      is.na(daily_mean_water_temperature_degree_c), -999,
      daily_mean_water_temperature_degree_c),
    
    file_type = if_else(
      date_ast < as_date("2022-01-01"), "calibration", "validation")
  ) %>% 
  select(
    file_type,
    date_ast, year_ast, month_ast, day_ast, 
    daily_mean_air_temperature_degree_c,
    daily_mean_water_temperature_degree_c
  )

dat_out %>% 
  pivot_longer(
    cols = contains("daily_mean"), values_to = "value", names_to = "variable"
  ) %>% 
  ggplot(aes(date_ast, value, col = file_type)) +
  geom_point() +
  facet_wrap(~variable, ncol = 1, scales = "free")

# export as csv file ------------------------------------------------------

# dat_out %>% 
#   select(-file_type) %>% 
#   fwrite(here("data/8200574_NS01DL0009_data.csv"))


# Downscale projected air temperature ---------------------------------------
rm(air, air_na, air_raw, air_ts, dat, imp_2018, water, water_raw)

# rcp_raw <- fread(
#   here("data-raw/RCP85_airtemperature_data.csv"), data.table = FALSE
# )
# use rcp data from cell closer to pockwock
rcp_raw <- fread(
  here("data-raw/pockwock_air_temperature_RCP85.csv"), data.table = FALSE
)

rcp <- rcp_raw %>% 
  mutate(date_ast = as_date(paste(year, month, day, sep = "-"))) %>% 
  select(
    date_ast, year_ast = year, month_ast = month, day_ast = day,
    rcp_air_temperature = air_temperature
  ) 

ggplot(rcp, aes(date_ast, rcp_air_temperature)) +
  geom_point()

# note: 2025 dataset not complete for observed air temperature
unique(dat_out$year_ast)[which(unique(dat_out$year_ast) %in% unique(rcp$year))]

overlap <- c(2015:2024)

# T_a^h
observed_air_climatology <- dat_out %>% 
  filter(year_ast %in% overlap) %>% 
  group_by(month_ast, day_ast) %>% 
  summarise(observed_climatology = mean(daily_mean_air_temperature_degree_c)) %>% 
  ungroup() 

# T_a,mod^h
modelled_air_climatology <- rcp %>% 
  filter(year_ast %in% overlap) %>% 
  group_by(month_ast, day_ast) %>% 
  summarise(modelled_climatology = mean(rcp_air_temperature)) %>% 
  ungroup() 

# projection
air_temp_proj <- rcp %>% 
  left_join(observed_air_climatology, by = join_by(month_ast, day_ast)) %>% 
  left_join(modelled_air_climatology, by = join_by(month_ast, day_ast)) %>% 
  mutate(
    rcp_air_temperature_downscale = observed_climatology + 
      (rcp_air_temperature - modelled_climatology)
  )

air_temp_proj %>% 
  pivot_longer(cols = contains("rcp"), names_to = "variable") %>% 
  ggplot(aes(date_ast, value, col = variable)) +
  geom_point()

# Export Downscaled Data --------------------------------------------------

air_temp_proj %>% 
  mutate(water_temperature_degree_c = -999) %>% 
  select(
    date_ast, year_ast, month_ast, day_ast, 
    rcp_air_temperature_downscale,
    water_temperature_degree_c
  ) %>% 
  fwrite(here("data/rcp85_air_temperature_downscaled.csv"))

