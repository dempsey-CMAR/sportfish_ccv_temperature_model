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
 

library(data.table)
library(dplyr)
library(ggplot2)
library(here)
library(lubridate)
library(purrr)
library(readr)
library(tidyr)

theme_set(theme_light())

# solar radiation ---------------------------------------------------------

monthly_solar_rad <- data.frame(
  month_ast = c(1:12),
  solar_radiation = c(1.41, 2.26, 3.26, 4.02, 5.00, 5.59, 
                      5.56, 4.98, 3.76, 2.42, 1.41, 1.09)
)

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
  right_join(air_ts, by = join_by(date_ast)) %>% 
  left_join(monthly_solar_rad, by = "month_ast") %>% 
  rename(daily_mean_solar_radiation = solar_radiation)

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

dat_out %>% 
  select(-file_type) %>% 
  fwrite(
    here("data/8200574_NS01DL0009_data.csv")
  )


# Export calibration and validation as txt files ------------------------------------------------------

# Bedford Range station id: 8200574
# Pockwock station id: NS01DL0009

# cc file
dat_out %>% 
  filter(file_type == "calibration") %>% 
  select(-c(file_type, date_ast)) %>% 
  fwrite(
    here("simulation/pockwock/8200574_NS01DL0009_cc.txt"), 
    col.names = FALSE
  )

# cv file
dat_out %>% 
  filter(file_type == "validation") %>% 
  select(-c(file_type, date_ast)) %>% 
  fwrite(
    here("simulation/pockwock/8200574_NS01DL0009_cv.txt"), 
    col.names = FALSE
  )


# input file --------------------------------------------------------------

# start_date <- as.character(min(dat_out$date_ast, na.rm = TRUE))
# end_date <- as.character(max(dat_out$date_ast, na.rm = TRUE))
# 
# sim_folder <- here("simulation_folder")
# 
# input_txt_path <- file.path(sim_folder, "input.txt")
# input_lines <- c("input_data/input_data.csv", start_date, end_date, "10")
# writeLines(input_lines, con = input_txt_path, sep = "\n")



