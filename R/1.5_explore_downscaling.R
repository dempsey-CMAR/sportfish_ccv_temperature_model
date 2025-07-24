# July 23, 2025

# Explore downscaling 
# Following Piccolroaz et et 2021
# T_adjusted = T_climatology_observed + (T_modelled - T_climatology_modelled)

library(data.table)
library(dplyr)
library(ggplot2)
library(here)
library(lubridate)
library(plotly)
library(tidyr)

theme_set(theme_light())

air_obs <- fread(here("data/8200574_NS01DL0009_data.csv"), data.table = FALSE)

# rcp_raw <- fread(
#   here("data-raw/RCP85_airtemperature_data.csv"), data.table = FALSE
# )

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
unique(air_obs$year_ast)[which(unique(air_obs$year_ast) %in% unique(rcp$year))]

overlap <- c(2015:2024)

# By Month + Day ----------------------------------------------------------

# T_a^h
observed_air_climatology <- air_obs %>% 
  filter(year_ast %in% overlap) %>% 
  group_by(month_ast, day_ast) %>% 
  summarise(observed_climatology = mean(daily_mean_air_temperature_degree_c)) %>% 
  ungroup() %>% 
  mutate(date_holder = as_date(paste("2024", month_ast, day_ast, sep = "-")))
 
# T_a,mod^h
modelled_air_climatology <- rcp %>% 
  filter(year_ast %in% overlap) %>% 
  group_by(month_ast, day_ast) %>% 
  summarise(modelled_climatology = mean(rcp_air_temperature)) %>% 
  ungroup() %>% 
  mutate(date_holder = as_date(paste("2024", month_ast, day_ast, sep = "-")))

# combine
climatology <- observed_air_climatology %>% 
  full_join(
    modelled_air_climatology, by = join_by(month_ast, day_ast, date_holder)
 )

climatology %>% 
  pivot_longer(cols = contains("climatology"), names_to = "variable") %>% 
  ggplot(aes(date_holder, value, col = variable)) +
  scale_x_date(date_labels = "%B-%d") +
  geom_point()


air_temp_proj <- rcp %>% 
  left_join(climatology, by = join_by(month_ast, day_ast)) %>% 
  select(-date_holder) %>% 
  mutate(
    diff = rcp_air_temperature - modelled_climatology,
    rcp_air_temperature_downscale = observed_climatology + diff
  )

ggplot(air_temp_proj, aes(date_ast, diff)) +
  geom_point()


air_temp_proj %>% 
  pivot_longer(cols = contains("rcp"), names_to = "variable") %>% 
  ggplot(aes(date_ast, value, col = variable)) +
  geom_point()

p <- air_temp_proj %>% 
  filter(year_ast %in% c(2025:2030)) %>% 
  select(-diff) %>% 
  pivot_longer(
    rcp_air_temperature:rcp_air_temperature_downscale,
    names_to = "air_temperature_series"
  ) %>% 
  ggplot(aes(date_ast, value, colour = air_temperature_series)) +
  geom_point() 
 
ggplotly(p)

p <- air_temp_proj %>% 
  filter(year_ast %in% c(2050:2055)) %>% 
  select(-diff) %>% 
  pivot_longer(
    rcp_air_temperature:rcp_air_temperature_downscale,
    names_to = "air_temperature_series"
  ) %>% 
  ggplot(aes(date_ast, value, colour = air_temperature_series)) +
  geom_point() 

ggplotly(p)


# check
x <- air_temp_proj %>% 
  filter(year_ast %in% overlap) %>% 
  group_by(month_ast, day_ast, observed_climatology, modelled_climatology) %>% 
  summarise(
    downscaled_climatology = mean(rcp_air_temperature_downscale)
  ) %>% 
  ungroup()

p <- x %>% 
  pivot_longer(cols = contains("climatology"), names_to = "variable") %>% 
  mutate(date_holder = as_date(paste("2024", month_ast, day_ast, sep = "-"))) %>% 
  ggplot(aes(date_holder, value, col = variable)) +
  scale_x_date(date_labels = "%B-%d") +
  geom_point()


ggplotly(p)


# By MONTH ----------------------------------------------------------------

# T_a^h
observed_air_climatology_month <- air_obs %>% 
  filter(year_ast %in% overlap) %>% 
  group_by(month_ast) %>% 
  summarise(observed_climatology = mean(daily_mean_air_temperature_degree_c)) %>% 
  ungroup() %>% 
  mutate(date_holder = as_date(paste("2024", month_ast, "01", sep = "-")))

# T_a,mod^h
modelled_air_climatology_month <- rcp %>% 
  filter(year_ast %in% overlap) %>% 
  group_by(month_ast) %>% 
  summarise(modelled_climatology = mean(rcp_air_temperature)) %>% 
  ungroup() %>% 
  mutate(date_holder = as_date(paste("2024", month_ast, "01", sep = "-")))

# combine
climatology_month <- observed_air_climatology_month %>% 
  full_join(
    modelled_air_climatology_month, by = join_by(month_ast, date_holder)
  )

climatology_month %>% 
  pivot_longer(cols = contains("climatology"), names_to = "variable") %>% 
  ggplot(aes(date_holder, value, col = variable)) +
  scale_x_date(date_labels = "%B-%d") +
  geom_point()


air_temp_proj_month <- rcp %>% 
  left_join(climatology_month, by = join_by(month_ast)) %>% 
  select(-date_holder) %>% 
  mutate(
    diff = rcp_air_temperature - modelled_climatology,
    rcp_air_temperature_downscale = observed_climatology + diff
  )

ggplot(air_temp_proj_month, aes(date_ast, diff)) +
  geom_point()

air_temp_proj_month %>% 
  pivot_longer(cols = contains("rcp"), names_to = "variable") %>% 
  ggplot(aes(date_ast, value, col = variable)) +
  geom_point()

p <- air_temp_proj_month %>% 
  filter(year_ast %in% c(2025:2030)) %>% 
  select(-diff) %>% 
  pivot_longer(
    rcp_air_temperature:rcp_air_temperature_downscale,
    names_to = "air_temperature_series"
  ) %>% 
  ggplot(aes(date_ast, value, colour = air_temperature_series)) +
  geom_point() 

ggplotly(p)

p <- air_temp_proj_month %>% 
  filter(year_ast %in% c(2050:2055)) %>% 
  select(-diff) %>% 
  pivot_longer(
    rcp_air_temperature:rcp_air_temperature_downscale,
    names_to = "air_temperature_series"
  ) %>% 
  ggplot(aes(date_ast, value, colour = air_temperature_series)) +
  geom_point() 

ggplotly(p)

# check
x_month <- air_temp_proj_month %>% 
  filter(year_ast %in% overlap) %>% 
  group_by(month_ast, day_ast, observed_climatology, modelled_climatology) %>% 
  summarise(
    downscaled_climatology = mean(rcp_air_temperature_downscale)
  ) %>% 
  ungroup()

p <- x_month %>% 
  pivot_longer(cols = contains("climatology"), names_to = "variable") %>% 
  mutate(date_holder = as_date(paste("2024", month_ast, day_ast, sep = "-"))) %>% 
  ggplot(aes(date_holder, value, col = variable)) +
  scale_x_date(date_labels = "%B-%d") +
  geom_point()

ggplotly(p)


# Compare -----------------------------------------------------------------

all.equal(air_temp_proj$rcp_air_temperature_downscale, 
          air_temp_proj_month$rcp_air_temperature_downscale)


ggplot(air_temp_proj, aes(date_ast, rcp_air_temperature_downscale)) +
  geom_point(col = 1) +
  geom_point(
    data = air_temp_proj_month, 
    aes(date_ast, rcp_air_temperature_downscale), 
    col = "purple", alpha = 0.75
  )




