# July 22, 2025

library(air2wateR)
library(data.table)
library(dplyr)
library(DT)
library(ggplot2)
library(here)
library(lubridate)
library(plotly)
library(readr)
library(stringr)
library(tidyr)

theme_set(theme_light())

##################### UPDATE THESE ###################################
lake <- "pockwock"
depth_m <- 40
start_val <- 2025

# Copy files from cal folder ----------------------------------------------

cal_path <- here(paste0("output/3_final_cal/final_", depth_m, "_", start_val))
sim_path <- here(paste0("output/4_final_projections/final_", depth_m, "_", start_val))

# create required folders
dir.create(paste0(sim_path, "/pockwock"))
dir.create(paste0(sim_path, "/pockwock/output_2"))

# copy files 
inputs <- list.files(cal_path, pattern = "txt", full.names = TRUE)
outputs <- list.files(paste0(cal_path, "/pockwock/output_2"), full.names = TRUE)

file.copy(inputs, sim_path)
file.copy(outputs, paste0(sim_path, "/pockwock/output_2"))

file.copy(
  paste0(cal_path, "/pockwock/output_2/1_PSO_RMS_8200574_NS01DL0009_c_1d.out"),
  paste0(sim_path, "/parameters_forward.txt")
)

# PROJECTED AIR TEMP ------------------------------------------------------

# read in data 
fread(here("data/rcp85_air_temperature_downscaled.csv")) %>% 
  filter(year_ast > 2025) %>% 
  select(-date_ast) %>% 
  fwrite(
    paste0(sim_path, "/", lake, "/8200574_NS01DL0009_cc.txt"), col.names = FALSE
  )

################# RUN MODEL #######################

# Run the model. Previously calibrated 
run_air2water(sim_folder = sim_path, mode = "forward")

################# FIGURES #######################

out_raw <- read.table(
  paste0(sim_path, "/", lake, "/output_2/2_FORWARD_RMS_8200574_NS01DL0009_cc_1d.out")
) 

out <- out_raw %>% 
  select(
    year = V1, month = V2, day = V3,
    observed_air_temperature = V4,
    observed_water_temperature = V5,
    simulated_water_temperature = V6
  ) %>% 
  filter(month != -999) %>% 
  mutate(
    timestamp_ast = as_date(paste(year, month, day, sep = "-")),
    across(contains("temperature"), \(x) na_if(x, -999)),
    status = "forward"
    ) %>% 
  select(timestamp_ast, contains("temperature"), status)

out_long <- out %>% 
  pivot_longer(
    cols = contains("temperature"), values_to = "value", names_to = "variable")

## Temperature Data
out_long %>% 
  filter(variable == "observed_air_temperature") %>% 
  ggplot(aes(timestamp_ast, value, color = status)) +
  geom_point(size = 0.5) +
  scale_color_manual(values = c("#FFD118", "#22A884")) +
  ylab('Temperature (\u00B0C)') +
  facet_wrap(~variable, ncol = 1, scales = "free") +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(axis.title.x = element_blank())


## Projected Air and Modelled Water Temperature
out_long %>% 
  mutate(
    variable = str_remove(variable, "_temperature"),
    variable = str_replace(variable, "_", " "),
    variable = str_to_title(variable)
  ) %>% 
  ggplot(aes(timestamp_ast, value, col = variable)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    "Temperature", values = c("lightgrey", "#063E4D", "#7AD151")
  ) +
  ylab('Temperature (\u00B0C)') +
  facet_wrap(~status, nrow = 2, scales = "free_x") +
  theme(axis.title.x = element_blank())


