# July 23, 2025

# STEP 3: calibrate the model with all data for best fit

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

source(here("functions/generate_figures.R"))
source(here("functions/generate_tables.R"))
source(here("functions/extract_calibration_output.R"))

theme_set(theme_light())

##################### UPDATE THESE ###################################
lake <- "pockwock"
depth_m <- 24
start_val <- 2020 # this is for file naming purposes only

##################### FILE PATHS ###################################
sim_path <- here("sim_folder")
cal_path <- paste0(sim_path, "/3_cal/cal_", depth_m, "_", start_val)
proj_path <- paste0(sim_path, "/4_proj/proj_", depth_m, "_", start_val)
lake_path <- paste(proj_path, lake, sep = "/")
output_path <- paste0(lake_path, "/output_2")
fig_path <- paste0(proj_path, "/figures")

##################### CREATE FOLDERS & COPY INPUT FILES (if needed) ###########################
inputs <- list.files(cal_path, pattern = "txt", full.names = TRUE)
outputs <- list.files(paste0(cal_path, "/pockwock/output_2"), full.names = TRUE)

dir.create(proj_path)
dir.create(lake_path)
dir.create(output_path)
dir.create(fig_path)

file.copy(inputs, proj_path)
file.copy(outputs, paste0(lake_path, "/output_2/"))
file.copy(outputs, output_path)

# PROJECTED AIR TEMP ------------------------------------------------------

# read in data 
fread(here("data/rcp85_air_temperature_downscaled.csv")) %>% 
  filter(year_ast > 2025) %>% 
  select(-date_ast) %>% 
  fwrite(
    paste0(lake_path, "/8200574_NS01DL0009_cc.txt"), col.names = FALSE
  )

################# RUN MODEL #######################

# Run the model. Previously calibrated 
run_air2water(sim_folder = proj_path, mode = "forward")

# model output
mod <- aw_extract_cal_output(
  list.files(paste0(lake_path, "/output_2"), pattern = "2_FORWARD", full.names = TRUE),
  status = "forward"
)

mod_long <- mod %>% 
  pivot_longer(cols = contains("temperature"), names_to = "variable")# %>% 
#  filter(!is.na(observed_water_temperature))

################# FIGURES #######################
fig_prefix <- paste0("proj_", depth_m, "_", start_val)

## Temperature Data
mod_long %>% 
  filter(variable == "observed_air_temperature") %>% 
  aw_plot_calval() 

ggsave(
  paste0(fig_path, "/", fig_prefix, "_temperature_inputs.png"),
  device = "png", dpi = 600, width = 20, height = 12, units = "cm"
)

## Simulated vs. Observed Water Temperature
aw_plot_model_ts(mod_long) 

ggsave(
  paste0(fig_path, "/", fig_prefix, "_temperature_output.png"),
  device = "png", dpi = 600, width = 20, height = 12, units = "cm"
)


