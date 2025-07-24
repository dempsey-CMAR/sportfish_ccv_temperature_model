# July 23, 2025

# STEP 2: calibrate and validate model to demonstrate good performance
## use half the years to calibrate and the remaining to validate

library(air2wateR)
library(data.table)
library(dplyr)
library(DT)
library(ggplot2)
library(here)
library(plotly)
library(readr)
library(stringr)
library(tidyr)

source(here("functions/generate_figures.R"))
source(here("functions/generate_tables.R"))

theme_set(theme_light())

##################### UPDATE THESE ###################################
lake <- "pockwock"
depth_m <- 24
start_val <- 2020

##################### FILE PATHS ###################################
sim_path <- here("sim_folder")
cal_val_path <- paste0(sim_path, "/2_cal_val/calval_", depth_m, "_", start_val)
lake_path <- paste(cal_val_path, lake, sep = "/")
fig_path <- paste0(cal_val_path, "/figures")

##################### CREATE FOLDERS & COPY INPUT FILES (if needed) ###########################
inputs <- list.files(paste0(sim_path, "/input_files"), pattern = "txt", full.names = TRUE)

dir.create(cal_val_path)
dir.create(lake_path)
dir.create(fig_path)

file.copy(inputs, cal_val_path)

################# EXPORT CALIBRATION & VALIDATION DATA #######################

# read in data and assign calibration and validation
dat <- fread(here("data/8200574_NS01DL0009_data.csv")) %>% 
  mutate(
    file_type = if_else(year_ast >= start_val, "validation", "calibration")
  )

# cc file
dat %>% 
  filter(file_type == "calibration") %>% 
  select(-c(file_type, date_ast)) %>% 
  fwrite(paste0(lake_path, "/8200574_NS01DL0009_cc.txt"), col.names = FALSE)

# cv file
dat %>% 
  filter(file_type == "validation") %>% 
  select(-c(file_type, date_ast)) %>% 
  fwrite(paste0(lake_path, "/8200574_NS01DL0009_cv.txt"), col.names = FALSE)

################# CALIBRATE MODEL #######################

# generate params (writes txt to cal_val_path)
gen_param(cal_val_path, mean_depth = depth_m)

# Run the model
run_air2water(sim_folder = cal_val_path, mode = "pso")

# model output
mod <- get_outputs(sim_folder = cal_val_path) %>% 
  select(
    timestamp_ast = datetime,
    air_temperature = AT,
    observed_water_temperature = LSWT_obs,
    simulated_water_temperature = LSWT_sim,
    status
  ) 

mod_long <- mod %>% 
  pivot_longer(cols = contains("temperature"), names_to = "variable") 

################# FIGURES #######################
fig_prefix <- paste0("cal_val_", depth_m, "_", start_val)

## Temperature Data
mod_long %>% 
  filter(variable != "simulated_water_temperature") %>% 
  aw_plot_calval() 

ggsave(
  paste0(fig_path, "/", fig_prefix, "_temperature_inputs.png"),
  device = "png", dpi = 600, width = 20, height = 12, units = "cm"
)

## Model Parameters
plot_param(sim_folder = cal_val_path) +
  ylab('RMSE (\u00B0C)') 

ggsave(
  paste0(fig_path, "/", fig_prefix, "_dotty_plots.png"),
  device = "png", dpi = 600, width = 15, height = 12, units = "cm"
)

## Simulated vs. Observed Water Temperature
aw_plot_model_ts(mod_long) 

ggsave(
  paste0(fig_path, "/", fig_prefix, "_temperature_output.png"),
  device = "png", dpi = 600, width = 20, height = 12, units = "cm"
)

## Correlation
aw_plot_correlation(mod)

ggsave(
  paste0(fig_path, "/", fig_prefix, "_correlaion.png"),
  device = "png", dpi = 600, width = 20, height = 12, units = "cm"
)

## Model Fit
fit <- aw_calculate_fit(mod)

fwrite(fit, file = paste0(fig_path, "/", fig_prefix, "_fit.csv"),)

