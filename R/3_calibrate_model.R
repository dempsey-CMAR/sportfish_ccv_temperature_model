# July 21, 2025

# average depth: 40 m
## this is used to determine the "acceptable" range of parameters
## does not need to be precise; order of magnitude is fine (based on meeting)
## 40 m is cited as the maximum depth in [Ref]

# using all data to calibrate

# Instructions:
## You must create a folder final_cal_val/sim_{depth_m}_{start_val}
## manually create a folder named {lake} (e.g., "pockwock")
## manually copy/paste the input.txt and POS.txt files into the folder

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

sim_path <- here(paste0("output/3_final_cal/final_", depth_m, "_", start_val))

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
  fwrite(
    paste0(sim_path, "/", lake, "/8200574_NS01DL0009_cc.txt"), col.names = FALSE
  )

# cv file
dat %>% 
  filter(file_type == "validation") %>% 
  select(-c(file_type, date_ast)) %>% 
  fwrite(
    paste0(sim_path, "/", lake, "/8200574_NS01DL0009_cv.txt"), col.names = FALSE
  )

################# CALIBRATE MODEL #######################

# generate params (writes txt to sim_{depth_m}_{start_val}/{lake} folder)
gen_param(sim_path, mean_depth = depth_m)

# Run the model
run_air2water(sim_folder = sim_path, mode = "pso")

################# FIGURES #######################

# model output
out_raw <-  read.table(
  paste0(sim_path, "/", lake, "/output_2/2_PSO_RMS_8200574_NS01DL0009_cc_1d.out")
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
    status = "calibration"
  ) %>% 
  select(timestamp_ast, contains("temperature"), status)

out_long <- out %>% 
  pivot_longer(
    cols = contains("temperature"), values_to = "value", names_to = "variable") 

## Temperature Data
out_long %>% 
  filter(variable != "simulated_water_temperature") %>% 
  ggplot(aes(timestamp_ast, value, color = status)) +
  geom_point(size = 0.5) +
  scale_color_manual(values = c("#FFD118", "#22A884")) +
  ylab('Temperature (\u00B0C)') +
  facet_wrap(~variable, ncol = 1, scales = "free") +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(axis.title.x = element_blank())

## Model Parameters
plot_param(sim_folder = sim_path) +
  ylab('RMSE (\u00B0C)') 

## Simulated vs. Observed Water Temperature
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

### Correlation
ggplot(out, aes(observed_water_temperature, simulated_water_temperature)) +
  geom_point(col = "#414487") +
  geom_abline(slope = 1, intercept = 0, col = "darkgrey", linewidth = 1.5) +
  facet_wrap(~status, nrow = 2) +
  theme(panel.spacing.y = unit(2, "lines"))

## Model Fit
tss <- out %>% 
  pivot_longer(
    cols = contains("temperature"), values_to = "value",
    names_to = "variable") %>% 
  group_by(variable, status) %>% 
  mutate(
    sample_mean = mean(value, na.rm = TRUE),
    diff_squared = (value - sample_mean)^2
  ) %>% 
  summarise(
    tss = sum(diff_squared, na.rm = TRUE)
  )

fit <- out %>% 
  group_by(status) %>% 
  mutate(
    squared_error = 
      (observed_water_temperature - simulated_water_temperature)^2 
  ) %>% 
  summarise(
    sse = sum(squared_error, na.rm = TRUE),
    mse = mean(squared_error, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  left_join(
    filter(tss, variable == "observed_water_temperature"), 
    by = join_by(status)
  ) %>% 
  mutate(
    rmse = sqrt(mse),
    r2 = 1 - sse / tss,
    
    across(.cols = where(is.numeric), .fns = ~round(.x, digits = 2))
  ) %>% 
  select(status, tss, sse, mse, rmse, r2)

# # check fit
# cal <- out %>% 
#   filter(
#     status == "calibration", 
#     !is.na(observed_water_temperature),
#     !is.na(simulated_water_temperature)
#   )
# summary(lm(observed_water_temperature ~ simulated_water_temperature, data = cal))
# 
# cor(cal$observed_water_temperature, cal$simulated_water_temperature)^2
