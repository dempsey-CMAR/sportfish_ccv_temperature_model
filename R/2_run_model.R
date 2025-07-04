# July 4, 2025

# You must create a folder script_sim_folder/sim_{depth_m}_{start_val}
# manually create a folder named {lake} (e.g., "pockwock")
# manually copy/paste the input.txt and POS.txt files into the folder

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

theme_set(theme_light())

##################### UPDATE THESE ###################################
lake <- "pockwock"
depth_m <- 40
start_val <- 2020

sim_path <- here(paste0("script_sim_folder/sim_", depth_m, "_", start_val))

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

################# RUN MODEL #######################

# generate params (writes txt to sim_{depth_m}_{start_val}/{lake} folder)
gen_param(sim_path, mean_depth = depth_m)

# Run the model
run_air2water(sim_folder = sim_path, mode = "pso")

################# FIGURES #######################

# model output
out <- get_outputs(sim_folder = sim_path) %>% 
  select(
    timestamp_ast = datetime,
    air_temperature = AT,
    observed_water_temperature = LSWT_obs,
    simulated_water_temperature = LSWT_sim,
    C,
    status
  ) 

out_long <- out %>% 
  pivot_longer(
    cols = contains("temperature"), values_to = "value", names_to = "variable") 

## Temperature Data
out_long %>% 
  filter(variable != "simulated_water_temperature") %>% 
  ggplot(aes(timestamp_ast, value, color = status)) +
  geom_point(size = 0.5) +
  scale_color_manual(values = c("#D95F02", "#7570B3")) +
  ylab('Temperature (\u00B0C)') +
  facet_wrap(~variable, ncol = 1, scales = "free") +
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
  geom_line(size = 1) +
  scale_color_manual(
    "Temperature", values = c("darkgrey","#1B9E77", "#E7298A")
  ) +
  ylab('Temperature (\u00B0C)') +
  facet_wrap(~status, nrow = 2, scales = "free_x") +
  theme(axis.title.x = element_blank())

### Correlation
ggplot(out, aes(observed_water_temperature, simulated_water_temperature)) +
  geom_point(col = "#7570B3") +
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





