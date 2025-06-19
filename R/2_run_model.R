# June 19, 2025

# library(devtools)
# devtools::install_github("aemon-j/air2wateR", force = TRUE)

# max depth:
# https://www.researchgate.net/publication/233472680_Tracking_long-term_acidification_trends_in_Pockwock_Lake_Halifax_Nova_Scotia_the_water_supply_for_a_major_eastern_Canadian_city/figures?lo=1

library(air2wateR)
library(data.table)
library(dplyr)
library(here)

# this worked - changing index.txt doesn't result in different params
# chaning the mean depth does
sim_folder <- here("simulation")
gen_param(sim_folder, mean_depth = 14)

# Run the model
run_air2water(sim_folder = sim_folder, mode = "pso")

plot_param(sim_folder = sim_folder) +
  ylab('RMSE (\u00B0C)') +
  theme_classic(base_size = 18)

# Compare simulated to observed -------------------------------------------

out <- get_outputs(sim_folder = sim_folder)
ggplot(out) +
  geom_line(aes(datetime, LSWT_sim, colour = 'Sim')) +
  geom_point(aes(datetime, LSWT_obs, colour = 'Obs')) +
  facet_wrap(~status, nrow = 2, scales = "free_x") +
  ylab('Temperature (\u00B0C)') +
  theme_classic(base_size = 20)


# Calculate and plot well-mixed layer -------------------------------------

mean_depth <- 14 
out$wml <- out$C * mean_depth # estimate well-mixed layer as a function of mean depth


ggplot(out) +
  geom_line(aes(datetime, wml)) +
  facet_wrap(~status, nrow = 2, scales = "free_x") +
  ylab('Depth (m)') +
  scale_y_reverse() +
  coord_cartesian(ylim = c(mean_depth, 0)) +
  theme_classic(base_size = 20)

