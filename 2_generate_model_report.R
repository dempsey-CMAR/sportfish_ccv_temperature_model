
# June 20, 2025

library(here)
library(quarto)

# user defined parameters -------------------------------------------------

mean_depth_m <- 30
start_val <- 2020

# export html file --------------------------------------------------------

quarto::quarto_render(
  here("2_model_template.qmd"),
  execute_params = list(depth_m = mean_depth_m, start_val = start_val),
  output_file = paste0(
    paste(
      "pockwock_temperature_model",
      mean_depth_m,
      start_val,
      sep = "_"
    ), ".html")
)




