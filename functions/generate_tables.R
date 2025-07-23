
aw_calculate_fit <- function(dat) {
  tss <- dat %>% 
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
  
  dat %>% 
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
}