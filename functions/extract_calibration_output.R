
# https://github.com/aemon-j/air2wateR

aw_extract_cal_output <- function(path, status) {
  
 read.table(path) %>%  
    select(
      year_ast = V1, month_ast = V2, day_ast = V3,
      observed_air_temperature = V4,
      observed_water_temperature = V5,
      simulated_water_temperature = V6
    ) %>% 
    filter(month_ast != -999) %>% 
    mutate(
      timestamp_ast = as_date(paste(year_ast, month_ast, day_ast, sep = "-")),
      across(contains("temperature"), \(x) na_if(x, -999)),
      status = status
    ) %>% 
    select(timestamp_ast, contains("temperature"), status)

}