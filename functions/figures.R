
aw_plot_calval <- function(dat, pal = c("#FFD118", "#22A884")) {

ggplot(dat, aes(timestamp_ast, value, color = status)) +
  geom_point(size = 0.5) +
  scale_color_manual(values = pal) +
  ylab('Temperature (\u00B0C)') +
  facet_wrap(~variable, ncol = 1, scales = "free") +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(axis.title.x = element_blank())

}


aw_plot_model_ts <- function(dat, pal = c("lightgrey", "#063E4D", "#7AD151")) {
 
  dat %>% 
    mutate(
      variable = str_remove(variable, "_temperature"),
      variable = str_replace(variable, "_", " "),
      variable = str_to_title(variable)
    ) %>%
    ggplot(aes(timestamp_ast, value, col = variable)) +
    geom_line(linewidth = 1) +
    scale_color_manual("Temperature", values = pal) +
    ylab('Temperature (\u00B0C)') +
    facet_wrap(~status, nrow = 2, scales = "free_x") +
    theme(axis.title.x = element_blank())
  
}

aw_plot_temperature_correlation <- function(dat, pal =  "#414487") {
  
  ggplot(dat, aes(observed_water_temperature, simulated_water_temperature)) +
    geom_point(col = pal) +
    geom_abline(slope = 1, intercept = 0, col = "darkgrey", linewidth = 1.5) +
    facet_wrap(~status, nrow = 2) +
    theme(panel.spacing.y = unit(2, "lines"))
  
}





