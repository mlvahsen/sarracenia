make_cont_plot <- function(data, plot_type = c("plant_volume", "num_pitchers", 
                                                "pitchers_per_volume")) {
  
  plot_type <- match.arg(plot_type)
  
  # Set plot-specific variables
  if (plot_type == "plant_volume") {
    y_var <- "plantvol"
    y_lab <- expression("plant volume (" * cm^3 * ")")
    
    p <- data |> 
      ggplot(aes(x = month_ord, 
                 y = .data[[y_var]],
                 group = plantid, 
                 color = soilmdiff))
    
  } else if (plot_type == "num_pitchers") {
    y_var <- "numpitchc"
    y_lab <- "number of pitchers"
    
    p <- data |> 
      ggplot(aes(x = month_ord, 
                 y = .data[[y_var]],
                 group = plantid, 
                 color = soilmdiff))
    
  } else if (plot_type == "pitchers_per_volume") {
    
    p <- data |> 
      ggplot(aes(x = month_ord, y = log(numpitchc / plantvol), group = plantid, 
                 color = soilmdiff))
    
    y_lab <- expression(
      ln * bgroup("(", frac("number of pitchers",
                            "plant volume (" * cm^3 * ")"),")"))}
  
  # Shared plot formatting
  p +
    geom_line() +
    geom_point() +
    labs(x = "month", y = y_lab, color = "soil moisture difference (%)") +
    theme_classic(base_size = 14) +
    scale_color_continuous(palette = c("#08519c", "#6baed6", "#eff3ff")) +
    theme(legend.position = "top") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray47") +
    geom_vline(xintercept = 7,linetype = "dashed", color = "gray47")
}