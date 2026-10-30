make_endpoint_plot <- function(data, response, initial_var, y_label,
                               n_points = 100) {
  
  model_formula <- as.formula(paste(response, "~ avgsoilmdiff +", initial_var))
  mod <- lm(model_formula, data = data)
  
  newdat <- data.frame(
    avgsoilmdiff = seq(min(data$avgsoilmdiff, na.rm = TRUE),
                       max(data$avgsoilmdiff, na.rm = TRUE),
                       length.out = n_points),
    initial_value = mean(data[[initial_var]], na.rm = TRUE)
  )
  
  names(newdat)[names(newdat) == "initial_value"] <- initial_var
  
  pred <- predict(mod, newdat, interval = "confidence")
  plot_data <- cbind(newdat, pred)
  plot_data[[response]] <- plot_data$fit
  
  p <- data |>
    ggplot(aes(x = avgsoilmdiff, y = .data[[response]])) +
    geom_point(aes(fill = avgsoilmdiff), shape = 21, size = 3, stroke = 0.6) +
    geom_ribbon(data = plot_data, aes(ymin = lwr, ymax = upr),
                fill = "black", alpha = 0.2) +
    geom_line(data = plot_data, aes(x = avgsoilmdiff, y = .data[[response]]),
              color = "black", linewidth = 1.2) +
    theme_classic(base_size = 12) +
    scale_fill_continuous(palette = c("#08519c", "#6baed6", "#eff3ff")) +
    labs(x = "soil moisture difference (%)\n wet\u2192dry",
         y = y_label, fill = "soil moisture difference (%)")
  
  list(model = mod, newdata = newdat, predictions = plot_data, plot = p)
}