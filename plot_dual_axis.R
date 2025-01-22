library(ggplot2)
library(data.table)

plot_dual_axis <- function(dt, xaxis, col1, col2, color1 = "blue", color2 = "red", 
                           col1_label, col2_label, xaxis_label, x_labels = NULL) {
  # Ensure the input is a data.table
  dt <- as.data.table(dt)
  
  # Normalize col2 to match the scale of col1
  scale_factor <- max(dt[[col1]], na.rm = TRUE) / max(dt[[col2]], na.rm = TRUE)
  
  ggplot(dt, aes_string(x = xaxis)) +
    geom_line(aes(y = .data[[col1]], color = col1), size = 1) +
    geom_line(aes(y = .data[[col2]] * scale_factor, color = col2), size = 1) +
    scale_x_continuous(breaks = dt[[xaxis]], labels = x_labels %||% dt[[xaxis]]) +
    scale_y_continuous(
      name = col1_label,
      sec.axis = sec_axis(~ . / scale_factor, name = col2_label)
    ) +
    scale_color_manual(values = setNames(c(color1, color2), c(col1, col2)),
                       labels = c(col1_label, col2_label)) +
    labs(x = xaxis_label) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank())
}

# # Example usage
# dt <- data.table(year = 2000:2010, val1 = rnorm(11, 100, 10), val2 = rnorm(11, 50, 5))
# custom_labels <- paste0("Y", dt$year)  # Example of custom x-axis labels
# 
# plot_dual_axis(dt, "year", "val1", "val2", 
#                color1 = "green", color2 = "orange", 
#                col1_label = "Series 1", col2_label = "Series 2", 
#                xaxis_label = "Year", x_labels = custom_labels)
