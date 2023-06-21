library(ggplot2)
library(dplyr)

create_average_points_with_horizontal_lines_plot <- function(data) {
    # Calculate average points per position
    average_points <- data %>%
        group_by(position) %>%
        summarize(avg_points = mean(total_points))

    # Plot line graphs with average points as horizontal lines
    plot <- ggplot(data, aes(x = GW, y = total_points, color = position)) +
        geom_line() +
        geom_point() +
        geom_hline(data = average_points, aes(yintercept = avg_points, color = position), linetype = "dashed") +
        labs(title = "Average Points Scored per Position per Gameweek",
             x = "Gameweek",
             y = "Average Points") +
        scale_color_manual(values = c("red", "blue", "green", "purple", "orange")) +
        theme_bw() +
        facet_wrap(~ position)

    return(plot)
}


