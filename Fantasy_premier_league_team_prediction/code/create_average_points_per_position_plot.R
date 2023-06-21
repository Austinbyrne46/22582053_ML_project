library(ggplot2)

create_average_points_per_position_plot <- function(data) {
    # Calculate average points per position per gameweek
    average_points_position <- aggregate(total_points ~ GW + position, data = data, FUN = mean)

    # Create the line graph
    average_points_per_position_combined_plot <- ggplot(average_points_position, aes(x = GW, y = total_points, color = position)) +
        geom_line() +
        geom_point() +
        labs(title = "Average Points Scored per Position per Gameweek",
             x = "Gameweek",
             y = "Average Points") +
        scale_color_manual(values = c("red", "blue", "green", "purple", "orange")) +
        theme_bw()

    return(average_points_per_position_combined_plot)
}


