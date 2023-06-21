create_scatter_plot_with_top_players <- function(data, num_players) {
    # Calculate points per dollar for each player
    player_points_per_dollar <- data %>%
        mutate(points_per_dollar = total_points / value)

    # Select the top players based on points per dollar
    top_players <- player_points_per_dollar %>%
        top_n(num_players, points_per_dollar)

    # Create the scatter plot with color by position and label the top players
    plot <- ggplot(data, aes(x = value, y = total_points, color = position)) +
        geom_point() +
        labs(title = "Total Points vs. Player Value",
             x = "Player Value",
             y = "Total Points") +
        scale_color_manual(values = c("red", "blue", "green", "purple", "orange")) +
        theme_bw() +
        geom_text(data = top_players, aes(label = name), nudge_x = 1, nudge_y = 1)

    return(plot)
}


