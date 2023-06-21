create_team_points_distribution_plot <- function(data) {
    # Calculate the minimum, average, and maximum points for each team
    team_stats <- data %>%
        group_by(team) %>%
        summarise(min_points = min(total_points),
                  avg_points = mean(total_points),
                  max_points = max(total_points)) %>%
        arrange(desc(avg_points)) %>%
        mutate(team = factor(team, levels = team))

    # Create the plot
    plot <- ggplot(team_stats, aes(x = avg_points, y = team)) +
        geom_point(aes(color = "Average Points")) +
        geom_point(aes(x = min_points, color = "Minimum Points"), size = 2) +
        geom_point(aes(x = max_points, color = "Maximum Points"), size = 2) +
        labs(title = "Points Distribution by Team",
             x = "Points",
             y = "Team") +
        scale_color_manual(values = c("red", "blue", "green")) +
        theme_bw()

    return(plot)
}

