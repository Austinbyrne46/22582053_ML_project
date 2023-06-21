library(tidyverse)

create_player_correlation_plot <- function(data) {
    # Player attributes that can be seen before the gameweek
    variables_corr_player <- c("creativity", "element", "fixture", "ict_index", "influence", "opponent_team", "selected", "threat", "total_points", "transfers_in", "transfers_out")

    # Calculate the correlation matrix
    correlation_matrix_player <- cor(data[variables_corr_player])

    # Convert the correlation matrix into a data frame
    player_correlation_df <- as.data.frame(correlation_matrix_player)

    # Convert the correlation matrix to long format
    player_correlation_df_tidy <- player_correlation_df %>%
        rownames_to_column(var = "Var1") %>%
        gather(Var2, value, -Var1)

    # Plot the correlation plot using ggplot
    player_correlation_plot <- ggplot(player_correlation_df_tidy, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Correlation plot player attributes and total points", x = "", y = "")

    return(player_correlation_plot)
}

