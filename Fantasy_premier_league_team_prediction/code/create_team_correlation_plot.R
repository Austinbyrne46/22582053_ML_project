library(tidyverse)

create_team_correlation_plot <- function(data) {
    # Team attributes
    variables_corr_team <- c("total_points","fixture", "opponent_team", "round", "was_home")

    # Calculate the correlation matrix
    correlation_matrix_team <- cor(data[variables_corr_team])

    # Convert the correlation matrix into a data frame
    team_correlation_df <- as.data.frame(correlation_matrix_team)

    # Convert the correlation matrix to long format
    team_correlation_df_tidy <- team_correlation_df %>%
        rownames_to_column(var = "Var1") %>%
        gather(Var2, value, -Var1)

    # Plot the correlation plot using ggplot
    team_correlation_plot <- ggplot(team_correlation_df_tidy, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Correlation plot team attributes and total points", x = "", y = "")

    return(team_correlation_plot)
}


