select_best_team_with_captain <- function(gameweek_number) {
    # Filter the dataset for the specified gameweek
    gameweek_data <- twenty22_test[twenty22_test$gameweek == gameweek_number, ]

    # Check if the dataset is empty for the given gameweek
    if (nrow(gameweek_data) == 0) {
        message("No data available for gameweek ", gameweek_number)
        return(NULL)
    }

    # Sort the dataset by predicted points in descending order
    top_players <- gameweek_data[order(-gameweek_data$predicted_points), ]

    # Create empty lists to store selected players and team/position counts
    selected_players <- list()

    # Get the unique team and position levels from the top_players data
    team_levels <- unique(top_players$team)
    position_levels <- unique(top_players$position)

    # Initialize the team and position counts as empty tables with the required levels
    team_counts <- table(factor(levels = team_levels))
    position_counts <- table(factor(levels = position_levels))

    # Define the required number of players per position
    required_positions <- c("GK" = 1, "DEF" = 3, "MID" = 5, "FWD" = 2)

    # Define the maximum number of players allowed from each team
    max_players_per_team <- 3

    # Iterate through the top players and select the best team
    for (i in 1:nrow(top_players)) {
        player <- top_players[i, ]

        # Check if adding the player violates any restrictions
        if (team_counts[player$team] < max_players_per_team && position_counts[player$position] < required_positions[player$position]) {
            # Add the player to the selected players list
            selected_players[[length(selected_players) + 1]] <- player

            # Update the team and position counts
            team_counts[player$team] <- team_counts[player$team] + 1
            position_counts[player$position] <- position_counts[player$position] + 1
        }

        # Check if 11 players have been selected or if all position requirements and team restrictions are met
        if (length(selected_players) == 11 || (all(position_counts >= required_positions) && all(team_counts <= max_players_per_team))) {
            break
        }
    }

    # Check if no players were selected
    if (length(selected_players) == 0) {
        message("No valid team can be selected for gameweek ", gameweek_number)
        return(NULL)
    }

    # Convert the selected players list to a data frame
    selected_players_df <- do.call(rbind, selected_players)

    # Call the select_captain function with the selected team as the input
    select_captain(selected_players_df)

    # Return the selected team
    return(selected_players_df[, c("names_22_23", "predicted_points", "total_points", "position", "team", "value")])
}

