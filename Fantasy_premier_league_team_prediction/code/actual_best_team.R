actual_best_team <- function(gameweek_number) {
    # Filtering data set to get game week
    gameweek_data <- twenty22_test[twenty22_test$gameweek == gameweek_number, ]

    # creating a check to see if the specified game week is valid
    if (nrow(gameweek_data) == 0) {
        message("No gameweek data available ", gameweek_number)
        return(NULL)
    }

    # ordering the predicted points to obtain highest predicted points
    top_players <- gameweek_data[order(-gameweek_data$total_points), ]

    # Creating an empty list
    selected_players <- list()

    # Get the team and posistion of the top players
    team_levels <- unique(top_players$team)
    position_levels <- unique(top_players$position)

    # creating tables that will help see if a new player added will violate a restriction
    team_counts <- table(factor(levels = team_levels))
    position_counts <- table(factor(levels = position_levels))

    # telling the function how many players allowed per posistion
    required_positions <- c("GK" = 1, "DEF" = 3, "MID" = 5, "FWD" = 2)

    # telling r how many players allowed per team
    max_players_per_team <- 3

    # creating a for loop thta finds the best players while obeying the restrictions set
    for (i in 1:nrow(top_players)) {
        player <- top_players[i, ]

        # Checking if restrictions are met
        if (team_counts[player$team] < max_players_per_team && position_counts[player$position] < required_positions[player$position]) {
            # Adding player to list so the function can keep track
            selected_players[[length(selected_players) + 1]] <- player

            # Updating counts
            team_counts[player$team] <- team_counts[player$team] + 1
            position_counts[player$position] <- position_counts[player$position] + 1
        }

        # Checking if valid team is created
        if (length(selected_players) == 11 || (all(position_counts >= required_positions) && all(team_counts <= max_players_per_team))) {
            break
        }
    }

    # Checking if there was in fact a valid team
    if (length(selected_players) == 0) {
        message("No team for gameweek, sorry. ", gameweek_number)
        return(NULL)
    }

    # Creating a data frame
    selected_players_df <- do.call(rbind, selected_players)

    # Call select_captain function
    select_captain(selected_players_df)

    # Return the selected team
    return(selected_players_df[, c("names_22_23", "predicted_points", "total_points", "position", "team", "value")])
}

