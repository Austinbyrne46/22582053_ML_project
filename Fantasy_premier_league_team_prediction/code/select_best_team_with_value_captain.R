select_best_team_with_value_captain <- function(gameweek_number) {
    # Filtering data set for game week
    gameweek_data <- twenty22_test[twenty22_test$gameweek == gameweek_number, ]

    # Checking if valid game week
    if (nrow(gameweek_data) == 0) {
        message("No gameweek data available ", gameweek_number)
        return(NULL)
    }

    # Sorting players by predicted points
    top_players <- gameweek_data[order(-gameweek_data$predicted_points), ]

    # Creating empty list
    selected_players <- list()

    # Getting the team and postion of new player
    team_levels <- unique(top_players$team)
    position_levels <- unique(top_players$position)

    # creating empty tables that will see if restrictions are not violated
    team_counts <- table(factor(levels = team_levels))
    position_counts <- table(factor(levels = position_levels))

    # telling r how many players are allowed per posistion
    required_positions <- c("GK" = 1, "DEF" = 3, "MID" = 5, "FWD" = 2)

    # telling r how many players are allowed per team
    max_players_per_team <- 3

    # telling r the maximum value of the entire team
    max_team_value <- 100

    # for loop to find the best players
    for (i in 1:nrow(top_players)) {
        player <- top_players[i, ]

        # Checking restrictions are met and not violated
        if (team_counts[player$team] < max_players_per_team &&
            position_counts[player$position] < required_positions[player$position] &&
            sum(unlist(lapply(selected_players, function(x) x$value)), na.rm = TRUE) + player$value <= max_team_value) {

            # updating the list
            selected_players[[length(selected_players) + 1]] <- player

            # Updating the team counts
            team_counts[player$team] <- team_counts[player$team] + 1
            position_counts[player$position] <- position_counts[player$position] + 1
        }

        # Checking if valid team is picked
        if (length(selected_players) == 11 || (all(position_counts >= required_positions) && all(team_counts <= max_players_per_team))) {
            break
        }
    }

    # Checking if valid team is picked
    if (length(selected_players) == 0) {
        message("No team for game week, sorry. ", gameweek_number)
        return(NULL)
    }

    # creating new data frame
    selected_players_df <- do.call(rbind, selected_players)

    # Calling select_captain function
    select_captain(selected_players_df)

    # Return the selected team
    return(selected_players_df[, c("names_22_23", "predicted_points", "total_points", "position", "team", "value")])
}

