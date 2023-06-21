select_captain <- function(selected_team) {
    # Sort the selected team by predicted points in descending order
    selected_team <- selected_team[order(-selected_team$predicted_points), ]

    # Get the player with the highest predicted points
    captain <- selected_team[1, "names_22_23"]

    # Get the player with the second highest predicted points
    vice_captain <- selected_team[2, "names_22_23"]

    # Print the captain and vice-captain recommendations
    message("Make '", captain, "' captain")
    message("Make '", vice_captain, "' vice-captain")
}