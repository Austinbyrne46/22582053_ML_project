select_captain <- function(selected_team) {
    # Sorting in decending order
    selected_team <- selected_team[order(-selected_team$predicted_points), ]

    # find the player with the highest points to make captain
    captain <- selected_team[1, "names_22_23"]

    # find the player with the second highest points to make vice captain
    vice_captain <- selected_team[2, "names_22_23"]

    # print message that states who to make captain and vice captain
    message("Make '", captain, "' captain")
    message("Make '", vice_captain, "' vice-captain")
}
