## Window function
library(Lahman)
batting <- select(tbl_df(Batting), playerID, yearID, teamID, AB:H)
batting <- arrange(batting, playerID, yearID, teamID)
players <- group_by(batting, playerID)

# For each player, find the two years with most hits
filter(players, H > 0, min_rank(desc(H)) <= 2) %>% arrange(playerID, desc(H))
# Within each player, select best 10% of years
filter(players, cume_dist(desc(R)) < 0.1)
# For each player, find every year that was better than the previous year
filter(players, R > lag(R)) %>% View()
# For each player, find when a player changed teams
players$teamID <- as.character(players$teamID)
filter(players, teamID != lag(teamID, order_by = yearID))
