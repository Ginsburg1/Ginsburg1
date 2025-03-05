library(Lahman)
library(tidyverse)
library(baseballr)
library(retrosheet)
library(ggplot2)

#The haversine_distance function takes in a latitude and longitude for two different 
# coordinates and then find the distance between them and converts it into miles.
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  R <- 6371 
  distance_km <- R * c
  
  distance_miles <- distance_km * 0.621371
  
  return(distance_miles)
}

#This is a list of all the team names in the MLB.
teamNames <- c("HOU","TOR","CIN","COL","DET","LAN","SEA","MIA","CHN"
               ,"MIL","PHI","NYN","SDN","PIT","WAS","SFN","ARI","SLN"
               ,"BOS","BAL","ANA","CHA","NYA","KCA","CLE","OAK","MIN",
                "TBA","ATL","TEX")

team_data <- list()

#This loop will get the data for each team in the MLB for the 2023 season.
for (i in teamNames) {
  team_data[[i]] <- getRetrosheet("game", 2023) %>% 
  filter(VisTm == i | HmTm == i) %>% 
  mutate(TeamName = i, OpposingTeam =ifelse(VisTm == i,HmTm,VisTm))
}
#This data frame contains the latitude and longitude of all the stadiums in the MLB. 
stadiums_df <- data.frame(
  Team = c("HOU", "TOR", "CIN", "COL", "DET", "LAN", "SEA", "MIA", "CHN", "MIL", 
           "PHI", "NYN", "SDN", "PIT", "WAS", "SFN", "ARI", "SLN", "BOS", "BAL", 
           "ANA", "CHA", "NYA", "KCA", "CLE", "OAK", "MIN", "TBA", "ATL", "TEX"),
  Latitude = c(29.7573, 43.6414, 39.0979, 39.7559, 42.3391, 34.0736, 47.5914, 25.7786, 
               41.9484, 43.0280, 39.9056, 40.7571, 32.7076, 40.4469, 38.8730, 37.7786, 
               33.4452, 38.6226, 42.3465, 39.2839, 33.8003, 41.8299, 40.8296, 39.0517, 
               41.4958, 37.7516, 44.9817, 27.7683, 33.8908, 32.7473),
  Longitude = c(-95.3554, -79.3894, -84.5086, -104.9942, -83.0486, -118.2400, -122.3324, 
                -80.2194, -87.6553, -87.9712, -75.1667, -73.8458, -117.1570, -80.0057, 
                -77.0074, -122.3893, -112.0667, -90.1928, -71.0970, -76.6215, -117.8827, 
                -87.6338, -73.9262, -94.4803, -81.6853, -122.2005, -93.2775, -82.6486, 
                -84.4678, -97.0826)
)

all_team_data <- bind_rows(team_data)

#This loop will add the latitude and longitude of the stadiums to the data frame.
#It will also calculate the distance traveled for each game and the cumulative distance traveled,
# if the team won or lost the game, and the total distance traveled for each team.
all_team_data <- all_team_data %>%
  left_join(stadiums_df %>% rename(AwayTeamLat = Latitude, AwayTeamLon = Longitude),
            by = c("VisTm" = "Team")) %>%
  left_join(stadiums_df %>% rename(HomeTeamLat = Latitude, HomeTeamLon = Longitude),
            by = c("HmTm" = "Team")) %>% 
  mutate(GameLat = HomeTeamLat, GameLon = HomeTeamLon) %>% 
  arrange(TeamName, Date) %>%
  group_by(TeamName) %>%
  mutate(PrevGameLat = lag(GameLat),
         PrevGameLon = lag(GameLon),
         DistanceTravelled = ifelse(is.na(PrevGameLat) | is.na(PrevGameLon), 0,
                                    ifelse(PrevGameLat == GameLat & PrevGameLon == GameLon, 0,
                                           haversine_distance(PrevGameLat, PrevGameLon, GameLat, GameLon))),
         WinLoss = ifelse(HmRuns > VisRuns & HmTm == TeamName, "W", 
                          (ifelse((HmRuns < VisRuns & VisTm == TeamName), "W", "L"))),
         CumulativeTravel = cumsum(DistanceTravelled),
         TravelDay = ifelse(DistanceTravelled > 0, "Yes", "No"),
         RunsScored = ifelse(HmTm == TeamName, HmRuns, VisRuns)) %>%
  ungroup()


#This will show the data frame with the distance traveled for each game, the cumulative distance traveled,
# the latitude and longitude of the stadiums, and the latitude and longitude of the games.
all_team_data %>% 
  select(TeamName, HmTm, VisTm, AwayTeamLat, AwayTeamLon, HomeTeamLat, HomeTeamLon, GameLat, GameLon, DistanceTravelled) %>% 
  view()

#This shows the total distance each team traveled
all_team_data %>% 
  group_by(TeamName) %>% 
  summarize(travel = sum(DistanceTravelled)) %>% 
  arrange(desc(travel)) %>% 
  view()

#Plot for how far each team traveled versus Number of Wins a team Had
Team_Travel_Wins <- all_team_data %>% 
  group_by(TeamName) %>% 
  summarise(TotalDistance = sum(DistanceTravelled), Wins = sum(WinLoss == "W")) %>% 
  arrange(TotalDistance) 

  ggplot(Team_Travel_Wins, aes(x = TotalDistance, y = Wins, label = TeamName)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Total Distance Traveled by Teams vs Number of Wins",
       x = "Total Distance Traveled",
       y = "Number of Wins") +
  theme_minimal()

#Linear Regression for how far a team Traveled versus Number of Wins 
Team_Travel_Wins_Reg <- lm(Wins ~ TotalDistance, data =  Team_Travel_Wins)
  summary(Team_Travel_Wins_Reg)


#Cumulative travel distances over the season for all teams 
ggplot(all_team_data, aes(x = as.Date(Date, format="%Y-%m-%d"), y = CumulativeTravel, color = TeamName)) +
  geom_point() +
  geom_line() +
  labs(title = "Cumulative Travel Distances for All Teams in 2023",
       x = "Date",
       y = "Cumulative Distance Travelled (miles)") +
  theme_minimal() +
  theme(legend.position = "none")


#Regression to show if there is a collecation between team performance and if a team had
#traveled the day before 
travel_games <- all_team_data %>% 
  filter(DistanceTravelled > 0)

travel_games_regression <-lm(RunsScored ~ DistanceTravelled, data = travel_games)
summary(travel_games_regression)

#Average Runs scored on travel days versus non travel days 
model4 <- all_team_data %>% 
  group_by(TravelDay) %>% 
  summarize(Wins = sum(WinLoss == "W"), Loss = sum(WinLoss == "L"), Percent = Wins/(Wins +Loss), .groups = 'keep')

#Make a graph that has a stacked bar plot based on wins 
team_data <- all_team_data %>%
  filter(DistanceTravelled >0 )

# Create a histogram of DistanceTravelled for wins
ggplot(team_data, aes(x = DistanceTravelled, fill = WinLoss)) + 
  geom_histogram(bins = 8, color = "black", position = "stack") +
  labs(title = "Distribution of Distance Traveled for Wins and Losses",
       x = "Distance Travelled (miles)",
       y = "Number of Games",
       fill = "Outcome") +
  theme_minimal() +
  scale_fill_manual(values = c("W" = "blue", "L" = "red"))

#Check to see if there is a difference in runs scored on travel versus non travel days 
Travel_Runs <- all_team_data %>%
  group_by(TeamName, TravelDay) %>% 
  summarise(RunsScored = (sum(RunsScored)), CountGames = n(), AverageRuns = RunsScored/ CountGames,.groups = 'keep')
  
ggplot(Travel_Runs, aes(x = TeamName, y = AverageRuns, fill = TravelDay)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Runs Scored on Travel Days vs Non-Travel Days",
       x = "Team Name",
       y = "Average Runs Scored",
       fill = "Travel Day") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


regression_model <- lm(AverageRuns ~ TravelDay, data = Travel_Runs)
summary(regression_model)



#Same thing but now its for all teams not indivual 
Travel_Runs <- all_team_data %>%
  group_by(TravelDay) %>% 
  summarise(RunsScored = (sum(RunsScored)), CountGames = n(), AverageRuns = RunsScored/ CountGames,.groups = 'keep') 


ggplot(Travel_Runs, aes(x = TravelDay, y = AverageRuns, fill = TravelDay)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Runs Scored on Travel Days vs Non-Travel Days",
       x = "Team Name",
       y = "Average Runs Scored",
       fill = "Travel Day") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

regression_model <- lm(AverageRuns ~ TravelDay, data = Travel_Runs)
summary(regression_model)
