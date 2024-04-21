#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


## Player Performance Analysis
# 1. Read and combine tables

# Load necessary libraries
required_packages <- c("shiny", "shinydashboard", "dplyr", "ggplot2", "readr", "cowplot")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

require(shiny)
require(shinydashboard)
require(readr)
require(ggplot2)
require(dplyr)
require(cowplot)

master <- read.csv("archive/Master.csv")
battinga <- read.csv("archive/Batting.csv")
battingpost <- read.csv("archive/BattingPost.csv")
fielding <- read.csv("archive/Fielding.csv")
pitchinga <- read.csv("archive/Pitching.csv")
pitchingpost <- read.csv("archive/PitchingPost.csv")
salaries <- read.csv("archive/Salaries.csv")
teams <- read.csv("archive/Teams.csv")
series_post <- read.csv("archive/SeriesPost.csv")
awards <- read.csv("archive/AwardsPlayers.csv")

# Combine the regular and post

batting <- bind_rows(battinga, battingpost)

# Convert BAOpp in both data frames to numeric type
pitchinga$BAOpp <- as.numeric(as.character(pitchinga$BAOpp))
pitchingpost$BAOpp <- as.numeric(as.character(pitchingpost$BAOpp))

pitching <- bind_rows(pitchinga, pitchingpost)

# 2. Create criteria to evaluate the performance of a player in different positions.

# 2.1 Batting

# a. Career Performance Overview
# Group data by playerID and then aggregate the data

# First, ensure the conversion from factor/character to numeric is correct and check for NAs.
batting <- batting %>%
  mutate(
    X2B = as.numeric(as.character(X2B)),
    X3B = as.numeric(as.character(X3B))
  )

# Check if there were any NAs introduced during the conversion
sum(is.na(batting$X2B))
sum(is.na(batting$X3B))

# If NAs were introduced, decide on a strategy to handle them, such as replacing with zero
batting <- batting %>%
  mutate(
    X2B = ifelse(is.na(X2B), 0, X2B),
    X3B = ifelse(is.na(X3B), 0, X3B)
  )

# Summarisation without quotes around column names
batting_career_summary <- batting %>%
  group_by(playerID) %>%
  summarise(
    Total_HR = sum(HR, na.rm = TRUE),
    Total_H = sum(H, na.rm = TRUE),
    Total_AB = sum(AB, na.rm = TRUE),
    Total_BB = sum(BB, na.rm = TRUE),
    Total_HBP = sum(HBP, na.rm = TRUE),
    Total_SF = sum(SF, na.rm = TRUE),
    Total_2B = sum(X2B, na.rm = TRUE),  # Correctly referenced
    Total_3B = sum(X3B, na.rm = TRUE)   # Correctly referenced
  ) %>%
  mutate(
    Career_AVG = Total_H / Total_AB,
    Career_OBP = (Total_H + Total_BB + Total_HBP) / (Total_AB + Total_BB + Total_HBP + Total_SF),
    Career_SLG = (Total_H + Total_2B*2 + Total_3B*3 + Total_HR*4) / Total_AB,
    Career_OPS = Career_OBP + Career_SLG
  )

# Plots

p_avg <- ggplot(batting_career_summary, aes(x = Career_AVG)) +
  geom_histogram(binwidth = 0.01, fill = "lightblue", color = "black") +
  geom_vline(xintercept = 0.3, linetype = "dashed", color = "red") +
  labs(title = "Career AVG", x = "AVG", y = "Frequency") +
  theme_minimal()

p_obp <- ggplot(batting_career_summary, aes(x = Career_OBP)) +
  geom_histogram(binwidth = 0.01, fill = "lightgreen", color = "black") +
  geom_vline(xintercept = 0.37, linetype = "dashed", color = "red") +
  labs(title = "Career OBP", x = "OBP", y = "Frequency") +
  theme_minimal()

p_slg <- ggplot(batting_career_summary, aes(x = Career_SLG)) +
  geom_histogram(binwidth = 0.01, fill = "lightpink", color = "black") +
  geom_vline(xintercept = 0.45, linetype = "dashed", color = "red") +
  labs(title = "Career SLG", x = "SLG", y = "Frequency") +
  theme_minimal()

p_ops <- ggplot(batting_career_summary, aes(x = Career_OPS)) +
  geom_histogram(binwidth = 0.02, fill = "lightcoral", color = "black") +
  geom_vline(xintercept = 0.8, linetype = "dashed", color = "red") +
  labs(title = "Career OPS", x = "OPS", y = "Frequency") +
  theme_minimal()

# Arrange the plots in a 2x2 grid
battingCareerPerformance <- plot_grid(p_avg, p_obp, p_slg, p_ops, ncol = 2, align = 'v')

# b. Top 200 batter
# Rank players based on AVG, OPS and HR, then select the top 200
top_200_batter <- batting_career_summary %>%
  arrange(desc(Career_AVG), desc(Career_OPS), desc(Total_HR)) %>%
  distinct(playerID, .keep_all = TRUE) %>%  # Ensuring unique players, keeping the row with the highest OPS
  slice_head(n = 200)

# Merge with Master and select columns except Total_H, Total_AB, Total_BB, Total_2B, Total_3B, Total_HBP, Total_SF. 

top_200_batter_info <- top_200_batter %>%
  left_join(master, by = "playerID") %>%
  select(-c(Total_H, Total_AB, Total_BB, Total_2B, Total_3B, Total_HBP, Total_SF))

# View the top 20 players
head(top_200_batter_info)

# Plots
p_weight_with_data <- ggplot(top_200_batter_info, aes(x = factor(1), y = weight)) +
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +  # Hide outliers in boxplot to avoid double plotting
  geom_jitter(aes(color = weight), width = 0.2) +  # Add jittered points with color mapped to weight
  scale_y_continuous(name = "Weight (lbs)") +
  labs(title = "Weight Distribution of Top 200 Batters") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")  # Remove legend for color

# Height Distribution Plot with Actual Data Points
p_height_with_data <- ggplot(top_200_batter_info, aes(x = factor(1), y = height)) +
  geom_boxplot(fill = "lightgreen", outlier.shape = NA) +  # Hide outliers in boxplot to avoid double plotting
  geom_jitter(aes(color = height), width = 0.2) +  # Add jittered points with color mapped to height
  scale_y_continuous(name = "Height (inches)") +
  labs(title = "Height Distribution of Top 200 Batters") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")  # Remove legend for color

# Display the plots in a 1x2 format
battingBodyStats <- plot_grid(p_weight_with_data, p_height_with_data, ncol = 2, align = 'v')

# c. The Debut, Retire Age and Career Length of Top 200 Players

top_200_batter_info_age <- top_200_batter_info %>%
  mutate(
    # Extract the year from debut and finalGame dates
    debutYear = as.numeric(substr(debut, 1, 4)),
    finalYear = as.numeric(substr(finalGame, 1, 4)),
    # Calculate Debut Age, Retirement Age, and Career Length
    Debut_Age = debutYear - birthYear,
    Retirement_Age = finalYear - birthYear,
    Career_Length = finalYear - debutYear
  )


# View the first few rows of the aggregated information
head(top_200_batter_info_age)

# Plots
# Plot for Debut Age
p_debut_age <- ggplot(top_200_batter_info_age, aes(x = Debut_Age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Batter Debut Age Distribution", x = "Debut Age", y = "Count") +
  theme_minimal()

# Plot for Retirement Age
p_retire_age <- ggplot(top_200_batter_info_age, aes(x = Retirement_Age)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Batter Retirement Age Distribution", x = "Retirement Age", y = "Count") +
  theme_minimal()

# Plot for Career Length
p_career_length <- ggplot(top_200_batter_info_age, aes(x = Career_Length)) +
  geom_histogram(binwidth = 1, fill = "lightcoral", color = "black") +
  labs(title = "Batter Career Length Distribution", x = "Career Length", y = "Count") +
  theme_minimal()

# Arrange the plots in a single image
battingAgeMetrics <- plot_grid(p_debut_age, p_retire_age, p_career_length, ncol = 2, align = 'v')

# 2.2 Fielding

# a. Career Performance Overview
# Group data by playerID and then aggregate the data

fielding_career_summary <- fielding %>%
  mutate(
    FPCT = (PO + A) / (PO + A + E)
  ) %>%
  group_by(playerID) %>%
  summarise(
    Total_G = sum(G, na.rm = TRUE),
    Total_PO = sum(PO, na.rm = TRUE),
    Total_A = sum(A, na.rm = TRUE),
    Total_E = sum(E, na.rm = TRUE),
    Total_DP = sum(DP, na.rm = TRUE),
    Avg_FPCT = mean(FPCT, na.rm = TRUE)
  )

# Summary Statistics
fielding_summary_stats <- summary(fielding_career_summary[c("Total_DP", "Avg_FPCT")])


# Plots

# Plot for Fielding Percentage Distribution
p_fpct <- ggplot(fielding_career_summary, aes(x = Avg_FPCT)) +
  geom_histogram(binwidth = 0.01, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Average Fielding Percentage", x = "Average Fielding Percentage", y = "Frequency") +
  theme_minimal()

# Plot for Errors Distribution
p_errors <- ggplot(fielding_career_summary, aes(x = Total_DP)) +
  geom_histogram(binwidth = 1, fill = "lightcoral", color = "black") +
  labs(title = "Distribution of Total Double Plays", x = "Total Errors", y = "Frequency") +
  theme_minimal()

# Display the plots
fieldingCareerPerformance <- plot_grid(p_fpct, p_errors, ncol = 2, align = 'v')



# b. Top 200 batter
# Rank players based on AVG, OPS and HR, then select the top 200
top_200_fielder <- fielding_career_summary %>%
  arrange(desc(Avg_FPCT), desc(Total_DP)) %>%
  distinct(playerID, .keep_all = TRUE) %>%  # Ensuring unique players, keeping the row with the highest OPS
  slice_head(n = 200)

# Merge with Master and select columns.

top_200_fielder_info <- top_200_fielder %>%
  left_join(master, by = "playerID") %>%
  select(-c(Total_G, Total_PO, Total_A, Total_E))

# View the top 20 players
head(top_200_fielder_info)

# Body Summary Statistics
top_200_fielder_bodystat_summary <- summary(top_200_fielder_info[c("weight", "height")])


# Plots
p_weight_with_data <- ggplot(top_200_fielder_info, aes(x = factor(1), y = weight)) +
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +  # Hide outliers in boxplot to avoid double plotting
  geom_jitter(aes(color = weight), width = 0.2) +  # Add jittered points with color mapped to weight
  scale_y_continuous(name = "Weight (lbs)") +
  labs(title = "Weight Distribution of Top 200 Fielders") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")  # Remove legend for color

# Height Distribution Plot with Actual Data Points
p_height_with_data <- ggplot(top_200_fielder_info, aes(x = factor(1), y = height)) +
  geom_boxplot(fill = "lightgreen", outlier.shape = NA) +  # Hide outliers in boxplot to avoid double plotting
  geom_jitter(aes(color = height), width = 0.2) +  # Add jittered points with color mapped to height
  scale_y_continuous(name = "Height (inches)") +
  labs(title = "Height Distribution of Top 200 Fielders") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")  # Remove legend for color

# Display the plots in a 1x2 format
fieldingBodyStats <- plot_grid(p_weight_with_data, p_height_with_data, ncol = 2, align = 'v')

# c. The Debut, Retire Age and Career Length of Top 200 Players
top_200_fielder_info_age <- top_200_fielder_info %>%
  mutate(
    # Extract the year from debut and finalGame dates
    debutYear = as.numeric(substr(debut, 1, 4)),
    finalYear = as.numeric(substr(finalGame, 1, 4)),
    # Calculate Debut Age, Retirement Age, and Career Length
    Debut_Age = debutYear - birthYear,
    Retirement_Age = finalYear - birthYear,
    Career_Length = finalYear - debutYear
  )


# View the first few rows of the aggregated information
head(top_200_fielder_info_age)

# Plots
# Plot for Debut Age
p_debut_age <- ggplot(top_200_fielder_info_age, aes(x = Debut_Age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Fielder Debut Age Distribution", x = "Debut Age", y = "Count") +
  theme_minimal()

# Plot for Retirement Age
p_retire_age <- ggplot(top_200_fielder_info_age, aes(x = Retirement_Age)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Fielder Retirement Age Distribution", x = "Retirement Age", y = "Count") +
  theme_minimal()

# Plot for Career Length
p_career_length <- ggplot(top_200_fielder_info_age, aes(x = Career_Length)) +
  geom_histogram(binwidth = 1, fill = "lightcoral", color = "black") +
  labs(title = "Fielder Career Length Distribution", x = "Career Length", y = "Count") +
  theme_minimal()

# Arrange the plots in a single image
fieldingAgeMetrics <- plot_grid(p_debut_age, p_retire_age, p_career_length, ncol = 2, align = 'v')


# 2.3 Pitcher

# a. Career Performance Overview
# Group data by playerID and then aggregate the data

pitching_career_summary <- pitching %>%
  group_by(playerID) %>%
  summarise(
    Total_W = sum(W, na.rm = TRUE),
    Total_L = sum(L, na.rm = TRUE),
    Total_SO = sum(SO, na.rm = TRUE),
    Total_IP = sum(IPouts / 3, na.rm = TRUE),
    Avg_ERA = mean(ERA, na.rm = TRUE)
  ) %>%
  ungroup()

summary(pitching_career_summary[c("Total_W", "Total_L", "Total_SO", "Total_IP", "Avg_ERA")])

# Plots
# Plot for Career Wins
p_wins <- ggplot(pitching_career_summary, aes(x = Total_W)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribution of Career Wins", x = "Total Wins", y = "Number of Pitchers") +
  theme_minimal()

# Plot for Career Strikeouts
p_strikeouts <- ggplot(pitching_career_summary, aes(x = Total_SO)) +
  geom_histogram(binwidth = 50, fill = "red", color = "black") +
  labs(title = "Distribution of Career Strikeouts", x = "Total Strikeouts", y = "Number of Pitchers") +
  theme_minimal()

# Plot for Average ERA
p_era <- ggplot(pitching_career_summary, aes(x = Avg_ERA)) +
  geom_histogram(binwidth = 0.5, fill = "green", color = "black") +
  labs(title = "Distribution of Average ERA", x = "Average ERA", y = "Number of Pitchers") +
  xlim(0, 10) +  # Limit x-axis to filter extreme ERA values
  theme_minimal()

pitchingCareerPerformance <- plot_grid(p_wins, p_strikeouts, p_era, ncol = 2, align = 'v')



top_200_pitcher <- pitching_career_summary %>%
  filter(Total_IP >= 1000) %>%  # Filter for pitchers with substantial careers
  mutate(Score = Total_W + Total_SO - Avg_ERA * 100) %>%  # Example scoring: Wins + Strikeouts - 100 * ERA
  arrange(desc(Score)) %>%  # Arrange in descending order of the score
  slice_head(n = 200)  # Select the top 200

# Merge with Master and select columns.

top_200_pitcher_info <- top_200_pitcher %>%
  left_join(master, by = "playerID") %>%
  select(-c(Total_W, Total_L, Total_SO, Total_IP))

# View the top 20 players
head(top_200_pitcher_info)

# Body Summary Statistics
top_200_pitcher_bodystat_summary <- summary(top_200_pitcher_info[c("weight", "height")])


# Plots
p_weight_with_data <- ggplot(top_200_pitcher_info, aes(x = factor(1), y = weight)) +
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +  # Hide outliers in boxplot to avoid double plotting
  geom_jitter(aes(color = weight), width = 0.2) +  # Add jittered points with color mapped to weight
  scale_y_continuous(name = "Weight (lbs)") +
  labs(title = "Weight Distribution of Top 200 Pitchers") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")  # Remove legend for color

# Height Distribution Plot with Actual Data Points
p_height_with_data <- ggplot(top_200_pitcher_info, aes(x = factor(1), y = height)) +
  geom_boxplot(fill = "lightgreen", outlier.shape = NA) +  # Hide outliers in boxplot to avoid double plotting
  geom_jitter(aes(color = height), width = 0.2) +  # Add jittered points with color mapped to height
  scale_y_continuous(name = "Height (inches)") +
  labs(title = "Height Distribution of Top 200 Pitchers") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")  # Remove legend for color

# Display the plots in a 1x2 format
pitchingBodyStats <- plot_grid(p_weight_with_data, p_height_with_data, ncol = 2, align = 'v')



# c. The Debut, Retire Age and Career Length of Top 200 Players
top_200_pitcher_info_age <- top_200_pitcher_info %>%
  mutate(
    # Extract the year from debut and finalGame dates
    debutYear = as.numeric(substr(debut, 1, 4)),
    finalYear = as.numeric(substr(finalGame, 1, 4)),
    # Calculate Debut Age, Retirement Age, and Career Length
    Debut_Age = debutYear - birthYear,
    Retirement_Age = finalYear - birthYear,
    Career_Length = finalYear - debutYear
  )


# View the first few rows of the aggregated information
head(top_200_pitcher_info_age)

# Plots
# Plot for Debut Age
p_debut_age <- ggplot(top_200_pitcher_info_age, aes(x = Debut_Age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Pitcher Debut Age Distribution", x = "Debut Age", y = "Count") +
  theme_minimal()

# Plot for Retirement Age
p_retire_age <- ggplot(top_200_pitcher_info_age, aes(x = Retirement_Age)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Pitcher Retirement Age Distribution", x = "Retirement Age", y = "Count") +
  theme_minimal()

# Plot for Career Length
p_career_length <- ggplot(top_200_pitcher_info_age, aes(x = Career_Length)) +
  geom_histogram(binwidth = 1, fill = "lightcoral", color = "black") +
  labs(title = "Pitcher Career Length Distribution", x = "Career Length", y = "Count") +
  theme_minimal()

# Arrange the plots in a single image
pitchingAgeMetrics <- plot_grid(p_debut_age, p_retire_age, p_career_length, ncol = 2, align = 'v')

# 2.4 Salaries

# Select playerID and teamID from top 200 batters, pitchers, and fielders
top_batters_ids <- top_200_batter %>% select(playerID)
top_pitchers_ids <- top_200_pitcher %>% select(playerID)
top_fielders_ids <- top_200_fielder %>% select(playerID)

# Combine the three tables
combined_players <- bind_rows(top_batters_ids, top_pitchers_ids, top_fielders_ids)

# Remove duplicate rows, if any
combined_players_unique <- combined_players %>% distinct()


# Aggregate player salaries across years
avg_player_salaries <- salaries %>%
  group_by(playerID) %>%
  summarise(Avg_Salary = mean(salary, na.rm = TRUE)) %>%
  ungroup()

avg_player_salaries <- avg_player_salaries %>%
  filter(Avg_Salary != 0)
head(avg_player_salaries)

# Merge salary with Top players
combined_players_salaries <- left_join(combined_players_unique, avg_player_salaries, by = c("playerID"))

library(tidyr)
top_players_salaries <- combined_players_salaries %>%
  drop_na(Avg_Salary)

head(top_players_salaries)


# Plot
AverageSalaries <- ggplot(top_players_salaries, aes(x = Avg_Salary)) +
  geom_histogram(
    binwidth = 500000,  # Adjust binwidth for finer granularity if necessary
    fill = "steelblue",  # Change fill color for aesthetic preference
    color = "white"  # Change line color for bins to white for better contrast
  ) +
  scale_x_continuous(
    breaks = seq(0, max(top_players_salaries$Avg_Salary, na.rm = TRUE), by = 500000), 
    labels = function(x) format(x, big.mark = ",", scientific = FALSE) 
  ) +
  labs(
    title = "Distribution of Average Salaries Among Top Players",
    x = "Average Salary",
    y = "Count of Players",
    caption = "Data Source: MLB Salaries"  # Add a data source caption for context
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )

# 2. Team performance analysis

# a. Top 10 teams in the post series after 1960
# Aggregate wins and appearances in postseason
series_post <- series_post %>%
  filter(yearID>=1960)
postseason_stats <- series_post %>%
  mutate(winner = teamIDwinner, loser = teamIDloser) %>%
  select(yearID, winner, loser, wins, losses) %>%
  group_by(winner) %>%
  summarise(TotalWins = sum(wins, na.rm = TRUE),
            Appearances = n(),
            .groups = 'drop') %>%
  arrange(desc(TotalWins))

# View the top 10 teams by total postseason wins
top_10_teams_postseason <- head(postseason_stats, 10)
headerPanel(top_10_teams_postseason)


# Plotting the results
TotalWinsTop10 <- ggplot(top_10_teams_postseason, aes(x = reorder(winner, TotalWins), y = TotalWins, fill = Appearances)) +
  geom_col() +
  labs(title = "Top 10 Teams in Postseason by Total Wins",
       x = "Team",
       y = "Total Wins") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x labels for better readability



# The overall performance of top 10 teams after 1960
# Filter data after 1960
teams <- teams %>%
  filter(yearID >= 1960)

# Aggregate data
team_aggregates <- teams %>%
  group_by(teamID) %>%
  summarise(
    Total_Wins = sum(W, na.rm = TRUE),
    World_Series_Wins = sum(WSWin == "Y", na.rm = TRUE),
    Total_Games = sum(G, na.rm = TRUE),
    Average_Win_Pct = mean(W / G, na.rm = TRUE)
  )

# Merge with the top 10 teams
team_top10_performance <- top_10_teams_postseason %>%
  left_join(teams, by = c("winner" = "teamID"))

# Print the results
head(team_top10_performance)


team_top10_performance$Rank <- as.numeric(as.character(team_top10_performance$Rank))

# Analyze the rank of each team by year
team_ranks <- team_top10_performance %>%
  select(winner, yearID, Rank) %>%
  arrange(winner, yearID)

# Draw a plot of rank trends over years
Top10Rank <- ggplot(team_top10_performance, aes(x = yearID, y = Rank, group = winner, color = winner)) +
  geom_line() +
  geom_point() +
  scale_y_reverse() +  # Rank is reversed so lower numbers are at the top
  labs(title = "Rank Trends Over Years for Top 10 Teams", x = "Year", y = "Rank") +
  theme_minimal() +
  theme(legend.position = "bottom")

top_10_rank_summary <- summarise(team_top10_performance)


plotTotalWins <- plot_grid(TotalWinsTop10, Top10Rank, ncol = 1, align = 'v')



# b. Check the performance stability of top 10 teams after 1960
# Ranking stability
ranking_stability <- team_top10_performance %>%
  group_by(winner) %>%
  summarise(
    Avg_Rank = mean(Rank, na.rm = TRUE),
    SD_Rank = sd(Rank, na.rm = TRUE)
  )

# Win percentage stability
win_pct_stability <- team_top10_performance %>%
  mutate(Win_Pct = W / G) %>%
  group_by(winner) %>%
  summarise(
    Avg_Win_Pct = mean(Win_Pct, na.rm = TRUE),
    SD_Win_Pct = sd(Win_Pct, na.rm = TRUE)
  )

# Playoff appearances
playoff_appearances <- team_top10_performance %>%
  group_by(winner) %>%
  summarise(
    Playoff_Appearances = sum(WSWin == "Y" | LgWin == "Y" | DivWin == "Y", na.rm = TRUE)
  )

# Combine all measures into one data frame for analysis
stability_analysis <- left_join(ranking_stability, win_pct_stability, by = "winner") %>%
  left_join(playoff_appearances, by = "winner")

# View the combined stability analysis
head(stability_analysis)

# Plotting for visual analysis
# Plot for Ranking Stability
rankStability <- ggplot(ranking_stability, aes(x = winner, y = SD_Rank, fill = winner)) +
  geom_bar(stat = "identity") +
  labs(title = "Ranking Stability of Top 10 Teams", x = "Team", y = "Standard Deviation of Rank") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot for Win Percentage Stability
pctStability <- ggplot(win_pct_stability, aes(x = winner, y = SD_Win_Pct, fill = winner)) +
  geom_bar(stat = "identity") +
  labs(title = "Win Percentage Stability of Top 10 Teams", x = "Team", y = "Standard Deviation of Win Percentage") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot for Playoff Appearances
playoffStability <- ggplot(playoff_appearances, aes(x = winner, y = Playoff_Appearances, fill = winner)) +
  geom_bar(stat = "identity") +
  labs(title = "Playoff Appearances of Top 10 Teams", x = "Team", y = "Playoff Appearances") +
  theme_minimal() +
  theme(legend.position = "none")


# Rerank the top 10 teams according to the stability
# Assuming 'stability_analysis' contains the combined stability data
stability_analysis <- stability_analysis %>%
  mutate(
    # Normalize the standard deviations by subtracting from 1 since lower is better
    Norm_SD_Rank = 1 - (SD_Rank / max(SD_Rank, na.rm = TRUE)),
    Norm_SD_Win_Pct = 1 - (SD_Win_Pct / max(SD_Win_Pct, na.rm = TRUE)),
    # Combine normalized stability measures into a single score
    Stability_Score = (Norm_SD_Rank + Norm_SD_Win_Pct) / 2
  ) %>%
  arrange(desc(Stability_Score), desc(Playoff_Appearances))  # Arrange by stability score and then playoff appearances

# Add a rank column for stability
stability_analysis <- mutate(stability_analysis, Stability_Rank = row_number())

# Now print the stability analysis with ranks
head(stability_analysis[, c("winner", "Stability_Rank", "Stability_Score", "Playoff_Appearances")])

# plot
Top10Stability <- ggplot(stability_analysis, aes(x = reorder(winner, Stability_Rank), y = Stability_Score, fill = winner)) +
  geom_col() +
  labs(title = "Stability Ranking of Top 10 Teams", x = "Team", y = "Stability Score") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_discrete(limits = stability_analysis$winner)  # Order the teams by their stability rank on the x-axis

plotStability <- plot_grid(rankStability, pctStability, playoffStability, Top10Stability, ncol = 2, align = 'v')



# c. Compare the stability rank and total wins rank
# Add ranking column to total wins ranking table
top_10_teams_postseason <- top_10_teams_postseason %>%
  arrange(desc(TotalWins)) %>%
  mutate(Total_Wins_Rank = row_number())

# Merge the datasets on the team identifier (winner)
comparison_data <- merge(top_10_teams_postseason, stability_analysis, by = "winner")

# Create a scatter plot to compare the ranks
plotComparison <- ggplot(comparison_data, aes(x = Stability_Rank, y = Total_Wins_Rank, label = winner)) +
  geom_point(size = 4, color = "darkblue") +  # Increase point size and set color
  geom_text(aes(label = winner), vjust = -0.5, hjust = 0.5, color = "black", size = 3.5, angle = 45) +  # Adjust label position, size, and angle
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +  # Add a reference line where ranks are equal
  scale_x_continuous(breaks = 1:max(comparison_data$Stability_Rank), labels = as.character(1:max(comparison_data$Stability_Rank))) +  # Ensure whole number breaks for ranks
  scale_y_continuous(breaks = 1:max(comparison_data$Total_Wins_Rank), labels = as.character(1:max(comparison_data$Total_Wins_Rank))) +  # Ensure whole number breaks for ranks
  scale_y_reverse(limits = c(max(comparison_data$Total_Wins_Rank), 1)) +  # So that the best rank is at the top
  scale_x_reverse(limits = c(max(comparison_data$Stability_Rank), 1)) +  # So that the best rank is at the top
  labs(
    title = "Comparison of Stability Rank and Total Wins Rank for Top 10 Teams",
    subtitle = "A lower rank number indicates a better performance",
    x = "Stability Rank (1 is most stable)",
    y = "Total Wins Rank (1 is most wins)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  ) +
  coord_fixed(ratio = 1)  # Keep a 1:1 aspect ratio

# 3. Career Achievements of Players

# a. Select the top 20 players according to the times they won the prize

# Count the number of prizes for each player and sort in descending order
player_prize_counts <- awards %>%
  count(playerID, sort = TRUE)

# Select the top 20 players
top_20_awardees <- head(player_prize_counts, 20)

# Print the top 20 players
head(top_20_awardees)

top20Awardees <- ggplot(top_20_awardees, aes(x = reorder(playerID, -n), y = n, fill = playerID)) +
  geom_col() +  # Use column chart to represent the counts
  labs(title = "Top 20 Players by Number of Prizes Won", x = "Player ID", y = "Number of Prizes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility



# b. Compare the times they won the awards with their career length
master <- master %>%
  mutate(
    # Extract the year from debut and finalGame dates
    debutYear = as.numeric(substr(debut, 1, 4)),
    finalYear = as.numeric(substr(finalGame, 1, 4)),
    # Calculate Debut Age, Retirement Age, and Career Length
    Career_Length = finalYear - debutYear
  )

top_20_with_career <- merge(top_20_awardees, master, by = "playerID")
head(top_20_with_career)
# Plot
awardsCareerlength <- ggplot(top_20_with_career, aes(x = Career_Length, y = n, label = nameGiven)) +
  geom_point(size = 4, color = "blue") +  # Increase point size and change color
  geom_text(aes(label = nameGiven), vjust = 1.5, hjust = 1.5, color = "darkred", size = 3.5, angle = 45) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +  # Add a linear model trend line
  labs(
    title = "Comparison of Award Counts and Career Lengths for Top 20 Awardees",
    x = "Career Length (Years)",
    y = "Number of Awards"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "none"
  )



# Merge salary with Top players
combined_awardees_salaries <- left_join(player_prize_counts, avg_player_salaries, by = c("playerID"))
print(combined_awardees_salaries)
awardees_salaries <- combined_awardees_salaries %>%
  drop_na(Avg_Salary)
head(awardees_salaries)

# Plot

awardsSalaries <- ggplot(awardees_salaries, aes(x = n, y = Avg_Salary)) +
  geom_point(aes(color = playerID), size = 3) +  # Color coding by player ID
  geom_smooth(method = "lm", se = FALSE, color = "lightblue") +  # Add a linear regression line without a confidence interval
  scale_y_continuous(labels = scales::dollar_format()) +  # Format the y-axis as dollar amounts
  labs(title = "Relationship Between Awards Count and Average Salary",
       x = "Number of Awards Won",
       y = "Average Annual Salary") +
  theme_minimal() +
  theme(legend.position = "none")





# Build website
library(shinyjs)
ui <- dashboardPage(
  dashboardHeader(title = "Baseball Statistics Analysis Dashboard",
                  tags$li(class = "dropdown", 
                          style = "color: white; padding: 10px;", 
                          "Created by Wensi Chen")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Datasets", tabName = "datasets", icon = icon("database")),
      menuItem("Players Analysis", tabName = "players_analysis", icon = icon("user")),
      menuItem("Teams Analysis", tabName = "teams_analysis", icon = icon("users")),
      menuItem("Achievements Analysis", tabName = "achievements_analysis", icon = icon("trophy"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "datasets",
              h2("Dataset Overview"),
              selectInput("dataset", "Choose a dataset:",
                          choices = c("Master", "Batting", "Batting Post", "Fielding", "Pitching", "Pitching Post", "Salaries", "Teams", "Series Post", "Awards")),
              tableOutput("dataView"),
              tags$p("Select a dataset from the dropdown to view its first few entries. This helps in quickly understanding the data structure and key fields."),
              h3("Description"),
              tags$p("The database contains the player statistics in different positions (Batting, Fielding, Pitching), the statistics of different teams (Teams, SeriesPost), personal information of all players (Master) and their salaries (Salaries).
                     Each player is assigned a unique number (playerID). All of the information relating to that player is tagged with his playerID."),
              h3("Introduction"),
              tags$p(HTML("The project analyzes the datasets from aspects of players, teams and achievements. The main content is as follow:<br><br>")),
              h4("1. Players Analysis"),
              tags$p(HTML("1.1 Career Performance Overview <br>1.2 Body Stats for Top 200 Players <br>1.3 The Debut, Retire Age and Career Length of Top 200 Players <br>1.4 The Salaries of the Top 200 Players<br> Tables used: Master, Salaries, Batting, BattingPost, Fielding, Pitching, PitchingPost")),
              h4("2. Teams Analysis"),
              tags$p(HTML("2.1 The overall performance of top 10 teams (Total wins) after 1960<br> 2.2 Check the performance stability of top 10 teams<br>2.3 Compare the stability rank and total wins rank<br> Tables used: Teams, SeriesPost")),
              h4("3. Achievements Analysis"),
              tags$p(HTML("3.1 Select the top 20 players according to the times they won the prize <br>3.2 Compare the times they won the awards with their career length<br>3.3 Relationship Between Awards Count and Average Salary<br> Tables used: Master, AwardsPlayers, Salaries"))
              ),
      tabItem(tabName = "players_analysis",
              h2("Players Analysis"),
              selectInput("positionSelect", "Select Position:",
                          choices = c("Batting", "Fielding", "Pitching")),
              uiOutput("positionContent"),  # Dynamic content based on position
              tags$p("Detailed analysis based on player positions. Choose a position to explore specific metrics and performance indicators relevant to each role.")
      ),
      tabItem(tabName = "teams_analysis",
              h2("Teams Performance Analysis"),
              tabsetPanel(
                tabPanel("Top 10",
                         h3("2.1 The overall performance of top 10 teams (Total wins) after 1960"),
                         plotOutput("plotTotalWins"),
                         tags$p("This visualization ranks the top 10 teams by total wins in the post series since 1960, providing insights into long-term team success.")),
                tabPanel("Stability",
                         h3("2.2 Check the performance stability of top 10 teams"),
                         plotOutput("plotStability"),
                         tags$p(HTML("Evaluates the stability of team performances over time by examining the variability in their annual wins.<br><br>The stability is evaluated by three criteria: 
                                     1. standard deviation of rank 2. standard deviation of win percentage 3. playoff appearance.<br>Then we create a new criterion called stability score and get 
                                     a new rank of the top 10 teams according to the stability score and playoff appearance. <br><br>Formula: Stability Score = (Norm_SD_Rank + Norm_SD_Win_Pct) / 2"
                         ))
                         ),
                tabPanel("Comparison",
                         h3("2.3 Compare the stability rank and total wins rank"),
                         plotOutput("plotComparison"),
                         tags$p("Compares teams based on their ranks in stability and total wins, identifying the performance of top teams according to both total wins and stability."))
              )),
      tabItem(tabName = "achievements_analysis",
              h2("Achievements Analysis"),
              tabsetPanel(
                tabPanel("Top 20",
                         h3("3.1 Select the top 20 players according to the times they won the prize"),
                         plotOutput("top20Awardees"),
                         tags$p("Showcases players with the highest number of awards, highlighting their dominance and recognition in the sport.")),
                tabPanel("Awards VS Career Length",
                         h3("3.2 Compare the times they won the awards with their career length"),
                         plotOutput("awardsCareerlength"),
                         tags$p("Analyzes the correlation between the length of players' careers and the number of awards they have received.")),
                tabPanel("Awards VS Salaries",
                         h3("3.3 Relationship Between Awards Count and Average Salary"),
                         plotOutput("awardsSalaries"),
                         tags$p("Investigates whether there is a relationship between the number of awards a player has won and their average salary, exploring potential financial benefits of high performance."))
              ))
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$toggleDesc, {
    shinyjs::toggle(id = "description")  # Toggle description
  })
  
  observeEvent(input$toggleIntro, {
    shinyjs::toggle(id = "introduction")  # Toggle introduction
  })
  
  output$dataView <- renderTable({
    dataset <- switch(input$dataset,
                      "Master" = master,
                      "Batting" = battinga,
                      "Batting Post" = battingpost,
                      "Fielding" = fielding,
                      "Pitching" = pitchinga,
                      "Pitching Post" = pitchingpost,
                      "Salaries" = salaries,
                      "Teams" = teams,
                      "Series Post" = series_post,
                      "Awards" = awards)
    head(dataset) 
  })
  
  # Player Analysis Plots
  output$positionContent <- renderUI({
    if (input$positionSelect == "Batting") {
      tabsetPanel(
        tabPanel("Career Performance",
                 h3("1.1 Career Performance Overview"),
                 plotOutput("battingCareerPerformance"),
                 tags$p(HTML("Here I build the following criteria to evaluate the performance of batters and select the top 200 according to these criteria:<br><br>1. Batting Average(AVG): Measures how often a player gets a hit.<br>Formula: AVG = Hits (H) / At-Bats (AB)<br>Interpretation: Higher values indicate better performance. A batting average over .300 is considered excellent.<br><br>2. On-Base Percentage(OBP): Measures how often a player reaches base.<br>Formula: OBP = (H + Walks (BB) + Hit by Pitch (HBP)) / (AB + BB + HBP + Sacrifice Flies (SF))<br>Interpretation: Reflects a player's ability to avoid making outs. An OBP over .370 is considered good.<br><br>3. Slugging Percentage (SLG): Measures a player's batting power.<br>Formula: SLG = Total Bases (TB) / AB<br>Interpretation: SLG accounts for the total bases a player records per at-bat, rewarding extra-base hits. A SLG over .450 is typically strong.<br><br>4. On-Base Plus Slugging(OPS): Combines OBP and SLG to provide a single metric.<br>Formula: OPS = OBP + SLG<br>Interpretation: A higher OPS indicates better overall offensive performance. An OPS above .800 is considered good."))),
        tabPanel("Body Stats",
                 h3("1.2 Body Stats for Top 200 Players"),
                 plotOutput("battingBodyStats"),
                 tags$p("Show the distribution of hight and weight of the top 200 batters.")),
        tabPanel("Age Metrics",
                 h3("1.3 The Debut, Retire Age and Career Length of Top 200 Players"),
                 plotOutput("battingAgeMetrics"),
                 tags$p("Show the distribution of debut, retire age and career length of the top 200 batters.")),
        tabPanel("Salaries",
                 h3("1.4 The Salaries of the Top Players"),
                 plotOutput("Salaries"),
                 tags$p("Show the average salaries of the top 200 batters, fielders and pitchers."))
      )
    } else if (input$positionSelect == "Fielding") {
      tabsetPanel(
        tabPanel("Career Performance", 
                 h3("1.1 Career Performance Overview"),
                 plotOutput("fieldingCareerPerformance"),
                 tags$p(HTML("Here I build the following criteria to evaluate the performance of fielders and select the top 200 according to these criteria:<br><br>1. Fielding Percentage (FPCT): Evaluate the reliability of a player in making plays without committing errors.<br>Formula: FPCT = Putouts (PO)+Assists (A) / Putouts (PO)+Assists (A)+Errors (E)<br>Interpretation: The Fielding Percentage ranges from 0 to 1, where 1 or 100% represents perfect fielding with no errors.A higher FPCT indicates a more reliable and efficient defender who makes fewer errors relative to the number of plays they are involved in.<br><br>2. Total Errors (TE): Refers to the total number of times a fielder fails to make a play that could have been made with ordinary effort, leading to the opposition gaining an advantage they would not have had otherwise.  <br>Interpretation: A high number of errors might suggest that a player has difficulties with fielding, which could be due to many factors such as poor hand-eye coordination, inadequate fielding technique, or even external factors like playing conditions."))),
        tabPanel("Body Stats",
                 h3("1.2 Body Stats for Top 200 Players"),
                 plotOutput("fieldingBodyStats"),
                 tags$p("Show the distribution of hight and weight of the top 200 fielders.")),
        tabPanel("Age Metrics", 
                 h3("1.3 The Debut, Retire Age and Career Length of Top 200 Players"),
                 plotOutput("fieldingAgeMetrics"),
                 tags$p("Show the distribution of debut, retire age and career length of the top 200 fielders.")),
        tabPanel("Salaries", 
                 h3("1.4 The Salaries of the Top Players"),
                 plotOutput("Salaries"),
                 tags$p("Show the average salaries of the top 200 batters, fielders and pitchers."))
      )
    } else if (input$positionSelect == "Pitching") {
      tabsetPanel(
        tabPanel("Career Performance", 
                 h3("1.1 Career Performance Overview"),
                 plotOutput("pitchingCareerPerformance"),
                 tags$p(HTML("Here I use the folloowing criteria to evaluate the performance of fielders and select the top 200 according to these criteria:<br><br>Wins (W):A win is credited to a pitcher who is the pitcher of record when his team takes the lead and subsequently holds onto that lead while he is in the game. A starting pitcher must pitch at least five innings to qualify for a win.<br>Interpretation: While a win is an important statistic, its value can be somewhat misleading as it also depends on the team’s offensive performance and the bullpen's effectiveness. Therefore, it's not solely indicative of a pitcher’s performance.<br><br>Earned Run Average (ERA):ERA calculates the average number of earned runs a pitcher allows per nine innings pitched.<br>Fomula: ERA = (Earned Runs(ER)/Innings Pitched(IP))×9<br>Interpretation:  A lower ERA is indicative of better performance, showing that the pitcher allows fewer runs to score. It helps isolate the pitcher’s performance by focusing on earned runs, hence offering a clearer view of how well a pitcher controls the game.<br><br>Strikeouts (SO):A strikeout occurs when a pitcher throws three strikes to a batter during his at-bat, leading to the batter's dismissal.<br>Interpretation: High strikeout totals often correlate with elite pitching performance. Pitchers who can achieve high strikeouts typically possess superior pitching skills, including velocity and pitch movement, and are less dependent on the fielding behind them to get outs."))),
        tabPanel("Body Stats", 
                 h3("1.2 Body Stats for Top 200 Players"),
                 plotOutput("pitchingBodyStats"),
                 tags$p("Show the distribution of hight and weight of the top 200 pitchers.")),
        tabPanel("Age Metrics", 
                 h3("1.3 The Debut, Retire Age and Career Length of Top 200 Players"),
                 plotOutput("pitchingAgeMetrics"),
                 tags$p("Show the distribution of debut, retire age and career length of the top 200 pitchers.")),
        tabPanel("Salaries", 
                 h3("1.4 The Salaries of the Top Players"),
                 plotOutput("Salaries"),
                 tags$p("Show the average salaries of the top 200 batters, fielders and pitchers."))
      )
    }
  })
  
  # batting
  output$battingCareerPerformance <- renderPlot({ battingCareerPerformance })
  output$battingBodyStats <- renderPlot({battingBodyStats})
  output$battingAgeMetrics <- renderPlot({battingAgeMetrics})
  output$Salaries <- renderPlot({AverageSalaries})
  # fielding
  output$fieldingCareerPerformance <- renderPlot({ fieldingCareerPerformance })
  output$fieldingBodyStats <- renderPlot({fieldingBodyStats})
  output$fieldingAgeMetrics <- renderPlot({fieldingAgeMetrics})
  output$fieldingSalaries <- renderPlot({AverageSalaries})
  # pitching
  output$pitchingCareerPerformance <- renderPlot({ pitchingCareerPerformance })
  output$pitchingBodyStats <- renderPlot({pitchingBodyStats})
  output$pitchingAgeMetrics <- renderPlot({pitchingAgeMetrics})
  output$pitchingSalaries <- renderPlot({AverageSalaries})
  
  # Team Analysis
  output$plotTotalWins <- renderPlot({ plotTotalWins })
  output$plotStability <- renderPlot({ plotStability })
  output$plotComparison <- renderPlot({ plotComparison })
  
  # Achievements Analysis
  output$top20Awardees <- renderPlot({ top20Awardees })
  output$awardsCareerlength <- renderPlot({ awardsCareerlength })
  output$awardsSalaries <- renderPlot({ awardsSalaries })
}

shinyApp(ui, server)