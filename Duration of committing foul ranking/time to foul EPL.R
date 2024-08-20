# Load necessary libraries
library(worldfootballR)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)

# Set working directory
setwd("D:/Code/R/Soccer")

# Retrieve team statistics data for the 2019-20 English Premier League season
defense_stats <- fb_season_team_stats("ENG", "M", 2020, "1st", "defense")
possession_stats <- fb_season_team_stats("ENG", "M", 2020, "1st", "possession")
miscellaneous_stats <- fb_season_team_stats("ENG", "M", 2020, "1st", "misc")
league_table <- fb_season_team_stats("ENG", "M", 2020, "1st", "league_table")

# Filter and select relevant columns from the miscellaneous and possession data
fouls_data <- miscellaneous_stats %>% 
  filter(Team_or_Opponent == 'team') %>% 
  select(Team = Squad, Fouls = Fls)

possession_data <- possession_stats %>% 
  filter(Team_or_Opponent == 'team') %>% 
  select(Team = Squad, Possession_Percentage = Poss)

# Filter and select relevant columns from the league table data
league_ranking <- league_table %>%
  filter(Team_or_Opponent == 'team') %>%
  select(Team = Squad, League_Rank = Rk)

# Merge the data frames by the 'Team' column
combined_data <- fouls_data %>%
  left_join(possession_data, by = "Team") %>%
  left_join(league_ranking, by = "Team")

# Calculate possession time and opponent's possession time in seconds
combined_data <- combined_data %>%
  mutate(
    Possession_Time_Secs = ((Possession_Percentage / 100) * 90) * 60,
    Opponent_Possession_Time_Secs = (((100 - Possession_Percentage) / 100) * 90) * 60
  )

# Calculate fouls per game, time to foul, and rank teams based on time to foul
combined_data <- combined_data %>%
  mutate(
    Fouls_Per_Game = Fouls / 38,
    Time_to_Foul_Secs = Opponent_Possession_Time_Secs / Fouls_Per_Game,
    Time_to_Foul_Rank = rank(Time_to_Foul_Secs, ties.method = "first")
  ) %>%
  arrange(Time_to_Foul_Rank)

# Create a table plot without row names
table_plot <- tableGrob(combined_data, rows = NULL)

# Reshape the data for plotting (long format)
combined_data_long <- combined_data %>%
  select(Team, Time_to_Foul_Rank, League_Rank) %>%
  pivot_longer(cols = c(Time_to_Foul_Rank, League_Rank), names_to = "Measure", values_to = "Value")

# Create a vertical bar plot comparing Time to Foul Rank and League Rank
plot <- ggplot(combined_data_long, aes(x = reorder(Team, -Value), y = Value, fill = Measure)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("orange", "skyblue"), 
                    name = "Measure", 
                    labels = c("Team Ranking", "Time to Foul Rank")) +  # Reversed the labels
  labs(
    title = "Team Time to Foul Rank vs. League Rank (2019-20 Season)",
    x = "Team",
    y = "Ranking"
  ) +
  scale_y_continuous(breaks = 1:20) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
    legend.position = "top"
  )

# Save the plot as an image file
ggsave(filename = "team_foul_rank_vs_league_rank_2019_20x.jpg", plot = plot, width = 12, height = 8, dpi = 300)

