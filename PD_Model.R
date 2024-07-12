library(tidyverse)
library(dplyr)
library(ggplot2)
library(baseballr)
library(openxlsx)
library(mlbplotR)
library(gtExtras)
library(gt)

FG <- try(fg_batter_leaders(startseason = 2024, endseason = 2024, qual = 100))

FG_TT <- try(fg_batter_leaders(startseason = 2023, endseason = 2023, qual = 100)) 

FG_PD <- FG %>%
  select(PlayerName, team_name, xMLBAMID, BB_pct, K_pct, `O-Swing_pct`, `Z-Swing_pct`, `Z-Contact_pct`, Contact_pct,
         SwStr_pct, Swing_pct, CStr_pct, `C+SwStr_pct`, `F-Strike_pct`) %>%
  mutate(K_pct_minus_BB_pct = round(K_pct - BB_pct, 3),
         team_name = case_when(
           team_name == "TBR" ~ "TB",
           team_name == "CHW" ~ "CWS",
           team_name == "WSN" ~ "WSH",
           team_name == "ARI" ~ "AZ",
           team_name == "KCR" ~ "KC",
           team_name == "SDP" ~ "SD",
           team_name == "SFG" ~ "SF",
           TRUE ~ team_name
         ))

###2024###
K_Minus_BB <- K_pct_minus_BB_pct ~ `O-Swing_pct` + `Z-Swing_pct` + Contact_pct +
  SwStr_pct + Swing_pct + `F-Strike_pct` + CStr_pct + `C+SwStr_pct`

# Fit the linear regression model
K_Minus_BB_model <- lm(K_Minus_BB, data = FG_PD)
summary(K_Minus_BB_model)
plot(K_Minus_BB_model)
coefficients(K_Minus_BB_model)
predictions <- predict(K_Minus_BB_model, newdata = FG_PD)


actual_K_Minus_BB_pct <- FG_PD$K_pct_minus_BB_pct
player_full_names <- FG_PD$PlayerName

Team_Name<- FG_PD$team_name
xMLBAMID <- FG_PD$xMLBAMID

# Create results dataframe
Results <- data.frame(Predicted_K_Minus_BB_pct = predictions,
                          Actual_K_Minus_BB_pct = actual_K_Minus_BB_pct,
                          Player_Full_Name = player_full_names,
                      Team = Team_Name,
                      xMLBAMID = xMLBAMID)

Results$Predicted_K_Minus_BB_pct <- round(Results$Predicted_K_Minus_BB_pct * 100, 1)
Results$Actual_K_Minus_BB_pct <- round(Results$Actual_K_Minus_BB_pct * 100, 1)

Results <- Results %>%
  mutate(Actual_Over_Expected = Actual_K_Minus_BB_pct- Predicted_K_Minus_BB_pct)

Results_Top_Ten <- Results %>%
  select(Player_Full_Name, Team, xMLBAMID, Actual_K_Minus_BB_pct, Predicted_K_Minus_BB_pct, Actual_Over_Expected) %>%
  arrange(desc(Predicted_K_Minus_BB_pct)) %>%
  slice_head(n = 10)

my_plot <- ggplot(data = Results, aes(x = Actual_K_Minus_BB_pct, y = Predicted_K_Minus_BB_pct)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    x = "Actual K-BB%",
    y = "Predcited K-BB%",
    title = "Hitters K-BB% vs their Predicted in the 2024 season",
    subtitle = "100 Plate Appearances minimum",
    caption = "Visual by Ethan Mann @ethanmann02| Data from FG through baseballR"
  ) +
  annotate(
    "text",
    x = max(Results$Actual_K_Minus_BB_pct) * 0.95,
    y = min(Results$Predicted_K_Minus_BB_pct) * 1.05,
    label = paste("R^2 =", round(summary(K_Minus_BB_model)$r.squared, 3)),
    hjust = 1,
    vjust = 0
  ) +
  theme_minimal()

ggsave(filename = "desktop/hitters_k_bb_plot.png", plot = my_plot, width = 10, height = 6)

###Load Headshots and Team logos###
Headshots <- load_headshots()
Teams <- load_mlb_teams()

###Building tables###
Results_Top_Ten<- left_join(Results_Top_Ten, Headshots, by = c("xMLBAMID" = "savant_id")) %>%
  select(Team, Player_Full_Name, espn_headshot, Actual_K_Minus_BB_pct, Predicted_K_Minus_BB_pct, Actual_Over_Expected)

Results_Top_Ten<- left_join(Results_Top_Ten, Teams, by = c("Team" = "team_abbr")) %>%
  select(Team, team_logo_espn, Player_Full_Name, espn_headshot, Actual_K_Minus_BB_pct, Predicted_K_Minus_BB_pct, Actual_Over_Expected)

Results_Top_Ten_Table <- Results_Top_Ten |> 
  gt() |>
  gt_img_rows(team_logo_espn) |>
  gt_img_rows(espn_headshot) |>
  opt_align_table_header("center") |> 
  cols_align("center") |> 
  tab_source_note("Table: Ethan Mann @ethanmann02| Data: FG through baseballR | Stats of 7/11") |> 
  cols_label(
    Actual_K_Minus_BB_pct = "Actual K-BB%",
    Predicted_K_Minus_BB_pct = "Predicted K-BB%",
    Actual_Over_Expected = "Actual over Expected",
    espn_headshot = "",
    Player_Full_Name = "Player", 
    Team = "Team",
    team_logo_espn = "") |>
  opt_row_striping() |> 
  tab_header(title = "Players with the highest predicted K-BB%") %>%
  gt_theme_538()


print(Results_Top_Ten_Table)

gtsave(Results_Top_Ten_Table, "Desktop/Results_Top_Ten_Table.png")

Results_Bottom_Ten <- Results %>%
  select(Player_Full_Name, Team, xMLBAMID, Actual_K_Minus_BB_pct, Predicted_K_Minus_BB_pct, Actual_Over_Expected) %>%
  arrange(Predicted_K_Minus_BB_pct) %>%
  slice_head(n = 10)

Results_Bottom_Ten<- left_join(Results_Bottom_Ten, Headshots, by = c("xMLBAMID" = "savant_id")) %>%
  select(Team, Player_Full_Name, espn_headshot, Actual_K_Minus_BB_pct, Predicted_K_Minus_BB_pct, Actual_Over_Expected)

Results_Bottom_Ten<- left_join(Results_Bottom_Ten, Teams, by = c("Team" = "team_abbr")) %>%
  select(Team, team_logo_espn, Player_Full_Name, espn_headshot, Actual_K_Minus_BB_pct, Predicted_K_Minus_BB_pct, Actual_Over_Expected)

Results_Bottom_Ten_Table <- Results_Bottom_Ten |> 
  gt() |>
  gt_img_rows(team_logo_espn) |>
  gt_img_rows(espn_headshot) |>
  opt_align_table_header("center") |> 
  cols_align("center") |> 
  tab_source_note("Table: Ethan Mann @ethanmann02| Data: FG through baseballR | Stats of 7/11") |> 
  cols_label(
    Actual_K_Minus_BB_pct = "Actual K-BB%",
    Predicted_K_Minus_BB_pct = "Predicted K-BB%",
    Actual_Over_Expected = "Actual over Expected",
    espn_headshot = "",
    Player_Full_Name = "Player", 
    Team = "Team",
    team_logo_espn = "") |>
  opt_row_striping() |> 
  tab_header(title = "Players with the lowest predicted K-BB%") %>%
  gt_theme_538()

print(Results_Bottom_Ten_Table)

gtsave(Results_Bottom_Ten_Table, "Desktop/Results_Bottom_Ten_Table.png")

Results_Bottom_Ten_Expected <- Results %>%
  select(Player_Full_Name, Team, xMLBAMID, Actual_K_Minus_BB_pct, Predicted_K_Minus_BB_pct, Actual_Over_Expected) %>%
  arrange(Actual_Over_Expected) %>%
  slice_head(n = 10)

Results_Bottom_Ten_Expected<- left_join(Results_Bottom_Ten_Expected, Headshots, by = c("xMLBAMID" = "savant_id")) %>%
  select(Team, Player_Full_Name, espn_headshot, Actual_K_Minus_BB_pct, Predicted_K_Minus_BB_pct, Actual_Over_Expected)

Results_Bottom_Ten_Expected<- left_join(Results_Bottom_Ten_Expected, Teams, by = c("Team" = "team_abbr")) %>%
  select(Team, team_logo_espn, Player_Full_Name, espn_headshot, Actual_K_Minus_BB_pct, Predicted_K_Minus_BB_pct, Actual_Over_Expected)

Results_Bottom_Ten_Expected_Table <- Results_Bottom_Ten_Expected |> 
  gt() |>
  gt_img_rows(team_logo_espn) |>
  gt_img_rows(espn_headshot) |>
  opt_align_table_header("center") |> 
  cols_align("center") |> 
  tab_source_note("Table: Ethan Mann @ethanmann02| Data: FG through baseballR | Stats of 7/11") |> 
  cols_label(
    Actual_K_Minus_BB_pct = "Actual K-BB%",
    Predicted_K_Minus_BB_pct = "Predicted K-BB%",
    Actual_Over_Expected = "Actual over Expected",
    espn_headshot = "",
    Player_Full_Name = "Player", 
    Team = "Team",
    team_logo_espn = "") |>
  opt_row_striping() |> 
  tab_header(title = "Players with the lowest Actual over Predicted K-BB%") %>%
  gt_theme_538()

print(Results_Bottom_Ten_Expected_Table)

gtsave(Results_Bottom_Ten_Expected_Table, "Desktop/Results_Bottom_Ten_Expected_Table.png")

Results_Top_Ten_Expected <- Results %>%
  select(Player_Full_Name, Team, xMLBAMID, Actual_K_Minus_BB_pct, Predicted_K_Minus_BB_pct, Actual_Over_Expected) %>%
  arrange(desc(Actual_Over_Expected)) %>%
  slice_head(n = 10)

Results_Top_Ten_Expected<- left_join(Results_Top_Ten_Expected, Headshots, by = c("xMLBAMID" = "savant_id")) %>%
  select(Team, Player_Full_Name, espn_headshot, Actual_K_Minus_BB_pct, Predicted_K_Minus_BB_pct, Actual_Over_Expected)

Results_Top_Ten_Expected<- left_join(Results_Top_Ten_Expected, Teams, by = c("Team" = "team_abbr")) %>%
  select(Team, team_logo_espn, Player_Full_Name, espn_headshot, Actual_K_Minus_BB_pct, Predicted_K_Minus_BB_pct, Actual_Over_Expected)

Results_Top_Ten_Expected_Table <- Results_Top_Ten_Expected |> 
  gt() |>
  gt_img_rows(team_logo_espn) |>
  gt_img_rows(espn_headshot) |>
  opt_align_table_header("center") |> 
  cols_align("center") |> 
  tab_source_note("Table: Ethan Mann @ethanmann02| Data: FG through baseballR | Stats of 7/11") |> 
  cols_label(
    Actual_K_Minus_BB_pct = "Actual K-BB%",
    Predicted_K_Minus_BB_pct = "Predicted K-BB%",
    Actual_Over_Expected = "Actual over Expected",
    espn_headshot = "",
    Player_Full_Name = "Player", 
    Team = "Team",
    team_logo_espn = "") |>
  opt_row_striping() |> 
  tab_header(title = "Players with the highest Actual over Predicted K-BB%") %>%
  gt_theme_538()

print(Results_Top_Ten_Expected_Table)

gtsave(Results_Top_Ten_Expected_Table, "Desktop/Results_Top_Ten_Expected_Table.png")

###2023###
FG_PD_TT <- FG_TT %>%
  select(PlayerName, BB_pct, K_pct, `O-Swing_pct`, `Z-Swing_pct`, `Z-Contact_pct`, Contact_pct,
         SwStr_pct, Swing_pct, CStr_pct, `C+SwStr_pct`, `F-Strike_pct`) %>%
  mutate(K_pct_minus_BB_pct = round(K_pct - BB_pct, 3))

K_Minus_BB_TT <- K_pct_minus_BB_pct ~ `O-Swing_pct` + `Z-Swing_pct` + Contact_pct +
  SwStr_pct + Swing_pct + `F-Strike_pct` + CStr_pct + `C+SwStr_pct`

# Fit the linear regression model
K_Minus_BB_model_TT <- lm(K_Minus_BB_TT, data = FG_PD_TT)
summary(K_Minus_BB_model_TT)
plot(K_Minus_BB_model_TT)
coefficients(K_Minus_BB_model_TT)
predictions_TT <- predict(K_Minus_BB_model_TT, newdata = FG_PD_TT)

actual_K_Minus_BB_pct_TT <- FG_PD_TT$K_pct_minus_BB_pct[1:374]  
player_full_names_TT <- FG_PD_TT$PlayerName[1:374]

Results_TT <- data.frame(Predicted_K_Minus_BB_pct = predictions_TT[1:374],
                         Actual_K_Minus_BB_pct = actual_K_Minus_BB_pct_TT,
                         Player_Full_Name = player_full_names_TT)

Results_TT$Predicted_K_Minus_BB_pct <- round(Results_TT$Predicted_K_Minus_BB_pct * 100, 1)
Results_TT$Actual_K_Minus_BB_pct <- round(Results_TT$Actual_K_Minus_BB_pct * 100, 1)

Results_TT <- Results_TT %>%
  mutate(Actual_Over_Expected = Actual_K_Minus_BB_pct - Predicted_K_Minus_BB_pct)

ggplot(data=Results, mapping = aes(x=Actual_K_Minus_BB_pct, y=Predicted_K_Minus_BB_pct)) +
  geom_point()+
  geom_smooth(method = "lm")

###Combining 2024 with 2023###
TwentyThree_AND_TwentyFour <- left_join(Results, Results_TT, by = "Player_Full_Name") %>%
  rename(
    Predicted_K_Minus_BB_pct_2024 = Predicted_K_Minus_BB_pct.x,
    Actual_K_Minus_BB_pct_2024 = Actual_K_Minus_BB_pct.x,
    Actual_Over_Expected_2024 = Actual_Over_Expected.x,
    Predicted_K_Minus_BB_pct_2023 = Predicted_K_Minus_BB_pct.y,
    Actual_K_Minus_BB_pct_2023 = Actual_K_Minus_BB_pct.y,
    Actual_Over_Expected_2023 = Actual_Over_Expected.y
  )

#Removing players who didn't qualify
TwentyThree_AND_TwentyFour <- TwentyThree_AND_TwentyFour %>%
  filter(!is.na(Predicted_K_Minus_BB_pct_2023) & !is.na(Actual_K_Minus_BB_pct_2024))

model <- lm(Actual_K_Minus_BB_pct_2024 ~ Predicted_K_Minus_BB_pct_2023, data = TwentyThree_AND_TwentyFour)
r_squared <- summary(model)$r.squared

model <- lm(Actual_K_Minus_BB_pct_2024 ~ Actual_Over_Expected_2023, data = TwentyThree_AND_TwentyFour)
r_squared <- summary(model)$r.squared


# Create the plot of 2023 v 2024
ggplot(data = TwentyThree_AND_TwentyFour, aes(x = Actual_K_Minus_BB_pct_2023, y = Actual_K_Minus_BB_pct_2024)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    x = "Predicted K-BB% in 2023",
    y = "Actual K-BB% in 2024",
    title = "Hitters Predicted K-BB% in 2023 vs their Actual K-BB% in the 2024 season",
    subtitle = "100 Plate Appearances minimum",
    caption = "Visual by Ethan Mann @ethanmann02| Data from FG through baseballR"
  ) +
  annotate(
    "text",
    x = max(TwentyThree_AND_TwentyFour$Predicted_K_Minus_BB_pct_2023) * 0.95,
    y = min(TwentyThree_AND_TwentyFour$Actual_K_Minus_BB_pct_2024) * 1.05,
    label = paste("R^2 =", round(r_squared, 3)),
    hjust = 1,
    vjust = 0
  ) +
  theme_minimal()

ggsave(filename = "desktop/hitters_k_bb_plot_comp.png", plot = my_plot_new, width = 10, height = 6)

