#calculating success in NCAA tournaments
#John Baker
#johnedgarbaker@gmail.com
#2024-03-18

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(purrr)
library(broom)
# here I input csv files with net and kenpom rankings along with number of wins
ncaa_net_24 <- read_csv("ncaa_net_2023-24.csv", skip = 1)
ncaa_tourney_wins <- read_csv("ncaa_tourney_wins.csv", col_names = FALSE)
ncaa_kenpom_24 <- read_csv("ncaa_kenpom_2023-24.csv")
ncaa_net_23 <- read_csv("ncaa_net_2022-23.csv")
ncaa_kenpom_23 <- read_csv("ncaa_kenpom_2022-23.csv")
ncaa_net_22 <- read_csv("ncaa_net_2021-22.csv")
ncaa_kenpom_22 <- read_csv("ncaa_kenpom_2021-22.csv")
ncaa_net_21 <- read_csv("ncaa_net_2020-21.csv")
ncaa_kenpom_21 <- read_csv("ncaa_kenpom_2020-21.csv")
# first involves data cleaning to allow for analysis
ncaa_net_24 <- ncaa_net_24 %>%
  separate(Record, into = c("wins", "losses"), sep = "-") %>%
  separate(Road, into = c("wins_road", "losses_road"), sep = "-") %>%
  separate(Neutral, into = c("wins_neutral", "losses_neutral"), sep = "-") %>%
  separate(Home, into = c("wins_home", "losses_home"), sep = "-") %>%
  separate(Quad1, into = c("wins_q1", "losses_q1"), sep = "-") %>%
  separate(Quad2, into = c("wins_q2", "losses_q2"), sep = "-") %>%
  separate(Quad3, into = c("wins_q3", "losses_q3"), sep = "-") %>%
  separate(Quad4, into = c("wins_q4", "losses_q4"), sep = "-")
character_columns <- c("wins", "losses", "wins_road", "losses_road", "wins_neutral",
                       "losses_neutral", "wins_home", "losses_home", "wins_q1",
                       "losses_q1", "wins_q2", "losses_q2", "wins_q3", "losses_q3",
                       "wins_q4", "losses_q4")
ncaa_net_24 <- ncaa_net_24 %>% mutate_at(vars(character_columns), as.numeric)
ncaa_net_23 <- ncaa_net_23 %>% select(Conference, Team, NET, 'Avg Opp NET', WL, `Conf. Record`,
                                      `Non-Conference Record`, `Road WL`,
                                      `Quadrant 1`, `Quadrant 2`, `Quadrant 3`,
                                      `Quadrant 4`)
ncaa_net_22 <- ncaa_net_22 %>% select(Conference, Team, NET, 'Avg Opp NET', WL, `Conf. Record`,
                                      `Non-Conference Record`, `Road WL`,
                                      `Quadrant 1`, `Quadrant 2`, `Quadrant 3`,
                                      `Quadrant 4`)
ncaa_net_21 <- ncaa_net_21 %>% select(Conference, Team, NET, 'Avg Opp NET', WL, `Conf. Record`,
                                      `Non-Conference Record`, `Road WL`,
                                      `Quadrant 1`, `Quadrant 2`, `Quadrant 3`,
                                      `Quadrant 4`)
ncaa_net_23 <- ncaa_net_23 %>% 
  separate(WL, into = c("wins", "losses"), sep = "-") %>%
  separate(`Conf. Record`, into = c("wins_conf", "losses_conf"), sep = "-") %>%
  separate(`Non-Conference Record`, into = c("wins_nonconf", "losses_nonconf"), sep = "-") %>%
  separate(`Road WL`, into = c("wins_road", "losses_road"), sep = "-") %>%
  separate(`Quadrant 1`, into = c("wins_q1", "losses_q1"), sep = "-") %>%
  separate(`Quadrant 2`, into = c("wins_q2", "losses_q2"), sep = "-") %>%
  separate(`Quadrant 3`, into = c("wins_q3", "losses_q3"), sep = "-") %>%
  separate(`Quadrant 4`, into = c("wins_q4", "losses_q4"), sep = "-")
character_columns_net23 <- c("wins", "losses", "wins_conf", "losses_conf", "wins_nonconf",
                             "losses_nonconf", "wins_road", "losses_road", "wins_q1",
                             "losses_q1", "wins_q2", "losses_q2", "wins_q3", "losses_q3",
                             "wins_q4", "losses_q4")
ncaa_net_23 <- ncaa_net_23 %>% mutate_at(vars(character_columns_net23), as.numeric)
head(ncaa_net_22)
ncaa_net_22 <- ncaa_net_22 %>% 
  separate(WL, into = c("wins", "losses"), sep = "-") %>%
  separate(`Conf. Record`, into = c("wins_conf", "losses_conf"), sep = "-") %>%
  separate(`Non-Conference Record`, into = c("wins_nonconf", "losses_nonconf"), sep = "-") %>%
  separate(`Road WL`, into = c("wins_road", "losses_road"), sep = "-") %>%
  separate(`Quadrant 1`, into = c("wins_q1", "losses_q1"), sep = "-") %>%
  separate(`Quadrant 2`, into = c("wins_q2", "losses_q2"), sep = "-") %>%
  separate(`Quadrant 3`, into = c("wins_q3", "losses_q3"), sep = "-") %>%
  separate(`Quadrant 4`, into = c("wins_q4", "losses_q4"), sep = "-")
#because column names for ncaa_net_23 and ncaa_net_22 are the same, I can copy code
ncaa_net_22 <- ncaa_net_22 %>% mutate_at(vars(character_columns_net23), as.numeric)
#this will not be the case for ncaa_net_21, as the column titles ise a "." in lieu of " "
#but I can just replace the " " with a ".", saving some time
head(ncaa_net_21)
ncaa_net_21 <- ncaa_net_21 %>% 
  separate(WL, into = c("wins", "losses"), sep = "-") %>%
  separate(`Conf. Record`, into = c("wins_conf", "losses_conf"), sep = "-") %>%
  separate(`Non-Conference Record`, into = c("wins_nonconf", "losses_nonconf"), sep = "-") %>%
  separate(`Road WL`, into = c("wins_road", "losses_road"), sep = "-") %>%
  separate(`Quadrant 1`, into = c("wins_q1", "losses_q1"), sep = "-") %>%
  separate(`Quadrant 2`, into = c("wins_q2", "losses_q2"), sep = "-") %>%
  separate(`Quadrant 3`, into = c("wins_q3", "losses_q3"), sep = "-") %>%
  separate(`Quadrant 4`, into = c("wins_q4", "losses_q4"), sep = "-")
#because the column names are now the same I should be able to use previous col names
ncaa_net_21 <- ncaa_net_21 %>% mutate_at(vars(character_columns_net23), as.numeric)
#now that nets have been cleaned, moving on to clean kenpom ratings
#this cleaning will be limiting columns with some separation, but fewer class shifts
ncaa_kenpom_24 <- ncaa_kenpom_24 %>% select(Rk, Team, Conf, 'W-L', AdjO, AdjD, 
                                            AdjT, OppO, OppD)
ncaa_kenpom_24 <- ncaa_kenpom_24 %>%
  separate('W-L', into = c("wins", "losses"), sep = "-")
ncaa_kenpom_24 <- ncaa_kenpom_24 %>%
  mutate_at(vars(c("wins", "losses")), as.numeric)
#note above, I had originally planned to pipe these three together but did so in steps
ncaa_kenpom_23 <- ncaa_kenpom_23 %>%
  select(Rk, Team, Conf, 'W-L', AdjO, AdjD, AdjT, OppO, OppD) %>%
  separate('W-L', into = c("wins", "losses"), sep = "-") %>%
  mutate_at(vars(c("wins", "losses")), as.numeric)
ncaa_kenpom_22 <- ncaa_kenpom_22 %>%
  select(Rk, Team, Conf, 'W-L', AdjO, AdjD, AdjT, OppO, OppD) %>%
  separate('W-L', into = c("wins", "losses"), sep = "-") %>%
  mutate_at(vars(c("wins", "losses")), as.numeric)
ncaa_kenpom_21 <- ncaa_kenpom_21 %>%
  select(Rk, Team, Conf, 'W-L', AdjO, AdjD, AdjT, OppO, OppD) %>%
  separate('W-L', into = c("wins", "losses"), sep = "-") %>%
  mutate_at(vars(c("wins", "losses")), as.numeric)
#now that data is cleaner for major datasets, I have to add info from NCAA tourney
#the first step is cleaning the data, keeping some of the simpler data included
ncaa_tourney_wins <- ncaa_tourney_wins %>% select(X1, X3:X5)
names(ncaa_tourney_wins) <- c("year", "team", "seed", "wins_number")
head(ncaa_tourney_wins)
ncaa_tourney_wins <- ncaa_tourney_wins %>%
  mutate(wins_number = case_when(
    wins_number == 68 ~ 0,
    wins_number == 64 ~ 0,
    wins_number == 32 ~ 1,
    wins_number == 16 ~ 2,
    wins_number == 8 ~ 3,
    wins_number == 4 ~ 4,
    wins_number == 2 ~ 5,
    wins_number == 1 ~ 6,
    TRUE ~ wins_number
  ))
ncaa_tourney_wins <- ncaa_tourney_wins %>% 
  mutate_at(vars(c("seed", "wins_number")), as.factor)
#next step is to split this dataframe by year
ncaa_tourney_wins_23 <- ncaa_tourney_wins %>% filter(year == "2023")
ncaa_tourney_wins_22 <- ncaa_tourney_wins %>% filter(year == "2022")
ncaa_tourney_wins_21 <- ncaa_tourney_wins %>% filter(year == "2021")
#some delayed cleaning to ensure I can join data by eliminating space/#s in string for Team
ncaa_kenpom_23 <- ncaa_kenpom_23 %>% 
  mutate(Team = str_trim(str_remove(Team, "\\d+$")))
ncaa_kenpom_22 <- ncaa_kenpom_22 %>% 
  mutate(Team = str_trim(str_remove(Team, "\\d+$")))
ncaa_kenpom_21 <- ncaa_kenpom_21 %>% 
  mutate(Team = str_trim(str_remove(Team, "\\d+$")))
ncaa_kenpom_23 <- ncaa_kenpom_23 %>% select(-Conf, -wins, -losses) %>%
  rename(rank = Rk)
ncaa_kenpom_22 <- ncaa_kenpom_22 %>% select(-Conf, -wins, -losses) %>%
  rename(rank = Rk)
ncaa_kenpom_21 <- ncaa_kenpom_21 %>% select(-Conf, -wins, -losses) %>%
  rename(rank = Rk)
ncaa_kenpom_23 <- ncaa_kenpom_23[!ncaa_kenpom_23$rank %in% c(NA, "Rk"), ] %>% 
  mutate(rank = as.numeric(rank), AdjO = as.numeric(AdjO), AdjD=as.numeric(AdjD),
         AdjT = as.numeric(AdjT), OppO = as.numeric(OppO), OppD=as.numeric(OppD))
ncaa_net_23 <- ncaa_net_23 %>% mutate(win_pct = wins/(wins+losses), 
                                       win_pct_conf = wins_conf/(wins_conf + losses_conf),
                                       win_pct_nconf = wins_nonconf/(wins_nonconf+losses_nonconf),
                                       win_pct_road = wins_road/(wins_road + losses_road),
                                       win_pct_q1 = wins_q1/(wins_q1 + losses_q1),
                                       win_pct_q2 = wins_q2/(wins_q2 + losses_q2),
                                       win_pct_q3 = wins_q3/(wins_q3 + losses_q3),
                                       win_pct_q4 = wins_q4/(wins_q4 + losses_q4)) %>%
  select(Conference:losses, wins_road, wins_q1, wins_q2, wins_q3, wins_q4, win_pct:win_pct_q4)
ncaa_net_22 <- ncaa_net_22 %>% mutate(win_pct = wins/(wins+losses), 
                                      win_pct_conf = wins_conf/(wins_conf + losses_conf),
                                      win_pct_nconf = wins_nonconf/(wins_nonconf+losses_nonconf),
                                      win_pct_road = wins_road/(wins_road + losses_road),
                                      win_pct_q1 = wins_q1/(wins_q1 + losses_q1),
                                      win_pct_q2 = wins_q2/(wins_q2 + losses_q2),
                                      win_pct_q3 = wins_q3/(wins_q3 + losses_q3),
                                      win_pct_q4 = wins_q4/(wins_q4 + losses_q4)) %>%
  select(Conference:losses, wins_road, wins_q1, wins_q2, wins_q3, wins_q4, win_pct:win_pct_q4)
ncaa_net_21 <- ncaa_net_21 %>% mutate(win_pct = wins/(wins+losses), 
                                      win_pct_conf = wins_conf/(wins_conf + losses_conf),
                                      win_pct_nconf = wins_nonconf/(wins_nonconf+losses_nonconf),
                                      win_pct_road = wins_road/(wins_road + losses_road),
                                      win_pct_q1 = wins_q1/(wins_q1 + losses_q1),
                                      win_pct_q2 = wins_q2/(wins_q2 + losses_q2),
                                      win_pct_q3 = wins_q3/(wins_q3 + losses_q3),
                                      win_pct_q4 = wins_q4/(wins_q4 + losses_q4)) %>%
  select(Conference:losses, wins_road, wins_q1, wins_q2, wins_q3, wins_q4, win_pct:win_pct_q4)
#join net and kenpom columns, then selecting relevant info in new dataframe
#but first, unfortunately some Team names are slightly different, so no inner join yet
#note, on this part I got help from ChatGPT on how to best use sapply withs str_detect
similar_names_23 <- sapply(ncaa_net_23$Team, function(Team) any(str_detect(ncaa_kenpom_23$Team, Team)))
net23_disc <- which(!similar_names_23)
#run inverse of previous code to determine each Team name needing to be changed
similar_names_23_inv <- sample <- sapply(ncaa_kenpom_23$Team, function(Team) any(str_detect(ncaa_net_23$Team, Team)))
kenpom23_disc <- which(!similar_names_23_inv)
#use stringr package to force team names to be the same. Included only tourney teams.
ncaa_net_23 <- ncaa_net_23 %>% 
  mutate_all(~str_replace_all(., c("UConn" = "Connecticut", "Fla. Atlantic" = "Florida Atlantic",
 "NC State" = "N.C. State", "Miami \\(FL\\)" = "Miami FL",
 "Sam Houston" = "Sam Houston St.", "LMU \\(CA\\)" = "Loyola Marymount",
 "Middle Tenn." = "Middle Tennessee", "Eastern Wash." = "Eastern Washington",
 "SFA" = "Stephen F. Austin", "Southern Ill." = "Southern Illinois",
 "South Fla." = "South Florida", "Eastern Ky." = "Eastern Kentucky",
 "UNCW" = "UNC Wilmington", "California Baptist" = "Cal Baptist",
 "Northern Ky." = "Northern Kentucky", "Western Ky." = "Western Kentucky",
 "A&M-Corpus Christi" = "Texas A&M Corpus Chris", "App State" = "Appalachian St.",
 "Grambling" = "Grambling St.", "FGCU" = "Florida Gulf Coast", 
 "N.C. Central" = "North Carolina Central", "Gardner-Webb" = "Gardner Webb", 
 "Ga. Southern" = "Georgia Southern", "Northern Ariz." = "Northern Arizona", 
 "Northern Colo." = "Northern Colorado", "NIU" = "Northern Iowa", 
 "Western Caro." = "Western Carolina", "SIUE" = "SIU Edwardsville", 
 "ETSU" = "East Tennessee St.", "UTRGV"= "UT Rio Grande Valley",
 "Southeast Mo. St." = "Southeast Missouri St.", "FDU" = "Fairleigh Dickinson",
 "Saint Mary's \\(CA\\)" = "Saint Mary’s", "Southern California" = "USC", 
 "Col. of Charleston" = "Charleston", "Southern Miss." = "Southern Miss", 
 "St. John's \\(NY\\)" = "St. John’s", "Ole Miss" = "Mississippi", "Seattle U" = "Seattle")))
ncaa_net_22 <- ncaa_net_22 %>% 
  mutate_all(~str_replace_all(., c("UConn" = "Connecticut", "Fla. Atlantic" = "Florida Atlantic",
                                   "NC State" = "N.C. State", "Miami \\(FL\\)" = "Miami FL",
                                   "Sam Houston" = "Sam Houston St.", "LMU \\(CA\\)" = "Loyola Marymount",
                                   "Middle Tenn." = "Middle Tennessee", "Eastern Wash." = "Eastern Washington",
                                   "SFA" = "Stephen F. Austin", "Southern Ill." = "Southern Illinois",
                                   "South Fla." = "South Florida", "Eastern Ky." = "Eastern Kentucky",
                                   "UNCW" = "UNC Wilmington", "California Baptist" = "Cal Baptist",
                                   "Northern Ky." = "Northern Kentucky", "Western Ky." = "Western Kentucky",
                                   "A&M-Corpus Christi" = "Texas A&M Corpus Chris", "App State" = "Appalachian St.",
                                   "Grambling" = "Grambling St.", "FGCU" = "Florida Gulf Coast", 
                                   "N.C. Central" = "North Carolina Central", "Gardner-Webb" = "Gardner Webb", 
                                   "Ga. Southern" = "Georgia Southern", "Northern Ariz." = "Northern Arizona", 
                                   "Northern Colo." = "Northern Colorado", "NIU" = "Northern Iowa", 
                                   "Western Caro." = "Western Carolina", "SIUE" = "SIU Edwardsville", 
                                   "ETSU" = "East Tennessee St.", "UTRGV"= "UT Rio Grande Valley",
                                   "Southeast Mo. St." = "Southeast Missouri St.", "FDU" = "Fairleigh Dickinson",
                                   "Saint Mary's \\(CA\\)" = "Saint Mary’s", "Southern California" = "USC", 
                                   "Col. of Charleston" = "Charleston", "Southern Miss." = "Southern Miss", 
                                   "St. John's \\(NY\\)" = "St. John’s", "Ole Miss" = "Mississippi", "Seattle U" = "Seattle")))
ncaa_net_21 <- ncaa_net_21 %>% 
  mutate_all(~str_replace_all(., c("UConn" = "Connecticut", "Fla. Atlantic" = "Florida Atlantic",
                                   "NC State" = "N.C. State", "Miami \\(FL\\)" = "Miami FL",
                                   "Sam Houston" = "Sam Houston St.", "LMU \\(CA\\)" = "Loyola Marymount",
                                   "Middle Tenn." = "Middle Tennessee", "Eastern Wash." = "Eastern Washington",
                                   "SFA" = "Stephen F. Austin", "Southern Ill." = "Southern Illinois",
                                   "South Fla." = "South Florida", "Eastern Ky." = "Eastern Kentucky",
                                   "UNCW" = "UNC Wilmington", "California Baptist" = "Cal Baptist",
                                   "Northern Ky." = "Northern Kentucky", "Western Ky." = "Western Kentucky",
                                   "A&M-Corpus Christi" = "Texas A&M Corpus Chris", "App State" = "Appalachian St.",
                                   "Grambling" = "Grambling St.", "FGCU" = "Florida Gulf Coast", 
                                   "N.C. Central" = "North Carolina Central", "Gardner-Webb" = "Gardner Webb", 
                                   "Ga. Southern" = "Georgia Southern", "Northern Ariz." = "Northern Arizona", 
                                   "Northern Colo." = "Northern Colorado", "NIU" = "Northern Iowa", 
                                   "Western Caro." = "Western Carolina", "SIUE" = "SIU Edwardsville", 
                                   "ETSU" = "East Tennessee St.", "UTRGV"= "UT Rio Grande Valley",
                                   "Southeast Mo. St." = "Southeast Missouri St.", "FDU" = "Fairleigh Dickinson",
                                   "Saint Mary's \\(CA\\)" = "Saint Mary’s", "Southern California" = "USC", 
                                   "Col. of Charleston" = "Charleston", "Southern Miss." = "Southern Miss", 
                                   "St. John's \\(NY\\)" = "St. John’s", "Ole Miss" = "Mississippi", "Seattle U" = "Seattle")))
#some final data wrangling
ncaa_net_23 <- ncaa_net_23 %>% mutate_at(vars(NET:win_pct_q4), as.numeric)
ncaa_net_22 <- ncaa_net_22 %>% mutate_at(vars(NET:win_pct_q4), as.numeric)
ncaa_net_21 <- ncaa_net_21 %>% mutate_at(vars(NET:win_pct_q4), as.numeric)
#this took a lot of time, but now on to the inner joining
ncaa_23_join <- ncaa_net_23 %>% inner_join(ncaa_kenpom_23, by = "Team") %>%
  select(Team, NET, rank, win_pct, wins, losses, Conference, AdjO, AdjD, AdjT, 
         OppO, OppD, everything()) %>% rename(kenpom = rank)
ncaa_22_join <- ncaa_net_22 %>% inner_join(ncaa_kenpom_22, by = "Team") %>%
  select(Team, NET, rank, win_pct, wins, losses, Conference, AdjO, AdjD, AdjT, 
         OppO, OppD, everything()) %>% rename(kenpom = rank)
ncaa_21_join <- ncaa_net_21 %>% inner_join(ncaa_kenpom_21, by = "Team") %>%
  select(Team, NET, rank, win_pct, wins, losses, Conference, AdjO, AdjD, AdjT, 
         OppO, OppD, everything()) %>% rename(kenpom = rank)
ncaa_23_total <- ncaa_23_join %>%
  inner_join(ncaa_tourney_wins_23, by = c("Team" = "team")) %>%
  select(Team, NET, kenpom, wins_number, seed, everything())
ncaa_22_total <- ncaa_22_join %>%
  inner_join(ncaa_tourney_wins_22, by = c("Team" = "team")) %>%
  select(Team, NET, kenpom, wins_number, seed, everything())
ncaa_21_total <- ncaa_21_join %>%
  inner_join(ncaa_tourney_wins_21, by = c("Team" = "team")) %>%
  select(Team, NET, kenpom, wins_number, seed, everything())
#it looks like it would be better for the wins_number and seed values to be numeric, not a category
ncaa_23_total$seed <- as.numeric(as.character(ncaa_23_total$seed))
ncaa_23_total$wins_number <- as.numeric(as.character(ncaa_23_total$wins_number))
ncaa_22_total$seed <- as.numeric(as.character(ncaa_22_total$seed))
ncaa_22_total$wins_number <- as.numeric(as.character(ncaa_22_total$wins_number))
ncaa_21_total$seed <- as.numeric(as.character(ncaa_21_total$seed))
ncaa_21_total$wins_number <- as.numeric(as.character(ncaa_21_total$wins_number))
#a few observations
ncaa_23_total %>% ggplot(aes(AdjO, AdjD, color=wins_number)) + geom_point()
ncaa_23_total %>% ggplot(aes(AdjO, AdjT, color=wins_number)) + geom_point()
ncaa_23_total %>% ggplot(aes(AdjT, AdjD, color=wins_number)) + geom_point()
#of the 2023 Kenpom metrics, AdjD has the least impact on number of wins. AdjO and AdjT have greater impact
ncaa_22_total %>% ggplot(aes(AdjO, AdjD, color=wins_number)) + geom_point() + scale_color_brewer()
ncaa_22_total %>% ggplot(aes(AdjO, AdjT, color=wins_number)) + geom_point() + scale_color_brewer()
ncaa_22_total %>% ggplot(aes(AdjT, AdjD, color=wins_number)) + geom_point() + scale_color_brewer()
#similar findings for the 22 metrics, with the exception of the champion as an outlier
ncaa_21_total %>% ggplot(aes(AdjO, AdjD, color=wins_number)) + geom_point() + scale_color_brewer()
ncaa_21_total %>% ggplot(aes(AdjO, AdjT, color=wins_number)) + geom_point() + scale_color_brewer()
ncaa_21_total %>% ggplot(aes(AdjT, AdjD, color=wins_number)) + geom_point() + scale_color_brewer()
#now to combine the three datasets
ncaa_total <- bind_rows(ncaa_23_total, ncaa_22_total, ncaa_21_total)
#now compare the Kenpom stats in earlier lines across all three years
adjO_model <- lm(AdjO ~ wins_number, data=ncaa_total)
adjD_model <- lm(AdjD ~ wins_number, data=ncaa_total)
adjT_model <- lm(AdjT ~ wins_number, data=ncaa_total)
oppO_model <- lm(OppO ~ wins_number, data=ncaa_total)
oppD_model <- lm(OppD ~ wins_number, data=ncaa_total)
#among these regressions, the Adjusted Offense ranking is most important, with a slope of 2.175
  #thus, better teams have a higher offense ranking than elsewise
adjO_wins_number <- coef(adjO_model)
adjO_wins_number <- adjO_wins_number[2]
adjD_wins_number <- coef(adjD_model)
adjD_wins_number <- adjD_wins_number[2]
adjT_wins_number <- coef(adjT_model)
adjT_wins_number <- adjT_wins_number[2]
oppO_wins_number <- coef(oppO_model)
oppO_wins_number <- oppO_wins_number[2]
oppD_wins_number <- coef(oppD_model)
oppD_wins_number <- oppD_wins_number[2]
kenpom_wins_number <- data.frame(adjO = adjO_wins_number, adjD = adjD_wins_number,
                                 adjT = adjT_wins_number, oppO = oppO_wins_number,
                                 oppD = oppD_wins_number)
print(kenpom_wins_number)
#here, I establish that the adjusted offense value is the most impactful, followed by adjD

