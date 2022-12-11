df <- read.csv("competitionResults.csv")
library(dplyr)

#
snitch_catcher <- ifelse(df$snitch == df$homeTeam, "home", "away")
df[,"snitchCatcher"] <- as.data.frame(snitch_catcher)
#

#
df$homeGoals <- ifelse(df$snitchCatcher == "home",
                       df$homeGoals + 5,
                       df$homeGoals
) 

df$awayGoals <- ifelse(df$snitchCatcher == "away",
                       df$awayGoals + 5,
                       df$awayGoals
)
#

#
df[,"homeScore"] <- df$homeGoals * 10
df[,"awayScore"] <- df$awayGoals * 10
#

#
df <- df %>%               #create match number Var using dplyr
  group_by(Season) %>%
  mutate(matchNumber = row_number())
#

#
head(
  df[,c("Season", "round", "matchNumber",
        "homeTeam", "awayTeam", "homeScore", "awayScore")]
)
#

#
df$homePoint <- ifelse(df$homeScore > df$awayScore,
                       3,
                       0
) 

df$awayPoint <- ifelse(df$homeScore < df$awayScore,
                       3,
                       0
) 

df$homePoint <- ifelse(df$homeScore == df$awayScore,
                       df$homePoint + 1,
                       df$homePoint
) 

df$awayPoint <- ifelse(df$homeScore == df$awayScore,
                       df$awayPoint + 1,
                       df$awayPoint
) 
#

#
season2015 <- df[df$Season == 2015,]
UnitedStatesChimeras <- season2015[season2015$homeTeam == "United States Chimeras",]

season2015 %>%
  group_by(homeTeam) %>%
  summarise(season2015, sum(homePoint, awayPoint))

#TESTING AREA
TeamList <- as.data.frame(unique(df$homeTeam))        #get each team
AAA <- filter(df, homeTeam == "United States Chimeras")
as.data.frame(AAA)


sum(df$homePoint)

summarise(df, sum(homePoint, awayPoint))

season2015 %>%
  rowwise(homeTeam)