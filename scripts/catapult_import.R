# devtools::install_github("SBGSports/catapultr", dependencies = TRUE, build_vignettes = FALSE)

library(catapultR)
library(lubridate)
library(dplyr)
library(tidyr)
library(readr)

# package?catapultR
# help(package="catapultR")
# library(help="catapultR")
# browseVignettes("catapultR")
# packageVersion("catapultR")
# packageDescription("catapultR")

# credentials
token <- ofCloudCreateToken(sToken = Sys.getenv("CATAPULT_API_TOKEN"),
                            sRegion = "America")

# list of teams available
teams <- ofCloudGetTeams(token)
glimpse(teams)

# list of parameters available
parameters <- ofCloudGetParameters(token)
glimpse(parameters)

# list of activities available
activities <- ofCloudGetActivities(token)
glimpse(activities)

stats_df <- ofCloudGetStatistics(
  token, 
  params = c("athlete_name", "date", "start_time", "end_time", "position_name", 
             "total_distance", "total_duration", "total_player_load", "max_vel", 
             "hsr_efforts", "max_heart_rate", "mean_heart_rate", 
             "period_id", "period_name", "activity_name"), 
  groupby = c("athlete", "period", "activity"), 
  filters = list(name = "activity_id",
                 comparison = "=",
                 values = activities$id[1])
)
