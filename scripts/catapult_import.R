# devtools::install_github("SBGSports/catapultr", dependencies = TRUE, build_vignettes = FALSE)

library(catapultR)
library(lubridate)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)

# package?catapultR
# help(package="catapultR")
# library(help="catapultR")
# browseVignettes("catapultR")
# packageVersion("catapultR")
# packageDescription("catapultR")

# credentials
token <- ofCloudCreateToken(sToken = Sys.getenv("CATAPULT_API_TOKEN"),
                            sRegion = "America")

# -------------------------------------------------------------------------------
# list of teams available
teams <- ofCloudGetTeams(token)

# list of parameters available
parameters <- ofCloudGetParameters(token)

# list of activities available
activities <- ofCloudGetActivities(token) # pull all activities
# filter only activities after the first 2026 training session
activities <- activities %>%
  mutate(modified_at = ymd_hms(modified_at, tz = "UTC")) %>% 
  select(id, name, modified_at) %>% 
  filter(modified_at > ymd_hms("2026-01-26 21:04:29", tz = "UTC"))

# -------------------------------------------------------------------------------
# initial pull of all stats from 1/26/2026
# catapult_initial <- map_dfr(activities$id[1:6], function(act_id) {
#   ofCloudGetStatistics(
#     token,
#     params = c(
#       "athlete_name", "date", 
#       "total_distance", "total_duration", "total_player_load", "player_load_per_minute"," average_player_load_session",
#       "explosive_efforts", "total_distance", "max_vel", "high_speed_distance_set_2_12mph",
#       "sprint_distance_set_2_16mph", "total_lineman_contact_count", "average_lineman_contact_load",
#       "average_football_impact_load_session", "average_football_impact_count_session","max_effort_acceleration", "max_effort_deceleration", 
#       "hsr_efforts", "period_id", "period_name", "activity_name"
#     ), 
#     groupby = c("athlete", "period", "activity"),
#     filters = list(
#       name = "activity_id",
#       comparison = "=",
#       values = act_id
#     )
#   )
# })

# pull last 3 activities
activities_recent <- activities %>%
  mutate(modified_at = ymd_hms(modified_at, tz = "UTC")) %>%
  arrange(desc(modified_at)) %>%
  slice(1:3)

catapult_append <- map_dfr(activities_recent$id, function(act_id) {
  ofCloudGetStatistics(
    token, 
    params = c(
      "athlete_name", "date", 
      "total_distance", "total_duration", "total_player_load", "player_load_per_minute"," average_player_load_session",
      "explosive_efforts", "total_distance", "max_vel", "high_speed_distance_set_2_12mph",
      "sprint_distance_set_2_16mph", "total_lineman_contact_count", "average_lineman_contact_load",
      "average_football_impact_load_session", "average_football_impact_count_session","max_effort_acceleration", "max_effort_deceleration", 
      "hsr_efforts", "period_id", "period_name", "activity_name"
    ), 
    groupby = c("athlete", "period", "activity"), 
    filters = list(
      name = "activity_id",
      comparison = "=",
      values = act_id
    )
  )
})

# -------------------------------------------------------------------------------
# APPENDING TO DATA FOLDER
catapult_path <- "data/catapult.csv"

if (file.exists(catapult_path)) {
  catapult_existing <- read_csv(catapult_path, show_col_types = FALSE)
  
  catapult_final <- bind_rows(
    catapult_existing,
    catapult_append
  ) %>%
    distinct(
      athlete_name,
      date,
      activity_name,
      .keep_all = TRUE
    )
} else {
  catapult_final <- catapult_initial
}

write_csv(catapult_final, catapult_path)

slug_df <- tibble(
  slug = unique(parameters$slug)
)
