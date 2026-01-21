library("valdr")
library("valdrViz")
library("tidyverse")

# get_config()
set_credentials(
  client_id     = "REDACTED",
  client_secret = "REDACTED",
  tenant_id     = "REDACTED",
  region        = "use"
)

set_start_date("2026-01-09T00:00:00Z")
# -------------------------------------------------------------------------------

# TAKES LONG TO RUN -- JUST RELOAD AS RDS
# profiles <- get_profiles_only()

# profile_groups <- get_profiles_groups_categories_mapping()

# profiles_with_groups <- profiles %>%
#   left_join(profile_groups, by = "profileId") %>%
#   filter(categoryName == "Football") %>%
#   group_by(profileId) %>%
#   summarise(
#     firstName = first(givenName),
#     lastName  = first(familyName),
#     groupName = paste(unique(groupName), collapse = ", "),
#     .groups = "drop"
#   )
# -------------------------------------------------------------------------------
# pull forcedeck tests
forcedecks <- get_forcedecks_tests_only()

# filter forcedeck tests for team only
forcedeck_tests_team <- forcedecks %>%
  semi_join(profiles_with_groups, by = "profileId") %>%  
  left_join(profiles_with_groups, by = "profileId")     

# pull forcedeck trials for team only
forcedeck_trials   <- get_forcedecks_trials_only(tests_df = forcedeck_tests_team)
forcedeck_trials <- forcedeck_trials %>% 
  select(-hubAthleteId, -recordedOffset, -recordedTimezone, -trialLastModifiedUTC) %>% 
  rename(profileId = athleteId) %>% 
  left_join(profiles_with_groups, by = "profileId")



# forcedecks_performance_dashboard()
# save as an .rds file to easily import into R Shiny