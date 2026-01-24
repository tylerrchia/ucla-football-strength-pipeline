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

# TAKES LONG TO RUN -- RELOAD AS RDS

# profiles <- get_profiles_only()
profiles <- read_rds("~/Desktop/football 26/profiles.rds")

# profile_groups <- get_profiles_groups_categories_mapping()
profile_groups <- read_rds("~/Desktop/football 26/profile_groups.rds")

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
profiles_with_groups <- read_rds("~/Desktop/football 26/profiles_with_groups.rds")

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
  select(-hubAthleteId, -recordedOffset, -recordedTimezone, -trialLastModifiedUTC, -startTime, -endTime,
         -time, -definition_repeatable, -resultId, no_repeats, -definition_result, -definition_id, -trialLimb) %>% # remove unnecessary columns
  rename(profileId = athleteId) %>% # rename for consistency 
  left_join(profiles_with_groups, by = "profileId") %>%  # join to get name & positions
  left_join(
    forcedeck_tests_team %>% select(testId, testType),
    by = "testId"
  )
# reorder trials
forcedeck_trials <- forcedeck_trials %>%  
  relocate("testType", .after = 1) %>% 
  relocate("firstName", .after = 2) %>% 
  relocate("lastName", .after = 3) %>% 
  relocate("groupName", .after = 4)
# reformat date
forcedeck_trials <- forcedeck_trials %>% 
  mutate(
    date = format(ymd_hms(recordedUTC, tz = "UTC"), "%m/%d/%Y")
  )

# cleaning forcedeck data
forcedeck_trials <- forcedeck_trials %>% 
  # select desired metrics
  filter(definition_name %in% c("Athlete Standing Weight", "Jump Height (Imp-Mom)", "RSI-modified (Imp-Mom)", "Contraction Time",
                                "Peak Power / BM", "Eccentric Peak Velocity", "Eccentric Mean Braking Force", "Eccentric Braking RFD / BM",
                                "Force at Zero Velocity", "Countermovement Depth", "Concentric Mean Force / BM", "Eccentric Braking Impulse",
                                "Concentric Impulse", "Concentric Peak Velocity", "Velocity at Peak Power", "Force at Peak Power")) %>% 
  rename(
    metric = definition_name,
    metric_description = definition_description,
    metric_unit = definition_unit
  ) %>% 
  # convert units
  mutate(
    value = as.numeric(value),
    value = case_when(
      metric == "Athlete Standing Weight" ~ value * 2.20462,
      metric == "Jump Height (Imp-Mom)" ~ value / 2.54,
      metric == "Countermovement Depth" ~ value / 2.54,
      TRUE ~ value
    ),
    metric_unit = case_when(
      metric == "Athlete Standing Weight" ~ "Pounds",
      metric == "Jump Height (Imp-Mom)" ~ "Inches",
      metric == "Countermovement Depth" ~ "Inches",
      TRUE ~ metric_unit
    )
  ) %>% 
  # keep only the 'trial' -- don't care about left, right
  filter(resultLimb == "Trial")

# remove outlier jumps
forcedeck_trials <- forcedeck_trials %>%
  group_by(testId) %>%
  filter(
    !any(
      metric == "Jump Height (Imp-Mom)" &
        (value > 30 | value < 5),
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# average the value and put on one row for a given profileId, testType, metric
forcedeck_avg <- forcedeck_trials %>%
  group_by(testType, profileId, metric) %>%
  summarise(
    value = mean(value, na.rm = TRUE),
    firstName = first(firstName),
    lastName = first(lastName),
    groupName = first(groupName),
    date = first(date),
    metric_unit = first(metric_unit),
    .groups = "drop"
  )

# pivot to mimic format of vald export
forcedeck_wide <- forcedeck_avg %>%
  pivot_wider(
    id_cols = c(
      profileId,
      firstName,
      lastName,
      groupName,
      testType,
      date
    ),
    names_from = metric,
    values_from = value
  )

# -------------------------------------------------------------------------------
# pull nordbord tests
# -------------------------------------------------------------------------------
# pull forceframe tests
# -------------------------------------------------------------------------------
# pull dynamo tests


# forcedecks_performance_dashboard()
# save as an .rds file to easily import into R Shiny



cmj <- forcedeck_trials %>% 
  filter(testType == "CMJ")

cmj1 <- cmj %>% 
  filter(definition_result == "BODY_WEIGHT_LBS")
