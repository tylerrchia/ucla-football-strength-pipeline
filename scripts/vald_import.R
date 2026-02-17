library("valdr")
library("tidyr")
library("lubridate")
library("readr")
library("dplyr")

forcedeck_FINAL <- NULL
nordbord_FINAL  <- NULL

# get_config()

library(valdr)

stopifnot(
  nzchar(Sys.getenv("VALD_CLIENT_ID")),
  nzchar(Sys.getenv("VALD_CLIENT_SECRET")),
  nzchar(Sys.getenv("VALD_TENANT_ID")),
  nzchar(Sys.getenv("VALD_REGION"))
)

options(keyring_backend = "env")

set_credentials(
  client_id     = Sys.getenv("VALD_CLIENT_ID"),
  client_secret = Sys.getenv("VALD_CLIENT_SECRET"),
  tenant_id     = Sys.getenv("VALD_TENANT_ID"),
  region        = Sys.getenv("VALD_REGION")
)



# dynamic function to take only pull from 2 days prior to current date
start_date <- format(Sys.time() - days(2), "%Y-%m-%dT00:00:00Z")
set_start_date(start_date)
# set_start_date("2026-01-09T00:00:00Z")

# -------------------------------------------------------------------------------

# TAKES LONG TO RUN -- RELOAD AS RDS

# profiles <- get_profiles_only()
# profiles <- read_rds("~/Desktop/football 26/profiles.rds")

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

profiles_with_groups <- read_rds("data/profiles_with_groups.rds")
# -------------------------------------------------------------------------------
# pull forcedeck tests
forcedecks <- get_forcedecks_tests_only()

# filter forcedeck tests for team only
forcedeck_tests_team <- forcedecks %>%
  semi_join(profiles_with_groups, by = "profileId") %>%  
  left_join(profiles_with_groups, by = "profileId")     

# pull forcedeck trials for team only
forcedeck_trials   <- get_forcedecks_trials_only(tests_df = forcedeck_tests_team)

# safeguard to not run if there is no new data
if (is.null(forcedeck_trials) || nrow(forcedeck_trials) == 0) {
  message("No new Forcedeck tests found. Skipping Forcedeck processing.")
} else {
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
    group_by(testId, testType, profileId, metric) %>%
    summarise(
      value = mean(value, na.rm = TRUE),
      firstName = first(firstName),
      lastName = first(lastName),
      groupName = first(groupName),
      positionName = first(positionName),
      date = first(date),
      metric_unit = first(metric_unit),
      .groups = "drop"
    )
  
  # pivot to mimic format of vald export
  forcedeck_FINAL <- forcedeck_avg %>%
    pivot_wider(
      id_cols = c(
        profileId,
        testId,
        firstName,
        lastName,
        groupName,
        positionName,
        testType,
        date
      ),
      names_from = metric,
      values_from = value
    ) %>% 
    # combine name into one column
    mutate (
      name = paste(firstName, lastName)
    )
}

# -------------------------------------------------------------------------------
# dynamic function to take only pull from 7 days prior to current date
start_date <- format(Sys.time() - days(7), "%Y-%m-%dT00:00:00Z")
set_start_date(start_date)

# pull nordbord tests
nordbord_raw <- get_nordbord_data()
nordbord_profiles <- nordbord_raw$profiles
nordbord_tests <- nordbord_raw$tests

# safeguard to not run if there is no new data
if (is.null(nordbord_tests) || nrow(nordbord_tests) == 0) {
  message("No new Nordbord tests found. Skipping Nordbord processing.")
} else {
  nordbord_FINAL <- nordbord_tests %>%
    # rename for consistency
    rename(
      profileId = athleteId,
      testType  = testTypeName
    ) %>%
    # join to get profile data
    semi_join(profiles_with_groups, by = "profileId") %>%  # FILTER to team only
    left_join(profiles_with_groups, by = "profileId") %>%  # ADD group/position
    # reformat date
    mutate(
      date = format(ymd_hms(testDateUtc, tz = "UTC"), "%m/%d/%Y")
    ) %>%
    # make sure that a rep was actually recorded
    filter(leftRepetitions != 0 & rightRepetitions != 0) %>% 
    # remove extra columns
    select(-notes, -testTypeId, -modifiedDateUtc, -testDateUtc, -device, 
           -rightCalibration, -leftCalibration, -rightRepetitions, -leftRepetitions,
           -leftTorque, -rightTorque) %>%
    # calculate assymetry
    mutate(
      asymmetry = abs(100 * (leftMaxForce - rightMaxForce) /
                        ((leftMaxForce + rightMaxForce) / 2))
    ) %>% 
    # combine name into one column
    mutate (
      name = paste(firstName, lastName)
    )
}
# -------------------------------------------------------------------------------
# pull forceframe tests
# forceframe_raw <- get_forceframe_data()
# forceframe_profiles <- forceframe_raw$profiles
# forceframe_tests <- forceframe_raw$tests
# -------------------------------------------------------------------------------


# APPENDING TO DATA FOLDER
forcedeck_path <- "data/forcedecks.csv"

if (file.exists(forcedeck_path)) {
  forcedeck_existing <- read_csv(forcedeck_path, show_col_types = FALSE)
  
  forcedeck_FINAL <- bind_rows(
    forcedeck_existing,
    forcedeck_FINAL
  ) %>%
    distinct(profileId, testId, date, .keep_all = TRUE)
}

write_csv(forcedeck_FINAL, forcedeck_path)

nordbord_path <- "data/nordbord.csv"

if (file.exists(nordbord_path)) {
  nordbord_existing <- read_csv(nordbord_path, show_col_types = FALSE)
  
  nordbord_FINAL <- bind_rows(
    nordbord_existing,
    nordbord_FINAL
  ) %>%
    distinct(profileId, testId, date, .keep_all = TRUE)
}

write_csv(nordbord_FINAL, nordbord_path)
