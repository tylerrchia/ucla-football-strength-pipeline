library("valdr")
library("tidyr")
library("lubridate")
library("readr")
library("dplyr")

# -------------------------------------------------------------------------------
# Data output directory (controlled by GitHub Actions)
output_dir <- Sys.getenv("DATA_OUTPUT_DIR", unset = "data")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

forcedeck_FINAL <- NULL
nordbord_FINAL  <- NULL
forceframe_FINAL  <- NULL
RTP_jumps <- NULL

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

profiles_with_groups <- read_rds(
  file.path(output_dir, "profiles_with_groups.rds")
)
# -------------------------------------------------------------------------------
# pull forcedeck tests
forcedecks <- get_forcedecks_tests_only()

# filter forcedeck tests for team only
forcedeck_tests_team <- forcedecks %>%
  semi_join(profiles_with_groups, by = "profileId") %>%  
  left_join(profiles_with_groups, by = "profileId")     

# pull forcedeck trials for team only
forcedeck_trials   <- get_forcedecks_trials_only(tests_df = forcedeck_tests_team)

# copy for rtp trials
forcedeck_rtp_trials <- forcedeck_trials %>% 
  left_join(
  forcedeck_tests_team %>% select(testId, testType),
  by = "testId"
)

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
# RTP JUMPS PIPELINE — CMJ & SLJ (best jump height rep)
# -------------------------------------------------------------------------------
forcedeck_rtp_trials <- forcedeck_rtp_trials %>%
  rename(any_of(c(metric = "definition_name"))) %>%
  rename(profileId = athleteId) %>%
  left_join(profiles_with_groups, by = "profileId") %>%
  mutate(
    value = as.numeric(value),
    date  = format(ymd_hms(recordedUTC, tz = "UTC"), "%m/%d/%Y")
  ) %>%
  filter(testType %in% c("CMJ", "SLJ"))

cmj_metrics_rtp <- c(
  "Jump Height (Imp-Mom)",
  "Peak Power / BM",
  "RSI-modified (Imp-Mom)",
  "Contraction Time",
  "Eccentric Peak Velocity",
  "Concentric Peak Velocity",
  "Countermovement Depth",
  "Eccentric Deceleration Impulse",
  "Eccentric Deceleration Impulse / BM",
  "Concentric Impulse",
  "Concentric Impulse (Abs) / BM",
  "Force at Zero Velocity"
)

slj_metrics_rtp <- c(
  "Jump Height (Imp-Mom)",
  "Peak Power / BM",
  "RSI-modified (Imp-Mom)",
  "Eccentric Peak Velocity",
  "Concentric Peak Velocity",
  "Countermovement Depth",
  "Eccentric Deceleration Impulse",
  "Eccentric Deceleration Impulse / BM",
  "Concentric Impulse",
  "Concentric Impulse (Abs) / BM",
  "Force at Zero Velocity",
  "Eccentric Duration",
  "Concentric Duration"
)

 cmj_limb_split_metrics <- c(
  "Eccentric Deceleration Impulse",
  "Concentric Impulse",
  "Force at Zero Velocity"
)

RTP_jumps <- NULL

if (!is.null(forcedeck_rtp_trials) && nrow(forcedeck_rtp_trials) > 0) {
  
  # valdr already returns profileId, metric, firstName, lastName, 
  # groupName, positionName, testType, date — just cast value and filter
  
  # ---- CMJ --------------------------------------------------------
  cmj_rtp <- forcedeck_rtp_trials %>%
    filter(testType == "CMJ", metric %in% cmj_metrics_rtp)
  
  RTP_CMJ <- NULL
  
  if (nrow(cmj_rtp) > 0) {
    
    best_cmj <- cmj_rtp %>%
      filter(metric == "Jump Height (Imp-Mom)", resultLimb == "Trial") %>%
      group_by(profileId, testId) %>%
      slice_max(value, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(profileId, testId, trialId)
    
    RTP_CMJ <- cmj_rtp %>%
      semi_join(best_cmj, by = c("profileId", "testId", "trialId")) %>%
      filter(
        resultLimb == "Trial" |
          (metric %in% cmj_limb_split_metrics & resultLimb %in% c("Left", "Right", "Asym"))
      ) %>%
      mutate(
        col_name = case_when(
          resultLimb == "Trial" ~ metric,
          resultLimb == "Left"  ~ paste0(metric, " - Left"),
          resultLimb == "Right" ~ paste0(metric, " - Right"),
          resultLimb == "Asym"  ~ paste0(metric, " Asym. (%)"),
          TRUE ~ metric
        )
      ) %>%
      pivot_wider(
        id_cols     = c(profileId, testId, firstName, lastName, groupName, positionName, testType, date),
        names_from  = col_name,
        values_from = value
      ) %>%
      mutate(name = paste(firstName, lastName))
    
    message(paste("CMJ RTP rows:", nrow(RTP_CMJ)))
  } else {
    message("No CMJ tests found. Skipping.")
  }

  # ---- SLJ --------------------------------------------------------
  slj_rtp <- forcedeck_rtp_trials %>%
    filter(testType == "SLJ", metric %in% slj_metrics_rtp, resultLimb == "Trial")
  
  RTP_SLJ <- NULL
  
  if (nrow(slj_rtp) > 0) {
    
    # Best Left and Right trials found independently via trialLimb
    best_slj_left <- slj_rtp %>%
      filter(metric == "Jump Height (Imp-Mom)", trialLimb == "Left") %>%
      group_by(profileId, testId) %>%
      slice_max(value, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(profileId, testId, trialId_left = trialId)
    
    best_slj_right <- slj_rtp %>%
      filter(metric == "Jump Height (Imp-Mom)", trialLimb == "Right") %>%
      group_by(profileId, testId) %>%
      slice_max(value, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(profileId, testId, trialId_right = trialId)
    
    slj_left_data <- slj_rtp %>%
      filter(trialLimb == "Left") %>%
      inner_join(best_slj_left, by = c("profileId", "testId", "trialId" = "trialId_left"))
    
    slj_right_data <- slj_rtp %>%
      filter(trialLimb == "Right") %>%
      inner_join(best_slj_right, by = c("profileId", "testId", "trialId" = "trialId_right"))
    
    RTP_SLJ <- bind_rows(slj_left_data, slj_right_data) %>%
      mutate(col_name = paste0(metric, " - ", trialLimb)) %>%  # use trialLimb not resultLimb
      pivot_wider(
        id_cols     = c(profileId, testId, firstName, lastName, groupName, positionName, testType, date),
        names_from  = col_name,
        values_from = value
      ) %>%
      mutate(
        `Jump Height Asym. (%)`             = abs(100 * (`Jump Height (Imp-Mom) - Left` - `Jump Height (Imp-Mom) - Right`) /
                                                    ((`Jump Height (Imp-Mom) - Left` + `Jump Height (Imp-Mom) - Right`) / 2)),
        `Peak Power/BM Asym. (%)`           = abs(100 * (`Peak Power / BM - Left` - `Peak Power / BM - Right`) /
                                                    ((`Peak Power / BM - Left` + `Peak Power / BM - Right`) / 2)),
        `RSI-Mod. Asym. (%)`                = abs(100 * (`RSI-modified (Imp-Mom) - Left` - `RSI-modified (Imp-Mom) - Right`) /
                                                    ((`RSI-modified (Imp-Mom) - Left` + `RSI-modified (Imp-Mom) - Right`) / 2)),
        `Ecc. Peak Velocity Asym. (%)`      = abs(100 * (`Eccentric Peak Velocity - Left` - `Eccentric Peak Velocity - Right`) /
                                                    ((`Eccentric Peak Velocity - Left` + `Eccentric Peak Velocity - Right`) / 2)),
        `Con. Peak Velocity Asym. (%)`      = abs(100 * (`Concentric Peak Velocity - Left` - `Concentric Peak Velocity - Right`) /
                                                    ((`Concentric Peak Velocity - Left` + `Concentric Peak Velocity - Right`) / 2)),
        `Countermovement Depth Asym. (%)`   = abs(100 * (`Countermovement Depth - Left` - `Countermovement Depth - Right`) /
                                                    ((`Countermovement Depth - Left` + `Countermovement Depth - Right`) / 2)),
        `Ecc. Decel. Impulse Asym. (%)`     = abs(100 * (`Eccentric Deceleration Impulse - Left` - `Eccentric Deceleration Impulse - Right`) /
                                                    ((`Eccentric Deceleration Impulse - Left` + `Eccentric Deceleration Impulse - Right`) / 2)),
        `Ecc. Decel. Impulse/BM Asym. (%)`  = abs(100 * (`Eccentric Deceleration Impulse / BM - Left` - `Eccentric Deceleration Impulse / BM - Right`) /
                                                    ((`Eccentric Deceleration Impulse / BM - Left` + `Eccentric Deceleration Impulse / BM - Right`) / 2)),
        `Con. Impulse Asym. (%)`            = abs(100 * (`Concentric Impulse - Left` - `Concentric Impulse - Right`) /
                                                    ((`Concentric Impulse - Left` + `Concentric Impulse - Right`) / 2)),
        `Con. Impulse/BM Asym. (%)` = abs(100 * (`Concentric Impulse (Abs) / BM - Left` - `Concentric Impulse (Abs) / BM - Right`) /
                                            ((`Concentric Impulse (Abs) / BM - Left` + `Concentric Impulse (Abs) / BM - Right`) / 2)),
        `Force at Zero Velocity Asym. (%)`  = abs(100 * (`Force at Zero Velocity - Left` - `Force at Zero Velocity - Right`) /
                                                    ((`Force at Zero Velocity - Left` + `Force at Zero Velocity - Right`) / 2)),
        `Ecc. Duration Asym. (%)`           = abs(100 * (`Eccentric Duration - Left` - `Eccentric Duration - Right`) /
                                                    ((`Eccentric Duration - Left` + `Eccentric Duration - Right`) / 2)),
        `Con. Duration Asym. (%)`           = abs(100 * (`Concentric Duration - Left` - `Concentric Duration - Right`) /
                                                    ((`Concentric Duration - Left` + `Concentric Duration - Right`) / 2)),
        name = paste(firstName, lastName)
      )
    
    message(paste("SLJ RTP rows:", nrow(RTP_SLJ)))
  } else {
    message("No SLJ tests found. Skipping.")
  }
  
  RTP_jumps <- bind_rows(RTP_CMJ, RTP_SLJ)
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
    # calculate avg max force
    mutate(
      avg_max_force = (leftMaxForce + rightMaxForce) / 2
    ) %>% 
    # safeguard for cheated tests
    mutate(
      leftMaxForce = if_else(
        avg_max_force > 900,
        leftAvgForce,
        leftMaxForce
      ),
      rightMaxForce = if_else(
        avg_max_force > 900,
        rightAvgForce,
        rightMaxForce
      ),
      avg_max_force = if_else(
        avg_max_force > 900,
        (leftAvgForce + rightAvgForce) / 2,
        avg_max_force
      )
    )
}
# -------------------------------------------------------------------------------
# pull forceframe tests
forceframe_raw <- get_forceframe_data()
forceframe_profiles <- forceframe_raw$profiles
forceframe_tests <- forceframe_raw$tests

if (is.null(forceframe_tests) || nrow(forceframe_tests) == 0) {
  message("No new Nordbord tests found. Skipping Nordbord processing.")
} else {
  forceframe_FINAL <- forceframe_tests %>%
    # join to get profile data
    semi_join(profiles_with_groups, by = "profileId") %>%  # FILTER to team only
    left_join(profiles_with_groups, by = "profileId") %>%  # ADD group/position
    # filter for specific abduction / adduction tests
    filter(testPositionName == "Hip AD/AB - 45") %>% 
    mutate(
      date = format(ymd_hms(testDateUtc, tz = "UTC"), "%m/%d/%Y"),
      maxOuterForce = outerRightMaxForce + outerLeftMaxForce,
      maxInnerForce = innerRightMaxForce + innerLeftMaxForce,
      AB_AD_ratio = maxInnerForce / maxOuterForce,
      abduction_asymmetry = abs(100 * (outerLeftMaxForce - outerRightMaxForce) /
                                  ((outerLeftMaxForce + outerRightMaxForce) / 2)),
      adduction_asymmetry = abs(100 * (innerLeftMaxForce - innerRightMaxForce) /
                                  ((innerLeftMaxForce + innerRightMaxForce) / 2))
    ) %>% 
    select(-c("testDateUtc", "testTypeId", "testPositionId", "testTypeName", "device", "notes", "modifiedDateUtc", "year"))
}
# -------------------------------------------------------------------------------

# APPENDING TO DATA FOLDER

forcedeck_path <- file.path(output_dir, "forcedecks.csv")

if (!is.null(forcedeck_FINAL)) {
  if (file.exists(forcedeck_path)) {
  forcedeck_existing <- read_csv(forcedeck_path, show_col_types = FALSE)
  
  forcedeck_FINAL <- bind_rows(
    forcedeck_existing,
    forcedeck_FINAL
  ) %>%
    distinct(profileId, testId, date, .keep_all = TRUE)
  }
  
  write_csv(forcedeck_FINAL, forcedeck_path)
}


nordbord_path <- file.path(output_dir, "nordbord.csv")

if (!is.null(nordbord_FINAL)) {
  if (file.exists(nordbord_path)) {
  nordbord_existing <- read_csv(nordbord_path, show_col_types = FALSE)
  
  nordbord_FINAL <- bind_rows(
    nordbord_existing,
    nordbord_FINAL
  ) %>%
    distinct(profileId, testId, date, .keep_all = TRUE)
  }

  write_csv(nordbord_FINAL, nordbord_path)
}

forceframe_path <- file.path(output_dir, "forceframe.csv")

if (!is.null(forceframe_FINAL)) {
  if (file.exists(forceframe_path)) {
    forceframe_existing <- read_csv(forceframe_path, show_col_types = FALSE)
    
    forceframe_FINAL <- bind_rows(
      forceframe_existing,
      forceframe_FINAL
    ) %>%
      distinct(profileId, testPositionName, date, .keep_all = TRUE)
  }
  
  write_csv(forceframe_FINAL, forceframe_path)
}

rtp_path <- file.path(output_dir, "rtp.csv")
  
if (!is.null(RTP_jumps) && nrow(RTP_jumps) > 0) {
  if (file.exists(rtp_path)) {
    rtp_existing <- read_csv(rtp_path, show_col_types = FALSE)
    RTP_jumps <- bind_rows(rtp_existing, RTP_jumps) %>%
      distinct(profileId, testId, testType, date, .keep_all = TRUE)
  }
  write_csv(RTP_jumps, rtp_path)
}
