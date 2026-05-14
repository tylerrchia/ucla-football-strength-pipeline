import os
import sys
import time
import requests
import pandas as pd
import ast
from datetime import datetime, timedelta, timezone

clientId = os.getenv("VALD_CLIENT_ID")
clientSecret = os.getenv("VALD_CLIENT_SECRET")
team_id = os.getenv("VALD_TENANT_ID")

print(f"[smartspeed] client_id present: {bool(clientId)}")
print(f"[smartspeed] client_secret present: {bool(clientSecret)}")

# read in profiles for team
output_dir = os.getenv("DATA_OUTPUT_DIR", "data")
os.makedirs(output_dir, exist_ok=True)
profiles_path = os.path.join(output_dir, "profiles_with_groups.csv")
print(f"[smartspeed] Reading profiles from: {profiles_path}")
profiles = pd.read_csv(profiles_path)

# normalize column names to lowercase to handle any casing inconsistencies
profiles.columns = profiles.columns.str.strip().str.lower()

# guard: fail early with a clear message if expected columns are missing
required_cols = {'profileid', 'name'}
missing = required_cols - set(profiles.columns)
if missing:
    raise ValueError(
        f"profiles_with_groups.csv is missing expected columns: {missing}. "
        f"Found: {list(profiles.columns)}"
    )

# rename to match expected casing used throughout the script
profiles = profiles.rename(columns={'profileid': 'profileId'})
print(f"[smartspeed] Loaded {len(profiles)} profiles")

# --------------------------------------------------------------------------------------------------
def request_with_retry(method, url, max_attempts=3, **kwargs):
    for attempt in range(1, max_attempts + 1):
        response = requests.request(method, url, **kwargs)
        if response.status_code == 200:
            return response
        print(f"Attempt {attempt} failed: {response.status_code} - {response.text}")
        if attempt < max_attempts:
            time.sleep(30 * attempt)  # 30s, then 60s
    raise Exception(f"All {max_attempts} attempts failed for {url}")

token_cache = {
    "access_token": None,
    "expires_at": None
}

def get_token(client_id, client_secret):
    """
    Retrieves and caches a Bearer token using client_credentials flow.
    """

    # return cached token if still valid
    if (
        token_cache["access_token"]
        and token_cache["expires_at"]
        and token_cache["expires_at"] > time.time()
    ):
        return token_cache["access_token"]

    url = "https://auth.prd.vald.com/oauth/token"
    
    payload = {
        "grant_type": "client_credentials",
        "client_id": client_id,
        "client_secret": client_secret,
        "audience": "vald-api-external"
    }

    response = request_with_retry("POST", url, data=payload)

    if response.status_code != 200:
        raise Exception(f"Token request failed: {response.status_code} - {response.text}")

    data = response.json()

    access_token = data["access_token"]
    expires_in = data.get("expires_in", 7200)

    token_cache["access_token"] = f"Bearer {access_token}"
    token_cache["expires_at"] = time.time() + expires_in

    print("New token cached")

    return token_cache["access_token"]

token = get_token(clientId, clientSecret)

headers = {
    "Authorization": token,
    "Accept": "application/json"
}

# function to get last 7 days
def get_last_7_days_utc():
    seven_days_ago = datetime.now(timezone.utc) - timedelta(days=7)
    return seven_days_ago.strftime("%Y-%m-%dT%H:%M:%S.000Z")

# pull only last 7 days of data
def pull_last_7_days_tests(team_id: str) -> pd.DataFrame:
    all_data = []
    page = 1

    base_url = f"https://prd-use-api-extsmartspeed.valdperformance.com/v1/team/{team_id}/tests"
    print(f"[smartspeed] API URL: {base_url}")

    test_from_utc = get_last_7_days_utc()
    print(f"[smartspeed] Pulling tests from: {test_from_utc}")

    while True:
        headers = {
            "Authorization": get_token(clientId, clientSecret),
            "Accept": "application/json"
        }

        params = {
            "testFromUtc": test_from_utc,
            "page": page
        }

        response = request_with_retry("GET", base_url, headers=headers, params=params)

        if response.status_code != 200:
            raise Exception(f"API error {response.status_code}: {response.text}")

        # Log raw response structure on first page to diagnose format issues
        if page == 1:
            raw = response.json()
            print(f"[smartspeed] Page 1 raw response type: {type(raw).__name__}")
            if isinstance(raw, dict):
                print(f"[smartspeed] Page 1 response keys: {list(raw.keys())}")
                print(f"[smartspeed] WARNING: Expected a list but got a dict — API response format may have changed")
                # attempt to extract a list from common wrapper keys
                for key in ("results", "data", "tests", "items", "records"):
                    if key in raw and isinstance(raw[key], list):
                        print(f"[smartspeed] Found records under key '{key}', using that")
                        data = raw[key]
                        break
                else:
                    print(f"[smartspeed] Could not find a list in the response — exiting with no data")
                    sys.exit(0)
            elif isinstance(raw, list):
                data = raw
            else:
                print(f"[smartspeed] Unexpected response type: {type(raw).__name__} — exiting")
                sys.exit(0)
        else:
            data = response.json()
            if isinstance(data, dict):
                for key in ("results", "data", "tests", "items", "records"):
                    if key in data and isinstance(data[key], list):
                        data = data[key]
                        break

        if not data:
            print(f"[smartspeed] Page {page} returned empty — done paginating")
            break

        all_data.extend(data)
        print(f"[smartspeed] Pulled page {page} ({len(data)} records)")
        page += 1

    print(f"[smartspeed] Total records pulled (last 7 days): {len(all_data)}")
    return pd.DataFrame(all_data)

df = pull_last_7_days_tests(team_id)

if df.empty:
    print("[smartspeed] No new tests returned by the API in the last 7 days. Exiting.")
    print("[smartspeed] Check: correct team_id? SmartSpeed credentials valid? Tests synced to API?")
    sys.exit(0)

print(f"[smartspeed] Columns returned by API: {list(df.columns)}")

# if column is string, convert to dict
df['runningSummaryFields'] = df['runningSummaryFields'].apply(
    lambda x: ast.literal_eval(x) if isinstance(x, str) else x
)

# flatten completely
running_expanded = pd.json_normalize(df['runningSummaryFields'])

# merge back
df = pd.concat(
    [df.drop(columns=['runningSummaryFields']),
     running_expanded],
    axis=1
)

# normalize column format
df['profileId'] = (
    df['profileId']
    .astype(str)
    .str.strip()
    .str.lower()
)

profiles['profileId'] = (
    profiles['profileId']
    .astype(str)
    .str.strip()
    .str.lower()
)

print(f"[smartspeed] Unique profileIds from API: {df['profileId'].nunique()}")
print(f"[smartspeed] Unique profileIds in profiles: {profiles['profileId'].nunique()}")

# filter to only keep football players
df_filtered = df[df['profileId'].isin(profiles['profileId'])]
print(f"[smartspeed] Records matching football player profiles: {len(df_filtered)}")

if df_filtered.empty:
    print("[smartspeed] WARNING: API returned data but none of the profileIds matched profiles_with_groups.csv")
    print(f"[smartspeed] Sample profileIds from API: {df['profileId'].head(5).tolist()}")
    print(f"[smartspeed] Sample profileIds from profiles: {profiles['profileId'].head(5).tolist()}")
    sys.exit(0)

# add name column based on profileId
df_filtered = df_filtered.merge(
    profiles[['profileId', 'name']],
    on='profileId',
    how='inner'
)

# move name column to the front
cols = ['name'] + [col for col in df_filtered.columns if col != 'name']
df_filtered = df_filtered[cols]

# drop columns that are not needed (ignore if missing, e.g. if API changes)
df_filtered = df_filtered.drop(
    columns=['additionalOptionsFields', 'jumpingSummaryFields', 'groupUnderTestId'],
    errors='ignore'
)

print(f"[smartspeed] Records after profile filter and column drops: {len(df_filtered)}")

# --------------------------------------------------------------------------------------------------

# append new tests
file_path = os.path.join(output_dir, "smartspeed.csv")

if os.path.exists(file_path):
    existing_df = pd.read_csv(file_path)
    print(f"[smartspeed] Existing CSV has {len(existing_df)} rows")

    combined_df = (
        pd.concat([existing_df, df_filtered], ignore_index=True)
        .drop_duplicates(subset=["profileId", "testResultId", "testDateUtc"], keep="last")
    )
else:
    print("[smartspeed] No existing CSV found — creating from scratch")
    combined_df = df_filtered

print(f"[smartspeed] Combined rows after dedup on testResultId+testDateUtc: {len(combined_df)}")

# clean data for wrong test type
if "velocityFields.distance" in combined_df.columns:
    combined_df["velocityFields.distance"] = pd.to_numeric(
        combined_df["velocityFields.distance"],
        errors="coerce"
    )

    combined_df.loc[
        (combined_df["testName"].str.strip().str.lower() == "10m sprint") &
        (combined_df["velocityFields.distance"] == 10),
        "testName"
    ] = "Flying 10s"

# take the best test result for a given player on a given date
# ensure datetime format
combined_df["testDateUtc"] = pd.to_datetime(combined_df["testDateUtc"], errors="coerce")

# create date-only column (removes time)
combined_df["testDate"] = combined_df["testDateUtc"].dt.date

# make sure bestSplitSeconds is numeric
combined_df["bestSplitSeconds"] = pd.to_numeric(
    combined_df["bestSplitSeconds"],
    errors="coerce"
)

# remove reps before 2/6/26
cutoff_date = pd.Timestamp("2026-02-06")
combined_df = combined_df[
    combined_df["testDateUtc"] >= cutoff_date
]

# remove Flying 10s reps where bestSplitSeconds > 2
combined_df = combined_df[
    ~(
        (combined_df["testName"].str.strip().str.lower() == "flying 10s") &
        (combined_df["bestSplitSeconds"] > 2)
    )
]

# sort so lowest bestSplitSeconds comes first
combined_df = combined_df.sort_values(
    by=["profileId", "testDate", "bestSplitSeconds"],
    ascending=[True, True, True]
)

# drop duplicates keeping lowest per name per date
combined_df = combined_df.drop_duplicates(
    subset=["profileId", "testDate"],
    keep="first"
)

print(f"[smartspeed] Final rows after best-per-day dedup: {len(combined_df)}")
print(f"[smartspeed] Date range in output: {combined_df['testDate'].min()} to {combined_df['testDate'].max()}")

combined_df.to_csv(file_path, index=False)
print(f"[smartspeed] Wrote {len(combined_df)} rows to {file_path}")
