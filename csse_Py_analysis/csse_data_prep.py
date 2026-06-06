# =============================================================================
# CSSE 11+ Examination – Data Extraction & Cleaning Script
# =============================================================================
# Purpose : Read raw score files (2021–2026), harmonise structure, engineer
#           analysis-ready features, and export a single cleaned CSV.
# Author  : CSSE Analysis Project
# Date    : 2025
#
# Files expected (all in DATA_DIR):
#   2021-Entry-raw-scores-sorted-by-gender-and-month-of-birth-FINAL.xlsx
#   2022_FOR-PUBLICATION-RAW-SCORES-BY-GENDER-AND-BIRTH-MONTH-1-1.csv
#   Raw-Scores-for-2023-Entry-for-publication-.csv
#   Raw-scores-for-2024-Entry.csv
#   Raw-scores-for-2025-Entry.csv
#   Raw-scores-for-2026-Entry.csv
# =============================================================================

import os
import re
import numpy as np
import pandas as pd
from scipy import stats

# ---------------------------------------------------------------------------
# 0.  CONFIGURATION
# ---------------------------------------------------------------------------

PROJECT_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
DATA_DIR   = os.environ.get("CSSE_DATA_DIR", PROJECT_ROOT)
OUTPUT_CSV = os.path.join(DATA_DIR, "csse_cleaned.csv")

MAX_SCORE = 60  # common ceiling used for percentile / normalised columns


# ---------------------------------------------------------------------------
# 1.  HELPER FUNCTIONS
# ---------------------------------------------------------------------------

def parse_birth_date(raw: str) -> tuple:
    """
    Normalise the varied birth-date formats found in the raw files into a
    canonical (Birth_Month, Birth_Year) tuple.

    Observed formats
    ----------------
    'Sep-09'  ->  month='Sep', birth_year=2009   (2021 / 2023 / 2025 style)
    '10-Sep'  ->  month='Sep', birth_year=2010   (2022 style)
    '12-Sep'  ->  month='Sep', birth_year=2012   (2024 style)
    'Sep-14'  ->  month='Sep', birth_year=2014   (2026 style)
    """
    raw = str(raw).strip()

    # Pattern A: alpha-dash-numeric  e.g. 'Sep-09' or 'Sep-14'
    m = re.fullmatch(r"([A-Za-z]{3})-(\d{2})", raw)
    if m:
        month      = m.group(1).capitalize()
        birth_year = 2000 + int(m.group(2))
        return month, birth_year

    # Pattern B: numeric-dash-alpha  e.g. '10-Sep' or '12-Sep'
    m = re.fullmatch(r"(\d{2})-([A-Za-z]{3})", raw)
    if m:
        birth_year = 2000 + int(m.group(1))
        month      = m.group(2).capitalize()
        return month, birth_year

    return None, None


def assign_age_group(birth_month: str) -> str:
    """
    Assign one of three age-group labels based on birth month within the
    English academic year (September – August).

    The CSSE 11+ is taken in the autumn of Year 6.  Children born in
    September are the OLDEST in the year group (most mature at test time);
    those born in August are the YOUNGEST.

    Groups (4 months each, matching academic-year quarters):
        Older   : Sep, Oct, Nov, Dec
        Middle  : Jan, Feb, Mar, Apr
        Younger : May, Jun, Jul, Aug
    """
    older   = ["Sep", "Oct", "Nov", "Dec"]
    middle  = ["Jan", "Feb", "Mar", "Apr"]
    younger = ["May", "Jun", "Jul", "Aug"]

    if birth_month in older:
        return "Older (Sep-Dec)"
    elif birth_month in middle:
        return "Middle (Jan-Apr)"
    elif birth_month in younger:
        return "Younger (May-Aug)"
    return np.nan


def load_wide_csv(filepath: str, skip_rows: int, right_start: int) -> pd.DataFrame:
    """
    Load a side-by-side CSV where one gender block sits on the left (cols 0-3)
    and the other on the right (cols right_start to right_start+3), with blank
    separator columns in between.

    All columns are read as strings to prevent pandas from coercing all-NaN
    separator columns (and adjacent Gender columns with missing rows) to float.
    Numeric conversion is applied explicitly to the English and Maths columns.

    Parameters
    ----------
    filepath    : full path to the CSV file
    skip_rows   : number of metadata / header rows before the data rows begin
    right_start : 0-based column index of the Gender column in the right block
    """
    raw = pd.read_csv(
        filepath,
        header    = None,
        skiprows  = skip_rows,
        dtype     = str,          # read everything as string; coerce later
        na_values = ["", "NA"],
    )

    left  = raw.iloc[:, [0, 1, 2, 3]].copy()
    right = raw.iloc[:, [right_start,
                          right_start + 1,
                          right_start + 2,
                          right_start + 3]].copy()

    left.columns  = ["Gender", "Birth_Date", "English", "Maths"]
    right.columns = ["Gender", "Birth_Date", "English", "Maths"]

    combined = pd.concat([left, right], ignore_index=True)

    combined["English"] = pd.to_numeric(combined["English"], errors="coerce")
    combined["Maths"]   = pd.to_numeric(combined["Maths"],   errors="coerce")

    return combined


# ---------------------------------------------------------------------------
# 2.  LOAD RAW FILES
# ---------------------------------------------------------------------------

print("=" * 65)
print("  CSSE 11+ Data Extraction – Reading source files ...")
print("=" * 65)

raw_frames = []

# ------------------------------------------------------------------
# 2021  –  Excel workbook, wide format
#           Row index 6 = header row; data starts at row index 7.
#           Boys  block : cols 0-3  (indices 0,1,2,3)
#           Girls block : cols 6-9  (indices 6,7,8,9)
# ------------------------------------------------------------------
fp_2021 = os.path.join(DATA_DIR,
    "2021-Entry-raw-scores-sorted-by-gender-and-month-of-birth-FINAL.xlsx")

raw_21 = pd.read_excel(fp_2021, header=None)

boys_21  = raw_21.iloc[7:, [0, 1, 2, 3]].copy()
girls_21 = raw_21.iloc[7:, [6, 7, 8, 9]].copy()

for blk in [boys_21, girls_21]:
    blk.columns = ["Gender", "Birth_Date", "English", "Maths"]

combined_21 = pd.concat([boys_21, girls_21], ignore_index=True)
combined_21["English"]    = pd.to_numeric(combined_21["English"], errors="coerce")
combined_21["Maths"]      = pd.to_numeric(combined_21["Maths"],   errors="coerce")
combined_21["Entry_Year"] = 2021

raw_frames.append(combined_21)
print(f"  ✓ 2021 loaded  ({len(combined_21):,} rows before cleaning)")

# ------------------------------------------------------------------
# 2022  –  Tidy long CSV  (one row per candidate; already has a Year col)
#           The English column header contains a trailing space ("English ").
#           We strip all column-name whitespace before selecting.
# ------------------------------------------------------------------
fp_2022 = os.path.join(DATA_DIR,
    "2022_FOR-PUBLICATION-RAW-SCORES-BY-GENDER-AND-BIRTH-MONTH-1-1.csv")

df_22 = pd.read_csv(fp_2022)
df_22.columns = df_22.columns.str.strip()          # remove any surrounding whitespace
df_22 = df_22.rename(columns={"Birth Date": "Birth_Date"})
df_22["Entry_Year"] = 2022
combined_22 = df_22[["Gender", "Birth_Date", "English", "Maths", "Entry_Year"]].copy()

raw_frames.append(combined_22)
print(f"  ✓ 2022 loaded  ({len(combined_22):,} rows before cleaning)")

# ------------------------------------------------------------------
# 2023  –  Wide CSV  (Girls left cols 0-3, Boys right cols 6-9)
#           5 metadata rows precede the data rows.
# ------------------------------------------------------------------
fp_2023 = os.path.join(DATA_DIR,
    "Raw-Scores-for-2023-Entry-for-publication-.csv")

combined_23 = load_wide_csv(fp_2023, skip_rows=5, right_start=6)
combined_23["Entry_Year"] = 2023

raw_frames.append(combined_23)
print(f"  ✓ 2023 loaded  ({len(combined_23):,} rows before cleaning)")

# ------------------------------------------------------------------
# 2024  –  Wide CSV  (Girls left cols 0-3, Boys right cols 6-9)
#           The English header in this file reads "English Total";
#           the 0-based column positions are otherwise identical.
# ------------------------------------------------------------------
fp_2024 = os.path.join(DATA_DIR, "Raw-scores-for-2024-Entry.csv")

combined_24 = load_wide_csv(fp_2024, skip_rows=5, right_start=6)
combined_24["Entry_Year"] = 2024

raw_frames.append(combined_24)
print(f"  ✓ 2024 loaded  ({len(combined_24):,} rows before cleaning)")

# ------------------------------------------------------------------
# 2025  –  Wide CSV  (Girls left cols 0-3, Boys right cols 6-9)
#           This file has 11 columns (an extra trailing NaN column).
#           The right block still begins at column index 6.
# ------------------------------------------------------------------
fp_2025 = os.path.join(DATA_DIR, "Raw-scores-for-2025-Entry.csv")

combined_25 = load_wide_csv(fp_2025, skip_rows=5, right_start=6)
combined_25["Entry_Year"] = 2025

raw_frames.append(combined_25)
print(f"  ✓ 2025 loaded  ({len(combined_25):,} rows before cleaning)")

# ------------------------------------------------------------------
# 2026  –  Wide CSV  (Girls left cols 0-3, Boys right cols 5-8)
#           Only ONE blank separator column, so the right block starts
#           at column index 5 rather than 6.
# ------------------------------------------------------------------
fp_2026 = os.path.join(DATA_DIR, "Raw-scores-for-2026-Entry.csv")

combined_26 = load_wide_csv(fp_2026, skip_rows=6, right_start=5)
combined_26["Entry_Year"] = 2026

raw_frames.append(combined_26)
print(f"  ✓ 2026 loaded  ({len(combined_26):,} rows before cleaning)")


# ---------------------------------------------------------------------------
# 3.  CONCATENATE & CORE CLEANING
# ---------------------------------------------------------------------------

print("\n  Concatenating and cleaning ...")
df = pd.concat(raw_frames, ignore_index=True)

# Keep only valid gender codes
df = df[df["Gender"].isin(["M", "F"])].copy()

# Coerce scores to numeric and drop rows where both are missing
df["English"] = pd.to_numeric(df["English"], errors="coerce")
df["Maths"]   = pd.to_numeric(df["Maths"],   errors="coerce")
df = df.dropna(subset=["English", "Maths"], how="all")

# Parse birth dates into structured month and year columns
parsed = df["Birth_Date"].apply(lambda x: pd.Series(parse_birth_date(x),
                                                      index=["Birth_Month", "Birth_Year"]))
df = pd.concat([df, parsed], axis=1)
df = df.dropna(subset=["Birth_Month", "Birth_Year"])
df["Birth_Year"] = df["Birth_Year"].astype(int)

# Human-readable gender label
df["Gender_Label"] = df["Gender"].map({"M": "Male", "F": "Female"})

print(f"  ✓ Core cleaning done → {len(df):,} records retained")


# ---------------------------------------------------------------------------
# 4.  FEATURE ENGINEERING
# ---------------------------------------------------------------------------

print("  Engineering analysis features ...")

MONTH_ORDER = ["Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr",
               "May","Jun","Jul","Aug"]
AGE_ORDER   = ["Older (Sep-Dec)", "Middle (Jan-Apr)", "Younger (May-Aug)"]

# Ordered categorical birth month (academic-year sequence Sep=1 … Aug=12)
df["Birth_Month"] = pd.Categorical(df["Birth_Month"],
                                    categories=MONTH_ORDER, ordered=True)

# Age group (relative seniority within the school year)
df["Age_Group"] = df["Birth_Month"].astype(str).apply(assign_age_group)
df["Age_Group"] = pd.Categorical(df["Age_Group"],
                                  categories=AGE_ORDER, ordered=True)

# Total raw score
df["Total_Score"] = df["English"] + df["Maths"]

# Threshold flags  (1 = at or above, 0 = below)
df["Eng_Above40"] = (df["English"] >= 40).astype(int)
df["Eng_Above50"] = (df["English"] >= 50).astype(int)
df["Mat_Above40"] = (df["Maths"]   >= 40).astype(int)
df["Mat_Above50"] = (df["Maths"]   >= 50).astype(int)
df["Tot_Above80"] = (df["Total_Score"] >= 80).astype(int)
df["Tot_Above100"]= (df["Total_Score"] >= 100).astype(int)

# Within-year percentile ranks (each candidate ranked against their own cohort)
for subject in ["English", "Maths", "Total_Score"]:
    df[f"{subject}_Pct"] = (
        df.groupby("Entry_Year")[subject]
        .transform(lambda x: stats.rankdata(x.fillna(x.median()),
                                             method="average") / len(x) * 100)
        .round(1)
    )

# Within-year z-scores (standardised performance relative to cohort)
for subject in ["English", "Maths"]:
    df[f"{subject}_Z"] = (
        df.groupby("Entry_Year")[subject]
        .transform(lambda x: (x - x.mean()) / x.std())
        .round(3)
    )

# Convenience labels for chart axes
df["Year_Label"] = df["Entry_Year"].astype(str)
df["Cohort"]     = df["Birth_Year"].astype(str)

print(f"  ✓ Feature engineering complete — {df.shape[1]} columns total")


# ---------------------------------------------------------------------------
# 5.  DATA QUALITY REPORT
# ---------------------------------------------------------------------------

print("\n" + "=" * 65)
print("  DATA QUALITY SUMMARY")
print("=" * 65)

summary = (
    df.groupby("Entry_Year")
    .agg(
        N            = ("Gender",      "count"),
        Males        = ("Gender",      lambda x: (x == "M").sum()),
        Females      = ("Gender",      lambda x: (x == "F").sum()),
        Eng_Mean     = ("English",     "mean"),
        Eng_Median   = ("English",     "median"),
        Maths_Mean   = ("Maths",       "mean"),
        Maths_Median = ("Maths",       "median"),
        Total_Mean   = ("Total_Score", "mean"),
    )
    .round(2)
)
print(summary.to_string())


# ---------------------------------------------------------------------------
# 6.  EXPORT
# ---------------------------------------------------------------------------

df.to_csv(OUTPUT_CSV, index=False)
print(f"\n  ✅ Cleaned data exported → {OUTPUT_CSV}")
print(f"     Shape: {df.shape[0]:,} rows × {df.shape[1]} columns\n")
