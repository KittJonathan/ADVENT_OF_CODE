# ADVENT OF CODE
# 2024
# DAY 02
# RED-NOSED REPORTS
# 2024-12-02

# 📦 Load packages --------------------------------------------------------

library(tidyverse)
library(vroom)

# ⬇️ Import dataset -------------------------------------------------------

# Read raw data
raw_df <- readLines("2024/2024_DAY02_RED_NOSED_REPORTS/input.txt")

# 🧩 Solve the problem ----------------------------------------------------

# Part One

# Extract max number of elements in each reports (nb of whitespaces + 1)
max_elements <- max(str_count(string = raw_df, pattern = "\\ ")) + 1  # 8

# Transform dataset into long format
long_df <- raw_df |> 
  # get data into tibble format
  as_tibble() |> 
  # separate values into columns, using the max number of elements we extracted
  separate(col = value, into = paste0("level", seq_len(max_elements)), sep = " ") |> 
  # add a "report" column as ID
  rowid_to_column(var = "report") |> 
  # transform into long format & remove rows with NAs (no associated value to given level)
  pivot_longer(cols = -report,
               names_to = "level", names_prefix = "level",
               values_to = "value",
               values_drop_na = TRUE) |> 
  # set "value" to numeric
  mutate(value = as.numeric(value))

# Solve the problem using all available levels
reports_summary <- long_df |> 
  # count nb of levels in each report
  mutate(nb_levels = n(), .by = report) |> 
  # calculate difference between two consecutive levels in each report
  mutate(diff = value - lag(value), .by = report) |> 
  # count number of pos/neg diff in each report
  # extract min/max diff in each report
  mutate(diff_pos = sum(diff > 0, na.rm = TRUE),
         diff_neg = sum(diff < 0, na.rm = TRUE),
         diff_min = min(abs(diff), na.rm = TRUE),
         diff_max = max(abs(diff), na.rm = TRUE),
         .by = report) |> 
  # 1st condition: all diffs are either increasing or decreasing
  mutate(cond1 = case_when(diff_pos == (nb_levels - 1) | diff_neg == (nb_levels - 1) ~ 1,
                           .default = 0)) |> 
  # 2nd condition : diffs are at least 1 and at most 3
  mutate(cond2 = case_when(diff_min >= 1 & diff_max <= 3 ~ 1,
                           .default = 0)) |> 
  # Define reports as safe (both conditions are met) or unsafe
  mutate(status = case_when(cond1 == 1 & cond2 == 1 ~ "safe",
                            .default = "unsafe")) |> 
  # Keep one row per report
  slice(1, .by = report) |> 
  # Add col to indicate we kept all levels
  mutate(remove_level = "None") |> 
  # Select columns
  select(report, remove_level, status)

# Count number of safe report
reports_summary |> 
  count(status)

# Part Two

# Create iteration table to remove one level at a time in each report

iter <- long_df |> 
  select(report, level)

# Loop on each row of iteration table : remove one row and run the same code as above
# to determine if removing the level changes the report status from "unsafe" to "safe"

results <- list()

for (i in 1:nrow(iter)) {
  
  # Subset data
  results[[i]] <- long_df |> 
    filter(report == iter$report[i], level != iter$level[i]) |> 
    # count nb of levels in each report
    mutate(nb_levels = n()) |> 
    # calculate difference between two consecutive levels
    mutate(diff = value - lag(value)) |> 
    # count number of pos/neg diff in each report
    # extract min/max diff in each report
    mutate(diff_pos = sum(diff > 0, na.rm = TRUE),
           diff_neg = sum(diff < 0, na.rm = TRUE),
           diff_min = min(abs(diff), na.rm = TRUE),
           diff_max = max(abs(diff), na.rm = TRUE)) |> 
    # 1st condition: all diffs are either increasing or decreasing
    mutate(cond1 = case_when(diff_pos == (nb_levels - 1) | diff_neg == (nb_levels - 1) ~ 1,
                             .default = 0)) |> 
    # 2nd condition : diffs are at least 1 and at most 3
    mutate(cond2 = case_when(diff_min >= 1 & diff_max <= 3 ~ 1,
                             .default = 0)) |> 
    # Define reports as safe (both conditions are met) or unsafe
    mutate(status = case_when(cond1 == 1 & cond2 == 1 ~ "safe",
                           .default = "unsafe")) |> 
    # Keep first row
    slice(1) |> 
    # Add column to indicate which level was removed
    mutate(remove_level = iter$level[i]) |> 
    # Select columns
    select(report, remove_level, status)
  
  }

# Transform list of results into tibble format
bind_rows(results) |> 
  # add reports_summary
  rbind(reports_summary) |> 
  # sort data
  arrange(report) |> 
  # count number of reports with at least one "safe" status
  filter(status == "safe") |> 
  distinct(report) |> 
  count()

