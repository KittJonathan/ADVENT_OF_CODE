# ADVENT OF CODE
# 2024
# DAY 01
# HISTORIAN HYSTERIA
# 2024-12-01

# 📦 Load packages --------------------------------------------------------

library(tidyverse)

# ⬇️ Import dataset -------------------------------------------------------

df <- read_table("2024/2024_DAY01_HISTORIAN_HYSTERIA/input.txt", 
           col_names = c("nb1", "nb2"))

# 🧩 Solve the problem ----------------------------------------------------

# Part One

df |> 
  # Sort the two columns independently
  mutate(across(.cols = everything(),
                .fns = ~sort(.x))) |> 
  # Calculate the total distance
  summarise(total_distance = sum(abs(nb1 - nb2)))  # 2285373

# Part Two

# Extract list of uniques IDs from first column
col1_ids <- unique(df$nb1)

# Create a table of coefficients by counting the number of times each unique
# value from col1 appears in col2

coeff <- df |> 
  filter(nb2 %in% col1_ids) |> 
  count(nb2, name = "coeff")

# Add coefficients to the dataset and multiply each value from col1 that appears
# in col2 by the coefficient

df |> 
  # Only keep 1st column
  select(nb1) |> 
  # Join the coefficients table
  left_join(coeff, by = c("nb1" = "nb2")) |> 
  # Replace NAs with 0s
  replace_na(list(coeff = 0)) |> 
  # Multiply the values in col1 by the coefficient (= similarity score)
  mutate(similarity_score = nb1 * coeff) |> 
  # Calculate the sum of all similary score
  summarise(total_similarity_score = sum(similarity_score))  # 21142653
