################################################################################
#                                                                              #
#                           3. Summarize Data                                  #
#                                                                              #
################################################################################

### Step 3.1: Aggregate BA and TPA by plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Use `dplyr::group_by()` to group the data by plot.
#   - Use `dplyr::summarise()` to calculate the sum of BA and TPA for each plot.
#   - Suggested column names for the summarized data: `Plot`, `BA`, `TPA`.

# Example of aggregating data using dplyr:
#   dataframe %>% group_by(column_to_group_by) %>% summarise(new_column_name = sum(column_to_sum))

# Create a dataframe named `sum_u2_BA` to hold the summarized basal area per acre by plot.
#   - Use `dplyr::group_by()` and `dplyr::summarise()` to calculate the sum of `BA_pa` for each plot.
#   - Add the result as a new column named `BA` to `sum_u2_BA`.

#----------------
sum_u2_BA <-  trees %>% group_by(Plot) %>% summarise(BA = sum(BA_pa))
#----------------


### Step 3.2: Create a dataframe named `sum_u2_TPA` to hold the summarized trees per acre by plot.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Use `dplyr::group_by()` and `dplyr::summarise()` to calculate the sum of `TPA` for each plot.
#   - Add the result as a new column named `TPA` to `sum_u2_TPA`.

#----------------
sum_u2_TPA <-  trees %>% group_by(Plot) %>% summarise(TPA = sum(TPA))
#----------------


### Step 3.3: Merge the `sum_u2_BA and `sum_u2_TPA` dataframes into a single dataframe named `sum_u2`.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Use `dplyr::inner_join()` to combine `sum_u2_BA` and `sum_u2_TPA` based on the `Plot` column.
# Example of merging dataframes using dplyr:
# new_df_name <- dataframe1 %>% inner_join(dataframe2, by = "common_column")

#----------------
sum_u2 <- sum_u2_BA %>% inner_join(sum_u2_TPA, by = "Plot")
#----------------

# Question: Which plot has the maximum basal area?
# Example of sorting data using dplyr:
#   dataframe %>% arrange(desc(column_to_sort_by))
# YOUR ANSWER

#----------------
sum_u2 %>% arrange(desc("BA")) ## A5 has the highest basal area at 99.0
#----------------


# Checkpoint: Review the Largest DBH Values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use the following code to verify your results:
#head(sum_u2 %>% arrange(desc(TPA)))

# Your results should look similar to this:

#     A tibble: 6 × 3
#     Plot     BA   TPA
#     <chr> <dbl> <dbl>
#     F7     62.6   356
#     I3    116.    288
#     C7     71.3   284
#     B6    137.    268
#     E7     73.9   252
#     G6    126.    248

