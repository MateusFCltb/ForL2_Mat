################################################################################
#                                                                              #
#                           6. Final Summary Table                            #
#                                                                              #
################################################################################

### Step 6.1: Merge All Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merge `sum_u2` with `dom_cnt` and `richness`.
#   - Use `left_join()` to combine these dataframes based on the `Plot` column.
#   - Add species name and code in the summary file.

# Example of merging multiple dataframes using dplyr:
#   dataframe1 %>% left_join(dataframe2, by = "common_column") %>%
#   left_join(dataframe3, by = "common_column")

# Implement this step to merge all relevant data into `sum_u2`.

#----------------
#sum_u2 <- sum_u2 %>%
#----------------

### Step 6.1.1: Add Species Information and Rename Columns
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merge species information into the summary dataframe.
#   - Use `left_join()` to combine dataframes based on a common column.
#   - Rename the `Code` column to `Dom_species` and the count column (`n`) to `Abundance`.

# Example of merging dataframes using dplyr:
#   dataframe1 %>% left_join(dataframe2, by = c("column1" = "column2"))

# Example of renaming columns using dplyr:
#   dataframe %>% rename(new_name = old_name)

# Implement this step to add species information and rename columns.

#----------------
#sum_u2 <- sum_u2 %>%

#sum_u2 <- sum_u2 %>% rename()
#----------------

### Step 6.3: Convert Biomass Units
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert biomass from kilograms per acre to tons per acre.
#   - Divide the biomass in kilograms per acre by 1000 to convert to tons per acre.

# Example of converting units:
#   dataframe$new_column <- dataframe$old_column / conversion_factor

# Implement this step to convert biomass units.

#----------------

#----------------

### Step 6.4: Apply Dominance Threshold
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# If relative abundance < 50%, label the plot as "Mixed".
#   - Use `ifelse()` to apply this condition to the `Dom_species` and `Common.name` columns.
#   - Remove duplicate rows to ensure only one dominant species per plot.

# Example of using ifelse() in dplyr:
#   dataframe %>% mutate(column = ifelse(condition, value_if_true, value_if_false))

# Implement this step to apply the dominance threshold.

#----------------
#sum_u2 <-

# Remove duplicate rows
#sum_u2 <- 
#----------------

# Question 9: What’s the dominant species of plot D5 now? Compare with your previous answer.

### Step 6.5: Save the Final Dataset and Commit to GitHub
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save `sum_u2` to a CSV file for future use.
#   - Use `write.csv()` to export the dataframe to a CSV file.
#   - After saving, commit and push the file to your GitHub repository.

# Example of exporting a dataframe to a CSV file:
#   write.csv(dataframe, "filename.csv", row.names = FALSE)

# Implement this step to save the final dataset.

#----------------
#write.csv()
#----------------
