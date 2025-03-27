################################################################################
#                                                                              #
#                           4. Calculations – Biomass                           #
#                                                                              #
################################################################################

### Step 4.1: Load Biomass Equations from GitHub
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the `Biomass_Equation.csv` file directly from your GitHub repository.
#   - Keep only the columns `b0`, `b1`, and `Chojnacky_Code`.
#   - Save the result as a new dataframe named `Bm_equa`.

# Example of selecting columns:
#   dataframe <- read.csv("url")
#   dataframe <- dataframe %>% select(column1, column2, column3)

# Implement this step to load the biomass equations.

#----------------
Bm_equa <- read.csv("Biomass Equation.csv")
#----------------

### Step 4.2: Merge Biomass Equations with Tree Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merge the `trees` dataframe with `Bm_equa` using `Chojnacky_Code`.
#   - Ensure that all rows from `trees` are included in the merge.

# Example of merging dataframes using dplyr:
#   dataframe1 <- dataframe1 %>% left_join(dataframe2, by = "common_column")

# Implement this step to merge the biomass equations with the tree data.

#----------------
trees <- trees %>% left_join(Bm_equa, by = "Chojnacky_Code")
#----------------

### Step 4.3: Calculate Biomass
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert DBH from inches to centimeters.
#   - Use the equation: Bm (kg) = exp(b0 + b1 × ln(DBH in cm)).
#   - Add the result as a new column named `biomass` to the `trees` dataframe.

# Example of calculating biomass:
#   dataframe <- dataframe %>% mutate(biomass = exp(b0 + b1 * log(DBH * 2.54))) ## Added .x to the end of the b(num) vars

# Implement this step to calculate the biomass for each tree.

#----------------
trees <- trees %>% mutate(biomass = exp(b0.x + b1.x * log(DBH * 2.54)))
#----------------

### Step 4.4: Filter Biomass Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exclude trees with `b0 = 0` and `b1 = 0`.
#   - Use `filter()` to exclude rows based on conditions.
#   - The `!=` operator is used to exclude values that are not equal to a specified value.
#   - For example, `filter(column != 0)` will keep all rows where the value in `column` is not zero.

#----------------
trees <- trees %>% filter(b0.x != 0 & b1.x != 0)
#----------------

### Step 4.5: Aggregate Biomass by Plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Aggregate biomass by plot.
#   - Use `group_by()` to group the data by plot.
#   - Use `summarise()` to calculate the sum of biomass for each plot.

# Example of aggregating data using dplyr:
#   dataframe <- dataframe %>% group_by(group_column) %>% summarise(column = sum(column))

#----------------
sum_u2_bm <- trees %>% group_by(Plot) %>% summarise(biomass = sum(biomass))
#----------------

### Step 4.6: Calculate Biomass per Acre
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate biomass per acre.
#   - Use `mutate()` to add a new column for biomass per acre.
#   - Multiply the total biomass by the Expansion Factor (EF).

# Example of calculating a new column using dplyr:
#   dataframe <- dataframe %>% mutate(new_column = column * Expansion Factor (EF))

#----------------
sum_u2_bm <- sum_u2_bm %>% mutate(bm_pa = biomass * EF)
#----------------

### Step 4.7: Merge Biomass Data with Existing Summary
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merge the aggregated biomass data with the existing `sum_u2` dataframe.
#   - Use `left_join()` to combine the dataframes based on the `Plot` column.

# Example of merging dataframes using dplyr:
#   dataframe1 <- dataframe1 %>% left_join(dataframe2, by = "common_column")

#----------------
sum_u2 <- sum_u2 %>% left_join(sum_u2_bm, by = "Plot")
#----------------


# Question: Which plot has the highest amount of biomass?
#   - To find the plot with the highest biomass, you can sort the `sum_u2` dataframe by the `bm_pa` column in descending order.
# YOUR ANSWER

#----------------
sum_u2 %>% arrange(desc(bm_pa))
#---------

# Checkpoint: Review the Largest bm_pa Values == plot E3 with 139776 for bm_pa
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use the following code to verify your results:
#head(sum_u2 %>% arrange(desc(bm_pa)))

#     # A tibble: 6 × 5
#    Plot     BA   TPA biomass   bm_pa
#    <chr> <dbl> <dbl>   <dbl>   <dbl>
#    E3     171.   160  34944. 139776.
#    D1     170.   152  34900. 139599.
#    G3     158.   116  33948. 135791.
#    K6     160.   116  33866. 135465.
#    E4     167.   120  33840. 135361.
#    E6     167.   144  32236. 128945.