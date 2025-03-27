################################################################################
#                                                                              #
#                 5. Calculations – Species Dominance and Richness             #
#                                                                              #
################################################################################

### Step 5.1: Count Species Abundance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use `dplyr` to group by `Plot` and `Chojnacky_Code` and tally the number of trees.
#   - The `tally()` function counts the number of rows for each group.
#   - Add the result as a new dataframe named `tree_cnt`.

# Example of counting rows using dplyr:
#   dataframe %>% group_by(column1, column2) %>% tally()

# Implement this step to count the abundance of each species by plot.
      
#----------------
tree_cnt <- trees %>% group_by(Plot, Chojnacky_Code) %>% tally()
#----------------

### Step 5.2: Identify Dominant Species
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Filter the species with the highest count for each plot.
#   - Use `filter()` to select rows where the count equals the maximum count for each plot.
#   - Add the result as a new dataframe named `dom_cnt`.

# Example of filtering rows using dplyr:
#   dataframe %>% group_by(column) %>% filter(n == max(n))

# Implement this step to identify the dominant species for each plot.

#----------------
dom_cnt <- tree_cnt %>% group_by(Plot) %>% filter(n == max(n))
#----------------

# Question: Does `dom_cnt` have the same number of rows as `sum_u2`? If not, what caused the difference?
###### Answer: No, because dom_cnt just considers the dominant species by code per plot

### Step 5.3: Calculate Total Trees and Species Richness
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Aggregate total trees per plot.
#   - Use `group_by()` and `summarise()` to calculate the sum of trees for each plot.
#   - Add the result as a new column named `Ttl_Trees` to `tree_total`.

# Calculate species richness (number of unique species per plot).
#   - Use `n_distinct()` to count the number of unique species for each plot.
#   - Add the result as a new column named `richness` to `richness`.

# Example of aggregating data using dplyr:
#   dataframe %>% group_by(column) %>% summarise(new_column = sum(column))
# Example of counting unique values using dplyr:
#   dataframe %>% group_by(column) %>% summarise(new_column = n_distinct(column))

# Implement these steps to calculate total trees and species richness.

#----------------
tree_total <- tree_cnt %>% group_by(Plot) %>% summarise(Ttl_Trees = sum(n))

#richness <- tree_cnt %>%
#----------------

# Merge the dominant species data with the total trees and richness data.
#   - Use `left_join()` to combine `dom_cnt` with `tree_total` and `richness`.
#   - Ensure that all dataframes are merged based on the `Plot` column.

# Example of merging multiple dataframes using dplyr:
#   dataframe1 %>% left_join(dataframe2, by = "common_column") %>%
#   left_join(dataframe3, by = "common_column")

# Implement this step to merge the dominant species data with total trees and richness.

#----------------
#dom_cnt <- dom_cnt %>%
#----------------

# Question: What’s the most abundant species at plot D5?



### Step 5.4: Calculate Relative Abundance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Relative abundance = (Number of dominant trees / Total trees) × 100.
#   - Add a new column named `rel_abd` to `dom_cnt`.
#   - Round the relative abundance to one decimal place.

#----------------
#dom_cnt <- dom_cnt %>%
#----------------

# Question 7: How many trees are there at plot A5?
# Question 8: How many different tree species are there at plot D1?

# Checkpoint: Review the Largest rel_abd Values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use the following code to verify your results:
#head(dom_cnt %>% arrange(desc(rel_abd)))

#     # A tibble: 6 × 6
#     # Groups:   Plot [6]
#     Plot  Code      n Ttl_Trees richness rel_abd
#     <chr> <chr> <int>     <int>    <int>   <dbl>
#     F5    SUM      25        32        4    78.1
#     E4    CHO      23        30        7    76.7
#     C4    CHO      28        38        4    73.7
#     H2    SUM      21        29        5    72.4
#     R5    SUM      28        41        9    68.3
#     F4    CHO      19        28        4    67.9


