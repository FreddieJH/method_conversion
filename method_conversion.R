
# Installing required R libraries (uncomment to run the code - if not installed)
# install.packages("tidyverse")

# Loading required R libraries:
library(tidyverse)

# Load in the example dataset and assign to object 'm1_data'
m1_data <- read_csv("example_data.csv")

#	Method 1: individually measured (total length and/or standard length) and weighted. 
#	Method 2: individually measured, but not weighted; or fish measured in 1 cm size bins. Individual weight is not available. 
#	Method 3: only total catch is available; and/or length is indicated in large size bins (>1cm).  

# Converting from method 1 to method 2 ------------------------------------------------------------------------------

# Create a new column called "rounded length" which is total_length rounded to the nearest cm.
# This will be in place of the individual level total_length
# This can be replaced with std_length if using. 
m1_data_round <- mutate(m1_data, 
                        rounded_length = round(total_length))

# Group by all columns that you want to remove, (e.g. total_length, std_length, fork_length), or summarise (e.g. n_fish, fish_weight).
# Note: we can remove the total_length column as we have the length information contained within the new column called rounded_length.
# All columns will be grouped except those within the "-c(...)" code.
m1_data_group <- group_by(m1_data_round, 
                          across(-c(total_length, std_length, fork_length, fish_weight, n_fish)))

# Sum up the individual-level weight and number of fish (based on the above groupings)
m1_data_summary <- summarise(m1_data_group,
                             catch_weight = sum(fish_weight, na.rm = TRUE), 
                             n_fish = sum(n_fish, na.rm = TRUE), 
                             .groups = "drop")

# Add in the individual level columns that were removed, but with NA values
m1_data_addcols <- mutate(m1_data_summary,
                          fish_weight = NA, 
                          std_length = NA, 
                          fork_length = NA, 
                          total_length = NA) 

# Change all values in the method column to "2". 
m1_data_method <- mutate(m1_data_addcols,
                         method = 2) 

# Change the total_length to the rounded measure
# If using std_length then change the total_length to std_length here. 
m1_data_roundgroup <- mutate(m1_data_method,
                             total_length = rounded_length) 

# Select only the columns from the original database
m2_data <- select(m1_data_roundgroup, 
                  names(m1_data))
  

# Converting from method 1 to method 3 ------------------------------------------------------------------------------


# Group by all columns that you want to remove, (e.g. age, sex_lt), or summarise (e.g. fish_weight, total_length, n_fish).
# All columns will be grouped except those within the "-c(...)" code.
m1_data_group2 <- group_by(m1_data, 
                          across(-c(total_length, std_length, fork_length, fish_weight, n_fish, age, sex_lt)))

# Summarise the fish_weight and total_length into the min, max and total catch_weight, and the 
# ... min and max of the total_length, also sum up the number of fish in the group. 
m1_data_summary2 <- summarise(m1_data_group2, 
                              catch_weight_min = min(fish_weight, na.rm = TRUE), 
                              catch_weight_max = max(fish_weight, na.rm = TRUE),
                              catch_weight     = sum(fish_weight, na.rm = TRUE),
                              catch_length_min = min(total_length, na.rm = TRUE),
                              catch_length_max = max(total_length, na.rm = TRUE),
                              n_fish = sum(n_fish, na.rm = TRUE), 
                              .groups = "drop")


# Add in the individual level columns that were removed, but with NA values
m1_data_addcols2 <- mutate(m1_data_summary2,
                           total_length = NA, 
                          std_length = NA, 
                          fish_weight = NA,
                          fork_length = NA, 
                          age = NA, 
                          sex_lt = NA) 

# Change all values in the method column to "3". 
m1_data_method2 <- mutate(m1_data_addcols2,
                         method = 3) 

# Select only the columns from the original database
m3_data <- select(m1_data_method2, 
                  names(m1_data))


