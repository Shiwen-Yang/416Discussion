library(tidyverse)

path = 

# Some basic data manips in R
CP <- read.csv(path) %>% as_tibble()

# 1. Select rows of the df that satisfies certain criterion ---------------

# Example 1: All emplyees who are biological female
CP %>% 
  filter(Biological_Sex == "Female")

# Example 2: All employees whose age is under 32 or above 38
CP %>%
  filter(Age < 32 | Age > 38)

CP %>%
  filter(Age > 32 & Age < 38)


# 2. Select a subset of columns -------------------------------------------

# First we create some fake columns for demonstration purposes
CP$First_Exp <- 1
CP$Exp_Temp_Exp <- 2
CP$Exp_Temp_exp <- 3

# Example 1: Select columns by name
CP %>%
  select(Employee_ID_No, Age, Job_Rank)

# Example 2: Select columns by features of their names
# 3 of the easiest functions to use: starts_with, ends_with, contains
CP %>% 
  select(ends_with("_Name"))
CP %>% 
  select(starts_with("First_"))
CP %>% 
  select(contains("_Temp_"))

# Example 3: Select columns using regular expression (not easy, but can be immensely helpful)
# e.g. All names that contains exactly 1 "_"
CP %>%
  select(matches("^[^_]*_[^_]*$"))

# Example 4: Remove columns
CP %>% 
  select(-First_Exp)
# multiple columns can be removed together based on common features as well
CP <- CP %>%
  select(-contains("Exp"))


# 3. Make modifications to an existing column or creating a new column -------

# Example 1: Apply a function to a column to create a new column, e.g. Age-Squared
CP %>% 
  mutate(Age_Squared = Age^2) %>%
  select(matches("^Age"))

# Example 2: Binarize a categorical variable using if_else
CP %>% 
  mutate(Bin_Sex = if_else(Biological_Sex == "Male", 1, 0)) %>%
  select(contains("Sex"))

# Example 3: Convert an ordinal categorical variable to a numeric scale
CP %>%
  mutate(Job_Rank_Ord = case_when(
    Job_Rank == "Entry" ~ 0,
    Job_Rank == "Associate" ~ 1,
    Job_Rank == "Ast. Manager" ~ 2
  )) %>%
  select(contains("Rank"))


# 4. Summarize the data by group ------------------------------------------

# Example 1: Compare the mean hourly wage and hours worked among all job ranks

CP %>%
  group_by(Job_Rank) %>%
  summarize(mean_hourly_wage = mean(Hourly_Wage),
            mean_hours_worked = mean(Hours_Worked))

# Example 2: Compare the variance of the same variables, in case you need it:
# you can also apply the rounding function after computing the desired statistics 
CP %>%
  group_by(Job_Rank) %>%
  summarize(var_hourly_wage = var(Hourly_Wage),
            var_hours_worked = var(Hours_Worked) %>% round(., 4))

# Example 3: computing some custom statistics that is totally meaningless
CP %>% 
  group_by(Job_Rank) %>%
  summarize(cus_hourly_wage = sqrt(sum(Hourly_Wage)^2/(length(Hourly_Wage) - 1)))

# Example 4: Multiple groups can be grouped and summarized as well
CP %>% 
  group_by(Biological_Sex, Job_Rank) %>%
  summarize(mean_hourly_wage = mean(Hourly_Wage)) %>%
  # visualization
  ggplot(aes(x = Job_Rank, y = Biological_Sex, fill = mean_hourly_wage)) +
  geom_tile(color = "white") +  # Create the heatmap blocks with white borders
  geom_text(aes(label = round(mean_hourly_wage, 2)), color = "black") +  # Label each block with the mean hourly wage, rounded to 2 decimals
  scale_fill_gradient(low = "lightblue", high = "red") +  # Define color gradient for the heatmap
  labs(x = "Job Rank", y = "Biological Sex", fill = "Mean Hourly Wage", 
       title = "Heatmap of Mean Hourly Wage by Biological Sex and Job Rank") +
  theme_minimal() 


# 5. Renaming columns -----------------------------------------------------

CP %>% 
  rename(FN = First_Name,
         LN = Last_Name) # new name on the left, current name on the right




# Some suggestions regarding how to approach HW coding problems -----------

# 1. Instead of coding "the process" of computing something, code the function instead

# Example question 1: someone claimed that the mean hourly wage is 25. Compute the Z-statistic

# AVOID THIS
Z_stat <- (mean(CP$Hourly_Wage) - 25)/sd(CP$Hourly_Wage) 
p_val <- pnorm(Z_stat)
print(c(Z_stat, p_val))

#DO THIS
Z_stat <- function(X, H0) { 
  
  Z_stat <- (mean(X) - H0)/sd(X) 
  p_val <- pnorm(Z_stat)
  result = list("Z_stat" = Z_stat, "p_val" = p_val)
  
  return(result)
} 
Z_stat(CP$Hourly_Wage, 25)

# Practice question: create the same thing, but for a chi-square 
# statistic for variance instead


chi_square_stat <- function(X, H0) {
  # this funciton computes the chi-square statistics for ...
  # X is ...
  # H0 is ...
  # the function returns ...
  
  s <- var(X)
  n <- length(X)
  
  chi_square <-
    
    p_val <- pchisq(chi_square) 
  
  result <- list("chi_square" = chi_square, "p_val" = p_val)
  
  return(result)
}
