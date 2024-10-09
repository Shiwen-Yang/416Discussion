library(tidyverse)

# Computing ranks and the associated statistics

# Let's start with comparing 2 vectors, X1, X2
X1 <- c(5, 7, 8, 9, 10, 12)
X2 <- c(6, 9, 13, 14, 15, 15, 17)

df_table <- tibble(set = c(1, 2), value = list(X1, X2)) %>% 
  unnest(cols = c(value)) %>% 
  mutate(rank = rank(value, ties.method = "average")) %>%
  arrange(value)
  

# Now based on the above, we create a function that works in general
create_table <- function(list_of_vec, ties_method = "average") {
  k <- length(list_of_vec)
  df_table <- tibble(set = 1:k, value = list_of_vec) %>% 
    unnest(cols = c(value)) %>% 
    mutate(rank = rank(value, ties.method = ties_method)) %>% 
    arrange(value)
  
  return(df_table)
}


X1 <- c(5, 7, 8, 9, 10, 12)
X2 <- c(6, 9, 13, 14, 15, 15, 17)
# Z <- c(1, 4, 6, 9, 14, 18, 20)

X <- list(X1, X2)
X_tb <- create_table(X)


# Now we compute some test statistics
stats <- X_tb %>% 
  group_by(set) %>%
  summarize(total_rank = sum(rank),
            avg_rank = mean(rank),
            set_size = length(set)) %>% 
  mutate(U_stat = total_rank - set_size*(set_size + 1)/2)


# Kruskal-Wallis test
n <- nrow(X_tb)
R_bb <- (n + 1)/2

numerator <- stats %>% 
  mutate(temp = set_size * (avg_rank - R_bb)^2) %>%
  select(temp) %>%
  sum

denominator <- X_tb %>%
  mutate(temp = (rank - R_bb)^2) %>%
  select(temp) %>%
  sum

KW_stat <- (n - 1) * numerator/denominator
