library(tidyverse)
# Shiwen's email: shiweny@bu.edu
# Shiwen's office: CDS 310
# Potential office hour: Thursday morning


# defining a function in R
normalize <- function(x) {
  result = (x - mean(x))/sd(x)
  return(result)
}


# regarding distributions in R, functions are in groups of 4
# format: (d, p, q, r) + distr.
# d gives the density function, i.e. the p.d.f./p.m.f.
# p gives the distribution function, i.e. the c.d.f. P(X < q)
# q gives the inverse of the distribution function, i.e.
#   for a given probability, p, what's the value of q such that P(X < q) = p
# r generates samples of the distribution

# generate some normal random variables
sample_size = 10000
mu = 5
sigma = 10
X <- rnorm(sample_size, mu, sigma) 

# let's use Monte-Carlo to approximate the c.d.f.
q = 2

# first we compute P(X < q) directly
direct_computation_cdf <- pnorm(q, mean = mu, sd = sigma)


# using Monte-Carlo, P(X < q) can be approximated by the proportion of samples less than q
# NOTE: in R, TRUE/FALSE behaves almost identically as 1/0
monte_carlo_cdf <- function(samp, q) {
  sample_size = length(samp)
  result <- sum(samp < q)/sample_size # Samp is a vector, q is a number. The comparison is done element-wise
  return(result)
}


# here we can compute the percentage error of the monte-carlo est.
error_rate <- function(truth, est) {
  abs(truth - est)/truth
}


print(error_rate(direct_computation_cdf, monte_carlo_cdf(X, q)))

# how does the sample size affect the accuracy of Monte-Carlo? obviously, the more the merrier
# We can "verify" the accuracy of Monte-Carlo via Monte-Carlo
sample_sizes <- seq(1000, 10000, 1000)

df <- data.frame(matrix(ncol = length(sample_sizes)))
colnames(df) <- sample_sizes

for (i in 1:1000) {
  collection_est <- c()
  for (n in sample_sizes) {
    X <- rnorm(n, mu, sigma)
    est <- monte_carlo_cdf(X, q)
    collection_est <- c(collection_est, est)
  }
  df[i,] <- collection_est
}


df_err_rate <- error_rate(direct_computation_cdf, df)


# quick visualizations that you should know:

df_err_rate %>% 
  colMeans %>% 
  plot(sample_sizes, ., main = "Sample Size vs. Mean Error Rate", ylab = "Mean Error Rate")

df_err_rate %>% 
  var %>% 
  diag %>% 
  plot(sample_sizes, ., main = "Sample Size vs. Error Rate Variance", ylab = "Variance")

# visualizations that you don't need to how to produce, but 
# you should know how to interpret
df_err_rate_long <- df_err_rate %>%
  as_tibble() %>%
  mutate(simulation = row_number()) %>%
  pivot_longer(cols = -simulation, names_to = "sample_size", values_to = "error_rate")

df_err_rate_long %>% 
  ggplot(aes(x = factor(sample_size, levels = sort(unique(as.numeric(sample_size)))), y = error_rate)) +
  geom_boxplot() +
  labs(title = "Error Rate Distribution Across Sample Sizes", x = "Sample Size", y = "Error Rate")



par(mfrow = c(1, 1), pch = 19)
# qqnorm plots the sample distribution X against a standard normal distribution by default
qqnorm(normalize(X), cex = 0.3)

# abline(a, b) adds a line to the plot
# the line is specified by y = a + bx
# lwd: line width, lty: line type, are common parameters for lines in R
abline(0, 1, lwd = 2, lty = 1, col = "red")






Y = rexp(10000, 5)

X_norm <- normalize(X)
Y_norm <- normalize(Y)

par(mfrow = c(1, 2), pch = 19)
qqplot(X_norm, Y_norm, cex = 0.3)
qqplot(X, Y, cex = 0.3)
