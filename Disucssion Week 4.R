library(dplyr)

# ME 1 --------------------------------------------------------------------
# 1. False, it gives very strong evidence, but it doesn't prove anything
# 2. False, MC is used to estimate the distribution of the test stat
# the more MC samples you have, the more accurate the estimate is
# MC samples are artificially created, they don't affect the statistical power
# the samples that are collected IRL, affect the statistical power


# ME 2 --------------------------------------------------------------------

# 1. False, it's the compounding type I error
# 2. False, it's because the binomial distribution can be well-approximated by normal

# 3. 
n = 1000
mu = 3
sigma = 1.5

popu = rnorm(n, mu, sigma) 
samp = rbinom(1000, 10, 0.3) 

F_ = samp %>% pnorm(., mean = mu, sd = sigma) %>% sort
Psi_ = (1:n) / n
plot(F_, Psi_, pch = 19, cex = 0.5)


numerator = (F_ - Psi_)^2 * (F_ + Psi_)/2
denominator = F_ * (1 - F_)
denominator
sum(numerator/denominator)

# 4. Wald CI for proportion
n = 52
p_hat = 33/n
Z_alpha = 2.75

epsilon = 2.75 * sqrt(p_hat * (1 - p_hat)/n)

c(p_hat - epsilon, p_hat + epsilon) %>% round(2)



# ME 3 --------------------------------------------------------------------

# 1. False, homoscedastic means "equal variance"
# 2. False, any symmetric distribution does it

df = cbind(c(32, 42,38), c(13, 25, 27))

# 3. How many people did you survey?
sum(df)

# 4. Degree of freedom
(nrow(df) - 1)* (ncol(df) - 1)

# 5. Expected frequency of blue and country

col_T <- df %>% colSums() 
row_T <- df %>% rowSums() 

tcrossprod(row_T, col_T)/sum(df)

# 6. Doesn't really matter which critical value we are using
# in all cases, test stat is greater, so we reject H0
# variables are dependent on one another

# It's we are using the right tail because there is no pont
# checking if our test stat is smaller than the lower bound


# ME 4 --------------------------------------------------------------------
x1 = 76.5
s1 = 5.7
n1 = 23

# Question 1. 
t_22 = 2.0729
SE = s1/sqrt(n1)
print( c(x1 - t_22*SE, x1 + t_22*SE) %>% round(2))

# Question 2.
chisq_r = 36.7807
chisq_l = 10.9823 

c((n1 - 1)*s1^2/chisq_r, (n1 - 1)*s1^2/chisq_l) %>% sqrt %>% round(2)

# Question 3. 
x2 = 73.5
s2 = 5.3
n2 = 24

F_stat = s1^2/s2^2 %>% round(2)
F_22_23 = 2.0246


# Question 4.
xbb = (x1*n1 + x2*n2)/(n1 + n2)
SSM = n1 * (x1 - xbb)^2 + n2 * (x2 - xbb)^2
SSE = s1^2 * (n1 - 1) + s2^2 * (n2 - 1)

# 1. the distribution of the residuals is not linear
# 2. unequal sample size
# 3. small sample size





