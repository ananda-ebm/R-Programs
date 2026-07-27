# target is to generate (X, Y) X is Exp(rate = 2), Y is Weibull(shape 2, scale = 1) with Gaussian copula(rho = 0.7)

set.seed(14)

# Step 1 : to generate jointly distributed standard normals with correlation 0.7

n <- 10^4; p <- 2

rho.true <- 0.7

mu <- matrix(rep(0, 2), nrow = p)

Sigma <- matrix(c(1, rho.true,
                  rho.true, 1), nrow = p, ncol = p, byrow = TRUE)

a <- matrix(rnorm(n * p), nrow = p, ncol = n)

eig_Sigma <- eigen(Sigma)

Lambda <- diag(eig_Sigma$values)

matrix.P <- eig_Sigma$vectors

Z <- matrix.P %*% sqrt(Lambda) %*% a # each column of Z is a random sample

rowMeans(Z)

cov(t(Z))

# Step 2 : transform the marginals to uniform

U <- pnorm(Z) # each column of U is a Uniform(0, 1) dependent pair

# Step 3 : transform the uniforms into desired marginals

X <- -log(U[1,]) / 2
Y <- 1 * (-log(U[2,]))^(1 / 2)

cor(X, Y, method = "pearson")

cor(X, Y, method = "kendall")

(2 / pi) * asin(rho.true)

cor(X, Y, method = "spearman")

(6 / pi) * asin(rho.true / 2)

library(tidyverse)

data.frame(x = X, y = Y) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 2, col = "red")

data.frame(x = X) %>%
  ggplot(aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), 
                 fill = "#F54927", color = "black", bins = 15) +
  geom_density(linewidth = 1.25) +
  stat_function(fun = dexp, args = list(rate = 2), 
                linewidth = 1.25, color = "blue")

data.frame(x = Y) %>%
  ggplot(aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), 
                 fill = "#27E4F5", color = "black", bins = 15) +
  geom_density(linewidth = 1.25) +
  stat_function(fun = dweibull, args = list(shape = 2, scale = 1), 
                linewidth = 1.25, col = "blue")
