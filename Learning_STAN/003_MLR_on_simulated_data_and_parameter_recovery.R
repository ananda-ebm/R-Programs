data.path <- 'https://raw.githubusercontent.com/MatsuuraKentaro/Bayesian_Statistical_Modeling_with_Stan_R_and_Python/refs/heads/master/chap05/input/data-shopping-1.csv'

df <- read.csv(data.path)

head(df)

df$Income <- df$Income / 100

N <- nrow(df)

mu <- 0.2 + 0.15 * df$Sex + 0.4 * df$Income

sigma <- 0.1

set.seed(14)

Y.sim <- rnorm(n = N, mu, sigma)

simulated.data <- list(
  N = N,
  Sex = df$Sex,
  Income = df$Income,
  Y = df$Y
)

model.path <- "C:\\Users\\AnandaBiswas\\OneDrive - EBM Health Consultants LLP\\Documents\\R-Programs\\Learning_STAN\\003_Multiple_Linear_Regression.stan"

library(cmdstanr)
options(mc.cores = parallel::detectCores())

model <- cmdstan_model(model.path)

fit.simulated <- model$sample(
  data = simulated.data,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 5000,
  seed = 14
)

fit.simulated$cmdstan_diagnose()

fit.simulated$print(variables = c("b", "sigma"), digits = 3)

draws <- fit.simulated$draws(variables = c("b", "sigma"),
                             format = "df")

dim(draws)

apply(draws[,1:4], 2, quantile, probs = c(0.025, 0.975))
