data.path <- 'https://raw.githubusercontent.com/MatsuuraKentaro/Bayesian_Statistical_Modeling_with_Stan_R_and_Python/refs/heads/master/chap05/input/data-shopping-2.csv'

df <- read.csv(data.path)

my.data <- list(
  N = nrow(df),
  Sex = df$Sex,
  Income = df$Income / 100,
  M = df$M,
  Y = df$Y
)

library(cmdstanr)
options(mc.cores = parallel::detectCores())

model.path <- "C:\\Users\\AnandaBiswas\\OneDrive - EBM Health Consultants LLP\\Documents\\R-Programs\\Learning_STAN\\005_Binomial_Logistic_Regression.stan"

model <- cmdstan_model(model.path)

fit <- model$sample(
  data = my.data,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 5000,
  seed = 14
)

fit$cmdstan_diagnose()

fit$print(variables = c("b"), digits = 3)
