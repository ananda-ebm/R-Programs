library(cmdstanr)

options(mc.cores = parallel::detectCores())

set.seed(14)

x <- rnorm(5000, 5, 1)

my.data <- list(
  N = length(x),
  x = x
)

model.path <- "C:\\Users\\AnandaBiswas\\OneDrive - EBM Health Consultants LLP\\Documents\\R-Programs\\Learning_STAN\\001_Simple_Linear_Regression.stan"

model <- cmdstan_model(model.path)

fit <- model$sample(data = my.data)

print(fit)

draws <- fit$draws(variables = c("mu", "sigma"), format = "draws_df")

View(draws)
