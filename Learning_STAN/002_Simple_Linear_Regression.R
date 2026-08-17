data.path <- 'https://raw.githubusercontent.com/MatsuuraKentaro/Bayesian_Statistical_Modeling_with_Stan_R_and_Python/refs/heads/master/chap04/input/data-salary.csv'

df <- read.csv(data.path)

library(tidyverse)

df %>%
  ggplot(aes(x = X, y = Y)) +
  geom_point(size = 2, col = "purple")

fit1 <- lm(Y ~ X, data = df)

summary(fit1)

x.pred <- data.frame(X = 0:28)

ci <- predict(fit1, x.pred, interval = "confidence")
pi <- predict(fit1, x.pred, interval = "prediction")

library(cmdstanr)
options(mc.cores = parallel::detectCores())

model.path <- "C:\\Users\\AnandaBiswas\\OneDrive - EBM Health Consultants LLP\\Documents\\R-Programs\\Learning_STAN\\002_Simple_Linear_Regression.stan"

stan.model <- cmdstan_model(model.path)

my.data <- list(
  N = nrow(df),
  X = df$X,
  Y = df$Y
)

fit2 <- stan.model$sample(data = my.data)

fit2$cmdstan_diagnose()

fit2$cmdstan_summary()

fit2$print(variables = c("a", "b", "sigma"))

draws <- fit2$draws(variables = c("a", "b", "sigma"),
                    format = "df")

dim(draws)

quantile(draws$b, probs = c(0.025, 0.975))
