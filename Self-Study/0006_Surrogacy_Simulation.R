set.seed(14)

n.trial <- 30
n.patient <- 150

trt.effect.on.surrogate <- rnorm(n.trial, mean = 10, sd = 1.5)

trt.effect.on.true.endpoint <- rnorm(n.trial, mean = 2 + 0.5 * trt.effect.on.surrogate)

# R.Squared_trial - for validation of trial level surrogacy
cor(trt.effect.on.surrogate, trt.effect.on.true.endpoint)^2

fit1 <- lm(trt.effect.on.true.endpoint ~ trt.effect.on.surrogate)

summary(fit1)$r.squared
