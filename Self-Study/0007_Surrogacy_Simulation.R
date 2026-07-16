set.seed(14)

n.trial <- 30
n.patient <- 150

treatment.assignment <- matrix(NA,
                               nrow = n.trial, ncol = n.patient)

PFS <- matrix(NA,
              nrow = n.trial, ncol = n.patient)

OS <- matrix(NA,
             nrow = n.trial, ncol = n.patient)

for (i in 1:n.trial) {
  for (j in 1:n.patient) {
    treatment.assignment[i, j] <- rbinom(1, 1, 0.5)
    
    PFS[i, j] <- 10 + 2 * treatment.assignment[i, j] + rnorm(1, 0, sd = 1.5)
    
    OS[i, j] <- 17 + 2.5 * treatment.assignment[i, j] + 0.7 * PFS[i, j] + rnorm(1, 0, 3)
  }
}

# Between-group mean difference
trt.effect.on.surrogate <- c()
trt.effect.on.true.endpoint <- c()

for (i in 1:n.trial) {
  trt.effect.on.surrogate[i] <- mean(PFS[i, ][treatment.assignment[i, ] == 1]) - 
                                mean(PFS[i, ][treatment.assignment[i, ] == 0])
  
  trt.effect.on.true.endpoint[i] <- mean(OS[i, ][treatment.assignment[i, ] == 1]) -
                                    mean(OS[i, ][treatment.assignment[i, ] == 0])
}

head(trt.effect.on.surrogate)
head(trt.effect.on.true.endpoint)

# trial level validation
cor(trt.effect.on.surrogate, trt.effect.on.true.endpoint)^2

# individual level validation
df1 <- data.frame(trial.ID = rep(1:30, each = 150),
                  patient = rep(1:150, times = 30),
                  treatment = as.vector(t(treatment.assignment)),
                  PFS = as.vector(t(PFS)),
                  OS = as.vector(t(OS)))

View(df1)

# Method 1
cor(df1$PFS, df1$OS)

# Method 2
fit1 <- lm(OS ~ treatment + PFS, data = df1)
summary(fit1)

# Method 3
library(lme4)

fit2 <- lmer(OS ~ treatment + PFS + (1 | trial.ID), data = df1)
summary(fit2)

# variance of random effect is 0

library(MuMIn)

r.squaredGLMM(fit2)
