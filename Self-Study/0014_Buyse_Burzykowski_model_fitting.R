library(readxl)

data.path <- "C:\\Users\\AnandaBiswas\\OneDrive - EBM Health Consultants LLP\\Documents\\R-Programs\\Self-Study\\0014_data.xlsx"

df <- read_excel(data.path)

View(df)

N <- 40; n.patients <- 100 # known from simulation

muS.i = muT.i = alpha.i = beta.i = c()

residuals.surrogate = residuals.final = c()

library(tidyverse)

for (i in 1:N) {
  temp.df <- df %>% filter(`Trial ID` == i)
  
  fit.surrogate <- lm(outcome.S ~ Treatment, data = temp.df)
  
  fit.final <- lm(outcome.T ~ Treatment, data = temp.df)
  
  muS.i[i] <- summary(fit.surrogate)$coefficients[1, 1]
  alpha.i[i] <- summary(fit.surrogate)$coefficients[2, 1]
  
  residuals.surrogate <- append(residuals.surrogate, fit.surrogate$residuals)
  
  muT.i[i] <- summary(fit.final)$coefficients[1, 1]
  beta.i[i] <- summary(fit.final)$coefficients[2, 1]
  
  residuals.final <- append(residuals.final, fit.final$residuals)
}

first.stage.parameters.estimated <- matrix(c(muS.i, muT.i, alpha.i, beta.i),
                                           nrow = N, ncol = 4, byrow = FALSE)

D.estimated <- cov(first.stage.parameters.estimated)
D.estimated

D1.estimated <- D.estimated[-2, -2]
D1.estimated

R.sq.trial.f <- t(D1.estimated[1:2, 3]) %*% solve(D1.estimated[1:2, 1:2]) %*% D1.estimated[1:2, 3]
R.sq.trial.f

R.sq.trial.r <- D1.estimated[2, 3]^2 / (D1.estimated[2, 2] * D1.estimated[3, 3])
R.sq.trial.r

all.residuals <- matrix(c(residuals.surrogate, residuals.final),
                          nrow = N*n.patients, ncol = 2, byrow = FALSE)

Sigma.estimated <- cov(all.residuals)
Sigma.estimated

R.sq.individual <- Sigma.estimated[1, 2]^2 / (Sigma.estimated[1, 1] * Sigma.estimated[2, 2])
R.sq.individual
