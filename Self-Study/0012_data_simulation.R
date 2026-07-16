set.seed(14)

N <- 30

alpha.true <- 0; beta.true <- 1; tau.true <- 0.2

gamma.true <- rnorm(N)

theta.true <- rnorm(N,
                    mean = alpha.true + beta.true * gamma.true,
                    sd = tau.true)

sigma.true <- runif(N, 0.1, 0.5)
delta.true <- runif(N, 5, 10)
rho.true <- runif(N, -0.1, 0.1)

# Sigma <- matrix(c(0.2^2, 0.5 * 0.2 * 0.2,
#                   0.5 * 0.2 * 0.2, 0.2^2),
#                 nrow = 2, ncol = 2, byrow = TRUE)

Sigma <- array(0, dim = c(N, 2, 2))

for (i in 1:N) {
  Sigma[i, 1, 1] <- sigma.true[i]^2
  Sigma[i, 1, 2] <- rho.true[i] * sigma.true[i] * delta.true[i]
  Sigma[i, 2, 1] <- rho.true[i] * sigma.true[i] * delta.true[i]
  Sigma[i, 2, 2] <- delta.true[i]^2
}

library(MASS)

theta.hat = gamma.hat = c()

for (i in 1:N) {
  
  obs <- mvrnorm(1,
                mu = c(theta.true[i], gamma.true[i]),
                Sigma = Sigma[i,,])
  
  theta.hat[i] = obs[1]
  gamma.hat[i] = obs[2]
}

df <- data.frame(study = 1:N,
                 theta.hat = theta.hat,
                 sigma.hat = sigma.true,
                 gamma.hat = gamma.hat,
                 delta.hat = delta.true,
                 rho.hat = rho.true)

write.xlsx(df, "C:\\Users\\AnandaBiswas\\OneDrive - EBM Health Consultants LLP\\Documents\\R-Programs\\Self-Study\\0012_data.xlsx")
