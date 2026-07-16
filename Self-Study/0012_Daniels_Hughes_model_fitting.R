library(readxl)

data.path <- "C:\\Users\\AnandaBiswas\\OneDrive - EBM Health Consultants LLP\\Documents\\R-Programs\\Self-Study\\0012_data.xlsx"
file.exists(data.path)

df <- read_excel(data.path)

str(df)

View(df)

N <- nrow(df)

Sigma <- array(0, dim = c(N, 2, 2))

for (i in 1:N) {
  Sigma[i, 1, 1] <- df$sigma.hat[i]^2
  Sigma[i, 1, 2] <- df$rho.hat[i] * df$sigma.hat[i] * df$delta.hat[i]
  Sigma[i, 2, 1] <- df$rho.hat[i] * df$sigma.hat[i] * df$delta.hat[i]
  Sigma[i, 2, 2] <- df$delta.hat[i]^2
}

sigma_c_sq <- length(df$sigma.hat) / sum(1 / df$sigma.hat^2)

dataList <- list(
  N = N,
  theta_hat = df$theta.hat,
  gamma_hat = df$gamma.hat,
  Sigma = Sigma,
  sigma_c_sq = sigma_c_sq
)

library(cmdstanr)

model.path <- "C:\\Users\\AnandaBiswas\\OneDrive - EBM Health Consultants LLP\\Documents\\R-Programs\\Self-Study\\0012_Daniels_Hughes_model.stan"

model <- cmdstan_model(stan_file = model.path)

fit1 <- model$sample(
  data = dataList,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  seed = 123
)

fit1$summary(c("alpha", "beta", "tau"))
