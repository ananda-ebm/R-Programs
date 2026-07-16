true.mu <- 5; true.sigma <- 2
true.tau <- 1/true.sigma^2

x <- rnorm(10, mean = true.mu, sd = true.sigma)

library(rjags)

# sampling distribution > N(mu, sigma^2)
# prior distribution for mu > N(mean = 5, precision = 0.0001)
# prior distribution for tau > Gamma(3, 1)

dataList <- list(
  x = x,
  n = length(x)
)

model.path <- "C:\\Users\\AnandaBiswas\\OneDrive - EBM Health Consultants LLP\\Documents\\R-Programs\\Self-Study\\0009_model.txt"

model <- jags.model(file = model.path,
                    data = dataList)

update(model, 1000)

samples <- coda.samples(model = model,
                        variable.names = c("mu", "sigma", "tau"), # name of the variables you want posterior estimate of; you have to make sure mentioned variables are defined properly in .txt file
                        n.iter = 5000)

summary(samples)
