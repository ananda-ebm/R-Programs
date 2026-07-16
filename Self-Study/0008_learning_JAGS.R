library(rjags)

jags.version()

y <- c(8, 9, 10, 11, 12)

# sampling distribution > N(mu, sd = 2)
# prior distribution > N(0, precision = 0.0001)

dataList <- list(
  y = y,
  N = length(y),
  tau = 1/4 # as sd(sigma) = 2
)

model.path <- "C:\\Users\\AnandaBiswas\\OneDrive - EBM Health Consultants LLP\\Documents\\R-Programs\\Self-Study\\0008_model.txt"

model <- jags.model(file = model.path,
                    data = dataList)

update(model, 1000)

samples <- coda.samples(model,
                        variable.names = "mu",
                        n.iter = 5000)

summary(samples)
