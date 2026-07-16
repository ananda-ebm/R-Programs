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

# using textConnection() function

model.string <- "model{
	for(i in 1:n){
		x[i] ~ dnorm(mu, tau)
	}
	
	mu ~ dnorm(5, 0.0001)
	
	tau ~ dgamma(3, 1)
	
	sigma <- 1/sqrt(tau)
}"

model <- jags.model(file = textConnection(model.string),
                    data = dataList)

update(model, 1000)

samples <- coda.samples(model = model,
                        variable.names = c("mu", "sigma", "tau"), # name of the variables you want posterior estimate of; you have to make sure mentioned variables are defined properly in .txt file
                        n.iter = 5000)

summary(samples)
