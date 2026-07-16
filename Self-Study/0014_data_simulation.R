set.seed(14)

muS <- 3; muT <- 2; alpha <- 1.3; beta <- 0.95

D <- matrix(c(1.0, 0.4, 0, 0,
              0.4, 1, 0, 0,
              0, 0, 0.25, 0.225,
              0, 0, 0.225, 0.25),
            nrow = 4, ncol = 4, byrow = TRUE)

Sigma <- matrix(c(1, 0.6,
                  0.6, 1),
                nrow = 2, ncol = 2, byrow = TRUE)

N <- 40

second.stage.random.components <- matrix(NA, nrow = 4, ncol = N)

library(MASS)

for (i in 1:N) {
  s <- mvrnorm(1,
               mu = rep(0, 4),
               Sigma = D)
  
  second.stage.random.components[,i] <- s
}

second.stage.random.components

first.stage.parameters <- matrix(c(muS, muT, alpha, beta), 
                                 nrow = 4, ncol = N) + second.stage.random.components

first.stage.parameters

n.patients <- 100

first.stage.random.components <- matrix(NA, nrow = 2, ncol = N*n.patients)

for (i in 1:(N*n.patients)) {
  s <- mvrnorm(1,
               mu = rep(0, 2),
               Sigma = Sigma)
  
  first.stage.random.components[,i] <- s
}

first.stage.random.components

treatment.assignment <- matrix(NA, nrow = N, ncol = n.patients)

for (i in 1:N) {
  treatment.assignment[i,] <- rbinom(n.patients, 1, 0.5)
}

outcome.S = outcome.T = matrix(NA, nrow = N, ncol = n.patients)

for (i in 1:N) {
  for (j in 1:n.patients) {
    outcome.S[i, j] <- first.stage.parameters[1, i] +
                       first.stage.parameters[3, i] * treatment.assignment[i, j] +
                       first.stage.random.components[1,((i-1)*n.patients + j)]
    
    outcome.T[i, j] <- first.stage.parameters[2, i] +
                       first.stage.parameters[4, i] * treatment.assignment[i, j] +
                       first.stage.random.components[2,((i-1)*n.patients + j)]
  }
}

outcome.S <- as.data.frame(outcome.S)
outcome.T <- as.data.frame(outcome.T)

rownames(outcome.S) <- paste0("Trial", " ", 1:N)
colnames(outcome.S) <- paste0("Patient", " ", 1:n.patients)

rownames(outcome.T) <- paste0("Trial", " ", 1:N)
colnames(outcome.T) <- paste0("Patient", " ", 1:n.patients)

library(tidyverse)

outcome.S_melted <- outcome.S %>%
  pivot_longer(cols = starts_with("Patient"),
               names_to = "Patient No",
               values_to = "outcome.S")

View(outcome.S_melted)

outcome.T_melted <- outcome.T %>%
  pivot_longer(cols = starts_with("Patient"),
               names_to = "Patient No",
               values_to = "outcome.T")

View(outcome.T_melted)

df <- cbind(outcome.S_melted, outcome.T_melted[,2])
View(df)

df <- cbind(rep(1:40, each = 100), df)
View(df)

names(df)[1] <- "Trial ID"

View(df)

write.xlsx(df, "C:\\Users\\AnandaBiswas\\OneDrive - EBM Health Consultants LLP\\Documents\\R-Programs\\Self-Study\\0014_data.xlsx")
