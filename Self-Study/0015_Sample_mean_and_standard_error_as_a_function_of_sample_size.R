set.seed(14)

means = ses = c()

n <- 2:500

for (i in 1:length(n)) {
  s <- rnorm(n[i], 10, 2)
  
  means[i] <- mean(s)
  
  ses[i] <- sd(s) / n[i]
}

library(tidyverse)

data.frame(size = n, mean = means) %>%
  ggplot(aes(x = size, y = mean)) +
  geom_line(linewidth = 1) +
  labs(x = "Sample Size", y = "Mean")

data.frame(size = n, se = ses) %>%
  ggplot(aes(x = size, y = se)) +
  geom_line(linewidth = 1) +
  labs(x = "Sample Size", y = "Standard Error")
