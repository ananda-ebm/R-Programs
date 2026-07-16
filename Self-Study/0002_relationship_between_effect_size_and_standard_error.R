set.seed(14)

true.theta <- 0

sample.size <- rep(50:150, 50)

effect.size <- c()

se <- c()

for (i in 1:length(sample.size)) {
  s <- rnorm(sample.size[i], mean = true.theta)
  
  effect.size[i] <- mean(s)
  
  se[i] <- sd(s) / sqrt(sample.size[i])
}

library(tidyverse)

data.frame(es = effect.size, log.se = log(se)) %>%
  ggplot(aes(x = es, y = log.se)) +
  geom_point() +
  labs(x = "Effect Size", y = "log(Standard Error)") +
  theme_classic()
