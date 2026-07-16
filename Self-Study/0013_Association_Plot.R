data.path <- "C:\\Users\\AnandaBiswas\\OneDrive - EBM Health Consultants LLP\\Documents\\R-Programs\\Self-Study\\0013_data.xlsx"

library(readxl)

df <- read_excel(data.path)

View(df)

library(tidyverse)

df %>%
  ggplot(aes(x = gamma.hat, y = theta.hat)) +
  geom_point(size = 2, color = "red") +
  geom_smooth(method = "lm", formula = "y ~ x") +
  labs(x = "TE on Surrogate",
       y = "TE on Final endpoint",
       title = "Association Plot")
