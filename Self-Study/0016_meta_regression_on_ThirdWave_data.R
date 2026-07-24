library(dmetar)

data("ThirdWave")

View(ThirdWave)

library(meta)

m.gen <- metagen(TE = TE,
                 seTE = seTE,
                 studlab = Author,
                 data = ThirdWave,
                 sm = "SMD",
                 common = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 method.random.ci = "HK")

summary(m.gen)

year <- c(2014, 1998, 2010, 1999, 2005, 2014,
          2019, 2010, 1982, 2020, 1978, 2001,
          2018, 2002, 2009, 2011, 2011, 2013)

ThirdWave$year <- year

m.gen.reg <- metareg(m.gen, year)

summary(m.gen.reg)


bubble(m.gen.reg, studlab = TRUE)

library(tidyverse)

library(ggrepel)

ThirdWave %>%
  ggplot(aes(x = year, y = TE, size = m.gen$w.random, label = Author)) +
  geom_point(alpha = 0.5) +
  geom_text_repel() +
  theme(legend.position = "none")
