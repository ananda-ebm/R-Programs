library(dmetar)

data("SuicidePrevention")

View(SuicidePrevention)

library(metafor)

SP_calc <- escalc(measure = "SMD",
                  m1i = mean.e, m2i = mean.c,
                  sd1i = sd.e, sd2i = sd.c,
                  n1i = n.e, n2i = n.c,
                  data = SuicidePrevention) %>%
  as.data.frame()

View(SP_calc)

SP_calc$yi - 1.96 * sqrt(SP_calc$vi)
SP_calc$yi + 1.96 * sqrt(SP_calc$vi)

SP_calc$w <- 1 / SP_calc$vi^2

# fixed effects model
pooled.effect <- sum(SP_calc$w * SP_calc$yi) / sum(SP_calc$w)
pooled.effect
