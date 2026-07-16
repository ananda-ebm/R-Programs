library(dmetar)
data("SuicidePrevention")

library(meta)

View(SuicidePrevention)

m.cont <- metacont(n.e = n.e,
                   mean.e = mean.e,
                   sd.e = sd.e,
                   n.c = n.c,
                   mean.c = mean.c,
                   sd.c = sd.c,
                   studlab = author,
                   sm = "SMD",
                   method.smd = "Hedges",
                   common = FALSE,
                   random = TRUE,
                   data = SuicidePrevention,
                   method.tau = "REML",
                   method.random.ci = "HK")

summary(m.cont)
