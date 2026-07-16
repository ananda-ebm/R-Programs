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

m.gen$TE.random

m.gen$TE.common # or m.gen$TE.fixed

m.gen_update <- update(m.gen, method.tau = "PM")
m.gen_update$TE.random

m.gen_update$tau2
m.gen$tau2
