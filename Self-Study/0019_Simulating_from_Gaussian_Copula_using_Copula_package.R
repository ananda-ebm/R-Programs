library(copula)

cop <- normalCopula(param = 0.7, dim = 2)

U <- rCopula(10^3, cop)

cor(U[,1], U[,2], method = "spearman")

(6 / pi) * asin(0.7 / 2)
