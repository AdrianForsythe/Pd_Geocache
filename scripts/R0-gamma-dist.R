library(epitrix)

# convert R0 mean and sd into a distribution using:
param <- gamma_mucv2shapescale(2.100262, 0.8563529) # convertion to Gamma parameters

si <- distcrete::distcrete("gamma", interval = 1,
                           shape = param$shape,
                           scale = param$scale, w = 0)
si

set.seed(1)
x <- si$r(1000)
head(x, 10)

hist(x, col = "grey", border = "white",
     xlab = "Days between primary and secondary onset",
     main = "Simulated serial intervals")


si_fit <- fit_disc_gamma(x)
