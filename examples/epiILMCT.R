source("pairwise-distances.R")

library(EpiILMCT)

coords<-all_results_merge[!duplicated(all_results_merge$i),c("lat","lon")]
dis<-as.matrix(dist(coords, method = "euclidean"))

# beta is set to an arbitrary value
c.net<-contactnet(type = "powerlaw",location = coords,beta = 0.5)

# kernel.par and delta are set to arbitrary values
epi1<-datagen(type = "SIR",kerneltype = "distance",kernelmatrix = c.net,distancekernel = "powerlaw", kernel.par = 0.1,delta = c(1,2))

kernel1 <- list(2, c("gamma", 1, 0.01, 0.5))

## performing the MCMC-tool for analyzing the fully observed spatial data 
## under the SIR distance-based continuous ILM:
mcmcres2 <- epictmcmc(object = epi1,distancekernel="powerlaw",datatype="known epidemic",nsim=1000,kernel.par = kernel1)

plot(mcmcres2)
print(mcmcres2)
summary(mcmcres2)
