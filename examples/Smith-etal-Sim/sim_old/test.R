vars <- read.table("examples/Smith-etal-Sim/sim/first.txt", header = T)
adj <- read.table("examples/Smith-etal-Sim/sim/adjacency.txt")
res <- read.table("examples/Smith-etal-Sim/sim/out.txt", skip = 6)

obs <- res[,2]
exp <- res[,3]

x <- vars[,2]
y <- vars[,3]
plot(x,y)
points(x,y, cex = .2* sign(obs-exp)*(obs - exp)^2/exp, col = "blue") 
points(x,y, cex = -.2*sign(obs-exp)*(obs - exp)^2/exp, col = "red") 

chisq <- sum( (obs - exp)^2/exp)
