vars _ read.table ("first.txt", header = T)
adj _ read.table ("adjacency.txt")
res _ read.table ("out.txt", skip = 6)

obs _ res[,2]
exp _ res[,3]

x _ vars[,2]
y _ vars[,3]
plot(x,y, type = "n")
points(x,y, cex = .2* sign(obs-exp)*(obs - exp)^2/exp, col = "blue") 
points(x,y, cex = -.2*sign(obs-exp)*(obs - exp)^2/exp, col = "red") 

chisq _ sum( (obs - exp)^2/exp)
