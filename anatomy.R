library(igraph) 
ga.data <- read.csv('ga_edgelist.csv', header=TRUE) 
g <- graph.data.frame(ga.data, directed=FALSE)
summary(g)
V(g) $name
g$layout <- layout.fruchterman.reingold(g)
plot(g)

#Betweenness
betweenness(g, v=V(g), directed = FALSE, weights = NULL,
           nobigint = TRUE, normalized = FALSE)
edge.betweenness(g, e=E(g), directed = FALSE, weights = NULL)
betweenness.estimate(g, vids = V(g), directed = FALSE, cutoff = 0,
                     weights = NULL, nobigint = TRUE)
edge.betweenness.estimate(g, e=E(g),
                          directed = FALSE, cutoff = 0, weights = NULL)

 
V(g)$label <- betweenness(g)

plot(g)

len <- length(V(g))
betweenness <- matrix(V(g)$name, len, 2, FALSE)
colnames(betweenness, do.NULL = FALSE)
colnames(betweenness) <- c("Character","Betweenness")


for(i in 1:len)
{
  betweenness[i,2] <- V(g)$label[i]
}
betweenness

maxBet 
for(i in 1:len)
{
  betweenness[i,2] <- V(g)$label[i]
}


#Closeness
closeness(g, vids = V(g), mode = c("out", "in", "all", "total"),
          weights = NULL, normalized = FALSE)

estimate_closeness(g, vids = V(g), mode = c("out", "in", "all",
                                                    "total"), cutoff = 0, weights = NULL, normalized = FALSE)

V(g)$label <- closeness(g)

plot(g)
closeness <- matrix(V(g)$name, len, 2, FALSE)
colnames(closeness, do.NULL = FALSE)
colnames(closeness) <- c("Character","Closeness")

for(i in 1:len)
{
  closeness[i,2] <- V(g)$label[i]
}

closeness