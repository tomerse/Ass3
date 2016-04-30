library(igraph)
source("helper.R")

ga.data <- read.csv('~/R_workshop/ga/tp-edges.csv', header=TRUE, stringsAsFactors=FALSE)
ga.vrtx <- read.csv('~/R_workshop/ga/tp-vertices.csv', header=TRUE, stringsAsFactors=FALSE)
g <- graph.data.frame(ga.data, vertices=ga.vrtx, directed=FALSE)


# Choose a layout scheme and plot the networks.
g$layout <- layout.fruchterman.reingold(g)

#Betweenness
betweenness(g, v=V(g), directed = FALSE, weights = NULL,
            nobigint = TRUE, normalized = FALSE)
edge.betweenness(g, e=E(g), directed = FALSE, weights = NULL)
betweenness.estimate(g, vids = V(g), directed = FALSE, cutoff = 0,
                     weights = NULL, nobigint = TRUE)
edge.betweenness.estimate(g, e=E(g),
                          directed = FALSE, cutoff = 0, weights = NULL)


V(g)$label <- betweenness(g)

len <- length(V(g))

betweenness <- data.frame(name = letters[1:len], 
                  between = 1:len, stringsAsFactors = FALSE)


for(i in 1:len)
{
  betweenness[i,"name"] <- V(g)$name[i]
}

for(i in 1:len)
{
  betweenness[i,"between"] <- V(g)$label[i]
}

betweenness <- betweenness[-c(1:8), ]

betweenness <- sortByField(betweenness, "between")

head(betweenness)

#Closeness
closeness(g, vids = V(g), mode = c("out", "in", "all", "total"),
          weights = NULL, normalized = FALSE)

estimate_closeness(g, vids = V(g), mode = c("out", "in", "all",
                                            "total"), cutoff = 0, weights = NULL, normalized = FALSE)

V(g)$label <- closeness(g)

closeness <- data.frame(name = letters[1:len], 
                        closeness = 1:len, stringsAsFactors = FALSE)

for(i in 1:len)
{
  closeness[i,"name"] <- V(g)$name[i]
}

for(i in 1:len)
{
  closeness[i,"closeness"] <- V(g)$label[i]
}

closeness <- closeness[-c(1:8), ]

closeness <- sortByField(closeness, "closeness")

head(closeness)

#Eigenvector

V(g)$label <- evcent(g)$vector

eigenvector <- data.frame(name = letters[1:len], 
                          eigenvector = 1:len, stringsAsFactors = FALSE)

for(i in 1:len)
{
  eigenvector[i,"name"] <- V(g)$name[i]
}

for(i in 1:len)
{
  eigenvector[i,"eigenvector"] <- V(g)$label[i]
}

eigenvector <- eigenvector[-c(1:8), ]

eigenvector <- sortByField(eigenvector, "eigenvector")

head(eigenvector)

# Color nodes by gender.
V(g)$size <- 7 # Set size to all nodes
V(g)$color <- "powderblue"
teams <- which(V(g)$team == "TRUE")
V(g)$color[teams] <- "pink" 
plot(g)

# Remove nodes` labels.
# Use a different layout function.
V(g)$label <- NA # remove labels for now 
g$layout <- layout.kamada.kawai(g)
plot(g)

#Walktrap community finding algorithm
wc <- cluster_walktrap(g)
algorithm(wc)
groups(wc)
modularity(wc)
plot(wc, g)

#leading eigenvector method
wc <- cluster_leading_eigen(g, steps = -1, weights = NULL, start = NULL,
                            options = arpack_defaults, callback = NULL, extra = NULL,
                            env = parent.frame())
algorithm(wc)
groups(wc)
modularity(wc)
plot(wc, g)
