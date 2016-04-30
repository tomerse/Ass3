---
title: "Ass3"   
author: "Tomer Belzer & Tomer Segal"   
date: "April 30, 2016"   
output: html_document
---
```{r}
ga.data <- read.csv('ga_edgelist.csv', header=TRUE, stringsAsFactors=FALSE)
ga.vrtx <- read.csv('ga_actors.csv', header=TRUE, stringsAsFactors=FALSE)
g <- graph.data.frame(ga.data, vertices=ga.vrtx, directed=FALSE)
g$layout <- layout.fruchterman.reingold(g)

# Color nodes by gender.
V(g)$size <- 7 # Set size to all nodes
V(g)$color <- "powderblue"
females <- which(V(g)$gender == "F")
V(g)$color[females] <- "pink" 
plot(g)

# Remove nodes` labels.
# Use a different layout function.
V(g)$label <- NA # remove labels for now 
g$layout <- layout.kamada.kawai(g)
plot(g)
```
![](https://github.com/tomerse/Ass3/blob/master/images/anatomy-graph.PNG)

```{r}
#Betweenness
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

betweenness <- sortByField(betweenness, "between")

head(betweenness)
```
![](https://github.com/tomerse/Ass3/blob/master/images/anatomy-betweenes.PNG)

```{r}
#Closeness
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

closeness <- sortByField(closeness, "closeness")

head(closeness)
```

![](https://github.com/tomerse/Ass3/blob/master/images/anatomy-closennes.PNG)

```{r}
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

eigenvector <- sortByField(eigenvector, "eigenvector")

head(eigenvector)
```

![](https://github.com/tomerse/Ass3/blob/master/images/anatomy-eigenvector.PNG)

```{r}
#Walktrap community finding algorithm
wc <- cluster_walktrap(g)
plot(wc, g)
```

![](https://github.com/tomerse/Ass3/blob/master/images/anatomy-walktrap.PNG)

```{r}
algorithm(wc)
```

![](https://github.com/tomerse/Ass3/blob/master/images/anatomy-alg-walk.PNG)

```{r}
groups(wc)
```

![](https://github.com/tomerse/Ass3/blob/master/images/anatomy-groups-walk.PNG)

```{r}
modularity(wc)
```

![](https://github.com/tomerse/Ass3/blob/master/images/anatomy-mod-walk.PNG)

```{r}
#leading eigenvector algorithm
wc <- cluster_leading_eigen(g, steps = -1, weights = NULL, start = NULL,
                            options = arpack_defaults, callback = NULL, extra = NULL,
                            env = parent.frame())
plot(wc, g)
```

![](https://github.com/tomerse/Ass3/blob/master/images/anatomy-leading eigenvector.PNG)

```{r}
algorithm(wc)
```

![](https://github.com/tomerse/Ass3/blob/master/images/anatomy-alg-lead.PNG)

```{r}
groups(wc)
```

![](https://github.com/tomerse/Ass3/blob/master/images/anatomy-groups-lead.PNG)

```{r}
modularity(wc)
```

![](https://github.com/tomerse/Ass3/blob/master/images/anatomy-mod-lead.PNG)

======

```{r}
ga.data <- read.csv('~/R_workshop/ga/tp-edges.csv', header=TRUE, stringsAsFactors=FALSE)
ga.vrtx <- read.csv('~/R_workshop/ga/tp-vertices.csv', header=TRUE, stringsAsFactors=FALSE)
g <- graph.data.frame(ga.data, vertices=ga.vrtx, directed=FALSE)

# Color nodes by team/player
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
```

![](https://github.com/tomerse/Ass3/blob/master/images/team-graph.PNG)

```{r}
#Betweenness
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

#removing teams from graph
betweenness <- betweenness[-c(1:8), ]

betweenness <- sortByField(betweenness, "between")

head(betweenness)
```

![](https://github.com/tomerse/Ass3/blob/master/images/team-betweenes.PNG)

```{r}
#Closeness
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

#removing teams from graph
closeness <- closeness[-c(1:8), ]

closeness <- sortByField(closeness, "closeness")

head(closeness)
```

![](https://github.com/tomerse/Ass3/blob/master/images/team-closennes.PNG)

```{r}
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

#removing teams from graph
eigenvector <- eigenvector[-c(1:8), ]

eigenvector <- sortByField(eigenvector, "eigenvector")

head(eigenvector)
```

![](https://github.com/tomerse/Ass3/blob/master/images/team-eigenvector.PNG)

```{r}
#Walktrap community finding algorithm
wc <- cluster_walktrap(g)
plot(wc, g)
```

![](https://github.com/tomerse/Ass3/blob/master/images/team-walktrap.PNG)

```{r}
algorithm(wc)
```

![](https://github.com/tomerse/Ass3/blob/master/images/team-alg-walk.PNG)

```{r}
groups(wc)
```

![](https://github.com/tomerse/Ass3/blob/master/images/team-groups-walk.PNG)

```{r}
modularity(wc)
```

![](https://github.com/tomerse/Ass3/blob/master/images/team-mod-walk.PNG)

```{r}
#leading eigenvector algorithm
wc <- cluster_leading_eigen(g, steps = -1, weights = NULL, start = NULL,
                            options = arpack_defaults, callback = NULL, extra = NULL,
                            env = parent.frame())
plot(wc, g)
```

![](https://github.com/tomerse/Ass3/blob/master/images/team-leadingeigenvector.PNG)

```{r}
algorithm(wc)
```

![](https://github.com/tomerse/Ass3/blob/master/images/team-alg-lead.PNG)

```{r}
groups(wc)
```

![](https://github.com/tomerse/Ass3/blob/master/images/team-groups-lead.PNG)

```{r}
modularity(wc)
```

![](https://github.com/tomerse/Ass3/blob/master/images/team-mod-lead.PNG)
