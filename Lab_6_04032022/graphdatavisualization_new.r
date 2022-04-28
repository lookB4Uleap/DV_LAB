#install.packages("igraph")
# install.packages("igraph")
library(igraph)
g<-graph.empty(n=10, directed=TRUE)
plot(g)

g<-graph.full(n=10, directed = FALSE, loops = FALSE)
plot(g)

g<-graph.star(n=10, mode="out")
plot(g)

g<-graph.star(n=10, mode="in")

plot(g)
g<-graph.ring(n=10)
plot(g)

edges <- c(1,2, 3,2, 2,4)
g<-graph(edges, n=max(edges), directed=TRUE)
plot(g)
E(g)
V(g)
m <-get.adjacency(g)
m
plot(g)
g
g1 <- graph( edges=c(1,2, 2,3, 3, 1), n=3, directed=F ) 
plot(g1) 
class(g1)
g1
# Now with 10 vertices, and directed by default:

g2 <- graph( edges=c(1,2, 2,3, 3, 1), n=10 )

plot(g2)   
g2
g3 <- graph( c("John", "Jim", "Jim", "Jill", "Jill", "John")) # named vertices

# When the edge list has vertex names, the number of nodes is not needed

plot(g3)
g3
plot(g3, edge.arrow.size=.5, vertex.color="gold", vertex.size=15, 
     
     vertex.frame.color="gray", vertex.label.color="black", 
     
     vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2)
#Small graphs can also be generated with a description of this kind:
#- for undirected tie,
#+- or -+ for directed ties pointing left & right,
#++ for a symmetric tie, 
#and ":" for sets of vertices.
plot(graph_from_literal(a---b, b---c)) # the number of dashes doesn't matter
plot(graph_from_literal(a--+b, b+--c))
plot(graph_from_literal(a+-+b, b+-+c)) 
plot(graph_from_literal(a:b:c---c:d:e))
gl <- graph_from_literal(a-b-c-d-e-f, a-g-h-b, h-e:f:i, j)

plot(gl)
g1[]
# to get the vertex attribute 
g <- make_ring(10) %>%
  set_vertex_attr("label", value = LETTERS[1:10])
g
plot(g)

g <- graph.ring(10)
g <- set.graph.attribute(g, "name", "RING")
# It is the same as
g$name <- "RING"
g$name

g <- set.vertex.attribute(g, "color", value=c("red", "green"))
get.vertex.attribute(g, "color")
plot(g)

g <- set.edge.attribute(g, "weight", value=runif(ecount(g)))
get.edge.attribute(g, "weight")
plot(g)

#######

nodes <- read.csv("G://Dataset1-Media-Example-NODES.csv")
links <- read.csv("G://Dataset1-Media-Example-EDGES.csv")


head(nodes)
head(links)
nrow(nodes); 
length(unique(nodes$id))
nrow(links);
nrow(unique(links[,c("from", "to")]))
links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL
links

library(igraph)
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
class(net)
net

E(net)       # The edges of the "net" object
V(net)       # The vertices of the "net" object
E(net)$type  # Edge attribute "type"
V(net)$media # Vertex attribute "media"

plot(net, edge.arrow.size=.4,vertex.label=NA)
net <- simplify(net, remove.multiple = F, remove.loops = T) 
plot(net, edge.arrow.size=.4,vertex.label=NA)
plot(net, edge.arrow.size=.2, edge.curved=0,
     vertex.color="orange", vertex.frame.color="#555555",
     vertex.label=V(net)$media, vertex.label.color="black",
     vertex.label.cex=.7) 
# The second way to set attributes is to add them to the igraph object.

# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

# Set node size based on audience size:
V(net)$size <- V(net)$audience.size*0.7

# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label.color <- "black"
V(net)$label <- NA

# Set edge width based on weight:
E(net)$width <- E(net)$weight/6

#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"

plot(net)
plot(net,vertex.label=V(net)$media) 

hist(links$weight)
mean(links$weight)
sd(links$weight)

# Heatmap of the network matrix:
netm <- get.adjacency(net, attr="weight", sparse=F)
netm
colnames(netm) <- V(net)$media
rownames(netm) <- V(net)$media

palf <- colorRampPalette(c("glod", "dark orange")) 
heatmap(netm[,17:1], Rowv = NA, Colv = NA, col = palf(20), 
        scale="none", margins=c(10,10) )



