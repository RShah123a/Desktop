

#TASK 1.1.1----------------------------------------------------------

euclidean <- function(a, b) {
  return(sqrt(sum((a - b)^2)))
}

euclidean(123612, 13892347912)
euclidean(100, 1000)

#TASK 1.1.2----------------------------------------------------------


library(cppRouting)
DJAlgo<-function(){
edges <-
  data.frame(from_vertex=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
         to_vertex=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
         cost=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
nodes<-unique(c(edges$from_vertex,edges$to_vertex))
directed_graph<-makegraph(edges,directed=TRUE)
non_directed<-makegraph(edges,directed=FALSE)
dir_dist<-get_distance_matrix(Graph=directed_graph, from=nodes, to=nodes, allcores=FALSE)
non_dir_dist<-get_distance_matrix(Graph=non_directed, from=nodes, to=nodes, allcores=FALSE)
print(dir_dist)
print(non_dir_dist)
}

DJAlgo

#package.skeleton(name="rshah")









