#' Dijkstra Algorithm
#'
#' @param graph A data frame that represents the graph.
#' @param  init_node A number that represents the starting node .
#' @return Shortest path to every other node
#' @examples
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)
dijkstra <- function(graph,init_node){
  stopifnot(is.data.frame(graph),is.numeric(init_node))
  nodes <- length(unique(c(graph$v1,graph$v2)))
  Q <- c(1:nodes)
  dist <- rep(Inf,nodes)
  prev <- rep(333,nodes)
  dist[init_node] <- 0
  while(length(Q)!=0){
    u <- which(dist==dist[Q][which.min(dist[Q])])
    Q <- Q[Q!=u]
    v_nodes <- graph[which(graph['v1']==u),]
    v2_nodes <- v_nodes['v2']
    final_v2 <- v2_nodes[(unlist(v2_nodes) %in% Q),]
    for(i in unlist(final_v2)){
      alt <- dist[u] + v_nodes[which(v_nodes['v2']==i),]$w
      if (alt<dist[i]){
        dist[i] <- alt
        prev[i] <- u
      }
    }
    indi_dist <- dist[Q]
  }
  return(dist)
}