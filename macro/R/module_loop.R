random_directed_graph_by_density <- function(vertex_cnt, edge_cnt) {
  require(igraph)
  # a complete graph has n (n-1) / 2 edges.
  gg <- igraph::make_empty_graph(n = vertex_cnt, directed = TRUE)
  for (add_idx in 1:10000) {
    candidate <- sample(1:vertex_cnt, 2)
    existing <- E(gg)[.from(candidate[1]) & .to(candidate[2])]
    if (length(existing) == 0) {
      added <- add.edges(gg, candidate)
      if (is.dag(added)) {
        gg <- added
        if (ecount(gg) == edge_cnt) break
      }
    }
  }
  gg
}



step_mainloop <- function(mainloop) {
  module_graph <- mainloop$task_graph
  vertex_attr(module_graph, "done") <- FALSE
  data_cache <- vector(mode = "list", length = 0L)

  while (!all(vertex_attr(module_graph, "done"))) {
    inputs_met <- lapply(
      adjacent_vertices(module_graph, V(module_graph), mode = "in"),
      function(x) all(vertex_attr(module_graph, "done", x))
      )
    inputs_met_names <- names(inputs_met)[as.logical(inputs_met)]
    to_run <- inputs_met_names[V(module_graph)[inputs_met_names]$done]

    data_cache <- mainloop$run_modules(to_run, data_cache)
    V(module_graph)[to_run]$done <- TRUE
  }
  mainloop
}


test_mainloop <- function() {
  gg <- random_directed_graph_by_density(5, 7)
  vertex_attr()
}
