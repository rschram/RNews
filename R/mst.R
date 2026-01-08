library(igraph)
library(Matrix)
library(dplyr)
library(ggraph)

#' Core Topology Pipeline
#' @param corpus A standard corpus list object (meta + tokens)
#' @param label String label for the corpus (e.g., "FijiTimes")
#' @param artifacts Vector of terms to exclude (e.g. c("ft", "letters"))
run_topology_pipeline <- function(corpus, label, artifacts = c()) {
  
  message(sprintf(">> [%s] Starting Topology Pipeline...", label))
  
  # 1. Sparse Matrix Generation (Full Vocabulary)
  dtm <- get_document_matrix(corpus, method = "binary")
  
  # Remove artifacts
  keep_terms <- setdiff(colnames(dtm), artifacts)
  dtm <- dtm[, keep_terms]
  
  message(sprintf("   Vocabulary: %d terms", ncol(dtm)))
  
  # 2. Similarity (Sparse Math)
  # A. Intersection (A AND B)
  inter <- Matrix::crossprod(dtm)
  
  # B. Union (A OR B)
  s <- Matrix::diag(inter)
  union_mat <- outer(s, s, "+") - as.matrix(inter) # Note: 'outer' creates dense, be careful if >20k terms
  
  # Optimization for 45k terms: 
  # If 'outer' crashes RAM, use the sparse Jaccard approximation or looping.
  # For 2000 docs, the non-zero co-occurrences are usually manageable.
  
  sim_mat <- as.matrix(inter) / union_mat
  sim_mat[is.nan(sim_mat)] <- 0
  diag(sim_mat) <- 0
  
  # 3. Distance & MST
  dist_mat <- 1 - sim_mat
  
  # Build Full Graph (Weighted)
  g_full <- graph_from_adjacency_matrix(dist_mat, mode = "undirected", weighted = TRUE, diag = FALSE)
  
  message("   Calculating Minimum Spanning Tree...")
  g_mst <- mst(g_full, weights = E(g_full)$weight)
  
  # 4. Metrics (The "Skeleton" Report)
  # Diameter = The Semantic Gradient Depth
  d_diam <- diameter(g_mst, directed=FALSE)
  
  # Leaf Ratio = Specificity Index
  leaf_ratio <- sum(degree(g_mst) == 1) / vcount(g_mst)
  
  # Betweenness = Middleware Detection
  # (Only calculate for top 10% nodes to save time if N > 20k)
  bet <- betweenness(g_mst, directed = FALSE)
  
  # 5. Return Object
  list(
    label = label,
    mst = g_mst,
    full_graph = g_full, # Keep for "Chords" phase
    metrics = data.frame(
      Metric = c("Nodes", "Edges", "Diameter", "Leaf_Ratio", "Max_Betweenness"),
      Value = c(vcount(g_mst), ecount(g_mst), d_diam, leaf_ratio, max(bet))
    ),
    betweenness = bet
  )
}

#' Add Chords (The "MST-Plus" Layer)
#' @param topo_result The output from run_topology_pipeline
#' @param k_factor Multiplier for extra edges (e.g., 1.0 = add N edges)
add_chords <- function(topo_result, k_factor = 1.0) {
  
  g_mst <- topo_result$mst
  g_full <- topo_result$full_graph
  
  n_edges_to_add <- vcount(g_mst) * k_factor
  message(sprintf(">> Adding %d chord edges...", as.integer(n_edges_to_add)))
  
  # 1. Get all potential edges sorted by weight (Distance)
  # We want the SMALLEST distance (Strongest similarity) that isn't in the tree
  # Note: This effectively re-sorts the edge list.
  
  # Extract edge list from full graph
  el_full <- as_data_frame(g_full, what = "edges")
  el_mst  <- as_data_frame(g_mst, what = "edges")
  
  # Create a composite ID to filter existing edges
  el_full$id <- paste(pmin(el_full$from, el_full$to), pmax(el_full$from, el_full$to))
  el_mst$id  <- paste(pmin(el_mst$from, el_mst$to), pmax(el_mst$from, el_mst$to))
  
  # Filter: Candidates are edges in Full but NOT in MST
  candidates <- el_full[!el_full$id %in% el_mst$id, ]
  
  # Sort by Weight (Low Distance = High Similarity)
  candidates <- candidates[order(candidates$weight), ]
  
  # 2. Select Top K
  top_chords <- head(candidates, n_edges_to_add)
  
  # 3. Add to Graph
  g_plus <- g_mst %>%
    add_edges(as.vector(t(as.matrix(top_chords[, c("from", "to")]))), 
              attr = list(weight = top_chords$weight, type = "chord"))
  
  return(g_plus)
}

plot_topology_map <- function(g, root_node, title, filename) {
  
  # Find Root Index
  root_id <- which(V(g)$name == root_node)
  if(length(root_id) == 0) root_id <- 1
  
  # Centrality for Coloring
  V(g)$deg <- degree(g)
  
  p <- ggraph(g, layout = 'tree', circular = TRUE, root = root_id) + 
    # The Chords (Faint)
    geom_edge_link(aes(filter = (type == "chord")), 
                   color = "lightblue", alpha = 0.2, width = 0.1) +
    # The Skeleton (Dark)
    geom_edge_diagonal(aes(filter = is.na(type)), 
                       color = "grey40", width = 0.3) +
    # The Nodes
    geom_node_point(aes(color = log(deg)), size = 0.5, show.legend = FALSE) +
    # Labels (Outer Ring)
    geom_node_text(aes(x = x*1.05, y = y*1.05, 
                       label = name, 
                       angle = -((-node_angle(x, y) + 90) %% 180) + 90,
                       filter = leaf),
                   hjust = 'outward', size = 1.5, alpha = 0.8) +
    theme_void() +
    scale_color_viridis_c(option = "magma") +
    expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3)) +
    labs(title = title, subtitle = "Skeleton (Grey) + Chords (Blue)")
  
  ggsave(filename, plot = p, width = 20, height = 20, limitsize = FALSE)
  message(sprintf("Saved %s", filename))
}

#' Analyze MST Shape and Trace Lineages
#' @param mst_res The result object from analyze_semantic_mst
analyze_tree_shape <- function(mst_res) {
  g <- mst_res$graph
  root <- mst_res$root
  
  # 1. Tree Metrics
  diam <- diameter(g, directed = FALSE, unconnected = TRUE)
  mean_path <- mean_distance(g, directed = FALSE, unconnected = TRUE)
  
  # Leafiness (Nodes with degree 1)
  leaves <- sum(degree(g) == 1)
  leaf_ratio <- leaves / vcount(g)
  
  # Assortativity (Do high-degree nodes connect to high-degree nodes?)
  # In a semantic tree, this is usually negative (Hubs connect to Leaves)
  assort <- assortativity_degree(g)
  
  message(sprintf(">> Tree Shape Metrics:"))
  message(sprintf("   Diameter (Max Depth): %.2f steps", diam))
  message(sprintf("   Avg Path Length:      %.2f steps", mean_path))
  message(sprintf("   Leaf Ratio:           %.1f%% (Specific terms)", leaf_ratio * 100))
  message(sprintf("   Assortativity:        %.3f (Hierarchy Index)", assort))
  
  return(list(diameter=diam, mean_path=mean_path, leaf_ratio=leaf_ratio))
}

#' Trace Semantic Lineage
#' Shows the path from the Root (Generic) to a specific target term.
#' @param mst_res The result object
#' @param target_term The specific word to trace (e.g., "christian")
trace_lineage <- function(mst_res, target_term) {
  g <- mst_res$graph
  root <- mst_res$root
  
  # Check if term exists
  if (!target_term %in% V(g)$name) {
    message("Term not found in top N terms.")
    return(NULL)
  }
  
  # Find Shortest Path (Unique in a tree)
  path <- shortest_paths(g, from = root, to = target_term, mode = "all")
  
  # Extract Names
  path_names <- V(g)$name[path$vpath[[1]]]
  
  # Print nicely
  message(sprintf("\n>> Lineage for '%s':", target_term))
  cat(paste(path_names, collapse = " -> "))
  cat("\n")
}


# # Execution 
# # 1. Run Pipeline
# res_A <- run_topology_pipeline(corpus_A, "FijiTimes", artifacts=c("ft","editor"))
# res_B <- run_topology_pipeline(corpus_B, "LongCorpus", artifacts=c("page","table"))
# 
# # 2. Compare Metrics
# report <- merge(res_A$metrics, res_B$metrics, by="Metric", suffixes = c("_A", "_B"))
# print(report)
# 
# # 3. Add Chords
# g_plus_A <- add_chords(res_A, k_factor = 1.0)
# g_plus_B <- add_chords(res_B, k_factor = 1.0)
# 
# # 4. Render
# # Identify the Roots (Max Degree node)
# root_A <- names(which.max(degree(res_A$mst)))
# root_B <- names(which.max(degree(res_B$mst)))
# 
# plot_topology_map(g_plus_A, root_A, "Corpus A: Topological Map", "map_A.pdf")
# plot_topology_map(g_plus_B, root_B, "Corpus B: Topological Map", "map_B.pdf")