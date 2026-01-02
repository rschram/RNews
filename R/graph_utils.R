#' Filter Graph for Visualization (Strict Subset)
#' @param g The full igraph object
#' @param focus_doc_ids Vector of doc IDs matched by search (or NULL for all)
#' @return visNetwork data list (Nodes/Edges)
prepare_visual_graph <- function(g, focus_doc_ids = NULL) {
  
  # A. GLOBAL VIEW (No Search)
  if (is.null(focus_doc_ids) || length(focus_doc_ids) == 0) {
    # SAFETY CHECK: If graph is huge, return a simplified "Skeleton"
    # (Top 20% of nodes by Degree to prevent crash)
    if (igraph::vcount(g) > 5000) {
      message(">> Graph too large for global view. Subsetting core...")
      core_nodes <- which(igraph::degree(g) > quantile(igraph::degree(g), 0.8))
      g_sub <- igraph::induced_subgraph(g, core_nodes)
      return(visNetwork::toVisNetworkData(g_sub))
    }
    return(visNetwork::toVisNetworkData(g))
  }
  
  # B. SEARCH VIEW (Strict Subset)
  # Instead of sending the whole graph with ghosting, we send ONLY the relevant nodes.
  
  # 1. Expand selection to include immediate neighbors (Context)
  # This makes the graph "walkable" from the search results
  bfs_result <- igraph::ego(g, order = 1, nodes = focus_doc_ids)
  keep_ids <- unique(names(unlist(bfs_result)))
  
  # 2. Strict Subset
  g_sub <- igraph::induced_subgraph(g, keep_ids)
  
  # 3. Convert
  vis_data <- visNetwork::toVisNetworkData(g_sub)
  
  vis_data$nodes$size <- 50 
  vis_data$nodes$shape <- "dot"
  
  # Keep your border highlight logic
  if (!is.null(focus_doc_ids)) {
    vis_data$nodes$color.border <- ifelse(vis_data$nodes$id %in% focus_doc_ids, "black", "#2B7CE9")
    vis_data$nodes$borderWidth <- ifelse(vis_data$nodes$id %in% focus_doc_ids, 3, 1)
  }
  
  return(vis_data)
}

#' 1. Edge Composition Inspector
#' Returns the terms that connect two documents.
#' @param doc_id_A ID of first document
#' @param doc_id_B ID of second document
#' @param dtm The sparse document-term matrix
#' @param top_n Number of terms to return
get_edge_composition <- function(doc_id_A, doc_id_B, dtm, top_n = 20) {
  # Safety Check
  if (!doc_id_A %in% rownames(dtm) || !doc_id_B %in% rownames(dtm)) {
    return(data.frame(Term = "Error", Contribution = 0))
  }
  
  # Get vectors for both docs
  vec_A <- dtm[doc_id_A, , drop = FALSE]
  vec_B <- dtm[doc_id_B, , drop = FALSE]
  
  # Find shared terms (Exact Match)
  # (We assume sparse matrix logic here)
  terms_A <- names(which(vec_A[1, ] > 0))
  terms_B <- names(which(vec_B[1, ] > 0))
  common  <- intersect(terms_A, terms_B)
  
  if (length(common) == 0) return(data.frame(Term="None", Contribution=0))
  
  # Calculate "Contribution" (Product of weights)
  # If term is strong in both A and B, it's a strong connector.
  scores <- as.numeric(vec_A[1, common] * vec_B[1, common])
  
  data.frame(Term = common, Contribution = scores) %>%
    dplyr::arrange(dplyr::desc(Contribution)) %>%
    head(top_n)
}

#' Dynamic Graph Builder
#' Uses Embeddings (if available) or DTM to calculate similarity.
build_dynamic_graph <- function(dtm, vectors = NULL, threshold = 0.3, vocab_terms = NULL) {
  
  # A. Choose Similarity Source
  if (!is.null(vectors)) {
    # --- MODE 1: SEMANTIC EMBEDDINGS (The "Real" Model) ---
    valid_ids <- intersect(rownames(dtm), rownames(vectors))
    
    if(length(valid_ids) == 0) {
      matrix_for_sim <- dtm
    } else {
      matrix_for_sim <- vectors[valid_ids, , drop=FALSE]
    }
    
  } else {
    # --- MODE 2: WORD OVERLAP (Fallback) ---
    dtm_active <- dtm
    if (!is.null(vocab_terms)) {
      dtm_active <- dtm[, vocab_terms, drop=FALSE]
      dtm_active <- dtm_active[Matrix::rowSums(dtm_active) > 0, ]
    }
    matrix_for_sim <- dtm_active
  }
  
  # B. Calculate Similarity
  sim <- text2vec::sim2(matrix_for_sim, method = "cosine", norm = "l2")
  
  # C. Thresholding
  sim[sim < threshold] <- 0
  diag(sim) <- 0
  
  # D. Build Graph
  g <- igraph::graph_from_adjacency_matrix(sim, mode = "max", weighted = TRUE)
  g <- igraph::induced_subgraph(g, igraph::degree(g) > 0)
  
  return(g)
}

#' Calculate Fast Graph Metrics
#' Avoids slow computations like extensive path lengths on dense graphs.
get_graph_metrics <- function(g) {
  if (is.null(g) || igraph::vcount(g) == 0) return(NULL)
  
  # 1. Giant Component Ratio (GCR)
  comps <- igraph::components(g)
  gcr <- max(comps$csize) / igraph::vcount(g)
  
  # 2. Modularity (Community Structure)
  # Louvain is fast; if graph is tiny, it might fail, so wrap in try
  mod <- tryCatch({
    comm <- igraph::cluster_louvain(igraph::as.undirected(g))
    igraph::modularity(comm)
  }, error = function(e) 0)
  
  # 3. Transitivity (Clustering Coefficient)
  trans <- igraph::transitivity(g, type = "global")
  
  # 4. Assortativity (Degree correlation)
  assort <- igraph::assortativity_degree(g, directed = FALSE)
  
  # 5. Edge Density (Basic but useful)
  dens <- igraph::edge_density(g)
  
  list(
    Nodes = igraph::vcount(g),
    Edges = igraph::ecount(g),
    GCR = gcr,
    Modularity = mod,
    Transitivity = trans,
    Assortativity = assort,
    Density = dens
  )
}
