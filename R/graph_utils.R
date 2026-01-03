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

#' Extract Graph Backbone (MST + Strongest Edges)
#' Preserves topology while reducing visual density.
get_graph_backbone <- function(g, keep_fraction = 0.05) {
  
  if (igraph::ecount(g) == 0) return(g)
  
  # 1. Calculate MST (Minimize negative weight)
  igraph::E(g)$neg_weight <- -igraph::E(g)$weight
  g_mst <- igraph::mst(g, weights = igraph::E(g)$neg_weight)
  
  # 2. Helper to create robust IDs (Sorted A|B)
  get_edge_ids <- function(df) {
    p1 <- pmin(df$from, df$to)
    p2 <- pmax(df$from, df$to)
    paste(p1, p2, sep="|")
  }
  
  # 3. Identify Edges
  edges_df <- igraph::as_data_frame(g, what = "edges")
  mst_edges <- igraph::as_data_frame(g_mst, what = "edges")
  
  mst_ids <- get_edge_ids(mst_edges)
  
  # 4. Top Strongest Edges
  n_total <- nrow(edges_df)
  n_keep <- ceiling(n_total * keep_fraction)
  
  sorted_edges <- edges_df[order(edges_df$weight, decreasing = TRUE), ]
  top_edges <- head(sorted_edges, n_keep)
  top_ids <- get_edge_ids(top_edges)
  
  # 5. Union & Filter
  keep_ids <- union(mst_ids, top_ids)
  
  el_df <- as.data.frame(igraph::as_edgelist(g), stringsAsFactors = FALSE)
  colnames(el_df) <- c("from", "to")
  all_ids <- get_edge_ids(el_df)
  
  g_backbone <- igraph::subgraph.edges(g, which(all_ids %in% keep_ids), delete.vertices = FALSE)
  
  return(g_backbone)
}

#' Explain SCM Edge (Vectorized)
get_scm_edge_breakdown <- function(doc_id_A, doc_id_B, dtm, vectors, top_n = 10) {
  
  # 1. Get Terms
  vec_A <- dtm[doc_id_A, , drop = FALSE]
  vec_B <- dtm[doc_id_B, , drop = FALSE]
  
  terms_A <- names(which(vec_A[1, ] > 0))
  terms_B <- names(which(vec_B[1, ] > 0))
  
  # 2. Filter Vectors
  all_terms <- union(terms_A, terms_B)
  valid_terms <- intersect(all_terms, rownames(vectors))
  
  if (length(valid_terms) == 0) return(NULL)
  
  sub_vecs <- vectors[valid_terms, , drop = FALSE]
  
  # 3. Calculate Sim Matrix (valid_terms x valid_terms)
  term_sim <- text2vec::sim2(sub_vecs, method = "cosine")
  
  # 4. Calculate Contribution Matrix (Algebra instead of Loops)
  # Extract weights as dense vectors for the valid subset
  wA <- as.numeric(vec_A[1, valid_terms])
  wB <- as.numeric(vec_B[1, valid_terms])
  
  # Outer product: Matrix of (weight_A * weight_B)
  weight_mat <- outer(wA, wB)
  
  # Element-wise multiply by similarity
  contrib_mat <- weight_mat * term_sim
  
  # 5. Filter & Format
  # Set generic threshold to ignore noise
  contrib_mat[contrib_mat < 0.3] <- 0
  
  # Find indices of non-zero contributions
  # arr.ind=TRUE returns row/col indices
  hits <- which(contrib_mat > 0, arr.ind = TRUE)
  
  if (nrow(hits) == 0) return(data.frame(Info="No significant semantic links"))
  
  # Construct Result DataFrame
  results <- data.frame(
    Term_A = valid_terms[hits[,1]],
    Term_B = valid_terms[hits[,2]],
    Sim = term_sim[hits],
    Contrib = contrib_mat[hits],
    stringsAsFactors = FALSE
  )
  
  # Sort and return
  results %>%
    arrange(desc(Contrib)) %>%
    head(top_n) %>%
    mutate(
      Pair = paste0(Term_A, " <-> ", Term_B),
      Score = round(Contrib, 3)
    ) %>%
    select(Pair, Score, Sim)
}


