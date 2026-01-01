library(igraph)

# ==============================================================================
# PHASE 3: The Topology (Graph Construction & Global Metrics)
# ==============================================================================

#' Build Graph from Similarity Matrix
build_graph <- function(sim_matrix, threshold = 0.001) {
  
  # 1. Binarize/Threshold
  adj <- sim_matrix
  adj[adj < threshold] <- 0
  
  # 2. Create Graph
  # mode = "max" is the safe way to enforce undirected behavior on sparse matrices in igraph 1.6+
  g <- graph_from_adjacency_matrix(adj, mode = "max", weighted = TRUE, diag = FALSE)
  
  return(g)
}

#' Calculate Global Graph Metrics
calc_global_metrics <- function(g) {
  
  # 1. Survivors
  degs <- degree(g)
  survivors <- sum(degs > 0)
  n_total <- vcount(g)
  
  if (survivors < 2) {
    return(list(
      survivors = survivors,
      gcr = 0,
      modularity = NA,
      avg_path = NA,
      transitivity = NA,
      assortativity = NA
    ))
  }
  
  # 2. Giant Component Ratio
  comps <- components(g)
  gcr <- max(comps$csize) / n_total
  
  # 3. Transitivity
  trans <- transitivity(g, type = "global")
  
  # 4. Assortativity
  assort <- assortativity_degree(g)
  
  # 5. Average Path Length
  giant_node_ids <- which(comps$membership == which.max(comps$csize))
  giant_subgraph <- induced_subgraph(g, giant_node_ids)
  avg_path <- mean_distance(giant_subgraph, directed = FALSE)
  
  # 6. Modularity
  leiden_comm <- cluster_leiden(g, objective_function = "modularity", weights = E(g)$weight)
  mod_score <- modularity(g, membership(leiden_comm), weights = E(g)$weight)
  n_modules <- length(unique(membership(leiden_comm)))
  
  list(
    survivors = survivors,
    gcr = gcr,
    modularity = mod_score,
    n_modules = n_modules,
    avg_path = avg_path,
    transitivity = trans,
    assortativity = assort
  )
}

#' Bootstrap Global Metrics
#' 
#' Resamples the corpus, rebuilds the graph, and aggregates metrics.
#' Supports: "tfidf", "bm25", "binary", "embeddings_mean", "embeddings_weighted"
bootstrap_topology <- function(corpus, method = "tfidf", threshold = 0.2, n_iter = 30, vectors = NULL) {
  
  message(sprintf(">> Bootstrapping %s (Thresh: %.2f, Iter: %d)...", method, threshold, n_iter))
  
  results_df <- data.frame()
  
  # Validate Vectors for Embedding Methods
  if(str_detect(method, "embeddings") && is.null(vectors)) {
    stop("Error: You must provide 'vectors' for embedding methods.")
  }
  
  for(i in 1:n_iter) {
    
    # 1. Resample Documents
    all_docs <- unique(corpus$meta$doc_id)
    sampled_docs <- sample(all_docs, length(all_docs), replace = TRUE)
    
    sample_map <- data.frame(original_id = sampled_docs) %>%
      mutate(instance_id = paste0(original_id, "_", row_number()))
    
    # 2. Filter Tokens
    boot_tokens <- corpus$tokens %>%
      inner_join(sample_map, by = c("doc_id" = "original_id")) %>%
      select(doc_id = instance_id, term, n)
    
    boot_corpus <- list(
      tokens = boot_tokens,
      meta = data.frame(doc_id = sample_map$instance_id),
      term_stats = corpus$term_stats 
    )
    
    # 3. Build Matrix / Vectors (The Branching Logic)
    if (method == "embeddings_weighted") {
      # Method B: TF-IDF Weighted
      dtm <- get_weighted_document_vectors(boot_corpus$tokens, vectors)
      sim <- text2vec::sim2(dtm, method = "cosine", norm = "l2")
      
    } else if (method == "embeddings_mean") {
      # Method A: Simple Mean
      dtm <- get_mean_document_vectors(boot_corpus$tokens, vectors)
      sim <- text2vec::sim2(dtm, method = "cosine", norm = "l2")
      
    } else {
      # Standard Sparse Methods
      dtm <- get_document_matrix(boot_corpus, method = method)
      sim <- compute_similarity(dtm, method = ifelse(method=="binary", "jaccard", "cosine"))
    }
    
    # 4. Build Graph & Metrics
    g <- build_graph(sim, threshold)
    met <- calc_global_metrics(g)
    met$iteration <- i
    results_df <- bind_rows(results_df, as.data.frame(met))
  }
  
  # 5. Summarize (Robust Quantile Method)
  summary_stats <- results_df %>%
    summarise(across(everything(), list(
      mean = mean, 
      sd = sd,
      ci_low  = ~ mean(.x) - (1.96 * sd(.x)),
      ci_high = ~ mean(.x) + (1.96 * sd(.x))
    ), .names = "{.col}_{.fn}")) %>%
    select(-starts_with("iteration"))
  
  return(list(summary = summary_stats, raw = results_df))
}

#' Measure Global Bridge Scores (Betweenness)
#' 
#' Calculates "Bridgeness" without needing clusters.
#' It identifies documents that act as "Brokers" or "Gatekeepers" in the network.
#'
#' @param g The igraph object from build_graph().
#' @param normalize Boolean. If TRUE, scales score 0-1.
#' @return A dataframe of documents ranked by bridge score.
measure_node_bridges <- function(g, normalize = TRUE) {
  
  message(">> Calculating Betweenness Centrality (Global Bridge Score)...")
  
  # 1. Calculate Betweenness
  # (How many shortest paths pass through this node?)
  # For 2,000 nodes, this takes ~1-2 seconds. For 50,000, it can be slow.
  betw <- betweenness(g, directed = FALSE, normalized = normalize)
  
  # 2. Calculate Burt's Constraint (Structural Holes)
  # (Low Constraint = High Bridging Opportunity)
  # "Am I connected to people who are connected to each other?" (High Constraint)
  # "Am I connected to people who DON'T know each other?" (Low Constraint -> Bridge)
  cons <- constraint(g)
  
  # 3. Calculate Degree (for context)
  deg <- degree(g)
  
  # 4. Compile Results
  # We invert constraint to make a positive "Structural Hole Score"
  results <- data.frame(
    doc_id = V(g)$name,
    bridge_score_betweenness = betw,
    structural_hole_score = round(1 - cons, 3), # Higher is better
    degree = deg,
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(bridge_score_betweenness))
  
  return(results)
}

#' Perform Similarity Parameter Sweep with Structural Stress Analysis
#' 
#' @param corpus The corpus object containing $tokens and $meta.
#' @param vectors The embeddings matrix (required for embedding methods).
#' @param method The similarity method ("embeddings_weighted", "embeddings_mean", "tfidf", etc.).
#' @param thresholds A numeric vector of thresholds to test (e.g., seq(0.1, 0.8, 0.05)).
#' @return A dataframe of topological metrics for each threshold.
perform_similarity_sweep <- function(corpus, vectors = NULL, method = "embeddings_weighted", thresholds = seq(0.1, 0.8, 0.05)) {
  
  message(sprintf(">> Starting Similarity Sweep (%s) across %d thresholds...", method, length(thresholds)))
  
  # 1. PRE-CALCULATE SIMILARITY MATRIX (Efficiency Optimization)
  # We do this once so we don't rebuild the matrix inside the loop.
  
  if (str_detect(method, "embeddings")) {
    if (is.null(vectors)) stop("Error: 'vectors' must be provided for embedding methods.")
    
    if (method == "embeddings_weighted") {
      dtm <- get_weighted_document_vectors(corpus$tokens, vectors)
    } else {
      dtm <- get_mean_document_vectors(corpus$tokens, vectors)
    }
    # Calculate Cosine Similarity
    sim_matrix <- text2vec::sim2(dtm, method = "cosine", norm = "l2")
    
  } else {
    # Baseline methods (TF-IDF, etc.)
    dtm <- get_document_matrix(corpus, method = method)
    sim_matrix <- compute_similarity(dtm, method = ifelse(method=="binary", "jaccard", "cosine"))
  }
  
  results_list <- list()
  total_docs <- nrow(corpus$meta)
  
  # 2. RUN THE SWEEP
  for (t in thresholds) {
    
    # A. Build Graph at Threshold
    adj <- sim_matrix
    adj[adj < t] <- 0
    g <- graph_from_adjacency_matrix(adj, mode = "max", weighted = TRUE, diag = FALSE)
    
    # B. Basic Connectivity Metrics
    comps <- components(g)
    giant_id <- which.max(comps$csize)
    gcr <- max(comps$csize) / total_docs
    
    survivors <- sum(degree(g) > 0)
    survivor_pct <- survivors / total_docs
    
    # C. Structure Metrics (Modularity)
    # Use Leiden to find topics
    leiden_comm <- cluster_leiden(g, objective_function = "modularity", weights = E(g)$weight, resolution_parameter = 1.0)
    modularity_score <- modularity(g, membership(leiden_comm), weights = E(g)$weight)
    n_modules <- length(unique(membership(leiden_comm)))
    
    # D. Structural Stress (Betweenness) - CALCULATED ON GIANT COMPONENT ONLY
    # We isolate the giant component to measure the stress on the "connected world"
    giant_nodes <- names(comps$membership[comps$membership == giant_id])
    
    if (length(giant_nodes) > 2) {
      g_giant <- induced_subgraph(g, giant_nodes)
      
      # Calculate Average Path (Efficiency)
      avg_path <- mean_distance(g_giant, directed = FALSE)
      
      # Calculate Betweenness (Stress)
      # Normalized = TRUE allows comparison across different graph sizes
      node_betw <- betweenness(g_giant, normalized = TRUE)
      avg_betw <- mean(node_betw)
      max_betw <- max(node_betw)
      
    } else {
      # Fallback if graph is shattered
      avg_path <- 0
      avg_betw <- 0
      max_betw <- 0
    }
    
    # E. Store Results
    metrics <- data.frame(
      Threshold = t,
      GCR = gcr,
      Modularity = modularity_score,
      Survivor_Pct = survivor_pct,
      Avg_Path = avg_path,
      Avg_Betweenness = avg_betw,   # <--- The Leading Indicator (Systemic Stress)
      Max_Betweenness = max_betw,   # <--- The Keystone Indicator (Bridge Strength)
      Cluster_Count = n_modules
    )
    
    results_list[[as.character(t)]] <- metrics
    
    # Optional: Print progress for slow runs
    if (which(thresholds == t) %% 5 == 0) message(sprintf("   ...finished threshold %.2f", t))
  }
  
  results_df <- bind_rows(results_list)
  return(results_df)
}