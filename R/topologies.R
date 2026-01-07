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
      # Standard Sparse Methods & SCM
      dtm <- get_document_matrix(boot_corpus, method = method)
      
      if (method == "scm") {
        sim <- compute_similarity(dtm, method = "scm", vectors = vectors)
      } else {
        sim <- compute_similarity(dtm, method = ifelse(method=="binary", "jaccard", "cosine"))
      }
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
 
#' Perform k-Nearest Neighbor (k-NN) Sweep for Percolation Analysis
#' 
#' @param corpus The corpus object containing $tokens and $meta.
#' @param method The similarity method ("jaccard", "cosine", "tfidf").
#' @param k_levels A numeric vector of k values to test (e.g., 1:20).
#' @return A dataframe of topological metrics for each level of k.
perform_knn_sweep <- function(corpus, method = "jaccard", k_levels = 1:20) {
  
  message(sprintf(">> Starting k-NN Sweep (%s) across k = %d to %d...", 
                  method, min(k_levels), max(k_levels)))
  
  # 1. PRE-CALCULATE FULL SIMILARITY MATRIX
  # For k-NN, we need the full ranked similarities for every document
  dtm <- get_document_matrix(corpus, method = method)
  
  # Ensure we use Jaccard for binary similarity if method is set to 'jaccard'
  sim_matrix <- compute_similarity(dtm, method = ifelse(method == "jaccard", "jaccard", "cosine"))
  
  # Remove self-loops (diagonal) so a node isn't its own neighbor
  diag(sim_matrix) <- 0
  
  results_list <- list()
  total_docs <- nrow(corpus$meta)
  
  # 2. RUN THE k-NN SWEEP
  for (k in k_levels) {
    
    # A. BUILD k-NN ADJACENCY MATRIX
    # For each row, keep only the top k similarities
    adj <- matrix(0, nrow = nrow(sim_matrix), ncol = ncol(sim_matrix))
    rownames(adj) <- rownames(sim_matrix)
    colnames(adj) <- colnames(sim_matrix)
    
    for (i in 1:nrow(sim_matrix)) {
      # Find indices of top k values
      # Using 'partial' sort or order to handle the ranking
      top_indices <- order(sim_matrix[i, ], decreasing = TRUE)[1:k]
      adj[i, top_indices] <- sim_matrix[i, top_indices]
    }
    
    # B. CREATE GRAPH
    # Note: k-NN is inherently directed (A likes B, but B might not like A).
    # We use mode = "max" to make it undirected (if either A or B is in top k, they link).
    g <- graph_from_adjacency_matrix(adj, mode = "max", weighted = TRUE, diag = FALSE)
    
    # C. CONNECTIVITY & PERCOLATION METRICS
    comps <- components(g)
    giant_id <- which.max(comps$csize)
    gcr <- max(comps$csize) / total_docs
    
    # In k-NN, survivors are usually 100% since every node gets k edges,
    # but we check degree > 0 in case of empty documents
    survivors <- sum(degree(g) > 0)
    survivor_pct <- survivors / total_docs
    
    # D. MODULARITY (THE ARCHIPELAGO RESOLUTION)
    leiden_comm <- cluster_leiden(g, objective_function = "modularity", 
                                  weights = E(g)$weight, resolution_parameter = 1.0)
    modularity_score <- modularity(g, membership(leiden_comm), weights = E(g)$weight)
    n_modules <- length(unique(membership(leiden_comm)))
    
    # E. STRUCTURAL STRESS (CALCULATED ON GIANT COMPONENT)
    giant_nodes <- names(comps$membership[comps$membership == giant_id])
    
    if (length(giant_nodes) > 2) {
      g_giant <- induced_subgraph(g, giant_nodes)
      avg_path <- mean_distance(g_giant, directed = FALSE)
      
      node_betw <- betweenness(g_giant, normalized = TRUE)
      avg_betw <- mean(node_betw)
      max_betw <- max(node_betw)
    } else {
      avg_path <- 0; avg_betw <- 0; max_betw <- 0
    }
    
    # F. STORE RESULTS
    results_list[[as.character(k)]] <- data.frame(
      k = k,
      GCR = gcr,
      Modularity = modularity_score,
      Survivor_Pct = survivor_pct,
      Avg_Path = avg_path,
      Avg_Betweenness = avg_betw,
      Max_Betweenness = max_betw,
      Cluster_Count = n_modules
    )
    
    if (k %% 5 == 0) message(sprintf("   ...finished k = %d", k))
  }
  
  return(bind_rows(results_list))
}

#' Extract Discursive Bridge Terms from the k=1 Percolation Skeleton
#' 
#' @param corpus The corpus object containing $tokens and $meta.
#' @param method The similarity method ("jaccard" or "cosine").
#' @param top_n Number of keystone edges to inspect for bridge terms.
#' @return A ranked dataframe of terms that hold the 108 modules together.
get_percolation_bridges <- function(corpus, method = "jaccard", top_n = 50) {
  
  message(">> Extracting Signal DTM...")
  # Use the pre-existing document matrix logic from your suite
  # Assuming get_document_matrix is available in your environment
  dtm_signal <- get_document_matrix(corpus, method = method)
  
  message(">> Building k=1 Skeleton...")
  # 1. Calculate Full Similarity Matrix
  # (Crucial for identifying the single best neighbor for every doc)
  sim_matrix <- compute_similarity(dtm_signal, method = method)
  diag(sim_matrix) <- 0
  
  # 2. Build k=1 Adjacency Matrix
  # Every row (document) gets exactly one edge to its most similar neighbor
  adj <- matrix(0, nrow = nrow(sim_matrix), ncol = ncol(sim_matrix))
  rownames(adj) <- rownames(sim_matrix)
  colnames(adj) <- colnames(sim_matrix)
  
  for (i in 1:nrow(sim_matrix)) {
    # If a document has no similarity to others, skip to avoid errors
    if(max(sim_matrix[i, ]) > 0) {
      top_index <- which.max(sim_matrix[i, ])
      adj[i, top_index] <- sim_matrix[i, top_index]
    }
  }
  
  # 3. Create Graph and Calculate Edge Betweenness
  # mode="max" makes it undirected; if A's best is B, they are linked.
  g_skele <- graph_from_adjacency_matrix(adj, mode = "max", weighted = TRUE, diag = FALSE)
  
  message(">> Identifying Keystone Edges (Bottlenecks)...")
  # High betweenness on a k=1 graph identifies the "trunks" connecting the modules
  eb <- edge_betweenness(g_skele)
  
  # Get indices of the top N edges with the most 'stress'
  keystone_indices <- order(eb, decreasing = TRUE)[1:min(top_n, gsize(g_skele))]
  keystone_ends    <- ends(g_skele, keystone_indices)
  
  message(">> Mapping Bridges to Signal Terms...")
  bridge_term_counts <- list()
  
  for(i in 1:nrow(keystone_ends)) {
    doc_a <- keystone_ends[i, 1]
    doc_b <- keystone_ends[i, 2]
    
    # Identify terms shared by the two documents connected by the keystone edge
    # These terms are the physical 'reason' the modules are connected
    shared_indices <- which(dtm_signal[doc_a, ] > 0 & dtm_signal[doc_b, ] > 0)
    term_names <- colnames(dtm_signal)[shared_indices]
    
    for(term in term_names) {
      bridge_term_counts[[term]] <- (bridge_term_counts[[term]] %||% 0) + 1
    }
  }
  
  # 4. Final Ranking of Bridge Terms
  bridge_df <- data.frame(
    Term = names(bridge_term_counts),
    Bridge_Frequency = unlist(bridge_term_counts)
  ) %>%
    arrange(desc(Bridge_Frequency)) %>%
    mutate(Relative_Strength = Bridge_Frequency / top_n)
  
  return(bridge_df)
}

library(igraph)
library(dplyr)
library(ggplot2)
library(tidyr)

library(igraph)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gt) # For the nice table

# --- MODULE 1: MATRIX PREPARATION ---
#' Pre-calculate similarity matrix
get_knn_similarity <- function(corpus, method = "jaccard") {
  message(">> [Module 1] Computing Similarity Matrix...")
  dtm <- get_document_matrix(corpus, method = method)
  sim_matrix <- compute_similarity(dtm, method = ifelse(method == "jaccard", "jaccard", "cosine"))
  diag(sim_matrix) <- 0
  return(sim_matrix)
}

# --- MODULE 2: TOPOLOGY ENGINE (WITH Z-SCORES) ---
analyze_topology <- function(g, n_rand = 5) {
  
  # Ensure undirected
  if(is_directed(g)) g <- as.undirected(g, mode = "collapse")
  
  # 1. OBSERVED METRICS
  n_nodes <- vcount(g)
  n_edges <- ecount(g)
  comps <- components(g)
  gcr <- max(comps$csize) / n_nodes
  
  # Transitivity & Assortativity
  trans_obs <- transitivity(g, type = "global")
  if(is.nan(trans_obs)) trans_obs <- 0
  
  assort_obs <- tryCatch(assortativity_degree(g), error = function(e) NA)
  
  # Path Length (on Giant Component)
  giant_indices <- which(comps$membership == which.max(comps$csize))
  
  if (length(giant_indices) > 10) {
    g_giant <- induced_subgraph(g, giant_indices)
    path_obs <- mean_distance(g_giant, directed = FALSE, weights = NA)
    betw_max <- max(betweenness(g_giant, normalized = TRUE, directed = FALSE, weights = NA))
  } else {
    path_obs <- NA; betw_max <- NA
  }
  
  # 2. RANDOM BASELINES (Collecting Distribution)
  rand_stats <- data.frame(
    trans = numeric(n_rand),
    assort = numeric(n_rand), 
    path = numeric(n_rand)
  )
  
  if (n_rand > 0) {
    for(i in 1:n_rand) {
      g_rand <- sample_gnm(n = n_nodes, m = n_edges, directed = FALSE)
      
      # Transitivity
      r_trans <- transitivity(g_rand, type = "global")
      rand_stats$trans[i] <- ifelse(is.nan(r_trans), 0, r_trans)
      
      # Assortativity
      rand_stats$assort[i] <- assortativity_degree(g_rand)
      
      # Path Length
      r_comps <- components(g_rand)
      r_giant_idx <- which(r_comps$membership == which.max(r_comps$csize))
      
      if(length(r_giant_idx) > 1) {
        r_giant <- induced_subgraph(g_rand, r_giant_idx)
        rand_stats$path[i] <- mean_distance(r_giant, directed = FALSE, weights = NA)
      } else {
        rand_stats$path[i] <- NA
      }
    }
  }
  
  # 3. CALCULATE STATS & Z-SCORES
  # Helper to safe-guard against SD=0 (which causes Inf Z-scores)
  calc_z <- function(obs, values) {
    if(is.na(obs)) return(NA)
    mu <- mean(values, na.rm = TRUE)
    sigma <- sd(values, na.rm = TRUE)
    if(is.na(sigma) || sigma == 0) return(NA) # Avoid division by zero
    return((obs - mu) / sigma)
  }
  
  # Means
  trans_rand_mean  <- mean(rand_stats$trans, na.rm=TRUE)
  assort_rand_mean <- mean(rand_stats$assort, na.rm=TRUE)
  path_rand_mean   <- mean(rand_stats$path, na.rm=TRUE)
  
  # Z-Scores
  z_trans  <- calc_z(trans_obs, rand_stats$trans)
  z_assort <- calc_z(assort_obs, rand_stats$assort)
  z_path   <- calc_z(path_obs, rand_stats$path)
  
  # Sigma Calculation
  t_rand_safe <- ifelse(is.na(trans_rand_mean) || trans_rand_mean == 0, 1e-9, trans_rand_mean)
  p_rand_safe <- ifelse(is.na(path_rand_mean) || path_rand_mean == 0, 1e-9, path_rand_mean)
  sigma_sw <- (trans_obs / t_rand_safe) / (path_obs / p_rand_safe)
  
  # 4. RETURN
  return(list(
    GCR = gcr,
    Max_Betweenness = betw_max,
    Sigma = sigma_sw,
    
    # Values
    Transitivity = trans_obs,
    Transitivity_Rand = trans_rand_mean,
    Z_Transitivity = z_trans,
    
    Assortativity = assort_obs,
    Assortativity_Rand = assort_rand_mean,
    Z_Assortativity = z_assort,
    
    PathLength = path_obs,
    PathLength_Rand = path_rand_mean,
    Z_PathLength = z_path
  ))
}


# --- MODULE 3: THE SWEEP ---
run_knn_sweep <- function(corpus, method="jaccard", k_levels=1:10, n_rand=2) {
  
  sim_matrix <- get_knn_similarity(corpus, method)
  total_docs <- nrow(sim_matrix)
  
  output_list <- list()
  
  message(sprintf(">> [Module 3] Starting Sweep (k=%d to %d)...", min(k_levels), max(k_levels)))
  
  for (k in k_levels) {
    # Matrix Init
    adj <- matrix(0, nrow=total_docs, ncol=total_docs)
    rownames(adj) <- rownames(sim_matrix)
    colnames(adj) <- colnames(sim_matrix)
    
    # Top-K Selection
    for (i in 1:total_docs) {
      top_idx <- order(sim_matrix[i, ], decreasing = TRUE)[1:k]
      adj[i, top_idx] <- sim_matrix[i, top_idx]
    }
    
    # Graph Creation
    g_raw <- graph_from_adjacency_matrix(adj, mode="max", weighted=TRUE, diag=FALSE)
    g <- as.undirected(g_raw, mode = "collapse")
    
    # Analysis
    message(sprintf("   ... Analyzing k=%d", k))
    metrics <- analyze_topology(g, n_rand = n_rand)
    
    metrics$k <- k
    output_list[[as.character(k)]] <- as.data.frame(metrics)
  }
  
  return(bind_rows(output_list))
}

# --- MODULE 4: VISUALIZATION ---
plot_knn_metric <- function(df, metric_name, title = NULL) {
  
  obs_col <- metric_name
  rand_col <- paste0(metric_name, "_Rand")
  
  # Check if columns exist
  if (!all(c(obs_col, rand_col) %in% colnames(df))) {
    stop(sprintf("Columns '%s' or '%s' not found in dataframe.", obs_col, rand_col))
  }
  
  p <- ggplot(df, aes(x = k)) +
    # Random Baseline (Dashed, Gray) - Plot first so it's behind
    geom_line(aes_string(y = rand_col, color = "'Random'"), linetype = "dashed", size = 1) +
    # Observed Line (Solid, Blue)
    geom_line(aes_string(y = obs_col, color = "'Observed'"), size = 1.2) +
    geom_point(aes_string(y = obs_col, color = "'Observed'"), size = 3) +
    
    theme_minimal() +
    labs(title = ifelse(is.null(title), paste("k-NN Sweep:", metric_name), title),
         y = metric_name, x = "k (Nearest Neighbors)") +
    scale_color_manual(name = "Model", values = c("Observed" = "#0072B2", "Random" = "#999999")) +
    theme(legend.position = "bottom")
  
  return(p)
}

# --- MODULE 5: TABLE GENERATION (WITH Z-SCORES) ---
get_comparison_table <- function(sweep_df) {
  sweep_df %>%
    # Select key metrics and their Z-scores
    select(k, Sigma, 
           Transitivity, Z_Transitivity, 
           Assortativity, Z_Assortativity, 
           PathLength, Z_PathLength) %>%
    gt() %>%
    tab_header(
      title = "Structural Significance Analysis",
      subtitle = "Z-Scores indicate deviation from Random Baseline (Z > 1.96 is significant)"
    ) %>%
    fmt_number(columns = where(is.numeric), decimals = 3) %>%
    fmt_number(columns = starts_with("Z_"), decimals = 1) %>% # Z-scores usually 1 decimal
    
    # Rename columns for space
    cols_label(
      Z_Transitivity = "Z (Clust)",
      Z_Assortativity = "Z (Assort)",
      Z_PathLength = "Z (Path)",
      Transitivity = "Clustering"
    ) %>%
    
    # 1. Color Sigma (The "Small World" Signal) - Blue intensity
    data_color(
      columns = c(Sigma),
      colors = scales::col_numeric(palette = c("white", "#E6F5FF", "#0072B2"), domain = NULL)
    ) %>%
    
    # 2. Color Z-Scores (Diverging: Red = Lower than random, Blue = Higher)
    data_color(
      columns = starts_with("Z_"),
      colors = scales::col_bin(
        palette = c("#D55E00", "white", "#009E73"), # Red, White, Green
        domain = c(-100, 100), # Cap domain to prevent outliers skewing color
        bins = c(-Inf, -1.96, 1.96, Inf) # Only color if significant
      )
    )
}

library(igraph)
library(dplyr)

#' Extract Bridge Terms for a specific k-NN level
#' @param corpus The corpus object
#' @param k The k-NN level to analyze
#' @param top_n Number of highest-betweenness edges to inspect (default 200)
get_knn_bridges <- function(corpus, k, top_n = 200) {
  
  # 1. Build the specific k-NN Graph
  # We reuse the logic from the sweep, but for a single k
  sim_matrix <- get_knn_similarity(corpus, method = "jaccard")
  total_docs <- nrow(sim_matrix)
  
  adj <- matrix(0, nrow=total_docs, ncol=total_docs)
  rownames(adj) <- rownames(sim_matrix); colnames(adj) <- colnames(sim_matrix)
  
  for (i in 1:total_docs) {
    top_idx <- order(sim_matrix[i, ], decreasing = TRUE)[1:k]
    adj[i, top_idx] <- sim_matrix[i, top_idx]
  }
  
  g_raw <- graph_from_adjacency_matrix(adj, mode="max", weighted=TRUE, diag=FALSE)
  g <- as.undirected(g_raw, mode = "collapse")
  
  # 2. Identify Keystone Edges (High Betweenness)
  message(sprintf("   ... Calculating Edge Betweenness for k=%d (this may take a moment)", k))
  
  # We calculate betweenness for ALL edges to rank them accurately
  eb <- edge_betweenness(g, directed = FALSE)
  
  # Select the Top N edges (the structural highways)
  top_indices <- order(eb, decreasing = TRUE)[1:min(top_n, ecount(g))]
  keystone_ends <- ends(g, top_indices)
  
  # 3. Map Edges back to Shared Terms
  dtm <- get_document_matrix(corpus, method = "jaccard")
  term_counts <- list()
  
  for(i in 1:nrow(keystone_ends)) {
    doc_a <- keystone_ends[i, 1]
    doc_b <- keystone_ends[i, 2]
    
    # Find overlapping terms between the two connected docs
    shared_idx <- which(dtm[doc_a, ] > 0 & dtm[doc_b, ] > 0)
    terms <- colnames(dtm)[shared_idx]
    
    for(t in terms) {
      term_counts[[t]] <- (term_counts[[t]] %||% 0) + 1
    }
  }
  
  # Return sorted dataframe
  if(length(term_counts) == 0) return(data.frame(Term=character(), Freq=numeric()))
  
  return(data.frame(Term = names(term_counts), Freq = unlist(term_counts)) %>% 
           arrange(desc(Freq)))
}

#' Identify New Terms that emerge at Higher k
#' @param corpus Corpus object
#' @param k_low The baseline k (e.g., 2)
#' @param k_high The target k (e.g., 3)
compare_bridge_evolution <- function(corpus, k_low = 2, k_high = 3, top_n = 200) {
  
  message(sprintf(">> Comparing Bridges: k=%d vs k=%d...", k_low, k_high))
  
  # Get Baseline Terms
  df_low <- get_knn_bridges(corpus, k = k_low, top_n = top_n)
  message(sprintf("   > k=%d has %d unique bridge terms in top %d edges.", k_low, nrow(df_low), top_n))
  
  # Get Target Terms
  df_high <- get_knn_bridges(corpus, k = k_high, top_n = top_n)
  message(sprintf("   > k=%d has %d unique bridge terms in top %d edges.", k_high, nrow(df_high), top_n))
  
  # Find New Terms (In High but NOT in Low)
  new_terms <- df_high %>%
    filter(!Term %in% df_low$Term) %>%
    rename(Freq_at_k_high = Freq) %>%
    mutate(Relative_Strength = Freq_at_k_high / top_n) %>%
    arrange(desc(Freq_at_k_high))
  
  return(new_terms)
}

library(igraph)
library(ggraph)
library(tidygraph)
library(dplyr)
library(stringr)
library(ggplot2)
library(Matrix)

#' Plot Elbow Dendrogram with Entropy-Sorted Edge Labels
#' @param corpus The corpus object
#' @param meta The metadata object
#' @param filename Output filename
plot_elbow_dendrogram_entropy <- function(corpus, meta, filename = "fiji_tree_entropy.pdf") {
  
  # --- 1. PREP & ENTROPY CALCULATION ---
  message(">> Step 1: Calculating Term Entropy...")
  
  # Get Matrix
  dtm <- get_document_matrix(corpus, method = "jaccard")
  term_list <- colnames(dtm)
  
  # Get total frequency for each term (column sums)
  term_sums <- colSums(dtm)
  
  # FIX: Normalize COLUMNS of the original DTM
  # (N x M) %*% (M x M) = (N x M)
  # Result: Each column t sums to 1 (Distribution of Term t across Docs)
  P_dt <- dtm %*% Diagonal(x = 1/term_sums)
  
  # Calculate p * log(p)
  # Note: 0 * log(0) is 0, so we can ignore 0 entries safely in sparse matrix
  # We operate directly on the non-zero values (@x)
  P_dt@x <- P_dt@x * log(P_dt@x)
  
  # Sum columns to get negative entropy
  # H(t) = -sum( p(d|t) log p(d|t) )
  term_entropy <- -1 * colSums(P_dt)
  names(term_entropy) <- term_list
  
  message("   > Entropy calculated. Min: ", round(min(term_entropy),2), 
          " | Max: ", round(max(term_entropy),2))
  
  # --- 2. BUILD MST ---
  message(">> Step 2: Building MST...")
  
  sim_matrix <- get_knn_similarity(corpus, method = "jaccard")
  total_docs <- nrow(sim_matrix)
  adj <- matrix(0, nrow=total_docs, ncol=total_docs)
  rownames(adj) <- rownames(sim_matrix); colnames(adj) <- colnames(sim_matrix)
  
  for (i in 1:total_docs) {
    top_idx <- order(sim_matrix[i, ], decreasing = TRUE)[1]
    adj[i, top_idx] <- sim_matrix[i, top_idx]
  }
  
  g_raw <- graph_from_adjacency_matrix(adj, mode="max", weighted=TRUE, diag=FALSE)
  g_undirected <- as.undirected(g_raw, mode = "collapse")
  
  if(components(g_undirected)$no > 1) {
    cl <- components(g_undirected)
    g_undirected <- induced_subgraph(g_undirected, which(cl$membership == which.max(cl$csize)))
  }
  
  # --- 3. ORIENTATION & LABEL LOGIC (UPDATED FOR DEGREE ROOT) ---
  message(">> Step 3: Generating Labels (Rooted by Max Degree)...")
  
  # Calculate degree for all nodes
  degs <- degree(g_undirected)
  
  # Identify the Root as the node with the highest degree (The Attractor Hub)
  root_index <- which.max(degs)
  
  # Perform BFS starting from this hub to orient the tree for the dendrogram
  bfs_res <- bfs(g_undirected, root = root_index, mode = "all", order = TRUE, father = TRUE)
  
  fathers_int <- as.numeric(bfs_res$father)
  children_int <- as.numeric(bfs_res$order)
  valid_mask <- !is.na(fathers_int[children_int])
  
  child_indices <- children_int[valid_mask]
  father_indices <- fathers_int[child_indices]
  
  node_intersections <- vector("list", vcount(g_undirected)) 
  edge_labels <- character(length(child_indices))
  
  pb <- txtProgressBar(min = 0, max = length(child_indices), style = 3)
  
  for(i in seq_along(child_indices)) {
    child_idx <- child_indices[i]
    father_idx <- father_indices[i]
    
    # Intersection Logic
    row_f <- dtm[father_idx, ]
    row_c <- dtm[child_idx, ]
    current_intersection <- term_list[(row_f > 0) & (row_c > 0)]
    node_intersections[[child_idx]] <- current_intersection
    
    prev_intersection <- node_intersections[[father_idx]]
    
    # Innovation Logic
    if (is.null(prev_intersection)) {
      innovation <- current_intersection
    } else {
      innovation <- setdiff(current_intersection, prev_intersection)
    }
    
    candidates <- if(length(innovation) > 0) innovation else current_intersection
    
    if(length(candidates) > 0) {
      # RETRIEVE ENTROPY
      candidate_H <- term_entropy[candidates]
      
      # SORT ASCENDING (Lowest Entropy = Most Specific = First)
      sorted_candidates <- names(sort(candidate_H, decreasing = FALSE))
      
      # Pick top 5
      top_terms <- head(sorted_candidates, 5)
      edge_labels[i] <- paste(top_terms, collapse = ", ")
    } else {
      edge_labels[i] <- ""
    }
    
    if(i %% 50 == 0) setTxtProgressBar(pb, i)
  }
  close(pb)
  
  # --- 4. RENDER ---
  message("\n>> Step 4: Rendering to PDF...")
  
  all_names <- V(g_undirected)$name
  source_names <- all_names[father_indices]
  target_names <- all_names[child_indices]
  
  df_edges <- data.frame(from = source_names, to = target_names, label = edge_labels)
  g_directed <- graph_from_data_frame(df_edges, vertices = data.frame(name = all_names))
  
  meta_map <- setNames(meta$headline, meta$doc_id)
  V(g_directed)$headline <- sapply(V(g_directed)$name, function(id) {
    if(id %in% names(meta_map)) {
      h <- gsub("\\[1 Edition\\]", "", as.character(meta_map[id]))
      return(substr(h, 1, 60)) 
    } else return(id)
  })
  
  V(g_directed)$type <- ifelse(V(g_directed)$name == names(root_index), "ROOT", "Leaf")
  
  p <- ggraph(g_directed, layout = 'dendrogram', circular = FALSE) + 
    geom_edge_elbow(aes(label = label), 
                    color = "grey70", 
                    width = 0.5,
                    angle_calc = 'none',   
                    label_dodge = unit(2, 'mm'),
                    angle = 90,             
                    label_size = 2,           
                    label_colour = "blue") + 
    geom_node_point(aes(color = type), size = 0.5) +
    geom_node_text(aes(label = headline), 
                   angle = 90, hjust = 1, nudge_y = -0.1, size = 2) +
    scale_color_manual(values = c("ROOT" = "red", "Leaf" = "black")) +
    theme_void() +
    theme(legend.position = "none") +
    expand_limits(y = -5)
  
  ggsave(filename, plot = p, width = 200, height = 40, limitsize = FALSE)
  message(">> Done.")
}

#' Diagnostic Test for 1-NN Graph Topology
#' 
#' @param corpus The corpus object.
#' @param method The similarity method used (e.g., "jaccard").
#' @return A report on the graph's structural classification.
test_1nn_topology <- function(corpus, method = "jaccard") {
  
  message(">> Constructing 1-NN Graph for Diagnostic...")
  
  # 1. Use your existing sweep logic for k=1
  # n_rand=0 because we only need the observed structure for this test
  results_1nn <- run_knn_sweep(corpus, method = method, k_levels = 1, n_rand = 0)
  
  # 2. Re-generate the specific graph object to inspect components
  # This mirrors the logic inside your run_knn_sweep
  sim_matrix <- get_knn_similarity(corpus, method)
  total_docs <- nrow(sim_matrix)
  adj <- matrix(0, nrow=total_docs, ncol=total_docs)
  
  for (i in 1:total_docs) {
    top_idx <- order(sim_matrix[i, ], decreasing = TRUE)[1]
    adj[i, top_idx] <- sim_matrix[i, top_idx]
  }
  
  g <- graph_from_adjacency_matrix(adj, mode="max", weighted=TRUE, diag=FALSE)
  g <- as.undirected(g, mode = "collapse")
  
  # 3. Structural Metrics
  n_nodes <- vcount(g)
  n_edges <- ecount(g)
  comps <- components(g)
  n_components <- comps$no
  giant_size <- max(comps$csize)
  gcr <- giant_size / n_nodes
  
  # 4. Classification Logic
  structure_type <- "Unknown"
  if (n_components == 1) {
    # In a connected 1-NN undirected graph, V usually equals E 
    # because of the one mandatory cycle (reciprocal pair).
    structure_type <- "Single Pseudo-Tree (Connected Unicyclic Graph)"
  } else if (n_components > 1 && gcr > 0.5) {
    structure_type <- "Fragmented Forest with a Dominant Giant Component"
  } else {
    structure_type <- "Highly Fragmented Pseudo-Forest"
  }
  
  # 5. Output Report
  cat("\n=== 1-NN TOPOLOGY DIAGNOSTIC ===\n")
  cat(sprintf("Total Documents:    %d\n", n_nodes))
  cat(sprintf("Total Edges:        %d\n", n_edges))
  cat(sprintf("Total Components:   %d\n", n_components))
  cat(sprintf("Giant Component:    %d (%.2f%% of corpus)\n", giant_size, gcr * 100))
  cat(sprintf("Classification:     %s\n", structure_type))
  cat("================================\n")
  
  if (n_components == 1) {
    cat("RESULT: The corpus is a single, completely connected tree-like structure.\n")
    cat("This confirms a global 'Universal Donor' or a continuous semantic gradient.\n")
  } else {
    cat(sprintf("RESULT: The corpus consists of %d disconnected 'islands'.\n", n_components))
  }
  
  return(list(graph = g, components = comps, type = structure_type))
}

get_residual_2nn_links <- function(corpus, g_1nn) {
  # Generate DTM once
  dtm <- get_document_matrix(corpus, method = "jaccard")
  
  # Get the edge list using actual character names immediately
  # This prevents the index-to-name lookup error 
  all_edges <- as_edgelist(g_1nn, names = TRUE)
  
  message(">> Step 1: Identifying 1-NN Intersection Terms...")
  
  used_terms_mask <- logical(ncol(dtm))
  dtm_rownames <- rownames(dtm)
  
  for(i in 1:nrow(all_edges)) {
    name_a <- all_edges[i, 1]
    name_b <- all_edges[i, 2]
    
    # Check if both documents exist in the DTM 
    if (name_a %in% dtm_rownames && name_b %in% dtm_rownames) {
      # Use drop = FALSE to keep it as a matrix/vector 
      row_a <- dtm[name_a, , drop = FALSE]
      row_b <- dtm[name_b, , drop = FALSE]
      
      # Determine shared terms (intersection)
      # We cast to logical to ensure the mask works correctly 
      shared_mask <- as.vector(row_a > 0 & row_b > 0)
      used_terms_mask <- used_terms_mask | shared_mask
    }
  }
  
  # REDACT: Create the residual lexicon
  used_terms_idx <- which(used_terms_mask)
  dtm_residual <- dtm[, !used_terms_mask, drop = FALSE]
  
  # --- CRITICAL FIX: Ensure no documents are empty after redaction ---
  # Documents with 0 terms will cause Jaccard to return NaN 
  valid_docs <- Matrix::rowSums(dtm_residual) > 0
  dtm_residual <- dtm_residual[valid_docs, , drop = FALSE]
  
  message(sprintf(">> Residual Lexicon: %d terms. Valid Docs: %d", 
                  ncol(dtm_residual), nrow(dtm_residual)))
  
  # Step 2: Compute Similarity
  sim_residual <- compute_similarity(dtm_residual, method = "jaccard")
  diag(sim_residual) <- 0
  
  # Step 3: Extract Links
  res_list <- list()
  for(i in 1:nrow(sim_residual)) {
    row_sims <- sim_residual[i, ]
    
    # Handle any remaining NaNs/NAs from the similarity calculation 
    row_sims[is.na(row_sims) | is.nan(row_sims)] <- 0 
    
    if(any(row_sims > 0)) {
      best_idx <- which.max(row_sims)
      res_list[[i]] <- data.frame(
        from = rownames(sim_residual)[i],
        to = colnames(sim_residual)[best_idx],
        weight = row_sims[best_idx],
        stringsAsFactors = FALSE
      )
    }
  }
  
  return(do.call(rbind, res_list))
}

analyze_ablated_topology <- function(corpus, residual_edges, threshold = 0.05) {
  
  message(">> Step 1: Ablating Vocabulary (Subtracting 1-NN Terms)...")
  
  # 1. Setup Data Structures
  dtm <- get_document_matrix(corpus, method = "jaccard")
  sim_matrix <- compute_similarity(dtm, method = "jaccard")
  diag(sim_matrix) <- 0
  
  # 2. Identify "Spent" Terms for every document
  # (Terms responsible for the 1-NN link)
  spent_masks <- list()
  doc_indices <- 1:nrow(dtm)
  names(doc_indices) <- rownames(dtm)
  
  for(i in 1:nrow(dtm)) {
    if(max(sim_matrix[i,]) > 0) {
      best_idx <- which.max(sim_matrix[i,])
      # The mask is the intersection of Doc and its 1-NN Parent/Sibling
      spent_masks[[i]] <- as.logical(dtm[i, ] & dtm[best_idx, ])
    } else {
      spent_masks[[i]] <- logical(ncol(dtm)) # No 1-NN, no spent terms
    }
  }
  
  # 3. Recalculate Weights for Residual Edges based on Ablated Vocab
  message(">> Step 2: Recalculating Edge Weights...")
  
  ablated_edges <- residual_edges
  new_weights <- numeric(nrow(residual_edges))
  
  for(k in 1:nrow(residual_edges)) {
    id_a <- residual_edges$from[k]
    id_b <- residual_edges$to[k]
    
    # Map IDs to matrix indices
    idx_a <- doc_indices[id_a]
    idx_b <- doc_indices[id_b]
    
    if(!is.na(idx_a) && !is.na(idx_b)) {
      # Get raw binary vectors
      vec_a <- as.logical(dtm[idx_a, ])
      vec_b <- as.logical(dtm[idx_b, ])
      
      # The "Available" vocabulary for this specific pair
      # We ignore words that A used for its 1-NN and words B used for its 1-NN
      mask_a <- spent_masks[[idx_a]]
      mask_b <- spent_masks[[idx_b]]
      
      # The strictly "Residual" Intersection
      # (Terms shared by A and B that NEITHER used for their primary link)
      # Note: You could argue we only exclude terms if BOTH used them, 
      # but strict ablation says: "If this word is part of A's primary definition, 
      # it cannot be the driver of a secondary 'latent' link."
      valid_intersection <- (vec_a & vec_b) & (!mask_a) & (!mask_b)
      
      # The Union (for Jaccard) is also ablated to be fair
      valid_union <- (vec_a | vec_b) & (!mask_a) & (!mask_b)
      
      if(sum(valid_union) > 0) {
        new_weights[k] <- sum(valid_intersection) / sum(valid_union)
      } else {
        new_weights[k] <- 0
      }
    }
  }
  
  ablated_edges$weight <- new_weights
  
  # Filter by new ablated threshold
  final_edges <- ablated_edges[ablated_edges$weight >= threshold, ]
  
  message(sprintf("   > Retained %d edges after ablation and filtering (Threshold: %.3f)", 
                  nrow(final_edges), threshold))
  
  if(nrow(final_edges) == 0) {
    warning("No edges passed the ablation threshold.")
    return(NULL)
  }
  
  # 4. Build Graph
  g_res <- graph_from_data_frame(final_edges, directed = FALSE)
  
  message(">> Step 3: Analyzing Topology & Small World Metrics...")
  
  # Focus on Giant Component for Path Length calculations
  comps <- components(g_res)
  g_giant <- induced_subgraph(g_res, which(comps$membership == which.max(comps$csize)))
  
  # METRICS
  # C_real: Transitivity (Global Clustering Coefficient)
  C_real <- transitivity(g_giant, type = "global")
  if(is.na(C_real)) C_real <- 0
  
  # L_real: Average Path Length
  L_real <- mean_distance(g_giant, directed = FALSE)
  
  # RANDOM REFERENCE (Erdos-Renyi)
  # Generate a random graph with same Nodes and Edges
  g_rand <- sample_gnm(n = vcount(g_giant), m = ecount(g_giant))
  
  C_rand <- transitivity(g_rand, type = "global")
  if(is.na(C_rand)) C_rand <- 0
  L_rand <- mean_distance(g_rand, directed = FALSE)
  
  # SIGMA (Small World Coefficient)
  # Sigma = (C / C_rand) / (L / L_rand)
  # > 1 implies Small World
  sigma <- (C_real / C_rand) / (L_real / L_rand)
  
  message(sprintf(">> TOPOLOGY REPORT (Giant Component):"))
  message(sprintf("   - Nodes: %d | Edges: %d", vcount(g_giant), ecount(g_giant)))
  message(sprintf("   - Transitivity (Clustering): %.4f (Random: %.4f)", C_real, C_rand))
  message(sprintf("   - Avg Path Length: %.4f (Random: %.4f)", L_real, L_rand))
  message(sprintf("   - SIGMA (Small World): %.4f", sigma))
  
  if(sigma > 1) {
    message("   >> RESULT: Network shows Small-World properties (Sigma > 1)")
  } else {
    message("   >> RESULT: Network is closer to Random or Regular Lattice (Sigma <= 1)")
  }
  
  # Assortativity
  assort <- assortativity_degree(g_giant)
  message(sprintf("   - Degree Assortativity: %.3f", assort))
  
  return(g_giant)
}

get_true_residual_terms <- function(corpus, residual_edges, top_n_links = 20) {
  
  message(">> Step 1: Re-identifying the 'Spent' 1-NN Lexicon...")
  
  # 1. Re-calculate the mask of words used in the 1-NN Tree
  # (We have to do this to know what to hide)
  dtm <- get_document_matrix(corpus, method = "jaccard")
  
  # Re-build the 1-NN adjacency to find the "used" terms
  sim_matrix <- compute_similarity(dtm, method = "jaccard")
  diag(sim_matrix) <- 0
  
  used_terms_mask <- logical(ncol(dtm))
  
  # Iterate to mark used terms (This mimics your original logic)
  # We do this quickly for the whole corpus
  for(i in 1:nrow(sim_matrix)) {
    # Find the 1-NN neighbor for doc i
    if(max(sim_matrix[i,]) > 0) {
      best_idx <- which.max(sim_matrix[i,])
      # Mark the intersection as "Spent"
      shared_mask <- as.logical(dtm[i, ] & dtm[best_idx, ])
      used_terms_mask <- used_terms_mask | shared_mask
    }
  }
  
  message(sprintf("   > Masked %d 'Primary Topic' terms (e.g., 'the', 'said').", sum(used_terms_mask)))
  
  message(">> Step 2: Extracting Pure Residual Bridges...")
  
  # 2. Get the top residual edges
  top_edges <- head(residual_edges[order(residual_edges$weight, decreasing = TRUE), ], top_n_links)
  
  results <- list()
  
  for(i in 1:nrow(top_edges)) {
    doc_a <- top_edges$from[i]
    doc_b <- top_edges$to[i]
    
    # Get raw rows
    row_a <- dtm[doc_a, , drop = FALSE]
    row_b <- dtm[doc_b, , drop = FALSE]
    
    # Find intersection
    raw_intersection <- as.logical(row_a > 0 & row_b > 0)
    
    # CRITICAL STEP: Subtract the 'Used' mask
    # We only want words that are SHARED but NOT USED in the 1-NN global set
    true_residual_mask <- raw_intersection & (!used_terms_mask)
    
    residual_words <- colnames(dtm)[true_residual_mask]
    
    # Create a clean string
    term_string <- paste(head(residual_words, 10), collapse = ", ")
    if(length(residual_words) == 0) term_string <- "(No residual terms found? Check logic)"
    
    results[[i]] <- data.frame(
      Rank = i,
      Weight = round(top_edges$weight[i], 3),
      Residual_Terms = term_string,
      Count = length(residual_words)
    )
  }
  
  return(do.call(rbind, results))
}

get_lexicon_gradient <- function(corpus, g_1nn, root_id, weighted_hops = FALSE) {
  # 1. Calculate distances from Root to all documents (BFS)
  if (weighted_hops == TRUE) { dists <- igraph::distances(g_1nn, v = root_id, to = igraph::V(g_1nn)) } else { dists <- igraph::distances(g_1nn, v = root_id, to = igraph::V(g_1nn), weights = NA) }
  doc_ranks <- setNames(as.vector(dists), igraph::V(g_1nn)$name)
  
  # 2. Map Terms to their "Birth Rank" (Minimum distance to a doc containing the term)
  dtm <- get_document_matrix(corpus, method = "jaccard") # Sparse Binary Matrix
  
  # Find document indices for each term
  # For each term (column), find the minimum doc_rank among documents where it appears
  term_birth_ranks <- apply(dtm, 2, function(col) {
    active_docs <- names(which(col > 0))
    if(length(active_docs) == 0) return(NA)
    min(doc_ranks[active_docs])
  })
  
  # 3. Combine with Frequency for tie-breaking
  term_stats <- data.frame(
    Term = names(term_birth_ranks),
    Birth_Rank = as.numeric(term_birth_ranks),
    Frequency = as.numeric(Matrix::colSums(dtm))
  ) %>%
    arrange(Birth_Rank, desc(Frequency))
  
  return(term_stats)
}
