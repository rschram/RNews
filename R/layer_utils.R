library(tidyverse)
library(tidytext)
library(igraph)
library(Matrix)

#' Generate Light Model for App
#' Pre-calculates Map Layers (Clusters, Entropy, Bridges) and strips heavy objects.
#' 
#' @param model The full model object (from model_current.rds)
#' @return A simplified model list ready for the GUI
  
generate_light_model <- function(model) {
    
  message(">> Generating Map Layers...")
  
  # --- LAYER 1: CLUSTER SPECIFICITY ---
  
  # CHECK: Do we already have clusters in metadata?
  if ("cluster_id" %in% names(model$meta)) {
    message("   Using pre-calculated clusters from metadata...")
    doc_clusters <- model$meta %>% select(doc_id, cluster_id)
    
  } else {
    # Fallback: Calculate from the graph provided
    message("   Calculating clusters from graph (Fallback)...")
    if(is.null(model$graph)) stop("Model missing graph for clustering.")
    
    comm <- igraph::cluster_leiden(model$graph, objective_function = "modularity", weights = igraph::E(model$graph)$weight)
    doc_clusters <- data.frame(
      doc_id = names(igraph::membership(comm)),
      cluster_id = as.numeric(igraph::membership(comm))
    )
  }
  
  # 2. Aggregate DTM by Cluster
  # This is much faster than SQL. We sum the rows of the DTM for each cluster.
  # Create a mapping matrix (Docs x Clusters)
  clusters <- sort(unique(doc_clusters$cluster_id))
  
  # We need to ensure we only use docs that exist in the DTM
  valid_docs <- intersect(rownames(model$dtm), doc_clusters$doc_id)
  dtm_sub <- model$dtm[valid_docs, ]
  cluster_map <- doc_clusters[match(valid_docs, doc_clusters$doc_id), ]
  
  # Loop over clusters to calculate TF-IDF specific to the cluster "Meta-Document"
  # (Treating all docs in cluster as one big doc)
  cluster_layers <- list()
  
  # Global IDF (already calculated in stats, but we can re-use or re-calc)
  # We'll use the corpus-level IDF from stats for consistency
  term_idf <- setNames(model$stats$Entropy, model$stats$Term) # Proxy or recalculate? 
  # Actually, standard TF-IDF for "Key Terms" usually works best with:
  # TF = Count in Cluster / Total Words in Cluster
  # IDF = log(N_Clusters / Clusters_containing_term)
  
  message("   ...calculating cluster keywords")
  
  # 1. Aggregate Counts
  # Convert DTM to Tidy format briefly for aggregation (or use Matrix algebra)
  # Matrix algebra is faster: P = Cluster_Matrix * DTM
  # Construct Cluster Matrix (Clusters x Docs)
  C <- Matrix::sparseMatrix(
    i = as.numeric(factor(cluster_map$cluster_id)),
    j = 1:nrow(cluster_map),
    x = 1,
    dims = c(length(clusters), nrow(cluster_map))
  )
  
  # Cluster Term Counts (Clusters x Terms)
  Cluster_DTM <- C %*% dtm_sub
  rownames(Cluster_DTM) <- levels(factor(cluster_map$cluster_id))
  
  # 2. Calculate TF-IDF per cluster
  # TF
  row_sums <- Matrix::rowSums(Cluster_DTM)
  TF <- Cluster_DTM / row_sums
  
  # IDF (across clusters)
  # How many clusters does the term appear in?
  doc_freq <- Matrix::colSums(Cluster_DTM > 0)
  IDF <- log(nrow(Cluster_DTM) / (doc_freq + 1))
  
  # TF-IDF
  TFIDF <- t(t(TF) * IDF) # Broadcast multiplication
  
  # 3. Extract Top Terms per Cluster
  for(i in 1:nrow(TFIDF)) {
    cid <- rownames(TFIDF)[i]
    
    # Get scores for this cluster
    scores <- TFIDF[i, ]
    
    # Filter for non-zero and significant
    top_indices <- which(scores > 0)
    
    if(length(top_indices) > 0) {
      cluster_vec <- scores[top_indices]
      names(cluster_vec) <- colnames(TFIDF)[top_indices]
      
      # Normalize 0-1 for Opacity
      max_s <- max(cluster_vec)
      min_s <- min(cluster_vec)
      if(max_s > min_s) {
        norm_scores <- (cluster_vec - min_s) / (max_s - min_s)
      } else {
        norm_scores <- cluster_vec
      }
      
      # Keep top 500 to save RAM/Time in regex
      norm_scores <- sort(norm_scores, decreasing = TRUE)
      if(length(norm_scores) > 500) norm_scores <- norm_scores[1:500]
      
      cluster_layers[[cid]] <- norm_scores
    }
  }
  
  # --- LAYER 2: ENTROPY (BURSTINESS) ---
  message("   ...calculating entropy layer")
  entropy_layer <- model$stats %>%
    filter(Entropy_Gap < 0) %>%
    mutate(score = abs(Entropy_Gap) / max(abs(Entropy_Gap), na.rm=TRUE)) %>%
    select(Term, score)
  
  entropy_vec <- setNames(entropy_layer$score, entropy_layer$Term)
  
  # --- LAYER 3: BRIDGES ---
  message("   ...calculating bridge layer")
  # Use Term_Betweenness if available, otherwise 0
  if("Term_Betweenness" %in% names(model$stats)) {
    bridge_layer <- model$stats %>%
      filter(Term_Betweenness > 0) %>%
      mutate(score = Term_Betweenness / max(Term_Betweenness, na.rm=TRUE)) %>%
      select(Term, score)
    bridge_vec <- setNames(bridge_layer$score, bridge_layer$Term)
  } else {
    bridge_vec <- c()
  }
  
  # --- ASSEMBLE LIGHT MODEL ---
  message(">> Light Model Complete.")
  
  # SAFE MERGE: Remove existing cluster_id from meta before joining new one
  clean_meta <- model$meta %>% 
    select(-any_of(c("cluster_id", "cluster_id.x", "cluster_id.y")))
  
  list(
    meta = clean_meta %>% left_join(doc_clusters, by="doc_id"),
    full_text = model$full_text,
    dtm = model$dtm,
    layers = list(
      clusters = cluster_layers,
      entropy = entropy_vec,
      bridge = bridge_vec
    )
  )
}