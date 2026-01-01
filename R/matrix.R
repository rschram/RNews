library(Matrix)
library(text2vec) 

# ==============================================================================
# PHASE 2: The Engines (Matrix Generation & Similarity)
# ==============================================================================

#' Generate Document-Term Matrix (Factory)
#' 
#' Creates the weighted sparse matrix for the requested method.
#' 
#' @param corpus The standard corpus object from Phase 1.
#' @param method One of "tf", "tfidf", "bm25", "binary", or "jaccard".
#' @param k BM25 parameter (default 1.2).
#' @param b BM25 parameter (default 0.75).
get_document_matrix <- function(corpus, method = "tfidf", k = 1.2, b = 1.0) {
  
  message(sprintf(">> Generating Matrix: %s...", method))
  
  # 1. Map IDs to Integers
  tokens <- corpus$tokens %>%
    mutate(
      doc_idx = as.integer(factor(doc_id, levels = unique(corpus$meta$doc_id))),
      term_idx = as.integer(factor(term, levels = unique(corpus$term_stats$term)))
    )
  
  # 2. Compute Weights
  if (method == "tf") {
    tokens$weight <- tokens$n
  } else if (method == "tfidf") {
    tokens <- tokens %>%
      inner_join(corpus$term_stats %>% select(term, idf), by = "term") %>%
      mutate(weight = n * idf)
  } else if (method == "bm25") {
    doc_lens <- corpus$tokens %>%
      group_by(doc_id) %>%
      summarise(len = sum(n), .groups = "drop")
    
    avg_dl <- mean(doc_lens$len)
    
    tokens <- tokens %>%
      inner_join(corpus$term_stats %>% select(term, idf_bm25), by = "term") %>%
      inner_join(doc_lens, by = "doc_id") %>%
      mutate(
        denom = n + (k * (1 - b + (b * (len / avg_dl)))),
        weight = idf_bm25 * ((n * (k + 1)) / denom)
      )
  } else if (method == "binary" || method == "jaccard") {
    # <--- FIXED: Accept "jaccard" here
    tokens$weight <- 1
  } else {
    stop(sprintf("Unknown method: %s", method))
  }
  
  # 3. Construct Sparse Matrix (dgCMatrix)
  dtm <- sparseMatrix(
    i = tokens$doc_idx,
    j = tokens$term_idx,
    x = tokens$weight,
    dims = c(length(unique(corpus$meta$doc_id)), length(unique(corpus$term_stats$term))),
    dimnames = list(unique(corpus$meta$doc_id), unique(corpus$term_stats$term))
  )
  
  # Ensure strictly binary if requested
  if (method %in% c("binary", "jaccard")) {
    dtm@x[dtm@x > 0] <- 1
  }
  
  return(dtm)
}

#' Calculate Similarity Matrix
#' 
#' Switches between Cosine and Jaccard based on the input.
#' @param dtm Sparse Matrix from get_document_matrix().
#' @param method "cosine" or "jaccard".
compute_similarity <- function(dtm, method = "cosine") {
  
  message(sprintf(">> Calculating %s Similarity (%d x %d)...", 
                  method, nrow(dtm), nrow(dtm)))
  
  if (method == "cosine") {
    # 1. Calculate Cosine
    sim_dsT <- text2vec::sim2(dtm, method = "cosine", norm = "l2")
    
    # 2. FORCE CONVERSION to Standard Sparse (dgCMatrix)
    sim <- as(sim_dsT, "dgCMatrix")
    
  } else if (method == "jaccard") {
    # Manual Jaccard Calculation
    
    # 1. Ensure binary structure
    dtm_bin <- dtm
    dtm_bin@x[dtm_bin@x > 0] <- 1
    dtm_bin <- drop0(dtm_bin)
    
    # 2. Intersection (A * t(B)) -> dgCMatrix
    intersection <- Matrix::tcrossprod(dtm_bin)
    
    # 3. Calculate Union components
    doc_sizes <- Matrix::diag(intersection)
    
    # 4. Convert Intersection to Triplet format
    ix_triplet <- as(intersection, "TsparseMatrix")
    
    # 5. Calculate Jaccard for non-zero pairs only
    i_idx <- ix_triplet@i + 1
    j_idx <- ix_triplet@j + 1
    
    union_vals <- doc_sizes[i_idx] + doc_sizes[j_idx] - ix_triplet@x
    jaccard_vals <- ix_triplet@x / union_vals
    
    # 6. Reconstruct Standard Sparse Matrix (dgCMatrix)
    sim <- sparseMatrix(
      i = i_idx,
      j = j_idx,
      x = jaccard_vals,
      dims = dim(intersection),
      dimnames = dimnames(intersection)
    )
    
  } else {
    stop("Method must be 'cosine' or 'jaccard'")
  }
  
  return(sim)
}

#' Get Jaccard Medoid
get_jaccard_medoid <- function(doc_ids, sim_matrix) {
  
  if(length(doc_ids) == 1) return(doc_ids)
  valid_ids <- intersect(doc_ids, rownames(sim_matrix))
  if(length(valid_ids) == 0) return(NA)
  
  sub_mat <- sim_matrix[valid_ids, valid_ids, drop=FALSE]
  avg_sims <- rowMeans(sub_mat)
  
  return(names(which.max(avg_sims)))
}