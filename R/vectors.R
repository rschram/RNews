library(text2vec)
library(tidyverse)
library(Matrix)
library(irlba)

# ==============================================================================
# PART 1: VECTOR GENERATION (PPMI-SVD)
# ==============================================================================

#' Generate Corpus Vectors (PPMI-SVD)
#' 
#' The original code to create word embeddings from raw text.
#' @param text_df Tibble with 'doc_id' and 'text'.
generate_corpus_vectors <- function(text_df, min_count = 5, window_size = 10, rank = 100) {
  
  message(">> Generating PPMI-SVD Vectors...")
  
  # Pre-processing
  clean_text <- function(x) {
    x <- str_to_lower(x)
    x <- str_replace_all(x, "<.*?>", " ") 
    x <- str_replace_all(x, "[0-9]+", " ") 
    return(x)
  }
  
  # Iterator & Vocab
  it <- itoken(clean_text(text_df$text), ids = text_df$doc_id, tokenizer = word_tokenizer, progressbar = FALSE)
  vocab <- create_vocabulary(it) %>% prune_vocabulary(term_count_min = min_count)
  
  # TCM & PPMI
  vectorizer <- vocab_vectorizer(vocab)
  tcm <- create_tcm(it, vectorizer, skip_grams_window = window_size)
  
  sum_tcm <- sum(tcm)
  row_sums <- rowSums(tcm)
  col_sums <- colSums(tcm)
  
  ppmi <- tcm
  ppmi@x <- log(ppmi@x * sum_tcm / (row_sums[ppmi@i + 1] * col_sums[ppmi@j + 1]))
  ppmi@x[ppmi@x < 0] <- 0 
  
  # SVD
  svd_res <- irlba(ppmi, nv = rank, nu = rank)
  word_vectors <- svd_res$u
  rownames(word_vectors) <- vocab$term
  
  return(list(vectors = word_vectors, stats = vocab))
}

# ==============================================================================
# PART 2: DOCUMENT VECTOR AGGREGATORS
# ==============================================================================

#' Get Weighted Document Vectors (Method B)
#' TF-IDF Weighted Average of Word Vectors
get_weighted_document_vectors <- function(tokens_df, term_vectors) {
  
  # 1. Calculate TF-IDF Weights locally
  tfidf <- tokens_df %>%
    count(doc_id, term, name = "n") %>%
    bind_tf_idf(term, doc_id, n) %>%
    select(doc_id, term, weight = tf_idf)
  
  # 2. Join with Vectors
  vec_df <- as.data.frame(term_vectors) %>% rownames_to_column("term")
  joined <- tfidf %>% inner_join(vec_df, by = "term")
  
  # 3. Compute Weighted Mean
  doc_vectors <- joined %>%
    group_by(doc_id) %>%
    summarise(
      total_weight = sum(weight),
      across(starts_with("V"), ~ sum(.x * weight) / total_weight)
    ) %>%
    select(-total_weight) %>%
    column_to_rownames("doc_id") %>%
    as.matrix()
  
  return(doc_vectors)
}

#' Get Mean Document Vectors (Method A)
#' Simple Average of Word Vectors (No Weights)
get_mean_document_vectors <- function(tokens_df, term_vectors) {
  
  # 1. Join Tokens with Vectors
  vec_df <- as.data.frame(term_vectors) %>% rownames_to_column("term")
  joined <- tokens_df %>% inner_join(vec_df, by = "term")
  
  # 2. Compute Simple Mean
  doc_vectors <- joined %>%
    group_by(doc_id) %>%
    summarise(across(starts_with("V"), mean)) %>%
    column_to_rownames("doc_id") %>%
    as.matrix()
  
  return(doc_vectors)
}

#' Bootstrap Embedding Stability
#' Checks if nearest neighbors remain consistent across corpus resamples.
#' 
#' @param text_df The content dataframe (doc_id, text)
#' @param original_vectors The matrix from the full run
#' @param n_iter Number of bootstraps (e.g., 10)
#' @param test_terms Vector of terms to check (or NULL for top 50 freq)
#' @return A dataframe of stability scores per term
test_embedding_stability <- function(text_df, original_vectors, vocab_stats, n_iter = 10, test_terms = NULL) {
  
  message(sprintf(">> Testing Embedding Stability (%d iters)...", n_iter))
  
  # 1. Define Test Set
  if(is.null(test_terms)) {
    # Default: Top 50 most frequent terms
    test_terms <- vocab_stats %>% 
      arrange(desc(term_count)) %>% 
      head(50) %>% 
      pull(term)
  }
  
  # Filter to terms actually in vectors
  test_terms <- intersect(test_terms, rownames(original_vectors))
  if(length(test_terms) == 0) stop("No test terms found in vector model.")
  
  # 2. Helper: Get Top 10 Neighbors
  get_nn <- function(target, vec_matrix, k=10) {
    # Cosine Sim
    sims <- text2vec::sim2(vec_matrix, vec_matrix[target, , drop=FALSE], method="cosine")
    names(sort(sims[,1], decreasing = TRUE))[2:(k+1)] # Skip self
  }
  
  # 3. Calculate Original Neighbors
  base_nn <- lapply(test_terms, function(t) get_nn(t, original_vectors))
  names(base_nn) <- test_terms
  
  stability_scores <- list()
  
  # 4. Bootstrap Loop
  for(i in 1:n_iter) {
    # Resample Content
    boot_indices <- sample(nrow(text_df), nrow(text_df), replace = TRUE)
    boot_df <- text_df[boot_indices, ]
    boot_df$doc_id <- paste0(boot_df$doc_id, "_", 1:nrow(boot_df)) # Uniquify IDs
    
    # Train New Vectors (Silence the logs)
    # Note: We must use the same params (Window, Rank) as the main run
    suppressMessages({
      boot_res <- generate_corpus_vectors(boot_df, min_count = 5, window_size = 10, rank = ncol(original_vectors))
    })
    boot_vecs <- boot_res$vectors
    
    # Compare Neighbors
    iter_scores <- numeric()
    for(term in test_terms) {
      if(term %in% rownames(boot_vecs)) {
        # Overlap
        set_a <- base_nn[[term]]
        set_b <- get_nn(term, boot_vecs)
        overlap <- length(intersect(set_a, set_b)) / length(union(set_a, set_b))
        iter_scores[term] <- overlap
      } else {
        iter_scores[term] <- 0 # Term dropped out of vocab (Unstable)
      }
    }
    stability_scores[[i]] <- iter_scores
  }
  
  # 5. Aggregate
  results <- do.call(rbind, stability_scores)
  summary_df <- data.frame(
    term = colnames(results),
    stability_score = colMeans(results, na.rm=TRUE)
  ) %>% arrange(stability_score)
  
  return(summary_df)
}