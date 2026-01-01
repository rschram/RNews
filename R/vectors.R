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