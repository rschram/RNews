library(igraph)
library(tidyverse)
library(text2vec)

#' Ingest ProQuest Exports into SQLite (With Boilerplate Removal)
#'
#' Parses raw text files exported from ProQuest, cleans them of specific patterns,
#' and loads them into a SQLite database. Handles deduplication via content hashing.
#'
#' @param input_path String. Path to a single .txt file or a directory containing .txt files.
#' @param db_connection DBIConnection. An active connection to the SQLite database.
#' @param boilerplate_patterns Character Vector. Regex patterns to strip from text BEFORE processing.
#' @return Invisible TRUE on success.
#' @export
#' @importFrom fs dir_ls is_file
#' @importFrom readr read_file
#' @importFrom stringr str_split str_trim str_length str_detect str_replace str_subset str_remove_all
#' @importFrom lubridate mdy
#' @importFrom digest digest
#' @importFrom purrr map_dfr walk
#' @importFrom dplyr bind_rows distinct anti_join select
#' @importFrom DBI dbExecute dbWriteTable dbGetQuery
ingest_proquest <- function(input_path, db_connection, boilerplate_patterns = NULL) {
  
  # 1. Identify Files
  if (fs::is_file(input_path)) {
    files <- input_path
  } else {
    files <- fs::dir_ls(input_path, glob = "*.txt")
  }
  
  if (length(files) == 0) stop("No .txt files found in input path.")
  
  message(paste(">> Found", length(files), "file(s). Starting ingestion..."))
  if(!is.null(boilerplate_patterns)) {
    message(paste(">> cleaning", length(boilerplate_patterns), "boilerplate patterns on the fly."))
  }
  
  # 2. Ensure Database Schema Exists
  DBI::dbExecute(db_connection, "
    CREATE TABLE IF NOT EXISTS doc_index (
      doc_id TEXT PRIMARY KEY,
      headline TEXT,
      date TEXT,
      author TEXT,
      original_file TEXT,
      ingest_date TIMESTAMP
    )
  ")
  
  DBI::dbExecute(db_connection, "
    CREATE TABLE IF NOT EXISTS doc_content (
      doc_id TEXT PRIMARY KEY,
      full_text TEXT,
      FOREIGN KEY(doc_id) REFERENCES doc_index(doc_id)
    )
  ")
  
  # 3. Process Each File
  purrr::walk(files, function(f) {
    message(paste("   Processing:", basename(f)))
    
    # Read the whole file
    raw_content <- readr::read_file(f)
    
    # ProQuest separates articles with 5+ underscores
    raw_articles <- stringr::str_split(raw_content, "_{5,}")[[1]]
    # Remove empty splits
    raw_articles <- raw_articles[stringr::str_length(stringr::str_trim(raw_articles)) > 0]
    
    if(length(raw_articles) == 0) {
      warning(paste("No articles found in", basename(f)))
      return(NULL)
    }
    
    # Parse the batch (Passing the boilerplate patterns down)
    parsed_df <- purrr::map_dfr(raw_articles, ~parse_single_article(.x, basename(f), boilerplate_patterns))
    
    if(nrow(parsed_df) == 0) return(NULL)
    
    # 4. Deduplication against DB
    existing_ids <- DBI::dbGetQuery(db_connection, "SELECT doc_id FROM doc_index")
    
    new_records <- parsed_df %>%
      dplyr::anti_join(existing_ids, by = "doc_id") %>%
      dplyr::distinct(doc_id, .keep_all = TRUE)
    
    if(nrow(new_records) > 0) {
      DBI::dbWriteTable(db_connection, "doc_index",
                        new_records %>% dplyr::select(doc_id, headline, date, author, original_file, ingest_date),
                        append = TRUE)
      
      DBI::dbWriteTable(db_connection, "doc_content",
                        new_records %>% dplyr::select(doc_id, full_text),
                        append = TRUE)
      
      message(paste("   + Imported", nrow(new_records), "new articles."))
    } else {
      message("   . No new articles (all duplicates).")
    }
  })
  
  message(">> Ingestion complete.")
  return(invisible(TRUE))
}

#' Internal: Parse a Single ProQuest Article String
#' @noRd
parse_single_article <- function(article_text, filename, boilerplate_patterns = NULL) {
  
  # --- A. CLEANING (The New Step) ---
  # Remove patterns BEFORE splitting into lines. 
  # This prevents boilerplate from confusing the metadata parsers.
  if (!is.null(boilerplate_patterns)) {
    for (pat in boilerplate_patterns) {
      article_text <- stringr::str_remove_all(article_text, pat)
    }
  }
  
  lines <- stringr::str_split(article_text, "\n")[[1]] %>% stringr::str_trim()
  lines <- lines[lines != ""]
  
  if(length(lines) == 0) return(NULL)
  
  # --- B. METADATA EXTRACTION ---
  headline <- lines[1]
  
  extract_field <- function(pattern) {
    match <- stringr::str_subset(lines, pattern)
    if (length(match) > 0) return(stringr::str_replace(match[1], pattern, "\\1"))
    return(NA_character_)
  }
  
  raw_author <- extract_field("^Author:\\s+(.*)")
  raw_date   <- extract_field("^Publication date:\\s+(.*)")
  
  clean_date <- tryCatch(
    as.character(lubridate::mdy(raw_date)),
    warning = function(w) return(raw_date),
    error = function(e) return(raw_date)
  )
  if(is.na(clean_date)) clean_date <- raw_date
  
  # --- C. CONTENT EXTRACTION ---
  start_idx <- which(stringr::str_detect(lines, "^Full text:"))
  
  full_text <- ""
  if (length(start_idx) > 0) {
    end_markers <- c("^Credit:", "^Title:", "^Publication title:", "^Subject:", "^Copyright:", "^Publication info:")
    marker_indices <- unlist(purrr::map(end_markers, ~grep(.x, lines)))
    valid_end_indices <- marker_indices[marker_indices > start_idx[1]]
    
    end_idx <- if (length(valid_end_indices) > 0) min(valid_end_indices) - 1 else length(lines)
    
    body_lines <- lines[(start_idx[1] + 1):end_idx]
    body_lines <- body_lines[body_lines != "Full text:"]
    
    full_text <- paste(c(headline, body_lines), collapse = "\n")
  } else {
    full_text <- paste(lines, collapse = "\n")
  }
  
  # --- D. HASHING (Now based on CLEAN text) ---
  doc_id <- digest::digest(full_text, algo = "md5")
  
  tibble::tibble(
    doc_id = doc_id,
    headline = headline,
    date = clean_date,
    author = raw_author,
    full_text = full_text,
    original_file = filename,
    ingest_date = as.character(Sys.time())
  )
}

#' Manage Corpus Storage and Indexing
#'
#' These functions handle the transformation of raw text into a statistical index
#' (term counts) inside the SQLite database.
#'
#' @name manage_storage
NULL

#' Update Token Index
#'
#' Scans the database for documents that have been ingested but not yet tokenized.
#' It tokenizes them and updates the `token_index` table.
#'
#' @param db_connection DBIConnection. Active connection to the SQLite database.
#' @param remove_stop_words Boolean. Should standard English stop words be removed? Default TRUE.
#' @return Invisible TRUE on success.
#' @export
#' @importFrom dplyr tbl collect filter anti_join select count mutate bind_rows inner_join pull
#' @importFrom tidytext unnest_tokens
#' @importFrom DBI dbExecute dbWriteTable dbGetQuery
#' @importFrom tidyr tibble
#' @importFrom stringr str_replace_all
update_token_index <- function(db_connection, remove_stop_words = TRUE) {
  
  # 1. Ensure Schema Exists
  DBI::dbExecute(db_connection, "
    CREATE TABLE IF NOT EXISTS token_index (
      doc_id TEXT,
      term TEXT,
      n INTEGER,
      PRIMARY KEY (doc_id, term),
      FOREIGN KEY(doc_id) REFERENCES doc_index(doc_id)
    )
  ")
  
  # 2. Identify Untokenized Documents
  # We compare the content table (source) vs the index table (destination)
  message(">> Checking for new documents to index...")
  
  # Get all IDs that HAVE content
  all_ids <- DBI::dbGetQuery(db_connection, "SELECT doc_id FROM doc_content")
  
  # Get all IDs that ARE indexed
  # (We use distinct to be safe, though PK handles uniqueness)
  indexed_ids <- DBI::dbGetQuery(db_connection, "SELECT DISTINCT doc_id FROM token_index")
  
  # Find the difference
  ids_to_process <- setdiff(all_ids$doc_id, indexed_ids$doc_id)
  
  if(length(ids_to_process) == 0) {
    message("   . Index is up to date.")
    return(invisible(TRUE))
  }
  
  message(paste("   + Found", length(ids_to_process), "new documents. Processing..."))
  
  # 3. Process in Batches (to avoid memory overflow on massive corpora)
  batch_size <- 500
  batches <- split(ids_to_process, ceiling(seq_along(ids_to_process)/batch_size))
  
  for(i in seq_along(batches)) {
    batch_ids <- batches[[i]]
    
    # Fetch raw text for this batch
    # SQLite doesn't have a clean 'WHERE IN' for vectors in DBI, so we construct the query or pull logic
    # Safe approach: Pull all relevant rows. For massive DBs, we'd construct a SQL string.
    # Here we construct a SQL string safely since IDs are hashes (alphanumeric)
    id_str <- paste0("'", batch_ids, "'", collapse = ",")
    query <- paste0("SELECT doc_id, full_text FROM doc_content WHERE doc_id IN (", id_str, ")")
    
    raw_data <- DBI::dbGetQuery(db_connection, query) %>%
      dplyr::as_tibble()
    
    # Tokenize
    tokens <- raw_data %>%
      # Basic cleanup: lowercase, remove punctuation handles by unnest_tokens,
      # but we might want to remove numbers or strict punctuation first if needed.
      tidytext::unnest_tokens(term, full_text, token = "words")
    
    # Remove Stop Words (Optional)
    if(remove_stop_words) {
      tokens <- tokens %>%
        dplyr::filter(!term %in% tidytext::stop_words$word)
    }
    
    # Count Terms (The "Bag of Words")
    term_counts <- tokens %>%
      dplyr::count(doc_id, term, name = "n")
    
    # Write to DB
    DBI::dbWriteTable(db_connection, "token_index", term_counts, append = TRUE)
    
    message(paste("     Batch", i, "/", length(batches), "indexed."))
  }
  
  message(">> Indexing complete.")
  return(invisible(TRUE))
}

#' Get Corpus Statistics
#'
#' Returns a summary of the current state of the corpus (Total Docs, Total Terms, etc.)
#'
#' @param db_connection DBIConnection.
#' @return A list of statistics.
#' @export
get_corpus_stats <- function(db_connection) {
  
  n_docs <- DBI::dbGetQuery(db_connection, "SELECT count(*) as n FROM doc_index")$n
  n_tokens <- DBI::dbGetQuery(db_connection, "SELECT sum(n) as n FROM token_index")$n
  n_terms <- DBI::dbGetQuery(db_connection, "SELECT count(distinct term) as n FROM token_index")$n
  
  list(
    documents = n_docs,
    total_tokens = n_tokens,
    unique_terms = n_terms
  )
}

library(tidyverse)
library(tidytext)
library(Matrix)
library(igraph)

# ==============================================================================
# PHASE 1: The Chassis (Input Standardization & Rank Analytics)
# ==============================================================================

#' Create Standardized Corpus Object
#'
#' Decouples cleaning from analysis. Expects clean inputs.
#' @param tokens_df Tibble with columns: doc_id, term, n (count)
#' @param meta_df Tibble with columns: doc_id, headline, [other metadata]
create_corpus_object <- function(tokens_df, meta_df) {
  
  # Basic Validation
  req_cols <- c("doc_id", "term", "n")
  if(!all(req_cols %in% names(tokens_df))) stop("Tokens DF missing required columns")
  
  # 1. Global Stats
  n_docs <- n_distinct(tokens_df$doc_id)
  n_terms <- n_distinct(tokens_df$term)
  n_tokens <- sum(tokens_df$n)
  
  # 2. Term Stats (Pre-calculate IDF once for efficiency)
  term_stats <- tokens_df %>%
    group_by(term) %>%
    summarise(
      tf_global = sum(n),
      df = n_distinct(doc_id),
      .groups = "drop"
    ) %>%
    mutate(
      idf = log(n_docs / (df + 1)), # Standard IDF
      idf_bm25 = log((n_docs - df + 0.5) / (df + 0.5) + 1) # Probabilistic IDF
    )
  
  list(
    tokens = tokens_df,
    meta = meta_df,
    stats = list(
      n_docs = n_docs,
      n_terms = n_terms,
      n_tokens = n_tokens,
      avg_doc_len = n_tokens / n_docs
    ),
    term_stats = term_stats
  )
}

#' Calculate Rank-Order Curves
#' 
#' Generates the data for the Log-Log plots of term distributions.
#' @param corpus The object from create_corpus_object()
#' @param k BM25 parameter (default 1.2)
#' @param b BM25 parameter (default 0.75)
analyze_rank_curves <- function(corpus, k = 1.2, b = 0.75) {
  
  message(">> Calculating Rank-Order Distributions...")
  
  # 1. Raw TF Rank
  curve_tf <- corpus$term_stats %>%
    arrange(desc(tf_global)) %>%
    mutate(rank = row_number(), score = tf_global, metric = "Raw Frequency (Zipf)")
  
  # 2. TF * IDF Rank
  # (Global Sum of TF) * IDF
  curve_tfidf <- corpus$term_stats %>%
    mutate(score = tf_global * idf) %>%
    arrange(desc(score)) %>%
    mutate(rank = row_number(), metric = "Global TF-IDF")
  
  # 3. Global BM25 Approximation
  # We approximate global importance by assuming an 'Average Document' context
  # Score = IDF * (TF * (k+1)) / (TF + k)
  # Here TF is the average TF per document where it appears
  curve_bm25 <- corpus$term_stats %>%
    mutate(
      avg_tf = tf_global / df,
      # BM25 weight for an "average" appearance
      score = idf_bm25 * ((avg_tf * (k + 1)) / (avg_tf + k)) * df 
    ) %>%
    arrange(desc(score)) %>%
    mutate(rank = row_number(), metric = "Global BM25 Mass")
  
  # Combine
  bind_rows(
    curve_tf %>% select(rank, score, metric, term),
    curve_tfidf %>% select(rank, score, metric, term),
    curve_bm25 %>% select(rank, score, metric, term)
  )
}

#' Plot the Rank Curves
plot_rank_curves <- function(curve_data) {
  ggplot(curve_data, aes(x = rank, y = score, color = metric)) +
    geom_line(size = 1) +
    scale_x_log10(labels = scales::comma) +
    scale_y_log10(labels = scales::comma) +
    facet_wrap(~metric, scales = "free_y", ncol = 1) +
    theme_minimal() +
    labs(
      title = "LexicalRank: Comparing Term Importance Models",
      subtitle = "Log-Log Scale to identify Power Laws vs. Information Content",
      x = "Log Rank",
      y = "Log Score"
    )
}


# ==============================================================================
# 1. MODEL SUITE WRAPPER (Step 1)
# ==============================================================================
run_model_suite <- function(corpus, vectors = NULL, threshold = 0.3, boot_iter = 50) {
  
  message(sprintf("\n>> RUNNING MODEL SUITE (Threshold: %.2f, Boot: %d)...", threshold, boot_iter))
  
  suite_results <- list()
  
  # --- Define Models to Test ---
  models <- list(
    "TF-IDF"            = list(method = "tfidf", vecs = NULL),
    "BM25"              = list(method = "bm25",  vecs = NULL),
    "Jaccard"           = list(method = "jaccard", vecs = NULL), 
    "Embeddings (Wgt)"  = list(method = "embeddings_weighted", vecs = vectors)
    # Add others here if needed (Jaccard, TF, etc.)
  )
  
  comparison_table <- data.frame()
  
  for (name in names(models)) {
    cfg <- models[[name]]
    
    # Skip embedding models if vectors are missing
    if (str_detect(cfg$method, "embeddings") && is.null(vectors)) {
      message(sprintf("   [SKIP] %s (No vectors provided)", name))
      next
    }
    
    # Run the existing test bench function
    # (Assumes run_test_bench is loaded from chassis.R)
    res <- run_test_bench(
      corpus, 
      method = cfg$method, 
      threshold = threshold, 
      vectors = cfg$vecs,
      boot_iter = boot_iter
    )
    
    # Store full result object
    suite_results[[name]] <- res
    
    # Compile Global Metrics
    global_row <- res$global %>% mutate(Method = name) %>% select(Method, everything())
    comparison_table <- bind_rows(comparison_table, global_row)
  }
  
  return(list(
    results = suite_results,
    comparison = comparison_table
  ))
}


# ==============================================================================
# 3. SIMILARITY SWEEP (Step 3)
# ==============================================================================
perform_similarity_sweep <- function(corpus, vectors, method="embeddings_weighted", range=seq(0.1, 0.9, 0.05)) {
  
  message(sprintf("\n>> STARTING PHASE SWEEP (%s)...", method))
  
  # Pre-calculate Sim Matrix (Optimization)
  dtm <- get_weighted_document_vectors(corpus$tokens, vectors)
  sim_matrix <- text2vec::sim2(dtm, method = "cosine", norm = "l2")
  
  results_list <- list()
  total_docs <- nrow(corpus$meta)
  
  for (t in range) {
    # progress check
    if (which(range == t) %% 5 == 0) message(sprintf("   ...processing threshold %.2f", t))
    
    # 1. Build Graph
    adj <- sim_matrix
    adj[adj < t] <- 0
    g <- graph_from_adjacency_matrix(adj, mode = "max", weighted = TRUE, diag = FALSE)
    
    # 2. Basic Metrics
    comps <- components(g)
    gcr <- max(comps$csize) / total_docs
    survivors <- sum(degree(g) > 0) / total_docs
    
    # 3. Structure (Modularity)
    leiden <- cluster_leiden(g, objective_function = "modularity", weights = E(g)$weight, resolution_parameter = 1.0)
    mod <- modularity(g, membership(leiden), weights = E(g)$weight)
    n_clus <- length(unique(membership(leiden)))
    
    # 4. Advanced Topology (Transitivity / Assortativity)
    trans <- transitivity(g, type = "global")
    assort <- assortativity_degree(g)
    
    # 5. Stress Metrics (On Giant Component Only)
    giant_nodes <- names(comps$membership[comps$membership == which.max(comps$csize)])
    
    if (length(giant_nodes) > 2) {
      g_giant <- induced_subgraph(g, giant_nodes)
      avg_path <- mean_distance(g_giant, directed = FALSE)
      
      # Betweenness (Normalized)
      betw <- betweenness(g_giant, normalized = TRUE)
      avg_betw <- mean(betw)
      max_betw <- max(betw)
    } else {
      avg_path <- 0; avg_betw <- 0; max_betw <- 0
    }
    
    results_list[[as.character(t)]] <- data.frame(
      Threshold = t,
      GCR = gcr,
      Modularity = mod,
      Survivors = survivors,
      Avg_Path = avg_path,
      Avg_Betweenness = avg_betw,
      Max_Betweenness = max_betw,
      Transitivity = trans,
      Assortativity = assort,
      N_Clusters = n_clus
    )
  }
  
  return(bind_rows(results_list))
}



