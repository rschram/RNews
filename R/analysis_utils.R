library(igraph)
library(tidyverse)
library(text2vec)
library(tidytext)
library(Matrix)
library(stringi)


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




#' Analyze Clusters (The Main Reporting Engine)
analyze_clusters <- function(g, dtm, corpus, method = "tfidf", resolution = 1.0) {
  
  message(sprintf(">> Running Leiden Clustering (Resolution: %.2f)...", resolution))
  
  # 1. Run Leiden Algorithm
  comm <- cluster_leiden(
    g, 
    objective_function = "modularity", 
    weights = E(g)$weight,
    resolution = resolution
  )
  membership <- membership(comm)
  cluster_ids <- sort(unique(membership))
  
  # --- PRE-CALCULATE DISTINCTIVE TERMS ---
  message(">> identifying distinctive terms per cluster...")
  
  cluster_map <- data.frame(
    doc_id = names(membership),
    cluster_id = as.numeric(membership),
    stringsAsFactors = FALSE
  )
  
  cluster_tokens <- corpus$tokens %>%
    inner_join(cluster_map, by = "doc_id")
  
  distinctive_terms_df <- cluster_tokens %>%
    count(cluster_id, term, name = "n") %>%
    bind_tf_idf(term, cluster_id, n) %>%
    group_by(cluster_id) %>%
    arrange(desc(tf_idf)) %>%
    slice_head(n = 15) %>%  
    summarise(
      top_terms = paste(term, collapse = ", ")
    )
  
  metrics_list <- list()
  audit_list <- list()
  
  # 2. Iterate through Clusters
  for(cid in cluster_ids) {
    
    # Get Members
    member_ids <- names(membership)[membership == cid]
    n_docs <- length(member_ids)
    
    # Subgraph
    subg <- induced_subgraph(g, member_ids)
    
    # --- A. Topological Metrics ---
    dens <- edge_density(subg)
    
    local_trans <- transitivity(subg, type = "local", vids = V(subg))
    local_trans[is.na(local_trans)] <- 0 
    mean_trans <- mean(local_trans)
    sd_trans <- sd(local_trans)
    
    if (n_docs > 1) {
      paths <- distance_table(subg)$res
      if (length(paths) > 0) {
        raw_paths <- rep(seq_along(paths), times = paths)
        mean_path <- mean(raw_paths)
        sd_path <- sd(raw_paths)
      } else { mean_path <- 0; sd_path <- 0 }
    } else { mean_path <- 0; sd_path <- 0 }
    
    vol <- sum(strength(g, vids = member_ids))
    cut <- vol - (2 * sum(E(subg)$weight)) 
    cond <- if(vol > 0) cut / vol else 0
    
    # --- B. Vector Metrics ---
    sub_dtm <- dtm[member_ids, , drop=FALSE]
    
    if (method %in% c("binary", "jaccard")) {
      # Jaccard Logic
      m_bin <- as.matrix(sub_dtm)
      m_bin[m_bin > 0] <- 1
      inter <- m_bin %*% t(m_bin)
      sz <- diag(inter)
      union_mat <- outer(sz, sz, "+") - inter
      sim_mat <- inter / union_mat
      sim_mat[is.nan(sim_mat)] <- 0
      
      avg_sims <- rowMeans(sim_mat)
      medoid_idx <- which.max(avg_sims)
      centroid_ref <- names(avg_sims)[medoid_idx]
      centrality_scores <- sim_mat[, medoid_idx]
    } else {
      # Vector Logic
      centroid_vec <- colMeans(sub_dtm)
      centrality_scores <- text2vec::sim2(sub_dtm, matrix(centroid_vec, nrow=1), method = "cosine")[,1]
      centroid_ref <- "Mathematical Mean"
    }
    
    # --- C. Retrieve Top Terms ---
    term_str <- distinctive_terms_df$top_terms[distinctive_terms_df$cluster_id == cid]
    if(length(term_str) == 0) term_str <- "N/A"
    
    # --- D. Compile Data ---
    headlines <- corpus$meta$headline[match(member_ids, corpus$meta$doc_id)]
    
    cluster_audit <- data.frame(
      doc_id = member_ids,
      cluster_id = cid,
      headline = headlines,
      centrality = as.numeric(centrality_scores),
      stringsAsFactors = FALSE
    )
    audit_list[[cid]] <- cluster_audit
    
    metrics_list[[cid]] <- data.frame(
      cluster_id = cid,
      n_docs = n_docs,
      density = dens,
      conductance = cond,
      mean_trans = mean_trans,
      sd_trans = sd_trans,
      mean_path = mean_path,
      sd_path = sd_path,
      mean_centrality = mean(centrality_scores),
      sd_centrality = sd(centrality_scores),
      top_terms = term_str,
      centroid_ref = centroid_ref
    )
  }
  
  metrics_df <- bind_rows(metrics_list)
  audit_df <- bind_rows(audit_list) %>% arrange(cluster_id, desc(centrality))
  
  return(list(metrics = metrics_df, audit = audit_df))
}

#' Generate Full Test Bench Report
run_test_bench <- function(corpus, method = "tfidf", threshold = 0.2, 
                           bm25_k = 1.2, bm25_b = 1.0, boot_iter = 30,
                           resolution = 1.0, vectors = NULL) {
  
  message(sprintf("\n=== RUNNING TEST BENCH: %s (Thresh: %.2f) ===", toupper(method), threshold))
  
  # 1. Phase 2: Generate Matrix & Sim
  if (method == "embeddings_weighted") {
    if(is.null(vectors)) stop("Missing 'vectors' argument.")
    valid_terms <- intersect(rownames(vectors), corpus$term_stats$term)
    subset_vecs <- vectors[valid_terms, ]
    
    message(">> Calculating Weighted Document Vectors...")
    dtm <- get_weighted_document_vectors(corpus$tokens, subset_vecs)
    sim <- text2vec::sim2(dtm, method = "cosine", norm = "l2")
    
  } else if (method == "embeddings_mean") {
    if(is.null(vectors)) stop("Missing 'vectors' argument.")
    valid_terms <- intersect(rownames(vectors), corpus$term_stats$term)
    subset_vecs <- vectors[valid_terms, ]
    
    message(">> Calculating Mean Document Vectors...")
    dtm <- get_mean_document_vectors(corpus$tokens, subset_vecs)
    sim <- text2vec::sim2(dtm, method = "cosine", norm = "l2")
    
  } else {
    # Sparse Methods (TFIDF, BM25, SCM)
    dtm <- get_document_matrix(corpus, method = method, k = bm25_k, b = bm25_b)
    
    # Logic to handle SCM or fallback to Jaccard/Cosine
    if (method == "scm") {
      sim_type <- "scm"
    } else {
      sim_type <- ifelse(method %in% c("binary", "jaccard"), "jaccard", "cosine")
    }
    
    sim <- compute_similarity(dtm, method = sim_type, vectors = vectors)
  }
  
  # 2. Phase 3: Bootstrap Topology
  message(sprintf(">> Step A: Bootstrapping Global Topology (%d iters)...", boot_iter))
  
  boot_res <- bootstrap_topology(
    corpus, 
    method = method, 
    threshold = threshold, 
    n_iter = boot_iter, 
    vectors = vectors
  )
  
  # 3. Phase 3b: Build Single Graph
  g <- build_graph(sim, threshold)
  
  # 4. Phase 4: Clustering
  message(">> Step B: Analyzing Clusters...")
  cluster_res <- analyze_clusters(g, dtm, corpus, method = method, resolution = resolution)
  
  message(sprintf(">> COMPLETE. Graph has %d clusters.", nrow(cluster_res$metrics)))
  
  return(list(
    global = boot_res$summary,
    clusters = cluster_res$metrics,
    audit = cluster_res$audit,
    graph = g
  ))
}



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
    mutate(rank = row_number(), score = tf_global, metric = "Raw frequency (Zipf)")
  
  # 2. TF * IDF Rank
  # (Global Sum of TF) * IDF
  curve_tfidf <- corpus$term_stats %>%
    mutate(score = tf_global * idf) %>%
    arrange(desc(score)) %>%
    mutate(rank = row_number(), metric = "Global term frequency * inverse document frequency (TF*IDF)")
  
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
    mutate(rank = row_number(), metric = "Global BM25 mass")
  
  # Combine
  bind_rows(
    curve_tf %>% select(rank, score, metric, term),
    curve_tfidf %>% select(rank, score, metric, term),
    curve_bm25 %>% select(rank, score, metric, term)
  )
}

#' Plot the Rank Curves (Combined)
plot_rank_curves <- function(curve_data) {
  
  # FIX: Filter out non-positive scores to prevent log-scale warnings
  # (Removes stopwords with negative/zero IDF that break the log plot)
  plot_data <- curve_data %>% 
    filter(score > 1e-9) 
  
  # Calculate intercept for reference line
  max_score <- max(plot_data$score, na.rm = TRUE)
  
  ggplot(plot_data, aes(x = rank, y = score, color = metric)) +
    # Reference Line
    geom_abline(slope = -1, intercept = log10(max_score), 
                linetype = "dashed", color = "black", alpha = 0.5) +
    
    # The Curves
    geom_line(size = 1) +
    
    # Scales
    scale_x_log10(labels = scales::comma) +
    scale_y_log10(labels = scales::comma) +
    
    # Legend Wrapping
    scale_color_discrete(labels = function(x) stringr::str_wrap(x, width = 25)) +
    
    theme_minimal() +
    labs(
      title = "Lexical rank: Comparing the Zipf rank-frequency curve to formulas for term importance",
      subtitle = "Solid lines = Ranking formulae; Dashed line = Theoretical Zipfian distribution (Slope -1)",
      x = "Log rank",
      y = "Log score",
      color = NULL 
    )
}

run_model_suite <- function(corpus, vectors = NULL, threshold = 0.3, boot_iter = 50) {
  
  message(sprintf("\n>> RUNNING MODEL SUITE (Threshold: %.2f, Boot: %d)...", threshold, boot_iter))
  
  suite_results <- list()
  
  # --- Define Models to Test ---
  models <- list(
    "Term frequency"    = list(method = "tf", vecs = NULL),
    "TF-IDF"            = list(method = "tfidf", vecs = NULL),
    "BM25"              = list(method = "bm25",  vecs = NULL),
    "Jaccard"           = list(method = "jaccard", vecs = NULL)#, 
    #"Embeddings (mean)" = list(method = "embeddings_mean", vecs = vectors),
    #"Embeddings (TF-IDF weighted)"  = list(method = "embeddings_weighted", vecs = vectors), 
    #"Soft cosine measure" = list(method = "scm", vecs = vectors)
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


#' Calculate Vocabulary Statistics (Entropy + PageRank)
#' @param dtm Sparse Document-Term Matrix
#' @param similarity_threshold Cutoff for the PageRank graph (to keep it sparse)
calc_vocab_stats <- function(dtm, similarity_threshold = 0.2) {
  
  # 1. ENTROPY CALCULATION (Distribution)
  term_freqs <- Matrix::colSums(dtm)
  inv_freq_diag <- Matrix::Diagonal(x = 1 / term_freqs)
  P_dt <- dtm %*% inv_freq_diag
  P_dt@x <- P_dt@x * log2(P_dt@x)
  H_t <- -Matrix::colSums(P_dt)
  
  # 2. PAGERANK CALCULATION (Structure)
  # We need a Term-Term Graph. 
  # A. Cosine Similarity between Terms (not Docs!)
  # Formula: T = DTM_norm' %*% DTM_norm
  
  # Normalize columns (Terms) to length 1
  # (Standard L2 normalization for Cosine)
  norm_vec <- sqrt(Matrix::colSums(dtm^2))
  dtm_norm <- dtm %*% Matrix::Diagonal(x = 1/norm_vec)
  
  # Compute Term-Term Similarity Matrix (Sparse)
  # This can be heavy for >10k terms. 
  # Optimization: Only compute for terms appearing > X times?
  # For now, we assume vocab < 10k or sufficient RAM.
  term_sim <- Matrix::crossprod(dtm_norm) # t(DTM) %*% DTM
  
  # B. Prune weak links to make PageRank efficient/meaningful
  # PageRank acts weird on fully connected weighted graphs (everything becomes equal).
  term_sim[term_sim < similarity_threshold] <- 0
  diag(term_sim) <- 0
  
  # C. Run PageRank
  # 'igraph' is faster than manual power iteration in R
  g_term <- igraph::graph_from_adjacency_matrix(term_sim, mode="undirected", weighted=TRUE)
  pr_res <- igraph::page_rank(g_term, damping = 0.85)$vector
  
  # 3. COMPILE RESULTS
  stats <- data.frame(
    Term = names(term_freqs),
    Frequency = as.numeric(term_freqs),
    Entropy = as.numeric(H_t),
    PageRank = as.numeric(pr_res)
  ) %>%
    filter(Frequency > 1) %>%
    mutate(
      Expected_Entropy = log2(Frequency),
      Entropy_Gap = Entropy - Expected_Entropy, # High Negative = Bursty
      
      # Rank metrics for easier plotting
      Rank_Gap = percent_rank(desc(Entropy_Gap)),
      Rank_PR = percent_rank(PageRank)
    )
  
  return(stats)
}


#' Update Positional Index (Token + Offset Tracking)
#' 
#' Scans the database for new documents. 
#' Splits content into paragraphs and tokenizes each, storing strict character offsets.
#' This table is the "Coordinate System" for annotations and highlighting.
#' 
#' @param db_connection DBIConnection.
#' @export
update_positional_index <- function(db_connection) {
  
  require(stringi)
  
  # 1. Define Schema
  # doc_tokens: Stores the precise location of every word
  DBI::dbExecute(db_connection, "
    CREATE TABLE IF NOT EXISTS doc_tokens (
      doc_id TEXT,
      para_id INTEGER,
      token_id INTEGER,
      term TEXT,
      char_start INTEGER,
      char_end INTEGER,
      PRIMARY KEY (doc_id, para_id, token_id),
      FOREIGN KEY(doc_id) REFERENCES doc_index(doc_id)
    )
  ")
  
  # 2. Identify New Docs
  message(">> Checking for new documents to index...")
  all_ids <- DBI::dbGetQuery(db_connection, "SELECT doc_id FROM doc_content")
  indexed_ids <- DBI::dbGetQuery(db_connection, "SELECT DISTINCT doc_id FROM doc_tokens")
  ids_to_process <- setdiff(all_ids$doc_id, indexed_ids$doc_id)
  
  if(length(ids_to_process) == 0) {
    message("   . Index is up to date.")
    return(invisible(TRUE))
  }
  
  message(paste("   + Found", length(ids_to_process), "new documents."))
  
  # 3. Process in Batches
  batch_size <- 100
  batches <- split(ids_to_process, ceiling(seq_along(ids_to_process)/batch_size))
  
  for(i in seq_along(batches)) {
    batch_ids <- batches[[i]]
    id_str <- paste0("'", batch_ids, "'", collapse = ",")
    
    # Fetch content
    raw_data <- DBI::dbGetQuery(db_connection, 
                                paste0("SELECT doc_id, full_text FROM doc_content WHERE doc_id IN (", id_str, ")"))
    
    # Process Batch
    token_data_list <- list()
    
    for(row_idx in 1:nrow(raw_data)) {
      curr_doc <- raw_data$doc_id[row_idx]
      curr_text <- raw_data$full_text[row_idx]
      
      # A. Split into Paragraphs (Position 1 is Para 1)
      paras <- stringi::stri_split_lines(curr_text)[[1]]
      
      # B. Tokenize each paragraph with offsets
      doc_res <- lapply(seq_along(paras), function(p_idx) {
        p_text <- paras[p_idx]
        if(nchar(trimws(p_text)) == 0) return(NULL)
        
        # stri_locate_all_words gives start/end matrix
        locs <- stringi::stri_locate_all_words(p_text)[[1]]
        if(nrow(locs) == 0) return(NULL)
        
        words <- stringi::stri_sub(p_text, locs[,1], locs[,2])
        
        data.frame(
          doc_id = curr_doc,
          para_id = p_idx,
          token_id = seq_len(nrow(locs)),
          term = tolower(words), # Normalized for searching
          char_start = locs[,1],
          char_end = locs[,2]
        )
      })
      
      token_data_list[[row_idx]] <- dplyr::bind_rows(doc_res)
    }
    
    # Bulk Write
    big_df <- dplyr::bind_rows(token_data_list)
    if(nrow(big_df) > 0) {
      DBI::dbWriteTable(db_connection, "doc_tokens", big_df, append = TRUE)
    }
    message(paste("     Batch", i, "/", length(batches), "indexed."))
  }
  
  message(">> Positional indexing complete.")
  return(invisible(TRUE))
}