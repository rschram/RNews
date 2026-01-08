# ==============================================================================
# MASTER RUNNER: Model Generation -> Layer Calculation -> App Launch
# ==============================================================================

# 1. SETUP
library(here)
library(tidyverse)
library(DBI)
library(RSQLite)
library(text2vec)
library(Matrix)
library(igraph)

# Load Configuration & Library
source(here::here("config.R"))

# Source all functions in R/
# This ensures vectors.R, layer_utils.R, etc. are loaded
list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE) %>% 
  walk(source)

# ==============================================================================
# PHASE 1: DATA LOADING
# ==============================================================================
message("\n>>> PHASE 1: LOADING CORPUS")

corpus_name <- "Fiji Times (2005)"

DB_PATH <- file.path(PATH_PROC, "corpus_fiji_2005.sqlite")

if(!file.exists(DB_PATH)) { 
  message(">> Initializing Database...")
  in_con <- dbConnect(SQLite(), DB_PATH)
  
  # 1. Ingest Raw Text
  ingest_proquest(file.path(PATH_RAW, "FT_AugSep2005.txt"), in_con, boilerplate_patterns = "<[^>]+>")
  ingest_proquest(file.path(PATH_RAW, "FT_Oct2005.txt"), in_con, boilerplate_patterns = "<[^>]+>")
  
  # 2. Create Bag-of-Words Index (Old Method)
  update_token_index(in_con, remove_stop_words = FALSE)
  
  # 3. Create Positional Index (New Method for Blending/Annotations)
  update_positional_index(in_con)
  
  dbDisconnect(in_con)
}

con <- dbConnect(SQLite(), DB_PATH)
meta_df <- dbGetQuery(con, "SELECT doc_id, headline, date, author FROM doc_index") %>% as_tibble()
# We don't need tokens_df for the robust runner (we use DTM), saving RAM
content_df <- dbGetQuery(con, "SELECT doc_id, full_text FROM doc_content") %>% as_tibble()
dbDisconnect(con)

corpus <- create_corpus_object(data.frame(doc_id=character(), term=character(), n=integer()), meta_df) 
# Note: Empty tokens DF passed above because create_corpus_object expects it, 
# but we generate DTM from content_df below for robust handling.

# ==============================================================================
# PHASE 2: MODEL GENERATION (ROBUST HYBRID)
# ==============================================================================
message("\n>>> PHASE 2: GENERATING MODEL")

# 1. Vectors (for SCM Inspector only)
VECTOR_PATH <- file.path(PATH_PROC, "vectors.rds")
if(file.exists(VECTOR_PATH)) {
  message("   Loading cached vectors...")
  vectors_obj <- readRDS(VECTOR_PATH)
} else {
  message("   Training new vectors...")
  vectors_obj <- generate_corpus_vectors(content_df %>% rename(text = full_text))
  saveRDS(vectors_obj, VECTOR_PATH)
}

# 2. DTM & Stats
message("   Calculating DTM & Term Metrics...")
# Generate DTM from full text directly
it <- itoken(content_df$full_text, ids = content_df$doc_id, tokenizer = word_tokenizer)
vocab <- create_vocabulary(it) %>% prune_vocabulary(term_count_min = 2)
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(it, vectorizer)

# A. Calculate Entropy (The "Fruit Salad" Stats)
vocab_stats <- calc_vocab_stats(dtm, similarity_threshold = 0.2)

# B. Calculate Term Betweenness (The "Bridge" Stats)
# This is required for the "Cosmopolitan" map layer in the app
message("   Calculating Term Structure (TCM)...")
tcm <- create_tcm(it, vectorizer, skip_grams_window = 10)
norm_tcm <- tcm / rowSums(tcm)
norm_tcm@x[norm_tcm@x < 0.1] <- 0
norm_tcm <- drop0(norm_tcm)
g_terms <- graph_from_adjacency_matrix(norm_tcm, mode = "undirected", weighted = TRUE)

# Calculate for top terms only (optimization)
top_terms <- vocab_stats %>% arrange(desc(Frequency)) %>% head(3000) %>% pull(Term)
valid_nodes <- intersect(top_terms, V(g_terms)$name)
term_betw <- betweenness(induced_subgraph(g_terms, valid_nodes), normalized = TRUE)

# Merge back into stats
vocab_stats <- vocab_stats %>%
  left_join(data.frame(Term = names(term_betw), Term_Betweenness = as.numeric(term_betw)), by = "Term") %>%
  mutate(Term_Betweenness = ifelse(is.na(Term_Betweenness), 0, Term_Betweenness))


message("   Calculating Topic Clusters (Cosine @ 0.2)...")

# A. Build "Topic Graph" (Broad connections)
sim_topic <- compute_similarity(dtm, method = "cosine") # or "embeddings_mean" if preferred
g_topic <- build_graph(sim_topic, threshold = 0.2)

# B. Identify Communities
comm <- igraph::cluster_leiden(g_topic, objective_function = "modularity", weights = igraph::E(g_topic)$weight)
cluster_map <- data.frame(
  doc_id = names(igraph::membership(comm)),
  cluster_id = as.numeric(igraph::membership(comm))
)
message(sprintf("   Found %d clusters.", max(cluster_map$cluster_id)))

# C. Merge into Metadata
# This "bakes" the good clusters into the data, regardless of the graph we save later
meta_df <- meta_df %>%
  select(-any_of("cluster_id")) %>% # Remove old if exists
  left_join(cluster_map, by = "doc_id") %>%
  mutate(cluster_id = ifelse(is.na(cluster_id), 0, cluster_id))

# 3. Topology (Jaccard Skeleton)
message("   Building Topology (Jaccard @ 0.15)...")
# We can use a slightly lower threshold now that clusters are safe
sim_jaccard <- compute_similarity(dtm, method = "jaccard")
g_skeleton <- build_graph(sim_jaccard, threshold = 0.15) 

# 4. Save Winning Model
model_current <- list(
  model_name = "Hybrid Robust",
  graph = g_skeleton,     # The Sparse Skeleton for Bridge scores
  dtm = dtm,
  stats = vocab_stats,
  meta = meta_df,         # Contains the High-Quality Cluster IDs
  full_text = setNames(content_df$full_text, content_df$doc_id),
  vectors = vectors_obj$vectors
)

saveRDS(model_current, file.path(PATH_APP, "model_current.rds"))

# ==============================================================================
# PHASE 3: LAYER GENERATION & LAUNCH
# ==============================================================================
message("\n>>> PHASE 3: PREPARING APP LAYERS")

# This calls the function from R/layer_utils.R
model_light <- generate_light_model(model_current)

saveRDS(model_light, file.path(PATH_APP, "model_light.rds"))
message("   Light model saved.")

message("\n>>> LAUNCHING GUI...")
shiny::runApp(here::here("app", "app_gui.R"))