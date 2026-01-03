# R/cli_app.R
library(tidyverse)
library(here)
library(igraph)
library(text2vec)
library(htmltools)
library(gt)

# Load Toolkit
source(here::here("config.R"))
source_lib("text_utils.R")
source_lib("graph_utils.R")
source_lib("topologies.R")

# STORAGE FOR ANNOTATIONS
NOTE_PATH <- here::here("data", "annotations.csv")
if(!file.exists(NOTE_PATH)) write_csv(data.frame(doc_id=character(), note=character(), timestamp=character()), NOTE_PATH)

init_cli <- function() {
  message(">> Loading Model...")
  model <- readRDS(here::here("data", "app_ready", "model_current.rds"))
  
  # Calculate Document Bridge Scores
  message(">> Calculating Document Bridges...")
  bridges <- measure_node_bridges(model$graph) %>% select(doc_id, bridge_score = bridge_score_betweenness)
  
  # Calculate Clusters (Fast Leiden)
  comm <- igraph::cluster_leiden(model$graph, objective_function = "modularity", weights = igraph::E(model$graph)$weight)
  clusters <- data.frame(
    doc_id = names(igraph::membership(comm)),
    cluster_id = as.numeric(igraph::membership(comm))
  )
  
  index <- model$meta %>%
    left_join(bridges, by = "doc_id") %>%
    left_join(clusters, by = "doc_id") %>%
    mutate(
      bridge_score = ifelse(is.na(bridge_score), 0, bridge_score),
      cluster_id = ifelse(is.na(cluster_id), 0, cluster_id),
      headline = ifelse(is.na(headline) | headline == "", "(No Headline)", headline)
    )
  
  list(model = model, index = index)
}

run_cli <- function() {
  app <- init_cli()
  curr_res <- NULL # Buffer
  
  # --- HELPERS (Same as before) ---
  save_note <- function(id, text) {
    new_note <- data.frame(doc_id = id, note = text, timestamp = as.character(Sys.time()))
    write_csv(new_note, NOTE_PATH, append = TRUE)
    cat(">> Note saved.\n")
  }
  
  export_doc <- function(id, filename) {
    vec <- app$model$dtm[id, , drop=FALSE]
    terms <- names(which(vec[1,]>0))
    bursty <- app$model$stats %>% filter(Term %in% terms) %>% arrange(Entropy_Gap) %>% head(20) %>% pull(Term)
    
    all_notes <- read_csv(NOTE_PATH, show_col_types = FALSE) %>% filter(doc_id == id)
    note_html <- if(nrow(all_notes)>0) paste0("<ul>", paste0("<li>", all_notes$note, "</li>", collapse=""), "</ul>") else ""
    
    txt <- app$model$full_text[id]
    hl_txt <- highlight_fruit_salad(txt, list(bursty = bursty))
    
    html <- paste0(
      "<html><head><style>.hl-bursty { background-color: #d4edda; border-bottom: 2px solid #28a745; } body { font-family: sans-serif; max-width: 800px; margin: 2rem auto; }</style></head><body>",
      "<h1>", app$index$headline[app$index$doc_id == id], "</h1>",
      "<p><b>ID:</b> ", id, "</p><h3>Annotations</h3>", note_html, "<hr>", hl_txt, "</body></html>"
    )
    writeLines(html, filename)
    cat(sprintf(">> Exported to %s\n", filename))
  }
  
  render_compare <- function(id_A, id_B) {
    vec_A <- app$model$dtm[id_A, , drop=FALSE]; vec_B <- app$model$dtm[id_B, , drop=FALSE]
    terms_A <- names(which(vec_A[1,]>0)); terms_B <- names(which(vec_B[1,]>0))
    
    shared <- intersect(terms_A, terms_B)
    unique_A <- setdiff(terms_A, terms_B); unique_B <- setdiff(terms_B, terms_A)
    
    top_unique_A <- app$model$stats %>% filter(Term %in% unique_A) %>% arrange(Entropy_Gap) %>% head(10) %>% pull(Term)
    top_unique_B <- app$model$stats %>% filter(Term %in% unique_B) %>% arrange(Entropy_Gap) %>% head(10) %>% pull(Term)
    
    scm_df <- get_scm_edge_breakdown(id_A, id_B, app$model$dtm, app$model$vectors)
    scm_html <- if(is.null(scm_df)) "<p>No SCM vectors</p>" else scm_df %>% gt() %>% tab_header(title="Semantic Bridges") %>% as_raw_html()
    
    hl_A <- highlight_fruit_salad(app$model$full_text[id_A], list(shared=shared, distinctive=top_unique_A))
    hl_B <- highlight_fruit_salad(app$model$full_text[id_B], list(shared=shared, distinctive=top_unique_B))
    
    css <- ".hl-shared { background-color: #cce5ff; border-bottom: 2px solid #004085; } .hl-distinctive { background-color: #fff3cd; border-bottom: 2px solid #856404; } .col { flex: 1; padding: 10px; border: 1px solid #ddd; height: 600px; overflow-y: auto; }"
    html <- paste0("<html><head><style>", css, "</style></head><body><h2 style='text-align:center'>Comparison</h2><div style='display:flex; gap:10px;'><div class='col'>", hl_A, "</div><div style='width:300px'>", scm_html, "</div><div class='col'>", hl_B, "</div></div></body></html>")
    htmltools::browsable(htmltools::HTML(html))
  }
  
  # --- MAIN LOOP ---
  cat("CLI Ready. Commands:\n  s [term]\n  m bridges | m cluster [id]\n  v [n]\n  d [n1] [n2]\n  note [n] [text]\n  export [n] [file]\n")
  
  while(TRUE) {
    input <- readline(">> ")
    if(input == "q") break
    if(input == "") next
    
    args <- scan(text = input, what = "character", quiet = TRUE)
    cmd <- args[1]
    
    tryCatch({
      
      # --- SEARCH ---
      if(cmd == "s") {
        if(length(args) < 2) stop("Usage: s [term]")
        q <- paste(args[-1], collapse = " ") # Handle multi-word queries like 's climate change'
        
        # Check if term exists
        if(!any(str_detect(colnames(app$model$dtm), tolower(args[2])))) {
          cat("Term not found in vocabulary.\n")
          next
        }
        
        hits <- rownames(app$model$dtm)[which(app$model$dtm[, tolower(args[2])] > 0)]
        curr_res <- app$index %>% filter(doc_id %in% hits) %>% arrange(desc(date))
        
        print(curr_res %>% select(headline, date, cluster_id) %>% mutate(idx = row_number()) %>% select(idx, everything()) %>% head(20))
      }
      
      # --- METRICS ---
      else if(cmd == "m") {
        if(length(args) < 2) stop("Usage: m bridges | m cluster [id]")
        mode <- args[2]
        
        if(mode == "bridges") {
          curr_res <- app$index %>% arrange(desc(bridge_score))
          print(curr_res %>% select(headline, bridge_score) %>% mutate(idx = row_number()) %>% head(20))
        } 
        else if(mode == "cluster") {
          if(length(args) < 3) stop("Usage: m cluster [id]")
          cid <- as.numeric(args[3])
          curr_res <- app$index %>% filter(cluster_id == cid) %>% arrange(desc(bridge_score))
          print(curr_res %>% select(headline, bridge_score) %>% mutate(idx = row_number()) %>% head(20))
        } else {
          cat(sprintf("Unknown metric '%s'. Available: bridges, cluster\n", mode))
        }
      }
      
      # --- VIEW ---
      else if(cmd == "v") {
        if(is.null(curr_res)) stop("No search results. Run 's' or 'm' first.")
        if(length(args) < 2) stop("Usage: v [n]")
        idx <- as.numeric(args[2])
        if(is.na(idx) || idx > nrow(curr_res) || idx < 1) stop("Invalid index.")
        
        # Render view (using highlight_fruit_salad from utils)
        # Re-using export logic for simple view
        vec <- app$model$dtm[curr_res$doc_id[idx], , drop=FALSE]
        terms <- names(which(vec[1,]>0))
        bursty <- app$model$stats %>% filter(Term %in% terms) %>% arrange(Entropy_Gap) %>% head(20) %>% pull(Term)
        
        txt <- app$model$full_text[curr_res$doc_id[idx]]
        hl_txt <- highlight_fruit_salad(txt, list(bursty=bursty))
        
        html <- paste0("<html><head><style>.hl-bursty { background-color: #d4edda; }</style></head><body><h2>", curr_res$headline[idx], "</h2><hr>", hl_txt, "</body></html>")
        print(htmltools::browsable(htmltools::HTML(html)))
      }
      
      # --- DECOMPOSE ---
      else if(cmd == "d") {
        if(is.null(curr_res)) stop("No search results.")
        if(length(args) < 3) stop("Usage: d [n1] [n2] (Must provide two numbers)")
        
        idx1 <- as.numeric(args[2]); idx2 <- as.numeric(args[3])
        if(is.na(idx1) || is.na(idx2)) stop("Indices must be numbers.")
        if(idx1 > nrow(curr_res) || idx2 > nrow(curr_res)) stop("Index out of bounds.")
        
        print(render_compare(curr_res$doc_id[idx1], curr_res$doc_id[idx2]))
      }
      
      # --- NOTES ---
      else if(cmd == "note") {
        if(is.null(curr_res)) stop("No search results.")
        if(length(args) < 3) stop("Usage: note [n] [text]")
        idx <- as.numeric(args[2])
        txt <- paste(args[-(1:2)], collapse=" ")
        save_note(curr_res$doc_id[idx], txt)
      }
      
      # --- EXPORT ---
      else if(cmd == "export") {
        if(is.null(curr_res)) stop("No search results.")
        if(length(args) < 3) stop("Usage: export [n] [filename.html]")
        idx <- as.numeric(args[2])
        export_doc(curr_res$doc_id[idx], args[3])
      }
      
    }, error = function(e) {
      cat(sprintf("Error: %s\n", e$message))
    })
  }
}

if(interactive()) run_cli()