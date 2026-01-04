library(shiny)
library(tidyverse)
library(here)

source(here::here("config.R"))
source(here::here("R", "text_utils.R"))

# Load Light Model
model <- readRDS(here::here("data", "app_ready", "model_light.rds"))

# Setup Annotations File
NOTE_PATH <- here::here("data", "annotations.csv")
if(!file.exists(NOTE_PATH)) write_csv(data.frame(doc_id=character(), note=character(), timestamp=character()), NOTE_PATH)

# Get available clusters
available_clusters <- sort(unique(model$meta$cluster_id))
available_clusters <- available_clusters[available_clusters != 0]

ui <- fluidPage(
  tags$head(
    # 1. CSS for Blending & Selection
    tags$style("
      /* Token Base */
      .tok { cursor: text; border-radius: 2px; padding: 0 1px; }
      
      /* Visual Layers (Blending Strategy) */
      .hl-bursty { background-color: rgba(46, 204, 113, 0.25); border-bottom: 2px solid rgba(46, 204, 113, 1); }
      .hl-bridge { box-shadow: 0 2px 0 rgba(241, 196, 15, 0.8) inset; } /* Inset shadow prevents layout shifts */
      .hl-shared { border: 1px solid rgba(155, 89, 182, 0.6); }
      .hl-query  { background-color: rgba(255, 255, 0, 0.4); font-weight: bold; }
      
      /* User Annotation Selection (Temporary Visual) */
      .user-select { background-color: #ffe082 !important; outline: 1px dashed #f39c12; }
      
      /* Layout */
      .doc-box { border: 1px solid #ddd; padding: 20px; height: 75vh; overflow-y: auto; background: white; font-family: 'Georgia', serif; line-height: 1.6; }
      .note-box { background: #fff3cd; padding: 8px; margin-bottom: 8px; border-left: 4px solid #ffecb5; font-size: 0.9em; }
    "),
    
    # 2. JavaScript Selection Handler
    tags$script("
      $(document).on('mouseup', '.doc-box', function() {
        var sel = window.getSelection();
        if(sel.rangeCount > 0 && !sel.isCollapsed) {
          var range = sel.getRangeAt(0);
          
          // Find strictly the token spans inside the selection
          // (We use jQuery to find the closest .tok class to the start/end of selection)
          var startNode = $(range.startContainer).closest('.tok');
          var endNode = $(range.endContainer).closest('.tok');
          
          if(startNode.length && endNode.length) {
            var startId = startNode.attr('id'); // e.g. 'p1-t4'
            var endId = endNode.attr('id');     // e.g. 'p1-t8'
            
            // Send to Shiny
            Shiny.setInputValue('current_selection', {
              start: startId, 
              end: endId, 
              text: sel.toString()
            }, {priority: 'event'});
          }
        }
      });
    ")
  ),
  
  titlePanel("Corpus Reader"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("search", "Search Terms"),
      checkboxInput("show_search_highlights", "Highlight Search Terms", value = TRUE),
      hr(),
      h4("Map Layers"),
      checkboxInput("show_bursty", "Entropy (Burstiness)", value = FALSE),
      checkboxInput("show_bridge", "Bridges (Cosmopolitan)", value = FALSE),
      checkboxInput("show_shared", "Shared Terms (Comparisons)", value = FALSE),
      
      h5("Topic Clusters"),
      div(class = "cluster-scroll",
          checkboxGroupInput("show_clusters", NULL, choices = available_clusters, selected = NULL)
      ),
      
      hr(),
      h4("Annotation"),
      textAreaInput("note_input", "Add Note to Active Doc", rows = 3),
      actionButton("save_note", "Save Note", icon = icon("save"), class = "btn-primary btn-sm"),
      
      hr(),
      downloadButton("download_view", "Export View"),
      hr(),
      uiOutput("result_list")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(id = "main_tabs",
                  tabPanel("Single View", uiOutput("single_view")),
                  tabPanel("Comparison", uiOutput("compare_view"))
      )
    )
  )
)

server <- function(input, output, session) {
  

  
  # --- 1. DATA & SEARCH ---
  
  # Reactive Annotations
  notes_data <- reactiveVal({
    if(file.exists(NOTE_PATH)) read_csv(NOTE_PATH, show_col_types = FALSE) else data.frame()
  })
  
  found_docs <- reactive({
    req(input$search)
    q <- tolower(input$search)
    # Check if term exists to avoid error
    if(!q %in% colnames(model$dtm)) return(model$meta[0,])
    hits <- which(model$dtm[, q] > 0)
    model$meta %>% filter(doc_id %in% rownames(model$dtm)[hits]) %>% arrange(desc(date))
  })
  
  output$result_list <- renderUI({
    req(found_docs())
    df <- found_docs()
    if(nrow(df) == 0) return("No results.")
    
    tagList(
      p(class="text-muted", "Select to View/Compare."),
      checkboxGroupInput("compare_ids", NULL,
                         choices = setNames(df$doc_id, df$headline),
                         selected = NULL)
    )
  })
  
  # Auto-switch Logic
  observeEvent(input$compare_ids, {
    if(length(input$compare_ids) == 1) updateTabsetPanel(session, "main_tabs", selected = "Single View")
    else if(length(input$compare_ids) > 1) updateTabsetPanel(session, "main_tabs", selected = "Comparison")
  })
  
  # --- 2. LOGIC ---
  
  get_groups <- function(doc_id, other_ids = NULL) {
    groups <- list()
    if(input$show_bursty) groups$bursty <- model$layers$entropy
    if(input$show_bridge) groups$bridge <- model$layers$bridge
    
    # Clusters (Merge selected)
    if(!is.null(input$show_clusters)) {
      merged_c <- numeric()
      for(cid in input$show_clusters) {
        vec <- model$layers$clusters[[as.character(cid)]]
        if(!is.null(vec)) {
          # Merge by taking max score for overlapping terms
          keys <- unique(c(names(merged_c), names(vec)))
          v1 <- merged_c[keys]; v1[is.na(v1)] <- 0
          v2 <- vec[keys]; v2[is.na(v2)] <- 0
          merged_c <- pmax(v1, v2)
          names(merged_c) <- keys
        }
      }
      if(length(merged_c)>0) groups$cluster <- merged_c
    }
    
    # Shared Terms (Intersection of ALL compared docs)
    if(input$show_shared && !is.null(other_ids)) {
      # Start with doc_id terms
      common <- names(which(model$dtm[doc_id, ] > 0))
      
      # Intersect with others
      for(oid in other_ids) {
        if(oid == doc_id) next
        terms_o <- names(which(model$dtm[oid, ] > 0))
        common <- intersect(common, terms_o)
      }
      
      if(length(common) > 0) groups$shared <- setNames(rep(1.0, length(common)), common)
    }
    
    return(groups)
  }
  
  # --- 3. ANNOTATION ---
  
  # --- 3. ANNOTATION LOGIC (UPDATED) ---
  
  # A. Listen for Selection
  observeEvent(input$current_selection, {
    sel <- input$current_selection
    # Optional: You could show a UI feedback here, like "Selection: p1-t4 to p1-t8"
    updateTextAreaInput(session, "note_input", 
                        label = paste0("Add Note to selection (", substr(sel$text, 1, 20), "...)"),
                        placeholder = "Type your insight here...")
  })
  
  # B. Save Note with Coordinates
  observeEvent(input$save_note, {
    ids <- input$compare_ids
    if(length(ids) == 0) return()
    
    # If user has made a text selection, use those coords. Otherwise, default to "Doc Level"
    sel <- input$current_selection
    
    # Simple check: Is the selection recent/valid? (You might want to reset this after saving)
    has_selection <- !is.null(sel) && nzchar(sel$start)
    
    new_entry <- data.frame(
      doc_id = ids[1], # Attach to the first active doc
      start_token = if(has_selection) sel$start else NA,
      end_token   = if(has_selection) sel$end else NA,
      highlight_text = if(has_selection) sel$text else NA,
      note = input$note_input,
      timestamp = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )
    
    # Save (Append to CSV)
    write_csv(new_entry, NOTE_PATH, append = TRUE)
    
    # Refresh
    notes_data(read_csv(NOTE_PATH, show_col_types = FALSE))
    updateTextAreaInput(session, "note_input", value = "", label = "Add Note to Active Doc")
    showNotification("Annotation saved!", type = "message")
  })
  
  # C. Display Notes (Updated to show context)
  get_doc_notes <- function(id) {
    df <- notes_data()
    if(nrow(df) == 0) return("")
    
    doc_notes <- df %>% filter(doc_id == id) %>% arrange(desc(timestamp))
    if(nrow(doc_notes) == 0) return("")
    
    # Format: "Note text" (quoted text if available)
    items <- apply(doc_notes, 1, function(row) {
      context <- if(!is.na(row['highlight_text'])) paste0("<br><i>Ref: \"", substr(row['highlight_text'], 1, 50), "...\"</i>") else ""
      paste0("<li><b>", row['note'], "</b>", context, "</li>")
    })
    
    paste0("<div class='note-box'><ul>", paste(items, collapse=""), "</ul></div>")
  }
  
  # --- 4. VIEWS ---
  get_doc_tokens <- function(id) {
    # Open discrete connection or use global pool
    local_con <- dbConnect(RSQLite::SQLite(), here::here("data", "processed", "corpus_fiji_2005.sqlite"))
    on.exit(dbDisconnect(local_con))
    
    # Fetch tokens sorted by position
    dbGetQuery(local_con, paste0("SELECT * FROM doc_tokens WHERE doc_id = '", id, "' ORDER BY para_id, token_id"))
  }
  
  output$single_view <- renderUI({
    ids <- input$compare_ids
    if(length(ids) == 0) return(h4("Select a document."))
    
    id <- ids[1]
    raw_txt <- unname(model$full_text[id])
    
    # 1. Fetch Positional Data
    tokens <- get_doc_tokens(id)
    
    # 2. Get Highlights
    # (Assuming your existing get_groups function now returns list(query=..., bursty=...))
    groups <- get_groups(id)
    
    # 3. Add Query to Groups (Conditional)
    if(isTruthy(input$search) && isTRUE(input$show_search_highlights)) {
      groups$query <- c(input$search)
    }
    
    # 4. Render
    if(nrow(tokens) == 0) {
      # Fallback if doc wasn't indexed yet
      HTML(paste("<div class='alert alert-warning'>Positional index missing.</div>", gsub("\n", "<br>", raw_txt)))
    } else {
      html_content <- render_blended_document(raw_txt, tokens, groups)
      
      div(class="doc-box",
          HTML(get_doc_notes(id)), # Existing note logic
          h2(model$meta$headline[model$meta$doc_id == id]),
          HTML(html_content)
      )
    }
  })
  
  output$compare_view <- renderUI({
    ids <- input$compare_ids
    if(length(ids) < 2) return(h4("Select 2+ documents."))
    
    # Dynamic Columns
    cols <- lapply(ids, function(id) {
      raw_txt <- unname(model$full_text[id])
      
      # 1. Fetch Tokens
      tokens <- get_doc_tokens(id)
      
      # 2. Get Groups (Passing 'ids' ensures shared terms are calculated)
      groups <- get_groups(id, ids)
      
      # 3. Add Search Query Highlight
      if(isTruthy(input$search) && isTRUE(input$show_search_highlights)) { 
          groups$query <- c(input$search)
      }
      # 4. Render with Blending
      if(nrow(tokens) == 0) {
        # Fallback if positional index is missing
        html_content <- paste("<div class='alert alert-warning'>Positional index missing.</div>", gsub("\n", "<br>", raw_txt))
      } else {
        html_content <- render_blended_document(raw_txt, tokens, groups)
      }
      
      column(width = floor(12 / length(ids)), class="comp-col",
             div(class="doc-box",
                 HTML(get_doc_notes(id)),
                 h4(model$meta$headline[model$meta$doc_id == id]),
                 HTML(html_content)
             )
      )
    })
    
    do.call(fluidRow, cols)
  })
  
  # --- 5. EXPORT (QUARTO READY) ---
  
  output$download_view <- downloadHandler(
    filename = function() { paste0("exported_view_", Sys.Date(), ".html") },
    content = function(file) {
      ids <- input$compare_ids
      if(length(ids) == 0) return()
      
      # 1. Generate the HTML Content exactly as seen
      # We re-run the render function to get the clean HTML
      content_divs <- lapply(ids, function(id) {
        # Fetch tokens/groups (reusing your server helpers)
        tokens <- get_doc_tokens(id)
        groups <- get_groups(id, ids)
        if(isTruthy(input$search)) groups$query <- c(input$search)
        
        raw_txt <- unname(model$full_text[id])
        hl_html <- render_blended_document(raw_txt, tokens, groups)
        notes   <- get_doc_notes(id)
        
        paste0(
          "<div class='export-doc'>",
          "<div class='meta'>",
          "<h2>", model$meta$headline[model$meta$doc_id == id], "</h2>",
          "<p class='byline'>", model$meta$date[model$meta$doc_id == id], " | ", model$meta$author[model$meta$doc_id == id], "</p>",
          "</div>",
          "<div class='notes'>", notes, "</div>",
          "<div class='body'>", hl_html, "</div>",
          "</div>"
        )
      })
      
      # 2. Embed the CSS (Critical for static Quarto figures)
      # We embed the EXACT same CSS classes used in the app
      css_block <- "
        <style>
          body { font-family: 'Georgia', serif; font-size: 14px; line-height: 1.6; color: #333; }
          .export-doc { border: 1px solid #ccc; padding: 20px; margin-bottom: 20px; background: #fff; }
          .byline { color: #666; font-style: italic; margin-bottom: 20px; }
          .notes { background: #f9f9f9; padding: 10px; border-left: 4px solid #333; margin-bottom: 20px; }
          
          /* The Blending Classes */
          .tok { padding: 0 1px; }
          .hl-bursty { background-color: rgba(46, 204, 113, 0.3); }
          .hl-bridge { box-shadow: 0 2px 0 rgba(241, 196, 15, 0.8); }
          .hl-shared { border: 1px solid rgba(155, 89, 182, 0.6); }
          .hl-query  { background-color: rgba(255, 255, 0, 0.5); font-weight: bold; }
        </style>
      "
      
      # 3. Assemble
      full_html <- paste0(
        "<!DOCTYPE html><html><head><meta charset='utf-8'>", css_block, "</head><body>",
        paste(content_divs, collapse="\n"),
        "</body></html>"
      )
      
      writeLines(full_html, file)
    }
  )
}


shinyApp(ui, server)