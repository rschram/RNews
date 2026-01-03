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
    tags$style("
      .hl-base { padding: 0 1px; border-radius: 3px; }
      :root {
        --bursty-rgb: 46, 204, 113;
        --bridge-rgb: 241, 196, 15;
        --shared-rgb: 155, 89, 182;
        --cluster-rgb: 52, 152, 219; 
      }
      .hl-bursty { background-color: rgba(var(--bursty-rgb), var(--opacity)); }
      .hl-bridge { background-color: rgba(var(--bridge-rgb), var(--opacity)); }
      .hl-shared { background-color: rgba(var(--shared-rgb), var(--opacity)); }
      .hl-cluster { background-color: rgba(var(--cluster-rgb), var(--opacity)); }
      
      .doc-box { border: 1px solid #ddd; padding: 20px; height: 75vh; overflow-y: auto; background: white; }
      .comp-col { padding: 0 5px; }
      .cluster-scroll {
        max-height: 150px; overflow-y: auto; border: 1px solid #eee; padding: 5px; background: #f9f9f9;
      }
      .note-box { background: #fff3cd; padding: 10px; margin-bottom: 10px; border-left: 5px solid #ffecb5; }
    ")
  ),
  
  titlePanel("Corpus Reader"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("search", "Search Terms"),
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
  
  observeEvent(input$save_note, {
    ids <- input$compare_ids
    if(length(ids) == 0) return()
    
    # Save to the FIRST active document (or Single View doc)
    target_id <- ids[1]
    
    new_entry <- data.frame(
      doc_id = target_id,
      note = input$note_input,
      timestamp = as.character(Sys.time())
    )
    
    write_csv(new_entry, NOTE_PATH, append = TRUE)
    notes_data(read_csv(NOTE_PATH, show_col_types = FALSE)) # Refresh
    updateTextAreaInput(session, "note_input", value = "")
    showNotification("Note saved!", type = "message")
  })
  
  get_doc_notes <- function(id) {
    df <- notes_data()
    if(nrow(df) == 0) return("")
    
    doc_notes <- df %>% filter(doc_id == id) %>% arrange(desc(timestamp))
    if(nrow(doc_notes) == 0) return("")
    
    items <- paste0("<li>", doc_notes$note, " <small class='text-muted'>(", doc_notes$timestamp, ")</small></li>", collapse="")
    paste0("<div class='note-box'><ul>", items, "</ul></div>")
  }
  
  # --- 4. VIEWS ---
  
  output$single_view <- renderUI({
    ids <- input$compare_ids
    if(length(ids) == 0) return(h4("Select a document."))
    
    id <- ids[1]
    txt <- unname(model$full_text[id]) # UNNAME FIXES VCTRS ERROR
    print("--- DEBUG: Data Structure ---")
    str(txt) # Replace with the actual variable name passed to the highlighter
    str(get_groups(id)) # Replace with the actual dictionary/term variable
    hl_html <- highlight_fruit_salad_x(txt, get_groups(id))
    
    div(class="doc-box",
        HTML(get_doc_notes(id)),
        h2(model$meta$headline[model$meta$doc_id == id]),
        HTML(gsub("\n", "<br>", hl_html))
    )
  })
  
  output$compare_view <- renderUI({
    ids <- input$compare_ids
    if(length(ids) < 2) return(h4("Select 2+ documents."))
    
    # Dynamic Columns
    cols <- lapply(ids, function(id) {
      txt <- unname(model$full_text[id]) # UNNAME FIXES VCTRS ERROR
      
      # Pass ALL ids to get_groups to calculate intersection correctly
      hl_html <- highlight_fruit_salad_x(txt, get_groups(id, ids))
      
      column(width = floor(12 / length(ids)), class="comp-col",
             div(class="doc-box",
                 HTML(get_doc_notes(id)),
                 h4(model$meta$headline[model$meta$doc_id == id]),
                 HTML(gsub("\n", "<br>", hl_html))
             )
      )
    })
    
    do.call(fluidRow, cols)
  })
  
  # --- 5. EXPORT ---
  
  output$download_view <- downloadHandler(
    filename = function() { paste0("view_export_", Sys.Date(), ".html") },
    content = function(file) {
      ids <- input$compare_ids
      if(length(ids) == 0) return()
      
      # Generate content based on current selection
      divs <- lapply(ids, function(id) {
        txt <- unname(model$full_text[id])
        hl_html <- highlight_fruit_salad_x(txt, get_groups(id, if(length(ids)>1) ids else NULL))
        notes <- get_doc_notes(id)
        paste0("<div style='flex:1; padding:10px; border:1px solid #ccc; margin:5px;'>",
               notes, "<h2>", model$meta$headline[model$meta$doc_id == id], "</h2>",
               gsub("\n", "<br>", hl_html), "</div>")
      })
      
      body_content <- paste0("<div style='display:flex; flex-wrap:wrap;'>", paste(divs, collapse=""), "</div>")
      
      html <- paste0(
        "<html><head><style>",
        ".hl-base { border-radius: 3px; }",
        ".hl-bursty { background-color: rgba(46, 204, 113, var(--opacity)); }",
        ".hl-bridge { background-color: rgba(241, 196, 15, var(--opacity)); }",
        ".hl-shared { background-color: rgba(155, 89, 182, var(--opacity)); }",
        ".hl-cluster { background-color: rgba(52, 152, 219, var(--opacity)); }",
        ".note-box { background: #fff3cd; padding: 10px; border: 1px solid #ffeeba; }",
        "body { font-family: sans-serif; }",
        "</style></head><body><h1>Exported View</h1>", body_content, "</body></html>"
      )
      writeLines(html, file)
    }
  )
}

shinyApp(ui, server)