# app/app.R
source("../config.R")          # Load seeds/paths
source("../R/text_utils.R")    # Highlighter
source("../R/graph_utils.R")   # Graph Filter

library(shiny)
library(visNetwork)
library(tidyverse)
library(igraph)

# 1. LOAD THE WINNER (Pre-calculated in Analysis)
# We assume 'model_current.rds' contains: 
#   $graph (igraph), $dtm (sparse), $stats (entropy table), $full_text (named vector)
model <- readRDS("../data/app_ready/model_current.rds")

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* FRUIT SALAD CSS */
      .hl-bursty { background-color: #ffeb3b; } /* Yellow */
      .hl-query  { background-color: #4caf50; color: white; } /* Green */
      .hl-shared { border: 2px solid red; font-weight: bold; } /* Red Box */
      
      /* Toggles off state (CSS magic to hide highlights when class removed? 
         Actually better to handle in R, but we can do opacity here) */
    "))
  ),
  
  titlePanel("Interpublicity Workbench"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1. Navigation"),
      textInput("search_term", "Search Corpus:", placeholder = "e.g., kava"),
      hr(),
      
      h4("2. Fruit Salad Controls"),
      checkboxGroupInput("highlights", "Highlight Layers:",
                         choices = c("Search Terms" = "query", 
                                     "Bursty Words" = "bursty", 
                                     "Shared (Edge)" = "shared"),
                         selected = c("query", "bursty")),
      hr(),
      
      h4("3. Graph Settings"),
      sliderInput("threshold", "Edge Threshold", 0.1, 0.9, 0.3)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Global Graph", 
                 visNetworkOutput("net_plot", height = "700px")),
        
        tabPanel("Inspector",
                 h3(textOutput("doc_title")),
                 htmlOutput("doc_viewer"))
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  # --- A. SEARCH LOGIC ---
  # Find docs containing the term
  matched_docs <- reactive({
    req(input$search_term)
    term <- tolower(input$search_term)
    
    # Simple regex search in DTM terms or full text
    # (Fastest: Look up term in DTM columns)
    if (term %in% colnames(model$dtm)) {
      indices <- which(model$dtm[, term] > 0)
      return(rownames(model$dtm)[indices])
    }
    return(NULL)
  })
  
  # --- B. GRAPH RENDER ---
  output$net_plot <- renderVisNetwork({
    # 1. Get Base Graph (Static from Model)
    g <- model$graph 
    
    # 2. Apply Ghosting if Search Active
    target_ids <- if(nchar(input$search_term) > 0) matched_docs() else NULL
    vis_data <- prepare_visual_graph(g, target_ids)
    
    # 3. Render
    visNetwork(vis_data$nodes, vis_data$edges) %>%
      visPhysics(stabilization = FALSE) %>%
      visEvents(select = "function(nodes) { Shiny.onInputChange('sel_node', nodes.nodes); }")
  })
  
  # --- C. DOC INSPECTOR (FRUIT SALAD) ---
  observeEvent(input$sel_node, {
    req(input$sel_node)
    doc_id <- input$sel_node[1]
    
    # 1. Get Raw Text
    raw <- model$full_text[doc_id]
    
    # 2. Build Highlight Groups
    groups <- list()
    
    # Layer: Search Term
    if ("query" %in% input$highlights && nchar(input$search_term) > 0) {
      groups$query <- c(input$search_term)
    }
    
    # Layer: Bursty Words (Top 20 for this doc)
    if ("bursty" %in% input$highlights) {
      # Look up this doc's words in the Entropy stats
      # (This logic would live in R/metric_utils.R ideally)
      # Pseudocode:
      doc_terms <- names(which(model$dtm[doc_id,] > 0))
      top_bursty <- model$stats %>% 
        filter(Term %in% doc_terms) %>%
        arrange(Entropy_Gap) %>% # Lowest gap = most bursty
        head(20) %>%
        pull(Term)
      groups$bursty <- top_bursty
    }
    
    # 3. Apply Highlighting
    final_html <- highlight_fruit_salad(raw, groups)
    
    output$doc_viewer <- renderUI({ HTML(gsub("\n", "<br>", final_html)) })
    output$doc_title <- renderText({ paste("Document:", doc_id) })
  })
}

shinyApp(ui, server)