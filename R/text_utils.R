#' Multi-Layer Highlighter
#' @param text Raw text string
#' @param highlight_groups Named list of term vectors (e.g., list(bursty=c("kava"), query=c("fiji")))
#' @return HTML string with spanning tags
highlight_fruit_salad <- function(text, highlight_groups) {
  
  # 1. Tokenize (Simple space-based to preserve positions)
  # For production, consider 'tokenizers' package with span preservation
  # Here we use a robust regex replacement strategy
  
  processed_text <- text
  
  for (group_name in names(highlight_groups)) {
    terms <- highlight_groups[[group_name]]
    if (length(terms) == 0) next
    
    # Create Regex for all terms in this group
    # \b ensures distinct words (avoids highlighting 'the' inside 'theory')
    pattern <- paste0("\\b(", paste(terms, collapse = "|"), ")\\b")
    
    # Replacement adds a span with the group class
    replacement <- sprintf("<span class='hl-%s'>\\1</span>", group_name)
    
    # Case-insensitive replacement
    processed_text <- gsub(pattern, replacement, processed_text, ignore.case = TRUE)
  }
  
  return(processed_text)
}