library(stringr)

#' Escape Regex Characters
escape_regex <- function(string) {
  gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", string)
}

#' HTML Escape
html_escape <- function(text) {
  text <- gsub("&", "&amp;", text)
  text <- gsub("<", "&lt;", text)
  text <- gsub(">", "&gt;", text)
  text <- gsub("\"", "&quot;", text)
  text <- gsub("'", "&#39;", text)
  return(text)
}


#' Multi-Layer Highlighter V2 (Final Production)
#' Vectorized, Collision-Safe, Hex-Encoded
highlight_fruit_salad_x <- function(text, highlight_groups) {
  # 1. Sanitize Input
  if (is.null(text) || length(text) == 0) return("")
  processed_text <- as.character(unname(text)) 
  
  # 2. PHASE 1: TOKENIZATION (Vectorized)
  for (group_name in names(highlight_groups)) {
    term_vec <- highlight_groups[[group_name]]
    if (length(term_vec) == 0) next
    
    # Sort longest first
    term_vec <- term_vec[order(nchar(names(term_vec)), decreasing = TRUE)]
    terms <- names(term_vec)
    
    # Create score lookup map
    score_map <- term_vec
    names(score_map) <- tolower(names(term_vec))
    
    # Build Giant Regex
    escaped_terms <- escape_regex(terms)
    collapsed_pat <- paste(escaped_terms, collapse = "|")
    pat <- regex(paste0("\\b(", collapsed_pat, ")\\b"), ignore_case = TRUE)
    
    # Replacement
    processed_text <- str_replace_all(processed_text, pat, function(matches) {
      # 1. Get scores for ALL matches at once
      lower_matches <- tolower(matches)
      scores <- score_map[lower_matches]
      scores[is.na(scores)] <- 0.5 
      
      # 2. Process each match
      vapply(seq_along(matches), function(i) {
        m <- matches[i]
        s <- scores[i]
        hex_match <- paste(charToRaw(m), collapse = "")
        # NEW FORMAT: [[[HL:Group:Score:Hex]]]
        # distinct from 0.5|Hex__
        sprintf("[[[HL:%s:%s:%s]]]", group_name, round(s, 3), hex_match)
      }, character(1))
    })
  }
  
  # 3. PHASE 2: RENDERING
  
  # A. Handle NEW "Shared Terms" format: [[[HL:Group:Score:Hex]]]
  # Regex looks for triple brackets, preventing any collision with "Score|Hex__"
  processed_text <- str_replace_all(processed_text, "\\[\\[\\[HL:(.*?):(.*?):([0-9a-fA-F]+)\\]\\]\\]", function(matches) {
    vapply(matches, function(m) {
      # Strip delimiters
      inner <- str_remove(str_remove(m, "^\\[\\[\\[HL:"), "\\]\\]\\]$")
      parts <- str_split(inner, ":")[[1]]
      
      if(length(parts) < 3) return(m) 
      
      grp <- parts[1]
      score <- suppressWarnings(as.numeric(parts[2]))
      hex_str <- parts[3]
      if(is.na(score)) score <- 0.5
      
      decoded <- tryCatch({
        if(nchar(hex_str) %% 2 != 0) return("")
        # Decode Hex Pairs
        raw_vec <- as.raw(strtoi(substring(hex_str, seq(1, nchar(hex_str)-1, 2), seq(2, nchar(hex_str), 2)), 16L))
        rawToChar(raw_vec)
      }, error = function(e) "")
      
      op <- 0.2 + (score * 0.8)
      if(op > 1) op <- 1
      sprintf("<span class='hl-base hl-%s' style='--opacity:%s'>%s</span>", grp, round(op, 2), html_escape(decoded))
    }, character(1), USE.NAMES = FALSE)
  })
  
  # B. Handle OLD "Entropy" format: Score|Hex__ (Legacy)
  # This can no longer match the tags above because they don't contain | or __
  processed_text <- str_replace_all(processed_text, "([0-9.]+)\\|([0-9a-fA-F]+)__", function(matches) {
    vapply(matches, function(m) {
      parts <- str_split(m, "\\|")[[1]]
      score <- suppressWarnings(as.numeric(parts[1]))
      if(is.na(score)) score <- 0.5
      hex_str <- str_remove(parts[2], "__")
      
      decoded <- tryCatch({
        if(nchar(hex_str) %% 2 != 0) return("")
        raw_vec <- as.raw(strtoi(substring(hex_str, seq(1, nchar(hex_str)-1, 2), seq(2, nchar(hex_str), 2)), 16L))
        rawToChar(raw_vec)
      }, error = function(e) "")
      
      op <- 0.2 + (score * 0.8)
      if(op > 1) op <- 1
      sprintf("<span class='hl-base hl-entropy' style='--opacity:%s'>%s</span>", round(op, 2), html_escape(decoded))
    }, character(1), USE.NAMES = FALSE)
  })
  
  return(unname(processed_text))
}