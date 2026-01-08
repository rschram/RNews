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
highlight_fruit_salad_x <- function(text, highlight_groups) {
  if (is.null(text) || length(text) == 0) return("")
  processed_text <- as.character(unname(text)) 
  
  for (group_name in names(highlight_groups)) {
    term_vec <- highlight_groups[[group_name]]
    if (length(term_vec) == 0) next
    
    term_vec <- term_vec[order(nchar(names(term_vec)), decreasing = TRUE)]
    terms <- names(term_vec)
    score_map <- term_vec
    names(score_map) <- tolower(names(term_vec))
    
    escaped_terms <- escape_regex(terms)
    collapsed_pat <- paste(escaped_terms, collapse = "|")
    pat <- regex(paste0("\\b(", collapsed_pat, ")\\b"), ignore_case = TRUE)
    
    processed_text <- str_replace_all(processed_text, pat, function(matches) {
      lower_matches <- tolower(matches)
      scores <- score_map[lower_matches]
      scores[is.na(scores)] <- 0.5 
      vapply(seq_along(matches), function(i) {
        m <- matches[i]; s <- scores[i]
        hex_match <- paste(charToRaw(m), collapse = "")
        sprintf("[[[HL:%s:%s:%s]]]", group_name, round(s, 3), hex_match)
      }, character(1))
    })
  }
  
  processed_text <- str_replace_all(processed_text, "\\[\\[\\[HL:(.*?):(.*?):([0-9a-fA-F]+)\\]\\]\\]", function(matches) {
    vapply(matches, function(m) {
      inner <- str_remove(str_remove(m, "^\\[\\[\\[HL:"), "\\]\\]\\]$")
      parts <- str_split(inner, ":")[[1]]
      if(length(parts) < 3) return(m) 
      grp <- parts[1]; score <- suppressWarnings(as.numeric(parts[2])); hex_str <- parts[3]
      if(is.na(score)) score <- 0.5
      decoded <- tryCatch({
        if(nchar(hex_str) %% 2 != 0) return("")
        raw_vec <- as.raw(strtoi(substring(hex_str, seq(1, nchar(hex_str)-1, 2), seq(2, nchar(hex_str), 2)), 16L))
        rawToChar(raw_vec)
      }, error = function(e) "")
      op <- 0.2 + (score * 0.8)
      if(op > 1) op <- 1
      sprintf("<span class='hl-base hl-%s' style='--opacity:%s'>%s</span>", grp, round(op, 2), html_escape(decoded))
    }, character(1), USE.NAMES = FALSE)
  })
  
  return(unname(processed_text))
}

#' Render Blended Document (HTML)
render_blended_document <- function(full_text, doc_tokens, highlight_groups = list()) {
  
  require(stringi)
  
  # 1. Map Terms to Metadata
  term_meta_map <- list()
  
  for (grp_name in names(highlight_groups)) {
    
    raw_group <- highlight_groups[[grp_name]]
    if (is.null(raw_group) || length(raw_group) == 0) next
    
    if (!is.null(names(raw_group))) {
      terms <- names(raw_group)
      scores <- as.numeric(raw_group)
    } else {
      terms <- as.character(raw_group)
      scores <- rep(1.0, length(terms))
    }
    
    # --- SCALING LOGIC ---
    min_s <- min(scores, na.rm = TRUE)
    max_s <- max(scores, na.rm = TRUE)
    opacities <- numeric(length(scores))
    
    # EXPLICIT MODE SELECTION
    # We check the group name directly to decide logic
    is_entropy_group <- (grp_name == "bursty")
    
    if (is_entropy_group) {
      # MODE A: Inverted Logic (For Entropy/Noise)
      # High Score (1.0) = High Noise = Low Opacity
      # Low Score (0.0) = High Info = High Opacity
      
      range_s <- max_s - min_s
      if(range_s == 0) {
        opacities <- rep(1.0, length(scores))
      } else {
        # Formula: (Max - Value) / Range
        # If Value = Max (e.g. "the"), Ratio = 0.0 -> Opacity 0.25
        # If Value = Min (e.g. rare), Ratio = 1.0 -> Opacity 1.00
        ratio <- (max_s - scores) / range_s
        opacities <- 0.25 + (0.75 * ratio)
      }
      
    } else {
      # MODE B: Standard Logic (Bridge, Cluster, Query)
      # High Score = High Importance = High Opacity
      
      range_s <- max_s - min_s
      if(range_s == 0) {
        opacities <- rep(1.0, length(scores))
      } else {
        # Formula: (Value - Min) / Range
        opacities <- 0.4 + (0.6 * ((scores - min_s) / range_s))
      }
    }
    
    for(i in seq_along(terms)) {
      t <- terms[i]
      op <- opacities[i]
      raw_val <- scores[i]
      
      if (is.na(t) || t == "") next
      t_key <- tolower(trimws(t))
      
      if (is.null(term_meta_map[[t_key]])) {
        term_meta_map[[t_key]] <- list(classes = character(0), max_opacity = 0, force_dim = FALSE)
      }
      
      term_meta_map[[t_key]]$classes <- c(term_meta_map[[t_key]]$classes, paste0("hl-", grp_name))
      term_meta_map[[t_key]]$max_opacity <- max(term_meta_map[[t_key]]$max_opacity, op)
      
      # VETO LOGIC:
      # If this is the Entropy group and the term has a High Score (e.g. > 0.8),
      # we flag it to force it dim, even if the Bridge layer likes it.
      if (is_entropy_group) {
        # Using a threshold (e.g., top 20% of noise) to trigger the veto
        # Since 'op' is already inverted, Low Opacity means High Noise.
        if (op < 0.35) {
          term_meta_map[[t_key]]$force_dim <- TRUE
        }
      }
    }
  }
  
  # 2. Split Raw Text
  paras <- stringi::stri_split_lines(full_text)[[1]]
  
  # 3. Process Paragraphs
  html_paras <- vapply(unique(doc_tokens$para_id), function(pid) {
    
    if (pid > length(paras) || pid < 1) return("") 
    
    p_tokens <- doc_tokens[doc_tokens$para_id == pid, ]
    p_text <- paras[pid]
    
    if(nrow(p_tokens) == 0) {
      return(paste0("<p class='doc-para' data-pid='", pid, "'>", p_text, "</p>"))
    }
    
    buffer <- character(0)
    last_idx <- 1
    
    for(i in 1:nrow(p_tokens)) {
      t_row <- p_tokens[i, ]
      
      # A. Glue
      if (t_row$char_start > last_idx) {
        start_safe <- last_idx
        end_safe <- min(t_row$char_start - 1, nchar(p_text))
        if (start_safe <= end_safe) {
          buffer <- c(buffer, substr(p_text, start_safe, end_safe))
        }
      }
      
      # B. Token
      raw_word <- ""
      if (t_row$char_start <= nchar(p_text)) {
        raw_word <- substr(p_text, t_row$char_start, min(t_row$char_end, nchar(p_text)))
      }
      
      # C. Classes & Intensity
      classes <- c("tok")
      style_str <- ""
      
      term_key <- tolower(trimws(as.character(t_row$term)))
      
      if (!is.na(term_key) && length(term_key) > 0) {
        meta <- term_meta_map[[term_key]]
        
        if (!is.null(meta)) {
          classes <- c(classes, meta$classes)
          
          # APPLY VETO
          final_opacity <- meta$max_opacity
          if (isTRUE(meta$force_dim)) {
            final_opacity <- 0.25
          }
          
          style_str <- sprintf("style='opacity: %.2f;'", final_opacity)
        }
      }
      
      # D. Span
      span <- sprintf("<span id='p%d-t%d' class='%s' %s>%s</span>", 
                      pid, t_row$token_id, paste(classes, collapse=" "), style_str, raw_word)
      buffer <- c(buffer, span)
      
      last_idx <- t_row$char_end + 1
    }
    
    if (last_idx <= nchar(p_text)) {
      buffer <- c(buffer, substr(p_text, last_idx, nchar(p_text)))
    }
    
    paste0("<p class='doc-para' data-pid='", pid, "'>", paste(buffer, collapse=""), "</p>")
    
  }, character(1))
  
  return(paste(html_paras, collapse="\n"))
}