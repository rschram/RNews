#' Filter Graph for Visualization
#' @param g The full igraph object
#' @param focus_doc_ids Vector of doc IDs matched by search (or NULL for all)
#' @return visNetwork data list
prepare_visual_graph <- function(g, focus_doc_ids = NULL) {
  
  # 1. Convert to VisNetwork Data
  data <- toVisNetworkData(g)
  
  # 2. If NO Search, return standard colored graph
  if (is.null(focus_doc_ids) || length(focus_doc_ids) == 0) {
    return(data)
  }
  
  # 3. If Search Exists, apply "Ghosting"
  # Match IDs
  is_focus <- data$nodes$id %in% focus_doc_ids
  
  # Update Colors
  # Focused nodes keep their group color
  # Unfocused nodes turn gray and semi-transparent
  data$nodes$color.background <- ifelse(is_focus, NA, "#e0e0e0") # NA lets VisNetwork use group color
  data$nodes$color.border <- ifelse(is_focus, "black", "#cccccc")
  data$nodes$opacity <- ifelse(is_focus, 1.0, 0.2)
  
  # Optional: Hide edges connected to ghosts?
  # For now, let's keep structure visible but faint
  
  return(data)
}