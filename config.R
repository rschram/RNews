# config.R

# 1. Load the Anchor Package
# If not installed: install.packages("here")
library(here) 

# 2. Project Settings
PROJECT_SEED <- 420
set.seed(PROJECT_SEED)

# 3. Rigorous Path Definitions
# here::here() creates an absolute path from the project root (.Rproj)
# No matter where this script is sourced from, these paths will be correct.

PATH_ROOT <- here::here()
PATH_RAW  <- here::here("data", "raw")
PATH_PROC <- here::here("data", "processed")
PATH_APP  <- here::here("data", "app_ready")
PATH_R    <- here::here("R")

# 4. Helper to load local libraries
# This allows you to source functions like: source(file.path(PATH_R, "graph_utils.R"))
# Or simpler:
source_lib <- function(filename) {
  source(here::here("R", filename))
}

# 5. Global Constants
SIM_THRESHOLD <- 0.30