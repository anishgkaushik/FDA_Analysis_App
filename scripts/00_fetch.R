
# All of the raw FDA data comes in at the very first step, in scripts/00_fetch.R.
# The endpoint contains the URL for openFDA dataset

library(httr)
library(jsonlite)
library(tidyverse)


fetch_fda_data <- function(cfg) {
  dir.create(dirname(cfg$paths$raw_data), recursive=TRUE, showWarnings=FALSE)
  resp <- httr::GET(cfg$fda$endpoint, query = list(limit = cfg$fda$limit))
  httr::stop_for_status(resp)
  json <- httr::content(resp, "text", encoding="UTF-8")
  writeLines(json, cfg$paths$raw_data)
  tibble::as_tibble(jsonlite::fromJSON(json, flatten=TRUE)$results)
}
