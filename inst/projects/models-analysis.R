library(dplyr)
allModels <- dir("~/ml-simulation/", full.names = TRUE)

message("Number of models: ", length(allModels))

readMetrics <- function(path) {
  res <- readRDS(path)
  if(inherits(res, "try-error")) return(NULL)
  if(inherits(res$performanceData, "try-error")) return(NULL)
  metrics <- res$performanceData %>%
    select(metric, value) %>%
    tidyr::pivot_wider(names_from = metric, values_from = value)
  
  
  bind_cols(res$params, metrics)
  
}

result <- purrr::map_dfr(allModels, readMetrics)

saveRDS(result, "inst/projects/simulation-result-v2-2021-07-19.rds")
