library(dplyr)
allModels <- dir("~/ml-simulation/", full.names = TRUE)


readMetrics <- function(path) {
  res <- readRDS(path)
  
  metrics <- res$performanceData %>%
    select(metric, value) %>%
    tidyr::pivot_wider(names_from = metric, values_from = value)
  
  
  bind_cols(res$params, metrics)
  
}

result <- purrr::map_dfr(allModels, readMetrics)

library(ggplot2)

ggplot(result) + geom_boxplot(aes(Models, Accuracy, color = FeatureAlgo)) + 
  facet_wrap(c("hash", "hallmarkGeneSet"))
