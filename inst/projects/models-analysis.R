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

result %>% nrow()

library(ggplot2)

ggplot(result) + geom_boxplot(aes(Models, Accuracy, color = FeatureAlgo)) + 
  facet_wrap(c("hash", "hallmarkGeneSet", "FeatureNumbers"))


sumAcc <- result %>%
  group_by(Models, FeatureAlgo, FeatureNumbers, hash, hallmarkGeneSet) %>%
  summarise(
    MEAN = mean(Accuracy),
    Q05  = quantile(Accuracy, 0.05),
    Q95  = quantile(Accuracy, 0.05),
    MIN  = min(Accuracy),
    MAX  = max(Accuracy)
  ) %>% ungroup()

res <- sumAcc %>% mutate(FeatureNumbers = as.numeric(as.factor(FeatureNumbers)))

ggplot(res) + 
  geom_line(aes(FeatureNumbers, Q05, color = FeatureAlgo)) + 
  facet_wrap(c("hash", "hallmarkGeneSet"))




p
library(plotly)
ggplotly(p)


#
#unlink(file.path("~/ml-simulation/", setdiff(basename(allModels), basename(allPaths))))
