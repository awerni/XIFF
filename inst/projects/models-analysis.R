library(dplyr)
allModels <- dir("~/ml-simulation/", full.names = TRUE)

message("Number of models: ", length(allModels))

tbl <- "
|hash   |description                                              |geneSet                               |positive class|
|0766b1 |sensitivity/resistance to MDM2 CRISPR knockout           |P53_PATHWAY                           |sensitive     |
|06bd26 |DR5 = TRAILR2 sensitivity (Reddy et al.)                 |WP_APOPTOSIS_MODULATION_AND_SIGNALING |Sensitive     |
|2ef471 |KRAS sensitivity (class1 = resistant, class2 = sensitive)|KRAS_SIGNALING_UP                     |class2        |
|eb1e71 |Random Dataset                                           |KRAS_SIGNALING_UP                     |class1        |
"

dataTbl <- read.delim(text = tbl, sep = "|") %>% select(hash, hallmarkGeneSet = geneSet, positive.class)

dataTbl <- dataTbl %>% mutate_all(stringi::stri_trim_both)

readMetrics <- function(path) {
  res <- readRDS(path)
  if(inherits(res, "try-error")) return(NULL)
  if(inherits(res$performanceData, "try-error")) return(NULL)
  metrics <- res$performanceData %>%
    select(metric, value) %>%
    tidyr::pivot_wider(names_from = metric, values_from = value)
  
  params <- inner_join(res$params, dataTbl, by = c("hash", "hallmarkGeneSet"))
  
  if(nrow(params) == 0) {
    print(params)
    stop("Cannot find positive class")
  }
  
  data <- res$test
  
  hash <- params$hash
  if(hash == "0766b1") {
    
    positiveClass <- "class1"
    #class1 = cs %>% filter(MDM2_class == "sensitive") %>% pull
    
  } else if(hash == "81c5cc") {
    positiveClass <- "class2"
    #class2 = cs %>% filter(E2F_class == "high_score") %>% pull
  } else if(hash %in% "2ef471") {
    positiveClass <- "class2"
  } else if(hash == "eb1e71") {
    positiveClass <- "class1"
  } else if(hash == "06bd26") {
    positiveClass <- "class2"
  } else {
    stop("Hash not supported.")
  }
  
  # handle case when all predictions are equal to one class
  if(all(data$data$predicted_original == positiveClass)) {
    sensitivity <- 1
    specificity <- 0
  } else if(all(data$data$predicted_original != positiveClass)) {
    sensitivity <- 0
    specificity <- 1
  } else {
    sensitivity <- MLmetrics::Sensitivity(
      y_true = data$data$reference_original,
      y_pred = data$data$predicted_original,
      positive = positiveClass
    )
    specificity <- MLmetrics::Specificity(
      y_true = data$data$reference_original,
      y_pred = data$data$predicted_original,
      positive = positiveClass
    )
  }
  
  metrics$Sensitivity <- coalesce(sensitivity, 0)
  metrics$Specificity <- coalesce(specificity, 0)
  
  bind_cols(res$params, metrics)
  
}

allDtsList <- list()
progress <- progress::progress_bar$new(total = length(allModels),
                                       format = "[:bar] :percent eta: :eta")
for(i in seq_along(allModels)) {
  allDtsList[[i]] <- readMetrics(allModels[i])
  progress$tick()
}

result <- bind_rows(allDtsList)

saveRDS(result, "inst/projects/simulation-result-v2-2021-08-06.rds")
