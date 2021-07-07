# SECRET FILE

library(XIFF)
library(digest)
library(dplyr)

WORKERS <- 12
N_ITERATIONS <-  50
OUTPUT_PATH <- "~/ml-simulation"


initOutputDirectory <- function(path  = "~/ml-simulation") {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  invisible(path)
}

initOutputDirectory(OUTPUT_PATH)


XIFF::setDbOptions()

getData <- function(hash, hallmarkGeneSet = "P53_PATHWAY") {
  cs <- getStashedData(hash)
  
  if(hash == "0766b1") {
    cs <- list(
      class1 = cs %>% filter(MDM2_class == "sensitive") %>% pull,
      class2 = cs %>% filter(MDM2_class == "resistant") %>% pull
    )
    
  } else if(hash == "81c5cc") {
    cs <- list(
      class1 = cs %>% filter(E2F_class == "low_score") %>% pull,
      class2 = cs %>% filter(E2F_class == "high_score") %>% pull
    )
  } else {
    stop("Hash not supported.")
  }
  
  geneSet <- CLIFF::getGSEAdata("human", "hallmark")[[paste0("HALLMARK_", hallmarkGeneSet)]]
  geneAnno <- CLIFF::getGeneAnno()[["human"]]
  
  
  annoFocus <- CLIFF::getCellLineAnno("human") %>% dplyr::filter(celllinename %in% unlist(cs))
  
  
  tibble(
    cs = list(cs),
    geneSet = list(geneSet),
    geneAnno = list(geneAnno),
    hash = hash,
    hallmarkGeneSet = hallmarkGeneSet,
    annoFocus = list(annoFocus)
  )
}

allData <- bind_rows(
  expand.grid(hash = c("0766b1"), geneSet = c("P53_PATHWAY", "APOPTOSIS")),
  expand.grid(hash = c("81c5cc"), geneSet = c("EPITHELIAL_MESENCHYMAL_TRANSITION", "E2F_TARGETS", "G2M_CHECKPOINT"))
)

allDataList <- mapply(getData, allData$hash, allData$geneSet, SIMPLIFY = FALSE)
allData <- bind_rows(allDataList)

featureSelectionModels <- c("TTest", "BorutaTentative", "BorutaConfirmed")
featureSelectionNumbnerOfFeatures <- c(5, 50, Inf)
featuresCombinations <- expand.grid(
  FeatureAlgo = featureSelectionModels,
  FeatureNumbers = featureSelectionNumbnerOfFeatures
)


modelsWithFeatures <- dplyr::full_join(tibble(Models = unique(c(xiffSupportedModels()), "glmnet")), featuresCombinations, by = character())



allCombs <- full_join(modelsWithFeatures, allData, by = character())

generateSeeds <- function(n) {
  
  x <- rep(letters, n %/% length(letters) + 1)
  x <- paste0(head(x, n), 1:n)
  digest::digest2int(x)
}


seeds <- generateSeeds(N_ITERATIONS)
seeds <- tibble(N = 1:length(seeds), RandomSeed = seeds)

allSimulations <- full_join(allCombs, seeds, by = character())




makeModel <- function(i, allSimulations, OUTPUT_PATH) {
  
  try({
    library(XIFF)
    library(dplyr)
    XIFF::setDbOptions()
    
    params <- allSimulations[i,]
    set.seed(params$RandomSeed)
    
    
    # extract data
    cs <- params$cs[[1]]
    geneSet <- params$geneSet[[1]]
    geneAnno <- params$geneAnno[[1]]
    annoFocus <- params$annoFocus[[1]]
    
    # check file name
    params <- params %>% select(-cs, -geneSet, -geneAnno, -annoFocus)
    
    filePath <- file.path(OUTPUT_PATH, paste0(digest::digest(params), ".rds"))
    if(file.exists(filePath)) return(NULL)
    
    
    # prepare   
    if(params$FeatureAlgo == "TTest") {
      featureFnc <- XIFF::selectBestFeaturesTTest
      featureThreshold  <- 0.05
    } else if(params$FeatureAlgo == "BorutaTentative") {
      featureThreshold <- "Tentative"
      featureFnc <- XIFF::selectBestFeaturesBoruta
    } else if(params$FeatureAlgo == "BorutaConfirmed") {
      featureThreshold <- "Confirmed"
      featureFnc <- XIFF::selectBestFeaturesBoruta
    } else {
      stop("Not supported")
    }
    
    
    time <- system.time(model <- try(XIFF::buildMachineLearning(
      cs = cs,
      ensg_gene_set = geneSet,
      gene_anno = geneAnno,
      method = params$Models,
      maxFeatures = params$FeatureNumbers,
      threshold = featureThreshold,
      selectBestFeaturesFnc = featureFnc
    )))
    
    
    validation <- try(validateModel(
      model,
      validationSet = stackClasses(model$validationSet),
      anno = annoFocus
    ))
    
    performanceData <- try(
      getPerformanceDataFrame(validation$res$table)
    )
    
    
    allResult <- list(
      params = params,
      model = model,
      time = time,
      validation = validation,
      performanceData = performanceData
    )
    
    
    saveRDS(allResult, file = filePath)
  })
  
 
  
}

library(parallel)
x <- parallel::mclapply(X = 1:nrow(allSimulations), FUN = makeModel,
              allSimulations = allSimulations,
              OUTPUT_PATH = OUTPUT_PATH, mc.cores = WORKERS)
