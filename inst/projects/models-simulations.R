# SECRET FILE
.libPaths("~/R/fixedXIFF")
library(XIFF)
library(digest)
library(dplyr)
library(BIAutils)
library(BIClass)

WORKERS <- 16
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
  } else if(hash %in% c("2ef471", "eb1e71")) {
    cs <- list(
      class1 = cs %>% filter(class == "class1") %>% pull,
      class2 = cs %>% filter(class == "class2") %>% pull
    )
  } else if(hash == "06bd26") {
    cs <- list(
      class1 = cs %>% filter(consensus_sensitivity == "Resistant") %>% pull(celllinename),
      class2 = cs %>% filter(consensus_sensitivity == "Sensitive") %>% pull(celllinename)
    )
    
  } else {
    stop("Hash not supported.")
  }
  
  if(hallmarkGeneSet == "WP_APOPTOSIS_MODULATION_AND_SIGNALING") {
    geneSet <- CLIFF::getGSEAdata("human","mSigDB")[[hallmarkGeneSet]]
  } else {
    geneSet <- CLIFF::getGSEAdata("human", "hallmark")[[paste0("HALLMARK_", hallmarkGeneSet)]]
  }
  
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
  expand.grid(hash = c("0766b1"), geneSet = c("P53_PATHWAY")),
  expand.grid(hash = c("06bd26"), geneSet = c("WP_APOPTOSIS_MODULATION_AND_SIGNALING")),
  expand.grid(hash = c("2ef471"), geneSet = c("KRAS_SIGNALING_UP")),
  expand.grid(hash = c("eb1e71"), geneSet = c("KRAS_SIGNALING_UP"))
)

allData <- allData %>% mutate_all(as.character)

allDataList <- mapply(getData, allData$hash, allData$geneSet, SIMPLIFY = FALSE)
allData     <- bind_rows(allDataList)

################## Feature selection models ##############
featureSelectionModels <- c("TTest", "PrefilterAffinity", "AffinityPostfilter", "GLMNET", "Ratio")
featureSelectionFunctions  <- tibble::tibble(
  FeatureAlgo = featureSelectionModels,
  FeatureAlgoFunction = c(
    XIFF::selectBestFeaturesTTest,
    XIFF::selectBestFeaturesAffinityPrefilter,
    XIFF::selectBestFeaturesAffinityPostfilter,
    XIFF::selectBestFeaturesGlmnet,
    function(df, threshold = 0.05, maxFeatures = Inf, minFeatures = 15) {
      
      dfSelected <- get_representative_features(
        df, "class", pmin(minFeatures, ncol(df) * 0.8))
      
      dfSelected <- dfSelected %>% rename(class = classification)
      res <- XIFF::selectBestFeaturesTTest(dfSelected, threshold = 1, maxFeatures = Inf)
      
      res$method <- "get_representative_features"
      res  
    }
    
  ),
  FeatureSelectionThreshold = list(0.05,0.05,0.05,"auto",1)
)

featureSelectionNumbnerOfFeatures <- c(5, 25, 50, Inf)
featuresCombinations <- expand.grid(
  FeatureAlgo = featureSelectionModels,
  FeatureNumbers = featureSelectionNumbnerOfFeatures
)
featuresCombinations <- inner_join(as_tibble(featuresCombinations), featureSelectionFunctions)

################# ML models ################# 
models <- unique(c(xiffSupportedModels(), "glmnet", "glm"))
if(any(models == "neuralnetwork")) {
  models <- models[models != "neuralnetwork"]
  models <- c(models, "nn", "nn-scaled")
}

modelsWithFeatures <- dplyr::full_join(tibble(Models = models), featuresCombinations, by = character())

allCombs <- full_join(modelsWithFeatures, allData, by = character())

generateSeeds <- function(n) {
  
  x <- rep(letters, n %/% length(letters) + 1)
  x <- paste0(head(x, n), 1:n)
  digest::digest2int(x)
}


seeds <- generateSeeds(N_ITERATIONS)
seeds <- tibble(N = 1:length(seeds), RandomSeed = seeds)

allSimulations <- full_join(allCombs, seeds, by = character())
allSimulations <- allSimulations %>% mutate_if(is.factor, as.character)

makeFilePath <- function(params, OUTPUT_PATH) {
  
  hash <- digest(params)
  file.path(OUTPUT_PATH, paste0(digest::digest(params), ".rds"))
}

standardSimulations <- allSimulations %>% filter(Models != "glm" & FeatureAlgo != "Ratio")
ratioSimulations <- allSimulations %>% filter(FeatureAlgo == "Ratio", FeatureNumbers == max(FeatureNumbers))

allSimulations <- bind_rows(standardSimulations, ratioSimulations)

makeModel <- function(i, allSimulations, OUTPUT_PATH) {
  
  try({
    
    .libPaths("~/R/fixedXIFF")
    library(XIFF)
    library(dplyr)
    library(BIAutils)
    library(BIClass)
    XIFF::setDbOptions()
    
    params <- allSimulations[i,]
    set.seed(params$RandomSeed)
    
    
    # extract data
    cs <- params$cs[[1]]
    geneSet <- params$geneSet[[1]]
    geneAnno <- params$geneAnno[[1]]
    annoFocus <- params$annoFocus[[1]]
    featureAlgoFunction <- params$FeatureAlgoFunction[[1]]
    featureSelectionThreshold <- params$FeatureSelectionThreshold[[1]]
    
    # check file name
    params <- params %>% select(
      -cs,
      -geneSet,
      -geneAnno,
      -annoFocus,
      -FeatureAlgoFunction,
      -FeatureSelectionThreshold
    )
    filePath <- makeFilePath(params, OUTPUT_PATH)
    
    if(file.exists(filePath)) return(NULL)
    
    getBestFeaturesFnc <- function(x) x
    transformFeaturesFnc <- function(x, model) x
    getDataForModelFnc <- XIFF::getDataForModel
    
    if(params$FeatureAlgo == "Ratio") {
      
      getBestFeaturesFnc <- function(x) {
        unique(strsplit(x, split = "\\.") %>% unlist)
      }
      
      
      transformFeaturesFnc <- function(x, model) {
        
        dt2 <- x %>% mutate_if(is.numeric, function(x) 2^x)
        
        expr_RNAseq2 <- dt2 %>% select(-!!rlang::sym(getOption("xiff.column")))
        epsilon_RNAseq <- 5
        
        quotientMatrix <- get_log2_ratios(expr_RNAseq2, class_col = "class", epsilon_RNAseq)
        
        if(!all(model$bestFeatures %in%colnames(quotientMatrix))) {
          stop("Some ratios not available")
        }
        
        features <- as_tibble(as.data.frame(quotientMatrix[,model$bestFeatures]))
        
        res <- dt2 %>% select(class, !!rlang::sym(getOption("xiff.column")))
        bind_cols(res, features)  
      }
      
      ml_get_significant_features <- function (df, class_col = "classification", fdr = 0.05, max_n = 500) 
      {
        class_col <- sym(class_col)
        df <- select(df, !!class_col, dplyr::everything())
        data <- BIClass::class_feature_split(df)
        stats <- BIClass::calc_significant_features(data$features, data$outcome)
        stats <- stats %>% filter(p_adj <= fdr) %>% arrange(p_adj) %>% head(max_n)
        
        bind_cols(
          tibble(!!class_col := data$outcome),
          as.data.frame(data$features[, stats$feature])
        )
      }
      
      getDataRatios <- function(assignment, features, maxN = 500) {
        
        dt <- getDataForModel(assignment, features)
        dt2 <- dt %>% mutate_if(is.numeric, function(x) 2^x)
        
        expr_RNAseq2 <- dt2 %>% select(-!!rlang::sym(getOption("xiff.column")))
        epsilon_RNAseq <- 5
        
        quotientMatrix <- get_log2_ratios(expr_RNAseq2, class_col = "class", epsilon_RNAseq)
        quotientMatrix <- quotientMatrix %>% mutate(classification = as.factor(classification))
        
        sigQuotientMatrix <- ml_get_significant_features(
          quotientMatrix,
          fdr = 0.01,
          class_col = "classification",
          max_n = maxN)
        
        
        dt <- bind_cols(dt %>% select(!!rlang::sym(getOption("xiff.column"))), sigQuotientMatrix)
        dt <- dt %>% rename(class = classification)
        dt
      }
      
      getDataForModelFnc <- getDataRatios
    }
    
    
    if(params$Models %in% c("nn", "nn-scaled")) {
      
      prepProc <- if(params$Models == "nn-scaled") {
        "scale"
      } else {
        NULL
      }
      
      time <- system.time(model <- try(XIFF::buildMachineLearning(
        cs = cs,
        geneSet = geneSet,
        geneAnno = geneAnno,
        method = "neuralnetwork",
        maxFeatures = params$FeatureNumbers,
        threshold = featureSelectionThreshold,
        selectBestFeaturesFnc = featureAlgoFunction,
        preProcess = prepProc,
        getBestFeaturesFnc = getBestFeaturesFnc,
        transformFeaturesFnc = transformFeaturesFnc,
        getDataForModelFnc = getDataForModelFnc
      )))
    } else {
      time <- system.time(model <- try(XIFF::buildMachineLearning(
        cs = cs,
        geneSet = geneSet,
        geneAnno = geneAnno,
        method = params$Models,
        maxFeatures = params$FeatureNumbers,
        threshold = featureSelectionThreshold,
        selectBestFeaturesFnc = featureAlgoFunction,
        getBestFeaturesFnc = getBestFeaturesFnc,
        transformFeaturesFnc = transformFeaturesFnc,
        getDataForModelFnc = getDataForModelFnc
      )))
    }
    
    test <- try(testModel(
      model,
      testSet = stackClasses(model$testSet),
      anno = annoFocus
    ))
    
    performanceData <- try(
      getPerformanceDataFrame(test$res$table)
    )
    
    
    allResult <- list(
      params = params,
      #model = model,
      time = time,
      test = test,
      performanceData = performanceData
    )
    
    
    saveRDS(allResult, file = filePath)
  })
  
}

tmp <- allSimulations  %>% select(-cs,
                                  -geneSet,
                                  -geneAnno,
                                  -annoFocus,
                                  -FeatureAlgoFunction,
                                  -FeatureSelectionThreshold)

allPaths <- pbapply::pbsapply(1:nrow(allSimulations), function(i) makeFilePath(tmp[i,], OUTPUT_PATH))
allSimulations <- allSimulations[!file.exists(allPaths),]

allSimulations <- allSimulations[sample.int(nrow(allSimulations)),]

message("Models left: ", nrow(allSimulations))

library(parallel)
x <- parallel::mclapply(X = 1:nrow(allSimulations), FUN = makeModel,
              allSimulations = allSimulations,
              OUTPUT_PATH = OUTPUT_PATH, mc.cores = WORKERS)
