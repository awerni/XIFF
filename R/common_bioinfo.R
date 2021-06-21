#' @export
differentialBayesCommon <- function(sampleClasses, dbDataFun, idCol, scoreCol, 
                                    progressLabel, itemLabel, p = FALSE){
  progress <- ProcessProgress$new(progressLabel, p)
  progress$update(0.2, paste("fetching", itemLabel, "data..."))
  
  data <- prepareDataAndDesignCommon(
    sampleClasses = sampleClasses,
    dbDataFun = dbDataFun,
    idCol = idCol,
    scoreCol = scoreCol
  )
  if (is.character(data)) progress$error(data)
  
  progress$update(0.7, "fitting linear model...")
  
  fit <- limma::lmFit(data$data, data$design)
  fit <- limma::eBayes(fit)
  tt <-  limma::topTable(fit, number = 1e9, coef = ncol(data$design))
  res <- tt %>% tibble::rownames_to_column(idCol)
  
  progress$update(1.0, "job done")
  
  res %>% 
    mutate(higher = ifelse(logFC < 0, "class1", "class2")) %>%
    renameClassIdToLabel("higher", sampleClasses) %>%
    relocate(higher, .after = 1) %>%
    rename(adj.p.val = adj.P.Val) %>% 
    arrange(P.Value, adj.p.val)
}

#' @export
prepareDataAndDesignCommon <- function(sampleClasses, dbDataFun, idCol, 
                                       scoreCol = "score"){
  class1 <- sampleClasses$class1
  class2 <- sampleClasses$class2
  
  column <- getOption("xiff.column")
  column <- rlang::sym(column)
  
  samplenames <- c(class1, class2)
  data <- dbDataFun(samplenames)
  if (nrow(data) <= 2) return("not enough data available")
  
  scoreCol <- rlang::sym(scoreCol)
  dataWide <- data %>%
    tidyr::pivot_wider(names_from = !!column, values_from = !!scoreCol) %>%
    tibble::column_to_rownames(idCol)
  
  avail.samples <- colnames(dataWide)
  class1 <- intersect(class1, avail.samples)
  class2 <- intersect(class2, avail.samples)
  
  if (length(class1) == 0 || length(class2) == 0) {
    label <- getOption("xiff.name")
    return(paste(label, "from both classes need to be available"))
  }
  
  classification <- prepareClassificationTable(class1, class2)
  dataWide <- dataWide[, classification[[column]]]
  design <- model.matrix(~ class, data = classification)
  
  list(
    data = dataWide, 
    design = design
  )
}

#' @export
prepareClassificationTable <- function(class1, class2, addRownames = TRUE){
  sampleNames <- c(class1, class2)
  df <- data.frame(
    samples = sampleNames,
    class = c(
      rep("class1", length(class1)),
      rep("class2", length(class2))
    )
  )
  names(df)[1] <- getOption("xiff.column")
  
  if (addRownames){
    rownames(df) <- sampleNames
  }
  
  df
}

#' @export
reorderByScore <- function(df, orderCol = getOption("xiff.column"), 
                           valueCol = "score", .desc = TRUE){
  if (!is.data.frame(df) || nrow(df) == 0) return()
  
  valueCol <- sym(valueCol)
  df <- df %>% filter(!is.na(!!valueCol))
  
  if (nrow(df) == 0) return()
  orderCol <- sym(orderCol)
  
  df %>% mutate(
    !!orderCol := forcats::fct_reorder(!!orderCol, !!valueCol, .desc = .desc)
  )
}

#' @export
ensureCommonRownames <- function(m1, m2, sortRownames = FALSE, outNames = NULL){
  r1 <- rownames(m1)
  r2 <- rownames(m2)
  
  if (!identical(r1, r2)){
    rCommon <- intersect(r1, r2)
    if (sortRownames){
      rCommon <- sort(rCommon)
    }
    m1 <- m1[rCommon, ]
    m2 <- m2[rCommon, ]
  }
  
  res <- list(m1, m2)
  if (!is.null(outNames)){
    names(res) <- outNames
  }
  res
}

# Machine learning ------------------------------------------------------------
#' @export
splitTrainingValidationSets <- function(assignment, p_validation = 0.2){
  trainingSet <- assignment
  validationSet <- NULL

  if (p_validation > 0){
    n <- nrow(assignment)
    # full random selection; no stratification included
    validationIdx <- sort(sample(x = n, size = p_validation * n))
    validationSet <- assignment[validationIdx, ]
    trainingSet <- assignment[-validationIdx, ]
  }

  list(
    training = trainingSet,
    validation = validationSet
  )
}

#' @export
getRawDataForModel <- function(features, celllinenames = NULL){
  clFilter <- if (length(celllinenames) > 0){
    paste(" AND", getSQL_filter("celllinename", celllinenames))
  }

  sql <- paste0("SELECT celllinename, ensg, log2tpm AS score FROM cellline.processedrnaseqview ",
                "WHERE ", getSQL_filter("ensg", features), clFilter)
  getPostgresql(sql)
}

#' @export
getDataForModel <- function(assignment, features){
  getRawDataForModel(
    features = features,
    celllinenames =  assignment$celllinename
  ) %>%
    tidyr::pivot_wider(names_from = ensg, values_from = score) %>%
    left_join(assignment, by = "celllinename")
}


#' @export
selectBestFeatures <- function(df, threshold = "Confirmed", maxFeatures = Inf) {
  
  if(is.numeric(threshold)) { 
    warning("Opps! You've send the threshold as numeric value! Please use 'Confirmend' (default) or 'Tentative'.")
    threshold <- "Confirmed" 
  } # if CLIFF sends a numeric value
  
  selectBestFeaturesBoruta(df, threshold, maxFeatures)
}

#' @export
selectBestFeaturesTTest <- function(df, threshold = 0.05, maxFeatures = Inf){
  ttRes <- genefilter::colttests(
    x = df %>% select(-class) %>% as.matrix(),
    fac = df$class,
    na.rm = TRUE
  ) %>%
    tibble::rownames_to_column("ensg") %>%
    filter(p.value <= threshold) %>%
    arrange(p.value) %>%
    head(maxFeatures)
  
  bestFeatures <- ttRes %>% pull(ensg)
  df <- df %>% select_at(c("class", bestFeatures))
  
  list(
    stats = ttRes,
    df = df,
    method = "genefilter::colttests"
  )
}


#' Use Bortua algorithm for feature selection.
#'
#' @param df 
#' @param threshold 
#' @param maxFeatures maximal number of features to be returned.
#' @param threads number of threads to be used by Boruta algorithm.
#' 
#' 
#' @return
#' @export
#'
#' @examples
#' 
#' data("feature_selection_data", package = "XIFF")
#' featureFit <- selectBestFeaturesBoruta(feature_selection_data)
#' plot(featureFit$fit, las = 1, horizontal = TRUE)
#' 
selectBestFeaturesBoruta <- function(df, threshold = c("Confirmed", "Tentative"), maxFeatures = Inf, threads = getOption("xiff.boruta.threads", 2)) {
  
  threshold <- match.arg(threshold, c("Confirmed", "Tentative"))
  if(threshold == "Tentative") threshold <- c("Confirmed", "Tentative")
  if(maxFeatures < 1) {
    warning("XIFF:::selectBestFeaturesBoruta - maxFeatures should not be lower than 1. The function will return all the features.")
    maxFeatures <- Inf
  }
  
  fit <- Boruta::Boruta(class ~ ., df, num.threads = threads)
  
  fit$finalDecision <- fit$finalDecision[fit$finalDecision %in% threshold]
  fit$ImpHistory <- fit$ImpHistory[,c(names(fit$finalDecision), "shadowMax", "shadowMean", "shadowMin")]
  
  pvals <- apply(fit$ImpHistory[,names(fit$finalDecision)], 2, function(x) t.test(x, fit$ImpHistory[,"shadowMax"])$p.value) * (ncol(df) - 1)
  
  stats <- tibble(
    ensg = colnames(fit$ImpHistory[,names(fit$finalDecision)])
    , p.value = pvals
    , decision = fit$finalDecision 
  )
  
  stats <- stats[order(stats$p.value),]
  
  # Select the values based on maxFeatures
  stats <- head(stats, maxFeatures)
  fit$finalDecision <- fit$finalDecision[stats$ensg]
  fit$ImpHistory <- fit$ImpHistory[,c(stats$ensg, "shadowMax", "shadowMean", "shadowMin")]
  
  
  df <- df[, c(stats$ensg, "class")]
  list(
    fit = fit
    , stats = stats
    , df = df
    , method = "Boruta::Boruta"
  )
  
}

#' Train Machine Learning Models.
#' 
#' @param df 
#' @param method 
#' @param tuneLength 
#' @param number 
#' @param repeats 
#'
#' @export
#' 
#' @examples 
#' 
#' if(require("xiffModels")) {
#'   data("train_model_data", package = "xiffModels")
#'   train_model_data <- train_model_data[,-1]
#' 
#'   fitRF  <- trainModel(train_model_data, method = "rf", tuneLength = 1)
#'   fitSVM <- trainModel(train_model_data, method = "svmLinear2", tuneLength = 1)
#'   fitNeuralnet <- trainModel(train_model_data, method = "neuralnetwork")
#' }
#' 
#' 
trainModel <- function(df, method = "rf", tuneLength = 5, number = 10, repeats = 10){
  
  
  if(method == "neuralnetwork") {
    stopifnot(require("xiffModels"))
    method <- xiffModels::modelInfoNeuralNetwork()
  } 
  
    fitControl <- caret::trainControl(
      method = "repeatedcv",
      number = number,
      repeats = repeats,
      savePredictions = "final"
    )
    
    args <- list(
      as.formula("class ~ ."),
      data = df,
      method = method,
      trControl = fitControl,
      tuneLength = tuneLength
    )
    
    if (is.character(method) && method == "rf"){
      args[["ntree"]] <- 501 # odd number to make sure there won't be 50:50 votes
    }
  
  
  do.call(caret::train, args)
}

#' @export
#' 
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr rename arrange select
#' 
getVarImp <- function(model, stats){
  
  switch(
    EXPR = class(model),
    randomForest = list(
      df = varImp(model) %>%
        tibble::rownames_to_column("ensg") %>%
        dplyr::rename(importance = Overall) %>%
        dplyr::arrange(desc(importance)),
      importanceName = "Mean decrease Gini"
    ),
    svm = list(
      df = stats %>%
        dplyr::select(ensg, importance = p.value) %>%
        dplyr::arrange(importance),
      importanceName = "p.val"
    ),
    nn = list(
      df = xiffModels::modelInfoNeuralNetwork()$varImp(model) %>%
        tibble::rownames_to_column("ensg") %>%
        dplyr::arrange(desc(abs(importance)))
      , importanceName = "olden"
    ),
    stop(glue::glue("Variable imortance for {class(model)} not supported."))
  )
}

#' @export
#' 
#' @examples 
#' 
#' data("data_createMachineLearningModel", package = "XIFF")
#' trainingSet <- data_createMachineLearningModel$trainingSet
#' geneSet     <- data_createMachineLearningModel$geneSet
#' geneAnno    <- data_createMachineLearningModel$geneAnno
#' 
#' fit <- createMachineLearningModel(trainingSet, geneSet, geneAnno)
#' fitNN <- createMachineLearningModel(trainingSet, geneSet, geneAnno, method = "neuralnetwork")
#' 
createMachineLearningModel <- function(trainingSet, geneSet, geneAnno, p = FALSE,
                                       classLabel = list(class1_name = "class1", class2_name = "class2"),
                                       method = "rf", tuneLength = 5, number = 10, repeats = 10
                                       , selectBestFeaturesFnc = selectBestFeatures, threshold = "Confirmed" 
                                       , maxFeatures = "auto"
                                       , ...){
  progress <- ProcessProgress$new("Create ML model", p)
  progress$update(0.2, "fetching data...")

  df <- getDataForModel(
    assignment = trainingSet,
    features = geneSet
  ) %>%
    select(-celllinename)

  if (nrow(df) == 0) progress$error("No data available")

  # Feature selection
  progress$update(0.3, "selecting features...")

  if(maxFeatures == "auto") {
    # selecting custom number of features for 'neuralnetwork' - note that other methods are not 
    # constrained here. But if you want to make such constrain that is going to be a default for the users,
    # please do that here.
    maxFeatures <- case_when(
      method == "neuralnetwork" ~ 5
      , TRUE ~ Inf
    )
  } else if(!is.numeric(maxFeatures)) {
    stop("XIFF::createMachineLearningModel - maxFeatures must be a numeric value greater than zero or 'auto' string.")
  }
  selectedFeatures <- selectBestFeaturesFnc(
    df = df
    , threshold = threshold
    , maxFeatures = maxFeatures
    , ...
  )
  stats <- selectedFeatures$stats
  df <- selectedFeatures$df

  bestFeatures <- stats %>% pull(ensg)

  if (length(bestFeatures) == 0){
    progress$error(paste("no significant features found for threshold =", threshold))
  }

  progress$update(0.5, "training model...")

  trainingOutput <- trainModel(
    df = df,
    method = method,
    tuneLength = tuneLength,
    number = number,
    repeats = repeats
  )

  importanceRes <- getVarImp(trainingOutput$finalModel, stats)
  df <- importanceRes$df %>% left_join(geneAnno, by = "ensg")
  attr(df, "importanceName") <- importanceRes$importanceName

  progress$update(1.0, "job done")

  list(
    featureSelectionResult = selectedFeatures,
    model = machineLearningResult(
      res = list(
        trainingOutput = trainingOutput,
        df = df,
        trainingSet = trainingSet$celllinename
      ),
      classLabel = classLabel
    ),
    df = df,
    trainingOutput = trainingOutput
  )
}

#' @export
machineLearningResult <- function(res, classLabel){
  output <- structure(
    list(
      model = res$trainingOutput$finalModel,
      library = res$trainingOutput$modelInfo$library,
      classLabel = classLabel,
      bestFeatures = res$df[["ensg"]],
      trainingSet = res$trainingSet
    ),
    class = "machineLearningResult"
  )
}

#' @export
`newClassLabel<-` <- function(x, value){
  x$classLabel <- value
  x
}

#' Function that reutrns the proper prediction function for given model
.getPredictFunction <- function(model, library) {
  
  if (!packageInstalled(library)){
    stop(paste0("Package '", library, "' is not available."))
  }
  
  UseMethod(".getPredictFunction")
}

.getPredictFunction.default <- function(model, library) {
  
  modelClass <- class(model)
  predFun <- getS3method(
    f = "predict",
    class = modelClass,
    envir = asNamespace(library),
    optional = TRUE
  )
  
  if (is.null(predFun)){
    stop(paste0("No predict() method found for ", modelClass, "-class object"))
  }
  
  return(predFun)
}

.getPredictFunction.nn <- function(model, library) {
  
  if (!packageInstalled("xiffModels")){
    stop(paste0("Package '", "xiffModels", "' is not available."))
  }
  
  return(xiffModels::modelInfoNeuralNetwork()$predict)
}

#' @export
loadMachineLearningModel <- function(filepath, object = NULL){
  x <- if (is.null(object)){
    readRDS(filepath)
  } else {
    object
  }

  if (!is(x, "machineLearningResult")){
    stop("Incorrect input file. Please provide a file downloaded from the machine learning tab")
  }
  
  x[[".predFun"]] <- .getPredictFunction(x$model, x$library)
  class(x) <- "machineLearningResultReady"
  x
}

#' @export
predictFromModel <- function(x, df){
  if (!is(x, "machineLearningResultReady")){
    stop("Incorrect input object. Please provide an object returned by loadMachineLearningModel function")
  }

  predFun <- x[[".predFun"]]

  missingFeatures <- setdiff(x$bestFeatures, names(df))
  if (length(missingFeatures) > 0){
    stop(paste("Missing features:", paste(missingFeatures, collapse = ", ")))
  }

  df <- preparePredictionData(df)
  assignment <- try(predFun(x$model, df))
  if (is(assignment, "try-error")){
    stop("Error during prediction. Please contact the app author.")
  }

  if (is.list(assignment)){
    assignment <- unlist(assignment, use.names = FALSE)
  }
  assignment <- as.character(assignment)

  if (length(assignment) == 0 || any(! assignment %in% c("class1", "class2", NA))){
    stop("Incorrect prediction format. Did you use a custom model? Please contact the app author.")
  }

  assignment
}

preparePredictionData <- function(df){
  colname <- getOption("xiff.column")

  if (colname %in% names(df)){
    attr(df, "mlItems") <- df[[colname]]
    df[[colname]] <- NULL
  }

  df
}

# Unbalanced tumortypes -------------------------------------------------------
#' @export
dropUnbalancedTumortypes <- function(AnnotationFocus, classSelection){
  anno <- AnnotationFocus()
  if (is.null(anno) || nrow(anno) == 0) return()

  cs <- reactiveValuesToList(classSelection)
  balancedTT <- getValidTumorTypes(cs, anno)

  colname <- getOption("xiff.column")
  colname <- rlang::sym(colname)

  validItems <- anno %>%
    filter(tumortype %in% balancedTT) %>%
    pull(!!colname)

  classSelection$class1 <- intersect(classSelection$class1, validItems)
  classSelection$class2 <- intersect(classSelection$class2, validItems)
}

#' Get valid tumor types
#'
#' This function returns tumor types that are available in the data for both classes
#'
#' @param cs list, class selection
#' @param anno data.frame, item annotation
#'
#' @return character vector of valid tumortypes
#' @export
getValidTumorTypes <- function(cs, anno){
  if (is.null(anno) || nrow(anno) == 0) return()

  colname <- getOption("xiff.column")

  stackClasses(cs) %>%
    left_join(anno, by = colname) %>%
    group_by(class, tumortype) %>%
    summarise(n = dplyr::n(), .groups = "drop") %>%
    group_by(tumortype) %>%
    summarize(ok = dplyr::n() > 1, .groups = "drop") %>% # must be present in both classes
    filter(ok) %>%
    pull(tumortype) %>%
    as.character()
}

#' @export
getClassDistances <- function(mat, pheno, metric = "euclidean"){
  cv <- pheno[colnames(mat), "class"] # make sure the order is correct
  
  dn <- combn(
    x = ncol(mat), # get all pairs in the same way as dist() does this
    m = 2,
    FUN = function(x){
      cv[x[1]] != cv[x[2]] # look for class1 vs class2 pairs
    }
  )
  
  d <- dist(t(mat), method = metric) # calculate distance
  d[dn] <- max(d) + 1 # introduce maximal available distance to separate classes
  d
}

#' @export
scaleRows <- function(x){
  m <- apply(x, 1, mean, na.rm = TRUE)
  s <- apply(x, 1, sd, na.rm = TRUE)
  return((x - m)/s)
}
