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
splitTrainingTestSets <- function(assignment,
                                        p_test = 0.2){
  UseMethod("splitTrainingTestSets")
}

#' @export
splitTrainingTestSets.data.frame <- function(assignment,
                                                   p_test = 0.2){
  trainingSet <- assignment
  testSet <- NULL

  if (p_test > 0){
    n <- nrow(assignment)
    # full random selection; no stratification included
    testIdx <- sort(sample(x = n, size = p_test * n))
    testSet <- assignment[testIdx, ]
    trainingSet <- assignment[-testIdx, ]
  }

  list(
    training = trainingSet,
    test = testSet
  )
}

#' @export
splitTrainingTestSets.classAssignment <- function(assignment,
                                                        p_test = 0.2){
  trainingSet   <- assignment
  testSet <- NULL
  if(p_test > 0) {
    cl    <- getClassLabel(assignment)
    split <- splitTrainingTestSets(stackClasses(assignment))
    
    trainingSet <- XIFF::makeClassAssignment(
      split(split$training[[1]], split$training$class),
      classLabel = cl
    )
    testSet <- XIFF::makeClassAssignment(
      split(split$test[[1]], split$test$class),
      classLabel = cl
    )
  }
  
  list(
    training = trainingSet,
    test = testSet
  )
  
}


#' @export
selectBestFeatures <-
  function(df,
           threshold = "Confirmed",
           maxFeatures = Inf,
           .otherParams = list()) {
    
  
  if(is.numeric(threshold)) { 
    warning("Opps! You've send the threshold as numeric value! Please use 'Confirmend' (default) or 'Tentative'.")
    threshold <- "Confirmed" 
  } # if CLIFF sends a numeric value
  
  selectBestFeaturesBoruta(df, threshold, maxFeatures, .otherParams = .otherParams)
}

#' @export
selectBestFeaturesTTest <-
  function(df,
           threshold = 0.05,
           maxFeatures = Inf,
           .otherParams = list()) {
    
  
  stopifnot(
    "selectBestFeaturesTTest: threshold must be numeric" =
    is.numeric(threshold)
  )
  
  if(is.character(df$class)) df$class <- as.factor(df$class)
  
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
    .otherParams = .otherParams,
    method = "genefilter::colttests"
  )
}

#' @export
#' @importFrom glmnet cv.glmnet glmnet
selectBestFeaturesGlmnet <-
  function(df,
           threshold = 0.05,
           maxFeatures = Inf,
           .otherParams = list()) {
    
  
  x <- as.matrix(df %>% select(-class))
  
  if(is.numeric(threshold)) {
    fit <- glmnet::glmnet(x, df$class, family = "binomial", lambda = threshold) 
    coef <- coefficients(fit)
  } else {
    log_trace("selectBestFeaturesGlmnet - Using cv.glmnet for feature selection")
    fit <- glmnet::cv.glmnet(x, df$class, family = "binomial") 
    coef <- coefficients(fit, fit$lambda.1se)
  }
  
  coef <- as.matrix(coef)
  coef <- coef[coef != 0,][-1]
  
  if(length(coef) == 0) {
    stop("selectBestFeaturesGlmnet: cannot find any meaningful features")
  }
  
  stats <- tibble(ensg = names(coef), coef = coef) %>%
    arrange(desc(abs(coef))) %>% head(maxFeatures)
  
  finalDt <- df[, c("class", stats$ensg)]
  
  list(
    stats = stats,
    df = finalDt,
    .otherParams = .otherParams,
    method = "glmnet::glmnet"
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
#' @importFrom Boruta Boruta
#'
#' @examples
#' 
#' data("feature_selection_data", package = "XIFF")
#' featureFit <- selectBestFeaturesBoruta(feature_selection_data)
#' plot(featureFit$fit, las = 1, horizontal = TRUE)
#' 
selectBestFeaturesBoruta <-
  function(df,
           threshold = c("Confirmed", "Tentative"),
           maxFeatures = Inf,
           threads = getOption("xiff.boruta.threads", 2),
           .otherParams = list()
           ) {
    
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
    ensg = colnames(fit$ImpHistory[,names(fit$finalDecision)]),
    p.value = pvals,
    decision = fit$finalDecision 
  )
  
  stats <- stats[order(stats$p.value),]
  
  # Select the values based on maxFeatures
  stats <- head(stats, maxFeatures)
  fit$finalDecision <- fit$finalDecision[stats$ensg]
  fit$ImpHistory <- fit$ImpHistory[,c(stats$ensg, "shadowMax", "shadowMean", "shadowMin")]
  
  
  df <- df[, c(stats$ensg, "class")]
  list(
    fit = fit,
    stats = stats,
    df = df,
    .otherParams = .otherParams,
    method = "Boruta::Boruta"
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
#' @noRd
#' @examples 
#' 
#' \dontrun{
#'   data("train_model_data", package = "XIFF")
#'   train_model_data <- train_model_data[,-1]
#' 
#'   fitRF  <- trainModel(train_model_data, method = "rf", tuneLength = 1)
#'   fitSVM <- trainModel(train_model_data, method = "svmLinear2", tuneLength = 1)
#'   fitNeuralnet <- trainModel(train_model_data, method = "neuralnetwork")
#' }
#' 
#' 
trainModel <- function(df, method = "rf", tuneLength = 5, number = 10, repeats = 10, ...){
  
  
  if(method == "neuralnetwork") {
    method <- modelInfoNeuralNetwork()
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
    
  args <- c(args, list(...))
  
  
  do.call(caret::train, args)
}


tbl2XiffImportanceTable <- function(tbl, name) {
  class(tbl) <- c("XiffImportanceTable", class(tbl)[class(tbl) != "XiffImportanceTable"])
  attr(tbl, "importanceName") <- name
  tbl
}

#' A function that returns formatted table for variable importance.
#' 
#' If some model from caret requires some custom variable importance code
#' implement it here.
#' 
#' @noRd
#' 
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr rename arrange select
#' @importFrom neuralnet neuralnet
#' @importFrom NeuralNetTools olden
#' @importFrom caret varImp
#' 
getVarImp <- function(model, stats){
  
  varImp2table <- function(model, name) {
    
    tbl <- varImp(model) %>%
      tibble::rownames_to_column("ensg") %>%
      dplyr::rename(importance = Overall) %>%
      dplyr::arrange(desc(importance)) %>%
      tbl2XiffImportanceTable(name)
  }
  
  if(inherits(model, "lm")) {
    # to make it into switch statement below,
    # it requires that the expression is of the length 1
    class(model) <- "lm" 
  }
  
  if(length(class(model)) == 1) { 
    # e.g. result of glmnet has 2 classes "lognet" and "glmnet"
    # checks for custom implemented models
    result <- switch(
      EXPR = class(model),
      randomForest = varImp2table(model, "Mean decrease Gini"),
      svm = stats %>%
          dplyr::select(ensg, importance = p.value) %>%
          dplyr::arrange(importance) %>% 
          tbl2XiffImportanceTable("p.val")
      ,
      nn = modelInfoNeuralNetwork()$varImp(model) %>%
          tibble::rownames_to_column("ensg") %>%
          dplyr::arrange(desc(abs(importance))) %>%
          tbl2XiffImportanceTable("olden"),
      lm = {
        coef <- abs(coefficients(model)[-1]) # remove intercept
        data.frame(coef) %>% tibble::rownames_to_column("ensg") %>%
          rename(importance = coef) %>%
          dplyr::arrange(desc(abs(importance))) %>%
          tbl2XiffImportanceTable("Model Coefficient")
      },
      NULL
    )
    
    if(!is.null(result)) return(result)
  }
  
  result <- try(varImp2table(model, "Model Importance"))
  
  if(inherits(result, "try-error")) {
    stop(glue::glue("Variable imortance for {class(model)} not supported."))
  }
  
  return(result)
    
}

#' @importFrom glue glue
#' @importFrom logger log_trace
createMachineLearningModel <- function(trainingSet,
                                       geneSet,
                                       geneAnno,
                                       # Training data params
                                       trainingData = NULL,
                                       getDataForModelFnc = getDataForModel,
                                       dataParams = NULL,
                                       # Caret params
                                       method = "rf",
                                       tuneLength = 5,
                                       number = 10,
                                       repeats = 10,
                                       # Feature selection params
                                       selectBestFeaturesFnc = "auto",
                                       threshold = "Confirmed",
                                       maxFeatures = "auto",
                                       featuresParams = NULL,
                                       # Other params passed to caret::train
                                       ...,
                                       # misc parameters
                                       .verbose = TRUE,
                                       .progress = FALSE,
                                       .epsilonRNAseq = 10,
                                       .otherParams = list(),
                                       .extraClass = NULL
                                       ) {
  
  localMessage <- function(...) if(.verbose) message(glue::glue(...))
  
  progress <- ProcessProgress$new("Create ML model", .progress)
  progress$update(0.2, "fetching data...")
  
  if(is.list(trainingSet) && !is.data.frame(trainingSet)) {
    # allow user to pass simple list
    trainingSet <- XIFF::stackClasses(trainingSet, return_factor = TRUE)
  }
  
  #------------- Prepare data -------------
  if(is.null(trainingData)) {
    log_trace("xiffML: Fetching data using getDataForModelFnc.")
    
    dataParams <- c(
      list(assignment = trainingSet, features = geneSet),
      dataParams)
    
    df <- do.call(getDataForModelFnc, dataParams)
    
  } else if(inherits(trainingData, "data.frame")) {
    df <- trainingData
  } else {
    stop("createMachineLearningModel: trainingData must be NULL or data.frame")
  }
  df <- df[, !colnames(df) %in% getOption("xiff.column"), drop = FALSE]
  if (nrow(df) == 0) progress$error("No data available")

  #------------- GREP specific code ------------
  if(method == "GREP") {
    selectBestFeaturesFnc <- getGrepFeatureSelection
    if(is.character(threshold)) threshold <- 0.1
    if(maxFeatures == "auto") {
      maxFeatures <- 600
    }
    
    log_trace("GREP: Max Features: {maxFeatures}.")
    method <- "glm"
    modelSelectionMethod <- "GREP"
    .otherParams$epsilonRNAseq <- .epsilonRNAseq
    featuresParams <- c(featuresParams, list(epsilonRNAseq = .epsilonRNAseq))
    .extraClass <- "XiffGREP"
  }
  
  #------------- Feature selection -------------
  if(maxFeatures == "auto") {
    # selecting custom number of features for 'neuralnetwork' - note that other methods are not 
    # constrained here. But if you want to make such constrain that is going to be a default for the users,
    # please do that here.
    maxFeatures <- case_when(
      method == "neuralnetwork" ~ 5,
      TRUE ~ Inf
    )
    
    localMessage("Max features: {maxFeatures}")
  } else if(!is.numeric(maxFeatures)) {
    stop("XIFF::createMachineLearningModel - maxFeatures must be a numeric value greater than zero or 'auto' string.")
  }
  
  if(is.character(selectBestFeaturesFnc) && selectBestFeaturesFnc == "auto") {
    localMessage("Selecting feature selection method using auto.")
    if(method == "neuralnetwork") {
      localMessage("Selecting Boruta feature selection method for {method}")
      selectBestFeaturesFnc <- selectBestFeaturesBoruta
      modelSelectionMethod <- "Boruta"
      
      if(is.numeric(threshold)) {
        localMessage("Changing feature selection threshold from numeric",
                     " {threshold} to 'Confirmed'"
        )
        threshold <- "Confirmed"
      }
      
    } else {
      localMessage("Selecting T-test feature selection method for {method}")
      selectBestFeaturesFnc <- selectBestFeaturesTTest
      modelSelectionMethod <- "T-test"
      
      if(is.character(threshold)) {
        localMessage(
          "Changing feature selection threshold from character",
          " {threshold} to numeric '0.05'"
        )
        threshold <- 0.05
      }
    }
    
  } else {
    modelSelectionMethod <- "custom"
  }
  
  progress$update(
    0.3,
    glue::glue("selecting features (method: {modelSelectionMethod},",
                      " Max features: {maxFeatures})...")
  )
  
  featuresParams <- c(
    list(
      df = df,
      threshold = threshold,
      maxFeatures = maxFeatures,
      .otherParams = .otherParams
    ),
    featuresParams
  )
  
  selectedFeatures <- do.call(selectBestFeaturesFnc, featuresParams)
  stats <- selectedFeatures$stats
  df <- selectedFeatures$df
  .otherParams <- selectedFeatures$.otherParams
  
  bestFeatures <- stats %>% pull(ensg)

  if (length(bestFeatures) == 0){
    progress$error(paste("no significant features found for threshold =", threshold))
  }

  progress$update(0.5, "training model...")

  trainingOutput <- try(trainModel(
    df = df,
    method = method,
    tuneLength = tuneLength,
    number = number,
    repeats = repeats,
    ...
  ))
  
  if(is(trainingOutput, "try-error")) {
    
    if(method == "neuralnetwork") {
      msg <- paste(
        "Cannot train neuralnetwork the model.", 
        " Most probably there's no good parameter combination that satisfies",
        " the minimum fit criteria. Please try different model or dataset."
      )
      progress$error(msg)
    } else {
      msg <- paste(
        "An error occured during model training phase.", 
        " Please contact app authors."
      )
    }
    progress$error(msg)
  }

  importanceRes <- getVarImp(trainingOutput$finalModel, stats)
  if(!is.null(.extraClass) && .extraClass == "XiffGREP") {
    # TODO: expose this as an additional param to be able to
    # join any result from importanceRes with geneAnno
    # it should be tackled after solving #BIARD-295
    df <- mlGrepJoinAnno(importanceRes, geneAnno)
  } else {
    df <- importanceRes %>% left_join(geneAnno, by = "ensg")
  }
  df <- tbl2XiffImportanceTable(df, attr(importanceRes, "importanceName"))

  progress$update(1.0, "job done")

  class(trainingOutput) <- c(.extraClass, "MLXIFF", class(trainingOutput))
  
  trainingOutput$featureSelectionResult <- selectedFeatures
  trainingOutput$df <- df
  trainingOutput$bestFeatures <- df[["ensg"]]
  trainingOutput$trainingSet <- trainingSet
  trainingOutput$trainingItems <- trainingSet[[getOption("xiff.column")]]
  trainingOutput$otherParams <- .otherParams
  
  trainingOutput
}

print.MLXIFF <- function(x, ...) {
  
  class(x) <- class(x)[-1]
  cat("XIFF Raw Model\n\n")
  print(x$classLabel)
  
  cat("caret part:\n")
  print(x)
  
}



#' Convert classLabel list into vector of levels
#'
#' @param cl classLabel list
#'
#' @return character vector with 2 elements.
#' @export
#'
#' @examples
#' 
#' cl1 <- list(class1_name = "cl1", class2_name = "cl2")
#' classLabel2levels(cl1)
#' 
#' cl2 <- list(class2_name = "cl2", class1_name = "cl1")
#' classLabel2levels(cl1) == classLabel2levels(cl2)
#' 
classLabel2levels <- function(cl) {
  c(cl$class1_name, cl$class2_name)
}

#' @exportS3Method
predict.MLXIFF <- function(x, newdata = NULL, ..., useClassLabels = TRUE) {
  
  class(x) <- class(x)[!class(x) %in% c("MLXIFF")]
  result <- predict(x, newdata = newdata, ...)  
  if(useClassLabels && is.factor(result) && !is.null(x$classLabel)) {
    levels(result) <- classLabel2levels(x$classLabel)
  }
  result
}

#' @export
`newClassLabel<-` <- function(x, value){
  x$classLabel <- value
  x
}

# Unbalanced tumortypes -------------------------------------------------------
#' @export
shinyDropUnbalancedTumortypes <- function(AnnotationFocus, classSelection, minCount = 1){
  anno <- AnnotationFocus()
  if (is.null(anno) || nrow(anno) == 0) return()

  cs <- reactiveValuesToList(classSelection)
  balancedTT <- getBalancedTumortypes(cs, anno, minCount)

  colname <- getOption("xiff.column")
  colname <- rlang::sym(colname)

  validItems <- anno %>%
    filter(tumortype %in% balancedTT) %>%
    pull(!!colname)

  classSelection$class1 <- intersect(classSelection$class1, validItems)
  classSelection$class2 <- intersect(classSelection$class2, validItems)
}

#' @rdname getBalancedVariableValues
#' @export
dropUnbalancedVariableValues <- function(cs, anno, variable, minCount = 1) {
  
  colname <- getOption("xiff.column")
  colname <- rlang::sym(colname)
  
  if(is.numeric(anno[[variable]])) {
    validItems <- anno %>%
      filter(!is.na(!!rlang::sym(variable))) %>%
      pull(!!colname)
    
  } else {
    balanced <- getBalancedVariableValues(cs, anno, variable, minCount)
    validItems <- anno %>%
      filter(!!rlang::sym(variable) %in% balanced) %>%
      pull(!!colname)
  }
  
  cs$class1 <- intersect(cs$class1, validItems)
  cs$class2 <- intersect(cs$class2, validItems)
  cs
}

#' @rdname getBalancedVariableValues
#' @export
dropUnbalancedTumortypes <- function(cs, anno, minCount = 1) {
  dropUnbalancedVariableValues(cs, anno, "tumortype", minCount)
}

#' Get valid tumor types / variables
#'
#' This function returns tumor types that are available in the data for both classes
#'
#' @param cs list, class selection
#' @param anno data.frame, item annotation
#' @param variable column name from which the balanced values will be extracted
#' @param minCount minimum number of occurrences in each class to consider a level
#'        as valid. If \code{minCount} is lower that 1, then all levels will
#'        be returned (except NAs).
#'
#' @return character vector of valid tumortypes
#' @rdname getBalancedVariableValues
#' @export
#' 
#' @examples 
#' anno <- tibble(
#'  cl = c("a", "b", "c", "d", "e", "f", "g"),
#'  tumortype = c("x", "x", "y", "x", "x", "y", "z")
#' )
#' colnames(anno)[1] <- getOption("xiff.column")
#' 
#' cs <- classAssignment(cl1 = c("a", "b", "c"), cl2 = c("d", "e", "f", "g"))
#' 
#' getBalancedTumortypes(cs, anno) # "x" and "y" are available in both classes
#' 
#' # 'g' has unbalanced tumortype
#' dropUnbalancedTumortypes(cs, anno)
#' dropUnbalancedTumortypes(cs, anno)$class2
#' 
#' # only "x" appears at least 2 times in each class
#' getBalancedTumortypes(cs, anno, minCount = 2) 
#' 
#' cs2 <- dropUnbalancedTumortypes(cs, anno, 2)
#' cs2$class1
#' cs2$class2
#' 
#' # empty results
#' getBalancedTumortypes(cs, anno, 3)
#' dropUnbalancedTumortypes(cs, anno, 3)
#' 
#' # getBalancedVariableValues and dropUnbalancedVariableValues
#' getBalancedVariableValues(cs, anno, "tumortype", 1)
#' getBalancedVariableValues(cs, anno, "tumortype", 2)
#' dropUnbalancedVariableValues(cs, anno, "tumortype", 2)
#' 
#' # minCount == 0
#' getBalancedVariableValues(cs, anno, "tumortype", 0)
#' dropUnbalancedVariableValues(cs, anno, "tumortype", 0)
#' 
getValidTumorTypes <- function(cs, anno, minCount = 1) {
  .Deprecated("getBalancedTumortypes")
  getBalancedTumortypes(cs, anno, minCount)
}

#' @rdname getBalancedVariableValues
#' @export
getBalancedTumorTypes <- function(cs, anno, minCount = 1) {
  .Deprecated("getBalancedTumortypes")
  getBalancedTumortypes(cs, anno, minCount)
}

#' @rdname getBalancedVariableValues
#' @export
getBalancedTumortypes <- function(cs, anno, minCount = 1) {
  getBalancedVariableValues(cs, anno, "tumortype", minCount)
}

#' @rdname getBalancedVariableValues
#' @export
getBalancedVariableValues <- function(cs, anno, variable, minCount = 1){
  if (is.null(anno) || nrow(anno) == 0) return()

  colname <- getOption("xiff.column")

  stopifnot(variable %in% colnames(anno))
  
  if(minCount < 1) {
    # if minCount < 1, e.g. 0, then it returns all levels
    return(anno[[variable]] %>% na.omit() %>% unique() %>% as.character())
  }
  
  variable <- rlang::sym(variable)
  
  stackClasses(cs) %>%
    left_join(anno, by = colname) %>%
    group_by(class, !!variable) %>%
    summarise(n = dplyr::n(), .groups = "drop") %>%
    filter(n >= minCount) %>%
    group_by(!!variable) %>%
    summarize(ok = dplyr::n() > 1, .groups = "drop") %>% # must be present in both classes
    filter(ok) %>%
    pull(!!variable) %>%
    na.omit() %>%
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
