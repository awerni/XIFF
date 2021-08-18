handleClassSelection <- function(cs,
                                 classColumn = "class",
                                 classLabel    = NULL,
                                 names = getOption("xiff.column")) {
  
  if(is.list(cs) && !is.data.frame(cs)) {
    
    log_trace("handleClassSelection - using cs as list.")
    
    if(length(cs) != 2 ||
       !all(c("class1", "class2") %in% names(cs)) ||
       !all(vapply(cs, FUN.VALUE = TRUE, is.character))
    ) {
      stop("`cs` must be a list of 2 character vectors - class1 and class2.")
    }
    
    attr(cs, "classLabel") <- classLabel
    attr(cs, "classColumn")   <- classColumn
    
    return(cs)
  }
  
  
  if(!classColumn %in% names(cs)) {
    stop(glue::glue("`{classColumn}` is not a column name in `cs`."))
  }
  
  if(is.null(classLabel)) {
    levels <- sort(unique(cs[[classColumn]]))
    positiveClass <- levels[1]
  } else if(is.character(classLabel)) {
    positiveClass <- classLabel
    classLabel <- NULL
  } else {
    positiveClass <- classLabel$class1_name
  }
  
  if(!positiveClass %in% cs[[classColumn]]) {
    stop(glue::glue("'{positiveClass}' is not value in cs[['{classColumn}']]"))
  }
    
  classSelection <- list(
    class1 = subset(cs[[names]], cs[[classColumn]] == positiveClass),
    class2 = subset(cs[[names]], cs[[classColumn]] != positiveClass)
  )
  
  if(is.null(classLabel)) {
    classLabel <- list(
      class1_name = positiveClass,
      class2_name = setdiff(unique(cs[[classColumn]]), positiveClass)
    )
  }
  
  attr(classSelection, "classLabel") <- classLabel
  attr(classSelection, "classColumn")   <- classColumn
  classSelection
}

handleValidationSet <- function(classSelection, p_validation = 0.2) {
  
  assignment <- XIFF::stackClasses(classSelection, return_factor = TRUE)
  sets <- XIFF::splitTrainingValidationSets(assignment, p_validation)
  trainingSet <- sets$training
  validationSet <- sets$validation
  
  list(
    trainingSet = trainingSet,
    validationSet = validationSet
  )
  
}

#' Workhorse for machine learning.
#'
#' @param cs 
#' @param geneSet 
#' @param geneAnno 
#' @param species 
#' @param method 
#' @param p_validation 
#' @param ...
#' @param task 
#'
#' 
#' @importFrom FutureManager is.fmError
#' @export
#' @return
#'
buildMachineLearning <- function(cs,
                                 geneSet,
                                 geneAnno,
                                 species = "human",
                                 classColumn = "class",
                                 classLabel = NULL,
                                 p_validation = 0,
                                 itemsColumn = getOption("xiff.column"),
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
                                 .extraClass = NULL) {
  
  classSelection <- handleClassSelection(cs, classColumn, classLabel, itemsColumn)
  sets <- handleValidationSet(classSelection, p_validation)
  
  res <- createMachineLearningModel(
    trainingSet = sets$trainingSet,
    geneSet = geneSet,
    geneAnno = geneAnno,
    # Training data params
    trainingData = trainingData,
    getDataForModelFnc = getDataForModelFnc,
    dataParams = dataParams,
    # Caret params
    method = method,
    tuneLength = tuneLength,
    number = number,
    repeats = repeats,
    # Feature selection params
    selectBestFeaturesFnc = selectBestFeaturesFnc,
    threshold = threshold,
    maxFeatures = maxFeatures,
    featuresParams = featuresParams,
    # Other params passed to caret::train
    ...,
    # misc parameters
    .verbose = .verbose,
    .progress = .progress,
    .epsilonRNAseq = .epsilonRNAseq,
    .otherParams = .otherParams,
    .extraClass = .extraClass
  )
  

  if (is.null(res)) return() # handle the task cancel
  if (FutureManager::is.fmError(res)) return(res)
  
  res$validationSet <- sets$validationSet
  res$classLabel    <- attr(classSelection, "classLabel")
  res$species       <- species
  res$classColumn   <- classColumn
  
  res
  
}

#' Get a vector machine learning models that are supported by XIFF package.
#'
#' @return a character vector containing the model names that are supported by XIFF package.
#' 
#' @details First element should be treated as default model.
#' 
#' @export
#'
#' @examples
xiffSupportedModels <- function() {
  c(
    "Random Forest" = "rf",
    "SVM" = "svmLinear2",
    "Neural Network" = "neuralnetwork",
    "Regularized Logistic Regression" = "glmnet",
    "GREP" = "GREP"
  )
}


#' @export
getDataForModel <- function(assignment,
                            features,
                            schema = getOption("xiff.schema"),
                            column = getOption("xiff.column")) {
  UseMethod("getDataForModel", features)
}

#' @export
mlGetTableData <- function(model) {
  UseMethod("mlGetTableData")
}

#' @export
mlGetTpmData <- function(model, ensg, annoFocus) {
  UseMethod("mlGetTpmData")
}

#' @export
mlGenerateExpressionPlot <- function(model, df, ca, plotType = "point", gene) {
  UseMethod("mlGenerateExpressionPlot")
}

#' @export
getRawDataForModel <- function(features,
                                       names = NULL,
                                       schema = getOption("xiff.schema"),
                                       column = getOption("xiff.column")) {
  UseMethod("getRawDataForModel")
}

######## Default methods
#' @export
getRawDataForModel.default <- function(features,
                                       names = NULL,
                                       schema = getOption("xiff.schema"),
                                       column = getOption("xiff.column")) {
  
  clFilter <- if (length(names) > 0){
    paste(" AND", getSQL_filter(column, names))
  } else {
    ""
  }
  
  ensgSql <- getSQL_filter("ensg", features)
  
  sql <- glue::glue("
     SELECT 
      {column}, ensg, log2tpm AS score 
    FROM 
      {schema}.processedrnaseqview                
    WHERE
      {ensgSql}
      {clFilter}          
  ")
  
  
  getPostgresql(sql)
}


#' @export
mlGenerateExpressionPlot.default <- function(model, df, ca, plotType = "point", gene) {
  
  title <- paste("\n", gene$symbol, "-", gene$ensg)
  generatePlotByType(
    data = df,
    ca = ca,
    plotType = plotType,
    dataCol = "tpm",
    title = title,
    ylabel = "TPM",
    trans = "log10"
  )
}

#' @export
mlGetTableData.default <- function(model) {
  
  importanceName <- attr(model$df, "importanceName")
  importanceLabel <- paste0("importance (", importanceName, ")")
  
  species <- model$species
  
  model$df %>%
    mutate(location = getEnsemblLocationLink(location, species)) %>%
    mutate(importance = signif(importance, 3)) %>%
    rename(!!importanceLabel := importance)
  
}

#' @export
mlGetTpmData.default <- function(model, ensg, annoFocus) {
  
  cs <- model$cs
  data <- getDataGeneExpressionById(ensg, cs) %>% 
    addTumortypes(annoFocus)
  
}

############## getDataForModel methods ############## 
#' @export
getDataForModel.character <- function(assignment,
                                    features,
                                    schema = getOption("xiff.schema"),
                                    column = getOption("xiff.column")) {
  
  if(is.list(assignment) && !is.data.frame(assignment)) {
    assignment <- stackClasses(assignment)
  }
  
  getRawDataForModel(
    features = features,
    names    = assignment[[column]],
    schema   = schema,
    column   = column
  ) %>%
    tidyr::pivot_wider(names_from = ensg, values_from = score) %>%
    left_join(assignment, by = column)
}

#' @export
getDataForModel.MLXIFF <- function(assignment,
                                      features,
                                      schema = getOption("xiff.schema"),
                                      column = getOption("xiff.column")) {
  
  getDataForModel(assignment,
                  features$bestFeatures,
                  schema = schema,
                  column = column)
}

#' @export
getRawDataForModel.MLXIFF <- function(features,
                                       names = NULL,
                                       schema = getOption("xiff.schema"),
                                       column = getOption("xiff.column")) {
  getRawDataForModel(features$bestFeatures, names, schema, column)
}

