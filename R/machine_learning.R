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
    
    attr(cs, "classLabel")  <- classLabel
    attr(cs, "classColumn") <- classColumn
    
    return(cs)
  }
  
  
  if(!classColumn %in% names(cs)) {
    stop(glue::glue("`{classColumn}` is not a column name in `cs`."))
  }
  
  if(is.null(classLabel)) {
    # if classLabel name is not available
    # then the first level in alfabetical order is
    # selected as positive class
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
  
  attr(classSelection, "classLabel")  <- classLabel
  attr(classSelection, "classColumn") <- classColumn
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
#' @param cs class selection. Can be a list with two character vectors or
#'           data.frame.
#' @param geneSet list of genes to be used as model features.
#' @param geneAnno gene annotation table.
#' @param species species.
#' @param method name of ml methods (use values from 
#'        \code{xiffSupportedModels()}) or model names from caret package.
#' @param p_validation percentage of the cs that will be assigned as validation.
#' @param classColumn name of the column that will be used to assgin classes.
#'        Used when cs is data.frame.
#' @param classLabel classLable assigment (list with class1_name and
#'        class2_name) or single string containing the name of the positive
#'        class.
#' @param itemsColumn if cs is data.frame, specifies the name of column that 
#'        contains items (celllines or tissues). Usually it should not be
#'        used.
#' @param trainingData data.frame if user wants to use custom data instead of 
#'        the result of \code{getDataForModel}.
#' @param getDataForModelFnc if the user wants to use custom function to get 
#'        the data it can repleace \code{getDataForModel}.
#' @param dataParams list with additional params to be passed to
#'        \code{getDataForModelFnc}
#' @param tuneLength caret fitting parameter.
#' @param number caret fitting parameter.
#' @param repeats caret fitting parameter.
#' @param selectBestFeaturesFnc 
#' @param threshold threshold for feature selection.
#' @param maxFeatures max number of features to be used in the fitting process.
#'        Each feature selection methods contain a way to limit its number.
#' @param featuresParams additional params to be passed to selectBestFeaturesFnc
#'        if custom function is used.
#' @param .verbose logical. if true prints additional values.
#' @param task internal param to be used by applications.
#' @param .epsilonRNAseq GREP param.
#' @param .otherParams list of other parameters to be saved with the model.
#' @param .extraClass other class to be added to \code{class} vector. Allows to
#'        use custom methods. For advanced users.
#' @param ...
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
                                 task = FALSE,
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
    .progress = task,
    .epsilonRNAseq = .epsilonRNAseq,
    .otherParams = .otherParams,
    .extraClass = .extraClass
  )
  

  if (is.null(res)) return() # handle the task cancel
  if (FutureManager::is.fmError(res)) return(res)
  
  res$classLabel    <- attr(classSelection, "classLabel")
  
  if(!is.null(res$validationSet)) {
    res$validationSet <-
      mlSets2OriginalNames(sets$validationSet, classColumn, res$classLabel)
  }
  res$trainingSet <-
    mlSets2OriginalNames(res$trainingSet, classColumn, res$classLabel)
  
  res$species       <- species
  res$classColumn   <- classColumn
  res$itemsColumn   <- itemsColumn
  
  res
  
}

#' Rename column name and levels in the ml set to original values.
mlSets2OriginalNames <- function(set, classColumn, classLabel) {
  levels(set[["class"]]) <- classLabel2levels(classLabel)
  names(set)[names(set) == "class"] <- classColumn
  set  
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
  
  cs <- model$trainingItems
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

