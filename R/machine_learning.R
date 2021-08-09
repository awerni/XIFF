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
                                 method = "rf",
                                 p_validation = 0.2,
                                 ...,
                                 task = FALSE) {
  
  
  assignment <- XIFF::stackClasses(cs, return_factor = TRUE)
  
  sets <- XIFF::splitTrainingValidationSets(assignment, p_validation)
  trainingSet <- sets$training
  validationSet <- sets$validation
  
  if (!is.null(validationSet)){
    validationSet <- split(validationSet[[getOption("xiff.column")]], validationSet$class)
    cs$class1 <- setdiff(cs$class1, validationSet$class1)
    cs$class2 <- setdiff(cs$class2, validationSet$class2)
  }
  
  res <- createMachineLearningModel(
    trainingSet = trainingSet,
    geneSet = geneSet,
    geneAnno = geneAnno,
    method = method,
    .progress = task,
    ...
  )
  
  if (is.null(res)) return() # handle the task cancel
  if (FutureManager::is.fmError(res)) return(res)
  
  res$cs <- cs
  res$species <- species
  res$validationSet <- validationSet
  
  class(res) <- c("XiffMachineLearningResult", class(res))
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

######## Default methods
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

