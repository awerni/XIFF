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
    "Regularized Logistic Regression" = "glmnet"
  )
}
