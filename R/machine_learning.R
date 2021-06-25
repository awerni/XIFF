#' Workhorse for machine learning.
#'
#' @param task 
#' @param cs 
#' @param ensg_gene_set 
#' @param gene_anno 
#' @param species 
#' @param method 
#' @param p_validation 
#'
#' 
#' @importFrom FutureManager is.fmError
#' @export
#' @return
#'
#' @examples
#' 
#' data(machine_learning_long_fun_example_data, package = "XIFF")
#' attach(machine_learning_long_fun_example_data)
#' task <- FALSE
#' 
#' fit <- buildMachineLearning(task, cs, ensg_gene_set, gene_anno, species = "human", method = "rf", p_validation = 0.2)
#' fitNN <- buildMachineLearning(task, cs, ensg_gene_set, gene_anno, species = "human", method = "neuralnetwork", p_validation = 0.2)
#' 
buildMachineLearning <- function(task, cs, ensg_gene_set, gene_anno, species = "human", method = "rf", p_validation = 0.2, ...){
  
  assignment <- XIFF::stackClasses(cs, return_factor = TRUE)
  
  sets <- XIFF::splitTrainingValidationSets(assignment, p_validation)
  trainingSet <- sets$training
  validationSet <- sets$validation
  
  if (!is.null(validationSet)){
    validationSet <- split(validationSet$celllinename, validationSet$class)
    cs$class1 <- setdiff(cs$class1, validationSet$class1)
    cs$class2 <- setdiff(cs$class2, validationSet$class2)
  }
  
  res <- createMachineLearningModel(
    trainingSet = trainingSet,
    geneSet = ensg_gene_set,
    geneAnno = gene_anno,
    p = task,
    method = method,
    ...
  )
  
  if (is.null(res)) return() # handle the task cancel
  if (FutureManager::is.fmError(res)) return(res)
  
  list(
    model = res$model,
    df = res$df,
    trainingOutput = res$trainingOutput,
    validationSet = validationSet,
    species = species,
    cs = cs,
    featureSelectionResult = res$featureSelectionResult
  )
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
    "Random forrest" = "rf",
    "SVM" = "svmLinear2",
    "Neuralnetwork" = "neuralnetwork"
  )
}
