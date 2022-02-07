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
    
    if(is.null(classLabel)) {
      
      if(is(cs, "classAssignment")) {
        classLabel <- getClassLabel(cs)
      } else {
        classLabel <- list(
          class1_name = "class1",
          class2_name = "class2"
        )
      }
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

handleTestSet <- function(classSelection, p_test = 0.2) {

  assignment <- XIFF::stackClasses(classSelection, return_factor = TRUE)
  sets <- XIFF::splitTrainingTestSets(assignment, p_test)
  trainingSet <- sets$training
  testSet <- sets$test
  
  list(
    trainingSet = trainingSet,
    testSet = testSet
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
#' @param p_test percentage of the cs that will be assigned as test.
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
#' @return MLXIFF object
#'
buildMachineLearning <- function(cs,
                                 geneSet,
                                 geneAnno,
                                 species = "human",
                                 classColumn = "class",
                                 classLabel = NULL,
                                 p_test = 0,
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
  sets <- handleTestSet(classSelection, p_test)
  
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
  
  if(!is.null(sets$testSet)) {
    res$testSet <-
      mlSets2OriginalNames(sets$testSet, classColumn, res$classLabel)
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
xiffSupportedModels <- function() {
  c(
    "Random Forest" = "rf",
    "SVM" = "svmLinear2",
    "Neural Network" = "neuralnetwork",
    "Regularized Logistic Regression" = "glmnet",
    "GREP" = "GREP"
  )
}



#' Get Data for Machine Learning Model
#'
#' @param assignment stackClass assignment of items or classAssignment object 
#' @param features character vector of genes ensg or fitted XIFF Machine Learning model
#' @param schema data base schema, default value should be used
#' @param column column containing the items, default value shoud be used
#' @param classLabel list with alternative class labels
#'
#' @return raw data.frame with features for MLXIFF model
#' @export
#'
#' @examples
#' if(require("CLIFF"))
#'   CLIFF::setDbOptions()
#'   ca <- CLIFF::exampleClassAssigment()
#'   geneSet <- head(CLIFF::getGSEAdata("human", gene_set = "HALLMARK_P53_PATHWAY"), 3)
#' 
#'   modelData <- getDataForModel(ca, geneSet)
#'   head(modelData)
#' 
#'   getDataForModel(ca, geneSet, classLabel = list(class1 = "c1", class2 = "c2"))
#' }
#' 
getDataForModel <- function(assignment,
                            features,
                            schema = getOption("xiff.schema"),
                            column = getOption("xiff.column"),
                            classLabel = NULL) {
  UseMethod("getDataForModel", features)
}


#' Summarise Model Parameters in Table
#'
#' @param model machine learning model fitted in XIFF
#'
#' @return summary of MLXIFF model
#' @export
#'
#' @examples
mlGetTableData <- function(model) {
  UseMethod("mlGetTableData")
}


#' Get TPM data for given ML Model
#'
#' @param model MLXIFF object
#' @param ensg gene ensg
#' @param annoFocus celline annotation data 
#'
#' @details This function is required to be implemented for new MLXIFF model.
#' Seel \code{mlGetTableData.XiffGREP} for an example.
#'
#' @return data.frame with columns: celllinename, ensg, tpm, tumortype
#' @export
#' 
#' @example 
#' 
#' \dontrun{
#' 
#' df <- mlGetTpmData(model, "ENSG00000147889", annoFocus)
#' 
#' }
#' 
mlGetTpmData <- function(model, ensg, annoFocus) {
  UseMethod("mlGetTpmData")
}


#' Generate Expression Plot for Machine Learning Model
#'
#' @param model MLXIFF object
#' @param df result of \code{mlGetTpmData}
#' @param ca classAssigment object
#' @param plotType plot type
#' @param gene list with gene symbol and ensg
#'
#' @details this plot is mostly used in the ShinyApplication
#' @return ggplot2
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' 
#' df <- mlGetTpmData(model, "ENSG00000147889", annoFocus)
#' gene <- list(symbol = getGeneSymbol("ENSG00000147889"), ensg = "ENSG00000147889")
#' mlGenerateExpressionPlot(model, df, trainingSet, gene = gene)
#' }
#' 
mlGenerateExpressionPlot <- function(model, df, ca, plotType = "point", gene) {
  UseMethod("mlGenerateExpressionPlot")
}


#' Get Raw Data For MLXIFF Model
#'
#' @param features character vector with features or MLXIFF object
#' @param names character vector with items for which the data needs to be extracted
#' @param schema data base schema to be used
#' (in most cases default value is sufficient)
#' @param column name of the column which stores the items
#' (in most cases default value is sufficient)
#'
#' @return long form data.frame containing {itemname}, ensg, and score
#' @export
#'
#' @examples
#' 
#' if(require("CLIFF")) {
#' 
#'   ca <- CLIFF::exampleClassAssigment()
#'   geneSet <- CLIFF::getGSEAdata("human", "hallmark", "HALLMARK_P53_PATHWAY")
#'   getRawDataForModel(geneSet, unlist(ca))
#' 
#' }
#' 
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
                                    column = getOption("xiff.column"),
                                    classLabel = NULL) {
  
  if(is.null(classLabel) && is(assignment, "classAssignment")) {
    classLabel <- getClassLabel(assignment)
  }
  
  if(is.list(assignment) && !is.data.frame(assignment)) {
    assignment <- stackClasses(assignment, classLabel = classLabel)
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
                                   column = getOption("xiff.column"),
                                   classLabel = NULL) {
  
  if(is.null(classLabel)) features$classLabel
  
  getDataForModel(assignment,
                  features$bestFeatures,
                  schema = schema,
                  column = column, 
                  classLabel = classLabel)
}

#' @export
getRawDataForModel.MLXIFF <- function(features,
                                       names = NULL,
                                       schema = getOption("xiff.schema"),
                                       column = getOption("xiff.column")) {
  getRawDataForModel(features$bestFeatures, names, schema, column)
}


##########
#' Create all ML plots in one go.
#'
#' @param model MLXIFF model
#' @param testSet test set
#' @param annoFocus annoFocus
#'
#' @export
makeMlModelPlots <- function(model, testSet, annoFocus) {
  
  
  msg <- paste("generateTrainingModelPlots(model)",
               " and generateTestModelPlots(testResult)")
  .Deprecated(msg)
  
  test <- testModel(
    model,
    testSet = testSet,
    anno = annoFocus
  )
  
  labels <- XIFF:::classLabel2levels(model$classLabel)
  
  df <- prepareTablePlotData(
    df = test$data,
    positive_preds = labels[1],
    positive_refs = labels[1],
    labels_preds = labels,
    labels_refs = labels,
    labels = c("positive", "negative")
  )
  
  
  df2 <- generateTestPerformanceData(test$res$table)
  list(
    TablePlot = generateTablePlot(df),
    ApplyPerformancePlot = generateTestPerformancePlot(df2),
    PerformancePlot = generatePerformancePlot(model),
    VariableImportancePlot = generateVarImpPlot(model)
  )
}

#' Generate Machine Learning Models Plots on Training Set
#'
#' @param model machine learning model created with XIFF (MLXIFF object)
#'
#' @return list with plots created on based on the training set.
#' @export
#'
generateTrainingModelPlots <- function(model) {
  UseMethod("generateTrainingModelPlots")
}

#' @exportS3Method 
generateTrainingModelPlots.default <- function(model) {
  stop("`model` must be MLXIFF object. Check ?buildMachineLearning function.")
}

#' @exportS3Method 
generateTrainingModelPlots.MLXIFF <- function(model) {
  list(
    PerformancePlot = generatePerformancePlot(model),
    VariableImportancePlot = generateVarImpPlot(model)
  )
}

#' Generate Machine Learning Model based on the Test Results.
#'
#' @param testResults MLModelTestsResult object created with testModel function.
#'
#' @return list with plots created on test result.
#' @export
#'
generateTestModelPlots <- function(testResult) {
  UseMethod("generateTestModelPlots")
}

#' @export
#' @exportS3Method 
generateTestModelPlots.default <- function(testResult) {
  stop("`testResults` must be `MLModelTestsResult` object.",
       "\nCheck `testModel(model, testSet, anno)` function.")
}

#' @export
#' @exportS3Method 
generateTestModelPlots.MLModelTestsResult <- function(testResult) {
  
  list(
    TablePlot = generateTablePlot(testResult),
    ApplyPerformancePlot = generateTestPerformancePlot(testResult)
  )
}
