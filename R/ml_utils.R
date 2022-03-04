#' Get class column name from MLXIFF model.
#'
#' @param model MLXIFF object. 
#' @param asSymbol logical. If FALSE returns string. If true returns symbol.
#'
#' @export
#' 
mlGetClassColumn <- function(model, data = NULL, asSymbol = FALSE) {
  column <- if(is.null(model$classColumn)) "class" else model$classColumn
  
  if(!is.null(data) && !column %in% colnames(data)) {
    # handle the case when classColumn is not present in selected data.
    # requried for UI when it might be just 'class' column
    column <- "class"
  }
  
  if(asSymbol) {
    column <- rlang::sym(column)
  }
  
  column
}
 

mlModelSet2ClassSelectionList <- function(model, set = NULL) {
  
  if(is.null(set)) {
    set <- model$trainingSet
  }
  
  cs <- split(
    set[[model$itemsColumn]],
    set[[model$classColumn]]
  )
  
  names(cs) <- c("class1", "class2")
  attr(cs, "classLabel") <- model$classLabel
  cs
  
}
 
getPredictionSummary <- function(items,
                                 preds,
                                 refs,
                                 positive_model,
                                 positive_cs,
                                 classes,
                                 classes_model,
                                 classes_cs,
                                 annoFocus,
                                 itemColumn = getOption("xiff.column")) {
  
  
  itemColumnSymbol <- rlang::sym(itemColumn)
  
  preds_pn <- ifelse(preds == positive_model, classes[1], classes[2]) %>%
    factor(levels = classes)
  
  refs_pn <- ifelse(refs == positive_cs, classes[1], classes[2]) %>%
    factor(levels = classes)
  
  res <- caret::confusionMatrix(
    data = preds_pn,
    reference = refs_pn,
    positive = "positive",
    mode = "sens_spec"
  )
  
  anno <- annoFocus %>% select(!!itemColumnSymbol, tumortype)
  
  # handle raw forecasts
  preds <- if(all(preds %in% c("class1", "class2"))) {
    ifelse(preds == "class1", classes_model[1], classes_model[2])
  } else {
    preds
  }
  
  refs <- if(all(refs %in% c("class1", "class2"))) {
    ifelse(refs == "class1", classes_cs[1], classes_cs[2])
  } else {
    refs
  }
  
  df <- tibble(
    !!itemColumnSymbol := items,
    predicted = preds_pn,
    reference = refs_pn,
    predicted_original = preds,
    reference_original = refs,
    correct = preds_pn == refs_pn,
  ) %>%
    left_join(anno, by = itemColumn) %>%
    relocate(tumortype, .after = !!itemColumnSymbol)
  
  
  list(
    res = res,
    data = df
  )
}

#' Get Performance Data Frame
#'
#' @param result MLModelTestsResult object or 
#' confusion matrix (table object) or 
#'
#' @return table with columns: metric, n1, n2, value, labelPos, label
#' @export
#'
#' @examples
#' 
#' confMatrix <- as.table(matrix(
#'   c(19L, 9L, 2L, 66L),
#'   nrow = 2,
#'   dimnames = list(
#'     Prediction = c("positive", "negative"),
#'     Reference =  c("positive", "negative")
#'   )
#' ))
#' generateTestPerformanceData(confMatrix)
#' 
generateTestPerformanceData <- function(result) {
  UseMethod("generateTestPerformanceData") 
}

#' @export
#' @exportS3Method 
generateTestPerformanceData.MLModelTestsResult <- function(result) {
  generateTestPerformanceData(result$res$table)
}

#' @export
#' @exportS3Method 
generateTestPerformanceData.table <- function(result) {
  
  t <- result
  TP <- t[1,1] # True Positive
  FP <- t[1,2] # False Positive
  FN <- t[2,1] # False Negative
  TN <- t[2,2] # True Negative
  
  tibble(
    metric = forcats::as_factor(c(
      "Accuracy", 
      "Sensitivity", 
      "Specificity", 
      "Pos Pred Value", 
      "Neg Pred Value"
    )),
    n1 = c(
      TP + TN, # acc
      TP, # sens
      TN, # spec
      TP, # PPV
      TN # NPV
    ),
    n2 = c(
      TP + FP + FN + TN, # acc
      TP + FN, # sens
      FP + TN, # spec
      TP + FP, # PPV
      FN + TN # NPV
    ),
    value = n1 / n2,
    labelPos = value + 0.1,
    label = glue::glue("{sprintf('%.3f', value)}\n({n1} of {n2})")
  )
}

prepareTablePlotData <- function(df, positive_preds, positive_refs, labels_preds, labels_refs, labels){
  if (positive_preds == "class2"){
    labels_preds <- rev(labels_preds)
  } 
  labels_preds <- paste0(labels_preds, "\n(", labels, ")")
  
  if (positive_refs == "class2"){
    labels_refs <- rev(labels_refs)
  } 
  labels_refs <- paste0(labels_refs, "\n(", labels, ")")
  
  df %>%
    select(predicted, reference) %>%
    mutate(
      predicted = as.character(predicted),
      predicted = ifelse(predicted == "positive", labels_preds[1], labels_preds[2]),
      predicted = factor(predicted, levels = rev(labels_preds)), # revert on axis
      reference = as.character(reference),
      reference = ifelse(reference == "positive", labels_refs[1], labels_refs[2]),
      reference = factor(reference, levels = labels_refs)
    )
}


#' Test model on Test Data
#'
#' @param model machine learning model created with XIFF
#' @param testSet test set (classAssignment object with)
#' @param anno items annotation data frame.
#' @param itemColumn for advanced users only. 
#' Name of the column which contains information about items.
#' @param classColumn for advanced users only. 
#' Name of the column which contains information about classes.
#'
#' @return MLModelTestsResults object.
#' 
#' @export
testModel <- function(model,
                      testSet,
                      anno,
                      itemColumn = getOption("xiff.column"),
                      classColumn = NULL
                      ) {
  
  itemColumnSymbol <- rlang::sym(itemColumn)
  
  if(is(testSet, "classAssignment")) {
    testSet <- stackClasses(testSet, getClassLabel(testSet))
  }
  
  df <- getDataForModel(
    assignment = testSet,
    features = model
  )
  
  if(is.null(classColumn)) {
    classColumn <- model$classColumn
    if(!classColumn %in% colnames(df)) {
      classColumn <- "class"
    }
  }
  classColumnSymbol <- rlang::sym(classColumn)
  
  refs <- df[[classColumn]]
  items <- df[[itemColumn]]
  df <- df %>% select(-!!classColumnSymbol, -!!itemColumnSymbol)
  preds <- predict(model, newdata = df)
  
  cl <- classLabel2levels(model$classLabel)
  
  result <- getPredictionSummary(
    items = items,
    preds = preds,
    refs = refs,
    positive_model = levels(preds)[1],
    positive_cs = levels(preds)[1],
    classes = c("positive", "negative"),
    classes_model = cl,
    classes_cs = cl,
    annoFocus = anno
  )
  
  tablePlotData <- prepareTablePlotData(
    df = result$data,
    positive_preds = cl[1],
    positive_refs = cl[1],
    labels_preds = cl,
    labels_refs = cl,
    labels = c("positive", "negative")
  )
  
  result$tablePlotData <- tablePlotData
  
  result$classLabel <- cl
  class(result) <- c("MLModelTestsResult", class(result))
  
  result
}

#' Print method for MLModelTestsResults
#'
#' @param x MLModelTestsResults object
#' @param ... discarded.
#'
#' @return invisible x
#' @export
#'
print.MLModelTestsResult <- function(x, ...) {
  
  cat("Machine Learning Test Result:\n\n")
  print(x[c("res", "data")])
  
  cat("\nTables Methods:")
  cat("\n  generateTestPerformanceData(x)")
  
  cat("\n\nPlots Methods:")
  cat("\n  generateTablePlot(x)")
  cat("\n  generateTestPerformancePlot(x)")
  cat("\n  generateTestModelPlots(x)")
  
  invisible(x)
}

#' gatherPredictionResults
#' 
#' @details TODO: is this function even needed?
#' 
#' @export
gatherPredictionResults <- function(predictions, ...){
  predictions %>%
    purrr::map_dfr(
      .f = function(x){
        tibble::tibble(
          Accuracy = x$res$overall[["Accuracy"]],
          Sensitivity = x$res$byClass[["Sensitivity"]],
          Specificity = x$res$byClass[["Specificity"]],
          `Pos Pred Value` = x$res$byClass[["Pos Pred Value"]],
          `Neg Pred Value` = x$res$byClass[["Neg Pred Value"]]
        )
      },
      ...
    ) %>%
    arrange(desc(Accuracy))
}

#' Utility function for checking if an object is XiffMachineLearningResult.
#'
#' @param model should be XiffMachineLearningResult
#'
#' @return nothing. Called for side effect in shiny.
#' @importFrom shiny validate need
#' @export
#'
validateXiffMachineLearningResult <- function(model) {
  shiny::validate(
    shiny::need(
      inherits(model, "MLXIFF"), 
      paste0(
        "Inccorect model format.",
        " Please provide a model created using CLIFF application",
        " or if you were using R console to create model -",
        " XIFF::buildMachineLearning or XIFF::createMachineLearningModel"
      )
    )
  )
}
