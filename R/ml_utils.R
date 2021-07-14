#' @export
getPredictionSummary <- function(
  items,
  preds,
  refs,
  positive_model,
  positive_cs,
  classes, 
  classes_model,
  classes_cs,
  annoFocus,
  itemColumn = getOption("xiff.column")
  ){
  
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
  
  df <- tibble(
    !!itemColumnSymbol := items,
    predicted = preds_pn,
    reference = refs_pn,
    predicted_original = ifelse(preds == "class1", classes_model[1], classes_model[2]),
    reference_original = ifelse(refs == "class1", classes_cs[1], classes_cs[2]),
    correct = preds_pn == refs_pn,
  ) %>%
    left_join(anno, by = itemColumn) %>%
    relocate(tumortype, .after = !!itemColumnSymbol)
  
  
  list(
    res = res,
    data = df
  )
}

#' @export
getPerformanceDataFrame <- function(t){
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

#' @export
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

#' @export
validateModel <- function(
  m,
  validationSet,
  anno,
  itemColumn = getOption("xiff.column")
){
  itemColumnSymbol <- rlang::sym(itemColumn)
  
  df <- getDataForModel(
    assignment = validationSet,
    features = m$bestFeatures
  )
  
  refs <- df$class
  items <- df[[itemColumn]]
  df <- df %>% select(-class, -!!itemColumnSymbol)
  preds <- predict(m, newdata = df)
  
  cl <- unlist(m$classLabel, use.names = FALSE)
  
  getPredictionSummary(
    items = items,
    preds = preds,
    refs = refs,
    positive_model = "class1",
    positive_cs = "class1",
    classes = c("positive", "negative"),
    classes_model = cl,
    classes_cs = cl,
    annoFocus = anno
  )
}

#' gatherPredictionResults
#' 
#' @details TODO: is this function even needed?
#' 
#' @export
#' 
#' 
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
  shiny::validate(shiny::need(inherits(model, "XiffMachineLearningResult"), paste0(
    "Inccorect model format. Please provide a model created using CLIFF application",
    " or XIFF::buildMachineLearning (if you were using R console to create model)")))
}
