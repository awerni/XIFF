#' @export
getPredictionSummary <- function(celllinenames, preds, refs, positive_model, positive_cs, classes, 
                                 classes_model, classes_cs, annoFocus){
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
  
  anno <- annoFocus %>% select(celllinename, tumortype)
  
  df <- tibble(
    celllinename = celllinenames,
    predicted = preds_pn,
    reference = refs_pn,
    predicted_original = ifelse(preds == "class1", classes_model[1], classes_model[2]),
    reference_original = ifelse(refs == "class1", classes_cs[1], classes_cs[2]),
    correct = preds_pn == refs_pn,
  ) %>%
    left_join(anno, by = "celllinename") %>%
    relocate(tumortype, .after = celllinename)
  
  
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
validateModel <- function(m, validationSet, anno){
  df <- getDataForModel(
    assignment = validationSet,
    features = m$model$bestFeatures
  )
  
  refs <- df$class
  celllinenames <- df$celllinename
  df <- df %>% select(-class, -celllinename)
  modelReady <- loadMachineLearningModel(object = m$model)
  preds <- predictFromModel(modelReady, df)
  
  cl <- unlist(modelReady$classLabel, use.names = FALSE)
  
  getPredictionSummary(
    celllinenames = celllinenames,
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
