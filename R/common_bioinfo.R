#' @export
loadMachineLearningModel <- function(filepath, errorId, session, object = NULL){
  x <- if (is.null(object)){
    readRDS(filepath)
  } else {
    object
  }

  if (is(x, "machineLearningResult")){
    modelLibrary <- x$library
    isPackageInstalled <- packageInstalled(modelLibrary)

    if (isPackageInstalled){
      modelClass <- class(x$model)
      predFun <- getS3method(
        f = "predict",
        class = modelClass,
        envir = asNamespace(modelLibrary),
        optional = TRUE
      )

      if (is.null(predFun)){
        errorMsg <- paste0("No predict() method found for ", modelClass, "-class object")
      } else {
        x[[".predFun"]] <- predFun
        return(x)
      }
    } else {
      errorMsg <- paste0("Package '", modelLibrary, "' is not available.")
    }
  } else {
    errorMsg <- "Incorrect input file. Please provide a file downloaded from the machine learning tab"
  }

  shinyBS::createAlert(
    session = session,
    anchorId = errorId,
    content = errorMsg,
    style = "danger"
  )

  return()
}

#' @export
predictFromModel <- function(m, df, errorId, session){
  predFun <- m[[".predFun"]]

  showPredictError <- function(msg){
    shinyBS::createAlert(
      session = session,
      anchorId = errorId,
      content = msg,
      style = "danger"
    )
  }

  missingFeatures <- setdiff(m$bestFeatures, names(df))
  if (length(missingFeatures) > 0){
    showPredictError(paste("Missing features:", paste(missingFeatures, collapse = ", ")))
    return()
  }

  df <- preparePredictionData(df)
  assignment <- try(predFun(m$model, df))
  if (is(assignment, "try-error")){
    showPredictError("Error during prediction. Please contact the app author.")
    return()
  }

  if (is.list(assignment)){
    assignment <- unlist(assignment, use.names = FALSE)
  }
  assignment <- as.character(assignment)

  if (length(assignment) == 0 || any(! assignment %in% c("class1", "class2", NA))){
    showPredictError("Incorrect prediction format. Did you use a custom model? Please contact the app author.")
    return()
  }

  assignment
}

preparePredictionData <- function(df){
  colname <- getOption("xiff.column")

  if (colname %in% names(df)){
    attr(df, "mlItems") <- df[[colname]]
    df[[colname]] <- NULL
  }

  df
}

#' @export
dropUnbalancedTumortypes <- function(AnnotationFocus, classSelection){
  anno <- AnnotationFocus()
  if (is.null(anno) || nrow(anno) == 0) return()
  
  cs <- reactiveValuesToList(classSelection)
  balancedTT <- getValidTumorTypes(cs, anno)
  
  colname <- getOption("xiff.column")
  colname <- rlang::sym(colname)
  
  validItems <- anno %>% 
    filter(tumortype %in% balancedTT) %>%
    pull(!!colname)
  
  classSelection$class1 <- intersect(classSelection$class1, validItems)
  classSelection$class2 <- intersect(classSelection$class2, validItems)
}

#' Get valid tumor types
#' 
#' This function returns tumor types that are available in the data for both classes
#' 
#' @param cs list, class selection
#' @param anno data.frame, item annotation
#' 
#' @return character vector of valid tumortypes
#' @export
getValidTumorTypes <- function(cs, anno){
  if (is.null(anno) || nrow(anno) == 0) return()
  
  colname <- getOption("xiff.column")
  
  stackClasses(cs) %>%
    left_join(anno, by = colname) %>%
    group_by(class, tumortype) %>%
    summarise(n = dplyr::n(), .groups = "drop") %>%
    group_by(tumortype) %>%
    summarize(ok = dplyr::n() > 1, .groups = "drop") %>% # must be present in both classes
    filter(ok) %>%
    pull(tumortype) %>%
    as.character()
}
