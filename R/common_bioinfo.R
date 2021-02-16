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

  assignment <- predFun(m$model, df)
  if (is.list(assignment)){
    assignment <- unlist(assignment, use.names = FALSE)
  }
  assignment <- as.character(assignment)

  if (length(assignment) == 0 || any(! assignment %in% c("class1", "class2", NA))){
    shinyBS::createAlert(
      session = session,
      anchorId = errorId,
      content = "Incorrect prediction format. Did you use a custom model? Please contact the app author.",
      style = "danger"
    )
    return()
  }

  assignment
}
