# Class assignment ------------------------------------------------------------
#' Class assignment
#' 
#' Helper class that holds class samples with class labels
#' 
#' @param ... named character vectors
#' @return classAssignment object
#' @export
classAssignment <- function(...){
  theDots <- list(...)
  
  if (length(theDots) == 1 && is.list(theDots[[1]])){
    theDots <- theDots[[1]]
  }
  
  if (length(theDots) != 2){
    stop("please provide 2 sample name vectors")
  }
  
  labels <- names(theDots)
  if (is.null(labels)){
    labels <- c("class1", "class2")
  }
  
  structure(
    list(
      class1 = theDots[[1]],
      class2 = theDots[[2]]
    ),
    labels = list(
      class1 = labels[1],
      class2 = labels[2]
    ),
    class = c("classAssignment", "list")
  )
}

#' @export
makeClassAssignment <- function(sampleClasses, classLabel){
  
  shiny::validate(
    shiny::need(
      !is.null(sampleClasses),
      message = "sampleClasses cannot be null."
    ),
    shiny::need(
      !is.null(classLabel),
      message = "classLabel cannot be null."
    )
  )
  
  if(is.character(classLabel)) {
    
    if(length(classLabel) == 2) {
      classLabel <- list(
        class1_name = classLabel[1],
        class2_name = classLabel[2]
      )
    } else if(length(classLabel) == 1) {
      
      if(!classLabel %in% names(sampleClasses)) {
        stop("classLabel must be one of the names of sampleClasses")
      }
      levels <- names(sampleClasses)
      classLabel <- c(classLabel, levels[classLabel != levels])
      classLabel <- list(
        class1_name = classLabel[1],
        class2_name = classLabel[2]
      )
      
    } else {
      stop("classLabel should be character vector",
           " with 1 or 2 elements or a list")
    }
    
    sampleClasses <- sampleClasses[classLabel2levels(classLabel)]
    names(sampleClasses) <- c("class1", "class2")
  }
  
  
  # make sure the order is fine
  sampleClasses <- sampleClasses[c("class1", "class2")]
  names(sampleClasses) <- c(classLabel$class1_name, classLabel$class2_name)
  classAssignment(sampleClasses)
}

#' @export
print.classAssignment <- function(x){
  dict <- attr(x, "labels")
  n1 <- length(x$class1)
  n2 <- length(x$class2)
  cat("Class assignment object\n")
  cat(paste0("Class1 (", dict$class1, "): ", n1, " items <-- Positive Class\n"))
  cat(paste0("Class2 (", dict$class2, "): ", n2, " items\n"))
}

#' @export
classIdToLabel <- function(x, ca){
  returnFactor <- is.factor(x)
  stopifnot(
    is(ca, "classAssignment"),
    is.character(x) || returnFactor
  )
  
  dict <- attr(ca, "labels")
  labels <- c(dict$class1, dict$class2)
  res <- ifelse(x == "class1", labels[1], labels[2])
  
  if (returnFactor){
    res <- factor(res, levels = labels)
  }
  
  res
}


#' Get class label list from classAssignment object.
#'
#' @param ca classAssignment object
#'
#' @export
getClassLabel <- function(ca) {
  stopifnot(
    is(ca, "classAssignment")
  )
  
  labels <- attr(ca, "labels")
  names(labels) <- paste0(names(labels), "_name")
  labels
}

#' @export
renameClassIdToLabel <- function(df, column, ca){
  if (is(ca, "classAssignment")){
    column <- rlang::sym(column)
    df %>% mutate(!!column := classIdToLabel(!!column, ca))
  } else {
    df
  }
}

#' @export
`setClassItems<-` <- function(ca, classId, value = NULL){
  stopifnot(
    is(ca, "classAssignment"),
    is.character(classId) && length(classId) == 1 && classId %in% c("class1", "class2"),
    is.null(value) || is.character(value)
  )
  
  ca[classId] <- list(value)
  ca
}

#' @export
`setClassLabel<-` <- function(ca, classId, value){
  stopifnot(
    is(ca, "classAssignment"),
    is.character(classId) && classId %in% c("class1", "class2"),
    is.character(value) && length(value) == 1
  )
  
  labels <- attr(ca, "labels")
  labels[[classId]] <- value
  attr(ca, "labels") <- labels
  ca
}

#' @export
getAssignmentDf <- function(ca, useLabels = TRUE, returnFactor = TRUE){
  stopifnot(is(ca, "classAssignment"))
  
  classLabel <- if (useLabels){
    labels <- attr(ca, "labels")
    list(
      class1_name = labels$class1, 
      class2_name = labels$class2
    )
  }
  
  stackClasses(
    sampleClasses = ca,
    classLabel = classLabel,
    return_factor = returnFactor
  )
}

# Others ----------------------------------------------------------------------
#' Null default
#' @name op-null-default
#' @export
`%||%` <- function(x, y){
  if (rlang::is_null(x) || length(x) == 0)
    y
  else x
}

#' @export
invalid <- function(x){
  is.null(x) || is.na(x) || !nzchar(x)
}

#' @export
dropNulls <- function(x){
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

#' @export
stackClasses <- function(sampleClasses, classLabel = NULL, return_factor = FALSE) {
  colname <- getOption("xiff.column")

  sampleClasses[sapply(sampleClasses, is.null)] <- NULL
  assignment <- sampleClasses %>%
    tibble::enframe(name = "class", value = colname) %>%
    tidyr::unnest(cols = !!colname) %>%
    as.data.frame()

  if (!is.null(classLabel)) {
    my_labels <- utils::stack(classLabel) %>%
      mutate(class = gsub("_name$", "", ind)) %>%
      select(-ind) %>%
      arrange(class)
    assignment <- assignment %>%
      inner_join(my_labels, by = "class") %>%
      select(-class) %>%
      rename(class = values)
    if (return_factor) assignment$class <- factor(assignment$class, levels = my_labels$values)
  } else {
    if (return_factor) assignment$class <- factor(assignment$class, levels = c("class1", "class2"))
  }
  
  assignment <- assignment[, c(colname, "class")]
  rownames(assignment) <- assignment[[colname]]
  return(assignment)
}

#' @export
fmButtonOneClass <- function(inputId, fm, cs, defaultValue = FALSE){
  condition <- length(cs$class1) == 0 && length(cs$class2) == 0

  FutureManager::fmRunButton(
    inputId = inputId,
    fm = fm,
    defaultValue = defaultValue,
    blocked = condition
  )
}

#' @export
fmButtonBothClasses <- function(inputId, fm, cs, defaultValue = FALSE){
  condition <- length(cs$class1) == 0 || length(cs$class2) == 0

  FutureManager::fmRunButton(
    inputId = inputId,
    fm = fm,
    defaultValue = defaultValue,
    blocked = condition
  )
}

#' @export
getPropertyFractions <- function(data, annotation, annotationFocus, prop1, prop2){
  focusLevels <- as.character(unique(annotationFocus[[prop1]]))

  available <- data %>%
    mutate(prop1 = as.character(prop1)) %>%
    group_by(class, prop1, prop2) %>%
    summarize(n = dplyr::n())
  notOthers <- setdiff(available[["prop1"]], "other")

  annotation %>%
    select(prop1 = !!prop1) %>%
    filter(prop1 %in% focusLevels) %>%
    mutate(
      prop1 = as.character(prop1),
      prop1 = ifelse(is.na(prop1), "NA", prop1),
      prop1 = ifelse(prop1 %in% notOthers, prop1, "other")
    ) %>%
    group_by(prop1) %>%
    summarize(nTotal = dplyr::n()) %>%
    left_join(available, ., by = "prop1") %>%
    filter(!is.na(class)) %>%
    mutate(percent = n/nTotal * 100) %>%
    select(class, prop1, prop2, percent)
}

#' @export
get_label <- function(n) {
  paste0(n, " ", getOption("xiff.label"), ifelse(n != 1, "s", ""))
}

#' @export
isDifferent <- function(x, y){
  !isTRUE(all.equal(x, y))
}

#' @export
replaceClassLabels <- function(df, cl) {
  names(cl) <- gsub("_name", "", names(cl))
  myLabel <- stack(cl) %>% rename(class = values, classold = ind) %>%
    mutate(class = as.factor(class), classold = as.character(classold))
  suppressWarnings(df <- df %>% rename(classold = class) %>% left_join(myLabel, by = "classold") %>% select(-classold))
  return(df)
}

#' Add tumor types
#' 
#' A simple function that adds `tumortype` column to the data
#' 
#' @param data data.frame, `celllinename` column required
#' @param anno data.frame, cell line annotation
#' 
#' @return data data.frame with the additional column
#' @export
addTumortypes <- function(data, anno){
  strColname <- getOption("xiff.column")
  colname <- rlang::sym(strColname)
  
  tt <- anno %>% select(!!colname, tumortype)
  data %>% left_join(tt, by = strColname)
}

#' @export
textAreaContentToVector <- function(content){
  ret <- if (stringr::str_length(content) > 0) as.vector(sapply(strsplit(content,'\n'), stringr::str_trim)) else c()
  if (any(ret == "")) ret <- ret[ret != ""]
  return(ret)
}

#' @export
checkClassSelection <- function(cs) {
  (length(cs$class1) > 0 && length(cs$class1)) > 0
}

#' @export
n_common <- function(x, y){
  length(intersect(x, y))
}

#' @export
getHallmarkGeneSetChoices <- function(geneSets){
  if (length(geneSets) > 0){
    structure(geneSets, names = gsub("^HALLMARK_", "", geneSets))
  }
}

#' @export
getEnsemblLocationLink <- function(location, species = "human"){
  species <- getEnsemblSpecies(species)
  glue::glue('<a href="https://www.ensembl.org/{species}/Location/View?db=core;r={location}" target="_blank">{location}</a>')
}

#' @export
getEnsemblGeneLink <- function(gene, species = "human"){
  species <- getEnsemblSpecies(species)
  glue::glue('<a href="https://www.ensembl.org/{species}/Gene/Summary?db=core;g={gene}" target="_blank">{gene}</a>')
}

#' @export
getEnsemblTranscriptLink <- function(transcript, species){
  species <- getEnsemblSpecies(species)
  glue::glue('<a href="https://www.ensembl.org/{species}/Transcript/Summary?db=core;t={transcript}" target="_blank">{transcript}</a>')
}

#' @export
getEnsemblSpecies <- function(species){
  species <- switch(
    EXPR = species,
    human = "Homo_sapiens",
    mouse = "Mus_musculus"
  )
  stopifnot(!is.null(species))
  species
}

#' @export
getGeneSetLink <- function(geneset, msigDBLink, rawGeneset = NULL){
  if(is.null(rawGeneset)) rawGeneset <- geneset
  paste0('<a href="', msigDBLink, rawGeneset, '" target="_blank">', geneset, '</a>')
}

#' @export
packageInstalled <- function(name){
  is.character(name) && name %in% rownames(installed.packages())
}

#' @export
napply <- function(X, FUN, ...){
  if (length(X) == 0) return(list())
  
  n <- names(X)
  if (is.null(n)){
    n <- seq_along(X)
  }
  
  mapply(
    FUN = FUN,
    X, n,
    ...
  )
}
