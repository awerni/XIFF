# Class assignment ------------------------------------------------------------
#' Class assignment
#' 
#' Helper class that holds class samples with class labels
#' 
#' @param ... named character vectors
#' @param positiveClass name of the positive class. If NULL (default), the first
#'        class is used.
#' 
#' @return classAssignment object
#' @export
#' 
#' @examples 
#' classAssignment(a = c("a","b"), b = c("x", "y", "z"))
#' classAssignment(a = c("a","b"), b = c("x", "y", "z"), positiveClass = "b")
#' 
#' cs <- list(a = c("a","b"), b = c("x", "y", "z"))
#' classAssignment(cs)
#' classAssignment(cs, positiveClass = "b")
#' 
classAssignment <- function(..., positiveClass = NULL) {
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
  } else {
    
    stopifnot("The classes names must be unique." =
              length(unique(labels)) == 2)
    stopifnot("One of the class names is an empty string" =
              all(nchar(labels) > 0))
    
  }
  
  if(!is.null(positiveClass)) {
    
    if(positiveClass %in% labels) {
      
      # for positiveClass == labels[1] nothing needs to be done
      if(positiveClass == labels[2]) {
        labels  <- rev(labels)
        theDots <- rev(theDots)
      }
      
    } else {
      stop("`positiveClass` must be one of the class labels.")
    }
    
  }
  
  theDots <- mapply(theDots, labels, 1:2, FUN = function(x, l, i) {
    if(anyDuplicated(x)) {
      warning(glue::glue("Duplicated values found in the Class{i} ({l})."), 
              " Only the unique values will be used.")
    }  
    x <- unique(as.character(x))
    if(length(x) == 0) NULL else x # enforce NULL when length of 0
  }, SIMPLIFY = FALSE)
  
  
  if(anyDuplicated(theDots[[1]])) {
    warning(glue::glue("Duplicated values found in Class1 ({labels[[1]]})"))
  }
  
  res <- structure(
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
  
  duplicated <- intersect(res$class1, res$class2)
  
  if(length(duplicated) > 0) {
    entries <-
      substr(paste(duplicated, collapse = ", "), 1 , getOption("width") * 0.8)
    stop("Duplicated values between classes are prohibited: ", entries)
  }
  
  res
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


#' Convert classId to class labels
#'
#' @param x vector with class labels (class1/class2)
#' @param ca classAssignment object
#'
#' @return vector containing the labels from classAssignment
#' @export
#'
#' @examples
#' 
#' ca <- classAssignment(sensitive = c("a", "b", "c"), resistant = NULL)
#' classIdToLabel(c("class1", "class2", "class2", "class1", "class1"), ca)
#' 
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
#' 
#' @param x any R object
#' @param y defalut value used if x is NULL
#' 
#' @name op-null-default
#' @export
#' @examples 
#' 
#' 1 %||% 2
#' NULL %||% 2
#' 
`%||%` <- function(x, y){
  if (rlang::is_null(x) || length(x) == 0)
    y
  else x
}


#' Test if value is empty string, NA or NULL
#'
#' @param x any R object
#'
#' @return logical value
#' @export
#'
#' @examples
#' 
#' invalid("")
#' invalid(NULL)
#' invalid(NA)
#' 
invalid <- function(x){
  is.null(x) || is.na(x) || !nzchar(x)
}


#' Drop NULL values from list
#'
#' @param x list 
#'
#' @return list x without NULL values
#' @export
#'
#' @examples
#' 
#' ls <- list(1, NULL, 2)
#' dropNulls(ls)
#' 
dropNulls <- function(x){
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}


#' Stack Class Assignment object
#'
#' @param sampleClasses classAssignment object
#' @param classLabel 
#' @param return_factor 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' ca <- exampleClassAssigment()
#' stackClasses(ca) %>% str
#' stackClasses(ca, return_factor = TRUE) %>% str
#' 
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

  tagList(
    FutureManager::fmRunButton(
      inputId = inputId,
      fm = fm,
      defaultValue = defaultValue,
      blocked = condition
    ),
    uiOutput(paste0(inputId, "ClassLabel"))
  )
}

#' @export
fmButtonBothClasses <- function(inputId, fm, cs, defaultValue = FALSE){
  condition <- length(cs$class1) == 0 || length(cs$class2) == 0

  tagList(
    FutureManager::fmRunButton(
      inputId = inputId,
      fm = fm,
      defaultValue = defaultValue,
      blocked = condition
    ),
    uiOutput(paste0(inputId, "ClassLabel"))
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


#' Convert the input of Text Area into Vector
#'
#' @param content content of textArea
#'
#' @return character vector, or NULL if content is empty
#' @export
#'
#' @examples
#' 
#' textAreaContentToVector("")
#' textAreaContentToVector("A\nB\nC")
#' 
textAreaContentToVector <- function(content){
  ret <- if (stringr::str_length(content) > 0) as.vector(sapply(strsplit(content,'\n'), stringr::str_trim)) else c()
  if (any(ret == "")) ret <- ret[ret != ""]
  return(ret)
}


#' Check if both classes in classAssigment object are not empty
#'
#' @param cs classAssigment object
#'
#' @return TRUE if class1 and class2 are not empty, FALSE otherwise
#'
#' TODO: check the intent
#'
#' @export
#' @examples
#' 
#' ca <- exampleClassAssigment()
#' checkClassSelection(ca)
#' ca$class2 <- c()
#' checkClassSelection(ca)
#' 
checkClassSelection <- function(cs) {
  (length(cs$class1) > 0 && length(cs$class1)) > 0
}


#' Number of common entries in two vectors
#'
#' @param x first vector
#' @param y second vector
#'
#' @return interger denoting the number of common items in both vectors
#' @export
#'
#' @examples
#' 
#' n_common(1:4,2:5)
#' n_common(1:4,5:10)
#' 
n_common <- function(x, y){
  length(intersect(x, y))
}


#' Get Hallmark Gene Set Choices
#'
#' @param geneSets vector with HALLMARK genes setes names
#'
#' @return
#' @export
#'
#' @examples
#' 
#' if(require("CLIFF")) {
#' 
#'    dt <- CLIFF::differentialHallmarkSets(CLIFF::exampleClassAssigment()) %>%
#'      head %>% select(gene_set)
#'    names(getHallmarkGeneSetChoices(dt$gene_set))
#' 
#' }
#' 
getHallmarkGeneSetChoices <- function(geneSets){
  if (length(geneSets) > 0){
    structure(geneSets, names = gsub("^HALLMARK_", "", geneSets))
  }
}


#' Get HTML tags with links
#'
#' @param location 
#' @param species 
#'
#' @return
#' @export
#' @rdname get-links-a-tags
#'
getEnsemblLocationLink <- function(location, species = "human"){
  species <- getEnsemblSpecies(species)
  glue::glue('<a href="https://www.ensembl.org/{species}/Location/View?db=core;r={location}" target="_blank">{location}</a>')
}

#' @export
#' @rdname get-links-a-tags
getEnsemblGeneLink <- function(gene, species = "human"){
  species <- getEnsemblSpecies(species)
  glue::glue('<a href="https://www.ensembl.org/{species}/Gene/Summary?db=core;g={gene}" target="_blank">{gene}</a>')
}

#' @export
#' @rdname get-links-a-tags
getEnsemblTranscriptLink <- function(transcript, species){
  species <- getEnsemblSpecies(species)
  glue::glue('<a href="https://www.ensembl.org/{species}/Transcript/Summary?db=core;t={transcript}" target="_blank">{transcript}</a>')
}

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
#' @rdname get-links-a-tags
getGeneSetLink <- function(geneset, msigDBLink, rawGeneset = NULL){
  if(is.null(rawGeneset)) rawGeneset <- geneset
  paste0('<a href="', msigDBLink, rawGeneset, '" target="_blank">', geneset, '</a>')
}


#' Check if Package is Available
#'
#' @param name package name
#'
#' @return logical, \code{TRUE} if package is available
#' @export
#'
#' @examples
#' 
#' packageInstalled("XIFF")
#' packageInstalled("XIFFFF")
#' 
packageInstalled <- function(name){
  is.character(name) && name %in% rownames(installed.packages())
}


#' mapply with vector names as argument
#'
#' @param X named vector 
#' @param FUN function with at least two parameters
#' @param ... other arguments passed to mapply
#'
#' @return vector resulted from mapply
#' @export
#'
#' @examples
#' 
#' x <- setNames(1:3, c("A", "B", "C"))
#' napply(x, function(x, name) paste(name, x^2, sep = ": "))
#' 
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

#' Example Class Assignment to be used in Examples
#'
#' @return classAssignment object
#' @export
#'
#' @examples
#' 
#' exampleClassAssigment()
#' 
exampleClassAssigment <- function() {
  structure(
    list(
      class1 = c("fb1b6", "2abec", "0ed25", "972d2", "45f5d"),
      class2 = c("102a0", "65d7c", "0a389", "f7f21", "8ad8a", "e1942")
    ),
    labels = list(class1 = "class1", class2 = "class2"),
    class = c("classAssignment", "list")
  )
}

#' Example Data Frame to be used in Examples
#'
#' @return data.frame object
#' @export
#'
#' @examples
#' 
#' exampleDataFrame()
#' 
exampleDataFrame <- function() {
  structure(
    list(
      celllinename = c(
        "fb1b6", "2abec", "0ed25", "972d2",
        "45f5d", "102a0", "65d7c", "0a389",
        "f7f21", "8ad8a", "e1942"
      ),
      tpm = c(
        11.62, 27.44, 9.92, 11.02,
        39.69, 13.08, 12.06, 13.72,
        9.31, 18.75, 9.97
      )
    ),
    row.names = c(NA, 11L),
    class = c("ClassAssigmentInAttribute",
              "data.frame")
  )
}
