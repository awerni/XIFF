#' Null default
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
      dplyr::mutate(class = gsub("_name$", "", ind)) %>%
      dplyr::select(-ind) %>%
      dplyr::arrange(class)
    assignment <- assignment %>%
      dplyr::inner_join(my_labels, by = "class") %>%
      dplyr::select(-class) %>%
      dplyr::rename(class = values)
    if (return_factor) assignment$class <- factor(assignment$class, levels = my_labels$values)
  } else {
    if (return_factor) assignment$class <- factor(assignment$class, levels = c("class1", "class2"))
  }
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
    dplyr::mutate(prop1 = as.character(prop1)) %>%
    dplyr::group_by(class, prop1, prop2) %>%
    dplyr::summarize(n = dplyr::n())
  notOthers <- setdiff(available[["prop1"]], "other")

  annotation %>%
    dplyr::select(prop1 = !!prop1) %>%
    dplyr::filter(prop1 %in% focusLevels) %>%
    dplyr::mutate(
      prop1 = as.character(prop1),
      prop1 = ifelse(is.na(prop1), "NA", prop1),
      prop1 = ifelse(prop1 %in% notOthers, prop1, "other")
    ) %>%
    dplyr::group_by(prop1) %>%
    dplyr::summarize(nTotal = dplyr::n()) %>%
    dplyr::left_join(available, ., by = "prop1") %>%
    dplyr::filter(!is.na(class)) %>%
    dplyr::mutate(percent = n/nTotal * 100) %>%
    dplyr::select(class, prop1, prop2, percent)
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
  suppressWarnings(df <- df %>% rename(classold = class) %>% left_join(myLabel, by = "classold") %>% dplyr::select(-classold))
  return(df)
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
  structure(geneSets, names = gsub("^HALLMARK_", "", geneSets))
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
packageInstalled <- function(name){
  is.character(name) && name %in% rownames(installed.packages())
}
