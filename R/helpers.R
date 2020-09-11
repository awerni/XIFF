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
