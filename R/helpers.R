#' @export
invalid <- function(x){
  is.null(x) || is.na(x) || !nzchar(x)
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
