#' Get class label list from classAssignment object.
#'
#' @param ca classAssignment object
#'
#' @export
#' @rdname ClassAssigmentUtils
#' 
#' @examples 
#' 
#' ca <- classAssignment(cl1 = c("setosa"), cl2 = c("versicolor", "virginica"))
#' iris2 <- addClassAssigmentAttribute(iris, ca)
#' 
#' getClassLabel(ca)
#' getClassLabel(iris2)
#' getClassLabelVector(ca)
#' getClassLabelVector(iris2)
#' 
getClassLabel <- function(ca) {
  
  if(!any(class(ca) %in% c("classAssignment", "ClassAssigmentInAttribute"))) {
    stop("`ca` must be `classAssignment` or `ClassAssigmentInAttribute` object")
  }
  
  if(inherits(ca, "ClassAssigmentInAttribute")) {
    ca <- getClassAssigmentAttribute(ca)
  }
  
  labels <- attr(ca, "labels")
  names(labels) <- paste0(names(labels), "_name")
  labels
}

#' @export
#' @rdname ClassAssigmentUtils
getClassLabelVector <- function(ca) {
  cl <- getClassLabel(ca)
  c(cl$class1_name, cl$class2_name)
}

#' Utility function for working with classAssignment objects.
#'
#' @param ca character vector or classAssignment object
#'
#' @export
#' @rdname ClassAssigmentUtils
#' @details This function is dedicated mostly for CLIFF/XIFF developers.
#' 
unlistClassAssignment <- function(ca) {
  
  if(!(is.character(ca) || inherits(ca, "classAssignment"))) {
    stop("`ca` must be character vector or `classAssignment` object.")
  }
  
  if(inherits(ca, "classAssignment")) {
    ca <- unlist(ca)
  }
  ca
}

#' @param ca classAssigment object
#' @rdname ClassAssigmentUtils
#' @export
addClassAssigmentAttribute <- function(x, ca) {
  attr(x, "ClassSelection") <- ca
  attr(x, "class") <- unique(c("ClassAssigmentInAttribute", class(x)))
  x
}

#' @param x an ClassAssigmentInAttribute object or object to be transformed to 
#'          ClassAssigmentInAttribute.
#' @rdname ClassAssigmentUtils
#' @export
getClassAssigmentAttribute <- function(x) {
  
  if(!inherits(x, "ClassAssigmentInAttribute")) {
    stop("x is not an `ClassAssigmentInAttribute`.")
  }
  attr(x, "ClassSelection")
}
