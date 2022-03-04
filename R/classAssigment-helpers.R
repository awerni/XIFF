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

#' @rdname ClassAssigmentUtils
#' @export
getClassAssigmentAttributeIfNull <- function(ca, x) {
  if(!is.null(ca)) return(ca)
  if(!inherits(x, "ClassAssigmentInAttribute")) {
    stop("Cannot infer the classAssigment from input data.",
         " Please provide the `ca` parameter on your own.")
  }
  
  attr(x, "ClassSelection")
}

#' Convert classAssigment to data.frame containing class assignment column
#'
#' @param ca classAssigment object
#'
#' @return data.frame
#' @export
#'
#' @examples
#' 
#' ca <- classAssignment(cl1 = 1:5, cl2 = 6:10)
#' classAssignment2df(ca)
#' 
classAssignment2df <- function(ca) {
  
  if(inherits(ca, "list") && !inherits(ca, "classAssignment")) {
    
    if (length(ca) > 2){
      stop("`ca` must contain 2 list elements")
    }
    
    rlang::warn(
      paste0(
        "`ca` is a list but not a classAssignment.",
        " classAssignment2df can work with lists but it is ",
        " recommended to use `classAssignment` objects."),
      .frequency = "once",
      .frequency_id = "xiff_classAssignment2df_list"
    )
    
    ca <- classAssignment(ca)
  }
  
  if(!inherits(ca, "classAssignment")) {
    stop("`ca` must be a `list` or `classAssignment` object.")
  }
  
  labels <- getClassLabel(ca)
  
  stackClasses(ca, classLabel = labels)
}
