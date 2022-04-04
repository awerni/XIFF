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
  
  isClassAssignmentList <- function(ca) {
    # many modules in CLIFF still use the old interface which was based on the list
    is.list(ca) && all(names(ca) %in% c("class1", "class2"))
  }
  
  if(!(is.character(ca) || inherits(ca, "classAssignment") || isClassAssignmentList(ca))) {
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



#' Functions to check if an object is a classAssignment
#'
#' @param x any R object
#'
#' @return logical value, TRUE if x is an classAssignment object
#' @export
#' @rdname is.classAssignment
#' 
#' @details Note that this functions uses 'duck typing', so if an object 
#' can be used without any conversion as a \code{classAssignment}, then
#' the returned value is \code{TRUE}.
#'
#' @examples
#' 
#' is.classAssignment(list()) # FALSE
#' is.classAssignment(list(x = 1, y = 2)) # FALSE
#' is.classAssignment(classAssignment(list(x = 1, y = 2))) # TRUE - classAssignment object
#' is.classAssignment(list(class1 = 1, class2 = 2)) # TRUE - duck typing
#' 
is.classAssignment <- function(x) {
  
  # note that the second condition is
  # If it walks like a duck and it quacks like a duck, then it must be a duck
  
  inherits(x, "classAssignment") || 
  (is.list(x) && length(x) == 2 && all(names(x) %in% c("class1", "class2")))
}

#' @export
#' @rdname is.classAssignment
assertClassAssignment <- function(x) {
  if(!is.classAssignment(x)) {
    cl <- class(x)[1]
    
    
    msg <- glue::glue("The '{cl}' class was used but the 'classAssignment' is expected.")
    if(is.list(cl)) {
      msg <- paste(msg,
                   "In most cases lists can easily be transformed", 
                   "to 'classAssignment' object by calling `classAssignment(x)`")
    }
    
    stop(msg)
  }
}
