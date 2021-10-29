library(XIFF)
library(testthat)

test_that(
  desc = "class assignment",
  code = {
    ca <- classAssignment(c("a", "b", "c"), c("d", "e", "f"))
    expect_is(ca, "list")
    expect_is(ca, "classAssignment")
    expect_length(ca, 2)
    expect_named(ca, c("class1", "class2"))
    expect_equal(ca$class1, c("a", "b", "c"))
    expect_equal(ca$class2, c("d", "e", "f"))
    
    labels <- attr(ca, "labels")
    expect_is(labels, "list")
    expect_length(labels, 2)
    expect_named(labels, c("class1", "class2"))
    expect_equal(labels$class1, "class1")
    expect_equal(labels$class2, "class2")
    
    ca2 <- classAssignment(sensitive = c("a", "b", "c"), resistant = c("d", "e", "f"))
    expect_is(ca2, "list")
    expect_is(ca2, "classAssignment")
    expect_length(ca2, 2)
    expect_named(ca2, c("class1", "class2"))
    expect_equal(ca2$class1, c("a", "b", "c"))
    expect_equal(ca2$class2, c("d", "e", "f"))
    
    labels2 <- attr(ca2, "labels")
    expect_is(labels2, "list")
    expect_length(labels2, 2)
    expect_named(labels2, c("class1", "class2"))
    expect_equal(labels2$class1, "sensitive")
    expect_equal(labels2$class2, "resistant")
    
    ca3 <- classAssignment(sensitive = c("a", "b", "c"), resistant = NULL)
    expect_is(ca3, "list")
    expect_is(ca3, "classAssignment")
    expect_length(ca3, 2)
    expect_named(ca3, c("class1", "class2"))
    expect_equal(ca3$class1, c("a", "b", "c"))
    expect_null(ca3$class2)
    
    labels3 <- attr(ca3, "labels")
    expect_is(labels3, "list")
    expect_length(labels3, 2)
    expect_named(labels3, c("class1", "class2"))
    expect_equal(labels3$class1, "sensitive")
    expect_equal(labels3$class2, "resistant")
    
    expect_error(classAssignment())
    expect_error(classAssignment(c("a", "b", "c")))
    expect_error(classAssignment(c1 = c("a", "b", "c")))
    expect_error(classAssignment(c("a", "b"), c("c", "d"), c("e", "f")))
    expect_error(classAssignment(c1 = c("a", "b"), c2 = c("c", "d"), c3 = c("e", "f")))
    
    setClassItems(ca3, "class2") <- c("x", "y", "z")
    expect_is(ca3, "list")
    expect_is(ca3, "classAssignment")
    expect_length(ca3, 2)
    expect_named(ca3, c("class1", "class2"))
    expect_equal(ca3$class1, c("a", "b", "c"))
    expect_equal(ca3$class2, c("x", "y", "z"))
    
    setClassLabel(ca3, "class2") <- "dummy"
    labels3 <- attr(ca3, "labels")
    expect_is(labels3, "list")
    expect_length(labels3, 2)
    expect_named(labels3, c("class1", "class2"))
    expect_equal(labels3$class1, "sensitive")
    expect_equal(labels3$class2, "dummy")
    
    labels <- classIdToLabel(c("class1", "class2", "class2", "class1", "class1"), ca3)
    expect_is(labels, "character")
    expect_length(labels, 5)
    expect_equal(labels, c("sensitive", "dummy", "dummy", "sensitive", "sensitive"))
    
    labels <- classIdToLabel(factor(c("class1", "class2", "class2", "class1", "class1")), ca3)
    expect_is(labels, "factor")
    expect_length(labels, 5)
    expect_equal(
      labels, 
      factor(
        c("sensitive", "dummy", "dummy", "sensitive", "sensitive"), 
        levels = c("sensitive", "dummy")
      )
    )
    
    res <- getAssignmentDf(ca3)
    expect_is(res, "data.frame")
    expect_equal(nrow(res), 6)
    expect_equal(ncol(res), 2)
    expect_named(res, c("celllinename", "class"))
    expect_equal(rownames(res), c("a", "b", "c", "x", "y", "z"))
    expect_equal(res[["celllinename"]], c("a", "b", "c", "x", "y", "z"))
    expect_equal(
      res[["class"]], 
      factor(
        c("sensitive", "sensitive", "sensitive", "dummy", "dummy", "dummy"), 
        levels =  c("sensitive", "dummy")
      )
    )
    
    res2 <- getAssignmentDf(ca3, useLabels = FALSE)
    expect_is(res2, "data.frame")
    expect_equal(nrow(res2), 6)
    expect_equal(ncol(res2), 2)
    expect_named(res2, c("celllinename", "class"))
    expect_equal(rownames(res2), c("a", "b", "c", "x", "y", "z"))
    expect_equal(res2[["celllinename"]], c("a", "b", "c", "x", "y", "z"))
    expect_equal(
      res2[["class"]], 
      factor(
        c("class1", "class1", "class1", "class2", "class2", "class2"), 
        levels =  c("class1", "class2")
      )
    )
    
    res3 <- getAssignmentDf(ca3, useLabels = FALSE, returnFactor = FALSE)
    expect_is(res3, "data.frame")
    expect_equal(nrow(res3), 6)
    expect_equal(ncol(res3), 2)
    expect_named(res3, c("celllinename", "class"))
    expect_equal(rownames(res3), c("a", "b", "c", "x", "y", "z"))
    expect_equal(res3[["celllinename"]], c("a", "b", "c", "x", "y", "z"))
    expect_equal(
      res3[["class"]], 
      c("class1", "class1", "class1", "class2", "class2", "class2")
    )
    
    ca <- classAssignment(
      c1 = c("a", "b", "c"),
      c2 = c("d", "e", "f"),
      positiveClass = "c1"
    )
    expect_equal(classLabel2levels(getClassLabel(ca)), c("c1", "c2"))
    
    ca <- classAssignment(
      c1 = c("a", "b", "c"),
      c2 = c("d", "e", "f"),
      positiveClass = "c2"
    )
    expect_equal(classLabel2levels(getClassLabel(ca)), c("c2", "c1"))
    
    expect_error(classAssignment(1:5, 1:3), regexp = "1, 2, 3")
    expect_warning(classAssignment(c(1:5, 1:3), 6:10),
                   regexp = "Duplicated values found in the Class1")
    expect_warning(classAssignment(1:5, c(6:10, 6)),
                   regexp = "Duplicated values found in the Class2")
    expect_warning(classAssignment(c(1:5, 1:2), c(6:10, 6)),
                   regexp = "Duplicated values found in the Class")
    expect_error(classAssignment(c(1:5, 1:2), class2 = c(6:10, 6)),
                 "One of the class names is an empty string")
    
    ca <- suppressWarnings(classAssignment(c(1:5, 1:2), c(6:10, 6)))
    
    expect_equal(ca$class1, as.character(1:5))
    expect_equal(ca$class2, as.character(6:10))
       
  }
)

test_that(
  desc = "class assignment - classAssignment2df",
  code = {
    
    ca <- classAssignment(cl1 = 1, cl2 = 2)
    
    dt <- structure(list(
      celllinename = c("1", "2"),
      class = c("cl1", "cl2")
    ),
    class = "data.frame",
    row.names = c("1", "2"))
    
    expect_equal(classAssignment2df(ca), dt)
    
    # warning once per session
    expect_warning(dt2 <- classAssignment2df(list(c1 = 1, c2 = 2)))
     
  }
)
