library(XIFF)
library(testthat)

test_that(
  desc = "Classification table is generated properly",
  code = {
    class1 <- c("a", "b", "c", "d")
    class2 <- c("e", "f", "g", "h")
    
    obj <- prepareClassificationTable(class1, class2)
    expect_is(obj, "data.frame")
    expect_equal(nrow(obj), 8)
    expect_equal(colnames(obj), c("celllinename", "class"))
    expect_equal(rownames(obj), c("a", "b", "c", "d", "e", "f", "g", "h"))
    expect_equal(obj[["celllinename"]], c("a", "b", "c", "d", "e", "f", "g", "h"))
    expect_equal(obj[["class"]], c(
      "class1", "class1", "class1", "class1", 
      "class2", "class2", "class2", "class2"
    ))
    
    obj2 <- prepareClassificationTable(class2, class1)
    expect_is(obj2, "data.frame")
    expect_equal(nrow(obj2), 8)
    expect_equal(colnames(obj2), c("celllinename", "class"))
    expect_equal(rownames(obj2), c("e", "f", "g", "h", "a", "b", "c", "d"))
    expect_equal(obj2[["celllinename"]], c("e", "f", "g", "h", "a", "b", "c", "d"))
    expect_equal(obj2[["class"]], c(
      "class1", "class1", "class1", "class1", 
      "class2", "class2", "class2", "class2"
    ))
    
    obj3 <- prepareClassificationTable(class1, class2, addRownames = FALSE)
    expect_is(obj3, "data.frame")
    expect_equal(nrow(obj3), 8)
    expect_equal(colnames(obj3), c("celllinename", "class"))
    expect_equal(rownames(obj3), c("1", "2", "3", "4", "5", "6", "7", "8"))
  }
)
