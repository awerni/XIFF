test_that(
  desc = "Common rownames",
  code = {
    mat1 <- matrix(
      data = c(
        3, 3, 3, 
        5, 5, 5,
        2, 2, 2,
        1, 1, 1,
        4, 4, 4
      ),
      nrow = 5,
      ncol = 3,
      byrow = TRUE
    )
    rownames(mat1) <- c("c", "e", "b", "a", "d")
    
    mat2 <- matrix(
      data = c(
        4, 4, 4, 4, 4,
        2, 2, 2, 2, 2,
        1, 1, 1, 1, 1,
        5, 5, 5, 5, 5
      ),
      nrow = 4,
      ncol = 5,
      byrow = TRUE
    )
    rownames(mat2) <- c("d", "b", "a", "e")
    
    res <- ensureCommonRownames(mat1, mat2)
    expect_is(res, "list")
    expect_length(res, 2)
    
    m1 <- res[[1]]
    m2 <- res[[2]]
    
    expect_equal(nrow(m1), 4)
    expect_equal(nrow(m2), 4)
    expect_equal(ncol(m1), 3)
    expect_equal(ncol(m2), 5)
    
    r1 <- rownames(m1)
    r2 <- rownames(m2)
    expect_true(identical(r1, r2))
    expect_equal(r1, c("e", "b", "a", "d"))
    expect_equal(m1[, 1], c(e = 5, b = 2, a = 1, d = 4))
    expect_equal(m1[, 2], c(e = 5, b = 2, a = 1, d = 4))
    
    res2 <- ensureCommonRownames(mat1, mat2, outNames = c("x1", "x2"))
    expect_is(res2, "list")
    expect_named(res2, c("x1", "x2"))
    expect_identical(res2[["x1"]], m1)
    expect_identical(res2[["x2"]], m2)
    
    res3 <- ensureCommonRownames(mat1, mat2, outNames = c("x1", "x2"), sortRownames = TRUE)
    expect_equal(rownames(res3[["x1"]]), c("a", "b", "d", "e"))
    expect_equal(res3[["x1"]][,1], c(a = 1, b = 2, d = 4, e = 5))
    expect_equal(res3[["x2"]][,1], c(a = 1, b = 2, d = 4, e = 5))
  }
)
