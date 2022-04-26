renameRecommendColumns <- function(res, fac_levels) {
  colnames(res) <- gsub(colnames(res), pattern = "\\.x$", replacement = paste0(".", fac_levels[1]))
  colnames(res) <- gsub(colnames(res), pattern = "\\.y$", replacement = paste0(".", fac_levels[2]))
  res
}

#' Double Class Test
#'
#' @param data data.frame containing \code{byVar}, \code{x}, \code{y} columns.
#' @param byVar a column name used to split the data for tests.
#' @param x column name which contains two levels used to split \code{y} column.
#' @param y column name from \code{data}, which contains continuous variable.
#' @param test function used to perform the tests. It needs to take two matrices. 
#' @param ... other params passed to test function.
#'
#' @return data.frame with statistical tests applied across all the \code{byVar} levels.
#' @export
#'
#' @examples
#' 
#' set.seed(123)
#' dt <- data.frame(
#'   by = c(rep("A", 15), rep("B",10)),
#'   x  = c(
#'      rep("a",7), rep("b",8), # A
#'      rep("a",6), rep("b",4)  # B
#'    ),
#'   y  = c(
#'      rnorm(7), rnorm(8, 2),  # A
#'      rnorm(6), rnorm(4, 4)  # B
#'    )
#' )
#' 
#' res <- doubleClassTestByVariable(dt, byVar = "by", x = "x", y = "y")
#' res <- res %>% arrange(by)
#' 
#' atest <- t.test(y~x, data = dt %>% filter(by == "A"), var.equal = TRUE)
#' btest <- t.test(y~x, data = dt %>% filter(by == "B"), var.equal = TRUE)
#' all.equal(res$pvalue, c(atest$p.value, btest$p.value))
#' 
doubleClassTestByVariable <- function(data, byVar, x, y, test = matrixTests::row_t_equalvar, ...) {
  
  data[[byVar]] <- as.factor(data[[byVar]])
  tbl <- tibble(byVar = levels(data[[byVar]]))
  colnames(tbl) <- byVar
  
  data[[byVar]] <- as.integer(data[[byVar]])
  data[[x]] <- as.factor(data[[x]])
  fac_levels <- levels(data[[x]])
  data[[x]] <- as.integer(data[[x]])
  
  data <- data[order(data[[byVar]], data[[x]]), ]
  
  data[[".idx"]] <- sequence(tabulate((data[[byVar]] - 1L) * 2 + data[[x]]))
  
  data1 <- data[data[[x]] == 1,]
  data2 <- data[data[[x]] == 2,]
  
  n <- max(data[[byVar]])
  m <- max(data[[".idx"]])
  
  to_matrix <- function(dx) {
    mat <- matrix(NA_real_, nrow = n, ncol = m)
    mat[cbind(dx[[byVar]], dx[[".idx"]])] <- dx[[y]]
    mat
  }
  
  m1 <- to_matrix(data1)
  m2 <- to_matrix(data2)
  
  result <- dplyr::bind_cols(tbl, test(m1, m2, ...)) %>%
    arrange(pvalue) %>%
    renameRecommendColumns(fac_levels) %>% 
    mutate(adj.pvalue = p.adjust(pvalue, method = "fdr")) %>%
    relocate(adj.pvalue, .after = pvalue)
}

#' Continuous Test
#'
#' @param data data.frame containing \code{byVar}, \code{x}, \code{y} columns.
#' @param byVar a column name used to split the data for tests.
#' @param x column name from \code{data}, which contains continuous variable.
#' @param y column name from \code{data}, which contains continuous variable.
#' @param test function used to perform the tests. It needs to take two matrices. 
#' @param ... other params passed to test function.
#'
#' @return data.frame with statistical tests applied across all the \code{byVar} levels.
#' @export
#'
#' @examples
#' 
#' set.seed(123)
#' 
#' # prepare some data for A and B groups
#' dt <- data.frame(
#'   by = c(rep("A", 15), rep("B",10))
#' )
#' 
#' adt <- MASS::mvrnorm(15, c(0,0), cbind(c(1,0.8), c(0.8,1)))
#' bdt <- MASS::mvrnorm(10, c(0,0), cbind(c(1,0.1), c(0.1,1)))
#' 
#' mat <- rbind(adt, bdt)
#' colnames(mat) <- c("x", "y")
#' dt <- cbind(dt, mat)
#' 
#' res <- continuousTestByVariable(dt, by = "by", x = "x", y = "y")
#' res <- res %>% arrange(by)
#' 
#' atest <- cor.test(adt[,1], adt[,2])
#' btest <- cor.test(bdt[,1], bdt[,2])
#' all.equal(res$pvalue, c(atest$p.value, btest$p.value))
#' 
continuousTestByVariable <- function(data, byVar, x, y, test = matrixTests::row_cor_pearson, ...) {
  
  data[[byVar]] <- as.factor(data[[byVar]])
  tbl <- tibble(byVar = levels(data[[byVar]]))
  colnames(tbl) <- byVar
  
  data[[byVar]] <- as.integer(data[[byVar]])
  
  data <- data[order(data[[byVar]]), ]
  
  data[[".idx"]] <- sequence(tabulate(data[[byVar]]))
  
  n <- max(data[[byVar]])
  m <- max(data[[".idx"]])
  
  to_matrix <- function(dx, col) {
    mat <- matrix(NA_real_, nrow = n, ncol = m)
    mat[cbind(dx[[byVar]], dx[[".idx"]])] <- dx[[col]]
    mat
  }
  
  m1 <- to_matrix(data, x)
  m2 <- to_matrix(data, y)
  
  dplyr::bind_cols(tbl, test(m1, m2, ...)) %>%
    arrange(pvalue) %>% 
    mutate(adj.pvalue = p.adjust(pvalue, method = "fdr")) %>%
    relocate(adj.pvalue, .after = pvalue)
}
