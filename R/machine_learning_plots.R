# Machine learning ------------------------------------------------------------

#' Generate Machine Learning Performance Plot
#' 
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_dfr map
#' @importFrom ggplot2 ggplot xlab ylab geom_boxplot
#' 
#' @examples 
#' \dontrun{
#' # TODO: create models here
#' generatePerformancePlot(fit$trainingOutput)
#' generatePerformancePlot(fitNN$trainingOutput)
#' 
#' }
#' 
generatePerformancePlot <- function(x){
  metrics <- x$pred %>%
    split(
      x = .,
      f = .$Resample
    ) %>%
    purrr::map(~ caret::confusionMatrix(
      data = .x$pred,
      reference = .x$obs,
      mode = "sens_spec",
      positive = "class1"
    )) %>%
    purrr::map_dfr(~ .x$byClass) %>%
    select(Sensitivity, Specificity, `Pos Pred Value`, `Neg Pred Value`) %>%
    pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
    bind_rows(tibble(metric = "Accuracy", value = x$resample$Accuracy)) %>%
    filter(is.finite(value)) %>% # remove NaNs
    mutate(metric = factor(
      x = metric,
      levels = c("Neg Pred Value", "Pos Pred Value", "Specificity", "Sensitivity", "Accuracy"))
    )
  
  ggplot(
    data = metrics,
    mapping = aes(
      x = value,
      y = metric
    )
  ) +
    geom_boxplot() +
    xlab("Value") +
    ylab("Metric") +
    ggtitle("\nModel performance") +
    theme(
      text = element_text(size = 16),
      plot.title = element_text(hjust = 0.5),
      axis.title.y = element_blank()
    ) +
    coord_cartesian(xlim = c(0, 1))
}

#' Create Machine Learning Variable Importance Plot. 
#' 
#' @export
#' @examples 
#' 
#' \dontrun{
#' # TODO: create models here
#' generateVarImpPlot(fit)
#' generateVarImpPlot(fitNN)
#' 
#' }
#' 
generateVarImpPlot <- function(x){
  
  if(!inherits(x, "MLXIFF")) {
    stop("Error in XIFF::generateVarImpPlot - x must be MLXIFF object!")
  }
  
  importanceData <- x$df[, c("ensg", "importance")]
  importanceName <- attr(x$df, "importanceName")
  
  importanceData <- importanceData %>% arrange(importance) %>%
    mutate(ensg = factor(ensg, ordered = TRUE, levels = importanceData$ensg))
  
  ggplot(
    data = importanceData,
    mapping = aes(
      x = importance,
      y = ensg
    )
  ) +
    geom_point() +
    xlab(importanceName) +
    ggtitle("\nVariable importance") +
    theme(
      text = element_text(size = 16),
      plot.title = element_text(hjust = 0.5),
      axis.title.y = element_blank()
    ) +
    coord_cartesian(xlim = c(
      floor(min(importanceData$importance, na.rm = TRUE)),
      ceiling(max(importanceData$importance, na.rm = TRUE))
    ))
  
}



#' Generate Error Plot for XIFF Random Forrest Machine Learning Model
#'
#' @param x Xiff Machine Learning MOdel
#' @param cl list with class labels
#'
#' @return ggplot2 object containing error plot
#' @export
#'
generateErrorPlot <- function(x, cl = list(class1_name = "sensitive", class2_name = "resistant")){
  
  m <- x$finalModel
  
  dict <- c(
    class1 = cl$class1_name,
    class2 = cl$class2_name,
    OOB = "OOB"
  )
  
  data <- m$err.rate
  if (is.null(data)) return()
  
  df <- data %>%
    as_tibble() %>%
    rowid_to_column("nTree") %>%
    pivot_longer(cols = -nTree, names_to = "type") %>%
    arrange(type) %>%
    mutate(
      type = unname(dict)[match(x = type, table = names(dict))],
      type = factor(type, levels = unname(dict))
    )
  
  ggplot(
    data = df,
    mapping = aes(
      x = nTree,
      y = value,
      color = type
    )
  ) +
    geom_line() +
    scale_color_manual(values = plotColors[c(1,2,7)]) +
    xlab("Trees") +
    ylab("Error") +
    ggtitle("\nError rates") +
    theme(
      text = element_text(size = 16),
      legend.position = "right",
      plot.title = element_text(hjust = 0.5)
    )
}

#' Generate Apply Performance Plot
#'
#' @param df MLModelTestsResult object or 
#' the result of \code{generateTestPerformanceData}
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' 
#' confMatrix <- as.table(matrix(
#'   c(19L, 9L, 2L, 66L),
#'   nrow = 2,
#'   dimnames = list(
#'     Prediction = c("positive", "negative"),
#'     Reference =  c("positive", "negative")
#'   )
#' ))
#' df <- generateTestPerformanceData(confMatrix)
#' generateTestPerformancePlot(df)
#' 
generateTestPerformancePlot <- function(df) {
  UseMethod("generateTestPerformancePlot")
}

#' @export
#' @exportS3Method 
generateTestPerformancePlot.MLModelTestsResult <- function(df) {
  df2 <- generateTestPerformanceData(df$res$table)
  generateTestPerformancePlot(df2)
}

#' @export
#' @exportS3Method 
generateTestPerformancePlot.data.frame <- function(df){
  ggplot(
    data = df,
    mapping = aes(x = metric, y = value, fill = metric)
  ) + 
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    geom_bar(stat = "identity") +
    geom_text(mapping = aes(y = labelPos, label = label), nudge_y = -0.03) +
    coord_cartesian(ylim = c(0, max(1, df$labelPos))) +
    xlab("Metric") + 
    ylab("Value") + 
    ggtitle("\nPerformance") + 
    theme(
      text = element_text(size = 16),
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    )
}

#' Generate Visualization of the confusion matrix.
#'
#' @param df MLModelTestsResult or data.frame with columns predicted and reference columns
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' 
#' lvls <- factor(
#'   c("resistant\n(negative)", "sensitive\n(positive)"),
#'   ordered = TRUE
#' )
#' 
#' tbl <- tibble(predicted = lvls[c(1,1,1,2,2)], reference = lvls[c(2,1,1,1,2)])
#' 
#' generateTablePlot(tbl)
#' 
generateTablePlot <- function(df) {
  UseMethod("generateTablePlot")
}

#' @export
#' @exportS3Method 
generateTablePlot.MLModelTestsResult <- function(df) {
  generateTablePlot(df$tablePlotData)
}

#' @export
#' @exportS3Method 
generateTablePlot.data.frame <- function(df){
  ggplot(
    data = df,
    mapping = aes(y = predicted, fill = predicted)
  ) + 
    geom_bar() +
    facet_wrap(~ reference) + 
    xlab("Count") + 
    ylab("Predicted (model output)") + 
    ggtitle("\nReference (current class assignment)") + 
    theme(
      text = element_text(size = 16),
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    ) + 
    scale_fill_manual(values = plotColors[c(2,1)]) # red for positive, blue for negative
}
