# Machine learning ------------------------------------------------------------
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_dfr map
#' @importFrom ggplot2 ggplot xlab ylab geom_boxplot
#' 
#' @examples 
#' \dontrun{
#' # TODO: create models here
#' # check buildMachineLearning docs for fit and fitNN creation.
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

#

.varImpPlot <- function(df, xl = "Mean decrease Gini") {
  ggplot(
    data = df,
    mapping = aes(
      x = x,
      y = ensg
    )
  ) +
    geom_point() +
    xlab(xl) +
    ggtitle("\nVariable importance") +
    theme(
      text = element_text(size = 16),
      plot.title = element_text(hjust = 0.5),
      axis.title.y = element_blank()
    ) +
    coord_cartesian(xlim = c(
      floor(min(df$x, na.rm = TRUE)),
      ceiling(max(df$x, na.rm = TRUE))
    ))
}



#' @export
#' @examples 
#' 
#' \dontrun{
#' # TODO: create models here
#' # check buildMachineLearning docs for fit and fitNN creation.
#' generateVarImpPlot(fit$trainingOutput)
#' generateVarImpPlot(fitNN$trainingOutput)
#' 
#' }
#' 
generateVarImpPlot <- function(x){
  .generateVarImpPlot(x$finalModel)
}

.generateVarImpPlot <- function(x) {
  UseMethod(".generateVarImpPlot")
}

.generateVarImpPlot.default <- function(x) {
  return(NULL)
}

.generateVarImpPlot.randomForest <- function(x) {
  data <- x$importance
  if (is.null(data)) return()
  
  df <- data %>%
    as_tibble(rownames = "ensg") %>%
    rename(x = MeanDecreaseGini) %>%
    arrange(x) %>%
    mutate(ensg = factor(ensg, levels = unique(ensg)))
  
  .varImpPlot(df)
}

.generateVarImpPlot.nn <- function(x) {
  
  df <- XIFF::modelInfoNeuralNetwork()$varImp(x) %>% arrange(importance)
  df <- rownames_to_column(df, var = "ensg")
  colnames(df)[2] <- "x"
  
  .varImpPlot(df, "Importance")
  
}

#' @export
#' 
#' @importFrom tibble rowid_to_column
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

#' @export
generateApplyPerformancePlot <- function(df){
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

#' @export
generateTablePlot <- function(df){
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