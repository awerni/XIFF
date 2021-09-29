#' @export
plotColors <- c("#d73027", "#4575b4", "#fc8d59", "#91bfdb", "#A8840D", "#24BF43", "#000000")

#' @export
commonPlotTheme <- function(legend.position = "right", textSize = 16){
  theme(
    text = element_text(
      size = textSize,
      family = "Roboto"
    ),
    legend.position = legend.position,
    plot.title = element_text(hjust = 0.5)
  )
}

#' @export
generatePlotByType <- function(data, plotType, dataCol, title = NULL, ca = NULL, 
                               rocPlotFun = generateROCPlot, 
                               diffPlotFun = generateDiffPlot, ...) {
  
  if(is.null(ca)) ca <- getClassAssigmentAttribute(data)
  
  if(plotType == "all") {
      types <- c("point", "roc", "violin", "box", "coverage")
      res <-
        lapply(types, function(t)
          generatePlotByType(
            data = data,
            ca = ca,
            plotType = t,
            dataCol = dataCol,
            rocPlotFun = rocPlotFun,
            diffPlotFun = diffPlotFun,
            title = title,
            ...
          ))
     return(res) 
  }
  
  switch(
    EXPR = plotType,
    roc = rocPlotFun(
      data = data, 
      ca = ca, 
      dataCol = dataCol,
      title = title
    ),
    point = diffPlotFun(
      data = data, 
      ca = ca, 
      dataCol = dataCol,
      plotFunc = geom_jitter,
      title = title, 
      width = 0.25, 
      height = 0, 
      mapping = aes(colour = class),
      ...
    ),
    violin = diffPlotFun(
      data = data, 
      ca = ca, 
      dataCol = dataCol,
      plotFunc = geom_violin,
      title = title,
      ...
    ),
    box = diffPlotFun(
      data = data, 
      ca = ca, 
      dataCol = dataCol,
      plotFunc = geom_boxplot,
      title = title,
      ...
    ),
    coverage = generateDataCoveragePlot(
      data = data, 
      col = dataCol, 
      ca = ca
    )
  )
}

#' @export
generateROCPlot <- function(data, ca, dataCol, title = "ROC plot") {
  if (is.null(data) || is.null(ca) || is.null(dataCol)) return()

  colname <- getOption("xiff.column")
  assignment <- stackClasses(ca)
  data <- data %>% inner_join(assignment, by = colname)

  ordering <- 0:1
  names(ordering) <- names(sort(sapply(split(data[[dataCol]], data[["class"]]), mean, na.rm = TRUE)))
  data[["d"]] <- ordering[as.character(data[["class"]])]

  dataCol <- rlang::sym(dataCol)
  p <- ggplot(data, aes(d = d, m = !!dataCol)) +
    plotROC::geom_roc() +
    commonPlotTheme() +
    ggtitle(title) +
    xlab("False positive rate") +
    ylab("True positive rate")
  # scale_fill_viridis(discrete = TRUE, option = "plasma")

  auc <- plotROC::calc_auc(p)$AUC
  p + annotate("text", x = .75, y = .25, label = paste("AUC =", signif(auc, 3)))
}

#' @export
generateDiffPlot <- function(data, ca, dataCol, plotFunc, title = NULL,
                             xlabel = "", ylabel = "", trans = "identity", ...) {
  if (is.null(data) || is.null(dataCol) || is.null(ca)) return()

  colname <- getOption("xiff.column")
  coldata <- getAssignmentDf(ca) %>%
    inner_join(data, by = colname)

  dataCol <- rlang::sym(dataCol)
  mapping <- tooltipAes(class, !!dataCol, fill = class, plotFunc = plotFunc)

  breaks <- if (trans == "identity"){
    scales::breaks_pretty(n = 5)
  } else {
    waiver()
  }

  ggplot(coldata, mapping) +
    plotFunc(...) +
    scale_y_continuous(
      trans = trans,
      breaks = breaks
    ) +
    commonPlotTheme("none") + 
    scale_fill_manual(values = plotColors) +
    scale_color_manual(values = plotColors) +
    # + scale_fill_viridis(discrete = TRUE, option = "plasma") +
    ggtitle(title) +
    xlab(xlabel) +
    ylab(ylabel)
}

#' @export
generateWaterfallPlot <-
  function(data,
           dataCol,
           xlabel = getOption("xiff.label"),
           ylabel = "score",
           trans = "identity",
           limits = NULL,
           fill = "tumortype") {
    
  if (is.null(data)) return()
    
  colname <- getOption("xiff.column")
  colname <- rlang::sym(colname)

  dataCol <- rlang::sym(dataCol)
  fill <- rlang::sym(fill)
  p <- ggplot(data, aes(x = !!colname, y = !!dataCol, fill = !!fill)) +
    geom_bar(stat = "identity", width = 1) +
    theme_bw() +
    commonPlotTheme("bottom") + 
    theme(
      axis.text.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    scale_y_continuous(trans = trans) +
    coord_cartesian(ylim = limits) +
    xlab(xlabel) +
    ylab(ylabel)

  nItems <- nrow(data)
  if (nItems > 100){
    p
  } else {
    p + theme(
      axis.text.x = element_text(
      angle = -90,
      hjust = 0,
      size = if_else(nItems <= 50, 15, 10)
    ))
  }
}

#' @export
generateDataCoveragePlot <- function(data, col, ca) {
  colname <- getOption("xiff.column")

  df <- getAssignmentDf(ca) %>%
    left_join(data, by = colname) %>%
    select(class, !!colname, x = !!col) %>%
    group_by(class) %>%
    summarize(
      missing = sum(is.na(x)),
      available = dplyr::n() - missing
    ) %>%
    tidyr::pivot_longer(missing:available, names_to = "type", values_to = "n") %>%
    mutate(type = factor(type, levels = c("missing", "available")))

  ggplot(
    data = df,
    mapping = aes(
      x = class,
      y = n,
      fill = type
    )
  ) +
    geom_bar(
      position = "stack",
      stat = "identity"
    ) +
    scale_fill_manual(
      values = c(
        missing = "gray80",
        available = "#91bfdb")
    ) +
    commonPlotTheme("right") +
    ggtitle(paste("\n", "Data coverage")) +
    xlab("") +
    ylab(paste0("Number of ", getOption("xiff.label"), "s"))
}

#' @export
generateDimRedPlot <- function(data, progressText, colorCol, showLabels = TRUE, fontSize = 10, p = FALSE) {
  ret <- list(status = "ok")

  progress <- ProcessProgress$new("DimRed plot", p)
  progress$update(0.5, progressText)

  colname <- getOption("xiff.column")
  colname <- rlang::sym(colname)
  nItems <- data[[getOption('xiff.name')]]

  if (progressText == "plot PCA") {
    mapping <- aes(
      x = PC1,
      y = PC2,
      color = class,
      label = plotlabel,
      dummy = !!colname
    )

    pV <- data$percentVar
    xlabel <- paste0("PC1: ", pV[1],"% variance")
    ylabel <- paste0("PC2: ", pV[2],"% variance")
    title <- glue::glue("PCA plot\n#{getOption('xiff.label')}s={nItems}")

  } else if (progressText == "plot t-SNE") {
    mapping <- aes(
      x = X1,
      y = X2,
      color = class,
      label = plotlabel,
      dummy = !!colname
    )

    xlabel <- "t-SNE-1"
    ylabel <- "t-SNE-2"
    title <- paste0("T-SNE plot\n", data$title)

  } else if (progressText == "plot umap") {

    mapping <- aes(
      x = X1,
      y = X2,
      color = class,
      label = plotlabel,
      dummy = !!colname
    )

    xlabel <- "umap-1"
    ylabel <- "umap-2"
    title <- paste0("UMAP plot\n", data$title)
  } else if(progressText == "plot PHATE") {
    
    mapping <- aes(
      x = X1,
      y = X2,
      color = class,
      label = plotlabel,
      dummy = !!colname
    )
    
    xlabel <- "PHATE-1"
    ylabel <- "PHATE-2"
    title <- "PHATE plot"
  }

  df <- data$data %>% mutate(class = factor(class))

  pl <- ggplot(
    data = df,
    mapping = mapping
  ) +
    geom_point(size = 3) +
    xlab(xlabel) +
    ylab(ylabel) +
    ggtitle(title) +
    commonPlotTheme("bottom", fontSize) +
    labs(color = colorCol)

  if (length(unique(data$data$class)) <= 7) {
    pl <- pl + scale_color_manual(values = plotColors)
  } else {
    #pl <- pl + viridis::scale_color_viridis(discrete = TRUE, option = "plasma")
  }

  if (showLabels) {
    if (nItems > 400) {
      ret$status <- "Can not display labels for more than 400 samples"
    } else {
      pl <- pl + ggrepel::geom_text_repel(
        size = round(4/15 * fontSize),
        show.legend = FALSE
      )
    }
  }

  ret$pl <- pl
  return(ret)
}

#' @export
generateClassSelectionPlot <- function(sampleClasses, classLabel, prop1, prop2, n_classes,
                                       plot_type = "bar", usePercent = FALSE, annotation, annotationFocus) {
  colname <- getOption("xiff.column")

  data <- stackClasses(sampleClasses, classLabel, return_factor = TRUE) %>%
    inner_join(annotationFocus, by = colname)

  isProp2None = prop2 == "none"

  data <- data %>%
    mutate(
      prop1 = .data[[prop1]],
      prop1 = ifelse(is.na(prop1) & !is.numeric(prop1), "NA", as.character(prop1)),
      prop1 = forcats::fct_lump(prop1, n = n_classes, other_level = "other")
    )

  if (isProp2None){
    data <- data %>% mutate(prop2 = "none")
    n_rows <- 0
    colorProp <- "prop1"

  } else {
    data <- data %>%
      mutate(
        prop2 = .data[[prop2]],
        prop2 = ifelse(is.na(prop2) & !is.numeric(prop2), "NA", as.character(prop2)),
        prop2 = forcats::fct_lump(prop2, n = n_classes, other_level = "other")
      )

    n_labels <- length(levels(data$prop2))
    n_char <- max(nchar(levels(data$prop2)))
    n_rows <- ceiling(n_labels/floor(100/n_char))
    colorProp <- "prop2"
  }

  colorProp <- rlang::sym(colorProp)

  if (plot_type == "bar") {
    if (n_classes < 16) {
      data <- data %>% mutate(prop1 = stringr::str_wrap(prop1, 30))
    }

    if (usePercent && (prop1 %in% names(annotation))){
      x <- getPropertyFractions(data, annotation, annotationFocus, prop1, prop2)
      mapping <- aes(
        x = prop1,
        y = percent,
        fill = !!colorProp
      )

      generateClassSelectionBarPlot(x, mapping, paste("% of respective", prop1), n_rows, prop2, "identity")
    } else {
      mapping <- aes(
        x = prop1,
        fill = !!colorProp
      )

      generateClassSelectionBarPlot(data, mapping, paste0("# ", getOption("xiff.label"), "s"), n_rows, prop2)
    }
  } else if (plot_type == "pie") {
    data_sum <- if (prop1 != prop2 && !isProp2None) {
      data %>% mutate(prop = paste(prop1, prop2, sep = "-"))
    } else {
      data %>% rename(prop = prop1)
    }

    data_sum <- data_sum %>%
      group_by(class, prop) %>%
      summarise(n = dplyr::n()) %>%
      mutate(percent = n/sum(n))

    ggplot(
      data = data_sum,
      mapping = aes(
        x = 1,
        y = percent,
        fill = prop
      )
    ) +
      geom_bar(stat = "identity") +
      facet_grid(facets = . ~ class) +
      coord_polar(theta = "y") +
      xlab("") +
      ylab("") +
      labs(fill = "") +
      commonPlotTheme("bottom", 20)
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
  }
}

generateClassSelectionBarPlot <- function(data, mapping, ylabel, n_rows, prop2, stat = "count"){
  isProp2None <- prop2 == "none"

  if (isProp2None) {
    legendPosition <- "none"
    colorProp <- "prop1"
  } else {
    legendPosition <- "bottom"
    colorProp <- "prop2"
  }

  p <- ggplot(
    data = data,
    mapping = mapping
  ) +
    geom_bar(stat = stat) +
    commonPlotTheme(legendPosition, 20) +
    xlab("") +
    ylab(ylabel) +
    coord_flip() +
    facet_wrap(~class, scales = "free_x")

  if (isProp2None){
    p
  } else {
    p +
      labs(fill = prop2) +
      guides(fill = guide_legend(
        nrow = n_rows,
        byrow = FALSE
      ))
  }
}

#' @export
generateScoreBarPlot <- function(data, score_desc) {
  if (nrow(data) == 0) return()

  g <- ggplot(
    data = data,
    mapping = aes(
      x = x_score,
      fill = x_score
    )
  ) +
    geom_bar() +
    commonPlotTheme("none") +
    xlab(score_desc)
  
  nItems <- length(unique(data$x_score))

  if (nItems > 100) {
    g + theme(axis.text.x = element_blank())
  } else if (nItems > 10) {
    g + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  } else {
    g
  }
}

#' @export
generateScoreWaterfallPlot <- function(data, score_desc, y_scale = "identity") {
  if (nrow(data) == 0) return()

  generateWaterfallPlot(
    data = data,
    dataCol = "x_score",
    ylabel = score_desc,
    trans = y_scale,
    fill = "tumortype"
  )
}

#' @export
print.customPlotPrint <- function(x){
  grid::grid.draw(x)
}
