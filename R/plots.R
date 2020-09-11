#' @export
plotColors <- c("#d73027", "#4575b4", "#fc8d59", "#91bfdb", "#A8840D", "#24BF43", "#000000")

#' @export
generateDataCoveragePlot <- function(data, col, sampleClasses, classLabel) {
  if (is.null(data) | is.null(col) | is.null(sampleClasses)) return(NULL)
  colname <- getOption("xiff.column")

  df <- stackClasses(sampleClasses, classLabel) %>%
    dplyr::left_join(data, by = colname) %>%
    dplyr::select(class, !!colname, x = !!col) %>%
    dplyr::group_by(class) %>%
    dplyr::summarize(
      missing = sum(is.na(x)),
      available = dplyr::n() - missing
    ) %>%
    tidyr::pivot_longer(missing:available, names_to = "type", values_to = "n") %>%
    dplyr::mutate(type = factor(type, levels = c("missing", "available")))

  ggplot2::ggplot(
    data = df,
    mapping = ggplot2::aes(
      x = class,
      y = n,
      fill = type
    )
  ) +
    ggplot2::geom_bar(
      position = "stack",
      stat = "identity"
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        missing = "gray80",
        available = "#91bfdb")
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 16),
      legend.position = "right",
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::ggtitle(paste("\n", "Data coverage")) +
    ggplot2::xlab("") +
    ggplot2::ylab(paste0("Number of ", getOption("xiff.label"), "s"))
}

generateDimRedPlot <- function(data, progressText, show.labels = TRUE, colorCol, fontSize = 10, p = TRUE) {
  ret <- list(status = "ok")

  if (p) {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = progressText, value = 0.5)
  }

  colname <- getOption("xiff.column")
  colname <- rlang::sym(colname)
  nItems <- data[[getOption('xiff.name')]]

  if (progressText == "plot PCA") {
    mapping <- ggplot2::aes(
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
    mapping <- ggplot2::aes(
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

    mapping <- ggplot2::aes(
      x = X1,
      y = X2,
      color = class,
      label = plotlabel,
      dummy = !!colname
    )

    xlabel <- "umap-1"
    ylabel <- "umap-2"
    title <- paste0("UMAP plot\n", data$title)
  }

  pl <- ggplot2::ggplot(
    data = data$data,
    mapping = mapping
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::xlab(xlabel) +
    ggplot2::ylab(ylabel) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      text = ggplot2::element_text(size = fontSize),
      legend.position = "bottom"
    ) +
    ggplot2::labs(color = colorCol)

  if (length(unique(data$data$class)) <= 7) {
    pl <- pl + ggplot2::scale_color_manual(values = plotColors)
  } else {
    #pl <- pl + viridis::scale_color_viridis(discrete = TRUE, option = "plasma")
  }

  if (show.labels) {
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
    inner_join(annotationFocus, by = colname) %>%
    mutate(prop1 = .data[[prop1]], prop2 = .data[[prop2]]) %>%
    mutate(prop1 = ifelse(is.na(prop1) & !is.numeric(prop1), "NA", as.character(prop1)),
           prop2 = ifelse(is.na(prop2) & !is.numeric(prop2), "NA", as.character(prop2))) %>%
    mutate(prop1 = forcats::fct_lump(prop1, n = n_classes, other_level = "other"),
           prop2 = forcats::fct_lump(prop2, n = n_classes, other_level = "other"))

  n_labels <- length(levels(data$prop2))
  n_char <- max(nchar(levels(data$prop2)))
  n_rows <- ceiling(n_labels/floor(100/n_char))

  if (plot_type == "bar") {
    if (n_classes < 16) {
      data <- data %>% mutate(prop1 =  str_wrap(prop1, 30))
    }

    if (usePercent && (prop1 %in% names(annotation))){
      x <- getPropertyFractions(data, annotation, annotationFocus, prop1, prop2)
      mapping <- ggplot2::aes(
        x = prop1,
        y = percent,
        fill = prop2
      )

      generateClassSelectionBarPlot(x, mapping, paste("% of respective", prop1), n_rows, "identity")
    } else {
      mapping <- ggplot2::aes(
        x = prop1,
        fill = prop2
      )

      generateClassSelectionBarPlot(data, mapping, paste0("# ", getOption("xiff.label"), "s"), n_rows)
    }
  } else if (plot_type == "pie") {
    data_sum <- if (prop1 != prop2) {
      data %>% dplyr::mutate(prop = paste(prop1, prop2, sep = "-"))
    } else {
      data %>% dplyr::rename(prop = prop1)
    }

    data_sum <- data_sum %>%
      dplyr::group_by(class, prop) %>%
      dplyr::summarise(n = n()) %>%
      dplyr::mutate(percent = n/sum(n))

    ggplot2::ggplot(
      data = data_sum,
      mapping = ggplot2::aes(
        x = 1,
        y = percent,
        fill = prop
      )
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::facet_grid(facets = . ~ class) +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::xlab("") +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        text = ggplot2::element_text(size = 20),
        legend.position = "bottom"
      )
  }
}

generateClassSelectionBarPlot <- function(data, mapping, ylabel, n_rows, stat = "count"){
  ggplot2::ggplot(
    data = data,
    mapping = mapping
  ) +
    ggplot2::geom_bar(stat = stat) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 20),
      legend.position = "bottom"
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab(ylabel) +
    ggplot2::labs(fill = prop2) +
    ggplot2::coord_flip() +
    ggplot2::guides(fill = ggplot2::guide_legend(
      nrow = n_rows,
      byrow = FALSE
    )) +
    ggplot2::facet_wrap(~class, scales = "free_x")
}

#' @export
generateScoreBarPlot <- function(data, score_desc) {
  if (nrow(data) == 0) return()

  g <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = x_score,
      fill = x_score
    )
  ) +
    ggplot2::geom_bar() +
    ggplot2::theme(
      legend.position = "none",
      text = ggplot2::element_text(size = 15)
    ) +
    ggplot2::xlab(score_desc)

  if (length(unique(data$x_score)) > 10) {
    g <- g + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  }

  nItems <- length(unique(data$x_score))


  if (nItems > 100) {
    g + ggplot2::theme(axis.text.x = ggplot2::element_blank())
  } else if (nItems > 10) {
    g + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  } else {
    g
  }
}

#' @export
generateScoreWaterfallPlot <- function(data, score_desc, y_scale = "norm") {
  if (nrow(data) == 0) return()
  colname <- getOption("xiff.column")
  colname <- rlang::sym(colname)

  g <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = !!colname,
      y = x_score,
      fill = tumortype
    )
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      width = 1
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      text = ggplot2::element_text(size = 15)
    ) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::xlab(getOption("xiff.label")) +
    ggplot2::ylab(score_desc)

  if (y_scale == "log10") {
    g <- g + ggplot2::scale_y_continuous(trans = scales::log10_trans())
  }

  nCL <- nrow(data)
  if (nCL > 100) {
    g + ggplot2::theme(axis.text.x = ggplot2::element_blank())
  } else {
    g + ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = -90,
        hjust = 0,
        size = dplyr::if_else(nCL <= 50, 15, 10)
      )
    )
  }
}
