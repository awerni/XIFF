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
    ggplot2::ylab(paste("Number of", getOption("xiff.label")))
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
    title <- glue::glue("PCA plot\n#{getOption('xiff.label')}={nItems}")

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
