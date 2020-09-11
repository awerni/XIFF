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
