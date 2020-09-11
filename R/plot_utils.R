#' @export
tooltipAes <- function(..., plotFunc){
  colname <- getOption("xiff.column")
  colname <- rlang::sym(colname)
  mapping <- ggplot2::aes(..., dummy = !!colname)
  isPointGeom <- identical(plotFunc, ggplot2::geom_point) || identical(plotFunc, ggplot2::geom_jitter)

  if (!isPointGeom){
    mapping$dummy <- NULL # use dummy only for geom_point
  }

  mapping
}

#' @export
getPheatmap <- function(..., silent = TRUE){
  old_dev <- grDevices::dev.cur()
  x <- pheatmap::pheatmap(..., silent = silent)
  if (old_dev > 1) grDevices::dev.set(old_dev)
  x
}
