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
  oldDev <- grDevices::dev.cur()
  x <- pheatmap::pheatmap(..., silent = silent)
  if (oldDev > 1) grDevices::dev.set(oldDev)
  x
}

#' @export
guessAxisScale <- function(values){
  myScale <- "identity"
  rng <- range(values, na.rm = TRUE)
  if (min(rng) > 0) {
    if ((rng[[2]] / rng[[1]]) >= 1000) {
      myScale <- "log10"
    }
  }

  myScale
}
