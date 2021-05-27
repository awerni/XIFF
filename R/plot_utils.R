#' @export
tooltipAes <- function(..., plotFunc){
  colname <- getOption("xiff.column")
  colname <- rlang::sym(colname)
  mapping <- aes(..., dummy = !!colname)
  isPointGeom <- identical(plotFunc, geom_point) || identical(plotFunc, geom_jitter)

  if (!isPointGeom){
    mapping$dummy <- NULL # use dummy only for geom_point
  }

  mapping
}

#' @export
getPheatmap <- function(mat, ..., silent = TRUE,
                        show_rownames = nrow(mat) < 100,
                        show_colnames = ncol(mat) < 100){
  oldDev <- grDevices::dev.cur()
  x <- pheatmap::pheatmap(
    mat = mat,
    ..., 
    show_rownames = show_rownames,
    show_colnames = show_colnames,
    silent = silent
  )
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
