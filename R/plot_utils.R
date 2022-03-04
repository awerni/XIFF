#' Add toltip aes
#'
#' @param ... 
#' @param plotFunc 
#'
#' @return ggplot2 aes
#' @export
#' 
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


#' Wrapper for pheatmap function
#'
#' @param mat numeric matrix of the values to be plotted.
#' @param ... other arguments passed to pheatmap
#' @param silent do not draw the plot (useful when using the gtable output)
#' @param show_rownames boolean specifying if column names are be shown.
#' @param show_colnames boolean specifying if column names are be shown.
#'
#' @details restores default graphical device after calling pheatmap function
#' @export
#'
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


#' Determine appropriate scale type for given set of values
#'
#' @param values vector with values
#'
#' @return preferred scale to be used for given values
#' @export
#'
#' @examples
#' 
#' x <- data.frame(x = 1:3, v = c(1,2,30))
#' y <- data.frame(x = 1:4, v = c(1,2,30,200))
#' 
#' 
#' guessAxisScale()
#' guessAxisScale()
#' 
#' library(ggplot2)
#' 
#' #identity
#' ggplot(x) + geom_bar(aes(x = x, y = v), stat = "identity") + 
#'  scale_y_continuous(trans = guessAxisScale(x$v))
#'  
#' # log10
#' ggplot(y) + geom_bar(aes(x = x, y = v), stat = "identity") + 
#'  scale_y_continuous(trans = guessAxisScale(y$v))
#' 
#' 
guessAxisScale <- function(values){
  myScale <- "identity"
  rng <- range(values, na.rm = TRUE)
  if (min(rng) > 0) {
    if ((rng[[2]] / rng[[1]]) >= 1000) {
      
      myScale <- "log10"
      
    } else {
      
      rang <- range(log10(values))
      rang[rang < 0] <- 0
      log10range <- diff(rang)
      
      if(log10range > 2.01) {
        myScale <- "log10"
      }
      
    }
  }

  myScale
}


#' XIFF Color Scale
#'
#' @param n number of colors to be returned
#'
#' @export
#'
#' @examples
#' 
#' cols <- ggColorHue(6)
#' pie(rep(1,6), col = cols)
#' pie(rep(1,200), col = ggColorHue(200), lty = 0, labels = "")
#' 
#' 
ggColorHue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

isFontInstalled <- function(name){
  fonts <- systemfonts::system_fonts()
  name %in% fonts$family
}
