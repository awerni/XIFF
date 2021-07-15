#' @import shiny
#' @import dplyr
#' @import ggplot2
.onLoad <- function(libname, pkgname){
  options(xiff.column = "celllinename")
  options(xiff.schema = "cellline")
  options(xiff.label = "cell line")
  options(xiff.name = "celllines")
  options(xiff.tooltipCallbackFun = tooltipCallbackFun)
  options(xiff.boruta.threads = 2)

  addResourcePath(
    prefix = "xiff",
    directoryPath = system.file("www", package = "XIFF")
  )

  t <- theme_get()
  t$text$family <- "Helvetica"
  theme_set(t)
}
