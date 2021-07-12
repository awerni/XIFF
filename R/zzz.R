#' @import shiny
#' @import dplyr
#' @import ggplot2
.onLoad <- function(libname, pkgname){
  options(xiff.column = "celllinename")
  options(xiff.label = "cell line")
  options(xiff.name = "celllines")
  options(xiff.tooltipCallbackFun = tooltipCallbackFun)
  options(xiff.boruta.threads = 2)

  addResourcePath(
    prefix = "xiff",
    directoryPath = system.file("www", package = "XIFF")
  )

  fontDir <- system.file("fonts", package = "XIFF")
  getRobotoDir <- function(type){
    glue::glue("{fontDir}/roboto_{type}.ttf")
  }
  
  systemfonts::register_font(
    name = "Roboto", 
    plain = list(getRobotoDir("regular"), 0), 
    bold = list(getRobotoDir("bold"), 0), 
    italic = list(getRobotoDir("italic"), 0),
    bolditalic = list(getRobotoDir("bolditalic"), 0)
  )
  
  t <- theme_get()
  t$text$family <- "Roboto"
  theme_set(t)
}
