.onLoad <- function(libname, pkgname){
  options(xiff.column = "celllinename")
  options(xiff.label = "cell line")
  options(xiff.name = "celllines")
  options(xiff.tooltipCallbackFun = tooltipCallbackFun)

  shiny::addResourcePath(
    prefix = "xiff",
    directoryPath = system.file("www", package = "XIFF")
  )
}
