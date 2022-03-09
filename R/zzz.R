#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @importFrom grDevices pdfFonts postscriptFonts
.onLoad <- function(libname, pkgname){
  
  options(xiff.column = getOption("xiff.column", default = "celllinename"))
  options(xiff.schema = getOption("xiff.schema", default = "cellline"))
  options(xiff.label = getOption("xiff.label", default = "cell line"))
  options(xiff.name = getOption("xiff.name", default = "celllines"))

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

  if (!isFontInstalled("Roboto")){
    systemfonts::register_font(
      name = "Roboto",
      plain = list(getRobotoDir("regular"), 0),
      bold = list(getRobotoDir("bold"), 0),
      italic = list(getRobotoDir("italic"), 0),
      bolditalic = list(getRobotoDir("bolditalic"), 0)
    )
  }
  
  if(!"Roboto" %in% names(grDevices::pdfFonts())) {
    # solve the warning regarding the missing font in pdf device.
    # It's a hack, but other solution didn't work. E.g. registering
    # the fonts using afm files (Type1Font - function). They make the
    # font usable for cairo_pdf device but not for the pdf device.
    z <- pdfFonts()[["Helvetica"]]
    z$family <- "Roboto"
    grDevices::pdfFonts(Roboto = z)
    grDevices::postscriptFonts(Roboto = z)
  }
  
  t <- theme_get()
  t$text$family <- "Roboto"
  theme_set(t)
}
