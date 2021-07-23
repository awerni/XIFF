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
  
  if(!"Roboto" %in% names(pdfFonts())) {
    # solve the warning regarding missing font in pdf device
    # it's a hack, but other solution didn't work e.g. registering
    # the fonts using afm files (Type1Font - function). They make the
    # font visible for cairo_pdf device but not the pdf device
    z <- pdfFonts()[["Helvetica"]]
    z$family <- "Roboto"
    pdfFonts(Roboto = z)
    postscriptFonts(Roboto = z)
  }
  
  t <- theme_get()
  t$text$family <- "Roboto"
  theme_set(t)
}
