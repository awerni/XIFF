#' Create href path css and js files
#'
#' @param filePath path to css or js file
#' @param wwwPath href path
#'
#' @return wwwPath with md5 attached.
#' @export
#'
#' @examples
#' 
#' withHash(
#'   filePath = system.file("www/common.css", package = "XIFF"),
#'   wwwPath = "xiff/common.css"
#' )
#' 
withHash <- function(filePath, wwwPath){
  hash <- tools::md5sum(filePath)
  paste(wwwPath, hash, sep = "?")
}


#' Add XIFF css and js to shiny app.
#'
#' @export
#'
useXIFF <- function(){
  list(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = withHash(
        filePath = system.file("www/common.css", package = "XIFF"),
        wwwPath = "xiff/common.css"
      )
    ),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "https://fonts.googleapis.com/css?family=Roboto"
    ),
    tags$script(src = withHash(
      filePath = system.file("www/common.js", package = "XIFF"),
      wwwPath = "xiff/common.js"
    ))
  )
}
