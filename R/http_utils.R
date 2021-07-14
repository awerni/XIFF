#' @export
withHash <- function(filePath, wwwPath){
  hash <- tools::md5sum(filePath)
  paste(wwwPath, hash, sep = "?")
}

#' @export
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
      href = "http://fonts.googleapis.com/css?family=Roboto"
    ),
    tags$script(src = withHash(
      filePath = system.file("www/common.js", package = "XIFF"),
      wwwPath = "xiff/common.js"
    ))
  )
}
