#' @export
withHash <- function(filePath, wwwPath){
  hash <- tools::md5sum(filePath)
  paste(wwwPath, hash, sep = "?")
}
