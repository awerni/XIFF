#' @export
getGSEACollection <- function(species = "human") {
  sql <- paste0("SELECT distinct collection, collection_name ",
                "FROM msigdb WHERE species = '", species, "'")
  res <- getPostgresql(sql) %>%
    mutate(collection = paste(collection, collection_name)) %>%
    bind_rows(data.frame(collection = "full Molecular Signature DB",
                         collection_name = "mSigDB", stringsAsFactors = FALSE)) %>%
    mutate(sortcol = ifelse(collection == "h hallmark", "aaaaa", collection)) %>%
    arrange(sortcol) %>%
    select(-sortcol)

  ret <- res$collection_name
  names(ret) <- res$collection
  ret
}
