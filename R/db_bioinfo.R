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

#' @export
getAntibodyInformation <- function(antibody = NA) {
  if (is.na(antibody)) {
    sql <- "SELECT antibody, validation_status, vendor, catalog_number FROM antibody"
    getPostgresql(sql) %>% mutate_at(vars(validation_status, vendor), forcats::as_factor)
  } else {
    sql <- paste0("SELECT validation_status, vendor, catalog_number FROM antibody WHERE antibody = '", antibody, "'")
    ret <- getPostgresql(sql)
    sql <- paste0("SELECT coalesce(symbol, g2a.ensg) AS gene FROM gene2antibody g2a JOIN gene g ON g.ensg = g2a.ensg WHERE antibody = '", antibody, "'")
    ret <- ret %>% mutate(genes = getPostgresql(sql)$gene %>% paste(collapse = ", "))
  }
}
