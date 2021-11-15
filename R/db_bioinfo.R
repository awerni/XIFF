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


#' Get Antibody Information
#'
#' @param antibody single character string for which to return the information
#' or \code{NA} (default), to return data for all antibodies.
#' @param checkData if true check for data avability in 
#' \code{processedproteinexpression} table and adds \code{has_data} column
#' with logical indicator
#'
#' @return data.frame with antibody, validation_status, vendor, catalog_number
#' columns and optionally if checkData is TRUE, logical column has_data which 
#' determines if specific antibody has data available in table
#' \code{processedproteinexpression}; 
#' if parameter \code{antibody} is a single value the result will also contain
#' column \code{genes} 
#' 
#' @export
#'
#' @importFrom dplyr mutate
#' 
getAntibodyInformation <- function(antibody = NA, checkData = FALSE) {
  if (is.na(antibody)) {
    sql <- "SELECT antibody, validation_status, vendor, catalog_number FROM antibody"
    result <- getPostgresql(sql) %>% mutate_at(vars(validation_status, vendor), forcats::as_factor)
  } else {
    sql <- paste0("SELECT validation_status, vendor, catalog_number FROM antibody WHERE antibody = '", antibody, "'")
    ret <- getPostgresql(sql)
    sql <- paste0("SELECT coalesce(symbol, g2a.ensg) AS gene FROM gene2antibody g2a JOIN gene g ON g.ensg = g2a.ensg WHERE antibody = '", antibody, "'")
    result <- ret %>% mutate(genes = getPostgresql(sql)$gene %>% paste(collapse = ", "))
  }
  
  if(checkData) {
    
    schema <- getOption("xiff.schema")
    sql <- glue::glue(
      "select 
         distinct antibody 
       from 
         {schema}.processedproteinexpression"
    )
    
    antibodiesWithData <- getPostgresql(sql) %>% pull
    result <- result %>% mutate(has_data = antibody %in% antibodiesWithData)
  }
  
  result
}
