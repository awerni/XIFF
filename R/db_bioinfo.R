
#' Get GSEA Collection
#'
#' @param species name of the species
#'
#' @return named vector with GSEA Collections
#' @export
#'
#' @examples
#' 
#' if(require("CLIFF")) {
#'   CLIFF::setDbOptions()
#'   getGSEACollection()
#' }
#' 
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

# GSEA ------------------------------------------------------------------------
#' Get gene set data.
#'
#' @param species selected species. E.g. 'human'.
#' @param collection_name name of the gene set collection.
#' @param gene_set name of single gene set to be returned. If specified then the
#'        function returns character vector containing that gene set.
#'
#' @return 
#' 
#' List of gene set. If \code{gene_set} is used, then it returns just a 
#' character vector with that gene set.
#' 
#' @export
#' 
getGSEAdata <- function(species, collection_name = "mSigDB", gene_set = NULL) {
  
  if (collection_name == "mSigDB") { 
    collection_name <- NULL
  }
  conditions <- prepareConditionSql(
    species = species,
    collection_name = collection_name,
    gene_set = gene_set
  )
  
  sql <- paste0(
    "SELECT gene_set, array_to_string(ensg_array, ',') AS ensg FROM public.msigdb ",
    "WHERE ", conditions
  )
  
  msig <- getPostgresql(sql)
  res <- stringr::str_split(msig$ensg, ",")
  
  if(!is.null(gene_set)) {
    res[[1]] # if gene_set is specified, then returns gene set directly
  } else {
    names(res) <- msig$gene_set
    res # returns list
  }
  
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

#' Get Gene Set Information
#'
#' @param geneset gene set name
#'
#' @return vector with genes ensg
#' @export
#' 
#' @importFrom glue glue_sql
#' @importFrom DBI ANSI
#' @rdname getGeneSet
#' @examples
#' 
#' setDbOptions()
#' meta <- getMetadataGeneSetTable()
#' meta
#' 
#' head(getGeneSet(meta$genesetname[1]))
#' 
getMetadataGeneSetTable <- function() {
  getPostgresql("
    SELECT genesetname, species FROM public.geneset
  ")
}

#' @rdname getGeneSet
#' @export
getAvailableGeneSets <- function(){
  sql <- "SELECT DISTINCT genesetname FROM public.geneassignment"
  getPostgresql(sql)[["genesetname"]]
}

#' @rdname getGeneSet
#' @export
getGeneSet <- function(geneset) {
  sql <- glue::glue_sql(
    "SELECT ensg FROM public.geneassignment WHERE genesetname = {geneset}",
    .con = DBI::ANSI()
  )
  
  getPostgresql(sql)[["ensg"]]
}

#' @rdname getGeneSet
#' @export
getGeneSetsForGene <- function(ensg){
  sql <- glue::glue_sql(
    "SELECT genesetname FROM public.geneassignment WHERE ensg = {ensg}",
    .con = DBI::ANSI()
  )
  
  getPostgresql(sql)[["genesetname"]]
}
