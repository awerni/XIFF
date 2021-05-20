#' @export
isDbOnline <- function(timeout = 5){
  info <- getDBConnectionData()
  RPostgres::dbCanConnect(
    drv = RPostgres::Postgres(),
    user = info$user,
    password = info$password,
    dbname = info$dbname,
    host = info$host,
    port = info$port,
    connect_timeout = timeout
  )
}

getDBConnectionData <- function(){
  myDB <- getOption("dbname")
  if (is.null(myDB)) myDB <- db
  DBhost <- getOption("dbhost")
  DBport <- getOption("dbport")

  user <- getOption("dbuser")
  password <- getOption("dbpass")

  compare <- function(x, y) (x == y | y == "*")

  if (user == Sys.getenv("USER")) {
    if (password == "" | is.na(password)) {
      pgpass <- strsplit(scan("~/.pgpass", what = "", quiet = TRUE), ":")
      n <- sapply(pgpass, function(x) {
        compare(DBhost, x[[1]]) & compare(myDB, x[[3]]) & compare(user, x[[4]])
      })
      password <- pgpass[[which(n)[1]]][[5]]
    }
  }

  list(
    user = user,
    password = password,
    dbname = myDB,
    host = DBhost,
    port = DBport
  )
}

getPostgresqlConnection <- function() {
  info <- getDBConnectionData()
  drv <- RPostgres::Postgres()
  con <- try(DBI::dbConnect(
    drv = drv,
    user = info$user,
    password = info$password,
    dbname = info$dbname,
    host = info$host,
    port = info$port
  ))

  return(con)
}

#' Get data from Postgres
#' 
#' Function that queries the DB
#' 
#' @param sql character string, the SQL query
#' @return data.frame
#' @export
getPostgresql <- function(sql) {
  con <- getPostgresqlConnection()
  if (class(con) == "try-error") stop("no connection to database")

  rs <- try(RPostgres::dbSendQuery(con, sql))
  if (class(rs) == "try-error") {
    RPostgres::dbDisconnect(con)
    stop("can not exectute sql command")
  }

  data <- try(RPostgres::dbFetch(rs, n = -1))

  if (class(rs) == "PqResult") RPostgres::dbClearResult(rs)
  RPostgres::dbDisconnect(con)
  if (class(data) == "try-error") stop("can not retrieve data")

  return(data)
}

#' @export
getSQL_filter <- function(filter_col, filter_options) {
  #if (typeof(filter_options) == "character") {
  #sql <- paste0(filter_col, " IN ('", paste(filter_options, collapse = "','"), "')")
  #sql <- paste0(filter_col, " = ANY('{", paste(filter_options, collapse = ","), "}'::text[])")
  #} else {
  #sql <- paste0(filter_col, " IN (", paste(filter_options, collapse = ","), ")")
  #}
  #return(sql)
  
  if (length(filter_options) > 0){
    paste0(filter_col, " IN ('", paste(filter_options, collapse = "','"), "')")
  }
}

#' @export
prepareConditionSql <- function(...){
  dots <- list(...)
  
  items <- napply(
    X = dots,
    FUN = function(x, name){
      if (is.character(x) && length(x) > 0){
        if (length(x) > 1){
          getSQL_filter(name, x)
        } else {
          paste0(name, " = '", x, "'")
        }
      }
    }
  )
  
  items <- dropNulls(items)
  if (length(items) > 0){
    paste(items, collapse = " AND ")
  }
}
