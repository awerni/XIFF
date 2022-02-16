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

# GCloud auth -----------------------------------------------------------------
#' Run GCloud command
#'
#' Runs gcloud command with additional args using system2()
#'
#' @param args character vector of the command arguments, see system2 docs for
#' details
#'
#' @export
#' @return the command output
#'
runGCloudCommand <- function(args){
  res <- tryCatch(
    expr = suppressWarnings(system2(
      command = "gcloud",
      args = args,
      stdout = TRUE,
      stderr = TRUE
    )),
    error = function(e){
      stop(
        "Error occured when trying to run gcloud. ",
        "Did you install and configure gcloud CLI?"
      )
    }
  )

  statusCode <- attr(res, "status")
  if (!is.null(statusCode)){
    msg <- paste(res, collapse = "\n")
    stop("Error occured when running gcloud:\n", msg)
  }

  res
}

#' Get current GCloud user
#'
#' Returns a currently logged-in GCloud user
#'
#' @return the current user email
#' @export
#'
getCurrentGCloudUser <- function(){
  testUserName <- getOption("xiff.testUserName", default = NULL)

  if (is.null(testUserName)){
    runGCloudCommand(c("config", "list", "account", "--format", "'value(core.account)'"))
  } else {
    warning("Using test user name! Please set xiff.testUserName option to NULL to avoid this")
    testUserName
  }
}

#' Create GCloud access token
#'
#' Retrieves a GCloud access token for the currently logged-in user
#'
#' @return list with fields: user, token, timestamp
#' @export
#'
createGCloudAccessToken <- function(){
  timestamp <- Sys.time()
  currentUser <- getCurrentGCloudUser()
  token <- runGCloudCommand(c("auth", "print-access-token"))

  list(
    user = currentUser,
    token = token,
    timestamp = timestamp
  )
}

#' Get GCloud access token
#'
#' Returns an active GCloud access token. If the token does not exist or is
#' outdated (was created more than 59min ago), a new one is created and stored
#' in xiff.gToken option.
#'
#' @return list with fields: user, token, timestamp
#' @export
#'
getGCloudAccessToken <- function(){
  token <- getOption("xiff.gToken")
  currentTime <- Sys.time()

  shouldRefresh <- FALSE

  if (is.null(token)){
    shouldRefresh <- TRUE
  } else {
    timestampDiff <- as.numeric(difftime(
      time1 = currentTime,
      time2 = token$timestamp,
      units = "m"
    ))

    if (timestampDiff > 59){
      shouldRefresh <- TRUE
    }
  }

  if (shouldRefresh){
    token <- createGCloudAccessToken()
    options("xiff.gToken" = token)
  }

  token
}

# DB connection ---------------------------------------------------------------
getDBConnectionData <- function(){
  myDB <- getOption("dbname")
  if (is.null(myDB)) myDB <- db
  DBhost <- getOption("dbhost")
  DBport <- getOption("dbport")

  useGCloud <- getOption("useGCloudAuth", default = FALSE)

  if (useGCloud){
    token <- getGCloudAccessToken()
    user <- token$user
    password <- token$token
  } else {
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
  x <- runDbQuery(sql)
  data <- try(RPostgres::dbFetch(x$rs, n = -1))

  if (class(x$rs) == "PqResult") RPostgres::dbClearResult(x$rs)
  RPostgres::dbDisconnect(x$con)
  if (class(data) == "try-error") stop("can not retrieve data")

  return(data)
}

#' Run set query in Postgres
#'
#' Function that queries the DB
#'
#' @param sql character string, the SQL query
#' @return TRUE if success
#' @export
setPostgresql <- function(sql){
  x <- runDbQuery(sql)
  if (class(x$rs) == "PqResult") RPostgres::dbClearResult(x$rs)
  RPostgres::dbDisconnect(x$con)
  TRUE
}

runDbQuery <- function(sql){
  con <- getPostgresqlConnection()
  if (class(con) == "try-error") stop("no connection to database")

  rs <- try(RPostgres::dbSendQuery(con, sql))
  if (class(rs) == "try-error") {
    RPostgres::dbDisconnect(con)
    stop("can not exectute sql command")
  }

  list(rs = rs, con = con)
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
      } else if(length(x) > 0) {
        if(is.list(x)) {
          msg <- glue::glue("`{name}` argument is a list, but it should be",
          " a character vector. Probably you've used `[\"{name}\"]`",
          " instead of `[[\"{name}\"]]`")
          stop(msg)
        } else {
          stop(glue::glue(
            "`{name}` is `{class(x)}`, but character vector is required."
          ))
        }
      }
    }
  )

  items <- dropNulls(items)
  if (length(items) > 0){
    paste(items, collapse = " AND ")
  }
}

#' Stash data and use them in apps
#'
#' For expert usage only. You have to have writing permissions to the DB.
#' Returned hash may be used in the Restore selection input mode.
#'
#' @param df data.frame data to store
#' @return character string, the dataset hash
#' @export
#' @examples
#' \dontrun{
#' setDbOptions(getSettings())
#' options(dbuser = Sys.getenv("USER"))
#' options(dbpass = NA)
#' df <- data.frame(celllinename = c("a", "b", "c"), property = c(1, 2, 3))
#' hash <- stashData(df)
#' df2 <- getStashedData(hash) # you can reach the data anywhere the DB is available
#' }
stashData <- function(df){
  stopifnot(is.data.frame(df))

  myHash <- substr(digest::digest(Sys.time()), 1, 6)
  payload <- jsonlite::toJSON(df)

  sql <- paste0(
    "INSERT INTO datastack (datastackid, playload, created) VALUES ('",
    myHash, "','", payload, "', now());"
  )

  invisible(setPostgresql(sql))
  myHash
}

#' @export
getStashedData <- function(hash){
  sql <- paste0("SELECT playload FROM datastack WHERE datastackid = '", hash, "'")
  res <- getPostgresql(sql)

  if (nrow(res) > 0){
    jsonlite::fromJSON(res$playload)
  }
}


#' Get gene symbol
#'
#' @param ensgs gene ensg
#' @param species single character string determining the species
#'
#' @return vector of gene symbols. If there's no ensg in the database the NA is
#' returned for that gene.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' getGeneSymbol(c("ENSG00000133703", "xx", "ENSG00000268173", "ENSG00000133703"))
#'
#' }
#'
getGeneSymbol <- function(ensgs, species = "human"){
  sql <- paste0(
    "SELECT ensg, coalesce(symbol, ensg) as symbol FROM gene ",
    "WHERE ", prepareConditionSql(ensg = ensgs, species = species)
  )
  res <- getPostgresql(sql)

  res <- (res %>% tibble::column_to_rownames("ensg"))[ensgs,,drop = FALSE]
  res$symbol
}

