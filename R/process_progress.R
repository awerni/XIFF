#' R6 ProcessProgress bar
#' 
#' @details Progress bar for shiny
#' 
#' @importFrom FutureManager fmIsInterrupted fmUpdateProgress fmError
#' @export
ProcessProgress <- R6::R6Class(
  classname = "ProcessProgress",
  public = list(
    initialize = function(label, p = FALSE){
      private$label <- label
      private$type <- if (is.logical(p)){
        if (p){
          "console"
        } else {
          "none"
        }
      } else if (is(p, "ShinySession") || is(p, "session_proxy")){
        private$shinyProgress <- Progress$new(session = p)
        "shiny"
      } else {
        private$fmTask <- p
        "fm"
      }

      xfun::exit_call(private$onExit, n = 3)
      invisible(self)
    },

    update = function(value, msg){
      if (private$type == "none") return()
      switch(
        EXPR = private$type,
        console = private$updateConsole(msg, value),
        shiny = private$updateShiny(msg, value),
        fm = private$updateFm(msg, value)
      )
    },

    error = function(msg){
      args <- if (private$type == "fm"){
        list(fmError(msg))
      } else if (private$type == "shiny"){
        list(msg)
      } else {
        stop(msg)
      }

      private$exitParentFunction(args)
    }
  ),

  private = list(
    type = NULL,
    label = NULL,
    fmTask = NULL,
    shinyProgress = NULL,

    exitParentFunction = function(args = list(), n = -2){
      do.call(
        what = return, # return() from the parent function
        args = args,
        envir = sys.frame(n)
      )
    },

    updateConsole = function(msg, value){
      cat(paste0(private$label, " [", value * 100, "%] ", msg, "\n"))
    },

    updateShiny = function(msg, value){
      private$shinyProgress$set(
        value = value,
        message = private$label,
        detail = msg
      )
    },

    updateFm = function(msg, value){
      task <- private$fmTask

      if (fmIsInterrupted(task)) private$exitParentFunction(n = -3)
      fmUpdateProgress(
        task = task,
        progress = value,
        msg = msg
      )
    },

    onExit = function(){
      if (private$type == "shiny"){
        private$shinyProgress$close()
      }
    }
  )
)


#' Utility function for checking the result of a function
#'
#' @param x result to be validated, if string, then the function will throw an
#' error.
#'
#' @return nothing, called for side effects
#' @export
#'
validateFunctionResult <- function(x){
  if (is.character(x)) validate(need(FALSE, x))
}
