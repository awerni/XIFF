#' @export
#' @rdname programmerImportInputMode
programmerImportInputModeUI <- function(id){
  ns <- NS(id)
  
  restoreSelectionInputModeUI(
    id = ns("display"),
    fluidRow_12(
      textInput(
        inputId = ns("hash"),
        label = "Dataset ID"
      )
    )
  )
}


#' Programmer Import Input Module
#'
#' @param id module id
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#' @param Annotation annotation data.frame
#'
#' @export
#' @rdname programmerImportInputMode
#'
programmerImportInputMode <- function(input, output, session, Annotation){
  colname <- getOption("xiff.column")
  
  StashedData <- reactive({
    hash <- gsub(input$hash, pattern = "[[:space:]]", replacement = "")
    validate(need(
      is.character(hash) && nchar(hash) == 6, "please provide 6-character ID"
    ))
    
    df <- getStashedData(hash)
    validate(need(is.data.frame(df), "no data found for the provided ID"))
    validate(need(
      colname %in% names(df), paste(colname, "column not found in the data")
    ))
        
    if (!"tumortype" %in% names(df)){
      df <- df %>% addTumortypes(Annotation())
    }
    
    df
  })
  
  Items <- callModule(
    module = restoreSelectionInputMode,
    id = "display",
    classStack = StashedData
  )
  
  Items
}
