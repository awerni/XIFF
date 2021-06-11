#' @export
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

#' @export
programmerImportInputMode <- function(input, output, session, Annotation){
  colname <- getOption("xiff.column")
  
  StashedData <- reactive({
    hash <- input$hash
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
