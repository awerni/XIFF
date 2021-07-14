machineLearningApplyModelTabUI_main <- function(id){
  ns <- NS(id)
  
  div(
    fluidRow(
      h4("Provide an RDS file obtained from the machine learning tab"),
      mlApplyModelUI_header(ns("tab"))
    ),
    fluidRow(
      fileInput(
        inputId = ns("upload"),
        label = "Choose file",
        multiple = FALSE,
        accept = ".rds"
      )
    ),
    mlApplyModelUI_main(ns("tab"))
  )
}

machineLearningApplyModelTabUI_sidebar <- function(id){
  ns <- NS(id)
  mlApplyModelUI_sidebar(ns("tab"))
}

machineLearningApplyModelTab <- function(input, output, session, classSelection, classLabel, CelllineAnnotationFocus){
  ns <- session$ns
  
  Model <- reactive({
    uploadInfo <- input$upload
    if (is.null(uploadInfo)) return()
    
    model <- readRDS(input$upload$datapath)
    validateXiffMachineLearningResult(model)
    model
    
  })
  
  callModule(
    module = mlApplyModel,
    id = "tab",
    Model = Model,
    classSelection = classSelection,
    classLabel = classLabel,
    CelllineAnnotationFocus = CelllineAnnotationFocus
  )
}
