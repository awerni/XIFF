machineLearningValidateModelTabUI_main <- function(id){
  ns <- NS(id)
  
  div(
    fluidRow(mlApplyModelUI_header(ns("tab"))),
    mlApplyModelUI_main(ns("tab"))
  )
}

machineLearningValidateModelTabUI_sidebar <- function(id){
  ns <- NS(id)
  mlApplyModelUI_sidebar(ns("tab"))
}

machineLearningValidateModelTab <- function(input, output, session, classLabel, Results, AnnotationFocus){
  classSelectionValidation <- reactiveValues(class1 = NULL, class2 = NULL)
  Model <- reactive({
    fmValidate(Results())
    res <- Results()[["value"]]
    
    validationSet <- res$validationSet
    validate(need(validationSet, "validation set not provided"))
    validateXiffMachineLearningResult(res)

    classSelectionValidation$class1 <- validationSet$class1
    classSelectionValidation$class2 <- validationSet$class2
    
    newClassLabel(res) <- reactiveValuesToList(classLabel)
    
    res
  })
  
  callModule(
    module = mlApplyModel,
    id = "tab",
    Model = Model,
    classSelection = classSelectionValidation,
    classLabel = classLabel,
    AnnotationFocus = AnnotationFocus
  )
}
