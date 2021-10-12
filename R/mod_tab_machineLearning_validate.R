machineLearningTestModelTabUI_main <- function(id){
  ns <- NS(id)
  
  div(
    fluidRow(mlApplyModelUI_header(ns("tab"))),
    mlApplyModelUI_main(ns("tab"))
  )
}

machineLearningTestModelTabUI_sidebar <- function(id){
  ns <- NS(id)
  mlApplyModelUI_sidebar(ns("tab"))
}

machineLearningTestModelTab <- function(input, output, session, classLabel, Results, AnnotationFocus){
  classSelectionTest <- reactiveValues(class1 = NULL, class2 = NULL)
  Model <- reactive({
    FutureManager::fmValidate(Results())
    res <- Results()[["value"]]
    
    testSet <- mlModelSet2ClassSelectionList(res, res$testSet)
    validate(need(testSet, "test set not provided"))
    validateXiffMachineLearningResult(res)

    classSelectionTest$class1 <- testSet$class1
    classSelectionTest$class2 <- testSet$class2
    
    newClassLabel(res) <- reactiveValuesToList(classLabel)
    
    res
  })
  
  callModule(
    module = mlApplyModel,
    id = "tab",
    Model = Model,
    classSelection = classSelectionTest,
    classLabel = classLabel,
    AnnotationFocus = AnnotationFocus
  )
}
