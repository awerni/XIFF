#' Title
#'
#' @param id 
#'
#' @rdname machineLearningTab
#' @export
#'
machineLearningTabUI_main <- function(id){
  ns <- NS(id)
  
  tabsetPanel(
    id = ns("tabset"),
    tabPanel(
      title = "create model",
      value = "create",
      machineLearningCreateModelTabUI_main(ns("create"))
    ),
    tabPanel(
      title = "validate model",
      value = "validate",
      machineLearningValidateModelTabUI_main(ns("validate"))
    ),
    tabPanel(
      title = "apply model",
      value = "apply",
      machineLearningApplyModelTabUI_main(ns("apply"))
    )
  )
}

#' Title
#'
#' @param id 
#'
#' @rdname machineLearningTab
#' @export
#'
machineLearningTabUI_sidebar <- function(id){
  ns <- NS(id)
  
  makeCondition <- function(x){
    tabInput <- paste0("input['", ns("tabset"), "']")
    paste0(tabInput, " && ", tabInput, " == '", x, "'")
  }
  
  div(
    conditionalPanel(
      condition = makeCondition("create"),
      machineLearningCreateModelTabUI_sidebar(ns("create"))
    ),
    conditionalPanel(
      condition = makeCondition("validate"),
      machineLearningValidateModelTabUI_sidebar(ns("validate"))
    ),
    conditionalPanel(
      condition = makeCondition("apply"),
      machineLearningApplyModelTabUI_sidebar(ns("apply"))
    )
  )
}

#' Title
#'
#' @param input 
#' @param output 
#' @param session 
#' @param fm 
#' @param classSelection 
#' @param classLabel 
#' @param gsea_data_hallmark 
#' @param gene_anno 
#' @param AnnotationFocus 
#' @param Species 
#'
#' @rdname machineLearningTab
#' @export
#'
machineLearningTab <- function(input, output, session, fm, classSelection, classLabel, 
                               gsea_data_hallmark, gene_anno, AnnotationFocus, Species){
  Results <- callModule(
    module = machineLearningCreateModelTab,
    id = "create",
    fm = fm, 
    classSelection = classSelection, 
    classLabel = classLabel, 
    gsea_data_hallmark = gsea_data_hallmark, 
    gene_anno = gene_anno, 
    AnnotationFocus = AnnotationFocus,
    Species = Species
  )
  
  callModule(
    module = machineLearningValidateModelTab,
    id = "validate",
    classLabel = classLabel,
    Results = Results,
    AnnotationFocus = AnnotationFocus
  )
  
  callModule(
    module = machineLearningApplyModelTab,
    id = "apply",
    classSelection = classSelection,
    classLabel = classLabel,
    AnnotationFocus = AnnotationFocus
  )
}
