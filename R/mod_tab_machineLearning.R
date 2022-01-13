#' machineLearningTabUI
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
      title = "test model",
      value = "test",
      machineLearningTestModelTabUI_main(ns("test"))
    ),
    tabPanel(
      title = "apply model",
      value = "apply",
      machineLearningApplyModelTabUI_main(ns("apply"))
    )
  )
}

#' @rdname machineLearningTab
#' @export
appUI_main_machine_learning <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Machine Learning",
    machineLearningTabUI(ns("ml"))
  )
}

#' @rdname machineLearningTab
#' @export
machineLearningTabUI <- function(id) {
  ns <- NS(id)
  
  tabsetPanel(
    id = ns("tabset"),
    columnTabPanel(
      title = "create model",
      value = "create",
      machineLearningCreateModelTabUI_sidebar(ns("create")),
      machineLearningCreateModelTabUI_main(ns("create"))
    ),
    columnTabPanel(
      title = "test model",
      value = "test",
      machineLearningTestModelTabUI_sidebar(ns("test")),
      machineLearningTestModelTabUI_main(ns("test"))
    ),
    columnTabPanel(
      title = "apply model",
      value = "apply",
      machineLearningApplyModelTabUI_sidebar(ns("apply")),
      machineLearningApplyModelTabUI_main(ns("apply"))
    )
  )
}

#' @rdname machineLearningTab
#' @export
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
      condition = makeCondition("test"),
      machineLearningTestModelTabUI_sidebar(ns("test"))
    ),
    conditionalPanel(
      condition = makeCondition("apply"),
      machineLearningApplyModelTabUI_sidebar(ns("apply"))
    )
  )
}

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
    module = machineLearningTestModelTab,
    id = "test",
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
