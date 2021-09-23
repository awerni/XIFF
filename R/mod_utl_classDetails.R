classDetailsUI_main <- function(id, label, defaultName){
  ns <- NS(id)
  styleDetails <- "padding:4px; margin:3px; font-size:90%"
  
  div(
    textInput(
      inputId = ns("name"), 
      label = paste(label, "name:"), 
      value = defaultName
    ),
    shinyjs::hidden(
      span("Classes must have different names.",
         " class1/class2 will be used.",
         class = "help-block red-help-block", id = ns("sameNamesInfo"))
    ),
    p(),
    actionButton(
      inputId = ns("add"), 
      label = "Add", 
      style = styleDetails
    ),
    actionButton(
      inputId = ns("remove"), 
      label = "Remove", 
      style = styleDetails
    ),
    actionButton(
      inputId = ns("intersect"), 
      label = "Intersect", 
      style = styleDetails
    ),
    actionButton(
      inputId = ns("clear"), 
      label = "Clear", 
      style = styleDetails
    ),
    textOutput(ns("n"))
  )
}

classDetailsUI_show <- function(id){
  ns <- NS(id)
  
  div(
    textOutput(ns("title")),
    verbatimTextOutput(ns("list"))
  )
}

classDetails <- function(input, output, session, index, classLabel, classSelection, Selected, onChange){
  colname <- getOption("xiff.column")
  selectionId <- paste0("class", index)
  labelId <- paste0(selectionId, "_name")
  anotherSelectionId <- paste0("class", index %% 2 + 1) # 1 -> 2 or 2 -> 1
  anotherLabelId <- paste0(anotherSelectionId, "_name")
  
  Selection <- reactive({
    classSelection[[selectionId]]
  })
  
  output$n <- renderText({
    get_label(length(Selection()))
  })
  
  output$list <- renderText({
    paste(Selection(), collapse = "\n")
  })
  
  output$title <- renderText({
    classLabel[[labelId]]
  })
  
  observeEvent(
    eventExpr = input$name, 
    handlerExpr = {
      
      if(input$name == classLabel[[anotherLabelId]]) {
        shinyjs::show("sameNamesInfo")
      } 
      classLabel[[labelId]] <- input$name
    }
  )
  
  observeEvent(
    eventExpr = input$add, 
    handlerExpr = {
      s <- Selected()
      old <- Selection()
      cl <- sort(unique(union(s[[colname]], old)))
      
      classSelection[[selectionId]] <- onChange(
        Selected = s, 
        old = old,
        new = cl,
        checkOverlap = TRUE,
        anotherSelection = classSelection[[anotherSelectionId]]
      )
    }
  )
  
  observeEvent(
    eventExpr = input$remove, 
    handlerExpr = {
      s <- Selected()
      old <- Selection()
      cl <- sort(unique(setdiff(old, s[[colname]])))
      
      classSelection[[selectionId]] <- onChange(
        Selected = s, 
        old = old,
        new = cl
      )
    }
  )
  
  observeEvent(
    eventExpr = input$intersect, 
    handlerExpr = {
      s <- Selected()
      old <- Selection()
      cl <- sort(unique(intersect(old, s[[colname]])))
      
      classSelection[[selectionId]] <- onChange(
        Selected = s, 
        old = old,
        new = cl
      )
    }
  )
  
  observeEvent(
    eventExpr = input$clear, 
    handlerExpr = {
      old <- Selection()
      classSelection[[selectionId]] <- onChange(
        Selected = Selected(), 
        old = old,
        new = NULL,
        reset = FALSE
      )
    }
  )
}

# Main class which handles all the tasks related to classDetails
# it contains two classDetails modules.
#' @export
classDetailsWrapperUI_main <- function(id,
                                       defaults = c("sensitive", "resistant")) {
  ns <- NS(id)
  tagList(
    classDetailsUI_main(
      id = ns("class1"), 
      label = "Class1", 
      defaultName = defaults[1]
    ),
    hr(),
    classDetailsUI_main(
      id = ns("class2"), 
      label = "Class 2", 
      defaultName = defaults[2]
    )
  )
  
}

#' @export
classDetailsWrapperUI_show <- function(id) {
  ns <- NS(id)
  tagList(
    column_2(
      br(),
      classDetailsUI_show(ns("class1"))
    ),
    column_2(
      br(),
      classDetailsUI_show(ns("class2"))
    )
  )
}



#' @export
classDetailsWrapper <- function(input, output, session, classLabel, classSelection, Selected, onChange){
  
  safeClassLabel <- reactiveValues(class1_name = "class1", class2_name = "class2")
    
  callModule(
    module = classDetails,
    id = "class1",
    index = 1,
    classLabel = safeClassLabel,
    classSelection = classSelection,
    Selected = Selected,
    onChange = onChange
  )
  
  callModule(
    module = classDetails,
    id = "class2",
    index = 2,
    classLabel = safeClassLabel,
    classSelection = classSelection,
    Selected = Selected,
    onChange = onChange
  )
  
  observe({
    
    cl <- reactiveValuesToList(safeClassLabel)
    if(anyDuplicated(unlist(cl))) {
      log_trace("Duplicated class labels")
      classLabel$class1_name <- "class1"
      classLabel$class2_name <- "class2"
    } else {
      
      log_trace("Normal class labels: c1: {safeClassLabel$class1_name}",
                " c2: {safeClassLabel$class2_name}")
      classLabel$class1_name <- safeClassLabel$class1_name
      classLabel$class2_name <- safeClassLabel$class2_name
      shinyjs::hide("class1-sameNamesInfo")
      shinyjs::hide("class2-sameNamesInfo")
    }
    
  })
  
}
