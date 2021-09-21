#' @export
classDetailsUI_main <- function(id, label, defaultName){
  ns <- NS(id)
  styleDetails <- "padding:4px; margin:3px; font-size:90%"
  
  div(
    textInput(
      inputId = ns("name"), 
      label = paste(label, "name:"), 
      value = defaultName
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

#' @export
classDetailsUI_show <- function(id){
  ns <- NS(id)
  
  div(
    textOutput(ns("title")),
    verbatimTextOutput(ns("list"))
  )
}

#' @export
classDetails <- function(input, output, session, index, classLabel, classSelection, SelectedCellline, onChange){
  selectionId <- paste0("class", index)
  labelId <- paste0(selectionId, "_name")
  anotherSelectionId <- paste0("class", index %% 2 + 1) # 1 -> 2 or 2 -> 1
  
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
      classLabel[[labelId]] <- input$name
    }
  )
  
  observeEvent(
    eventExpr = input$add, 
    handlerExpr = {
      s <- SelectedCellline()
      old <- Selection()
      cl <- sort(unique(union(s$celllinename, old)))
      
      classSelection[[selectionId]] <- onChange(
        SelectedCellline = s, 
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
      s <- SelectedCellline()
      old <- Selection()
      cl <- sort(unique(setdiff(old, s$celllinename)))
      
      classSelection[[selectionId]] <- onChange(
        SelectedCellline = s, 
        old = old,
        new = cl
      )
    }
  )
  
  observeEvent(
    eventExpr = input$intersect, 
    handlerExpr = {
      s <- SelectedCellline()
      old <- Selection()
      cl <- sort(unique(intersect(old, s$celllinename)))
      
      classSelection[[selectionId]] <- onChange(
        SelectedCellline = s, 
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
        SelectedCellline = SelectedCellline(), 
        old = old,
        new = NULL,
        reset = FALSE
      )
    }
  )
}
