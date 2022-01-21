#' @rdname expressionVolcanoTab
#' @export
expressionVolcanoTabUI_main <- function(id){
  ns <- NS(id)
  fluidRow_12(plotWrapperUI(ns("volcanoplot"), height = "800px"))
}

#' @rdname expressionVolcanoTab
#' @export
expressionVolcanoTabUI_sidebar <- function(id){
  ns <- NS(id)
  list(
    sliderInput(
      inputId = ns("pvalue"), 
      label = "-log10 p-value", 
      min = 0, 
      max = 10, 
      step = 0.1, 
      value = 2
    ),
    sliderInput(
      inputId = ns("adjpvalue"), 
      "-log10 adj-p-value", 
      min = 0, 
      max = 10, 
      step = 0.1, 
      value = 0
    ),
    hr(),
    sliderInput(
      inputId = ns("log2FC"), 
      label = "log2 fold change", 
      min = 0, 
      max = 6, 
      step = 0.1, 
      value = 2.5
    ),
    radioButtons(
      inputId = ns("FCside"), 
      label = "apply fold change filter", 
      choices = c("both sides", "left", "right"), 
      selected = "both sides"
    ),
    uiOutput(ns("indicator"))
  )
}


#' Expression Volcano Tab
#'
#' @param input 
#' @param output 
#' @param session 
#' @param classLabel 
#' @param TableData 
#'
#' @rdname expressionVolcanoTab
#' @export
#' 
expressionVolcanoTab <- function(input, output, session, classLabel, TableData){
  rv <- reactiveValues()
  isReady <- FALSE
  
  registerExtendedInputObserver(
    input = input, rv = rv,
    inputId = "pvalue"
  )
  registerExtendedInputObserver(
    input = input, rv = rv,
    inputId = "adjpvalue"
  )
  registerExtendedInputObserver(
    input = input, rv = rv,
    inputId = "log2FC"
  )
  
  output$indicator <- renderUI({
    d <- TableData()
    if (is.null(d)) return()
    
    i100th <- min(nrow(d), 100) # if there's less than 100 rows, then use last
    
    myR <- range(d$P.Value) %>% log10() * -1
    value <- d[i100th, "P.Value"] %>% log10() * -1
    updateSliderInput(
      session = session, 
      inputId = "pvalue", 
      min = floor(myR[[2]]), 
      max = ceiling(myR[[1]]),
      value = value, 
      step = 0.1
    )
    freezeReactiveValue(input, "pvalue")
    
    myR <- range(d$adj.p.val) %>% log10() * -1
    value <- d[i100th, "adj.p.val"] %>% log10() * -1
    rv$adjpvalue <- value
    updateSliderInput(
      session = session, 
      inputId = "adjpvalue", 
      min = floor(myR[[2]]), 
      max = ceiling(myR[[1]]),
      value = value, 
      step = 0.1
    )
    freezeReactiveValue(input, "adjpvalue")
    
    myR <- range(abs(d$logFC))
    value <- median(abs(d[1:i100th, "logFC"]))
    rv$log2FC <- value
    updateSliderInput(
      session = session, 
      inputId = "log2FC", 
      min = floor(myR[[1]]), 
      max = ceiling(myR[[2]]),
      value = value, 
      step = 0.1
    )
    freezeReactiveValue(input, "log2FC")
    
    isReady <<- TRUE
    
    NULL
  })
  
  VolcanoExpr <- reactive({
    validate(
      need(isReady, "wait"),
      need(TableData(), "wait"),
      need(rv$pvalue, "wait"),
      need(rv$adjpvalue, "wait"),
      need(rv$log2FC, "wait")
    )
    
    cl <- reactiveValuesToList(classLabel)
    classNames <- c(cl$class1_name, cl$class2_name)
    generateVolcanoPlot(
      diffExpr = TableData(), 
      minuslog10pval = rv$pvalue, 
      minuslog10adjpval = rv$adjpvalue, 
      log2FC = rv$log2FC, 
      FCside = input$FCside, 
      classLabels = classNames
    )
  })
  
  callModule(
    module = plotWrapper,
    id = "volcanoplot",
    PlotExpr = VolcanoExpr
  )
}
